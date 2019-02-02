
# VARIABLES -------------------------------------------------------

get_peloton_urls <- function(){
  list(
    base.url  = "https://api.pelotoncycle.com/api/",
    auth.url = "https://api.pelotoncycle.com/auth/login"
  )
}

get_workout_url <- function(userid){
  urls <- get_peloton_urls()
  paste0(urls$base.url, "user/", userid, "/workouts?joins=peloton.ride&limit=1000")
}

# AUTH / LOGIN ------------------------------------------------------------

parse_credentials_to_cookie <- function(credentials){
  headers <- credentials$headers
  peloton_session_string <- headers[grepl("peloton_session_id", headers)][[1]]
  peloton_session_string <- strsplit(peloton_session_string, ";")[[1]][1]
  peloton_cookie <- strsplit(peloton_session_string, "=")[[1]][2]
  return(peloton_cookie)
}

get_peloton_cookie <- function(username, password){
  urls <- get_peloton_urls()

  credentials <- httr::POST(urls$auth.url,
                            body = list(password = password, username_or_email = username),
                            encode = "json")

  parse_credentials_to_cookie(credentials)
}

# USER DATA ---------------------------------------------------------------

get_peloton_user_data <- function(userid){
  urls <- get_peloton_urls()
  json_user <- jsonlite::fromJSON(paste0(urls$base.url, "user/", userid))

  list(
    base_user = parse_base_user_data(json_user),
    workout_counts = parse_peloton_workout_counts(json_user),
    streaks = parse_peloton_streaks(json_user)
  )
}

parse_base_user_data <- function(json_user){
  base_columns <- c("id", "username", "location", "total_workouts",
                    "total_non_pedaling_metric_workouts", "last_workout_at",
                    "image_url", "total_following")

  base_user <- data.frame(json_user[base_columns])
  base_user <- .rename(base_user, "id", "user_id")
  base_user <- .rename(base_user, "image_url", "user_url")
  base_user$last_workout_at <- .parse_date(base_user$last_workout_at)
  return(base_user)
}

parse_peloton_workout_counts <- function(json_user){
  data.frame(do.call(cbind, json_user$workout_counts))
}

parse_peloton_streaks <- function(json_user){
  streak_data <- data.frame(do.call(cbind, json_user$streaks))
  streak_data$start_date_of_current_weekly <- .parse_date(streak_data$start_date_of_current_weekly)
  return(streak_data)
}


# WORKOUT STATS -----------------------------------------------------------

get_peloton_workout_detail <- function(userid, peloton_cookie){
  workout_url <- get_workout_url(userid)
  workouts_json <- get_workout_detail_json(workout_url, peloton_cookie)
  workout_ids <- workouts_json$data$id
  workout_summary_json <- get_workout_summary_json(workout_ids)
  achievements_json <- get_achievements_json(workout_ids)

  workouts_data <- list(
    workouts_all = unlist(workouts_json$data$id),
    ride_stats = parse_ride_stats(workouts_json),
    user_workouts = parse_user_workouts(workouts_json),
    workout_summary = parse_workout_summary(workout_summary_json),
    leaderboard_ranks = parse_leaderboard_rank(achievements_json),
    achievements = parse_achievements(achievements_json)
  )

  combine_workout_data(workouts_data)

}

combine_workout_data <- function(workouts_data){

  workout_data <- merge(
    workouts_data$ride_stats,
    workouts_data$workout_summary,
    by = "ride_id")

  workout_data <- merge(
    workout_data,
    workouts_data$user_workouts,
    by = "workout_id")

  workout_data <- merge(
    workout_data,
    workouts_data$leaderboard_ranks,
    by = "workout_id")

  achievements <- aggregate(workouts_data$achievements[,c("workout_id","name")],
                            by = workouts_data$achievements[c("workout_id")],
                            FUN = function(x) paste0(x, collapse = ", "))[,c(1,3)]

  achievements <- .rename(achievements, "name", "achievements")

  workout_data <- merge(
    workout_data,
    achievements,
    by = "workout_id",
    all.x = TRUE)

  return(unique(workout_data))
}

get_workout_detail_json <- function(workout_url, peloton_cookie){

  response <- httr::GET(workout_url, set_cookies(peloton_session_id = peloton_cookie))
  response <- httr::content(response, "text")
  response <- jsonlite::fromJSON(response, simplifyDataFrame = TRUE)
  return(response)

}

parse_ride_stats <- function(workouts_json){
  ride_stats_data <- as.data.frame(do.call(cbind, workouts_json$data$peloton$ride))
  ride_stat_columns <- c('id', 'series_id', 'ride_type_id', 'instructor_id', 'duration', 'pedaling_duration',
                         'title', 'description', 'fitness_discipline', 'difficulty_rating_count',
                         'difficulty_rating_avg', 'difficulty_estimate', 'overall_rating_count',
                         'overall_rating_avg', 'total_workouts', 'image_url')
  ride_stats_data <- ride_stats_data[,ride_stat_columns]
  ride_stats_data <- .rename(ride_stats_data, "id", "ride_id")
  ride_stats_data <- .rename(ride_stats_data, "image_url", "ride_url")
  ride_stats_data <- as.data.frame(lapply(ride_stats_data, unlist), stringsAsFactors = FALSE)
}

parse_user_workouts <- function(workouts_json){
  user_workouts <- as.data.frame(do.call(cbind, workouts_json$data))
  user_workouts_columns <- c('user_id', 'id', 'strava_id', 'fitbit_id', 'name', 'workout_type',
                             'is_total_work_personal_record', 'total_work',
                             'start_time', 'end_time', 'created_at')
  user_workouts <- user_workouts[,user_workouts_columns]
  user_workouts <- .rename(user_workouts, "id", "workout_id")
  user_workouts <- as.data.frame(lapply(user_workouts, unlist), stringsAsFactors = FALSE)

  # Parse Dates
  for(date_col  in c("start_time","end_time","created_at")){
    user_workouts[,date_col] <- .parse_date(user_workouts[,date_col])
  }

  return(user_workouts)
}

# WORKOUT SUMMARY ---------------------------------------------------------

get_workout_summary_json <- function(workout_ids){
  urls <- get_peloton_urls()
  lapply(sprintf(paste0(urls$base.url, "workout/%s/summary"), workout_ids),jsonlite::fromJSON)
}

parse_workout_summary <- function(workout_summary_json){
  workout_summary_data <- as.data.frame(do.call(rbind, workout_summary_json))
  workout_summary_columns <- c('workout_id', 'ride_id', 'avg_power', 'avg_cadence', 'avg_resistance',
                               'avg_speed', 'avg_heart_rate','max_power', 'max_cadence',
                               'max_resistance', 'max_speed', 'max_heart_rate', 'distance',
                               'calories', 'seconds_since_pedaling_start')
  workout_summary_data <- workout_summary_data[,workout_summary_columns]
  workout_summary_data <- lapply(workout_summary_data, function(x){x[sapply(x, is.null)] <- NA;x})
  workout_summary_data <- as.data.frame(do.call(cbind, workout_summary_data))
  workout_summary_data <- .unnest(workout_summary_data)
}

# ACHIEVEMENTS ------------------------------------------------------------

get_achievements_json <- function(workout_ids){
  urls <- get_peloton_urls()
  achievements_urls <- sprintf(paste0(urls$base.url, "workout/%s"), workout_ids)
  achievements_json <- lapply(achievements_urls, jsonlite::fromJSON)
  return(achievements_json)
}

parse_leaderboard_rank <- function(achievements_json){
  leaderboard_data <- as.data.frame(do.call(rbind, achievements_json))
  leaderboard_columns <- c("id", "leaderboard_rank")
  leaderboard_data <- leaderboard_data[,leaderboard_columns]
  leaderboard_data <- .rename(leaderboard_data, "id", "workout_id")
  leaderboard_data <- .unnest(leaderboard_data)
  return(leaderboard_data)
}

parse_achievements_list <- function(achievements_json){
  achievement_list_data <- as.data.frame(do.call(rbind, achievements_json))
  achievement_list_columns <- c("id", "achievement_templates")
  achievement_list_data <- achievement_list_data[,achievement_list_columns]
  achievement_list_data$achievement_count <- lengths(achievement_list_data$achievement_templates)
  achievement_list_data <- achievement_list_data[achievement_list_data$achievement_count != 0,]
  return(achievement_list_data)
}

parse_achievements <- function(achievements_json){
  achievements_list <- parse_achievements_list(achievements_json)
  templates <- achievements_list$achievement_templates
  ids <- achievements_list$id
  names(templates) <- ids
  templates_w_id <- lapply(ids, function(x){templates[[x]]$workout_id <- x; return(templates[[x]])})
  achievements_data <- do.call(rbind, templates_w_id)
  return(achievements_data)
}



# STREAM ------------------------------------------------------------------

get_peloton_workout_streams <- function(workout_ids, time_offset_interval = 1){
  stream_jsons <- lapply(workout_ids, get_peloton_workout_stream_json)
  stream_data  <- lapply(stream_jsons, parse_peloton_workout_stream)
  do.call(rbind, stream_data)
}

get_workout_stream_url <- function(workout_id, time_offset_interval = 1){
  urls <- get_peloton_urls()
  stream_url <- sprintf(paste0(urls$base.url,
                               "workout/%s/performance_graph?every_n=",
                               time_offset_interval),workout_id)
  return(stream_url)
}

get_peloton_workout_stream_json <- function(workout_id, time_offset_interval = 1){
  stream_url <- get_workout_stream_url(workout_id, time_offset_interval)
  stream_json <- jsonlite::fromJSON(stream_url)
  stream_json$workout_id <- workout_id
  return(stream_json)
}

parse_peloton_workout_stream <- function(stream_json){
  stream_data <- merge(
    parse_all_seconds(stream_json),
    parse_stream_values(stream_json),
    by = "seconds_since_start",
    all.x = TRUE)
  stream_columns <- c("workout_id", "seconds_since_start","name",
                      "Output","Cadence","Resistance","Speed")
  stream_data <- stream_data[,stream_columns]
  stream_data <- .rename(stream_data, "name","segment")
  names(stream_data) <- tolower(names(stream_data))
  return(stream_data)
}

parse_segment_list <- function(stream_json){
  segment_list_columns <- c("name","start_time_offset", "length", "intensity_in_mets")
  segment_list <- stream_json$segment_list[,segment_list_columns]
  segment_list$end_time_offset <- segment_list$start_time_offset + segment_list$length
  return(segment_list)
}

parse_all_seconds <- function(stream_json){
  seconds <- 0:stream_json$duration
  segment_list <- parse_segment_list(stream_json)[,c("name","start_time_offset", "end_time_offset")]
  all_seconds_data <- data.frame(seconds_since_start = seconds)
  seconds_since_pedaling <- data.frame(
    seconds_since_start = stream_json$seconds_since_pedaling_start,
    seconds_since_pedaling_start = stream_json$seconds_since_pedaling_start)
  all_seconds_data <- .join_between(x = all_seconds_data,
                                    y = segment_list,
                                    val = "seconds_since_start",
                                    btwn_lb = "start_time_offset",
                                    btwn_ub = "end_time_offset")
  # Adding one more row to complete the original data
  all_seconds_data <- rbind(
    all_seconds_data,
    data.frame(
      seconds_since_start = all_seconds_data[nrow(all_seconds_data),1] + 1,
      name = all_seconds_data[nrow(all_seconds_data),2]
    )
  )

  all_seconds_data <- merge(all_seconds_data, seconds_since_pedaling, by = "seconds_since_start", all.x = TRUE)
  all_seconds_data$workout_id <- stream_json$workout_id
  return(all_seconds_data)
}

parse_stream_values <- function(stream_json){
  values <- stream_json$metrics$values
  names(values) <- stream_json$metrics$display_name
  values <- as.data.frame(values)
  values$seconds_since_start <- stream_json$seconds_since_pedaling_start
  return(values)
}


# HELPER FUNCTIONS --------------------------------------------------------

.rename <- function(data, old_name, new_name){
  colnames(data)[colnames(data)==old_name] <- new_name
  return(data)
}

.parse_date <- function(date_value){
  as.POSIXct(as.POSIXlt(date_value, origin="1970-01-01"))
}

.unnest <- function(list_data){
  list_data <- lapply(list_data, function(x){x[sapply(x, is.null)] <- NA;x})
  as.data.frame(lapply(list_data, unlist), stringsAsFactors = FALSE)
}

.join_between <- function(x, y, val, btwn_lb, btwn_ub){
  cart_join <- merge(x, y, all = TRUE)
  gtet_lb <- cart_join[[val]] >= cart_join[[btwn_lb]]
  lt_ub   <- cart_join[[val]] < cart_join[[btwn_ub]]
  cart_join[gtet_lb & lt_ub,!(names(cart_join) %in% c(btwn_lb, btwn_ub))]
}

# TESTING -----------------------------------------------------------------
