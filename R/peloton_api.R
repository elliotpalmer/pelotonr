
# VARIABLES -------------------------------------------------------

#' Function for storing api urls
#'
#' Allows for storing API urls in one place for calling in other functions
#'
#' @param NULL
#'
#' @return a list containing the Peloton API urls
#'
#' @examples
#' get_peloton_urls()
#'
#' @export
get_peloton_urls <- function(){
  list(
    base.url  = "https://api.pelotoncycle.com/api/",
    auth.url = "https://api.pelotoncycle.com/auth/login"
  )
}

#' Get API url for a specific user id
#'
#' @param userid a string, the user id found in your Peloton profile
#'
#' @return the user workout API call
#'
#' @examples
#' user_id <- '44e4b4f4c14434444a4f554444a44bde4'
#' get_workout_url(user_id)
#'
#' @export
get_workout_url <- function(userid){
  urls <- get_peloton_urls()
  paste0(urls$base.url, "user/", userid, "/workouts?joins=peloton.ride&limit=1000")
}

#' Get API url for a workout's stream data
#'
#' @param workout_id a string of and individial workout id
#' @param time_offset_interval
#'
#' @return the user workout stream API call
#'
#' @examples
#' workout_id <- '44e4b4f4c14434444a4f554444a44bde4'
#' get_workout_stream_url(user_id)
#'
#' @export
get_workout_stream_url <- function(workout_id, time_offset_interval = 1){
  urls <- get_peloton_urls()
  stream_url <- sprintf(paste0(urls$base.url,
                               "workout/%s/performance_graph?every_n=",
                               time_offset_interval),workout_id)
  return(stream_url)
}

# AUTH / LOGIN ------------------------------------------------------------

#' Return a cookie string after authenticating with the Peloton API
#'
#' @param username a string, your Peloton account username
#' @param password a string, your Peloton account password
#'
#' @return a string, the cookie id
#'
#' @examples
#'
#' get_peloton_cookie('username','password')
#'
#' @export
get_peloton_cookie <- function(username, password){
  urls <- get_peloton_urls()

  credentials <- httr::POST(urls$auth.url,
                            body = list(password = password, username_or_email = username),
                            encode = "json")

  parse_credentials_to_cookie(credentials)
}

#' Helper function that parses the cookie id from JSON
#'
#' @param credentials JSON, the returned json from the API after authentication
#'
#' @return a string, cookie id
#'
#' @examples
#' parse_credentials_to_cookie(credentials)
#'
#' @export
parse_credentials_to_cookie <- function(credentials){
  headers <- credentials$headers
  peloton_session_string <- headers[grepl("peloton_session_id", headers)][[1]]
  peloton_session_string <- strsplit(peloton_session_string, ";")[[1]][1]
  peloton_cookie <- strsplit(peloton_session_string, "=")[[1]][2]
  return(peloton_cookie)
}

# USER DATA ---------------------------------------------------------------

#' Get User data from the Peloton API
#'
#' Includes data on User, workout counts, and workout streaks
#'
#' @param userid a string, the user id found in your Peloton profile
#'
#' @return a list, includes 3 data sets, base user data, workout counts and streaks
#'
#' @examples
#' get_peloton_user_data(user_id)
#'
#' @export
get_peloton_user_data <- function(userid){
  urls <- get_peloton_urls()
  json_user <- jsonlite::fromJSON(paste0(urls$base.url, "user/", userid))

  list(
    base_user = parse_base_user_data(json_user),
    workout_counts = parse_peloton_workout_counts(json_user),
    streaks = parse_peloton_streaks(json_user)
  )
}

#' Parses user json to return base user data
#'
#'
#' @param json_user JSON, json return from the user API call
#'
#' @return a dataframe
#'
#' @examples
#' user_id '5749258a9eb4389fbe23821'
#' urls <- get_peloton_urls()
#' json_user <- jsonlite::fromJSON(paste0(urls$base.url, "user/", userid))
#' parse_base_user_data(json_user)
#'
#' @export
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

#' Parses user json to return workout counts
#'
#' @param json_user JSON, json return from the user API call
#'
#' @return a dataframe
#'
#' @examples
#' user_id '5749258a9eb4389fbe23821'
#' urls <- get_peloton_urls()
#' json_user <- jsonlite::fromJSON(paste0(urls$base.url, "user/", userid))
#' parse_peloton_workout_counts(json_user)
#'
#' @export
parse_peloton_workout_counts <- function(json_user){
  data.frame(do.call(cbind, json_user$workout_counts))
}

#' Parses user json to return streaks
#'
#' @param json_user JSON, json return from the user API call
#'
#' @return a dataframe
#'
#' @examples
#' user_id '5749258a9eb4389fbe23821'
#' urls <- get_peloton_urls()
#' json_user <- jsonlite::fromJSON(paste0(urls$base.url, "user/", userid))
#' parse_peloton_streaks(json_user)
#'
#' @export
parse_peloton_streaks <- function(json_user){
  streak_data <- data.frame(do.call(cbind, json_user$streaks))
  streak_data$start_date_of_current_weekly <- .parse_date(streak_data$start_date_of_current_weekly)
  return(streak_data)
}


# WORKOUT STATS -----------------------------------------------------------

#' Return a data frame of workout data
#'
#' @param userid a string, the user id found in your Peloton profile
#' @param peloton_cookie a string, a cookie id
#'
#' @return a dataframe
#'
#' @examples
#' userid <-  '5749258a9eb4389fbe23821'
#' peloton_cookie <- get_peloton_cookie(username, password)
#' get_peloton_workout_data(userid, peloton_cookie)
#'
#' @export
get_peloton_workout_data <- function(userid, peloton_cookie){
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

#' A helper function to join the individual workout dataframes
#'
#' @param workouts_data a list of dataframes
#'
#' @return a dataframe
#'
#' @examples
#'
#' @export
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

#' Returns the json for the workout detail, requires authentication
#'
#' @param workout_url a string, generated from the get_workout_url() function
#' @param peloton_cookie a string, cookie id
#'
#' @return JSON
#'
#' @examples
#'
#' @export
get_workout_detail_json <- function(workout_url, peloton_cookie){

  response <- httr::GET(workout_url, set_cookies(peloton_session_id = peloton_cookie))
  response <- httr::content(response, "text")
  response <- jsonlite::fromJSON(response, simplifyDataFrame = TRUE)
  return(response)

}

#' Returns ride stats data frame
#'
#' @param workouts_json a string, generated from the get_workout_url() function
#'
#' @return a dataframe
#'
#' @examples
#'
#' @export
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

#' Returns user workouts data frame
#'
#' @param workouts_json a string, generated from the get_workout_url() function
#'
#' @return a dataframe
#'
#' @examples
#'
#' @export
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

#' @export
get_workout_summary_json <- function(workout_ids){
  urls <- get_peloton_urls()
  lapply(sprintf(paste0(urls$base.url, "workout/%s/summary"), workout_ids),jsonlite::fromJSON)
}

#' @export
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

#' @export
get_achievements_json <- function(workout_ids){
  urls <- get_peloton_urls()
  achievements_urls <- sprintf(paste0(urls$base.url, "workout/%s"), workout_ids)
  achievements_json <- lapply(achievements_urls, jsonlite::fromJSON)
  return(achievements_json)
}

#' @export
parse_leaderboard_rank <- function(achievements_json){
  leaderboard_data <- as.data.frame(do.call(rbind, achievements_json))
  leaderboard_columns <- c("id", "leaderboard_rank")
  leaderboard_data <- leaderboard_data[,leaderboard_columns]
  leaderboard_data <- .rename(leaderboard_data, "id", "workout_id")
  leaderboard_data <- .unnest(leaderboard_data)
  return(leaderboard_data)
}

#' @export
parse_achievements_list <- function(achievements_json){
  achievement_list_data <- as.data.frame(do.call(rbind, achievements_json))
  achievement_list_columns <- c("id", "achievement_templates")
  achievement_list_data <- achievement_list_data[,achievement_list_columns]
  achievement_list_data$achievement_count <- lengths(achievement_list_data$achievement_templates)
  achievement_list_data <- achievement_list_data[achievement_list_data$achievement_count != 0,]
  return(achievement_list_data)
}

#' @export
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

#' @export
get_peloton_stream_data <- function(workout_ids, time_offset_interval = 1){
  stream_jsons <- lapply(workout_ids, get_peloton_workout_stream_json)
  stream_data  <- lapply(stream_jsons, parse_peloton_workout_stream)
  do.call(rbind, stream_data)
}

#' @export
get_peloton_workout_stream_json <- function(workout_id, time_offset_interval = 1){
  stream_url <- get_workout_stream_url(workout_id, time_offset_interval)
  stream_json <- jsonlite::fromJSON(stream_url)
  stream_json$workout_id <- workout_id
  return(stream_json)
}

#' @export
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

#' @export
parse_segment_list <- function(stream_json){
  segment_list_columns <- c("name","start_time_offset", "length", "intensity_in_mets")
  segment_list <- stream_json$segment_list[,segment_list_columns]
  segment_list$end_time_offset <- segment_list$start_time_offset + segment_list$length
  return(segment_list)
}

#' @export
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

#' @export
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

