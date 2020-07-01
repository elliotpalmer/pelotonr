# UPDATE 2020-07-01
Check out the links below for a more complete and polished implementation:
* https://lgellis.github.io/pelotonR/
* https://www.littlemissdata.com/blog/pelotonr

# pelotonr

The goal of pelatonr is to make accessing your data from the Pelaton cycling API easy. 

This work relies heavily on the examples created by:  
https://github.com/AnfieldWest/Peloton-using-R

## Disclaimer

I only have the Pelaton Bike and not the Treadmill. So, I am only able to accurately get data for the bike from my profile. I more than welcome other Pelaton / Data Junkies to submit PR to add additional functionality for the Tread.

This library has no connection or sponsorship from Peloton and I make no presumption that the data here is without error.

## Example

``` r
devtools::github_install("epalmer/pelotonr")

# Username and Password Credentials to Log-in to Pelaton Profile
username <- "your.peloton.username"
password <- "your.peloton.password"

# Find this in your user profile. More detailed instructions below
userid <- "you.peloton.userid"

# Need this to access data from the API
peloton_cookie <- get_peloton_cookie(username, password)

# User Data: Includes high level stats about the user
user_data <- get_peloton_user_data(userid)

# Workout Data: Workout Summaries (1 line per workout)
workouts_data <- get_peloton_workout_detail(userid, peloton_cookie)

# Stream Data: 
# Note: Floor workouts generally don't have stream info. Future enhancements may include handling for this

cycling_workout_ids <- workout_data[workout_data$fitness_discipline == "cycling", "workout_id"]
  
stream_data <- get_peloton_workout_streams(cycling_workout_ids)
```
