#' apply_series_data
#'
#' @param p input dataframe
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
apply_series_data <- function(p)
{
  report("Applying series and series success")

  # identify broken games
  broken_games <- unique(p$game_id[is.na(p$yards_gained)
                                   & !(is.na(p$play_type) |
                                         p$play_type %in% c("note","no_play"))])

  # add in series and series_success variables
  p <- p %>% mutate(series=NA,series_success=0)

  # initialize loop trackers
  p$series[min(which(p$play_type != "kickoff"))] <- 1
  p$series_success[1:(min(which(p$play_type != "kickoff"))-1)] <- NA
  series <- 1
  lb <- 1

  # play loop
  for (r in (min(which(p$play_type != "kickoff"))+1):nrow(p))
  {

    # progress report
    if (r %% 10000 == 0)
    {
      report(paste("Series Data:",r,"of",nrow(p),"plays"))
      saveRDS(p,file=plays_filename)
    }

    # skip broken games or no-description plays
    if (p$game_id[r] %in% broken_games || is.na(p$desc[r]))
    {
      lb <- lb + 1
      next
    }

    # if posteam is not defined or is a timeout, mark as a non-series and skip
    if (p$play_type[r] == "note")
    {
      p$series[r] <- NA
      p$series_success[r] <- NA
      lb <- lb + 1
      next
    }

    # game has changed
    if (p$game_id[r] != p$game_id[r-lb])
    {
      if (p$yards_gained[r-lb] >= p$ydstogo[r-lb])
      {
        p$series_success[p$game_id == p$game_id[r-lb] & p$series == series] <- 1
      } else if (any(p$play_type[p$game_id == p$game_id[r] & p$series == series]
                     %in% c("qb_kneel","qb_spike"))) {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- NA
      } else if (p$down[r-lb] == 4) {
        p$series_success[p$game_id == p$game_id[r-lb] & p$series == series] <- 0
      } else {
        p$series_success[p$game_id == p$game_id[r-lb] & p$series == series] <- NA
      }
      series <- 1
      # beginning of 2nd half or overtime
    } else if (p$qtr[r] != p$qtr[r-lb] && (p$qtr[r] == 3 || p$qtr[r] >= 5)) {
      if (p$yards_gained[r-lb] >= p$ydstogo[r-lb])
      {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- 1
      } else if (any(p$play_type[p$game_id == p$game_id[r] & p$series == series]
                     %in% c("qb_kneel","qb_spike"))) {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- NA
      } else if (p$down[r-lb] == 4) {
        p$series_success[p$game_id == p$game_id[r-lb] & p$series == series] <- 0
      } else {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- NA
      }
      series <- series + 1
      # or drive has changed
    } else if (p$drive[r] != p$drive[r-lb]) {
      if (p$yards_gained[r-lb] >= p$ydstogo[r-lb])
      {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- 1
      } else if (any(p$play_type[p$game_id == p$game_id[r] & p$series == series]
                     %in% c("qb_kneel","qb_spike"))) {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- NA
      }
      series <- series + 1
      # first down or NA down with last play having enough yards or defensive penalty
    } else if ((is.na(p$down[r]) || p$down[r] == 1) &&
               ((!is.na(p$yards_gained[r-lb]) && p$yards_gained[r-lb] >= p$ydstogo[r-lb])
                || any(p$first_down_penalty[(r-lb):(r-1)] == 1,na.rm=TRUE))) {
      if (p$play_type[r-lb] != "kickoff" ||
          any(p$first_down_penalty[(r-lb):(r-1)] == 1,na.rm=TRUE))
      {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- 1
      }
      series <- series + 1
      # blocked field goal the same team recovered
    } else if (!is.na(p$down[r]) && p$down[r] == 1 &&
               !is.na(p$field_goal_result[r-lb]) &&
               p$field_goal_result[r-lb] == "blocked" &&
               p$posteam[r-lb] == p$posteam[r]) {
      p$series_success[p$game_id == p$game_id[r] & p$series == series] <- 0
      series <- series + 1
    }

    # mark series for kickoffs as NA
    if (!is.na(p$play_type[r]) && p$play_type[r] == "kickoff")
    {
      p$series_success[r] <- NA
      series <- series - 1  # otherwise it would advance 2, want to advance 1
    } else if ((!is.na(p$play_type[r]) && p$play_type[r] == "extra_point") ||
               (!is.na(p$two_point_attempt[r]) && p$two_point_attempt[r] == 1)) {
      p$series_success[r] <- NA
      series <- series - 1  # otherwise it would advance 2, want to advance 1
      # mark series for all other p
    } else {
      p$series[r] <- series
    }

    # if this is a real play, reset lookback to 1, otherwise increment it
    ## the looback defines the "previous" play
    ## we want to skip this for p that don't actually affect series data
    if (is.na(p$play_type[r]) ||
        p$play_type[r] %in% c("no_play","extra_point","note") ||
        (!is.na(p$two_point_attempt[r]) && p$two_point_attempt[r] == 1))
    {
      lb <- lb + 1
    } else {
      lb <- 1
    }

  }

  # handle final series in the data
  lb <- 0
  while(is.na(p$play_type[nrow(p)-lb]) ||
        p$play_type[nrow(p)-lb] %in% c("no_play","note"))
  {
    lb <- lb + 1
  }
  if (p$yards_gained[nrow(p)-lb] >= p$ydstogo[nrow(p)-lb])
  {
    p$series_success[p$game_id == p$game_id[nrow(p)-lb] & p$series == series] <- 1
  } else if (any(p$play_type[p$game_id == p$game_id[r] & p$series == series]
                 %in% c("qb_kneel","qb_spike"))) {
    p$series_success[p$game_id == p$game_id[r] & p$series == series] <- NA
  } else if (p$down[nrow(p)-lb] == 4) {
    p$series_success[p$game_id == p$game_id[nrow(p)-lb] & p$series == series] <- 0
  } else {
    p$series_success[p$game_id == p$game_id[nrow(p)] & p$series == series] <- NA
  }

  report(paste("Series Data Complete!"))
  return(p)
}
