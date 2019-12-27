#' double_games
#'
#' takes input where each game has one row with teams as `away_team` and `home_team`
#' returns with each game having two rows with teams listed as `team` and `opp`
#'
#' @param g input dataframe
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#'


double_games <- function(g)
{
  g1 <- g %>%
    rename(team=away_team,team_score=away_score,
           opp=home_team,opp_score=home_score) %>%
    mutate(location=ifelse(location == "Home","Away",location),
           result=-1*result)
  g2 <- g %>%
    rename(team=home_team,team_score=home_score,
           opp=away_team,opp_score=away_score)
  g <- bind_rows(g1,g2) %>%
    arrange(gameday,gametime,game_id,location)
  return(g)
}
