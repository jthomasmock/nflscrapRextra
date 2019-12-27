#' Title
#'
#' @param p input dataframe
#' @param all_plays all play by play data since 2009
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom mgcv gam
#' @import dplyr
apply_completion_probability <- function(p,all_plays)
{
  report("Applying completion probability")

  # sort p and create cp column
  p$cp <- NA

  # season loop
  ## since our data only goes back to 2009, no CP for 2009
  seasons <- unique(p$season[p$season > 2009])
  for (s in seasons)
  {
    # get data from previous three seasons
    old_data <- all_plays %>%
      filter(play == 1 & season >= s-3 & season <= s-1) %>%
      filter(complete_pass == 1 | incomplete_pass == 1 | interception == 1) %>%
      filter(air_yards >= -10 & !is.na(receiver_player_id) & !is.na(pass_location)) %>%
      mutate(air_is_zero=ifelse(air_yards,1,0))

    # determine CPOE formula
    cp_model <- gam(complete_pass ~ s(air_yards) + air_is_zero + factor(pass_location),
                    data=old_data,method="REML")

    # apply CPOE to current season
    new_data <- p %>%
      filter(play == 1 & season == s) %>%
      filter(complete_pass == 1 | incomplete_pass == 1 | interception == 1) %>%
      filter(air_yards >= -10 & !is.na(receiver_player_id) & !is.na(pass_location)) %>%
      mutate(air_is_zero=ifelse(air_yards,1,0))
    new_data$cp <- predict.gam(cp_model,new_data)
    new_data <- new_data %>%
      select(game_id,play_id,cp)

    # merge into p
    p <- p %>%
      left_join(new_data,by=c("game_id","play_id")) %>%
      mutate(cp=ifelse(!is.na(cp.y),cp.y,cp.x)) %>%
      select(-cp.x,-cp.y)
  }
  return(p)
}
