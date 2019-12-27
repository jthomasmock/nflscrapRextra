# fix inconsistent data types
#' fix_inconsistent_data_types
#'
#' @param p Input dataframe
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
fix_inconsistent_data_types <- function(p)
{
  p <- p %>%
    mutate(game_id=as.character(game_id),
           play_id=as.numeric(play_id),
           time=substr(as.character(time),1,5),
           down=as.numeric(down),
           blocked_player_id=as.character(blocked_player_id),
           fumble_recovery_2_yards=as.numeric(fumble_recovery_2_yards),
           fumble_recovery_2_player_id=as.character(fumble_recovery_2_player_id),
           forced_fumble_player_2_player_id=as.character(forced_fumble_player_2_player_id),
           tackle_for_loss_2_player_id=as.character(forced_fumble_player_2_player_id))
  return(p)
}
