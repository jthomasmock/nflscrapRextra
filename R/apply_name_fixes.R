#' apply_name_fixes
#'
#' @param p Input dataframe
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
apply_name_fixes <- function(p)
{
  report("Applying name fixes")
  p <- p %>%
    mutate(
      name=ifelse(name == "G.Minshew II","G.Minshew",name),
      passer_player_name=ifelse(name == "G.Minshew II","G.Minshew",passer_player_name),
      rusher_player_name=ifelse(name == "G.Minshew II","G.Minshew",rusher_player_name),
      name=ifelse(name == "Jos.Allen","J.Allen",name),
      passer_player_name=ifelse(name == "Jos.Allen","J.Allen",passer_player_name),
      rusher_player_name=ifelse(name == "Jos.Allen","J.Allen",rusher_player_name),
      receiver_player_name=ifelse(receiver_player_name == "D.Chark Jr.","D.Chark",receiver_player_name))
  return(p)
}
