# game data
#' apply_game_data
#'
#' @param p input dataframe
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
apply_game_data <- function(p)
{
  if (!("alt_game_id" %in% colnames(p)))  # already included, don't reapply
  {
    report("Applying game data")
    if (!exists("games"))
      games <- read_csv("http://www.habitatring.com/games.csv")
    games <- games %>%
      mutate(game_id=as.character(game_id))
    p <- p %>%
      fix_inconsistent_data_types() %>%
      inner_join(games,by=c("game_id","away_team","home_team"))
  }
  return(p)
}
