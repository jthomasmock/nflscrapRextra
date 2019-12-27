# Apply team logos and colors
#' apply_colors_and_logos
#'
#' @param p an input dataframe
#' @param team_col a column specifying team name/abbreviation
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr ggimage nflscrapR
#' @importFrom glue glue
apply_colors_and_logos <- function(p, team_col = "")
{
  # default team_col values
  team_col <- case_when(
    team_col != "" ~ team_col,
    "team" %in% colnames(p) ~ "team",
    "posteam" %in% colnames(p) ~ "posteam",
    "defteam" %in% colnames(p) ~ "defteam",
    TRUE ~ team_col
  )

  # raise error if column not present
  if (!(team_col %in% colnames(p))) stop(glue("Column {team_col} is not present"))

  # load data
  if (!exists("team_colors"))
    team_colors <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/teamcolors.csv")
  if (!exists("team_logos"))
    team_logos <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/logos.csv")

  # function to determine the brightness of a color
  brightness <- function(hex)
  {
    result <- rep(0,length(hex))
    for (i in 2:7)
    {
      ch <- substr(hex,i,i)
      result <- result + ifelse(i %% 2 == 0,16,1) * case_when(
        ch == "0" ~ 0, ch == "1" ~ 1, ch == "2" ~ 2, ch == "3" ~ 3, ch == "4" ~ 4,
        ch == "5" ~ 5, ch == "6" ~ 6, ch == "7" ~ 7, ch == "8" ~ 8, ch == "9" ~ 9,
        ch == "a" | ch == "A" ~ 10,
        ch == "b" | ch == "B" ~ 11,
        ch == "c" | ch == "C" ~ 12,
        ch == "d" | ch == "D" ~ 13,
        ch == "e" | ch == "E" ~ 14,
        ch == "f" | ch == "F" ~ 15,
        TRUE ~ 0
      )
    }
    return(result)
  }

  # use the primary color if brightness > 128, else grab secondary
  team_colors <- team_colors %>%
    mutate(use_color=ifelse(brightness(color) > 128,color,color2)) %>%
    select(team,use_color)

  # add to p
  p <- p %>%
    inner_join(team_colors,by=setNames("team",team_col)) %>%
    inner_join(team_logos,by=setNames("team",team_col))
}
