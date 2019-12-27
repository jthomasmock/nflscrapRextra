#' fix_team_abbreviations
#'
#' @param p Input dataframe
#' @param old_to_new convert old name to new name (TRUE/FALSE)
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
fix_team_abbreviations <- function(p, old_to_new=FALSE)
{
  for (col in grep_col("team",p))
  {
    x <- p %>% pull(col)
    if (typeof(x) == "character")
    {
      p[,col] <- case_when(
        x == "JAC" ~ "JAX",
        x == "LA" ~ "LAR",
        x == "SD" & old_to_new ~ "LAC",
        x == "STL" & old_to_new ~ "LAR",
        TRUE ~ x)
    }
  }
  return(p)
}
