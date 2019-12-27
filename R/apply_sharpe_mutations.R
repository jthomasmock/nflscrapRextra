#' apply_sharp_mutations
#'
#' @param p Input dataframe
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
apply_sharpe_mutations <- function(p)
{
  report("Applying Lee Sharpe mutations")
  p <- p %>%
    mutate(
      play_type=case_when(
        is.na(posteam) ~ "note",
        substr(desc,1,8) == "Timeout " ~ "note",
        desc == "*** play under review ***" ~ "note",
        TRUE ~ play_type
      ),
      special=ifelse(play_type %in%
                       c("extra_point","field_goal","kickoff","punt"),1,0))
}
