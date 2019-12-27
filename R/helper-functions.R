
#' report
#' report progress to console
#' @param msg
#'
#' @return
#'
#' @examples
#' @import dplyr
#'

report <- function(msg)
{
  print(paste0(Sys.time(),": ",msg))
}

#' grep_col
#' look for text in names of columns of data frame
#'
#' @param x
#' @param df
#'
#' @return
#' @export
#'
#' @examples
grep_col <- function(x,df=plays)
{
  return(colnames(df)[grepl(x,colnames(df))])
}
