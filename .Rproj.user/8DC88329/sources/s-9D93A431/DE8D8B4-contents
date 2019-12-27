#' apply_baldwin_mutations
#'
#' @param p input dataframe
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
apply_baldwin_mutations <- function(p)
{
  report("Applying Ben Baldwin mutations")
  p <- p %>%
    mutate(
      # identify passes and rushes
      ## note this treats qb scrambles as passes since a pass was called
      pass=ifelse(str_detect(desc,"( pass)|(sacked)|(scramble)"),1,0),
      rush=ifelse(str_detect(desc,"(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0,1,0),
      # plays are defined as successful when EPA is positive
      success=ifelse(is.na(epa),NA,ifelse(epa>0,1,0)),
      # fix player name fields so they aren't NA on penalty plays
      ## code for this from Keenan Abdoo
      passer_player_name=ifelse(play_type == "no_play" & pass == 1,
                                str_extract(desc,"(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s(( pass)|(sack)|(scramble)))"),
                                passer_player_name),
      receiver_player_name=ifelse(play_type == "no_play" & str_detect(desc," pass"),
                                  str_extract(desc,"(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
      rusher_player_name=ifelse(play_type == "no_play" & rush == 1,
                                str_extract(desc,"(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name),
      # this is shorthand so "name" is the QB (if pass) or runner (if run)
      name=ifelse(!is.na(passer_player_name),passer_player_name,rusher_player_name),
      # set yards_gained to be NA on penalties rather then 0
      yards_gained=ifelse(play_type == "no_play" | play_type == "note",NA,yards_gained),
      # easy filter: play is 1 if a "normal" play (including penalties), or 0 otherwise
      play=ifelse(!is.na(epa) & !is.na(posteam) &
                    desc != "*** play under review ***" &
                    substr(desc,1,8) != "Timeout " &
                    play_type %in% c("no_play","pass","run"),1,0))
  return(p)
}
