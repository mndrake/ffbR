# fantasy football projections - fantasypros.com

get_fantasypros <- function(week) {

  # quarterbacks
  qb_fp <- readHTMLTable(paste0('http://www.fantasypros.com/nfl/projections/qb.php?week=', week), stringsAsFactors = FALSE)$data
  colnames(qb_fp) <- c('player','passAtt','passComp','passYds','passTds','passInt','rushAtt','rushYds','rushTds','fumbles','points')
  qb_fp$pos <- 'QB'

  # widerecivers
  wr_fp <- readHTMLTable(paste0('http://www.fantasypros.com/nfl/projections/wr.php?week=', week), stringsAsFactors = FALSE)$data
  colnames(wr_fp) <- c("player", "rushAtt", "rushYds", "rushTds", "rec", "recYds", "recTds", "fumbles", "points")
  wr_fp$pos <- 'WR'

  # runningbacks
  rb_fp <- readHTMLTable(paste0('http://www.fantasypros.com/nfl/projections/rb.php?week=', week), stringsAsFactors = FALSE)$data
  colnames(rb_fp) <- c("player", "rushAtt", "rushYds", "rushTds", "rec", "recYds", "recTds", "fumbles", "points")
  rb_fp$pos <- 'RB'

  # tight ends
  te_fp <- readHTMLTable(paste0('http://www.fantasypros.com/nfl/projections/te.php?week=', week), stringsAsFactors = FALSE)$data
  colnames(te_fp) <- c("player", "rec", "recYds", "recTds", "fumbles", "points")
  te_fp$pos <- 'TE'

  # kickers
  k_fp <- readHTMLTable(paste0('http://www.fantasypros.com/nfl/projections/k.php?week=', week), stringsAsFactors = FALSE)$data
  colnames(k_fp) <- c("player", "fg", "fgAtt", "xp", "points")
  k_fp$pos <- 'K'

  # combine
  all_fp <- qb_fp %>% full_join(wr_fp) %>% full_join(rb_fp) %>% full_join(te_fp) %>% full_join(k_fp)
  numericVars <- c(intersect(colnames(all_fp), scoreCategories), 'points')
  all_fp[, numericVars] <- sapply(all_fp[, numericVars], as.numeric)
  all_fp[, numericVars][is.na(all_fp[, numericVars])] <- 0
  all_fp$name <- sapply(all_fp$player, clean_name)
  all_fp$team <- sapply(all_fp$player, clean_team)

  all_fp %>%
    mutate(custom_points = passYds / 20 + passTds * 4 + rushYds / 10 + rushTds * 6 + rec + recYds / 10 + recTds * 6 - fumbles * 2 + fg * 3 - (fgAtt - fg) + xp) %>%
    tbl_dt()
}
