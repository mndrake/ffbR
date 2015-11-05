.scoreCategories <- c("passAtt","passComp","passIncomp","passYds","passTds","passInt",
                      "rushAtt","rushYds","rushTds",
                      "rec","recTgt","recYds","recTds",
                      "returnTds","twoPts","fumbles",
                      "idpSolo","idpAst","idpSack","idpFumlRec","idpFumlForce","idpInt","idpPD",
                      "dstPtsAllow","dstYdsAllowed","dstSack","dstSafety","dstInt","dstFumlRec","dstFumlForce","dstBlk","dstTd",
                      "fg","fgAtt","fg0019","fg2029","fg3039","fg4049","fg50","xp")

.teams <- c("NE","CAR","SD","ATL","CIN","SEA","GB","NYG","NYJ","NO","PIT","KC","ARI","IND","DET","MIA","BAL","CHI",
            "HOU","MIN","OAK","DEN","TEN","TB","SF","CLE","STL","DAL","PHI","BUF","JAC","WAS")

#' @importFrom stringr str_detect
.clean_team <- function(x) {
  t <- str_detect(x, .teams)
  if (any(t)) {
    .teams[t]
  } else {
    'FA'
  }
}

#' @importFrom stringr str_split
.clean_name <- function(x) {
  x <- trimws(str_split(x, "Out:")[[1]][1])
  name <- trimws(str_split(x, .clean_team(x))[[1]][1])
  if (name == 'Christopher Ivory') {
    'Chris Ivory'
  } else if (name == 'Duke Johnson') {
    'Duke Johnson Jr.'
  } else {
    name
  }
}
