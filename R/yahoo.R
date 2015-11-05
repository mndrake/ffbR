# library(httr)
# library(RJSONIO)
# library(data.table)
# library(magrittr)
# library(stringr)
# source('fantasypros.R')

get_token <- function(token_file){
  creds = read.table(token_file, stringsAsFactors = FALSE)
  consumer_key = creds[1,1]
  consumer_secret = creds[2,1]
  myapp = oauth_app("MY_FFB", key = consumer_key, secret = consumer_secret)
  oauth1.0_token(oauth_endpoints("yahoo"), myapp)
}

get_league_key <- function(league_id, token){
  game_info = GET("http://fantasysports.yahooapis.com/fantasy/v2/game/nfl?format=json", config(token = token)) %>%
    as.character() %>%
    fromJSON(asText = TRUE) %>%
    unlist()
  as.character(game_info['fantasy_content.game.game_key']) %>%
    paste0('.l.', league_id)
}

get_rosters <- function(league_key, token) {
  get_team <- function(team_id) {
    my_team_key = paste0(league_key, ".t.", team_id)
    team_url = "http://fantasysports.yahooapis.com/fantasy/v2/team/"
    my_roster = GET(paste0(team_url, my_team_key, "/roster?format=json"), config(token = token)) %>%
      as.character() %>%
      fromJSON(asText = TRUE)
    my_team = unlist(my_roster$fantasy_content$team)
    get_player = function(player_id) {
      player_attr = function(x) my_team[paste0('roster.0.players.', player_id, '.player.', x)]
      team = toupper(as.character(player_attr('editorial_team_abbr')))
      list(
        league_team = as.character(my_team['name']),
        player_id = as.character(player_attr('player_id')),
        name = as.character(player_attr('name.full')),
        status = as.character(player_attr('status')),
        injury_note = as.character(player_attr('injury_note')),
        team = ifelse(team == 'JAX', 'JAC', team),
        pos = as.character(player_attr('eligible_positions.position')),
        bye_week = as.numeric(player_attr('bye_weeks.week')),
        selected_week = as.numeric(player_attr('selected_position.week')),
        selected_pos  = as.character(player_attr('selected_position.position'))
      )
    }
    rbindlist(lapply(0:(as.integer(my_team["roster.0.players.count"]) - 1), get_player))
  }
  get_teams <- function() {
    league = GET(paste0('http://fantasysports.yahooapis.com/fantasy/v2/league/', league_key, '/teams?format=json'),
                 config(token = token)) %>%
      as.character() %>%
      fromJSON(asText = TRUE) %>%
      unlist()
    get_team_info <- function(team_id) {
      team_attr <- function(x) league[paste0('fantasy_content.league.teams.', team_id, '.team.', x)]
      list(
        team_id = team_attr('team_id'),
        team_name = team_attr('name')
      )
    }
    rbindlist(lapply(0:(as.numeric(league['fantasy_content.league.teams.count']) - 1), get_team_info))
  }
  teams <- get_teams()
  dplyr::tbl_dt(rbindlist(lapply(teams$team_id, get_team)))
}


