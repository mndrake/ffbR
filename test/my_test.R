library(ffbR)
library(dplyr)

# get yahoo service token (https://developer.yahoo.com/apps/)
token <- get_token("yahoo.txt")

# get league key for your fantasy league
league_key <- get_league_key('960', token)

# get yahoo rosters for all teams in league
rosters <- get_rosters(league_key, token)

# get fantasy projects from fantasy pros
projections_fp <- get_fantasypros(week = 9)

combined <- left_join(rosters, projections_fp)

combined %>%
  filter(pos != 'DEF' & selected_pos != 'BN') %>%
  group_by(league_team) %>%
  summarize(pts = sum(custom_points, na.rm = TRUE))

combined %>% filter(league_team == 'Charlestown Chiefs') %>%
  select(name, status, injury_note, team, pos, bye_week, selected_pos, custom_points) %>%
  arrange(pos, desc(custom_points))

FA <-
  projections_fp %>%
  left_join(rosters) %>%
  filter(is.na(league_team))

FA %>% filter(pos == 'WR') %>% select(name, team, custom_points) %>% arrange(desc(custom_points))
FA %>% filter(pos == 'TE') %>% select(name, team, custom_points) %>% arrange(desc(custom_points))
FA %>% filter(pos == 'RB') %>% select(name, team, custom_points) %>% arrange(desc(custom_points))
