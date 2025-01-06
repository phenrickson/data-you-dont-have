add_gt_formatting = function(tbl, ...) {
  
  tbl |>
    gt::opt_row_striping(row_striping = F) |>
    gt::tab_options(table.font.size = 14,
                    ...)
}

standardize_start_date = function(data) {
  
  data |>
    mutate(start_date = lubridate::as_datetime(start_date), tz = "UTC") |>
    arrange(start_date)
  
}

add_game_info = function(data, game_info) {
  
  data |>
    join_team_divisions(games = game_info) |>
    prepare_fcs_teams() |>
    left_join(
      game_info |>
        add_game_outcomes() |>
        select(game_id, start_date, week, home_points, away_points, home_margin, home_win, total_points, completed)
    )
}

load_betting_lines = function(year, season_type = c('regular', 'postseason')) {
  
  map(
    season_type,
    ~ cfbfastR::cfbd_betting_lines(year = year,
                                   season_type = .x)
  ) |>
    list_rbind() |>
    as_tibble() |>
    standardize_start_date()
  
}

load_games = function(year, season_type = c("both"), ...) {
  
  cfbfastR::cfbd_game_info(year = year, season_type = season_type, ...) |>
    as_tibble() |>
    adjust_team_names() |>
    standardize_start_date()
  
}

prepare_games_for_model = function(data, estimates, season_week) {
  
  data |>
    prepare_game_info() |>
    add_game_weeks() |>
    prepare_fcs_teams() |>
    select(season,
           season_type,
           season_week,
           week,
           week_date,
           start_date,
           game_id,
           completed,
           neutral_site,
           home_id,
           away_id,
           home_team,
           away_team,
           home,
           away) |>
    join_team_estimates(estimates = estimates, season_week = season_week)
  
}

add_team_estimates = function(data, estimates) {
  
  data |>
    inner_join(
      estimates |>
        select(
          home_team = team,
          home_overall = overall,
          home_offense = offense,
          home_defense = defense,
          home_special = special
        ),
      by = c("home_team")
    ) |>
    inner_join(
      estimates |>
        select(
          away_team = team,
          away_overall = overall,
          away_offense = offense,
          away_defense = defense,
          away_special = special
        ),
      by = c("away_team")
    )
}

create_seeds = function(teams) {
  
  tibble(team = teams) |>
    mutate(seed = row_number())
}

create_initial_matchups <- function(teams) {
  round1_teams <- teams[teams$seed >= 5, ]
  matchups <- tibble(
    home_seed = round1_teams$seed[c(1, 2, 3, 4)],
    home_team = round1_teams$team[c(1, 2, 3, 4)],
    away_seed = round1_teams$seed[c(8, 7, 6, 5)],
    away_team = round1_teams$team[c(8, 7, 6, 5)],
    neutral_site = 0
  )
  
  return(matchups)
  
}

slice_estimates = function(data) {
  
  data |>
    group_by(team) |>
    slice_max(week_date, n =1) |>
    ungroup() |>
    select(
      season,
      week_date,
      team,
      overall = postgame_overall,
      offense = postgame_offense,
      defense = postgame_defense,
      special = postgame_special
    )
  
}

simulate_outcome = function(data, model, seed = 1999, ndraws = 4000, ...) {
  
  simulate_matchup = function(data, model, seed = 1999, ndraws = 4000, ...) {
    
    round_prediction <- function(x) {
      rounded <- round(x)
      ifelse(rounded == 0, ifelse(x > 0, 1, -1), rounded)
    }
    
    simulate_ot = function(data) {
      
      data |>
        mutate(.prediction = case_when(
          .prediction == 0 ~ sample(c(3, -3, 7, -7, 2, -2), size = 1, replace = T),
          TRUE ~ .prediction
        )) |>
        mutate(.prediction = round_prediction(.prediction))
    }
    
    set.seed(seed)
    sims =
      model |>
      tidybayes::predicted_draws(newdata = data, ndraws = ndraws,  seed = seed, ...) |>
      simulate_ot() |>
      select(any_of(".draw"), everything())
    
  }
  
  add_simulated_outcome = function(data) {
    
    data |>
      mutate(
        home_win = case_when(
          .prediction > 0 ~ "yes",
          .prediction == 0 ~ sample(c("yes", "no"), size = 1, replace = T),
          .prediction < 0 ~ "no"
        ),
        home_win = factor(home_win, levels = c("yes", "no"))
      ) |>
      mutate(
        winner = case_when(home_win == 'yes' ~ home_team, home_win == 'no' ~ away_team)
      )
  }
  
  add_matchup_outcome = function(data) {
    
    data |>
      mutate(winner_seed = case_when(home_win == 'yes' ~ home_seed, home_win == 'no' ~ away_seed))
  }
  
  data |>
    simulate_matchup(model = model, seed = seed, ndraws = ndraws, ...) |>
    ungroup() |>
    select(home_seed, home_team, away_seed, away_team, .draw, .prediction) |>
    add_simulated_outcome() |>
    add_matchup_outcome()
  
}

create_quarterfinal = function(byes, winners) {
  
  top_seeds = byes[order(byes$seed, decreasing = T),]
  
  tibble(
    home_seed = top_seeds$seed,
    home_team = top_seeds$team,
    away_seed = winners$seed,
    away_team = winners$team
  ) |>
    mutate(neutral_site = 1) |>
    mutate(matchup = factor(home_seed, levels = c(1, 4, 2, 3))) |>
    arrange(matchup) |>
    select(-matchup)
}

create_semifinal = function(winners) {
  
  semi_1 = winners[1:2,] |> arrange(seed)
  semi_2 = winners[3:4,] |> arrange(seed)
  
  
  matchup_1 =
    tibble(
      home_seed = semi_1[1,]$seed,
      home_team = semi_1[1,]$team,
      away_seed = semi_1[2,]$seed,
      away_team = semi_1[2,]$team
    )
  
  matchup_2 =
    tibble(
      home_seed = semi_2[1,]$seed,
      home_team = semi_2[1,]$team,
      away_seed = semi_2[2,]$seed,
      away_team = semi_2[2,]$team
    )
  
  bind_rows(matchup_1, matchup_2) |>
    mutate(neutral_site = 1)
  
}

create_championship = function(winners) {
  
  semis = winners |> arrange(seed)
  
  tibble(
    home_seed = semis[1,]$seed,
    home_team = semis[1,]$team,
    away_seed = semis[2,]$seed,
    away_team = semis[2,]$team
  ) |>
    mutate(neutral_site = 1)
  
}

simulate_playoff = function(playoff_teams, estimates, model) {
  
  playoff_seeds =
    playoff_teams |>
    create_seeds()
  
  round1_matchups =
    playoff_teams |>
    create_seeds() |>
    create_initial_matchups() |>
    add_team_estimates(estimates = estimates)
  
  round1 =
    round1_matchups |>
    simulate_outcome(model = model, ndraws = 1, seed = NULL)
  
  round1_winners =
    round1 |>
    select(team = winner,
           seed = winner_seed)
  
  round1_byes =
    playoff_seeds |>
    filter(seed <= 4)
  
  quarterfinal_matchups =
    create_quarterfinal(
      round1_byes,
      round1_winners
    ) |>
    add_team_estimates(estimates = estimates)
  
  quarterfinals =
    quarterfinal_matchups |>
    simulate_outcome(model = model, ndraws = 1, seed = NULL)
  
  quarterfinal_winners =
    quarterfinals |>
    select(team = winner, seed = winner_seed)
  
  semifinal_matchups =
    create_semifinal(
      quarterfinal_winners
    ) |>
    add_team_estimates(estimates = estimates)
  
  semifinals =
    semifinal_matchups |>
    simulate_outcome(model = model, ndraws = 1, seed = NULL)
  
  championship_matchup =
    semifinals |>
    select(team = winner, seed = winner_seed) |>
    create_championship() |>
    add_team_estimates(estimates = estimates)
  
  championship =
    championship_matchup |>
    simulate_outcome(model = model, ndraws = 1, seed = NULL)
  
  championship_winner =
    championship |>
    select(team = winner, seed = winner_seed)
  
  games =
    bind_rows(
      round1 |>
        mutate(round = 'round_1'),
      quarterfinals |>
        mutate(round = 'quarterfinal'),
      semifinals |>
        mutate(round = 'semifinal'),
      championship |>
        mutate(round = 'championship')
    ) |>
    select(round, everything()) |>
    group_by(round) |>
    mutate(round_game = row_number()) |>
    select(starts_with("round"), everything()) |>
    ungroup() |>
    mutate(round = factor(round, levels = c("round_1", "quarterfinal", "semifinal", "championship")))
  
  return(games)
  
}

simulate_playoff_after_round_1 =
  function(
    playoff_teams,
    round_1_teams,
    estimates,
    model) {
    
    playoff_seeds =
      playoff_teams |>
      create_seeds()
    
    round1_winners =
      playoff_seeds |>
      filter(team %in% round_1_teams) |>
      select(team, seed)
    
    round1_byes =
      playoff_seeds |>
      filter(seed <= 4)
    
    quarterfinal_matchups =
      create_quarterfinal(
        round1_byes,
        round1_winners
      ) |>
      add_team_estimates(estimates = estimates)
    
    quarterfinals =
      quarterfinal_matchups |>
      simulate_outcome(model = model, ndraws = 1, seed = NULL)
    
    quarterfinal_winners =
      quarterfinals |>
      select(team = winner, seed = winner_seed)
    
    semifinal_matchups =
      create_semifinal(
        quarterfinal_winners
      ) |>
      add_team_estimates(estimates = estimates)
    
    semifinals =
      semifinal_matchups |>
      simulate_outcome(model = model, ndraws = 1, seed = NULL)
    
    championship_matchup =
      semifinals |>
      select(team = winner, seed = winner_seed) |>
      create_championship() |>
      add_team_estimates(estimates = estimates)
    
    championship =
      championship_matchup |>
      simulate_outcome(model = model, ndraws = 1, seed = NULL)
    
    championship_winner =
      championship |>
      select(team = winner, seed = winner_seed)
    
    games =
      bind_rows(
        quarterfinals |>
          mutate(round = 'quarterfinal'),
        semifinals |>
          mutate(round = 'semifinal'),
        championship |>
          mutate(round = 'championship')
      ) |>
      select(round, everything()) |>
      group_by(round) |>
      mutate(round_game = row_number()) |>
      select(starts_with("round"), everything()) |>
      ungroup() |>
      mutate(round = factor(round, levels = c("round_1", "quarterfinal", "semifinal", "championship")))
    
    return(games)
    
  }

longer_playoffs = function(data) {
  
  home =
    data |>
    mutate(
      round,
      round_game,
      .draw,
      team = home_team,
      opponent = away_team,
      win = case_when(home_win == 'yes' ~ 'yes', TRUE ~ 'no'),
      .keep = 'none'
    ) |>
    ungroup()
  
  away =
    data |>
    mutate(
      round,
      round_game,
      .draw,
      team = away_team,
      opponent = home_team,
      win = case_when(home_win == 'yes' ~ 'no', TRUE ~ 'yes'),
      .keep = 'none'
    ) |>
    ungroup()
  
  bind_rows(home, away)
  
}

summarize_wins_by_round = function(data) {
  
  data |>
    group_by(round, winner) |>
    count() |>
    ungroup()
}

add_playoff_game_number = function(data, start) {
  
  data |>
    mutate(playoff_game_number =
             case_when(
               round == 'round_1' & round_game == 4 ~ 7.5,
               round == 'round_1' & round_game == 3 ~ 1.5,
               round == 'round_1' & round_game == 2 ~ 3.5,
               round == 'round_1' & round_game == 1 ~ 5.5,
               round == 'quarterfinal' & round_game == 1 ~ 8,
               round == 'quarterfinal' & round_game == 4 ~ 4,
               round == 'quarterfinal' & round_game == 3 ~ 2,
               round == 'quarterfinal' & round_game == 2 ~ 6,
               round == 'semifinal' & round_game == 1 ~ 6.75,
               round == 'semifinal' & round_game == 2 ~ 2.75,
               round == 'championship' ~ 4.25
             )
    )
}

summarize_wins_by_team = function(data) {
  
  data |>
    longer_playoffs() |>
    group_by(round, round_game, team) |>
    summarize(win = sum(win=='yes'), .groups = 'drop') |>
    group_by(round, round_game)  |>
    mutate(prop = win / sum(win)) |>
    ungroup()
  
  
}

gt_playoff_probs = function(data, ratings) {
  
  gt_playoff_probs_table = function(data) {
    
    data |>
      gt::gt() |>
      gt::cols_hide(columns = c("score", "special", "season")) |>
      gt::fmt_number(columns = everything(), decimals = 3) |>
      gtExtras::gt_theme_espn() |>
      gt::opt_row_striping(row_striping = F) |>
      gt::cols_label(
        "round_1" ~ html("Round 1"),
        "quarterfinal" ~ html("Quarterfinal"),
        "semifinal" ~ html("Semi-Final"),
        "championship" ~ html("Championship")
      ) |>
      gt::cols_width(
        logo ~ px(75),
        team ~ px(100),
        offense ~ px(90),
        defense ~ px(90),
        round_1 ~ px(125),
        quarterfinal ~ px(125),
        semifinal ~ px(125),
        championship ~ px(125)
      ) |>
      gt::cols_align(columns = -c("team"), align = "center") |>
      gt::data_color(columns = c("round_1", "quarterfinal", "semifinal", "championship"),
                     domain = c(0, .9),
                     na_color = "deepskyblue1",
                     palette = c("white", "deepskyblue1")) |>
      cfbplotR::gt_fmt_cfb_logo("logo") |>
      gt::tab_spanner(
        columns = c("round_1", "quarterfinal", "semifinal", "championship"),
        label = "win probability"
      ) |>
      gt::tab_spanner(
        columns = c("offense", "defense"),
        label = "team rating"
      ) |>
      gt::data_color(
        columns = c("offense", "defense", "special"),
        palette = c("orange", "white", "navy"),
        domain = c(-.25, .4)
      ) |>
      gt::tab_style(
        style = cell_borders(
          sides = "left",
          color = "white"
        ),
        locations = cells_body(
          columns = c("round_1")
        )
      ) |>
      gt::sub_values(
        values = "1",
        replacement = "✓"
      ) |>
      gtExtras::gt_theme_nytimes()
    
    
  }
  
  n_sims = max(data$.draw)
  
  data |>
    summarize_wins_by_round() |>
    pivot_wider(names_from = c("round"), values_from = c("n"), values_fill = 0) |>
    mutate(
      across(any_of(c("round_1", "quarterfinal", "semifinal", "championship")), ~ case_when(.x == 0 ~ n_sims,
                                                                                           TRUE ~ .x))) |>
    mutate(across(where(is.numeric), ~ .x / n_sims)) |>
    select(logo = winner, team = winner, round_1, quarterfinal, semifinal, championship) |>
    arrange(desc(championship)) |>
    inner_join(
      ratings
    ) |>
    select(logo, team, score, offense, defense, special, everything()) |>
    gt_playoff_probs_table()
  
}

plot_matchups = function(data) {
  
  data |>
    group_by(round, round_game, home_team, away_team, winner) |>
    count() |>
    mutate(matchup = paste(home_team, away_team, sep = "-")) |>
    mutate(round = factor(round, levels = c("round_1", "quarterfinal", "semifinal", 'championship'))) |>
    group_by(matchup, round_game) |>
    mutate(games = sum(n), prop = round(n / sum(n), 2)) |>
    ungroup() |>
    ggplot(aes(x=n, fill = winner, y=reorder(matchup, games)))+
    geom_col()+
    cfbplotR::scale_fill_cfb()+
#    facet_wrap(paste(round, round_game) ~., scales = "free")+
    labs(y = "", x = "sims")+
    theme_light()+
    geom_text(aes(label = prop), color = 'white', position = position_stack(vjust = 0.5), size = 2.5)+
    theme(panel.grid.minor = element_blank())
}

plot_bracket = function(data) {
  
  plot_data =
    data |>
    add_playoff_game_number() |>
    group_by(playoff_game_number) |>
    mutate(rank = rank(prop, ties.method = 'random') / 5) |>
    mutate(prop_label = sprintf("%.2f", prop)) |>
    mutate(playoff_game_number_team = playoff_game_number + rank) |>
    mutate(round = factor(round, levels = c("round_1", "quarterfinal", "semifinal", 'championship')))
  
  bracket =
    bracket =
    plot_data |>
    ggplot(aes(x=round, y = playoff_game_number_team, label = paste(team, prop_label), color = team))+
    geom_text(hjust = 1, position = position_nudge(x = 0.25))+
    theme_minimal()+
    theme(panel.grid = element_blank())+
    cfbplotR::scale_color_cfb(alt_colors = c("Clemson"))+
    labs(x = "", y ="")+
    coord_cartesian(ylim = c(1, 9))+
    theme(axis.text.y = element_blank())
  
  bracket +
    geom_step(
      data = plot_data,
      aes(color = team, group = team, linewidth = prop),
      position = position_nudge(x=-0.5),
      direction = "hv",
      linejoin = 'mitre',
      alpha = 0.25
    )+
    geom_segment(
      data = plot_data |>
        filter(round == 'championship'),
      aes(x = 3.5,, xend = 4.4, linewidth = prop),
      alpha = 0.25
    )+
    guides(linewidth = 'none')+
    scale_linewidth(range = c(0, 8))
  
}

prepare_spread_predictions = function(data) {
  
  data |>
    mutate(
      my_prediction = case_when(
        pred_margin > 0 ~ paste(home_team, pred_margin, sep = " by "),
        pred_margin < 0 ~ paste(away_team, -1 * pred_margin, sep = " by "),
        pred_margin == 0 & home_prob > 0.500 ~ paste(home_team, 0.5, sep = " by "),
        pred_margin == 0 & home_prob < 0.500 ~ paste(away_team, 0.5, sep = " by ")
      ),
      actual = case_when(
        home_margin > 0 ~ paste(home_team, home_margin, sep = " by "),
        home_margin < 0 ~ paste(away_team, -1 *home_margin, sep = " by ")
      ),
      spread_prediction = case_when(
        spread < 0 ~ paste(home_team, -1* spread, sep = " by "),
        spread > 0 ~ paste(away_team, spread, sep = " by ")
      )
    ) |>
    select(season,
           week,
           home_team,
           away_team,
           actual,
           home_margin,
           my_prediction,
           my_margin = pred_margin,
           provider,
           margin = spread_margin,
           prediction = spread_prediction
    ) |>
    pivot_wider(names_from = c("provider"),
                values_from = c("margin", "prediction"))
}

spread_predictions_tbl = function(data) {
  
  data |>
    gt_tbl() |>
    gt::cols_align(
      #    columns = -c(home_team, away_team),
      align = "center"
    ) |>
    gt::cols_hide(
      columns = ends_with("_NA")
    ) |>
    gt::cols_merge(
      columns = c("home_team", "away_team"),
      pattern = "{1} vs {2}",
    ) |>
    gt::cols_width(
      season ~ px(75),
      week ~ px(75)
    ) |>
    gt::cols_label(
      home_team = "Game",
      season = "Season",
      actual = "Result",
      week = "Week",
      my_prediction = "Prediction",
      prediction_DraftKings = "DraftKings",
      prediction_Bovada = "Bovada",
      `prediction_ESPN Bet` = "ESPN Bet"
    ) |>
    gt::cols_hide(
      columns = c("my_margin", "home_margin", "margin_DraftKings", "margin_Bovada", "margin_ESPN Bet")
    ) |>
    gt_pred_color(
      columns = "my_margin",
      target_columns = "my_prediction"
    ) |>
    gt_pred_color(
      columns = "margin_DraftKings",
      target_columns = "prediction_DraftKings"
    ) |>
    gt_pred_color(
      columns = "margin_Bovada",
      target_columns = "prediction_Bovada"
    ) |>
    gt_pred_color(
      columns = "margin_ESPN Bet",
      target_columns = "prediction_ESPN Bet"
    ) |>
    gt_pred_color(
      columns = "home_margin",
      target_columns = "actual"
    ) |>
    gt::opt_interactive(
      use_filters = T,
      use_compact_mode = T,
      page_size_default = 15
    )
}

plot_vs_betting_lines = function(data, lim = 60) {
  
  data |>
    ggplot(aes(x = -pred_margin, y = spread, label = paste(home_team, away_team, sep = " vs "))) +
    geom_point(alpha = 0.5) +
    geom_text(vjust = -1, size = 1.5, check_overlap = T) +
    geom_abline() +
    theme_cfb() +
    xlab("Estimated Spread") +
    ylab("Vegas Spread") +
    ggpubr::stat_cor(aes(label = ..r.label..)) +
    facet_wrap(week ~ .) +
    #facet_wrap(paste(season, paste("Week", week)) ~ .)+
    coord_cartesian(xlim = c(-lim, lim),
                    ylim = c(-lim, lim))
}

betting_lines_tbl = function(data) {
  
  data |>
    gt_tbl() |>
    gt::cols_align(
      align = "center"
    ) |>
    gt::cols_label(
      season = "Season",
      week = "Week",
      home_team = "Home",
      away_team = "Away",
    ) |>
    gt::data_color(
      method = "numeric",
      columns = c("Actual", "Prediction", "Bovada", "DraftKings", "ESPN Bet"),
      domain = c(-80, 80),
      palette = c("orange", "white", "dodgerblue3"),
      na_color = "white"
    ) |>
    gt::opt_interactive(
      use_filters = T,
      use_compact_mode = T,
      page_size_default = 25
    )
}

gt_quarterfinal_probs = function(data, ratings) {
  
  gt_playoff_probs_table = function(data) {
    
    data |>
      gt::gt() |>
      gt::cols_hide(columns = c("score", "special", "season")) |>
      gt::fmt_number(columns = everything(), decimals = 3) |>
      gtExtras::gt_theme_espn() |>
      gt::opt_row_striping(row_striping = F) |>
      gt::cols_label(
        "round_1" ~ html("Round 1"),
        "quarterfinal" ~ html("Quarterfinal"),
        "semifinal" ~ html("Semi-Final"),
        "championship" ~ html("Championship")
      ) |>
      gt::cols_width(
        logo ~ px(75),
        team ~ px(100),
        offense ~ px(90),
        defense ~ px(90),
        round_1 ~ px(125),
        quarterfinal ~ px(125),
        semifinal ~ px(125),
        championship ~ px(125)
      ) |>
      gt::cols_align(columns = -c("team"), align = "center") |>
      gt::data_color(columns = c("round_1", "quarterfinal", "semifinal", "championship"),
                     domain = c(0, .9),
                     na_color = "deepskyblue1",
                     palette = c("white", "deepskyblue1")) |>
      cfbplotR::gt_fmt_cfb_logo("logo") |>
      gt::tab_spanner(
        columns = c("round_1", "quarterfinal", "semifinal", "championship"),
        label = "win probability"
      ) |>
      gt::tab_spanner(
        columns = c("offense", "defense"),
        label = "team rating"
      ) |>
      gt::data_color(
        columns = c("offense", "defense", "special"),
        palette = c("orange", "white", "navy"),
        domain = c(-.25, .4)
      ) |>
      gt::tab_style(
        style = cell_borders(
          sides = "left",
          color = "white"
        ),
        locations = cells_body(
          columns = c("round_1")
        )
      ) |>
      gt::sub_values(
        values = "1",
        replacement = "✓"
      ) |>
      gtExtras::gt_theme_nytimes()
    
    
  }
  
  n_sims = max(data$.draw)
  
  data |>
    summarize_wins_by_round() |>
    pivot_wider(names_from = c("round"), values_from = c("n"), values_fill = 0) |>
    mutate(across(where(is.numeric), ~ .x / n_sims)) |>
    mutate(round_1 = 1) |>
    select(logo = winner, team = winner, round_1, quarterfinal, semifinal, championship) |>
    arrange(desc(championship)) |>
    inner_join(
      ratings
    ) |>
    select(logo, team, score, offense, defense, special, everything()) |>
    gt_playoff_probs_table()
  
}

simulate_playoff_after_quarterfinal =
  function(
    playoff_teams,
    round_1_teams,
    quarterfinal_teams,
    estimates,
    model) {
    
    playoff_seeds =
      playoff_teams |>
      create_seeds()
    
    round1_winners =
      playoff_seeds |>
      filter(team %in% round_1_teams) |>
      select(team, seed)
    
    round1_byes =
      playoff_seeds |>
      filter(seed <= 4)
  
    quarterfinal_winners =
      playoff_seeds |>
      filter(team %in% quarterfinal_teams) |>
      select(team, seed) |>
      reorder_seeds()
    
    semifinal_matchups =
      create_semifinal(
        quarterfinal_winners
      ) |>
      add_team_estimates(estimates = estimates)
    
    semifinals =
      semifinal_matchups |>
      simulate_outcome(model = model, ndraws = 1, seed = NULL)
    
    championship_matchup =
      semifinals |>
      select(team = winner, seed = winner_seed) |>
      create_championship() |>
      add_team_estimates(estimates = estimates)
    
    championship =
      championship_matchup |>
      simulate_outcome(model = model, ndraws = 1, seed = NULL)
    
    championship_winner =
      championship |>
      select(team = winner, seed = winner_seed)
    
    games =
      bind_rows(
        semifinals |>
          mutate(round = 'semifinal'),
        championship |>
          mutate(round = 'championship')
      ) |>
      select(round, everything()) |>
      group_by(round) |>
      mutate(round_game = row_number()) |>
      select(starts_with("round"), everything()) |>
      ungroup() |>
      mutate(round = factor(round, levels = c("round_1", "quarterfinal", "semifinal", "championship")))
    
    return(games)
    
  }


reorder_seeds <- function(df) {

  # Identify the rows with the lowest and highest seeds
  lowest_index <- which.min(df$seed)
  highest_index <- which.max(df$seed)
  
  # Extract the rows for the lowest and highest seeds
  lowest <- df[lowest_index, ]
  highest <- df[highest_index, ]
  
  # Create the reordered dataframe
  remaining <- df[-c(lowest_index, highest_index), ] # Exclude the lowest and highest rows
  reordered <- rbind(lowest, highest, remaining)
  
  return(reordered)
}

gt_semifinal_probs = function(data, ratings) {
  
  gt_playoff_probs_table = function(data) {
    
    data |>
      gt::gt() |>
      gt::cols_hide(columns = c("score", "special", "season")) |>
      gt::fmt_number(columns = everything(), decimals = 3) |>
      gtExtras::gt_theme_espn() |>
      gt::opt_row_striping(row_striping = F) |>
      gt::cols_label(
        "round_1" ~ html("Round 1"),
        "quarterfinal" ~ html("Quarterfinal"),
        "semifinal" ~ html("Semi-Final"),
        "championship" ~ html("Championship")
      ) |>
      gt::cols_width(
        logo ~ px(75),
        team ~ px(100),
        offense ~ px(90),
        defense ~ px(90),
        round_1 ~ px(125),
        quarterfinal ~ px(125),
        semifinal ~ px(125),
        championship ~ px(125)
      ) |>
      gt::cols_align(columns = -c("team"), align = "center") |>
      gt::data_color(columns = c("round_1", "quarterfinal", "semifinal", "championship"),
                     domain = c(0, .9),
                     na_color = "deepskyblue1",
                     palette = c("white", "deepskyblue1")) |>
      cfbplotR::gt_fmt_cfb_logo("logo") |>
      gt::tab_spanner(
        columns = c("round_1", "quarterfinal", "semifinal", "championship"),
        label = "win probability"
      ) |>
      gt::tab_spanner(
        columns = c("offense", "defense"),
        label = "team rating"
      ) |>
      gt::data_color(
        columns = c("offense", "defense", "special"),
        palette = c("orange", "white", "navy"),
        domain = c(-.25, .4)
      ) |>
      gt::tab_style(
        style = cell_borders(
          sides = "left",
          color = "white"
        ),
        locations = cells_body(
          columns = c("round_1")
        )
      ) |>
      gt::sub_values(
        values = "1",
        replacement = "✓"
      ) |>
      gtExtras::gt_theme_nytimes()
    
    
  }
  
  n_sims = max(data$.draw)
  
  data |>
    summarize_wins_by_round() |>
    pivot_wider(names_from = c("round"), values_from = c("n"), values_fill = 0) |>
    mutate(across(where(is.numeric), ~ .x / n_sims)) |>
    mutate(round_1 = 1,
           quarterfinal = 1) |>
    select(logo = winner, team = winner, round_1, quarterfinal, semifinal, championship) |>
    arrange(desc(championship)) |>
    inner_join(
      ratings
    ) |>
    select(logo, team, score, offense, defense, special, everything()) |>
    gt_playoff_probs_table()
  
}
