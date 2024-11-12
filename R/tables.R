gt_tbl = function(data,
                  ...) {
  
  data |>
    gt::gt() |>
    gt::sub_missing() |>
    gtExtras::gt_theme_espn() |>
    gt::tab_options(
      container.overflow.y = T,
      ...
    )
}

add_gt_formatting = function(tbl, ...) {
  
  tbl |>
    gt::opt_row_striping(row_striping = F) |>
    gt::tab_options(table.font.size = 14,
                    ...)
}

round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

plays_points_tbl <- function(plays) {
  plays |>
    rename(
      ep_pre = expected_points_pre,
      ep_post = expected_points_post,
      ep_added = predicted_points_added,
    ) |>
    select(season, game_id, offense, defense, period, yards_to_goal, down, distance, play_text, ep_pre, ep_post, ep_added) |>
    mutate_if(is.numeric, round, 3) |>
    gt_tbl() |>
    gt::cols_align(
      columns = c("period", "down", "distance", "yards_to_goal", "ep_pre", "ep_post", "ep_added"),
      align = "center"
    ) |>
    gt::data_color(
      columns = c(
        "ep_pre",
        "ep_post",
        "ep_added"
      ),
      method = "numeric",
      domain = c(-10, 10),
      palette = c("orange", "white", "dodgerblue"),
      na_color = "white"
    ) |>
    gt::tab_options(
      data_row.padding = px(20),
      table.font.size = 12
    ) |>
    gt::cols_label(
      yards_to_goal = "ytg"
    )
}

top_plays_by_game <- function(plays, var = predicted_points_added, n = 10) {
  plays |>
    group_by(game_id) |>
    slice_max(abs({{ var }}), n = n) |>
    group_by(season, game_id)
}

gt_correct_color = function(tbl) { 
  
  tbl |>
    tab_style(
      style = cell_fill(color = "deepskyblue"),
      locations = cells_body(
        columns = "actual",
        rows = correct == "yes"
      )
    )
}

# make table of game predictions
game_predictions_tbl <- function(data) {
  data |>
    gt_tbl() |>
    gt::fmt_number(
      columns = c(game_interest, game_quality),
      decimals = 0
    ) |>
    gt::fmt_datetime(
      columns = c(start_date),
      date_style = "MMMEd",
      time_style = "h_m_p"
    ) |>
    gt::cols_align(
      columns = everything(),
      align = "center"
    ) |>
    gt::cols_hide(
      columns = c(game_id, season_type, start_date, correct)
    ) |>
    gt::fmt_number(
      columns = c(home_prob),
      decimals = 3
    ) |>
    gt::cols_width(
      season ~ px(75),
      week ~ px(75),
      game_quality ~ px(85),
      game_interest ~ px(85),
      home_prob ~ px(85),
      start_date ~ px(100),
      prediction ~ px(150),
      actual ~ px(150)
    ) |>
    gt::cols_label(
      season = "Season",
      week = "Week",
      start_date = "Game Time",
      home_team = "Home",
      away_team = "Away",
      game_quality = "Quality",
      game_interest = "Interest",
      home_prob = "Pr(Home Win)",
      prediction = "Prediction",
      actual = "Result"
    ) |>
    gt::data_color(
      columns = c("game_quality", "game_interest"),
      domain = c(0, 100),
      palette = c("orange", "white", "dodgerblue3")
    ) |>
    gt::data_color(
      columns = c("home_prob"),
      domain = c(0, 1),
      palette = c("white", "deepskyblue1")
    ) |>
    gt::opt_interactive(
      use_filters = T,
      page_size_default = 15,
      use_compact_mode = T,
      use_highlight = T
    ) |>
    gt_correct_color()
}

prepare_team_category_estimates = function(data) {
  
  data |>
    add_team_ranks(groups = c("season", "season_week", "play_category", "type")) |>
    pivot_wider(names_from = c("play_category", "type"),
                values_from = c("estimate", "rank")) |>
    select(team, season, season_week, contains("pass_offense"), contains("rush_offense"), contains("pass_defense"), contains("rush_defense")) |>
    arrange(desc(estimate_pass_offense)) |>
    add_season_week() |>
    mutate(logo = team) |>
    select(season, season_week, week, logo, team, everything()) |>
    arrange(desc(estimate_rush_offense))
}

team_category_estimates_tbl = function(data) {
  
  data |>
    gt_tbl() |>
    gt::fmt_number(
      contains("estimate"),
      decimals = 3
    ) |>
    gt::cols_merge(
      columns = contains("pass_offense"),
      pattern = "{1}<< ({2})>>"
    ) |>
    gt::cols_merge(
      columns = contains("rush_offense"),
      pattern = "{1}<< ({2})>>"
    ) |>
    gt::cols_merge(
      columns = contains("pass_defense"),
      pattern = "{1}<< ({2})>>"
    ) |>
    gt::cols_merge(
      columns = contains("rush_defense"),
      pattern = "{1}<< ({2})>>"
    ) |>
    gt::cols_label(
      season = "Season",
      week = "Week",
      team = "Team",
      estimate_pass_offense = "Pass Offense",
      estimate_rush_offense = "Run Offense",
      estimate_pass_defense = "Pass Defense",
      estimate_rush_defense = "Run Defense"
    )  |>
    gt::cols_hide(
      c(season_week, week)
    ) |>
    gt::cols_width(
      season ~ px(75),
      week ~ px(75)
    ) |>
    gt::cols_align(
      c(contains("offense"), contains("defense"), "season", "week"),
      align = "center"
    ) |>
    gt_est_color(columns = c(contains("estimate")),
                 domain = c(-0.75, 0.75)) |>
    gt::opt_interactive(
      page_size_default = 15,
      use_filters = T
    ) |>
    cfbplotR::gt_fmt_cfb_logo(columns = "logo") |>
    gt::cols_label(
      logo = "Logo"
    ) |>
    gt::cols_width(
      logo ~ px(75)
    ) |>
    gt::cols_align(
      align = "center",
      columns = "logo"
    )
}

team_efficiency_category_tbl = function(data, teams, groups = c("season", "play_category", "metric", "type"), domain = c(-.75, .75)) {
  
  data |>
    filter(play_category != 'special') |>
    add_team_ranks(groups = groups) |>
    unite(type, c(play_category, type)) |>
    select(-any_of("intercept")) |>
    pivot_wider(names_from = c("type"),
                values_from = c("estimate", "rank")) |>
    filter(team == teams) |>
    efficiency_category_tbl(domain = domain)
}

efficiency_category_tbl = function(data, domain = c(-.75, .75)) {
  
  df = 
    data |>
    select(season,
           any_of(c("week")),
           team,
           estimate_pass_offense,
           estimate_rush_offense,
           estimate_pass_defense,
           estimate_rush_defense,
           contains("rank"))
  
  tab = 
    df |>
    gt_tbl() |>
    gt_est_color(
      domain = domain
    ) |>
    # gt::tab_spanner(
    #   label = "efficiency",
    #   columns = contains("estimate")
    # ) |>
    gt::tab_spanner(
      label = "offense efficiency",
      columns = contains("offense")
    ) |>
    gt::tab_spanner(
      label = "defense efficiency",
      columns = contains("defense")
    ) |>
    gt::fmt_number(
      columns = contains("estimate"),
      decimals = 3
    ) |>
    gt::cols_merge(
      contains("pass_offense"),
      pattern = "<<{1} ({2})>>"
    ) |>
    gt::cols_merge(
      contains("pass_defense"),
      pattern = "<<{1} ({2})>>"
    ) |>
    gt::cols_merge(
      contains("rush_offense"),
      pattern = "<<{1} ({2})>>"
    ) |>
    gt::cols_merge(
      contains("rush_defense"),
      pattern = "<<{1} ({2})>>"
    ) |>
    gt::cols_label(
      estimate_rush_offense = "rush",
      estimate_pass_offense = "pass",
      estimate_rush_defense = "rush",
      estimate_pass_defense = "pass"
      # rank_rush_offense = "Rush Offense",
      # rank_pass_offense = "Pass Offense",
      # rank_rush_defense = "Rush Defense",
      # rank_pass_defense = "Pass Defense",
    ) |>
    gt::cols_align(
      columns = c(contains("estimate"), contains("rank")),
      align = "center"
    ) |>
    gt::cols_align(
      columns = any_of(c("season")),
      align = "center"
    ) |>
    gt::cols_hide(columns = c("week"))
}

team_efficiency_overall_tbl = function(data, groups = c("season", "week", "type", "metric"), teams, overall_domain = c(-35, 35), est_domain = c(-.5, .5), table = T) {
  
  scores =
    data |>
    rename(overall = score) 
  
  ranks = 
    scores |>
    pivot_longer(
      cols = c(overall, offense, defense, special),
      names_to = c("type"),
      values_to = c("estimate")
    ) |>
    add_season_week() |>
    add_team_ranks(groups = groups)
  
  ranks_wider = 
    ranks |>
    pivot_wider(
      names_from = c("type"),
      values_from = c("estimate", "rank"),
      names_sep = "_"
    )
  
  if (table == T) {
    ranks_wider |>
      filter(team %in% teams) |>
      efficiency_overall_tbl(overall_domain = overall_domain, est_domain = est_domain)
  }
  
  else {
    ranks_wider
  }
  
}

efficiency_overall_tbl = function(data, overall_domain = c(-30, 40), est_domain = c(-.5, .5)) {
  
  data |>
    select(season,
           any_of(c("week")),
           team,
           estimate_overall,
           estimate_offense,
           estimate_defense,
           rank_overall,
           rank_offense,
           rank_defense
    ) |>
    gt_tbl() |>
    gt::fmt_number(
      columns = contains("estimate"),
      decimals = 3
    ) |>
    gt::fmt_number(
      columns = contains("estimate_overall"),
      decimals = 2
    ) |>
    gt::cols_merge(
      contains("_overall"),
      pattern = "<<{1} ({2})>>"
    ) |>
    gt::cols_merge(
      contains("_offense"),
      pattern = "<<{1} ({2})>>"
    ) |>
    gt::cols_merge(
      contains("_defense"),
      pattern = "<<{1} ({2})>>"
    ) |>
    gt::cols_label(
      estimate_overall = "overall",
      estimate_offense = "offense",
      estimate_defense  = "defense"
    ) |>
    gt::cols_align(
      align = c("center"),
      columns = -c(team)
    ) |>
    gt_est_color(domain = est_domain,
                 columns = c("estimate_offense", "estimate_defense")) |>
    gt_est_color(columns = "estimate_overall",
                 domain = overall_domain) |>
    gt::cols_hide(columns = "week")
  
}

efficiency_tbl = function(data, with_estimates = F, with_ranks = T) {
  
  tab =
    data |>
    # relocate(starts_with("estimate_"), .after = last_col()) |>
    gt_tbl() |>
    gt_est_color() |>
    gt::tab_spanner(
      label = "efficiency",
      columns = starts_with("estimate_")
    ) |>
    gt::cols_align(
      align = c("center"),
      columns = -c(team)
    ) |>
    gt::fmt_number(
      columns = starts_with("rank_"),
      decimals = 0
    ) |>
    gt::fmt_number(
      columns = c(starts_with("estimate")),
      decimals = 3
    )
  
  if (with_estimates == T) {
    
    tab =
      tab |>
      gt::cols_label(
        estimate_overall = "overall",
        estimate_offense = "offense",
        estimate_defense = "defense"
      )
  }
  
  if (with_ranks == T) {
    
    tab =
      tab |>
      gt_rank_color() |>
      gt::tab_spanner(
        label = "rank",
        columns = starts_with("rank_")
      ) |>
      gt::cols_label(
        rank_overall = "overall",
        rank_offense = "offense",
        rank_defense = "defense"
      )
  }
  
  tab
  
}

