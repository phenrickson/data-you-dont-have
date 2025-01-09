my_facet_theme = function(size = 12, ...) {

  theme(strip.background.y = element_blank(),
  strip.text.y = element_text(colour = 'grey20', size =size),
  ...)
}

find_team_season_score = function(data) {

  data |>
  group_by(season, team) |>
  add_season_week() |>
  slice_max(week, n =1) |>
  ungroup()

}

normalize_estimate = function(var, scale = 100) {

  scaled = (var - min(var)) / (max(var) - min(var))

  scaled * scale

}

plot_ranking = function(plot, alpha = 0.5, hjust = 1.4, ranking, groups = c("season", "season_week", "type", "metric")) {

  rank_data =
  plot$data |>
  add_team_ranks(groups = groups) |>
  filter(rank %in% c(ranking)) |>
  mutate(team = case_when(rank <= 75 ~ paste('top', rank),
  rank > 75 ~ paste('bottom', 135 - rank)))

  plot +
  geom_line(
    data =
    rank_data,
    aes(group = team),
    linetype = 'dashed',
    alpha = alpha
  ) +
  geom_text(
    data =
    rank_data |>
    filter(season_week == min(season_week)),
    aes(y = estimate,
      label = team
    ),
    hjust = hjust,
    size = 2
  )+
  coord_cartesian(clip = "off")

}

plot_team_score_by_season = function(data, team, ranking = c(25), hjust = 1.4) {

  p =
  data |>
  find_team_season_score() |>
  select(season, season_type, season_week, team, overall = score) |>
  pivot_longer(cols = c(overall),
  names_to = c("type"),
  values_to = c("estimate")) |>
  add_team_ranks() |>
  plot_team_efficiency(teams = team, title = F)+
  ylab("")+
  xlab("Season")+
  my_facet_theme()+
  ylab("")+
  labs(title = paste("Team Rating by Season", team, sep = " - "),
  subtitle = stringr::str_wrap(
    paste("Estimates based on opponent adjusted team efficiency and predicted points per play. Distribution in grey shows all FBS teams. Highlighted line shows", paste0(team, "'s"), "rating among all FBS teams."), 120),
    x = "Season",
    y= "Rating")

    p |>
    plot_ranking(ranking = ranking, hjust = hjust)
  }

  plot_team_estimates_by_season = function(data, team, ranking = 25, vjust = -0.25, hjust = 2.4) {

    p =
    data |>
    group_by(season, team) |>
    slice_max(season_week, n =1) |>
    rename(overall = score) |>
    pivot_longer(cols = c(offense, defense, special),
    names_to = c("type"),
    values_to = c("estimate")) |>
    add_team_ranks() |>
    mutate(type = factor(type, levels = c("offense", "defense", "special"))) |>
    # filter(type != 'special') |>
    plot_team_efficiency(team = team, vjust = vjust)+
    ylab("Net Points per Play")+
    xlab("Season")+
    my_facet_theme()

    p |>
    plot_ranking(ranking = ranking, hjust = hjust)
  }

  plot_team_category_by_season = function(data, team, vjust = -0.25) {

    p =
    data |>
    filter(play_category != 'special') |>
    select(season, season_week, week_date, week, play_category, metric, type, team, estimate) |>
    group_by(season, team, play_category, type) |>
    slice_max(season_week, n = 1) |>
    ungroup() |>
    add_season_week() |>
    add_team_ranks(groups = c("season", "season_week", "play_category", "metric", "type")) |>
    plot_team_efficiency(x = 'season', teams = team, point = T, label = T, title = T, vjust = vjust)+
    ggh4x::facet_nested(type + play_category ~.) +
    my_facet_theme()+
    labs(title = paste("Team Efficiency by Play Type", team, sep = " - "),
    subtitle = stringr::str_wrap(
      paste("Team rating indicates expected margin when playing an average FBS opponent. Estimates based on opponent adjusted team efficiency and predicted points per play. Distribution in grey shows all FBS teams. Highlighted line shows", paste0(team, "'s"), "rating among all FBS teams."), 120),
      x = 'Season',
      y = 'Net Points per Play'
    )

    p |>
    plot_ranking(groups = c("season", "season_week", "play_category", "metric", "type"),
    ranking = 25,
    hjust = 2.4)

  }

  plot_team_by_season = function(data, team, hjust = 2.5, heights = c(1, 2.5), ...) {

    a =
    team_scores |>
    plot_team_score_by_season(team = team, hjust = hjust)+
    labs(x = NULL,
      title = paste("Team Efficiency by Season", team, sep = " - ")
    )

    b =
    team_scores |>
    plot_team_estimates_by_season(team = team, hjust = hjust, ...)+
    labs(title = NULL,
      subtitle = NULL)

      a / b +
      plot_layout(heights = heights)
    }

    add_gt_formatting = function(tab, table.font.size = 14) {

      tab |>
      gt::tab_options(table.font.size = table.font.size) |>
      gt::opt_row_striping(row_striping = F)
    }

    set_gt_width = function(tab) {

      tab |>
      gt::cols_width(
        season ~ px(75),
        team ~ px(100)
      )
    }

    plot_team_score_by_week = function(data, team, ranking = c(10, 25, 50)) {

      p =
      data |>
      rename(overall = score) |>
      pivot_longer(
        cols = c(overall, offense, defense, special),
        names_to = c("type"),
        values_to = c("estimate")
      ) |>
      filter(type == 'overall') |>
      add_season_week() |>
      add_team_ranks(groups = c("season", "week", "type", "metric")) |>
      mutate(type = factor(type, levels = c("overall", "offense", "defense", "special"))) |>
      plot_team_efficiency(x = 'week', teams = team, label = F, point = F, line = T)+
      facet_grid(type ~ season, scales = "free_y")+
      scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)]})+
      xlab("Season Week")+
      my_facet_theme()

      p |>
      plot_ranking(ranking = ranking)+
      ylab("Rating")

    }

add_gt_formatting = function(tbl, ...) {

  tbl |>
    gt::opt_row_striping(row_striping = F) |>
    gt::tab_options(table.font.size = 14,
                    ...)
}

join_team_info <- function(data, teams = team_info) {
  data |>
    inner_join(
      teams |>
        adjust_team_names(cols = "school") |>
        select(
          team = school,
          conference,
          abbreviation
        ),
      by = join_by(team)
    )
}

team_scores_tbl = function(data) {

  data |>
    mutate(logo = team) |>
    select(season, season_type, season_week, week, rank, logo, team, score, diff, offense, defense, special) |>
    gt_tbl() |>
    gt::cols_hide(
      columns = c(season_type, season_week, week, special)
    ) |>
    gt::fmt_number(
      columns = c(score),
      decimals = 2
    ) |>
    gt::fmt_number(
      columns = c(offense, defense, special),
      decimals = 3
    ) |>
    gt::cols_align(
      columns = -c(team),
      align = "center"
    ) |>
    gt::cols_label(
      season = "Season",
      week = "Week",
      rank = "Rank",
      logo = "Logo",
      team = "Team",
      score = "Team Score",
      diff = "∆ Score",
      offense = "Offense",
      defense = "Defense",
      special = "Special Teams"
    ) |>
    gt::cols_width(
      season ~ px(75),
      week ~ px(75),
      rank ~ px(75),
      logo ~ px(75)
    ) |>
    cfbplotR::gt_fmt_cfb_logo(columns = "logo") |>
    gt::opt_interactive(
      use_compact_mode = T,
      use_filters = T,
      page_size_default = 15
    ) |>
    add_gt_formatting() |>
    gt_est_color(
      columns = c("offense", "defense"),
      domain = c(-.55, .55)
    ) |>
    # gt::cols_merge(
    #   columns = c("score", "diff"),
    #   pattern = "{1} ({2})"
    # ) |>
    gt_est_color(
      columns = "score",
      domain = c(-40, 40)
    ) |>
    gt::data_color(
      columns = c("diff"),
      domain = c(-12, 12),
      palette = rev(my_gt_palette()),
      na_color = 'white'
    ) |>
    gt::tab_spanner(
      columns = c("offense", "defense"),
      label = "Efficiency"
    )

}

team_rankings_tile = function(data, season = params$season) {

  data |>
    join_team_info() |>
    filter(season == params$season) |>
    pivot_longer(cols = c(score, offense, defense, special),
                 names_to = c("type"),
                 values_to = c("estimate")) |>
    add_team_ranks(groups = c("season", "season_week", "season_type", "type")) |>
    filter(type == 'score') |>
    add_season_week() |>
    filter(rank <= 25) |>
    ggplot(aes(x=week,
               y=factor(rank),
               color = team,
               fill = team,
               group = team,
               label = abbreviation))+
    geom_tile(color = 'white')+
    geom_text(color = 'white', size = 2)+
    cfbplotR::scale_color_cfb()+
    cfbplotR::scale_fill_cfb()+
    coord_cartesian(xlim = c(1, 20))+
    theme_cfb()+
    scale_x_continuous(n.breaks = 20)+
    theme(panel.grid = element_blank(),
          panel.border = element_blank())+
    xlab("Week")+
    ylab("Ranking")+
    scale_y_discrete(limits = rev)+
    labs(title = paste(season, "Team Rankings by Week"),
         subtitle = stringr::str_wrap("Team rankings based on expected margin of victory against average FBS opponent, estimated from opponent adjusted predicted points per play.", 100)
    )

}

plot_team_scores_by_conference = function(data, conference = 'Big Ten', span = 0.5, lines = c(0)) {

  plot_team_lines = function(data, ylim = c(-30, 30)) {

    data |>
      add_season_week() |>
      ggplot(aes(x=week,
                 y=score,
                 color = team,
                 label = abbreviation))+
      geom_line(stat = 'smooth', method = 'loess', formula = 'y ~ x', span = span)+
      cfbplotR::scale_color_cfb()+
      theme_cfb()+
      # coord_cartesian(ylim = ylim)+
      xlab("Season Week")+
      ylab("Team Score")+
      geom_hline(yintercept = lines, linetype = 'dashed')+
      coord_cartesian(
        #ylim = c(-33, 33),
        xlim = c(-1, 20)
      )

  }

  label_team = function(data, var, size = 3, nudge_x = 1.2) {

    ggrepel::geom_text_repel(
      aes(label = {{var}}),
      fontface = "bold",
      size = size,
      direction = "y",
      nudge_x = nudge_x,
      segment.alpha = .5,
      segment.linetype = "dotted",
      box.padding = .2,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 20,
      segment.size = 0.5

    )
  }

  data |>
    filter(season == params$season) |>
    join_team_info() |>
    inner_join(
      tibble(
        conference = conference
      ), by = join_by(conference)
    ) |>
    add_season_week() |>
    group_by(season, team) |>
    mutate(start_label = case_when(week == min(week) ~ abbreviation),
           end_label = case_when(week == max(week) ~ abbreviation)) |>
    plot_team_lines()+
    label_team(var = end_label, size = 3, nudge_x = 1.1)+
    label_team(var = start_label, size = 2, nudge_x = -0.9)+
    facet_wrap(conference ~.)
}

plot_team_scores = function(data, season = params$season) {

  data |>
    ggplot(aes(x=offense,
               y=defense,
               color = team,
               label = abbreviation)
    )+
    geom_label(size = 2.5, alpha = 0.8)+
    cfbplotR::scale_color_cfb()+
    theme_cfb()+
    geom_vline(xintercept = 0, linetype = 'dotted')+
    geom_hline(yintercept = 0, linetype = 'dotted')+
    coord_cartesian(
      xlim = c(-0.4, 0.4),
      ylim = c(-0.4, 0.4)
    )+
    xlab("Offensive Points Added per Play")+
    ylab("Defensive Points Added per Play")+
    labs(title = paste(season, "Team Efficiency"),
         subtitle = stringr::str_wrap("Offensive and defensive efficiency estimates based on opponent adjusted predicted points per play.", 90)
    )+
    annotate(
      geom = "label",
      x = -.35,
      y = .35,
      size = 3,
      alpha = 0.8,
      label = "Bad Offense\nGood Defense"
    ) +
    annotate(
      geom = "label",
      x = .35,
      y = .35,
      size = 3,
      alpha = 0.8,
      label = "Good Offense\nGood Defense"
    ) +
    annotate(
      geom = "label",
      x = .35,
      y = -.35,
      size = 3,
      alpha = 0.8,
      label = "Good Offense\nBad Defense"
    )+
    annotate(
      geom = "label",
      x = -.35,
      y = -.35,
      size = 3,
      alpha = 0.8,
      label = "Bad Offense\nBad Defense"
    )

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

gt_pred_color = function(tbl, columns, target_columns) {

  tbl |>
    gt::data_color(
      columns = columns,
      domain = c(-80, 80),
      method = "numeric",
      palette= c("orange", "white", "dodgerblue3"),
      target_columns = target_columns,
      direction = "column",
      na_color = "white"
    )

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

plot_games_margins = function(data) {

  data |>
    ggplot(
      aes(
        x=pred_margin,
        y=home_margin,
        color = correct,
        label = paste(home_team, away_team, sep = " vs "))
    )+
    geom_point()+
    geom_text(vjust = -1, size = 2, check_overlap = T)+
    scale_color_manual(values = c("red", "dodgerblue3"))+
    theme_cfb()+
    geom_vline(xintercept = 0, linetype = 'dotted')+
    geom_hline(yintercept = 0, linetype = 'dotted')+
    coord_cartesian(
      xlim = c(-75, 75),
      ylim = c(-75, 75)
    )+
    xlab("Predicted Home Margin")+
    ylab("Actual Home Margin")+
    guides(color = 'none')

}

add_spread_cover = function(data) {

  data |>
    mutate(
      pred_cover = case_when(
        spread_margin >0 & pred_margin >= spread_margin ~ 'yes',
        spread_margin <0 & pred_margin < spread_margin ~ 'yes',
        spread_margin >0 & pred_margin <= spread_margin ~ 'no',
        spread_margin <0 & pred_margin >= spread_margin ~ 'no'
      ),
      actual_cover = case_when(
        spread_margin >0 & home_margin >= spread_margin ~ 'yes',
        spread_margin <0 & home_margin < spread_margin ~ 'yes',
        spread_margin >0 & home_margin <= spread_margin ~ 'no',
        spread_margin <0 & home_margin >= spread_margin ~ 'no'
      )
    ) |>
    mutate(
      across(c(pred_cover, actual_cover), ~ factor(.x, levels = c("yes", "no")))
    )
}

assess_spread = function(data, groups = c("season", "provider", "week")) {


  accuracy =
    data |>
    group_by(across(any_of(groups))) |>
    mutate(games = n_distinct(game_id)) |>
    group_by(across(any_of(groups)), games) |>
    yardstick::accuracy(
      actual_cover,
      pred_cover
    )

  record =
    data |>
    mutate(correct = case_when(pred_cover == actual_cover ~ 'yes',
                               pred_cover != actual_cover ~ 'no')) |>
    filter(!is.na(correct)) |>
    group_by(across(any_of(groups)), correct) |>
    count() |>
    ungroup() |>
    pivot_wider(
      names_from = c("correct"),
      values_from = c("n")
    ) |>
    mutate(across(c("yes", "no"), ~ replace_na(.x, 0))) |>
    mutate(record = paste(yes, no, sep = "-")) |>
    select(any_of(groups), record)

  accuracy |>
    inner_join(record)

}

assess_spread_overall= function(data) {


  week =
    data |>
    assess_spread(groups = c("season", "provider", "week"))

  season =
    data |>
    assess_spread(groups = c("season", "provider")) |>
    mutate(week = 'overall')

  bind_rows(
    week |>
      mutate(week = as.character(week)),
    season
  )
}

assess_spread_tbl = function(data) {

  data |>
    select(season, provider, week, .estimate, record) |>
    pivot_wider(
      names_from = c("provider"),
      values_from = c(".estimate", "record")
    ) |>
    mutate_if(is.numeric, round, 3) |>
    gt_tbl() |>
    gt::cols_align(
      align = "center"
    ) |>
    cols_merge(
      columns = ends_with("Bovada"),
      pattern = "{2} ({1})"
    ) |>
    cols_merge(
      columns = ends_with("DraftKings"),
      pattern = "{2} ({1})"
    ) |>
    cols_merge(
      columns = ends_with("ESPN Bet"),
      pattern = "{2} ({1})"
    ) |>
    cols_label(
      .estimate_Bovada = "Bovada",
      .estimate_DraftKings = 'DraftKings',
      `.estimate_ESPN Bet` = 'ESPN Bet'
    )

}
