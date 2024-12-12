# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(crew)
library(gitcreds)

# authenticate
googleAuthR::gar_auth_service(
  json_file = Sys.getenv("GCS_AUTH_FILE"),
  scope = c(
    "https://www.googleapis.com/auth/devstorage.full_control",
    "https://www.googleapis.com/auth/cloud-platform"
  )
)

# set default bucket
suppressMessages({
  googleCloudStorageR::gcs_global_bucket(bucket = "cfb_models")
})

# Set target options:
tar_option_set(
  packages = c(
    "cfbfastR",
    "dplyr",
    "tidyr",
    "purrr",
    "stringr",
    "tidymodels",
    "glmnet"
  ),
  format = "qs",
  memory = "transient",
  resources =
    tar_resources(
      gcp = tar_resources_gcp(
        bucket = "cfb_models",
        predefined_acl = "bucketLevel",
        prefix = "data"
      )
    ),
  # controller =
  #   crew_controller_local(workers = 7),
  repository = "gcp"
)

# Run the R scripts in the R/ folder with your custom functions:
suppressMessages({
  tar_source("R")
})
# tar_source("other_functions.R") # Source other scripts as needed.

# running over seasons
seasons <- 2000:2023
current_season <- 2024

# Replace the target list below with your own:
list(
  ### cfbd data
  # load all games
  tar_target(
    cfbd_games_tbl,
    map_df(
      1869:cfbfastR:::most_recent_cfb_season(),
      ~ cfbd_game_info(
        year = .x,
        season_type = "both",
        division = "fbs"
      ) |>
        as_tibble()
    )
  ),
  # calendars
  tar_target(
    cfbd_calendar_tbl,
    map_df(
      seasons,
      ~ cfbd_calendar(year = .x)
    ) |>
      as_tibble()
  ),
  # conferences
  tar_target(
    cfbd_conferences_tbl,
    cfbd_conferences() |>
      as_tibble()
  ),
  # fbs team info
  tar_target(
    cfbd_team_info_tbl,
    map_df(
      seasons,
      ~ cfbd_team_info(
        only_fbs = T,
        year = .x
      ) |>
        as_tibble() |>
        mutate(season = .x)
    ) |>
      select(season, everything())
  ),
  # games for selected seasons
  tar_target(
    cfbd_game_info_tbl,
    map_df(
      seasons,
      ~ cfbd_game_info(
        year = .x,
        season_type = "both"
      )
    ) |>
      as_tibble()
  ),
  # # rankings
  tar_target(
    cfbd_game_rankings_tbl,
    {
      tmp <- expand_grid(
        season = seasons,
        type = c("regular", "postseason")
      )
      map2_df(
        .x = tmp$season,
        .y = tmp$type,
        ~ cfbd_rankings(
          year = .x,
          season_type = .y
        )
      )
    }
  ),
  # play types
  tar_target(
    cfbd_play_types_tbl,
    cfbd_play_types() |>
      as_tibble()
  ),
  # drives
  tar_target(
    cfbd_drives_tbl,
    map_df(
      seasons,
      ~ cfbd_drives(
        year = .x,
        season_type = "both"
      ) |>
        add_season(year = .x)
    )
  ),
  # get historical conferences and divisions
  # divisions
  tar_target(
    team_divisions,
    cfbd_game_info_tbl |>
      select(season, home_team, away_team, home_division, away_division) |>
      find_team_divisions()
  ),
  # conferences
  tar_target(
    team_conferences,
    cfbd_team_info_tbl |>
      select(
        season,
        school,
        conference,
        division
      ) |>
      distinct()
  ),
  # dynamic branch over seasons, weeks, and season type to get play by play
  tar_target(
    cfbd_season_week_games,
    cfbd_game_info_tbl |>
      select(season, week, season_type) |>
      distinct() |>
      filter(season_type %in% c("regular", "postseason")) |>
      group_by(season, week, season_type) |>
      tar_group(),
    iteration = "group"
  ),
  # get cleaned cfbd pbp (cfbfastR) for each branch
  tar_target(
    cfbd_pbp_data_tbl,
    get_cfbd_pbp_data(cfbd_season_week_games),
    pattern = map(cfbd_season_week_games),
    error = "null"
  ),
  # filter to only relevant
  tar_target(
    filtered_pbp,
    cfbd_pbp_data_tbl |>
      # filter to only games with fbs teams
      inner_join(
        cfbd_game_info_tbl |>
          filter(home_division == "fbs" | away_division == "fbs")
      ) |>
      # filter to games after 2005
      filter(season > 2005)
  ),
  # prepare pbp data using custom functions
  tar_target(
    prepared_pbp,
    filtered_pbp |>
      prepare_pbp() |>
      add_score_events()
  ),
  # expected points modeling
  tar_target(
    class_metrics,
    metric_set(
      yardstick::roc_auc,
      yardstick::mn_log_loss
    )
  ),
  # create split
  tar_target(
    split_pbp,
    prepared_pbp |>
      filter(season >= 2007 & season <= max(seasons)) |>
      split_seasons(
        end_train_year = 2017,
        valid_years = 2
      )
  ),
  # create recipe for pbp mmodel
  tar_target(
    pbp_recipe,
    split_pbp |>
      training() |>
      build_pbp_recipe()
  ),
  # create model specification for pbp model
  tar_target(
    pbp_model_spec,
    multinom_reg(
      mode = "classification",
      engine = "glmnet",
      penalty = 0,
      mixture = NULL
    )
  ),
  # create workflow for pbp
  tar_target(
    pbp_wflow,
    workflow() |>
      add_recipe(pbp_recipe) |>
      add_model(pbp_model_spec)
  ),
  # fit to training set; estimate on valid set
  tar_target(
    pbp_last_fit,
    pbp_wflow |>
      last_fit(
        split =
          split_pbp |>
          validation_set() |>
          pluck("splits", 1),
        metrics = class_metrics
      )
  ),
  # extract metrics
  tar_target(
    pbp_valid_metrics,
    pbp_last_fit |>
      collect_metrics()
  ),
  # extract predictions
  tar_target(
    pbp_valid_preds,
    pbp_last_fit |>
      collect_predictions() |>
      left_join(
        split_pbp |>
          validation() |>
          mutate(.row = row_number())
      )
  ),
  # predict test set
  tar_target(
    pbp_test_preds,
    pbp_last_fit |>
      extract_workflow() |>
      augment(split_pbp |> testing())
  ),
  # final fit
  tar_target(
    pbp_final_fit,
    pbp_last_fit |>
      extract_workflow() |>
      fit(
        split_pbp$data
      )
  ),
  # predict all plays with final model
  tar_target(
    pbp_all_preds,
    pbp_last_fit |>
      extract_workflow() |>
      augment(split_pbp$data)
  ),
  # calculate expected points
  tar_target(
    pbp_predicted,
    pbp_all_preds |>
      calculate_expected_points() |>
      calculate_points_added()
  ),
  # prepare for efficiency
  tar_target(
    pbp_efficiency,
    pbp_predicted |>
      prepare_efficiency(
        games = cfbd_game_info_tbl,
        game_type = c("regular", "postseason")
      )
  ),
  # now add in efficiency estimates
  # overall
  tar_target(
    raw_efficiency_overall,
    pbp_efficiency |>
      calculate_efficiency(groups = c("season", "type", "team"))
  ),
  tar_target(
    raw_efficiency_category,
    pbp_efficiency |>
      calculate_efficiency(groups = c("season", "type", "play_category", "team"))
  ),
  tar_target(
    adjusted_efficiency_overall_ppa,
    pbp_efficiency |>
      estimate_efficiency_overall(metric = "predicted_points_added")
  ),
  tar_target(
    adjusted_efficiency_category_ppa,
    pbp_efficiency |>
      estimate_efficiency_category(metric = "predicted_points_added")
  ),
  tar_target(
    cfb_season_weeks,
    cfbd_game_info_tbl |>
      find_season_weeks()
  ),
  tar_target(
    efficiency_weeks,
    cfb_season_weeks |>
      filter(season >= 2011) |>
      pull(week_date) |>
      unique()
  ),
  # branch over weeks and estimate efficiency in season
  # estimate offense/defense/sepcial
  tar_target(
    efficiency_ppa_by_week,
    pbp_efficiency |>
      estimate_efficiency_by_week(
        metric = "predicted_points_added",
        date = efficiency_weeks
      ),
    pattern = map(efficiency_weeks)
  ),
  # estimate pass/rush offense/defense
  tar_target(
    efficiency_category_ppa_by_week,
    pbp_efficiency |>
      estimate_efficiency_category_by_week(
        metric = "predicted_points_added",
        date = efficiency_weeks
      ),
    pattern = map(efficiency_weeks)
  ),
  # join with season weeks
  tar_target(
    efficiency_by_week,
    efficiency_ppa_by_week |>
      prepare_weekly_efficiency() |>
      inner_join(
        cfb_season_weeks
      )
  ),
  tar_target(
    efficiency_category_by_week,
    efficiency_category_ppa_by_week |>
      prepare_weekly_efficiency_category() |>
      inner_join(
        cfb_season_weeks
      )
  ),
  # prepare team estimates for use in games
  tar_target(
    team_estimates,
    efficiency_by_week |>
      prepare_team_estimates()
  ),
  # join games with team estimates
  tar_target(
    games_and_estimates,
    cfbd_game_info_tbl |>
      prepare_game_estimates(
        team_estimates = team_estimates,
        season_variables = c("season", "season_type", "season_week"),
        team_variables = c("pregame_overall", "pregame_offense", "pregame_defense", "pregame_special")
      ) |>
      add_game_outcomes() |>
      add_game_weights(ref = "2017-01-01", base = .999)
  ),
  tar_target(
    split_games,
    games_and_estimates |>
      split_by_season(
        end_train_season = 2021,
        valid_season = 1
      )
  ),
  tar_target(
    games_train_fit,
    build_games_wflow() |>
      fit(
        split_games |>
          training()
      ),
    packages = c("rstanarm")
  ),
  tar_target(
    games_final_fit,
    build_games_wflow() |>
      fit(
        split_games$data
      )
  ),
  tar_target(
    season_game_info,
    cfbfastR::cfbd_game_info(
      year = current_season,
      season_type = "both"
    ) |>
      arrange(as.Date(start_date)) |>
      as_tibble() |>
      adjust_team_names(),
    cue = tarchetypes::tar_cue_age(
      name = season_game_info,
      age = as.difftime(6, units = "days")
    )
  ),
  tar_target(
    season_weeks,
    season_game_info |>
      add_game_weeks() |>
      select(week_date, season_week) |>
      distinct() |>
      arrange(week_date) |>
      pull(season_week) |>
      unique()
  ),
  tar_target(
    season_completed_games,
    season_game_info |>
      filter(completed == T) |>
      select(season, week, season_type) |>
      distinct() |>
      filter(season_type %in% c("regular", "postseason")) |>
      group_by(season, week, season_type) |>
      tar_group()
  ),
  tar_target(
    season_pbp_raw,
    get_cfbd_pbp_data(season_completed_games),
    pattern = map(season_completed_games),
    error = "null"
  ),
  tar_target(
    pbp_model,
    pbp_final_fit
  ),
  tar_target(
    season_pbp_preds,
    pbp_model |>
      predict_pbp(data = season_pbp_raw)
  ),
  tar_target(
    # prep pbp data for efficiency estimates
    season_pbp_efficiency,
    season_pbp_preds |>
      prepare_efficiency(
        games = season_game_info
      )
  ),
  tar_target(
    season_completed_weeks,
    season_game_info |>
      filter(completed == T) |>
      find_season_weeks() |>
      pull(week_date)
  ),
  tar_target(
    season_efficiency_by_week,
    command =
      bind_rows(
        pbp_efficiency,
        season_pbp_efficiency
      ) |>
      estimate_efficiency_by_week(
        metric = "predicted_points_added",
        date = season_completed_weeks
      ),
    pattern = map(season_completed_weeks)
  ),
  tar_target(
    season_efficiency_category_by_week,
    command =
      bind_rows(
        pbp_efficiency,
        season_pbp_efficiency
      ) |>
      estimate_efficiency_category_by_week(
        metric = "predicted_points_added",
        date = season_completed_weeks
      ),
    pattern = map(season_completed_weeks)
  ),
  tar_target(
    season_team_category_estimates,
    command =
      season_efficiency_category_by_week |>
      prepare_weekly_efficiency_category() |>
      inner_join(
        season_game_info |>
          find_season_weeks()
      ) |>
      bind_rows(
        efficiency_category_by_week
      )
  ),
  tar_target(
    season_team_estimates,
    command =
      season_efficiency_by_week |>
      prepare_weekly_efficiency() |>
      inner_join(
        season_game_info |>
          find_season_weeks()
      ) |>
      bind_rows(
        efficiency_by_week
      ) |>
      prepare_team_estimates()
  ),
  tar_target(
    games_model,
    games_final_fit
  ),
  tar_target(
    team_scores,
    games_model |>
      calculate_team_scores(data = season_team_estimates)
  ),
  # simulate games
  tar_target(
    games_draws,
    {
      set.seed(1999)
      map(
        season_weeks,
        ~ games_model |>
          simulate_games(
            ndraws = 4000,
            seed = 1999,
            newdata =
              season_game_info |>
              prepare_games_for_prediction(estimates = season_team_estimates,
                                           season_week = .x) |>
              add_season_week() |>
              filter(season_week == .x)
          )
      ) |>
        list_rbind()
    }
  ),
  tar_target(
    games_sims,
    games_draws |>
      summarize_simulations()
  ),
  # tar_target(
  #   postseason_game_info,
  #   load_games(year = 2024, season_type = 'postseason'),
  #   cue = tarchetypes::tar_cue_age(
  #     name = postseason_games,
  #     age = as.difftime(6, units = "days")
  #   ),
  # ),
  # tar_target(
  #   playoff_ids,
  #   postseason_game_info |>
  #     filter(grepl("College Football Playoff", notes)) |>
  #     select(game_id)
  # ),
  tar_target(
    playoff_teams,
    c('Oregon', 
      'Georgia', 
      'Boise State', 
      'Arizona State', 
      'Texas', 
      'Penn State', 
      'Notre Dame', 
      'Ohio State', 
      'Tennessee', 
      'Indiana', 
      'SMU', 
      'Clemson')
  ),
  tar_target(
    current_team_estimates,
    season_team_estimates |>
      slice_estimates()
  ),
  tar_target(
    total_sims,
    10000
  ),
  tar_target(
    playoff_sims,
    map(
      1:total_sims,
      ~ simulate_playoff(
        playoff_teams,
        estimates = current_team_estimates,
        model = games_model |> extract_fit_engine()
      )
    ) |> list_rbind()
  )
)
