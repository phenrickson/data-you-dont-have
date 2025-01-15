load_data <- function(file = "data/raw/frmgham2.csv") {
  readr::read_csv(file) |>
    dplyr::rename_with(tolower)
}

prepare_data <- function(data) {
  data |>
    dplyr::mutate(sex = case_when(sex == 1 ~ "male", sex == 2 ~ "female")) |>
    dplyr::rename(
      chd = anychd,
      mifchd = mi_fchd,
      prev_chd = prevchd,
      prev_angina = prevap,
      prev_mi = prevmi,
      prev_stroke = prevstrk,
      prev_hyperten = prevhyp,
      time_angina = timeap,
      time_hospmi = timemi,
      time_mifchd = timemifc,
      time_chd = timechd,
      time_stroke = timestrk,
      time_cvd = timecvd,
      time_death = timedth,
      time_hyperten = timehyp
    )
}

add_outcomes <- function(data, var, label = "status", days = 10 * 365) {
  time_var <- paste("time", var, sep = "_")
  time_sym <- rlang::sym(time_var)
  status_var <- paste(label, var, sep = "_")

  data |>
    mutate(
      newtime_var = !!time_sym - time,
      status_days = days
    ) |>
    mutate(
      {{ status_var }} := as.factor(case_when(newtime_var < days ~ 1, TRUE ~ 0))
    )
}

add_ten_year_status <- function(data, var, label = "ten_year") {
  data |>
    add_outcomes(
      var = var,
      label = label,
      days = 10 * 365
    ) |>
    select(-any_of(starts_with("newtime")))
}

plot_histogram <- function(data, var) {
  data |>
    ggplot(aes(x = .data[[var]], fill = ten_year_chd)) +
    geom_histogram(position = "identity", bins = 50, alpha = 0.7, color = "white")
}

plot_ridge <- function(data, ids = c("ten_year_chd", "sex", "cursmoke"), vars, ncol = 2) {
  data |>
    pivot_longer(cols = -any_of(ids)) |>
    ggplot(aes(x = value, y = 0, fill = ten_year_chd)) +
    stat_density_ridges(quantile_lines = T, quantiles = 2, alpha = 0.5, color = "white") +
    facet_wrap(name ~ ., scales = "free", ncol = ncol)
}

build_recipe <- function(
    data,
    predictors = c("age", "sysbp", "diabp", "cursmoke", "bmi", "diabetes", "heartrte", "glucose", "ldlc", "hdlc"),
    outcome = "ten_year_chd") {
  recipe(
    data |>
      select(any_of(predictors), all_of(outcome))
  ) |>
    update_role(any_of(outcome), new_role = "outcome") |>
    update_role(-has_role("outcome"), new_role = "predictor")
}

fit_model <- function(data, model = logistic_reg()) {
  rec <-
    build_recipe(data) |>
    step_filter_missing(all_numeric_predictors(), threshold = 0.25) |>
    step_impute_bag(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors(), id = "normalize")

  workflow() |>
    add_recipe(rec) |>
    add_model(model) |>
    fit(data)
}

partial_data <- function(data, var, length = 10) {
  min_var <- min(data[, var], na.rm = T)
  max_var <- max(data[, var], na.rm = T)
  partial_range <- seq(min_var, max_var, length.out = length)

  map(
    partial_range,
    ~ data |>
      mutate({{ var }} := .x)
  ) |>
    list_rbind()
}

plot_marginal_effect <- function(data, var) {
  data |>
    ggplot(aes(x = .data[[var]], y = .pred)) +
    geom_line(aes(group = name), alpha = 0.25, color = "grey60") +
    coord_cartesian(ylim = c(0, 1)) +
    geom_smooth() +
    ylab("Pr(Heart Disease)")
}

wflow_marginal_effect <- function(wflow, data, var, normalize = T, plot = T) {
  mod <-
    wflow |>
    extract_fit_engine()

  dat <-
    data |>
    mutate(.rowIndex = row_number())

  betas <-
    MASS::mvrnorm(
      n = 1000,
      mu = coef(mod),
      Sigma = vcov(mod)
    ) |>
    t()

  baked <-
    wflow |>
    extract_recipe() |>
    bake(dat) |>
    mutate(.rowIndex = row_number()) |>
    select(.rowIndex, everything())

  partial_df <-
    baked |>
    partial_data(var = {{ var }})

  partial_mat <-
    partial_df |>
    select(where(is.numeric), -any_of(".rowIndex")) |>
    mutate(`(Intercept)` = 1) |>
    select(`(Intercept)`, everything()) |>
    as.matrix()

  sims <-
    partial_mat %*% betas |>
    plogis() |>
    as_tibble() |>
    bind_cols(
      partial_df |>
        select(.rowIndex, any_of(var))
    )

  sims_long <-
    sims |>
    pivot_longer(cols = -c(.rowIndex, any_of(var)), values_to = c(".pred_1")) |>
    group_by(name, across(any_of(var))) |>
    summarize(.pred = mean(.pred_1), .groups = "drop")

  if (normalize == T) {
    norm_values <-
      wflow |>
      extract_recipe() |>
      tidy(id = "normalize") |>
      pivot_wider(names_from = "statistic", values_from = c("value")) |>
      filter(terms == var)

    mean_var <- norm_values$mean
    sd_var <- norm_values$sd

    sims_long[, var] <- (sims_long[, var] * sd_var) + mean_var
  }

  if (plot == T) {
    sims_long |>
      plot_marginal_effect(var = {{ var }})
  } else {
    sims_long
  }
}

plot_coef <- function(data, color_var, size =1) {
  data |>
    filter(term != "(Intercept)") |>
    ggplot(aes(
      y = estimate,
      color = .data[[color_var]],
      x = reorder(term, estimate),
      ymin = conf.low, ymax = conf.high
    )) +
    geom_pointrange(size = size, position = position_dodge(width = .5)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    xlab("Feature") +
    ylab("Estimated Effect on Outcome") +
    coord_flip()
}

plot_effects <- function(results, var) {
  effects <-
    results |>
    mutate(
      partial =
        map2(
          fit,
          data,
          ~ wflow_marginal_effect(
            .x,
            .y,
            var = var,
            plot = F
          )
        )
    )

  # plot

  effects |>
    select(-any_of(c("data", "fit"))) |>
    unnest(partial) |>
    plot_marginal_effect(var = var)
}
