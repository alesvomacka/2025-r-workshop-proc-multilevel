# Regions example - compute error ----------------------------------------
#' Compute Model Prediction Errors by Region
#'
#' Calculates mean squared error and mean absolute error for a model's
#' regional predictions compared to observed prevalence values.
#'
#' @param model A fitted model object that can be used with
#'   `marginaleffects::avg_predictions()`. Typically a GLM or mixed-effects
#'   model with region-level predictions.
#' @param title A character string providing a descriptive name for the model
#'   (e.g., "No Pooling", "Complete Pooling", "Partial Pooling").
#'
#' @return A tibble with one row containing:
#'   \item{model}{The model title provided}
#'   \item{mean_sq_error}{Mean squared error across regions}
#'   \item{mean_abs_error}{Mean absolute error across regions}
#'
#' @details
#' The function computes average predictions by region using
#' `marginaleffects::avg_predictions()`, joins them with the `kraje` dataset
#' (defined inside the function), calculates prediction errors, and summarizes them.
#'
#' @examples
#' \dontrun{
#' # Fit a simple model
#' m1 <- glm(crime_experience ~ 1, data = kraje_sample, family = binomial())
#'
#' # Compute errors
#' compute_errors(model = m1, title = "No Pooling")
#' }
compute_errors <- function(model, title) {
  kraje <- tribble(
    ~region              , ~n      , ~prop  , ~prev ,
    "Praha"              , 1357326 , 0.125  , 0.519 ,
    "Středočeský"     , 1439391 , 0.133  , 0.569 ,
    "Jihočeský"        ,  652303 , 0.0602 , 0.471 ,
    "Plzeňský"         ,  605388 , 0.0559 , 0.344 ,
    "Karlovarský"       ,  293595 , 0.0271 , 0.695 ,
    "Ústecký"          ,  812337 , 0.075  , 0.446 ,
    "Liberecký"         ,  449177 , 0.0415 , 0.431 ,
    "Královéhradecký" ,  555267 , 0.0513 , 0.529 ,
    "Pardubický"        ,  528761 , 0.0488 , 0.358 ,
    "Vysočina"          ,  514777 , 0.0475 , 0.457 ,
    "Jihomoravský"      , 1217200 , 0.112  , 0.391 ,
    "Olomoucký"         ,  631802 , 0.0584 , 0.496 ,
    "Moravskoslezský"   , 1189674 , 0.11   , 0.600 ,
    "Zlínský"          ,  580531 , 0.0536 , 0.347
  )

  marginaleffects::avg_predictions(model, variables = "region") |>
    dplyr::left_join(kraje, by = "region") |>
    dplyr::mutate(error = prev - estimate) |>
    dplyr::summarise(
      mean_sq_error = mean(error^2),
      mean_abs_error = mean(abs(error))
    ) |>
    dplyr::mutate(model = title, .before = mean_sq_error)
}

#' Plot Predictions for a Sample of Schools
#'
#' Creates a line plot showing model predictions across a range of within-school
#' SES values for a random sample of schools.
#'
#' @param model A fitted model object that can be used with
#'   `marginaleffects::predictions()`. Typically a mixed-effects model with
#'   school-level random effects and an `ses_within` predictor.
#' @param n_schools Integer. The number of schools to randomly sample for
#'   plotting. Defaults to 5.
#' @param data A data frame containing the school data with a `school_id`
#'   column. Defaults to `schools`.
#'
#' @return A ggplot2 object showing predicted values (with confidence intervals)
#'   as a function of within-school SES, with separate lines for each sampled
#'   school.
#'
#' @details
#' The function randomly samples school IDs from the provided data, generates
#' predictions across a range of `ses_within` values (-3 to 3), and creates a
#' line plot with confidence bands. Note that the function currently uses a
#' hardcoded model object `m5` in the prediction call, which should be replaced
#' with the `model` parameter.
#'
#' @examples
#' \dontrun{
#' # Fit a multilevel model
#' m5 <- lmer(math_score ~ ses_within + (1 | school_id), data = schools)
#'
#' # Plot predictions for 5 random schools
#' plot_sample_schools(model = m5)
#'
#' # Plot predictions for 8 random schools
#' plot_sample_schools(model = m5, n_schools = 8)
#' }
plot_sample_schools <- function(model, n_schools = 5, data = schools) {
  school_ids_sample <- sample(unique(schools$school_id), n_schools)

  marginaleffects::predictions(
    model,
    newdata = marginaleffects::datagrid(
      school_id = school_ids_sample,
      ses_within = seq(-3, 3, 0.1)
    )
  ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = ses_within,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
      group = school_id
    )) +
    ggplot2::geom_line()
}
