#' Estimates the model using brms, takes the formula, a filename, and data
#' if use_model != null, then it updates the model provided
run_model <- function(form, filename, dat, force = FALSE, use_model = NULL, refresh = 0, ...) {
  if (!file.exists(filename) || force) {
    # Weakly informative default prior (Gelman et al., 2008; except for the scaling)

    if (!is.null(use_model)) {
      fit <- update(use_model, newdata = dat, formula = form, recompile = FALSE, refresh = refresh)
    } else {
      fit <- brm(form, data = dat, prior = prior(student_t(3, 0, 2.5), class = b), refresh = refresh, ...)
    }
    saveRDS(fit, filename)
  } else {
    fit <- readRDS(filename)
  }

  fit
}

# Add the category of the behaviors to a data frame
add_behavior_categories <- function(df, categories) {
  n <- nrow(df)
  df$category <- NULL

  for (i in seq(n)) {
    varname <- as.character(df$behavior[i])

    if (varname %in% categories$Civic) {
      category <- 'Civic action'
    } else if (varname %in% categories$Lifestyle) {
      category <- 'Lifestyle change'
    }

    df$category[i] <- category
  }

  df %>%
    mutate(category = factor(category, levels = c('Civic action', 'Lifestyle change', 'Academic')))
}

# Create a form for the models of the climate actions
make_form <- function(
    outcome, random_intercept = TRUE,
    random_slope = TRUE, binarize = TRUE, marginal = FALSE,
    worry = FALSE, informed = FALSE
) {

  climate_pred <- ifelse(binarize, 'climate_researcher', 'research_fact')

  if (marginal) {
    pred <- climate_pred
  } else {
    pred <- paste0(
      climate_pred, ifelse(worry, ' + Worry_std', ''), ifelse(informed, ' + Informed_std', ''),
      ' + Age_std + Political_std + position + field + continent + is_tenured + is_female + is_gender_other'
    )
  }

  if (random_intercept & !(random_slope)) {
    pred <- paste0(pred, ' + (1 | country)')
  } else if (random_slope) {
    re <- paste0(' + (1 + ', climate_pred, ' | country)')
    pred <- paste0(pred, re)
  }

  as.formula(paste0(outcome, ' ~ ', pred))
}


# Get average (adjusted) differences between climate and non-climate researchers
get_avg_comparisons <- function(fit_all, behaviors, type = 'marginal', cores = 2) {

  registerDoParallel(cores = cores)
  res <- foreach(i = seq(length(behaviors))) %dopar% {

    b <- names(behaviors)[i]
    fit <- fit_all[[i]][[b]]

    # Get multiplicative difference
    comp <- avg_comparisons(
      fit, variables = 'climate_researcher', comparison = 'ratio'#,
      # newdata = datagrid(grid_type = 'balanced') # Do not create a balanced data set, but use the empirical distribution
    )

    df <- data.frame(comp)
    df$type <- type
    df$behavior <- b

    df
  }

  df <- do.call('rbind', res)
  rownames(df) <- NULL
  df <- df %>% select(estimate, conf.low, conf.high, type, behavior)
  colnames(df) <- c('estimate', 'ci_lo', 'ci_hi', 'type', 'behavior')
  df
}


# Get average (adjusted) differences between climate and non-climate researchers for the number of actions
get_avg_comparisons_binom <- function(fit, behavior, type) {

  # Get multiplicative difference
  comp <- avg_comparisons(
    fit, variables = 'climate_researcher', comparison = 'ratio'
  )

  df <- data.frame(comp) %>%
    mutate(type = type, behavior = b) %>%
    select(estimate, conf.low, conf.high, type, behavior)

  df$type <- type
  df$behavior <- behavior
  rownames(df) <- NULL
  colnames(df) <- c('estimate', 'ci_lo', 'ci_hi', 'type', 'behavior')
  df
}

# Get marginal estimates of the fitted regression models for number of climate actions
get_me <- function(model) {
  df <- data.frame(fixef(model)) %>%
    rownames_to_column() %>%
    mutate(outcome = names(model$data)[1])

  post <- posterior_samples(model)
  ratio <- plogis(post[, 1] + post[, 2]) / plogis(post[, 1])

  res <- data.frame(
    behavior = df$outcome[1],
    theta_ratio = mean(ratio),
    theta_ratio_q005 = quantile(ratio, 0.005),
    theta_ratio_q995 = quantile(ratio, 0.995)
  )

  res
}

# Get average comparisons from fitted model
get_effects <- function(fit, behavior, type, metric = 'absolute', conf_level = 0.99) {
  df <- data.frame(avg_predictions(fit, variable = 'climate_researcher', conf_level = conf_level))

  df <- df %>%
    rename(
      ci_lo = conf.low,
      ci_hi = conf.high
    ) %>%
    mutate(
      behavior = behavior,
      type = type
    )

  df
}

# Compute correlations between variables
get_cors <- function(dat, fname) {

  if (!file.exists(fname)) {
    comb <- expand.grid(i = seq(p), j = seq(p))

    res <- foreach(iter = seq(nrow(comb)), .combine = rbind) %dopar% {

      i <- comb[iter, 'i']
      j <- comb[iter, 'j']

      if (i != j) {
        res <- cor.test(dat[, i], dat[, j], method = 'kendall')
        row <- c(res$estimate, res$p.value, i, j)
        row
      }
    }

    colnames(res) <- c('estimate', 'p.value', 'i', 'j')

    pvals <- matrix(NA, p, p)
    tau_mat <- pvals
    colnames(tau_mat) <- rownames(tau_mat) <- colnames(dat)

    for (iter in seq(nrow(res))) {
      row <- res[iter, ]
      i <- row[3]
      j <- row[4]
      tau <- row[1]
      pvalue <- row[2]

      tau_mat[i, j] <- tau_mat[j, i] <- tau
      pvals[i, j] <- pvals[j, i] <- pvalue

    }

    corr_res <- list('cor_mat' = tau_mat, 'pvals' = pvals, 'method' = 'kendall')
    saveRDS(corr_res, fname)

  } else {
    corr_res <- readRDS(fname)
  }

  corr_res
}
