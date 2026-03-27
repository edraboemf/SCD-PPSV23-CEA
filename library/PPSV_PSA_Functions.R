##******************************************************************************
##* Probabilistic Sensitivity Analysis (PSA)
##******************************************************************************

qpert <- function(p, min, mode, max, lambda = 4, ncp = 0) {  
  # handle invalid inputs safely
  if(is.na(min) || is.na(max) || is.na(mode) || max <= min) {
    return(rep(mode, length(p)))
  }
  
  # clamp mode
  mode <- pmin(pmax(mode, min), max)
  
  # compute alpha/beta
  alpha <- 1 + lambda * (mode - min) / (max - min)
  beta  <- 1 + lambda * (max - mode) / (max - min)
  
  # safety check
  if(length(alpha) != 1 || length(beta) != 1 ||
     is.na(alpha) || is.na(beta) || alpha <= 0 || beta <= 0) {
    return(rep(mode, length(p)))
  }

  # compute quantile
  q_val <- qbeta(
    p = p, 
    shape1 = alpha, 
    shape1 = beta, 
    ncp = ncp
  )

  return(min + (max - min) * q_val)
}


##******************************************
## General distribution fitting from CI
##******************************************
fit_distribution_from_ci <- function(
  dist = c(
    "beta", "gamma", "lognormal", "normal", "pert",
    "poisson", "binomial", "neg_binomial"
  ),
  par_mean,
  par_lower,
  par_upper,
  conf = 0.95,
  optim_method = "L-BFGS-B",
  ...
) {

  valid_dists <- c(
    "beta", "gamma", "lognormal", "normal", "pert",
    "poisson", "binomial", "neg_binomial"
  )
  extra_args <- list(...)
  
  dist_clean <- tolower(gsub("-", "", dist))
  
  if(is.na(dist_clean) || dist_clean == "na") {
    return(list(mean = par_mean, variance = 0))
  }
  
  if(!dist_clean %in% valid_dists) {
    stop(paste("Distribution not recognized:", dist_clean))
  }

  ## Beta distribution
  if(dist_clean == "beta") {
    sse <- function(par, par_mean, par_lower, par_upper, conf) {
      a <- par[1]
      b <- par[2]
      if(a <= 0 | b <= 0) return(1e6)
      p_low <- (1 - conf)/2
      p_high <- 1 - p_low
      val <- (qbeta(p_low, a, b) - par_lower)^2 + 
        (qbeta(p_high, a, b) - par_upper)^2
      return(val)
    }

    res <- optim(
      par = c(par_mean*10000, (1 - par_mean)*10000),
      par_mean = par_mean,
      par_lower = par_lower, 
      par_upper = par_upper, 
      fn = sse,
      method = optim_method, 
      lower = c(1e-6, 1e-6),
      conf = conf
    )
    
    a <- res$par[1]
    b <- res$par[2]
    var <- (a * b)/(((a + b)^2) * (a + b + 1))
    
    return(list(alpha = a, beta = b, variance = var))
  }

  ## Gamma
  if(dist_clean == "gamma") {
    sse <- function(par, par_mean, par_lower, par_upper, conf) {
      shape <- par[1]
      scale <- par[2]
      if(shape <= 0 | scale <= 0) return(1e6)
      p_low <- (1-conf)/2
      p_high <- 1 - p_low
      val <- (qgamma(p_low, shape, scale) - par_lower)^2 + 
        (qgamma(p_high, shape, scale) - par_upper)^2
      return(val)
    }

    par_var <- ((par_upper - par_lower)/4)^2
    res <- optim(
      par = c(par_mean^2/par_var, par_var/par_mean),
      par_mean = par_mean,
      par_lower = par_lower, 
      par_upper = par_upper, 
      fn = sse, 
      method = optim_method,
      lower = c(1e-6, 1e-6),
      conf = conf
    )

    shape <- res$par[1]
    scale <- res$par[2]
    var <- shape * (scale^2)
    
    return(list(shape = shape, scale = scale, variance = var))
  }

  ## Lognormal
  if(dist_clean == "lognormal") {
    sse <- function(par, par_lower, par_upper, par_mean, conf) {
      mu <- par[1]
      sigma <- par[2]
      if(sigma <= 0) return(1e6)      
      p_low <- (1 - conf)/2
      p_high <- 1 - p_low
      p_mean <- (p_low + p_high) / 2
      q_low <- tryCatch(qlnorm(p_low, mu, sigma), error = function(e) NA)
      q_high <- tryCatch(qlnorm(p_high, mu, sigma), error = function(e) NA)
      if(any(is.na(c(q_low, q_high)))) return(1e6)
      val <- (q_low - par_lower)^2 + (q_high - par_upper)^2
      return(val)
    }
    
    sigma_guess <- (log(par_upper) - log(par_lower)) / (2 * qnorm(1-(1-conf)/2))
    mu_guess <- log(par_mean) - 0.5 * (sigma_guess^2)

    res <- optim(
      par = c(mu_guess, sigma_guess),
      par_mean = par_mean,
      par_lower = par_lower, 
      par_upper = par_upper, 
      fn = sse,
      method = optim_method,
      lower = c(-1e-6, 1e-6),
      conf = conf
    )
    
    mu <- res$par[1]
    sigma <- res$par[2]
    var <- (exp(sigma^2) - 1) * exp(2*mu + sigma^2)  
    
    return(list(meanlog = mu, sdlog = sigma, variance = var))
  }

  ## Normal
  if(dist_clean == "normal") {
    sse <- function(par, par_mean, par_lower, par_upper, conf){
      mu <- par[1]
      sigma <- par[2]
      if(sigma <= 0) return(1e6)
      p_low <- (1 - conf)/2
      p_high <- 1 - p_low
      val <- (qnorm(p_low, mu, sigma) - par_lower)^2 + 
        (qnorm(p_high, mu, sigma) - par_upper)^2
      return(val)
    }
    z <- qnorm(1 - (1 - conf)/2)
    sigma_guess <- (par_upper - par_lower)/(2*z)
    
    res <- optim(
      par = c(par_mean, sigma_guess),
      par_mean = par_mean,
      par_lower = par_lower, 
      par_upper = par_upper, 
      fn = sse,
      method = optim_method,
      lower = c(-Inf, 1e-6),
      conf = conf
    )
    
    mu <- res$par[1]
    sigma <- res$par[2]
    var <- sigma^2
    
    return(list(mean = mu, sd = sigma, variance = var))
  }

  ## PERT (approx via beta)
  if(dist_clean == "pert") {
    a <- par_lower
    b <- par_upper
    m <- par_mean
    
    alpha <- 1 + 4 * (m - a)/(b - a)
    beta <- 1 + 4 * (b - m)/(b - a)

    var_beta <- (alpha * beta)/(((alpha + beta)^2) * (alpha + beta + 1))
    var <- var_beta * ((b - a)^2)
    
    return(list(mode = m, min = a, max = b, variance = var))
  }

  ## Poisson
  if(dist_clean == "poisson") {
    return(list(lambda = par_mean, variance = par_mean))
  }

  ## Binomial
  if(dist_clean == "binomial") {
    n <- extra_args$n_trials
    if(is.null(n)) stop("Need n_trials")
    p <- par_mean / n
    var <- n * p * (1 - p)
    return(list(n = n, p = p, variance = var))
  }

  ## Negative binomial
  if(dist_clean %in% c("negbinomial", "neg_binomial")) {
    var_est <- ((par_upper - par_lower)/4)^2
    r <- par_mean^2 / (var_est - par_mean)
    p <- r / (r + par_mean)
    var <- r * (1 - p) / p^2
    return(list(size = r, prob = p, variance = var))
  }
}


##******************************************
## Vectorized wrapper
##******************************************
fit_distributions_from_ci <- function(
  par_mean,
  par_lower,
  par_upper,
  dist = rep("beta", length(par_mean)),
  conf = 0.95,
  optim_method = "L-BFGS-B",
  extra_args_list = vector("list", length(par_mean))
) {
  stopifnot(
    length(par_mean) == length(par_lower),
    length(par_mean) == length(par_upper),
    length(par_mean) == length(dist),
    length(par_mean) == length(extra_args_list)
  )
  
  n <- length(par_mean)
  results <- vector("list", n)
  
  for(i in seq_len(n)) {
    args <- c(
      list(
        par_mean = par_mean[i],
        par_lower = par_lower[i],
        par_upper = par_upper[i],
        dist = dist[i],
        conf = conf,
        optim_method = optim_method
      ),
      extra_args_list[[i]]
    )
    
    res <- do.call(fit_distribution_from_ci, args)
    
    # store as a data.frame row
    results[[i]] <- data.frame(
      parameter = i,
      distribution = dist[i],
      as.list(res),
      stringsAsFactors = FALSE
    )
  }
  
  # bind rows safely, filling missing columns with NA
  out <- bind_rows(results) %>% as.data.frame()
  out$parameter <- rownames(out)
  rownames(out) <- NULL
  out
}


## rpert
rpert <- function(n, min, mode, max, lambda = 4, ncp = 0) {
  # checks
  if(any(max <= min)) stop("max must be > min")
  if(any(mode < min | mode > max)) stop("mode must be between min and max")
  if(lambda <= 0) stop("lambda must be > 0")
  
  # compute alpha/beta
  alpha <- 1 + lambda * (mode - min) / (max - min)
  beta  <- 1 + lambda * (max - mode) / (max - min)

  # sample
  x <- rbeta(
    n = n, 
    shape1 = alpha, 
    shape2 = beta,
    ncp = ncp
  )
  # scale to [min, max]
  return(min + x * (max - min))
}





draw_param_func <- function(fit_obj, n_sim = 1000) {
  
  n_param <- nrow(fit_obj)
  out <- matrix(NA, nrow = n_sim, ncol = n_param)
  colnames(out) <- make.unique(as.character(fit_obj$parameter))
  
  dist_clean <- tolower(gsub("-", "", fit_obj$distribution))
  
  for (i in seq_len(n_param)) {
    
    # helper (replacement for %||%)
    get_val <- function(x, alt) {
      if (!is.null(x) && !is.na(x[i])) x[i] else alt
    }
    
    # NA / constant
    if (is.na(dist_clean[i]) || dist_clean[i] == "" || dist_clean[i] == "na") {
      out[, i] <- rep(fit_obj$mean[i], n_sim)
      next
    }
    
    # Beta
    if (dist_clean[i] == "beta") {
      out[, i] <- rbeta(
        n_sim,
        shape1 = fit_obj$alpha[i],
        shape2 = fit_obj$beta[i]
      )
      next
    }
    
    # Gamma
    if (dist_clean[i] == "gamma") {
      out[, i] <- rgamma(
        n_sim,
        shape = fit_obj$shape[i],
        scale = fit_obj$scale[i]
      )
      next
    }
    
    # Lognormal
    if (dist_clean[i] == "lognormal") {
      out[, i] <- rlnorm(
        n_sim,
        meanlog = fit_obj$meanlog[i],
        sdlog = fit_obj$sdlog[i]
      )
      next
    }
    
    # Normal
    if (dist_clean[i] == "normal") {
      out[, i] <- rnorm(
        n_sim,
        mean = fit_obj$mean[i],
        sd = fit_obj$sd[i]
      )
      next
    }
    
    # PERT
    if (dist_clean[i] == "pert") {
      min_val <- get_val(fit_obj$min, get_val(fit_obj$lower, 0))
      max_val <- get_val(fit_obj$max, get_val(fit_obj$upper, 1))
      mode <- fit_obj$mode[i]
      lambda <- get_val(fit_obj$lambda, 4)
      
      mode <- pmin(pmax(mode, min_val), max_val)
      
      alpha <- 1 + lambda * (mode - min_val) / (max_val - min_val)
      beta  <- 1 + lambda * (max_val - mode) / (max_val - min_val)
      
      if (is.na(alpha) || is.na(beta) || alpha <= 0 || beta <= 0) {
        out[, i] <- rep(mode, n_sim)
        next
      }
      
      tval <- rbeta(n_sim, shape1 = alpha, shape2 = beta)
      out[, i] <- min_val + (max_val - min_val) * tval
      next
    }
    
    # Poisson
    if (dist_clean[i] == "poisson") {
      out[, i] <- rpois(n_sim, lambda = fit_obj$lambda[i])
      next
    }
    
    # Binomial
    if (dist_clean[i] == "binomial") {
      out[, i] <- rbinom(
        n_sim,
        size = fit_obj$n[i],
        prob = fit_obj$p[i]
      )
      next
    }
    
    # Negative Binomial
    if (dist_clean[i] %in% c("negbinomial", "neg_binomial")) {
      out[, i] <- rnbinom(
        n_sim,
        size = fit_obj$size[i],
        prob = fit_obj$prob[i]
      )
      next
    }
    
    stop(paste("Unsupported distribution:", dist_clean[i]))
  }
  
  as.data.frame(out)
}


##******************************************
## PSA data function
##******************************************
psa_func <- function(
  X = 1, 
  params = psa_params,
  ppsv23_params = ppsv23_params_value,
  cpi = ppsv23_params_cpi, 
  gdp = bfa_gdp,
  gdp_mult = 3,
  ref_yr = ref_year,
  strategies = c("PCV13", "PPSV23"),
  ref_strategy = "PCV13",
  add_pcv13 = FALSE,
  amc = FALSE,
  time = 0,
  nyears = 5,
  birth_cohort_update = NULL,
  alive_pop = NULL
) {
                      
  sim_params <- as.list(params[X, names(ppsv23_params)])
  
  result <- cea_func(
    params = sim_params,
    cpi = cpi, 
    gdp = gdp,
    gdp_mult = gdp_mult,
    ref_yr = ref_yr,
    strategies = strategies,
    ref_strategy = ref_strategy,
    add_pcv13 = add_pcv13,
    amc = amc,
    time = time,
    nyears = nyears,
    birth_cohort_update = birth_cohort_update,
    alive_pop = alive_pop
  )
  result$sim <- X
  
  ## ouput
  return(result)
}

##******************************************
## CEAC data function
##******************************************
ceac_data_func <- function(wtp, data = psa_results_3gdp) {
  val <- data %>%
    mutate(
      strategy = factor(strategy, levels = c(1, 2), labels = c("PCV13", "PPSV23")),
      cost = cost_hc,
      dalys = dalys,
      nmb = nmb_who_gdp_hc,
      nhb = nhb_who_gdp_hc,
      icer = icer_hc,
      inmb = inmb_who_gdp_hc,
      inhb = inhb_who_gdp_hc  
    ) %>%
    dplyr::select(sim, strategy, cost, dalys) %>%
    mutate(nmb = (- dalys * wtp) - cost) %>%
    dplyr::select(sim, strategy, nmb) %>%
    pivot_wider(names_from = strategy, values_from = c(nmb)) %>%
    dplyr::select(!sim) %>%
    mutate(best_strategy = ifelse(PPSV23 > PCV13, "PPSV23", "PCV13")) %>%
    dplyr::summarise(
      pcv13_prob = mean(best_strategy == "PCV13"),
      pcv13_n = sum(!is.na(best_strategy)),
    ) %>%
    mutate(
      pcv13_se = sqrt(pcv13_prob * (1 - pcv13_prob) / pcv13_n),
      pcv13_lower = max(0, pcv13_prob - 1.96 * pcv13_se),
      pcv13_upper = min(1, pcv13_prob + 1.96 * pcv13_se),
      ##
      ppsv23_prob = 1 - pcv13_prob,
      ppsv23_se = sqrt(ppsv23_prob * (1 - ppsv23_prob) / pcv13_n),
      ppsv23_lower = max(0, ppsv23_prob - 1.96 * ppsv23_se),
      ppsv23_upper = min(1, ppsv23_prob + 1.96 * ppsv23_se),
      ##
      wtp = wtp
    )

  return(val)
}

##******************************************
## Function to summarise PSA results
##******************************************
psa_summary <- function(
  data = psa_results_3gdp_renamed,
  boot_group_var = boot_group_var,
  ref_strategy = 1,
  wtp = 2693.087,
  conf = 0.95
) {

  # Wilson ci function
  wilson_ci <- function(x, conf = 0.95) {
    n <- length(x)
    p <- mean(x, na.rm = TRUE)
    z <- qnorm(1 - (1 - conf)/2)    
    denom <- 1 + (z^2) / n
    center <- (p + (z^2) / (2 * n)) / denom
    half_width <- z * sqrt((p * (1 - p) / n) + (z^2) / (4 * (n^2))) / denom
    c(
      lower = center - half_width, 
      upper = center + half_width
    )
  }
  
  # function to get bootstrap ci
  boot_mean_ci <- function(
    x, 
    stat = c("median", "mean")[2], 
    R = 2000, 
    type = c("norm", "basic", "perc", "bca")[3]
  ) {  
    stat <- match.arg(stat) 

    boot_stat <- function(data, indices) {
      d <- data[indices]
      if(stat == "median"){
        median(d, na.rm = TRUE)
      }else{
        mean(d, na.rm = TRUE)
      }
    }

    boot_obj <- boot::boot(
      data = x,
      statistic = boot_stat,
      R = R
    )    

    boot_mean <- mean(boot_obj$t, na.rm = TRUE)

    ci <- try(boot::boot.ci(boot_obj, type = type), silent = TRUE)    
    if (!inherits(ci, "try-error")) {
      if (type == "norm"  && !is.null(ci$normal)) {
        return(c(mean = boot_mean, lower = ci$normal[2], upper = ci$normal[3]))
      }
      if (type == "basic" && !is.null(ci$basic)) {
        return(c(mean = boot_mean, lower = ci$basic[4], upper = ci$basic[5]))
      }
      if (type == "perc"  && !is.null(ci$percent)) {
        return(c(mean = boot_mean, lower = ci$percent[4], upper = ci$percent[5]))
      }
      if (type == "bca"   && !is.null(ci$bca)) {
        return(c(mean = boot_mean, lower = ci$bca[4], upper = ci$bca[5]))
      }
    } else {
      fallback_order <- c("perc", "basic", "norm")
      for (t in fallback_order) {
        ci <- try(boot::boot.ci(boot_obj, type = t), silent = TRUE)        
        if (!inherits(ci, "try-error")) {
          if (t == "perc" && !is.null(ci$percent)) {
            return(c(mean = boot_mean, lower = ci$percent[4], upper = ci$percent[5]))
          }
          if (t == "basic" && !is.null(ci$basic)) {
            return(c(mean = boot_mean, lower = ci$basic[4], upper = ci$basic[5]))
          }
          if (t == "norm" && !is.null(ci$normal)) {
            return(c(mean = boot_mean, lower = ci$normal[2], upper = ci$normal[3]))
          }
        }
      }
      return(c(mean = boot_mean, lower = NA, upper = NA))
    }
  }

  ## function to get normal ci
  mean_ci <- function(x, conf = 0.95) {
    n <- length(x)
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    error <- qt(1 - (1 - conf) / 2, df = n - 1) * (s / sqrt(n))
    c(lower = m - error, upper = m + error)
  }

  # significance level
  alpha <- (1 - conf) / 2
  probs <- c(alpha, 1 - alpha)

  ## reference strategy data
  ref_strategy_df <- data %>%
    filter(.data[[boot_group_var]] == ref_strategy) %>%
    mutate(
      ref_nmb = - (dalys * wtp  + cost),
      ref_nhb = - (dalys + (cost / wtp)),
    ) %>%
    rename(
      ref_cost = cost,
      ref_dalys = dalys
    ) %>%
    select(
      all_of(
        c("sim", "ref_cost", "ref_dalys", "ref_nmb", "ref_nhb")
      )
    )

  ## comparator strategy data
  comp_strategy_df <- data %>%
    filter(.data[[boot_group_var]] != ref_strategy) %>%
    mutate(
      comp_nmb = - (dalys * wtp + cost),
      comp_nhb = - (dalys + (cost / wtp)),
    ) %>%
    rename(
      comp_cost = cost,
      comp_dalys = dalys
    ) %>%  
    select(
      all_of(
        c("sim", "wtp", "comp_cost", "comp_dalys", "comp_nmb", "comp_nhb")
      )
    )
  
  ## combined data
  cea_df <- comp_strategy_df %>%
    left_join(ref_strategy_df, by = "sim") %>%
    mutate(
      ref_inc_cost = ref_cost - ref_cost,
      comp_inc_cost = comp_cost - ref_cost,

      ref_inc_dalys = - (ref_dalys - ref_dalys),
      comp_inc_dalys = - (comp_dalys - ref_dalys),      

      ref_inmb = ref_nmb - ref_nmb,
      comp_inmb = comp_nmb - ref_nmb,

      ref_inhb = ref_nhb  - ref_nhb,
      comp_inhb = comp_nhb - ref_nhb,

      ref_ce = ifelse(ref_nmb >= comp_nmb, 1, 0),
      comp_ce = 1 - ref_ce,

      ref_icer = NA,
      comp_icer = comp_inc_cost / comp_inc_dalys

    )
  
  ## outcome means and medians of costs and effectieness measures by strategy
  ## means
  mean_df <- cea_df %>%
    group_by(wtp) %>%
    dplyr::summarise(
    dplyr::across(
      dplyr::all_of(
        c("ref_ce", "comp_ce")
      ), 
      mean, 
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    ref_ce = 100 * ref_ce,
    comp_ce = 100 * comp_ce
  )
  
  ## medians
  median_df <- cea_df %>%
    group_by(wtp) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(
          c(
            "ref_cost", "comp_cost",
            "ref_dalys", "comp_dalys",
            "ref_nmb", "comp_nmb", 
            "ref_nhb", "comp_nhb",  
            "ref_inc_cost", "comp_inc_cost",
            "ref_inc_dalys", "comp_inc_dalys",
            "ref_inmb", "comp_inmb",
            "ref_inhb", "comp_inhb"
          )
        ),
        ~ boot_mean_ci(.x, stat = "mean", type = "perc")["mean"],
        .names = "{.col}"
      ),
      .groups = "drop"
    )
  
  ## mean/medians
  mean_median_df <- median_df %>%
    dplyr::left_join(mean_df, by = "wtp")

  # ci for probability of being cost-effective
  ce_df <- cea_df %>%
    dplyr::group_by(wtp) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(
          c("ref_ce", "comp_ce")
        ),
        list(
          lower = ~ wilson_ci(.x)[1] * 100,
          upper = ~ wilson_ci(.x)[2] * 100
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) 
  
  # ci for other economic measures
  ci_df <- cea_df %>%
    dplyr::group_by(wtp) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(c(
          "ref_cost", "comp_cost",
          "ref_dalys", "comp_dalys",
          "ref_nmb", "comp_nmb", 
          "ref_nhb", "comp_nhb",  
          "ref_inc_cost", "comp_inc_cost",
          "ref_inc_dalys", "comp_inc_dalys",
          "ref_inmb", "comp_inmb",
          "ref_inhb", "comp_inhb"
        )),
        list(
          lower = ~ boot_mean_ci(.x, stat = "mean", type = "perc")["lower"],
          upper = ~ boot_mean_ci(.x, stat = "mean", type = "perc")["upper"]
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
    
  # combine
  out_wide <- mean_median_df %>%
    dplyr::left_join(ci_df, by = "wtp") %>%
    dplyr::left_join(ce_df, by = "wtp") 

  out_long <- out_wide %>%
    pivot_longer(
      cols = - wtp,
      names_to = c("strategy", "variable", "stat"),
      names_pattern = "^(ref|comp)_(.*?)(?:_(lower|upper))?$",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      stat = ifelse(is.na(stat), "mean", stat),
      var_stat = dplyr::case_when(
        stat == "mean" ~ variable, 
        TRUE ~ paste(variable, stat, sep = "_")
      ),
      var_stat = sub("_$", "", var_stat)
    ) %>%
    dplyr::select(strategy, var_stat, value)
  
  out_final <- out_long %>%
    tidyr::pivot_wider(
      names_from = var_stat,
      values_from = value
    ) %>%
    mutate(
      strategy = case_when(
        strategy == "ref" ~ "1",
        strategy == "comp" ~ "2",
        TRUE ~ as.character(strategy)
      ) 
    )

  out_final
}


#################################################################################################
## END OF MODULE
#################################################################################################