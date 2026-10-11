# Class-adjusted category means with normal SD/sqrt(n) intervals.
class_fe_category_summary <- function(data, value, category, reference) {
  groups <- levels(data[[category]])
  data[[category]] <- relevel(data[[category]], ref = reference)
  X <- model.matrix(reformulate(category), data)[, -1, drop = FALSE]
  fit <- fixest::feols(as.formula(paste(value, "~", category, "| class")),
    data = data, vcov = ~class,
    ssc = fixest::ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, t.df = "min"))
  beta <- coef(fit)
  stopifnot(nobs(fit) == nrow(data), identical(colnames(X), names(beta)))
  cluster <- match(data$class, unique(data$class))
  G <- max(cluster)
  class_n <- tabulate(cluster, nbins = G)
  class_X <- rowsum(X, cluster, reorder = FALSE)
  class_X_mean <- class_X / class_n
  class_y <- drop(rowsum(data[[value]], cluster, reorder = FALSE))
  alpha <- class_y / class_n - drop(class_X_mean %*% beta)
  alpha_mean <- sum(class_n * alpha) / nrow(data)
  adjusted <- data[[value]] - alpha[cluster] + alpha_mean
  category_X <- X[match(groups, as.character(data[[category]])), , drop = FALSE]
  rownames(category_X) <- groups
  category_means <- alpha_mean + drop(category_X %*% beta)
  stopifnot(max(abs(tapply(adjusted, data[[category]], mean)[groups] - category_means)) < 1e-10,
            max(abs(data[[value]] - (alpha[cluster] + drop(X %*% beta)) - resid(fit))) < 1e-10)

  stats <- data.frame(category = factor(groups, levels = groups), mean = category_means,
    sd = unname(tapply(adjusted, data[[category]], sd)[groups]),
    n = as.integer(table(data[[category]])[groups]), clusters = G, df = G - 1L)
  stats$se <- stats$sd / sqrt(stats$n)
  stats$low <- stats$mean - qnorm(.975) * stats$se
  stats$high <- stats$mean + qnorm(.975) * stats$se
  stats$mean_inference <- "Class-adjusted category means; normal CI using SD/sqrt(n)"
  list(fit = fit, stats = stats, adjusted = adjusted, category_X = category_X)
}
