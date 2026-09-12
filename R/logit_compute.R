# Classification Matrix of a logit model
logit_classification <- function(log_model, predictions = NULL, threshold = 0.5) {
  if (is.null(predictions)) {
    predictions <- predict(log_model, newdata = log_model$data, type = "response")
  }

  outcomes_df <- data.frame(
    actuals = log_model$y,
    predicted = ifelse(predictions >= threshold, 1, 0))

  class_table <- xtabs(~ predicted + actuals, data = outcomes_df)
  correct <- sum(diag(class_table))
  total <- sum(class_table)
  accuracy <- (100 * correct / total) |> round(2) |> paste("%", sep = "")
  perf <- performance(threshold, predictions, outcomes_df$actuals)

  list(
    class_table = class_table,
    correct = correct,
    total = total,
    accuracy = accuracy,
    performance = perf
  )
}

# pseudo-R^2 for any model given actual binary Y and model probabilities of Y
pseudo_rsq <- function(log_model) {
  Y <- log_model$y
  Yprob <- log_model$fitted.values
  n <- length(Y)
  Ytab <- table(Y)

  LL <- log_likelihood(Y, Yprob)             # -105.0325
  bLL <- log_likelihood(Y, baseline_PofY(Y)) # -133.6794

  LR <- -2*bLL - -2*LL

  cs_rsq <- 1 - exp((-2*LL - -2*bLL)/n)       # 0.19914
  n_rsq <- cs_rsq / (1 - exp(-(-2*bLL/n)))    # 0.30863

  list(
    LL = LL,
    bLL = bLL,
    cs_rsq = cs_rsq,
    n_rsq = n_rsq
  )
}

log_likelihood <- function(Y, PofY) {
  sum( Y*log(PofY) + (1 - Y)*(log(1 - PofY)) )
}

baseline_PofY <- function(Y) {
  Ytab <- table(Y)
  Ytab[[2]] / sum(Ytab) # num of cases of 1 over total num of cases
}


## ROC Functions

insample_logit_performance <- function(log_model, classfxn_data = NULL) {
  if (is.null(classfxn_data)) classfxn_data <- log_model$data
  dv <- as.character(log_model$terms[[2]])
  preds <- predict(log_model, newdata = classfxn_data,
                   type = "response") |> unname()
  roc_curve(actuals = classfxn_data[, dv], predictions = preds)
}

oosample_logit_performance <- function(log_model) {
  outcomes <- k_fold(log_model)
  roc_curve(outcomes$actuals, outcomes$predictions)
}

roc_curve <- function(actuals, predictions) {
  threshholds <- predictions |> sort() |> unique()
  threshholds <- c(0, threshholds, 1)
  sapply(threshholds, performance,
         preds = predictions, actuals = actuals) |> t()
}

logit_performance_point <- function(threshold, log_model, predictions = NULL, classfxn_data = NULL) {
  dv <- as.character(log_model$terms[[2]])
  if (is.null(classfxn_data)) classfxn_data <- log_model$data

  if (is.null(predictions)) predictions <- predict(log_model, newdata = classfxn_data, type="response")
  performance(threshold, preds = predictions, actuals = classfxn_data[, dv]) |> t()
}

performance <- function(threshold, preds, actuals) {
  thresh_pred_true <- ifelse(preds >= threshold, TRUE, FALSE)
  actual_trues <- actuals == 1
  actual_falses <- actuals == 0
  tp <- (thresh_pred_true) & (actual_trues)
  fp <- (thresh_pred_true) & (actual_falses)
  c(fpr = sum(fp) / sum(actual_falses),
    tpr = sum(tp) / sum(actual_trues),
    threshold = threshold)
}

## K=fold Cross Validation

k_fold <- function(log_model, k = NULL) {
  if (is.null(k)) k <- nrow(log_model$data)
  cases <- nrow(log_model$data) |> seq_len()
  shuffled_indices <- cases |> sample()
  dataset <- log_model$data[shuffled_indices, ]
  actuals <- log_model$y[shuffled_indices]

  folds <- cut(cases, breaks = k, labels = FALSE)

  kfold_predictions <-
    sapply(1:k, \(kth) { fold_predict(kth, folds, log_model, dataset) }) |>
    unlist()

  results <- data.frame(actuals, predictions = kfold_predictions)

  # unshuffle indices and return results
  results[order(as.numeric(row.names(results))), ]
}

 # fold_predict(1, folds, log_model, dataset)
fold_predict <- function(kth, folds, log_model, dataset) {
  test_indices <- which(folds == kth)
  test_set  <- dataset[test_indices, ]
  train_set <- dataset[-test_indices, ]
  trained_model <- update(log_model, data = train_set)

  predict(trained_model, newdata = test_set, type = "response")
}
