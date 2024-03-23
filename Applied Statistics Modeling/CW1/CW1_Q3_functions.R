max_balance_probability <- function(datasource, model) {
  # Find the maximum balance value
  max_balance <- max(datasource$balance)
  
  # Get the row(s) with the maximum balance
  row_with_max_balance <- datasource[datasource$balance == max_balance, ]
  
  # Remove the 'default' column
  row_with_max_balance$default <- NULL
  
  # Predict probabilities for the new data
  new_predicted_probabilities <- predict(model, newdata = row_with_max_balance, type = "response")
  
  # Return the predicted probabilities for the new data
  return(new_predicted_probabilities)
}

max_probability <- function(datasource, model) {
  # Predict probabilities for the entire dataset
  predicted_probabilities <- predict(model, type = "response")
  
  # Find the maximum predicted default probability
  max_predicted_default_probability <- max(predicted_probabilities)
  
  # Return the maximum predicted default probability
  return(max_predicted_default_probability)
}

mbp <- function(datasource, model) {
  # Predict probabilities for the entire dataset
  predicted_probabilities <- predict(model, type = "response")
  
  # Find the maximum predicted default probability
  max_predicted_default_probability <- max(predicted_probabilities)
  
  # Return the maximum predicted default probability
  return(max_predicted_default_probability)
}
