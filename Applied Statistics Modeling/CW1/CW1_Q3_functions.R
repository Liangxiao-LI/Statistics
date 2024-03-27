max_balance_probability <- function(datasource, model) {
  # Find the maximum balance value
  max_balance <- max(datasource$balance)
  
  # Get the row(s) with the maximum balance
  index_with_max_balance <- which(datasource$balance == max_balance)
  row_with_max_balance <- datasource[datasource$balance == max_balance, ]
  
  # Remove the 'default' column
  row_with_max_balance$default <- NULL
  
  # Predict probabilities for the new data
  new_predicted_probabilities <- predict(model, newdata = row_with_max_balance, type = "response")
  
  # Return the results
  results <- data.frame(index = index_with_max_balance, predicted_probabilities = new_predicted_probabilities)
  return(results)
}

max_probability <- function(datasource, model) {
  # Predict probabilities for the entire dataset
  predicted_probabilities <- predict(model,newdata = datasource, type = "response")
  
  # Find the maximum predicted default probability
  max_predicted_default_probability <- max(predicted_probabilities)
  
  # Get the row(s) with the maximum balance
  index_of_max_probability <- which.max(predicted_probabilities)
  
  # Return the maximum predicted default probability
  results <- data.frame(index = index_of_max_probability, max_probabilities = max_predicted_default_probability)
  return(results)
}

