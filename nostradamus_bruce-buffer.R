# Ensure necessary packages are installed and loaded
packages <- c("tidyverse", "caret", "pROC", "ggplot2", "doParallel", "brms", "ROSE", "doParallel")
sapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

#enable all cores but one
numCores <- detectCores() - 1
registerDoParallel(cores=numCores)

# Load and prepare data
fight_data <- read.csv('data.csv', stringsAsFactors = FALSE)
fight_data <- fight_data %>%
  select(-c(3:5, 7:8))
fight_data <- fight_data %>%
  select(-B_draw, -R_draw)
# Assuming 'Winner' is the target variable and that 'Red' indicates a win
fight_data$winner_binary <- ifelse(fight_data$Winner == 'Red', 1, 0)
colnames(fight_data)

# Assuming 'winner_binary' is your target variable in 'training_data'
# And assuming "0" represents the minority class and "1" the majority class
count_minority <- sum(fight_data$winner_binary == "0")
count_majority <- sum(fight_data$winner_binary == "1")

# Ideally, you want to increase the minority class to match the majority class size
# This is the correct N for balancing the classes
desired_N <- 2 * count_majority  # Target total dataset size after oversampling

#Perform oversampling to achieve a balanced class distribution
fight_data <- ovun.sample(winner_binary ~ ., data = fight_data, method = "over", p = 0.5, seed = 123)$data

# Ensure reproducibility
set.seed(123)

# Split data into training and testing sets
training_indices <- createDataPartition(fight_data$winner_binary, p = .8, list = FALSE)
training_data <- fight_data[training_indices, ]
testing_data <- fight_data[-training_indices, ]

# Convert the outcome variable to a factor and remove unused levels
training_data$winner_binary <- factor(training_data$winner_binary)
training_data$winner_binary <- droplevels(training_data$winner_binary)

# Identify all variable names except the target, fighter names, and any other variables you wish to exclude
independent_variables <- setdiff(names(training_data), 
                                 c("winner_binary", "R_fighter", "B_fighter", "Winner"))

# Construct the formula by collapsing the variable names into a single string
formula_str <- paste("winner_binary ~", paste(independent_variables, collapse = " + "))

# Convert the string to a formula object
model_formula <- as.formula(formula_str)


# Set up train control with more robust cross-validation
set.seed(123)  # For reproducibility
train_control <- trainControl(method = "repeatedcv", number = 3, repeats = 2, search = "grid", allowParallel = TRUE, verboseIter = TRUE)
# Prepare a tuning grid for Random Forest
tuneGrid <- expand.grid(.mtry = c(2, sqrt(ncol(training_data)-1), ncol(training_data)-1))
# Scale and center numeric predictors correctly specified
preProcValues <- c("center", "scale")
tuneGrid <- expand.grid(.mtry = c(2, sqrt(length(independent_variables)), length(independent_variables)))

#train model
ml_model <- train(model_formula, 
                  data = training_data, 
                  method = "rf",  
                  trControl = train_control,
                  tuneGrid = tuneGrid,
                  preProcess = c("center", "scale"))


# Assuming 'fight_data' is your full dataset with historical fighter stats
# 'ml_model' is your trained machine learning model ready for making predictions

# Function to prompt manual entry of fighter stats
library(dplyr)
library(tibble)


# Function to manually enter fighter stats if not found
manual_entry_function <- function(fighter_name) {
  cat(paste("Enter stats for", fighter_name, "\n"))
  
  height_cms <- as.numeric(readline(prompt = "Enter fighter height (cms): "))
  weight_lbs <- as.numeric(readline(prompt = "Enter fighter weight (lbs): "))
  reach_cms <- as.numeric(readline(prompt = "Enter fighter reach (cms): "))
  wins <- as.numeric(readline(prompt = "Enter number of wins: "))
  losses <- as.numeric(readline(prompt = "Enter number of losses: "))
  
return(tibble(height_cms = height_cms, weight_lbs = weight_lbs, reach_cms = reach_cms,
                wins = wins, losses = losses))
}

# Function to predict fight outcome between two fighters
predict_fight <- function(fight_data, ml_model, fighterA_name, fighterB_name) {
  # Placeholder for filtering logic; adjust based on your actual dataset
  fighterA_stats <- fight_data %>% filter(R_fighter == fighterA_name | B_fighter == fighterA_name)
  fighterB_stats <- fight_data %>% filter(R_fighter == fighterB_name | B_fighter == fighterB_name)
  
  if(nrow(fighterA_stats) == 0) {
    cat("Fighter A stats not found. Please enter manually.\n")
    fighterA_stats <- manual_entry_function(fighterA_name)
  }
  
  if(nrow(fighterB_stats) == 0) {
    cat("Fighter B stats not found. Please enter manually.\n")
    fighterB_stats <- manual_entry_function(fighterB_name)
  }
  
  combined_stats <- bind_rows(fighterA_stats, fighterB_stats) %>%
    select(-R_fighter, -B_fighter) # Ensure this matches the structure expected by your model
  
  if(nrow(combined_stats) == 0) {
    stop("No data available for prediction.")
  }
  
  prediction <- predict(ml_model, newdata = combined_stats, type = "prob")
  return(prediction)
}

# Ensure 'fight_data' and 'ml_model' are correctly defined and available in your environment before running this

result <- predict_fight(fight_data, ml_model, "CM Punk", "Mike Perry")
print(result)

# Convert to long format
result_long <- result %>% 
  pivot_longer(cols = everything(), names_to = "Outcome", values_to = "Probability")

# Calculate summary statistics for each outcome
summary_stats <- result_long %>%
  group_by(Outcome) %>%
  summarise(Avg_Probability = mean(Probability),
            SD = sd(Probability),
            Lower_CI = Avg_Probability - 1.96 * SD / sqrt(n()),
            Upper_CI = Avg_Probability + 1.96 * SD / sqrt(n()))

summary_stats

# Plot
# UFC-themed colors
ufc_red <- "#C1272D"
ufc_black <- "#231F20"
ufc_silver <- "#B7B7B8"
ufc_gold <- "#FFD700"  # Bright gold for better visibility

library(ggplot2)

library(ggplot2)

# Assuming 'result_long' contains individual probabilities (restructured as necessary)
# and 'summary_stats' contains the aggregated statistics for each outcome

# Plot
library(ggplot2)

# Plot
ggplot() +
  # Draw individual data points in red
  geom_point(data = result_long, aes(x = Outcome, y = Probability), 
             position = position_jitter(width = 0.1), alpha = 0.5, color = "red") +
  # Draw error bars for confidence intervals from summary statistics in gold
  geom_errorbar(data = summary_stats, aes(x = Outcome, y = Avg_Probability, 
                                          ymin = Lower_CI, ymax = Upper_CI),
                width = 0.2, size = 1, color = "#FFD700") +
  # Add points for average probabilities on top of error bars in gold
  geom_point(data = summary_stats, aes(x = Outcome, y = Avg_Probability), 
             size = 4, color = "#FFD700") +
  # Add labels for average probabilities next to the error bars in gold
  geom_text(data = summary_stats, aes(x = Outcome, y = Upper_CI, label = sprintf("%.3f", Avg_Probability)), 
            vjust = -0.5, nudge_y = 0.02, nudge_x = .1, color = "#FFD700") +
  # Customize the plot
  labs(x = "Outcome", y = "Probability", title = "Predicted Probabilities with Confidence Intervals") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#231F20", color = "#231F20"),
        panel.background = element_rect(fill = "#231F20"),
        text = element_text(color = "#B7B7B8"),
        plot.title = element_text(color = "#FFD700", size = 16, face = "bold"),
        axis.title = element_text(color = "#B7B7B8"),
        axis.text = element_text(color = "#B7B7B8"),
        legend.position = "none")

#### Bayesian analysis

# Load necessary libraries
library(tidyverse)
library(caret)
library(brms) # For Bayesian modeling
library(bayesplot) # For plotting Bayesian analysis results

prior <- c(
  set_prior("normal(0, 1)", class = "b"),  # Regularization for coefficients
  set_prior("cauchy(0, 2)", class = "Intercept")  # Less regularization for the intercept
)

# Fit the Bayesian model with regularization
bayesian_model <- brm(model_formula,
  data = training_data,
  family = bernoulli(),
  prior = prior,
  chains = 2,
  iter = 2000,
  warmup = 500,
  seed = 123,
  control = list(adapt_delta = 0.95)  # Adjust for convergence, if necessary
)

# Summary of the Bayesian model
print(summary(bayesian_model))

# Plotting the results
# Posterior distributions of the coefficients
plot1 <- plot(bayesian_model, ask = FALSE)

# Posterior predictive checks
pp_check(bayesian_model)

### get or prompt stats

get_or_prompt_fighter_stats <- function(fighter_name, data) {
  # Attempt to retrieve stats by name; define this function based on your dataset
  # Placeholder for demonstration
}

predict_fight_bayesian <- function(fighterA_name, fighterB_name, model, data) {
  fighterA_stats <- get_or_prompt_fighter_stats(fighterA_name, data)
  fighterB_stats <- get_or_prompt_fighter_stats(fighterB_name, data)
  
  # Combine stats for prediction
  input_data <- rbind(fighterA_stats, fighterB_stats) %>% 
    mutate(across(everything(), as.numeric))
  
  # Generating posterior predictive distributions for the input data
  posterior_predictive <- posterior_predict(model, newdata = input_data, allow_new_levels = TRUE)
  
  # Calculate and return the mean probability of fighter1 winning
  mean_probability_fighter1_wins <- mean(posterior_predictive[, 1])
  return(mean_probability_fighter1_wins)
}

outcome_bayesian <- predict_fight_bayesian("Fighter A Name", "Fighter B Name", bayesian_model, training_data)
print(outcome_bayesian)



### more graphs

# Assuming 'bayesian_model' is your fitted model and you have a binary outcome
# Here's how to correctly generate and visualize the predicted probabilities

expected_probs <- posterior_epred(bayesian_model, newdata = fighter1)

# Calculate the mean probability across all posterior draws
mean_probs <- rowMeans(expected_probs)

# Plotting
probabilities_df <- tibble(Probability = mean_probs)
ggplot(probabilities_df, aes(x = Probability)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Expected Probability Distribution for Fighter 1 Winning",
       x = "Probability",
       y = "Frequency") +
  theme_minimal()

mean_prob_fighter1 <- mean(mean_probs) # Example mean probability


# Enhanced UFC-themed graph with improved visibility for the prediction line and text
ggplot(probabilities_df, aes(x = Probability)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = ufc_red, color = ufc_black) +
  geom_vline(xintercept = 0.5, color = ufc_silver, linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean_prob_fighter1, color = ufc_gold, size = 1) +  # Changed to gold for visibility
  geom_density(alpha = 0.2, fill = ufc_black) +
  annotate("text", x = mean_prob_fighter1, y = max(density(probabilities_df$Probability)$y), 
           label = paste("Prediction:", round(mean_prob_fighter1 * 100, 2), "%"), 
           hjust = 1.5, vjust = 1, color = ufc_gold, size = 5) +  # Text color changed to gold
  scale_x_continuous(labels = scales::percent_format()) +
  labs(title = "Probability Distribution of Fighter 1 Winning",
       subtitle = paste("Mean Predicted Probability:", scales::percent(mean_prob_fighter1)),
       x = "Probability of Winning",
       y = "Density") +
  theme_minimal() +
  theme(text = element_text(color = ufc_silver),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = ufc_silver),
        plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5, color = ufc_silver),
        axis.title = element_text(size = 14, face = "bold", color = ufc_silver),
        axis.text = element_text(size = 12, color = ufc_silver),
        panel.background = element_rect(fill = ufc_black, color = ufc_black),
        plot.background = element_rect(fill = ufc_black, color = ufc_black),
        legend.background = element_rect(fill = ufc_black, color = ufc_black),
        legend.text = element_text(color = ufc_silver))



