## Is this data normally distributed?

```{r}

# functions for simulation
generate_data <- function(n,
                          mean,
                          sd) {
  
  dat <- tibble(score = rnorm(n = n, mean = mean, sd = sd))
  
  return(dat)
}

library(moments) # For skewness() and kurtosis()

analyze <- function(data) {
  
  res <- data |>
    summarize(
      n = n(),
      sample_sd = sd(score, na.rm = TRUE),
      # Calculate BC components
      skw = moments::skewness(score, na.rm = TRUE),
      krt = moments::kurtosis(score, na.rm = TRUE),
      # Bimodality Coefficient Formula
      bimodality_coef = (skw^2 + 1) / (krt + 3 * ((n - 1)^2 / ((n - 2) * (n - 3))))
    ) |>
    # Clean up intermediate steps
    select(sample_sd, bimodality_coef)
  
  return(res)
}

# simulation parameters
experiment_parameters <- expand_grid(
  n = 100,
  population_mean = 0,
  population_sd = 1,
  iteration = 1:1000
)

# set seed
set.seed(42)

# run simulation
simulation <- experiment_parameters |>
  mutate(generated_data = pmap(list(n = n, 
                                    mean = population_mean,
                                    sd = population_sd),
                               generate_data)) |>
  mutate(results = pmap(list(data = generated_data),
                        analyze)) |>
  # unnest results
  unnest(results) |>
  arrange(desc(bimodality_coef))

# # table of results
# simulation |>
#   kable() |>
#   kable_classic(full_width = FALSE)

ggplot(simulation$generated_data[[1]], aes(score)) +
  geom_density()

ggplot(simulation$generated_data[[2]], aes(score)) +
  geom_density()

ggplot(simulation$generated_data[[3]], aes(score)) +
  geom_density()

ggplot(simulation$generated_data[[7]], aes(score)) +
  geom_density()

simulation |>
  arrange(desc(sample_sd)) %>%
  ggplot(data = .$generated_data[[1]], aes(score)) +
  geom_density()

```