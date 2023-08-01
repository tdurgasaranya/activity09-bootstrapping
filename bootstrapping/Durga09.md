---
title: "Activity09"
author: "Durga"
date: "7/31/2023"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

- Load the required libraries/ install packages accordingly

```{r}
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(dplyr)
```



```{r}

# Set a random seed value so we can obtain the same "random" results
set.seed(2023)

# Create a data frame/tibble named sim_dat
sim_dat <- tibble(
# Explain what next line is doing
  x1 = runif(20, -5, 5),
# Explain what next line is doing
  x2 = runif(20, 0, 100),
# Explain what next line is doing
  x3 = rbinom(20, 1, 0.5)
  )

b0 <- 2
b1 <- 0.25
b2 <- -0.5
b3 <- 1
sigma <- 1.5

errors <- rnorm(20, 0, sigma)

sim_dat <- sim_dat %>% 
  mutate(
    y = b0 + b1*x1 + b2*x2 + b3*x3 + errors,
    x3 = case_when(
      x3 == 0 ~ "No",
      TRUE ~ "Yes"
      )
    )

sim_dat
```
- Dimensions : 4 columns, 20 rows

1. `set.seed(2023)`: This sets a random seed value (2023 in this case) to ensure reproducibility of the random numbers generated in subsequent code. Setting a fixed seed allows us to obtain the same "random" results each time the code is run.

2. `sim_dat <- tibble(...)`: This creates a data frame (tibble) named `sim_dat`. It contains three variables: `x1`, `x2`, and `x3`.

3. `x1 = runif(20, -5, 5)`: This generates 20 random numbers from a uniform distribution between -5 and 5 and assigns them to the variable `x1`.

4. `x2 = runif(20, 0, 100)`: This generates 20 random numbers from a uniform distribution between 0 and 100 and assigns them to the variable `x2`.

5. `x3 = rbinom(20, 1, 0.5)`: This generates 20 binary random numbers (0 or 1) with a probability of success (1) being 0.5, and assigns them to the variable `x3`.

6. `b0 <- 2`, `b1 <- 0.25`, `b2 <- -0.5`, `b3 <- 1`, `sigma <- 1.5`: These lines set the values of coefficients (`b0`, `b1`, `b2`, and `b3`) and the standard deviation of the errors (`sigma`) for the linear regression model.

7. `errors <- rnorm(20, 0, sigma)`: This generates 20 random numbers from a normal distribution with mean 0 and standard deviation `sigma` and assigns them to the variable `errors`. These errors will be added to the response variable to create noise in the synthetic data.

8. `sim_dat <- sim_dat %>% mutate(...)`: This modifies the `sim_dat` data frame by adding a new variable `y` and converting `x3` to a categorical variable.

9. `y = b0 + b1*x1 + b2*x2 + b3*x3 + errors`: This creates the response variable `y` based on the linear regression model using the coefficients and variables in `x1`, `x2`, and `x3`, along with the errors.

10. `x3 = case_when(...)`: This converts the binary variable `x3` into a categorical variable with levels "No" and "Yes" based on the values of 0 and 1, respectively.

After running this code, the `sim_dat` data frame will contain the synthetic dataset with variables `x1`, `x2`, `x3`, and `y`, where `y` is generated based on the provided linear regression model and the random errors added to it. The variable `x3` is converted to a categorical variable for easier interpretation.


```{r}

# Install and load the 'cowplot' package
library(cowplot)

# Scatter plots for the relationship between y and each x variable
scatter_y_x1 <- ggplot(sim_dat, aes(x = x1, y = y)) + geom_point() + labs(title = "Scatter Plot: y vs. x1")
plot_grid(scatter_y_x1, ncol = 2)

```


```{r}
scatter_y_x2 <- ggplot(sim_dat, aes(x = x2, y = y)) + geom_point() + labs(title = "Scatter Plot: y vs. x2")

plot_grid(scatter_y_x2, ncol = 2)

```


```{r}

# Bar plot for the relationship between y and x3
bar_y_x3 <- ggplot(sim_dat, aes(x = x3, y = y)) + geom_bar(stat = "summary", fun = "mean") + labs(title = "Mean of y by x3") +
  ylab("Mean of y")

plot_grid(bar_y_x3, ncol = 2)


```


```{r}

# Pairwise scatter plots for the relationship between each x variable pair
scatter_pairwise <- ggplot(sim_dat, aes(x = x1, y = x2)) + geom_point() + labs(title = "Scatter Plot: x1 vs. x2")


plot_grid(scatter_pairwise, ncol = 2)


```

# Summary of Plots:

- **Scatter Plot: y vs. x1**
- This scatter plot shows the relationship between the response variable `y` and the predictor variable `x1`. Each point on the plot represents a data point in the `sim_dat` dataset. The plot helps visualize how `y` changes concerning the values of `x1`. If there is a clear linear relationship between `y` and `x1`, the points will tend to cluster around a straight line.

- **Scatter Plot: y vs. x2**:
- Similar to the first scatter plot, this plot shows the relationship between the response variable `y` and the predictor variable `x2`. The plot helps visualize how `y` changes concerning the values of `x2`. If there is a clear linear relationship between `y` and `x2`, the points will tend to cluster around a straight line.

- **Bar Plot: Mean of y by x3**:
   This bar plot shows the mean of the response variable `y` for each category of the binary predictor variable `x3` (which was converted to "No" and "Yes"). The height of each bar represents the average value of `y` for each category. The plot allows you to compare the mean values of `y` between the "No" and "Yes" categories of `x3`.

- **Scatter Plot: x1 vs. x2**:
   This scatter plot shows the relationship between the predictor variables `x1` and `x2`. Each point represents a data point in the `sim_dat` dataset. The plot helps visualize the association between `x1` and `x2`. If there is a clear linear relationship between the two variables, the points will tend to follow a linear pattern.

These visualizations collectively provide insights into the relationships between different variables in the `sim_dat` dataset. They help identify potential patterns, associations, and correlations between the variables, which can be valuable for understanding the underlying data distribution and identifying any potential trends or dependencies.


```{r}
# Fitting MLR model

mlr_fit <- linear_reg() %>%
  set_mode("regression") %>% 
  set_engine("lm") %>% 
  fit(y ~ x1 + x2 + x3, data = sim_dat)

# confidence intervals for our estimated slope parameters
tidy(mlr_fit, conf.int = TRUE)


```



```{r}

# Set a random seed value so we can obtain the same "random" results
set.seed(631)

# Generate the 2000 bootstrap samples
boot_samps <- sim_dat %>% 
  bootstraps(times = 2000)

boot_samps


```



```{r}

boot_samps$splits[[1]] %>% analysis()

boot_samps$splits[[1]] %>% assessment()

```



```{r}

# Create a function that fits a fixed MLR model to one split dataset
fit_mlr_boots <- function(split) {
  lm(y ~ x1 + x2 + x3, data = analysis(split))
}

# Fit the model to each split and store the information
# Also, obtain the tidy model information
boot_models <- boot_samps %>% 
  mutate(
    model = map(splits, fit_mlr_boots),
    coef_info = map(model, tidy)
    )

boots_coefs <- boot_models %>% 
  unnest(coef_info)

boots_coefs

```


```{r}

boot_int <- int_pctl(boot_models, statistics = coef_info, alpha = 0.05)
boot_int

```



```{r}

ggplot(boots_coefs, aes(x = estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(data = boot_int, aes(xintercept = .lower), col = "blue") +
  geom_vline(data = boot_int, aes(xintercept = .upper), col = "blue")

```


