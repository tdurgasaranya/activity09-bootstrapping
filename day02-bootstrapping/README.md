Day 2 - Bootstrapping
================

In this repository/directory you should see two items:

- `README.md` - this document.
- `activity09.Rmd` - the file you will complete in RStudio for this
  week.

## Task 1: Open the RMarkdown document

Read these directions first, then work through them.

- In the **Files** pane of RStudio, locate and click on the
  `activity09.Rmd` file to open it.
- This file is essentially a blank document with only a `title` and
  `output` option (to produce a GitHub friendly Markdown file). You will
  follow the tasks in this `README` file and do the work (coding,
  responding, etc.) in RStudio.

As you work through this activity, be descriptive in your response to
questions and even leave comments in your code to help you understand
what you are doing. These are your notes to yourself. How can you make
it easier for *future* your to remember what *current* you is
thinking/doing?

## Task 2: Load the necessary packages

Again, we will use two packages from Posit (formerly
[RStudio](https://posit.co/)): `{tidyverse}` and `{tidymodels}`.

- Once you have verified that both `{tidyverse}` and `{tidymodels}` are
  already installed (remember how to do this in the **Packages** pane?),
  load these packages in the R chunk titled `setup`. Press Enter/Return
  after line 7 to add more code lines, then type the following:

  ``` r
  library(tidyverse)
  library(tidymodels)
  ```

- Run the `setup` code chunk or **knit**
  <img src="../README-img/knit-icon.png" alt="knit" width = "20"/> icon
  your Rmd document to verify that no errors occur.

Remember to organize your RMarkdown document using your amazing Markdown
skills 😄

## Task 3: Create the data

To help conceptualize bootstrapping to traditional methods that we
explore earlier this semester, we will create our own dataset. This way,
we will know the truth about the population from which we are drawing
data and can compare how bootstrapping and the traditional methods
compare (and are different).

- Create a new R code chunk and type the following code:

  ``` r
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
  ```

Complete the following tasks:

1.  Go back through the previous code and explain what each line is
    doing by providing a comment. You are provided with an example in
    the first line and places for the rest of that code “sentence”. Do
    this for all the previous code chunk.

2.  What is the true (population-level) model? Note that we are adding
    noise/variability, but based on the above code you can see what the
    “baseline” model is.

3.  Create graphical visualizations for the relationship between all
    variable *pair*s (i.e., `y` and each `x` and also each `x` pair).
    Provide a brief summary of what you see/notice. That is, how do
    these relationships compare with your comments from (1) and model in
    (2)? Especially for the relationship between `y` and each `x`. Hint:
    Do you remember what function/package makes this very easy to
    produce?

## Task 4: Traditional MLR model

First we will fit an estimated model to our simulated data. Recall that
we have done some similar work in past activities, but for ease of
searching I will tell you what to do.

- Create a new R code chunk and type the following code:

  ``` r
  mlr_fit <- linear_reg() %>%
    set_mode("regression") %>% 
    set_engine("lm") %>% 
    fit(y ~ x1 + x2 + x3, data = sim_dat)

  # Also include the confidence intervals for our estimated slope parameters
  tidy(mlr_fit, conf.int = TRUE)
  ```

Answer the following question:

4.  Looking at your population-level model from (2), how accurate are
    your results? Explain how you made this decision. That is, what did
    you use from your output and how did you use that information to
    decide?

## Task 5: Bootstrapping

Now, bootstrapping treats your sample of data as a psuedo-population
that you will create multiple new samples from. Each sample will be of
the same size and same number of variables as the original
($n = 20; p = 3$). However, once a row has been sampled, it will be put
back into the “population” (i.e., sampling with replacement). With
computing power being relatively cheap now, we can do this for a large
number of times to build up $B$ bootstrap samples. For example, we might
do $B = 2,000$ bootstrap samples each of $n = 20$.

- Create a new R code chunk and type the following code:

  ``` r
  # Set a random seed value so we can obtain the same "random" results
  set.seed(631)

  # Generate the 2000 bootstrap samples
  boot_samps <- sim_dat %>% 
    bootstraps(times = 2000)

  boot_samps
  ```

When viewing this outputted object, it probably looks a little odd. This
is a nested data set with two columns:

- `splits`: An `rsplit` object that has two main components: an analysis
  dataset and an assessment dataset.
- `id`: A label of which bootstrap sample it is.

You can view the first analysis dataset by typing the following in your
**Console**:

``` r
boot_samps$splits[[1]] %>% analysis()
```

And the first assessment dataset by typing the following in your
**Console**:

``` r
boot_samps$splits[[1]] %>% assessment()
```

For today, we will only focus on the analysis datasets.

Now we need to fit a linear model to each bootstrap sample. Hopefully,
from STA 518, you remember some of the handy iteration functions from
`{purrr}`.

- Create a new R code chunk and type the following code:

  ``` r
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

We can then calculate the bootstrap intervals by obtaining the
$2.5^{th}$ and $97.5^{th}$ percentiles - similar to a 95% confidence
interval as we are finding the values that contain the middle 95% of the
bootstrap values. Note that we provide the level of significance (1 -
confidence level): $\alpha = 0.05 = 1 - 0.95$.

- Create a new R code chunk and type the following code:

  ``` r
  boot_int <- int_pctl(boot_models, statistics = coef_info, alpha = 0.05)
  boot_int
  ```

I like to also visualize this information to get a sense of the
variability of my estimates.

- Create a new R code chunk and type the following code:

  ``` r
  ggplot(boots_coefs, aes(x = estimate)) +
    geom_histogram(bins = 30) +
    facet_wrap( ~ term, scales = "free") +
    geom_vline(data = boot_int, aes(xintercept = .lower), col = "blue") +
    geom_vline(data = boot_int, aes(xintercept = .upper), col = "blue")
  ```

Answer the following question:

5.  Looking at your population-level model from (2), how accurate are
    your results? Explain how you made this decision. That is, what did
    you use from your output and how did you use that information to
    decide?

### Challenge

Adding to your previous code chunk, add the following:

- A pair of red lines that correspond to the traditional method’s 95%
  confidence intervals
- A single green line that corresponds to the population slope value
  (there should be a unique green line in each plot)

Once you can verify these “simpler” asks, dress your plot up. Use colors
that speak to you. Use themes that speak to you. Make this plot yours!

## What is next?

We will explore cross validation methods.
