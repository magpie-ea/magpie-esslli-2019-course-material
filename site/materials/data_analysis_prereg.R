library(tidyverse)
library(brms)
options(mc.cores = parallel::detectCores())
library(faintr)
# install this package using devtools package:
# devtools::install_github(repo = "michael-franke/bayes_mixed_regression_tutorial",subdir = "faintr")
# see https://psyarxiv.com/cdxv3 for details about the 'faintr' package


#######################################
## read and prepare the data
#######################################

d = read_csv('data_raw_final.csv') %>% 
  # remove test run by the experimenter
  filter(is.na(comments) | comments != "TEST BY MF" ) %>% 
  # extract information to use for analysis
  mutate(
    # what was the "rating" / "group" for each participant?
    rating = ifelse(stringr::str_detect(optionLeft, "likely"), "cond_probability", "acceptability"),
    # what was the conditional-type?
    cond_type = factor(condition, levels = c("II", "AI", "DI"), ordered = T),
    # consecutively number the items
    item_nr = factor(QUD) %>% as.integer() %>% factor()
  )

#######################################
## data cleaning, following 
## pre-registered exclusion protocol
#######################################

# remove all participants who responded in less than 3 
# seconds to more than 2 trials
d = d %>% group_by(submission_id) %>% 
  mutate(subj_too_fast = sum(RT<3000) > 2) %>% 
  ungroup()

message("Removed all data from ", 
        filter(d, subj_too_fast == T) %>% pull(submission_id) %>% unique() %>% length(), 
        " participants for being too fast on more than 2 trials.")

d = filter(d, subj_too_fast == F)

# remove all trials faster than 3 seconds
d = d %>% 
  mutate(trial_too_fast = RT < 3000)
  
message("Removed data from ", 
        filter(d, trial_too_fast == T) %>% nrow(), 
        " trial for being too fast.")

d = filter(d, trial_too_fast == F)
#######################################
## plot average ratings
#######################################

# get aggregates
d_means = d %>% group_by(rating, cond_type) %>% 
  summarize(response = mean(response))

d_means %>% ggplot(aes(x = cond_type, y = response, fill = rating)) +
  geom_bar(stat = "identity", position = "dodge") 

#######################################
## H1: (near-)identity reading of AT
#######################################

fit_h1 = brm(
  formula = response ~ rating * cond_type +
    (1 | submission_id + item_nr),
  data = d %>% mutate(cond_type = case_when(condition == "AI" ~ "x_AI",
                                            condition == "II" ~ "x_II",
                                            TRUE ~ "DI")),
  prior = c(prior(normal(0, 10), class = b))
)

summary(fit_h1)

# check for main effect of RATING factor
faintr::compare_groups(
  model = fit_h1,
  lower = list(rating = "acceptability"),
  higher = list(rating = "cond_probability")
)

# check for effect of RATING factor on each conditional type

# deductive
faintr::compare_groups(
  model = fit_h1,
  lower = list(rating = "acceptability", cond_type = "DI"),
  higher = list(rating = "cond_probability", cond_type = "DI")
)

# abductive
faintr::compare_groups(
  model = fit_h1,
  lower = list(rating = "acceptability", cond_type = "x_AI"),
  higher = list(rating = "cond_probability", cond_type = "x_AI")
)

# inductive
faintr::compare_groups(
  model = fit_h1,
  lower = list(rating = "acceptability", cond_type = "x_II"),
  higher = list(rating = "cond_probability", cond_type = "x_II")
)


#######################################
## H2: correlation reading of AT
#######################################

# correlation analysis
d_cor = d %>% group_by(item_nr, cond_type, rating) %>% 
  summarize(response = mean(response)) %>% 
  spread(key = "rating", value = "response")
  ungroup()

# plot the correlation
d_cor %>% ggplot(aes(x = acceptability, y = cond_probability, color = cond_type)) +
  geom_point() + geom_smooth(method = "lm")

# test H2 with linear regression model

# all data
fit_h2 = brm(
  formula = acceptability ~ cond_probability,
  data = d_cor,
  prior = c(prior(normal(0, 10), class = b),
            prior(normal(1, 10), coef = "cond_probability"))
)

summary(fit_h2)

# deductive conditionals
fit_h2_type_DI = brm(
  formula = acceptability ~ cond_probability,
  data = d_cor %>% filter(cond_type == "DI"),
  prior = c(prior(normal(0, 10), class = b),
            prior(normal(1, 10), coef = "cond_probability"))
)
summary(fit_h2_type_DI)

# abuctive conditionals
fit_h2_type_AI = brm(
  formula = acceptability ~ cond_probability,
  data = d_cor %>% filter(cond_type == "AI"),
  prior = c(prior(normal(0, 10), class = b),
            prior(normal(1, 10), coef = "cond_probability"))
)
summary(fit_h2_type_AI)

# inductive conditionals
fit_h2_type_II = brm(
  formula = acceptability ~ cond_probability,
  data = d_cor %>% filter(cond_type == "II"),
  prior = c(prior(normal(0, 10), class = b),
            prior(normal(1, 10), coef = "cond_probability"))
)
summary(fit_h2_type_II)



