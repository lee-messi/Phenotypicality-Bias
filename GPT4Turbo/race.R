
## Anonymous
# Skin Tone Homogeneity Bias in Vision Language Models

## Script date: 20 Nov 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("lme4")){install.packages("lme4", dependencies = TRUE); require("lme4")}
if(!require("lmerTest")){install.packages("lmerTest", dependencies = TRUE); require("lmerTest")}
if(!require("effsize")){install.packages("effsize", dependencies = TRUE); require("effsize")}
if(!require("afex")){install.packages("afex", dependencies = TRUE); require("afex")}
if(!require("emmeans")){install.packages("emmeans", dependencies = TRUE); require("emmeans")}

# Load data --------------------------------------------------------------------

gpt4turbo = read.csv('gpt4turbo_cosines.csv') %>%
  mutate(gender = substr(condition, 1, 1)) %>% 
  mutate(black = substr(condition, 2, 2)) %>%
  mutate(gender = as.factor(gender), 
         blackness = as.factor(blackness)) %>% 
  mutate(pair_id_1 = sapply(str_split(image_1, "-"), `[`, 1)) %>% 
  mutate(pair_id_2 = sapply(str_split(image_2, "-"), `[`, 1)) %>% 
  mutate(pair_id = paste(pair_id_1, pair_id_2, sep = "-")) %>% 
  mutate(pair_id = as.factor(pair_id)) %>%
  mutate(cosine = as.numeric(scale(cosine)))

# Fit three fixed-effects models -----------------------------------------------

gpt4turbo.blackness <- lmer(cosine ~ 1 + blackness + (1 | pair_id), 
                           data = gpt4turbo,
                           control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(gpt4turbo.blackness)
logLik(gpt4turbo.blackness)

gpt4turbo.gender <- lmer(cosine ~ 1 + blackness + (1 | pair_id), 
                         data = gpt4turbo,
                         control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(gpt4turbo.gender)
logLik(gpt4turbo.gender)

gpt4turbo.blackness.gender <- lmer(cosine ~ 1 + blackness * gender + (1 | pair_id), 
                                  data = gpt4turbo,
                                  control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(gpt4turbo.blackness.gender)
logLik(gpt4turbo.blackness.gender)

# Simple slopes ----------------------------------------------------------------

summary(emmeans(gpt4turbo.blackness.gender, ~ blackness))

# Likelihood ratio tests -------------------------------------------------------

mixed(cosine ~ 1 + blackness * gender + (1 | pair_id), 
      data = gpt4turbo, 
      control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE),
      method = "LRT")
