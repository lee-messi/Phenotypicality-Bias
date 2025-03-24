
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

gpt4omini = read.csv('gpt4omini_cosines.csv') %>%
  mutate(gender = substr(condition, 1, 1)) %>% 
  mutate(blackness = substr(condition, 2, 2)) %>%
  mutate(gender = as.factor(gender), 
         blackness = as.factor(blackness)) %>% 
  mutate(pair_id_1 = sapply(str_split(image_1, "-"), `[`, 1)) %>% 
  mutate(pair_id_2 = sapply(str_split(image_2, "-"), `[`, 1)) %>% 
  mutate(pair_id = paste(pair_id_1, pair_id_2, sep = "-")) %>% 
  mutate(pair_id = as.factor(pair_id)) %>%
  mutate(cosine = as.numeric(scale(cosine)))

# Fit three fixed-effects models -----------------------------------------------

gpt4omini.blackness <- lmer(cosine ~ 1 + blackness + (1 | pair_id), 
                           data = gpt4omini,
                           control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(gpt4omini.blackness)
logLik(gpt4omini.blackness)

gpt4omini.gender <- lmer(cosine ~ 1 + gender + (1 | pair_id), 
                         data = gpt4omini,
                         control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(gpt4omini.gender)
logLik(gpt4omini.gender)

gpt4omini.blackness.gender <- lmer(cosine ~ 1 + blackness * gender + (1 | pair_id), 
                                  data = gpt4omini,
                                  control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(gpt4omini.blackness.gender)
logLik(gpt4omini.blackness.gender)

# Simple slopes ----------------------------------------------------------------

summary(emmeans(gpt4omini.blackness.gender, ~ blackness))

# Likelihood ratio tests -------------------------------------------------------

mixed(cosine ~ 1 + blackness * gender + (1 | pair_id), 
      data = gpt4omini, 
      control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE),
      method = "LRT")

