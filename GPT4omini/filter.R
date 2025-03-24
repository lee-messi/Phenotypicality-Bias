
## Anonymous
# Skin Tone Homogeneity Bias in Vision Language Models

## Script date: 16 Mar 2025

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("lme4")){install.packages("lme4", dependencies = TRUE); require("lme4")}
if(!require("lmerTest")){install.packages("lmerTest", dependencies = TRUE); require("lmerTest")}
if(!require("effsize")){install.packages("effsize", dependencies = TRUE); require("effsize")}
if(!require("afex")){install.packages("afex", dependencies = TRUE); require("afex")}
if(!require("emmeans")){install.packages("emmeans", dependencies = TRUE); require("emmeans")}

# Load data --------------------------------------------------------------------

gpt4omini = read.csv('gpt4omini.csv') %>% 
  mutate(gender = substr(condition, 1, 1)) %>% 
  mutate(blackness = substr(condition, 2, 2)) %>%
  mutate(gender = as.factor(gender), 
         blackness = as.factor(blackness))


