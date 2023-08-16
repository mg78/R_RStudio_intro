# Practical session on R and RStudio

# Step 1: Loading packages -------------------
# Check relevant packages are installed, if not install them. Only have to do this once.
install.packages("here")
install.packages("tidyverse")
install.packages("skimr")
install.packages("wordbankr")

## Having difficulty installing wordbankr, install directly from github
install.packages("devtools")
devtools::install_github("langcog/wordbankr")

# Loading packages (have to do this every time you start a new R session)
library(here)
library(tidyverse)
library(skimr)
library(wordbankr)

# Step 2: Read in the data --------------------
voc_cantonese <- get_administration_data(language = "Cantonese", form = "WS")

# Step 3: Exploring the data -------------------
dim(voc_cantonese)
str(voc_cantonese)
glimpse(voc_cantonese)
head(voc_cantonese)
tail(voc_cantonese)
skim(voc_cantonese)
summary(voc_cantonese)

# Step 4: Select relevant variables --------------------------
voc_cantonese_select <- select(voc_cantonese, age, language, sex, production)

voc_cantonese_select <- voc_cantonese %>%
  select(age, language, sex, production) %>%
  write_csv(here('voc_cantonese_select.csv'))

voc_cantonese_select <- read_csv(here('voc_cantonese_select.csv'))
#voc_cantonese_select <- read_csv(here("docs/files","voc_cantonese_select.csv"))
                                 
# Step 5: Visualise the data ---------------------

# Histogram for age
ggplot(voc_cantonese_select, aes(x = age)) + #Tell R what data to use in the first argument, then specify variable you want to plot in x = 
  geom_histogram() +                         #The geom 'geom_histrogram' tells R how to plot the data
  labs(x = "Age (in months)") +              #labs() can be used to label axes properly
  theme_bw()                                 #There are lots of different 'themes' in ggplot that give you options in terms of the look of the plot

# 5B: Histogram for productive vocabulary 
ggplot(voc_cantonese_select, aes(x = production)) + 
  geom_histogram() +
  labs(x = "Productive vocabulary") +
  theme_bw()

# Scatterplot for productive vocabulary as a function of age
ggplot(voc_cantonese_select, aes(x = age, y = production)) +
  geom_point() +
  theme_bw() +
  labs(x = "Age (in months)", y = "Productive vocabulary")

# Add geom_jitter() function 
ggplot(voc_cantonese_select, aes(x = age, y = production)) +
  geom_jitter() +
  theme_bw() +
  labs(x = "Age (in months)", y = "Productive vocabulary")

# Add alpha
ggplot(voc_cantonese_select, aes(x = age, y = production)) +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(x = "Age (in months)", y = "Productive vocabulary") 

# Add colour for sex
ggplot(voc_cantonese_select, aes(x = age, y = production, colour = sex)) +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(x = "Age (in months)", y = "Productive vocabulary") 

# Add lines of best fit 
ggplot(voc_cantonese_select, aes(x = age, y = production, colour = sex)) +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = 'lm', se = TRUE) +
  theme_bw() +
  labs(x = "Age (in months)", y = "Productive vocabulary")

# Step 6: Model the data ---------------------
# Descriptive statistics 
descriptives <- voc_cantonese_select %>%
  summarise(mean_voc = mean(production, na.rm = TRUE),
            sd_voc = sd(production, na.rm = TRUE),
            min_voc = min(production, na.rm = TRUE),
            max_voc = max(production, na.rm = TRUE),
            mean_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            min_age = min(age, na.rm = TRUE),
            max_age = max(age, na.rm = TRUE))
descriptives

# Linear model
mod <- lm(production ~ age + sex, data = voc_cantonese_select) 
mod_summary <- summary(mod)
mod_summary
