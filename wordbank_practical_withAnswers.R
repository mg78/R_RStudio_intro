# First /L+/ International Summer/Winter School on Language Acquisition
# Practical session on R and RStudio
# In this practical, we'll use data from the Wordbank database to explore, graph and model some vocabulary
# data in Cantonese.

# TASK 1: Loading packages -------------------
# 1A) Check the following packages are installed: here, tidyverse, skimr and wordbankr (hint: 'Packages' tab
# in the bottom right panel). If you've worked your way through the RYouWithMe BasicBasics units, you'll be
# familiar with the first three. The fourth we'll talk more about in task 2.
# 1B) If they have not been installed previously, install them (hint: install.packages("")).
# 1C) Add some code to the script to load them (hint: library()).

# ANSWER 1C):
library(here)
library(tidyverse)
library(skimr)
library(wordbankr)

# TASK 2: Read in the data --------------------
# We will be working with some 'Wordbank' data. Wordbank is a large open database of children's vocabulary
# development. Usually, you'll be accessing data by reading in a file (such as a .csv file). However, the
# Workbank data can be accessed from R via the wordbankr package. You can read more about Wordbank here:
# http://wordbank.stanford.edu or https://wordbank-book.stanford.edu.

# 2A) Let's check the vignette of the wordbankr package to learn a bit more about the package and accessing
# the data. Type ??wordbankr in the console. A link to the vignette should appear in the 'Help' tab in the
# bottom right window. Click on the link, have a look at the vignette and try to work out what the
# 'get_administration_data()' function does? How is it different from the 'get_item_data()' function?

# ANSWER 2A:
# The get_administration_data() function gives by-administration information. Can specify language and/or form.
# The get_item_data() function gives by-item information. Again, can specify language and/or form.

# 2B) Use the get_administration_data() function to read in data for WS (words and
# sentences) form in Cantonese. Assign the data to a dataframe called 'voc_cantonese'
# (hint: get_adminstration_data(language = "", form = "").

# ANSWER 2B:
voc_cantonese <- get_administration_data(language = "Cantonese", form = "WS")

# TASK 3: Exploring the data -------------------
# 3A: Now let's get a feel for these data. Use one or more of the following functions to have a look: dim(), str(),
# glimpse(), head(), tail(), skim() and summary(). Answer the following questions:
# 3B: How many observations are there for how many variables?
# 3C: Which of these variables numeric?
# 3D: Which of these variables contain missing values ('NA's)?
# 3E: What type of variable is 'longitudinal'?
# 3F: What is the minimum child age?
# 3G: Which of these functions do you find most informative and why?

# ANSWERS:
# 3A:
dim(voc_cantonese)
str(voc_cantonese)
glimpse(voc_cantonese)
head(voc_cantonese)
tail(voc_cantonese)
skim(voc_cantonese)
summary(voc_cantonese)

# 3B: 987 observations for 15 variables
# 3C: data_id, age, comprehension, production
# 3D: zygosity, birth_order, ethnicity, mom_ed
# 3E: A logical variable that can have the values 'TRUE' or 'FALSE'
# 3F: 16 months
# 3G: Personally, I find the skim() function most informative as it groups different types of variables (character,
# logical, numeric etc.) and provides useful overviews (missing values, some summary statistics). I do however,
# also always use the head() and tail() functions to get a better feeling for the data. 

# TASK 4: Select relevant variables --------------------------
# We don't need all the variables that are currently in the data frame. Let's use the select function (from the
# dplyr package, part of the tidyverse) to keep only those that we do need.

# 4A: Run the bit of code below to keep the following variables: age, language, sex, production and assign these to a new
# dataframe called 'voc_cantonese_select'. Check you understand what each bit in the code does. Remember, you can type 
# ?select in the console to get more information on the select() function.

voc_cantonese_select <- select(voc_cantonese, age, language, sex, production)

# Sometimes it is helpful to save the smaller dataframe to a csv file. This will allow you to just read in the
# relevant bits in future, without having to read in the complete data (which can be time-consuming if the
# complete data contains thousands of rows and lots of variables).We can do both of these steps in one 'pipe' 
# using the code below. Essentially, we are taking the dataframe with Cantonese vocabulary data (voc_cantonese),
# and then select relevant variables, and then write those to a csv-file. Again, check you understand what each bit in
# the code does and please do ask if you're not sure.

voc_cantonese_select <- voc_cantonese %>%
  select(age, language, sex, production) %>%
  write_csv(here('voc_cantonese_select.csv'))

# 4B: Just to practise reading in data from a .csv file, add the code to do so below (hint: read_csv())

# ANSWER 4B:
voc_cantonese_select <- read_csv(here('voc_cantonese_select.csv'))

# TASK 5: Visualise the data ---------------------
# We'll start with making some histograms to get an idea of the distribution of some of the variables.
# 5A: Run the code below to make a histogram for 'age'.
ggplot(voc_cantonese_select, aes(x = age)) + #Tell R what data to use in the first argument, then specify variable you want to plot in x = 
  geom_histogram() +                         #The geom 'geom_histrogram' tells R how to plot the data
  labs(x = "Age (in months)") +              #labs() can be used to label axes properly
  theme_bw()                                 #There are lots of different 'themes' in ggplot that give you options in terms of the look of the plot

# 5B: Add some code to the script to make a histograme for the productive vocabulary measure (hint: use the code above as a template).
# ANSWER 5B:
ggplot(voc_cantonese_select, aes(x = production)) + 
  geom_histogram() +
  labs(x = "Productive vocabulary") +
  theme_bw()

# 5C: What do these histograms tell you about the variables age and productive vocabulary?
# ANSWER 5C: There are roughly similar number of children across ages between 16 and 30 months. The vocabulary measure is highly skewed, with
# more children producing 50 words or less.

# Let's now plot the productive vocabulary data as a function of age in a scatterplot.
# 5D: Run the code below to do so.
ggplot(voc_cantonese_select, aes(x = age, y = production)) +
  geom_point() +
  theme_bw() +
  labs(x = "Age (in months)", y = "Productive vocabulary")

# In this plot, each dot represents the data from one child. Some are plotted on top of each other (having the same or very similar scores).
# We can use a different geom to help with that.
# 5E: Run the code below to do so.
# To find out more on what the geom_jitter() function does and what further options it gives you, type ?geom_jitter in the console. 

ggplot(voc_cantonese_select, aes(x = age, y = production)) +
  geom_jitter() +
  theme_bw() +
  labs(x = "Age (in months)", y = "Productive vocabulary")

# This plot does make more points visible, but there is still quite a few overlapping points. Have a look at this website to work
# what the 'alpha' aesthetic does: https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html 
# 5F: What does the alpha aesthetic do?
# ANSWER 5F: Its value determines the opacity (or transparency) of the individual
# data points. to get a better idea of this by specifying a different 'alpha' value as an argument of the geom_jitter() function.
# 5G: Specify a value of alpha as an argument to the geom_jitter() function to make the data points more transparent.

#ANSWER 5G:
ggplot(voc_cantonese_select, aes(x = age, y = production)) +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(x = "Age (in months)", y = "Productive vocabulary") 

# 5H: What do the darker areas of the plot represent?
# ANSWER 5H: That there are more overlapping data points in those areas.

# So far we have plotted the data for boys and girls together. But there is evidence that vocabulary development for boys and girls differs
# and that variable is present in the dataset. We can plot data for boys and girls separately by choosing different colours.
# 5I: Run the code below to do so.
ggplot(voc_cantonese_select, aes(x = age, y = production, colour = sex)) +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(x = "Age (in months)", y = "Productive vocabulary") 

# Finally, we can add lines of best fit to summarise the relationships between age and productive vocabulary for boys and girls.
# 5J: Run the code below to do so. What do you conclude from the plot?
ggplot(voc_cantonese_select, aes(x = age, y = production, colour = sex)) +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = 'lm', se = TRUE) +
  theme_bw() +
  labs(x = "Age (in months)", y = "Productive vocabulary")

# ANSWER 5J: That, on average, girls produce slightly more words than boys.

# TASK 6: Model the data ---------------------
# First, it might be useful to compute some descriptive statistics, and we'll do so for boys and girls separately.
# 6A: Run the code below to do so. Check you understand what each part of the code does. 
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

# Now, we will fit a regression model to test the hypothesis that age and sex account for variation in the number of words that a child produces.
# To keep it simple, we'll use linear regression, although there are reasons to believe that is a simplification (the relationship might not
# be linear, but this is just to give you an idea). To specify the regression model we use the lm() function. This takes as arguments the
# outcome or dependent variable y (production) as a function of the predictor or independent variables x1 (age) and x2 (sex), as well as the data
# to be use. We assign this output to a dataframe called 'mod' (for model). The summary() function summarises the main results of the model; we've
# assigned that summary to a dataframe called 'mod_summary'. To see the summary in the console, we have to call it.

# 6B: Run the code below to do so:
mod <- lm(production ~ age + sex, data = voc_cantonese_select) 
mod_summary <- summary(mod)
mod_summary

# When we call mod_summary, R writes the content of that dataframe to the console. It tells us that the overall model is significant (p-value: <2.2e-16),
# and that together, the predictors (age and sex) account for 52.2% of variance in the number of words children produce (Adjusted R-squared = 0.522*100).
# Under 'coefficients' you can see that both predictors (age and sex) are significant (values under 'Pr(>|t|) are smaller than 0.05). You can also see
# that the estimate for age is positive, indicating that with increasing age, productive vocabulary increases. The negative value for the estimate for sex
# indicates that boys ('Male' category) score lower than girls (the reference category, in this case 'Female').

# TASK 7: Write up the results ------------------------
# You could write this up as follows:
# "A linear regression was performed with productive vocabulary (M = 273, SD = 220) as the outcome variable and
# age (M = 23.2, SD = 4.2) and sex (485 girls, 502 boys) as the predictor variables. The results of the regression indicated that the
# model significantly predicted productive vocabulary (F(2, 984) = 539.4, p < .001, adjusted R2 = 0.52), accounting
# for 52% of the variance. Age was a significant positive predictor (beta = 37.29, p < .001): as age increased,
# productive vocabulary increased. Boys scored lower than girls (beta = -49.92, p < .001)."
# This follows APA guidelines (the Purdue Writing lab website https://owl.purdue.edu/owl/research_and_citation/apa6_style/apa_formatting_and_style_guide/statistics_in_apa.html)
# is helpful for guidance on punctuating statistics), which are widely used within psychology and psycholinguistics. Journals in linguistics
# might use different guidelines.