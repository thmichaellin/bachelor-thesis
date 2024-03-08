source('functions.R')
library(ggplot2)
library(dplyr) 
library(tidyr)
library(stringr) 
library(tidyverse)
library(lme4) # Multilevel model
library(nlme) # Non-linear mixed effect
library(brms)
library(sjPlot) # Tab for ANOVA
library(papaja) # apa figures
library(viridis) # plot colors
library(ggpmisc) # Plot nlme fit
library(loo) # Bayes model fit
library(patchwork) # Combine plots

logs %>% distinct(user_id) %>% count()

# Analogue items
items_analogue <- final_items %>% select(item_id, question_2)
items_analogue <- items_analogue %>% filter(str_starts(question_2, 'A') | 
                                                str_starts(question_2, 'B') | 
                                                str_starts(question_2, 'a') | 
                                                str_starts(question_2, 'b'))

logs_analogue <- logs %>% filter(item_id %in% items_analogue$item_id)

# Cross reference correct answers to categorize all (except 50) analogue items in logs

logs_analogue <- categorize_items(logs_analogue)

#logs_analogue %>% count(category)

# Filter items by analogue to text questions and rank by difficulty
analogue_final_items <- final_items %>% filter(item_id %in% items_analogue$item_id) %>%
    left_join(select(logs_analogue, item_id, category), by = 'item_id') %>% 
    filter(!is.na(category)) %>% # Remove items with unknown answers
    filter(question_1 == 'Hoe laat is het?') %>% 
    distinct(item_id, .keep_all = TRUE) %>% # Check for distinct id's and keep all values
    arrange(desc(item_rating)) %>% mutate(rank = row_number()) # Rank by difficulty

# Filter logs for analogue to text questions
logs_analogue_questions <- logs_analogue %>% filter(item_id %in% analogue_final_items$item_id)

# Total items/plays per category
analogue_final_items %>% count(category) %>% arrange(desc(n))
logs_analogue_questions %>% count(category) %>% arrange(desc(n))

# Filter by grade of first play
#logs_grade_3 <- filter_by_grade(logs_analogue_final, 3) 
#logs_grade_4 <- filter_by_grade(logs_analogue_final, 4)
#logs_grade_5 <- filter_by_grade(logs_analogue_final, 5)

# NOTE Grade 3: Hele uren, Grade 4: Halve uren, kwartieren, Grade 5: Op de minuut

# Grade count check
#logs_grade_3 %>% group_by(user_id) %>% count(grade) %>% group_by(grade) %>% summarize(sum(n))
#logs_grade_4 %>% group_by(user_id) %>% count(grade) %>% group_by(grade) %>% summarize(sum(n))
#logs_grade_5 %>% group_by(user_id) %>% count(grade) %>% group_by(grade) %>% summarize(sum(n))

# Calculate change in user rating for any user who was in grade 3, 4, OR(inclusive) 5 during the study duration
# and filter for students within IQR
#logs_grade_3 <- trim_by_delta_r(logs_grade_3)
#logs_grade_4 <- trim_by_delta_r(logs_grade_4)
#logs_grade_5 <- trim_by_delta_r(logs_grade_5)
logs_analogue_questions <- trim_by_delta_r(logs_analogue_questions)

