source('functions.R')

logs_analogue_final <- logs_analogue_questions %>% mutate(year_month = year(created) * 100 + month(created)) 
users <- logs_analogue_final %>% group_by(user_id, category) %>% 
    count(year_month) %>% filter(n >= 10)
logs_analogue_final <- logs_analogue_final %>% semi_join(users, by = c('user_id', 'category', 'year_month')) 

# Ensure >10 trials for each category
logs_correct <- prep_logs_cat_diff(logs_analogue_final)
logs_analogue_final %>% count(category) %>% arrange(desc(n))


# Prior
prior_exp <- prior(normal(11, 2), nlpar = "A") +
    prior(normal(4, 2), nlpar = "B") +
    prior(normal(0.16, 2), nlpar = "alpha")

prior_exp_uninf <- prior(student_t(3, 11, 3), nlpar = "A") +
    prior(student_t(3, 4, 3) , nlpar = "B") +
    prior(student_t(3, 0.16, 3) , nlpar = "alpha")

formula_exp <- bf(response_in_seconds ~ A + B * exp(-alpha * trials_category),
                  A ~ (1 | user_id), B + alpha ~ 1, nl = TRUE)


# MODELS #

bae_log_hour_0 <- logs_correct %>% filter(category == 'Hour' & difficulty == 0) %>% 
    filter(trials_category < 31) %>% mutate(user_id = as.factor(user_id))
bae_log_half_0 <- logs_correct %>% filter(category == 'Half hour' & difficulty == 0) %>% 
    filter(trials_category < 31) %>% mutate(user_id = as.factor(user_id))
bae_log_qtr_0 <- logs_correct %>% filter(category == 'Quarter past/to' & difficulty == 0) %>% 
    filter(trials_category < 31) %>% mutate(user_id = as.factor(user_id))

bae_log_hour_1 <- logs_correct %>% filter(category == 'Hour' & difficulty == 1) %>% 
    filter(trials_category < 31) %>% mutate(user_id = as.factor(user_id))
bae_log_half_1 <- logs_correct %>% filter(category == 'Half hour' & difficulty == 1) %>% 
    filter(trials_category < 31) %>% mutate(user_id = as.factor(user_id))
bae_log_qtr_1 <- logs_correct %>% filter(category == 'Quarter past/to' & difficulty == 1) %>% 
    filter(trials_category < 31) %>% mutate(user_id = as.factor(user_id))

bae_log_hour_2 <- logs_correct %>% filter(category == 'Hour' & difficulty == 2) %>% 
    filter(trials_category < 31) %>% mutate(user_id = as.factor(user_id))
bae_log_half_2 <- logs_correct %>% filter(category == 'Half hour' & difficulty == 2) %>% 
    filter(trials_category < 31) %>% mutate(user_id = as.factor(user_id))
bae_log_qtr_2 <- logs_correct %>% filter(category == 'Quarter past/to' & difficulty == 2) %>% 
    filter(trials_category < 31) %>% mutate(user_id = as.factor(user_id))

# DIFFICULTY 0#
bae_log_hour_1 %>% count(grade) %>% arrange(desc(n))
bae_log_half_2 %>% count(grade) %>% arrange(desc(n))
bae_log_qtr_2 %>% count(grade) %>% arrange(desc(n))


bae_lin_hour_0 
<- brm(response_in_seconds ~ trials_category + 
           (1 | user_id),
       data = bae_log_hour_0,
       chains = 4,
       iter = 2000,
       cores = 4)
#pp_check(bae_exp_hour_0, ndraw= 50)

bae_poly_hour_0
<- brm(response_in_seconds ~ poly(trials_category, 3) + 
           (1  | user_id),
       data = bae_log_hour_0,
       chains = 4,
       iter = 2000,
       cores = 4)

bae_exp_hour_0 
<- brm(formula_exp, 
       data = bae_log_hour_0,
       prior = prior_exp,
       chains = 4,
       iter = 2000,
       cores = 4)

bae_weak_hour_0 <- brm(formula_exp, 
                       data = bae_log_hour_0,
                       prior = prior_exp_uninf,
                       chains = 4,
                       iter = 2000,
                       cores = 4)

bae_lin_half_0 
<- brm(response_in_seconds ~ trials_category + 
           (1 | user_id),
       data = bae_log_half_0,
       chains = 4,
       iter = 2000,
       cores = 4)

bae_poly_half_0 
<- brm(response_in_seconds ~ poly(trials_category, 3) +   
           (1  | user_id),
       data = bae_log_half_0,
       chains = 4,
       iter = 2000,
       cores = 4)

bae_exp_half_0 
<- brm(formula_exp, 
       data = bae_log_half_0,
       prior = prior_exp, 
       chains = 4,
       iter = 2000,
       cores = 4)



bae_lin_qtr_0 
<- brm(response_in_seconds ~ trials_category + 
           (1 | user_id),
       data = bae_log_qtr_0,
       chains = 4,
       iter = 2000,
       cores = 4)

bae_poly_qtr_0 
<- brm(response_in_seconds ~ poly(trials_category, 3) +   
           (1  | user_id),
       data = bae_log_qtr_0,
       chains = 4,
       iter = 2000,
       cores = 4)

bae_exp_qtr_0 
<- brm(formula_exp, 
       data = bae_log_qtr_0,
       prior = prior_exp, 
       chains = 4, 
       iter = 2000,
       cores = 4)



# DIFFICULTY 1 #

bae_lin_hour_1 
<- brm(response_in_seconds ~ trials_category + 
           (1 | user_id),
       data = bae_log_hour_1,
       chains = 4,
       iter = 2000,
       cores = 4)

bae_poly_hour_1
<- brm(response_in_seconds ~ poly(trials_category, 3) +   
           (1  | user_id),
       data = bae_log_hour_1,
       chains = 4,
       iter = 2000,
       cores = 4)

bae_exp_hour_1
<- brm(formula_exp, 
       data = bae_log_hour_1,
       prior = prior_exp, 
       chains = 4,
       iter = 2000,
       cores = 4,
       control = list(adapt_delta = 0.9))




bae_lin_half_1 
<- brm(response_in_seconds ~ trials_category + 
           (1 | user_id),
       data = bae_log_half_1,
       chains = 4,
       iter = 2000,
       cores = 4)

bae_poly_half_1 
<- brm(response_in_seconds ~ poly(trials_category, 3) +   
           (1  | user_id),
       data = bae_log_half_1,
       chains = 4,
       iter = 2000,
       cores = 4)

bae_exp_half_1 
<- brm(formula_exp, 
       data = bae_log_half_1,
       prior = prior_exp, 
       chains = 4,
       iter = 2000,
       cores = 4)

bae_weak_half_1 <- brm(formula_exp, 
                       data = bae_log_half_1,
                       prior = prior_exp_uninf,
                       chains = 4,
                       iter = 2000,
                       cores = 4,
                       control = list(adapt_delta = 0.95))



bae_lin_qtr_1 
<- brm(response_in_seconds ~ trials_category + 
           (1 | user_id),
       data = bae_log_qtr_1,
       chains = 4,
       iter = 2000,
       cores = 4)

bae_poly_qtr_1 
<- brm(response_in_seconds ~ poly(trials_category, 3) +   
           (1  | user_id),
       data = bae_log_qtr_1,
       chains = 4,
       iter = 2000,
       cores = 4)


bae_exp_qtr_1 
<- brm(formula_exp, 
       data = bae_log_qtr_1,
       prior = prior_exp, 
       chains = 4, 
       iter = 2000,
       cores = 4)



# DIFFICULTY 2 #

bae_lin_hour_2 
<- brm(response_in_seconds ~ trials_category + 
           (1 | user_id),
       data = bae_log_hour_2,
       chains = 4,
       iter = 2000,
       cores = 4)

bae_poly_hour_2 
<- brm(response_in_seconds ~ poly(trials_category, 3) +   
           (1  | user_id),
       data = bae_log_hour_2,
       chains = 4,
       iter = 2000,
       cores = 4)

bae_exp_hour_2 
<- brm(formula_exp, 
       data = bae_log_hour_2, 
       prior = prior_exp, 
       chains = 4,
       iter = 2000,
       cores = 4,
       control = list(adapt_delta = 0.95))




bae_lin_half_2 
<- brm(response_in_seconds ~ trials_category + 
           (1 | user_id),
       data = bae_log_half_2, 
       chains = 4,
       iter = 2000,
       cores = 4)

bae_poly_half_2 
<- brm(response_in_seconds ~ poly(trials_category, 3) +   
           (1  | user_id),
       data = bae_log_half_2, 
       chains = 4,
       iter = 2000,
       cores = 4)

bae_exp_half_2 
<- brm(formula_exp, 
       data = bae_log_half_2,
       prior = prior_exp,  
       chains = 4,
       iter = 2000,
       cores = 4)




bae_lin_qtr_2 
<- brm(response_in_seconds ~ trials_category + 
           (1 | user_id),
       data = bae_log_qtr_2, 
       chains = 4,
       iter = 2000,
       cores = 4)

bae_poly_qtr_2 
<- brm(response_in_seconds ~ poly(trials_category, 3) + 
           (1  | user_id),
       data = bae_log_qtr_2, 
       chains = 4,
       iter = 2000,
       cores = 4)

bae_exp_qtr_2 
<- brm(formula_exp, 
       data = bae_log_qtr_2,
       prior = prior_exp,  
       chains = 4, 
       iter = 2000,
       cores = 4)

bae_weak_qtr_2 <- brm(formula_exp, 
                      data = bae_log_qtr_2,
                      prior = prior_exp_uninf,
                      chains = 4,
                      iter = 2000,
                      cores = 4)





bae_exp_qtr_2_prior_pred <- brm(formula_exp, 
                                data = bae_log_qtr_2,
                                prior = prior_exp, 
                                chains = 4, 
                                iter = 2000,
                                cores = 4,
                                sample_prior = "only")


# Model Comparison 
loo_lin_hour_0 <- loo(bae_lin_hour_0)
loo_poly_hour_0 <- loo(bae_poly_hour_0)
loo_exp_hour_0 <- loo(bae_exp_hour_0)
loo_compare(loo_lin_hour_0, loo_poly_hour_0, loo_exp_hour_0)

loo_lin_half_0 <- loo(bae_lin_half_0)
loo_poly_half_0 <- loo(bae_poly_half_0)
loo_exp_half_0 <- loo(bae_exp_half_0)
loo_compare(loo_lin_half_0, loo_poly_half_0, loo_exp_half_0)

loo_lin_qtr_0 <- loo(bae_lin_qtr_0)
loo_poly_qtr_0 <- loo(bae_poly_qtr_0)
loo_exp_qtr_0 <- loo(bae_exp_qtr_0)
loo_compare(loo_lin_qtr_0, loo_poly_qtr_0, loo_exp_qtr_0)

loo_lin_hour_1 <- loo(bae_lin_hour_1)
loo_poly_hour_1 <- loo(bae_poly_hour_1)
loo_exp_hour_1 <- loo(bae_exp_hour_1)
loo_compare(loo_lin_hour_1, loo_poly_hour_1, loo_exp_hour_1)

loo_lin_half_1 <- loo(bae_lin_half_1)
loo_poly_half_1 <- loo(bae_poly_half_1)
loo_exp_half_1 <- loo(bae_exp_half_1)
loo_compare(loo_lin_half_1, loo_poly_half_1, loo_exp_half_1)

loo_lin_qtr_1 <- loo(bae_lin_qtr_1)
loo_poly_qtr_1 <- loo(bae_poly_qtr_1)
loo_exp_qtr_1 <- loo(bae_exp_qtr_1)
loo_compare(loo_lin_qtr_1, loo_poly_qtr_1, loo_exp_qtr_1)

loo_lin_hour_2 <- loo(bae_lin_hour_2)
loo_poly_hour_2 <- loo(bae_poly_hour_2)
loo_exp_hour_2 <- loo(bae_exp_hour_2)
loo_compare(loo_lin_hour_2, loo_poly_hour_2, loo_exp_hour_2)

loo_lin_half_2 <- loo(bae_lin_half_2)
loo_poly_half_2 <- loo(bae_poly_half_2)
loo_exp_half_2 <- loo(bae_exp_half_2)
loo_compare(loo_lin_half_2, loo_poly_half_2, loo_exp_half_2)

loo_lin_qtr_2 <- loo(bae_lin_qtr_2)
loo_poly_qtr_2 <- loo(bae_poly_qtr_2)
loo_exp_qtr_2 <- loo(bae_exp_qtr_2)
loo_compare(loo_lin_qtr_2, loo_poly_qtr_2, loo_exp_qtr_2)



plot(bae_lin_hour_2)
plot(bae_poly_hour_2)
plot(bae_exp_hour_2)

plot(bae_lin_half_2)
plot(bae_poly_half_2)
plot(bae_exp_half_2)

plot(bae_lin_qtr_2)
plot(bae_poly_qtr_2)
plot(bae_exp_qtr_2)

summary(bae_lin_hour_0)


pp_check(bae_lin_hour_2, ndraws=50)
pp_check(bae_poly_hour_2, ndraws=50)
pp_check(bae_exp_hour_2, ndraws=50)

pp_check(bae_lin_half_2, ndraws=50)
pp_check(bae_poly_half_2, ndraws=50)
pp_check(bae_exp_half_2, ndraws=50)

pp_check(bae_lin_qtr_2, ndraws=50)
pp_check(bae_poly_qtr_2, ndraws=50)
pp_check(bae_exp_qtr_2, ndraws=50)


user_ids <- head(unique(bae_log_qtr_1$user_id), 9)
user_data <- bae_log_qtr_1 %>% filter(user_id %in% user_ids)
user_model <- update(bae_exp_qtr_1, newdata = user_data)
pp_check(user_model, ndraws=50, type='dens_overlay_grouped', group='user_id')



pp_check(bae_exp_qtr_2_prior_pred, ndraws = 50)
y_count <- bae_log_qtr_2 %>% count(response_in_seconds)
hist(y_count$response_in_seconds)

# Test plot individual
test <- bae_log_qtr_2 %>% filter(user_id == '3913509083')

ggplot(test, aes(x = trials_category, y = response_in_seconds)) +
    stat_smooth(
        method = 'nls',
        formula = y ~ asymp + (y_0 - asymp) * exp(- alpha * x),
        method.args = list(start = c(asymp = 10, y_0 = 25, alpha = 0.01), algorithm = 'port'),
        se = FALSE,
        col = viridis(2)[2]
    ) +
    geom_point() +
    labs(title = "Fitted vs. Observed Data | 3913509083",
         x = "Trials",
         y = "Response Time (s)") +
    theme_apa()
# Mean
bae_log_qtr_2 %>% summarize(mean(response_in_seconds))
lm(response_in_seconds ~ 1,
   data = bae_log_qtr_2)



bae_lin_hour_0
bae_poly_hour_0
bae_exp_hour_0


plot(bae_lin_hour_0)

# Fixed/Random
grouped_rt_user <- groupedData(response_in_seconds ~ trials_category | user_id, logs_correct)
bae_exp_freed <- brm(formula_exp, 
                     data = logs_correct,
                     prior = prior_exp, 
                     chains = 4, 
                     iter = 2000,
                     cores = 4)
bae_exp_fixed <- brm(formula_exp_fixed, 
                     data = logs_correct,
                     prior = prior_exp, 
                     chains = 4, 
                     iter = 2000,
                     cores = 4)

nlme_exp <- nlme(response_in_seconds ~ A + B * exp(-alpha * trials_category),
                 fixed = A + B + alpha ~ 1,
                 random = A + alpha ~ 1,
                 start = c(A = 11.7, B = 3.5, alpha = 0.13),
                 data = grouped_rt_user,
                 control = nlmeControl(tolerance = 0.7))



nlme_exp_fixed <- nlme(response_in_seconds ~ A + B * exp(-alpha * trials_category),
                       fixed = A + B + alpha ~ 1,
                       random = A ~ 1,
                       start = c(A = 11.0, B = 2.9, alpha = 0.18),
                       data = grouped_rt_user,
                       control = nlmeControl(tolerance = 0.02))
#polynomial degrees
lmer_poly2 <- lmer(response_in_seconds ~ poly(trials_category, 3) + (1 | user_id), 
                   data = logs_correct)
lmer_poly3 <- lmer(response_in_seconds ~ poly(trials_category, 3) + (1 | user_id), 
                   data = logs_correct)
lmer_poly4 <- lmer(response_in_seconds ~ poly(trials_category, 4) + (1 | user_id), 
                   data = logs_correct)
lmer_poly5 <- lmer(response_in_seconds ~ poly(trials_category, 5) + (1 | user_id), 
                   data = logs_correct)

anova(lmer_poly2, lmer_poly3, lmer_poly4, lmer_poly5)
AIC(lmer_poly2)
BIC(lmer_poly2)
AIC(lmer_poly3)
BIC(lmer_poly3)
AIC(lmer_poly4)
BIC(lmer_poly4)
AIC(lmer_poly5)
BIC(lmer_poly5)

plot(bae_weak_hour_0)
plot(bae_weak_half_1)
plot(bae_weak_qtr_2)


summary(nlme_exp)
anova(nlme_exp_fixed, nlme_exp)
anova(nlme_exp, nlme_exp_fixed)
