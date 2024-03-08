mean_logs <- logs_correct %>% filter(category == 'Hour' | category == 'Half hour' | 
                                         category == 'Quarter past/to')
mean_logs %>% group_by(category, difficulty, user_id) %>% count(user_id) %>% arrange(n)
#Group by category
mean_rt <- mean_logs %>% group_by(trials_category) %>% summarize(mean = mean(response_in_seconds))

mean_rt_30 <- mean_rt %>% filter(trials_category < 31)

ggplot(mean_rt, aes(x = trials_category, y = mean)) +
    geom_point() +
    labs(
        x = "Trials",
        y = "Mean Response Time (s)",
        title = "Mean Response Time across Trials"
    ) +
    theme_apa()

ggplot(mean_rt_30, aes(x = trials_category, y = mean)) +
    geom_point() +
    labs(
        x = "Trials",
        y = "Mean Response Time (s)",
        title = "Mean Response Time across Trials"
    ) +
    theme_apa()

#Group by cat
mean_rt_cat <- mean_logs %>% group_by(trials_category, category) %>% 
    summarize(mean = mean(response_in_seconds)) %>% filter(trials_category < 31)

#Group by difficulty
mean_rt_diff <- mean_logs %>% group_by(trials_category, difficulty) %>% 
    summarize(mean = mean(response_in_seconds)) %>% filter(trials_category < 31)

grouped_rt_cat <- groupedData(mean ~ trials_category | category, mean_rt_cat)
grouped_rt_diff <- groupedData(mean ~ trials_category | difficulty, mean_rt_diff)

mean_nlme_cat <- nlme(mean ~ A + B * trials_category^-b,
                      data = grouped_rt_cat,
                      fixed = A + B + b ~ 1,
                      random = A ~ 1,
                      start = c(A = 10, B = 4, b = .5))


mean_nlme_diff <- nlme(mean ~ A + B * trials_category^-b,
                       data = grouped_rt_diff,
                       fixed = A + B + b ~ 1,
                       random = A ~ 1,
                       start = c(A = 10, B = 4, b = .5))


mean_nlme_cat_scale <- update(mean_nlme_cat, random = pdDiag(A + B ~ 1))
mean_nlme_diff_scale <- update(mean_nlme_diff, random = pdDiag(A + B ~ 1))
mean_nlme_cat_slope <- update(mean_nlme_cat, random = pdDiag(A + B + b ~ 1))
mean_nlme_diff_slope <- update(mean_nlme_diff, random = pdDiag(A + B + b ~ 1))

mean_nls_power_cat <- nls(mean ~ A + B * trials_category^-b, 
                          data = grouped_rt_cat,
                          start = c(A = 10, B = 4, b = .5))

mean_nls_power_diff <- nls(mean ~ A + B * trials_category^-b, 
                           data = grouped_rt_diff,
                           start = c(A = 10, B = 4, b = .5))
summary(mean_nls_power_cat)
summary(mean_nlme_cat)
summary(mean_nlme_cat_slope)
AIC(mean_nlme_cat)
AIC(mean_nlme_cat_slope)
BIC(mean_nlme_cat)
BIC(mean_nlme_cat_slope)

summary(mean_nlme_diff)
summary(mean_nlme_diff_slope)
AIC(mean_nlme_diff)
AIC(mean_nlme_diff_slope)
BIC(mean_nlme_diff)
BIC(mean_nlme_diff_slope)

anova(mean_nlme_cat)
summary(anova(mean_nlme_diff))

anova(mean_nlme_cat, mean_nlme_cat_scale, mean_nlme_cat_slope)                          
anova(mean_nlme_diff, mean_nlme_diff_scale, mean_nlme_diff_slope)

# parameter 'b' does NOT differ on category
# parameter 'b' DOES differ on difficulty

# plot | category
ggplot(grouped_rt_cat, aes(x = trials_category, y = mean, 
                           color = as.factor(category))) +
    stat_smooth(
        method = 'nls',
        formula = y ~ yf + (y0 - yf) * x^-b,
        method.args = list(start = c(yf = 2, y0 = 30, b = .5)),
        se = FALSE,
        aes(group = category)
    ) +
    geom_point() +
    labs(title = "Fitted Models vs. Observed Data | Category",
         x = "Trials",
         y = "Mean Response Time (s)") +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = 'D',
                        name = "Category")


# plot | difficulty
ggplot(grouped_rt_diff, aes(x = trials_category, y = mean, 
                            color = as.factor(difficulty))) +
    stat_smooth(
        method = 'nls',
        formula = y ~ yf + (y0 - yf) * x^-b,
        method.args = list(start = c(yf = 2, y0 = 30, b = .5)),
        se = FALSE,
        aes(group = difficulty)
    ) +
    geom_point() +
    labs(title = "Fitted Models vs. Observed Data | Difficulty",
         x = "Trials",
         y = "Mean Response Time (s)") +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = 'D',
                        name = "Difficulty")

mean_nls_exp <- nls(mean ~ SSasymp(trials_category, yf, y0, a),
                    data = mean_rt_30)


citation('stats')
packageVersion('brms')
citation()
citation('brms')

mean_nls_power <- nls(mean ~ A + B * trials_category^-b, data = mean_rt_30,
                      start = c(A = 10, B = 4, b = .5))
mean_nls_exp <- nls(mean ~ A + B * exp(- alpha * trials_category), 
                    data = mean_rt_30,
                    start = c(A = 10, B = 4, alpha = 0.19),
                    algorithm = "port",
                    control = nls.control(maxiter = 100))

mean_nls_apex <- nls(mean ~ A + B * exp(- alpha * trials_category) * trials_category ^ - beta, 
                     data = mean_rt_30,
                     start = c(A = 9.9, B = 4, alpha = 0.01, beta = 0.5),
                     algorithm = "port")

ggplot(mean_rt_30) +
    aes(x = trials_category, y = mean) +
    stat_smooth(
        method = 'nls', 
        formula = y ~ A + B * exp(-a * x),
        method.args = list(start = c(A = 10, B = 4, a = .5)),
        se = FALSE,
        aes(color = 'Exponential')) +
    stat_smooth(
        method = 'nls',
        formula = y ~ A + B * x^-b,
        method.args = list(start = c(A = 2, B = 4, b = .5)),
        se = FALSE,
        aes(color = 'Power')) +
    #stat_smooth(
    #  method = 'nls',
    #  formula = y ~ A + B * exp(- alpha * x) * x ^ - beta,
    #  method.args = list(start = c(A = 9.9, B = 4, alpha = 0.01, beta = 0.5), algorithm = "port"),
    #  se = FALSE,
    #  aes(color = 'APEX')) +
    geom_point(aes(color = 'Observed Data')) +
    labs(title = "Fitted Models vs. Observed Data",
         x = "Trials",
         y = "Mean Response Time (s)") +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = 'D',
                        name = "Model")
ggplot(grouped_rt_cat) +
    aes(x = trials_category, y = mean) +
    stat_smooth(
        method = 'nls',
        formula = y ~ yf + (y0 - yf) * x^-b,
        method.args = list(start = c(yf = 2, y0 = 30, b = .5)),
        se = FALSE,
        aes(color = 'Power')) +
    #stat_smooth(
    #  method = 'nls',
    #  formula = y ~ A + B * exp(- alpha * x) * x ^ - beta,
    #  method.args = list(start = c(A = 9.9, B = 4, alpha = 0.01, beta = 0.5), algorithm = "port"),
    #  se = FALSE,
    #  aes(color = 'APEX')) +
    geom_point(aes(color = 'Observed Data')) +
    labs(title = "Fitted Models vs. Observed Data",
         x = "Trials",
         y = "Mean Response Time (s)") +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = 'D',
                        name = "Model")


c(AIC(mean_nls_exp)- AIC(mean_nls_power))
c(BIC(mean_nls_exp) - BIC(mean_nls_power))
summary(mean_nls_exp)
summary(mean_nlme_cat)
-log(0.1975)
summary(mean_nls_power)
summary(mean_nls_exp)
anova(mean_nls_exp, mean_nls_power)
#mean_nls_apex
