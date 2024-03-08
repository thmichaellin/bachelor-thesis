users <- bae_log_half_2 %>% distinct(user_id)
users <- sample(users$user_id, 9)



prior_exp_uninf <- prior(student_t(3, 11, 3), nlpar = "yf") +
    prior(student_t(3, 14, 3), nlpar = "y0") +
    prior(student_t(3, -1.6, 3), nlpar = "alpha")

bae_exp_half_2_uninf 
<- brm(formula_exp, 
       data = bae_log_half_2,
       prior = prior_exp_uninf, 
       chains = 4, 
       iter = 2000,
       cores = 4)

post_samples 
<- predict(bae_exp_half_2_uninf, bae_log_half_2)
mean(bae_log_half_2$response_in_seconds - post_samples[,1]) ^ 2


bae_exp_half_1_uninf 
<- brm(formula_exp, 
       data = bae_log_half_1,
       prior = prior_exp_uninf, 
       chains = 4, 
       iter = 2000,
       cores = 4)

bae_exp_qtr_2_uninf 
<- brm(formula_exp, 
       data = bae_log_qtr_2,
       prior = prior_exp_uninf, 
       chains = 4, 
       iter = 2000,
       cores = 4)

plot(bae_exp_qtr_2_uninf)
pp_check(bae_exp_qtr_2_uninf, ndraws=50)

formula_exp_simple <- bf(response_in_seconds ~ yf + (y0 - yf) * exp(-alpha * trials_category),
                         yf + y0 + alpha ~ 1, nl = TRUE)
subset_exp_orig_prior <- brm(formula_exp_simple, data = data1, prior = prior_exp,
                             chains = 4,
                             iter = 4000,
                             cores = 4)
subset_exp <- brm(formula_exp_simple, data = data1,
                  chains = 4,
                  iter = 4000,
                  cores = 4,
                  control = list(adapt_delta = 0.9, max_treedepth = 25))
subset_poly <- brm(response_in_seconds ~ poly(trials_category, 2), data = data7,
                   chains = 4,
                   iter = 4000,
                   cores = 4)

plot(subset_exp)
subset_a <- plot <-ggplot(data1, aes(x = trials_category, y = response_in_seconds)) +
    geom_point(aes(color = 'Observed')) +
    geom_point(aes(y = predict(subset_exp, data1)[,1], col = 'Exponential')) +
    labs(x = 'Trials', y = 'Response Time (s)', title = paste('Student', 'A')) +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = "D") +
    labs(color = 'Model')
subset_g <- plot <-ggplot(data7, aes(x = trials_category, y = response_in_seconds)) +
    geom_point(aes(color = 'Observed')) +
    geom_point(aes(y = predict(subset_poly, data7)[,1], col = 'Polynomial')) +
    labs(x = 'Trials', y = 'Response Time (s)', title = paste('Student', 'B')) +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = "D") +
    labs(color = 'Model')

data1 <- bae_log_half_2 %>% filter(user_id == '1147042500')
data2 <- bae_log_half_2 %>% filter(user_id == '2263341006')
data3 <- bae_log_half_2 %>% filter(user_id == '1425065856')
data4 <- bae_log_half_2 %>% filter(user_id == '2316775094')
data5 <- bae_log_half_2 %>% filter(user_id == '3800734575')
data6 <- bae_log_half_2 %>% filter(user_id == '3137154877')
data7 <- bae_log_half_2 %>% filter(user_id == '2472224104')
data8 <- bae_log_half_2 %>% filter(user_id == '1729902545')
data9 <- bae_log_half_2 %>% filter(user_id == '906841697')

p1 <- plot <-ggplot(data1, aes(x = trials_category, y = response_in_seconds)) +
    geom_point(aes(color = 'Observed')) +
    geom_point(aes(y = predict(bae_lin_half_2, data1)[,1], col = 'Linear')) +
    geom_point(aes(y = predict(bae_exp_half_2, data1)[,1], col = 'Exponential')) +
    geom_point(aes(y = predict(bae_poly_half_2, data1)[,1], col = 'Polynomial')) +
    labs(x = 'Trials', y = 'Response Time (s)', title = paste('Student', 'A')) +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = "D") +
    labs(color = 'Model')
p2 <- plot <-ggplot(data2, aes(x = trials_category, y = response_in_seconds)) +
    geom_point(aes(color = 'Observed')) +
    geom_point(aes(y = predict(bae_lin_half_2, data2)[,1], col = 'Linear')) +
    geom_point(aes(y = predict(bae_exp_half_2, data2)[,1], col = 'Exponential')) +
    geom_point(aes(y = predict(bae_poly_half_2, data2)[,1], col = 'Polynomial')) +
    labs(x = 'Trials', y = 'Response Time (s)', title = paste('Student', 'B')) +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = "D") +
    labs(color = 'Model')
p3 <- plot <-ggplot(data3, aes(x = trials_category, y = response_in_seconds)) +
    geom_point(aes(color = 'Observed')) +
    geom_point(aes(y = predict(bae_lin_half_2, data3)[,1], col = 'Linear')) +
    geom_point(aes(y = predict(bae_exp_half_2, data3)[,1], col = 'Exponential')) +
    geom_point(aes(y = predict(bae_poly_half_2, data3)[,1], col = 'Polynomial')) +
    labs(x = 'Trials', y = 'Response Time (s)', title = paste('Student', 'C')) +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = "D") +
    labs(color = 'Model')
p4 <- plot <-ggplot(data4, aes(x = trials_category, y = response_in_seconds)) +
    geom_point(aes(color = 'Observed')) +
    geom_point(aes(y = predict(bae_lin_half_2, data4)[,1], col = 'Linear')) +
    geom_point(aes(y = predict(bae_exp_half_2, data4)[,1], col = 'Exponential')) +
    geom_point(aes(y = predict(bae_poly_half_2, data4)[,1], col = 'Polynomial')) +
    labs(x = 'Trials', y = 'Response Time (s)', title = paste('Student', 'D')) +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = "D") +
    labs(color = 'Model')
p5 <- plot <-ggplot(data5, aes(x = trials_category, y = response_in_seconds)) +
    geom_point(aes(color = 'Observed')) +
    geom_point(aes(y = predict(bae_lin_half_2, data5)[,1], col = 'Linear')) +
    geom_point(aes(y = predict(bae_exp_half_2, data5)[,1], col = 'Exponential')) +
    geom_point(aes(y = predict(bae_poly_half_2, data5)[,1], col = 'Polynomial')) +
    labs(x = 'Trials', y = 'Response Time (s)', title = paste('Student', 'E')) +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = "D") +
    labs(color = 'Model')
p6 <- plot <-ggplot(data6, aes(x = trials_category, y = response_in_seconds)) +
    geom_point(aes(color = 'Observed')) +
    geom_point(aes(y = predict(bae_lin_half_2, data6)[,1], col = 'Linear')) +
    geom_point(aes(y = predict(bae_exp_half_2, data6)[,1], col = 'Exponential')) +
    geom_point(aes(y = predict(bae_poly_half_2, data6)[,1], col = 'Polynomial')) +
    labs(x = 'Trials', y = 'Response Time (s)', title = paste('Student', 'F')) +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = "D") +
    labs(color = 'Model')
p7 <- plot <-ggplot(data7, aes(x = trials_category, y = response_in_seconds)) +
    geom_point(aes(color = 'Observed')) +
    geom_point(aes(y = predict(bae_lin_half_2, data7)[,1], col = 'Linear')) +
    geom_point(aes(y = predict(bae_exp_half_2, data7)[,1], col = 'Exponential')) +
    geom_point(aes(y = predict(bae_poly_half_2, data7)[,1], col = 'Polynomial')) +
    labs(x = 'Trials', y = 'Response Time (s)', title = paste('Student', 'G')) +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = "D") +
    labs(color = 'Model')
p8 <- plot <-ggplot(data8, aes(x = trials_category, y = response_in_seconds)) +
    geom_point(aes(color = 'Observed')) +
    geom_point(aes(y = predict(bae_lin_half_2, data8)[,1], col = 'Linear')) +
    geom_point(aes(y = predict(bae_exp_half_2, data8)[,1], col = 'Exponential')) +
    geom_point(aes(y = predict(bae_poly_half_2, data8)[,1], col = 'Polynomial')) +
    labs(x = 'Trials', y = 'Response Time (s)', title = paste('Student', 'H')) +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = "D") +
    labs(color = 'Model')
p9 <- plot <-ggplot(data9, aes(x = trials_category, y = response_in_seconds)) +
    geom_point(aes(color = 'Observed')) +
    geom_point(aes(y = predict(bae_lin_half_2, data9)[,1], col = 'Linear')) +
    geom_point(aes(y = predict(bae_exp_half_2, data9)[,1], col = 'Exponential')) +
    geom_point(aes(y = predict(bae_poly_half_2, data9)[,1], col = 'Polynomial')) +
    labs(x = 'Trials', y = 'Response Time (s)', title = paste('Student', 'I')) +
    theme_apa() +
    scale_color_viridis(discrete = TRUE, option = "D") +
    labs(color = 'Model')


combined <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

