prep_logs_cat_diff <- function(log) {
    logs_hour_0 <- filter_by_cat_difficulty(log, 'Hour', 0)
    logs_half_0 <- filter_by_cat_difficulty(log, 'Half hour', 0)
    logs_qtr_0 <- filter_by_cat_difficulty(log, 'Quarter past/to', 0)
    
    logs_hour_1 <- filter_by_cat_difficulty(log, 'Hour', 1)
    logs_half_1 <- filter_by_cat_difficulty(log, 'Half hour', 1)
    logs_qtr_1 <- filter_by_cat_difficulty(log, 'Quarter past/to', 1)
    
    logs_hour_2 <- filter_by_cat_difficulty(log, 'Hour', 2)
    logs_half_2 <- filter_by_cat_difficulty(log, 'Half hour', 2)
    logs_qtr_2 <- filter_by_cat_difficulty(log, 'Quarter past/to', 2)
    
    # Take IQR of accuracy
    logs_hour_accuracy_0 <- trim_by_accuracy(logs_hour_0)
    logs_half_accuracy_0 <- trim_by_accuracy(logs_half_0)
    logs_qtr_accuracy_0 <- trim_by_accuracy(logs_qtr_0)
    
    logs_hour_accuracy_1 <- trim_by_accuracy(logs_hour_1)
    logs_half_accuracy_1 <- trim_by_accuracy(logs_half_1)
    logs_qtr_accuracy_1 <- trim_by_accuracy(logs_qtr_1)
    
    logs_hour_accuracy_2 <- trim_by_accuracy(logs_hour_2)
    logs_half_accuracy_2 <- trim_by_accuracy(logs_half_2)
    logs_qtr_accuracy_2 <- trim_by_accuracy(logs_qtr_2)
    
    
    
    # Combine categories
    logs_combined <- bind_rows(logs_hour_accuracy_0, logs_half_accuracy_0,
                               logs_qtr_accuracy_0, logs_hour_accuracy_1, 
                               logs_half_accuracy_1, logs_qtr_accuracy_1, 
                               logs_hour_accuracy_2, logs_half_accuracy_2,
                               logs_qtr_accuracy_2,)
    # Scale response to seconds
    logs_combined <- logs_combined %>% mutate(response_in_seconds = 
                                                  response_in_milliseconds/1000)
    # Add trial number total
    logs_final <- logs_combined %>% group_by(user_id) %>%
        mutate(trials_total = row_number())
    # Add trial number per category
    logs_final <- logs_final %>% group_by(user_id, category) %>% 
        mutate(trials_category = row_number())
    
    # Filter by correct
    logs_correct <- logs_final %>% filter(correct_answered == 1)
    
    # Add trial number per day
    logs_correct <- logs_correct %>% group_by(user_id, created, category) %>%
        mutate(trials_day = row_number())
    # Convert trial/day to hours
    logs_correct <- logs_correct %>% mutate(hours = ((trials_day - 1) / max(trials_day)) * 24 * 60)
    
    
    # Make difficulty, correctness discrete
    logs_final <- logs_final %>% mutate(difficulty = as.factor(difficulty)) %>% 
        mutate(correct_answered = as.factor(correct_answered)) %>% ungroup()
    logs_correct <- logs_correct %>% mutate(difficulty = as.factor(difficulty)) %>% 
        mutate(correct_answered = as.factor(correct_answered)) %>% ungroup()
    
    # Change date data-type
    logs_correct <- logs_correct %>% mutate(date_time = as.POSIXct(created) + dminutes(hours))
    
    return(logs_correct)
}

filter_by_grade <- function(log, filter_grade) {
    id_grade <- log %>% group_by(user_id) %>% slice_head() %>%
        filter(grade == filter_grade)
    logs_grade <- log %>% filter(user_id %in% id_grade$user_id) %>% filter(grade >= filter_grade)
    return(logs_grade)
}

filter_by_cat <- function(log, cat) {
    id_cat <- log %>% group_by(user_id) %>% filter(correct_answered == 1) %>% 
        count(category) %>% filter(category == cat & n >= 20 & n <= 200)
    logs_cat <- log %>% filter(user_id %in% id_cat$user_id) %>% 
        filter(category == cat)
    return(logs_cat)
    
}
filter_by_cat_difficulty <- function(log, cat, diff) {
    id_cat <- log %>% group_by(user_id) %>% filter(difficulty == diff) %>% 
        count(category) %>% filter(category == cat & n >= 20 & n <= 200)
    logs_cat <- log %>% filter(user_id %in% id_cat$user_id) %>% 
        filter(category == cat) %>% filter(difficulty == diff)
}

categorize_items <- function(log) {
    correct <- log %>% filter(correct_answered == 1) %>% 
        mutate(category = case_when(str_detect(answer, 'uur') ~ 'Hour',
                                    str_detect(answer, ':00') ~ 'Hour',
                                    str_detect(answer, '^half') ~ 'Half hour',
                                    str_detect(answer, ':30') ~ 'Half hour', 
                                    str_detect(answer, 'kwart over') ~ 'Quarter past/to',
                                    str_detect(answer, ':15') ~ 'Quarter past/to',
                                    str_detect(answer, 'kwart voor') ~ 'Quarter past/to',
                                    str_detect(answer, ':45') ~ 'Quarter past/to',
                                    str_detect(answer, 'over half') ~ 'Relative half hour',
                                    str_detect(answer, 'voor half') ~ 'Relative half hour',
                                    str_detect(answer, 'vijf over') ~ 'Five past/to',
                                    str_detect(answer, ':05') ~ 'Five past/to',
                                    str_detect(answer, 'tien over') ~ 'Ten past/to',
                                    str_detect(answer, ':10') ~ 'Ten past/to',
                                    str_detect(answer, 'vijf voor') ~ 'Five past/to',
                                    str_detect(answer, ':55') ~ 'Five past/to',
                                    str_detect(answer, 'tien voor') ~ 'Ten past/to',
                                    str_detect(answer, ':50') ~ 'Ten past/to',
                                    TRUE ~ 'Minutes past/to'))
    
    grouped <- correct %>% group_by(item_id, category) %>% distinct(item_id)
    log <- log %>% left_join(grouped, by='item_id')
    return(log)
}

trim_by_accuracy <- function(logs) {
    accuracy <- logs %>% group_by(user_id) %>% 
        summarise(acc = mean(correct_answered)) %>% arrange(desc(acc))
    trimmed_accuracy <- accuracy %>% filter(acc > 0, acc < 1) 
    quantiles <- quantile(trimmed_accuracy$acc, c(0, .25, .50, .75, 1))
    trimmed_accuracy <- trimmed_accuracy %>% filter(quantiles[2] < acc, acc < quantiles[4])
    logs_accuracy <- logs %>% filter(user_id %in% trimmed_accuracy$user_id)
    return(logs_accuracy)
}

trim_by_delta_r <- function(logs) {
    log_delta_r <- logs %>% group_by(user_id) %>% 
        summarize(delta_r = max(new_user_domain_rating) - min(new_user_domain_rating))
    quantiles <- quantile(log_delta_r$delta_r, c(0, .25, .50, .75, 1))
    # filter by interquartile range
    trimmed_delta_r <- log_delta_r %>% filter(quantiles[2] < delta_r, delta_r < quantiles[4])
    # Filter logs
    output_log <- logs %>% filter(user_id %in% trimmed_delta_r$user_id)
    return(output_log)
}

poly_format <- function(mode = 'weights') {
    if (mode == 'degrees') {
        dataframe <- data.frame(user_id = numeric(), category = character(), 
                                AIC = numeric(), BIC = numeric(), ANOVA = numeric())
    }
    else {
        dataframe <- data.frame(user_id = numeric(), category = character(),
                                aic_delta_2 = numeric(), aic_delta_3 = numeric(),
                                aic_delta_4 = numeric(), aic_delta_5 = numeric(),
                                bic_delta_2 = numeric(), bic_delta_3 = numeric(),
                                bic_delta_4 = numeric(), bic_delta_5 = numeric(),
                                aic_weights_2 = numeric(), aic_weights_3 = numeric(),
                                aic_weights_4 = numeric(), aic_weights_5 = numeric(),
                                bic_weights_2 = numeric(), bic_weights_3 = numeric(),
                                bic_weights_4 = numeric(), bic_weights_5 = numeric())
    }
    return(dataframe)
}

poly_fit <- function(data, raw) {
    fit_2 = lm(response_in_seconds ~ poly(trials_category,2), data = data)
    fit_3 = lm(response_in_seconds ~ poly(trials_category,3), data = data)
    fit_4 = lm(response_in_seconds ~ poly(trials_category,4), data = data)
    fit_5 = lm(response_in_seconds ~ poly(trials_category,5), data = data)
    fits <- list(fit_2, fit_3, fit_4, fit_5)
    aic_values <- sapply(fits, AIC)
    bic_values <- sapply(fits, BIC)
    if (raw == 0){
        anova <- anova(fit_2,fit_3,fit_4,fit_5)
        aic_degrees <- which.min(aic_values) + 1 # lowest index is 2 degrees
        bic_degrees <- which.min(bic_values) + 1
        anova_degrees <- 2
        p_vals <- anova$`Pr(>F)`
        sig_p_index <- which(p_vals < .05)
        if (length(sig_p_index) > 0) {
            min_p<- which.min(p_vals[sig_p_index])
            anova_degrees <- sig_p_index[min_p] + 1
        }
    }
    if (raw == 1){
        return(c(aic_values, bic_values)) # return raw values
    }
    else {
        return(c(aic_degrees, bic_degrees, anova_degrees)) # return number of degrees
    }
}

degrees_chooser <- function(cat, data, raw = 0) {
    
    cat_data = data %>% filter(category == cat)
    
    poly_results <- data.frame(user_id = numeric(), category = character(), 
                               AIC = numeric(), BIC = numeric(), ANOVA = numeric())
    
    i_ds <- cat_data %>% distinct(user_id)
    for (i_d in i_ds$user_id) {
        test <- cat_data %>% filter(user_id == i_d)
        if (nrow(test) >= 20) {
            result <- poly_fit(test, raw)
            
            output <- data.frame(user_id = i_d, category = cat, AIC = result[1],  
                                 BIC = result[2], ANOVA = result[3], 
                                 trial_number = nrow(test), 
                                 min_rt = min(test$response_in_seconds),
                                 max_rt = max(test$response_in_seconds))
            if (raw == 1){ 
                # Output AIC/BIC weights
                output$aic_delta_2 <- delta_IC(result[1:4])[1]
                output$aic_delta_3 <- delta_IC(result[1:4])[2]
                output$aic_delta_4 <- delta_IC(result[1:4])[3]
                output$aic_delta_5 <- delta_IC(result[1:4])[4]
                output$bic_delta_2 <- delta_IC(result[5:8])[1]
                output$bic_delta_3 <- delta_IC(result[5:8])[2]
                output$bic_delta_4 <- delta_IC(result[5:8])[3]
                output$bic_delta_5 <- delta_IC(result[5:8])[4]
                output$aic_weights_2 <- IC_weights(delta_IC(result[1:4]))[1]
                output$aic_weights_3 <- IC_weights(delta_IC(result[1:4]))[2]
                output$aic_weights_4 <- IC_weights(delta_IC(result[1:4]))[3]
                output$aic_weights_5 <- IC_weights(delta_IC(result[1:4]))[4]
                output$bic_weights_2 <- IC_weights(delta_IC(result[5:8]))[1]
                output$bic_weights_3 <- IC_weights(delta_IC(result[5:8]))[2]
                output$bic_weights_4 <- IC_weights(delta_IC(result[5:8]))[3]
                output$bic_weights_5 <- IC_weights(delta_IC(result[5:8]))[4]
            }
            
            poly_results <- bind_rows(poly_results, output)
            
            
            
            
        }
        else {
            next
        }
    }
    return(poly_results)
}

poly_output <- function(log, mode='weights') {
    categories <- c("Hour", "Half hour", "Quarter past/to")
    repetitions <- rep(4, 6)
    test_counts <- data.frame(category = rep(c('Hour', 'Half hour', 'Quarter past/to',
                                               'Ten past/to', 'Five past/to', 'Minutes past/to'), times = rep(4, 6)), 
                              degrees = rep(2:5, times=3))
    test_counts <- test_counts %>% mutate(category = as.factor(category),
                                          degrees = as.factor(degrees))
    if (mode == 'freq') {
        log <- log %>% mutate(AIC = as.factor(AIC), 
                              BIC = as.factor(BIC),
                              ANOVA = as.factor(ANOVA))
        aic_counts <- log %>% group_by(category, AIC) %>% count() %>% rename(aic_count = n)
        bic_counts <- log %>% group_by(category, BIC) %>% count() %>% rename(bic_count = n)
        anova_counts <- log %>% group_by(category, ANOVA) %>% count() %>% rename(anova_count = n)
        
        
        test_counts <- left_join(test_counts, aic_counts, by = c('category', 'degrees' = 'AIC'))
        test_counts <- left_join(test_counts, bic_counts, by = c('category', 'degrees' = 'BIC'))
        test_counts <- left_join(test_counts, anova_counts, by = c('category', 'degrees' = 'ANOVA'))
        
    }
    else if (mode == 'weights'){
        aic_mean_delta <- log %>% group_by(category) %>% 
            summarize(mean(aic_delta_2), mean(aic_delta_3), mean(aic_delta_4), mean(aic_delta_5))
        aic_mean_delta <- pivot_longer(aic_mean_delta, cols = -category) %>% # Move columns to rows
            mutate(degrees = str_extract(name, '\\d')) %>% select(-name) 
        # Convert names to degrees by extracting digit
        
        bic_mean_delta <- log %>% group_by(category) %>% 
            summarize(mean(bic_delta_2), mean(bic_delta_3), mean(bic_delta_4), mean(bic_delta_5))
        bic_mean_delta <- pivot_longer(bic_mean_delta, cols = -category) %>% 
            mutate(degrees = str_extract(name, '\\d')) %>% select(-name)
        
        aic_mean_weight <- log %>% group_by(category) %>% 
            summarize(sum(aic_weights_2)/n(), sum(aic_weights_3)/n(), 
                      sum(aic_weights_4)/n(),sum(aic_weights_5)/n())
        aic_mean_weight <- pivot_longer(aic_mean_weight, cols = -category) %>%
            mutate(degrees = str_extract(name, '\\d')) %>% select(-name)
        
        bic_mean_weight <- log %>% group_by(category) %>% 
            summarize(sum(bic_weights_2)/n(), sum(bic_weights_3)/n(), 
                      sum(bic_weights_4)/n(),sum(bic_weights_5)/n())
        bic_mean_weight <- pivot_longer(bic_mean_weight, cols = -category) %>%
            mutate(degrees = str_extract(name, '\\d')) %>% select(-name)
        
        
        test_counts <- left_join(test_counts, aic_mean_delta, by = c('category', 'degrees'))
        test_counts <- left_join(test_counts, aic_mean_weight, by = c('category', 'degrees'))
        test_counts <- left_join(test_counts, bic_mean_delta, by = c('category', 'degrees'))
        test_counts <- left_join(test_counts, bic_mean_weight, by = c('category', 'degrees'))
        test_counts <- test_counts %>%   rename(mean_delta_aic = value.x,
                                                mean_aic_weight = value.y,
                                                mean_delta_bic = value.x.x,
                                                mean_bic_weight = value.y.y)
    }
    return(test_counts)
}

get_mode <- function(values) {
    uniq <- unique(values)
    uniq[which.max(tabulate(match(values, uniq)))]
}


delta_IC <- function(values) {
    min <- which.min(values)
    deltas <- values - values[min]
    return(deltas)
}

relative_likelihood <- function(value) {
    return(exp(-.5 * value))
}

IC_weights <- function(values) {
    likelihoods <- c()
    for (value in values) {
        likelihoods <- c(likelihoods, relative_likelihood(value))
    }
    return(likelihoods / sum(likelihoods)) # Normalize likelihoods
}


logs_to_weights <- function(cat) {
    cat_log <- logs_correct %>% filter(category == cat)
    uniq_ids <- cat_log %>% distinct(user_id)
    print(nrow(uniq_ids))
    
    weights_aic <- data.frame(AIC_lin = 0, AIC_poly = 0, AIC_exp = 0)
    weights_bic <- data.frame(BIC_lin = 0, BIC_poly = 0, BIC_exp = 0)
    r_lin <- 0
    r_poly <- 0
    r_exp <- 0
    rows <- 0
    
    for (user_i_d in uniq_ids$user_id) {
        # print(user_i_d)
        test <- cat_log %>% filter(user_id == 'user_i_d') %>% filter(trials_category < 31)
        # print(user_i_d) #debug user
        lin_model <- lm(response_in_seconds ~ trials_category, data = test)
        r_lin <- r_lin + summary(lin_model)$adj.r.squared
        poly_model <- lm(response_in_seconds ~ poly(trials_category, 2), data = test)
        r_poly <- r_poly + summary(poly_model)$adj.r.squared
        exp_model <- lm(response_in_seconds ~ exp(-trials_category), data = test)
        r_exp <- r_exp + summary(exp_model)$adj.r.squared
        
        aic <- c(AIC(lin_model), AIC(poly_model), AIC(exp_model))
        bic <- c(BIC(lin_model), BIC(poly_model), BIC(exp_model))
        
        ic_weights_aic <- IC_weights(delta_IC(aic))
        ic_weights_bic <- IC_weights(delta_IC(bic))
        
        weights_aic <- weights_aic + ic_weights_aic
        weights_bic <- weights_bic + ic_weights_bic
        
        rows <- rows + 1
    }
    print(r_lin/rows)
    return(list(weights_aic / rows, weights_bic / rows))
}

start_vals <- function(diff, cat) {
    if (diff == 0 & cat == 'Hour'){
        return(c(yf = 10.8 , y0 = 54.4, alpha = 4.8, beta = -4.8))
    }
    else if (diff == 0 & cat == 'Half') {
        return(c(yf = 10.3, y0 = 18.2, a = -0.2))
    }
    else if (diff == 0 & cat == 'Quarter') {
        return(c(yf = 9.9, y0 = 16, a = -0.4))
    }
    else if (diff == 1 & cat == 'Hour'){
        return(c(yf = 12.3, y0 = 21.7, a = -0.01))
    }
    else if (diff == 1 & cat == 'Half') {
        return(c(yf = 12.1, y0 = 20, a = -1.8))
    }
    else if (diff == 1 & cat == 'Quarter') {
        return(c(yf = 12.1, y0 = 17.3, a = -0.8))
    }
    else if (diff == 2 & cat == 'Hour'){
        return(c(yf = 13.2, y0 = 18.7, a = -0.2))
    }
    else if (diff == 2 & cat == 'Half') {
        return(c(yf = 12.9, y0 = 19.1, a = -0.8))
    }
    else if (diff == 2 & cat == 'Quarter') {
        return(c(yf = 13.1, y0 = 19.8, a = -0.9))
    }
}         

group_data <- function(log, diff, cat) {
    
    grouped_data <- groupedData(response_in_seconds ~ trials_category | user_id, log) %>% 
        filter(trials_category < 50, difficulty == diff)
    grouped_data <- grouped_data %>% filter(category == cat)
    return(grouped_data)
}
