
visualize_multi_ate_group <- function(df, condition, model, subgroup, low, high){
        
        test <- subset_group(df, condition)
        test$age <- normalize(test$age)
        test$income <- normalize(test$income)
        
        ate.ideo.q1 <- ictreg(direct ~ ideo_con + ideo_lib + income + men + college + age, treat = "treat_f", 
                              J = 3, 
                              data = test, 
                              method = model)
        
        ate.par.q1 <- ictreg(direct ~ party_con + party_lib + income + men + college + age, treat = "treat_f", 
                             J = 3, 
                             data = test, 
                             method = model)
        
        ate.ideo.q2 <- ictreg(indirect ~ ideo_con + ideo_lib + income + men + college + age, treat = "treat_f", 
                              J = 3, 
                              data = test, 
                              method = model)
        
        ate.par.q2 <- ictreg(indirect ~ party_con + party_lib + income + men + college + age, treat = "treat_f", 
                             J = 3, 
                             data = test, 
                             method = model) 
        
        party <- bind_rows(mutate(analyze_multi_ate(ate.par.q1),
                                   Type = "Direct bias", Condition = "Partisanship"), 
                           mutate(analyze_multi_ate(ate.par.q2),
                                  Type = "Indirect bias", Condition = "Partisanship"))
                           
        ideo <- bind_rows(mutate(analyze_multi_ate(ate.ideo.q1),
                                   Type = "Direct bias", Condition = "Ideology"),
                            mutate(analyze_multi_ate(ate.ideo.q2),
                                   Type = "Indirect bias", Condition = "Ideology"))
        
        party_plot <- party %>%
                filter(Name != "(Intercept)") %>%
                mutate(Name = fct_relevel(Name, c("men", "age", "college", "income", "party_lib", "party_con"))) %>%
                ggplot(aes(Name, Estimate, ymax = Estimate + SE, ymin = Estimate - SE)) +
                geom_pointrange() +
                geom_hline(yintercept = c(0), linetype = "dashed") +
                labs(x = "") +
                coord_flip() +
                facet_grid(~Type) +
                scale_x_discrete(labels = 
                                         c("income" = "Income",
                                           "age" = "Age",
                                           "party_lib" = "Liberal",
                                           "men" = "Male",
                                           "college" = "College",
                                           "party_con" = "Conservative")
                                           ) +
                labs(subtitle = "Partisanship",
                     title = {{subgroup}}) +
                ylim(c({{low}}, {{high}})) 
        
        ideo_plot <- ideo %>%
                filter(Name != "(Intercept)") %>%
                mutate(Name = fct_relevel(Name, c("men", "age", "college", "income", "ideo_lib", "ideo_con"))) %>%
                ggplot(aes(Name, Estimate, ymax = Estimate + SE, ymin = Estimate - SE)) +
                geom_pointrange() +
                geom_hline(yintercept = c(0), linetype = "dashed") +
                labs(x = "") +
                coord_flip() +
                facet_grid(~Type) +
                scale_x_discrete(labels = 
                                         c("income" = "Income",
                                           "age" = "Age",
                                           "men" = "Male",
                                           "college" = "College",
                                           "ideo_lib" = "Liberal",
                                           "ideo_con" = "Conservative")
                ) +
                labs(subtitle = "Ideology") +
                ylim(c({{low}}, {{high}}))
        
        party_plot + ideo_plot 
        
}


table_multi_ate <- function(df, condition, model, subgroup, low, high){
        
        test <- subset_group(df, condition)
        test$age <- normalize(test$age)
        test$income <- normalize(test$income)
        
        ate.ideo.q1 <- ictreg(direct ~ ideo_con + ideo_lib + income + men + college + age, treat = "treat_f", 
                              J = 3, 
                              data = test, 
                              method = model)
        
        ate.par.q1 <- ictreg(direct ~ party_con + party_lib + income + men + college + age, treat = "treat_f", 
                             J = 3, 
                             data = test, 
                             method = model)
        
        ate.ideo.q2 <- ictreg(indirect ~ ideo_con + ideo_lib + income + men + college + age, treat = "treat_f", 
                              J = 3, 
                              data = test, 
                              method = model)
        
        ate.par.q2 <- ictreg(indirect ~ party_con + party_lib + income + men + college + age, treat = "treat_f", 
                             J = 3, 
                             data = test, 
                             method = model) 
        
        party <- bind_rows(mutate(analyze_multi_ate(ate.par.q1),
                                  Type = "Direct bias", Condition = "Partisanship"), 
                           mutate(analyze_multi_ate(ate.par.q2),
                                  Type = "Indirect bias", Condition = "Partisanship"))
        
        ideo <- bind_rows(mutate(analyze_multi_ate(ate.ideo.q1),
                                 Type = "Direct bias", Condition = "Ideology"),
                          mutate(analyze_multi_ate(ate.ideo.q2),
                                 Type = "Indirect bias", Condition = "Ideology"))
        
        bind_rows(party, ideo) 
        
}

analyze_multi_ate <- function(data){
        
        treat <- bind_cols(data$par.treat %>% tidy(), data$se.treat %>% tidy())
        
        treat <- treat[,-3]
        
        colnames(treat) <- c("Name", "Estimate", "SE")
        
        control <- bind_cols(data$par.control %>% tidy(), data$se.control %>% tidy())
        
        control <- control[,-3]
        
        colnames(control) <- c("Name", "Estimate", "SE")
        
        treat
}

plot_multi_ate <- function(result){
        
        result %>%
                filter(Name != "(Intercept)") %>%
                ggplot(aes(fct_reorder(Name, Estimate), Estimate, ymax = Estimate + SE, ymin = Estimate - SE)) +
                geom_pointrange() +
                geom_hline(yintercept = c(0), linetype = "dashed") +
                labs(x = "") +
                coord_flip()
        
}


ideo_label <- function(result){
        
        result %>%
                filter(Name != "(Intercept)") %>%
                ggplot(aes(fct_reorder(Name, Estimate), Estimate, ymax = Estimate + SE, ymin = Estimate - SE)) +
                geom_pointrange() +
                geom_hline(yintercept = c(0), linetype = "dashed") +
                labs(x = "") +
                coord_flip() +
                scale_x_discrete(labels = 
                                         c("income" = "Income",
                                           "age" = "Age",
                                           "ideo_lib" = "Liberal",
                                           "men" = "Male",
                                           "college" = "College",
                                           "ideo_con" = "Conservative"))
        
}

party_label <- function(result){
        
        result %>%
                filter(Name != "(Intercept)") %>%
                ggplot(aes(fct_reorder(Name, Estimate), Estimate, ymax = Estimate + SE, ymin = Estimate - SE)) +
                geom_pointrange() +
                geom_hline(yintercept = c(0), linetype = "dashed") +
                labs(x = "") +
                coord_flip() +
                scale_x_discrete(labels = 
                                         c("income" = "Income",
                                           "age" = "Age",
                                           "party_lib" = "Liberal",
                                           "men" = "Male",
                                           "college" = "College",
                                           "party_con" = "Conservative"))
        
}


select_nk_party <-function(data){
        dplyr::select(data, northwho, partyid)}

calculate_cate <- diff_means_test <- function(data) {
        
        diff_summary <- data %>%
                
                # Summarize 
                summarise_each(
                        funs(
                                
                                # Different in means 
                                diff_t = mean(.[treat == 3], na.rm = T) - mean(.[treat == 1], na.rm = T),
                                
                                # Calculating confidence intervals
                                
                                
                                conf_t = (t.test(.[treat == 3], .[treat == 1])$conf.int[2] - t.test(.[treat == 1], .[treat == 1])$conf.int[1]) / 2,
                                
                        ),
                        direct, indirect
                )
        
        diff_summary %>%
                gather(stat, val) %>% # stat = variables, val = values 
                separate(stat, into = c("var", "stat", "treat"), sep = "_") %>% # var = measures, stat = diff or conf, group = treatment status, val = values  
                spread(stat, val) %>% # reorder columns
                mutate(var = replace(var, var == "direct", "Direct bias")) %>% # rename variables 
                mutate(var = replace(var, var == "indirect", "Indirect bias")) 
        
}

        
diff_means_test <- function(data) {
        
        diff_summary <- data %>%
                
                # Summarize 
                summarise_each(
                        funs(
                                
                                # Different in means 
                                diff_t1 = mean(.[treat == 2], na.rm = T) - mean(.[treat == 1], na.rm = T),
                                diff_t2 = mean(.[treat == 3], na.rm = T) - mean(.[treat == 1], na.rm = T),
                                diff_t3 = mean(.[treat == 4], na.rm = T) - mean(.[treat == 1], na.rm = T),
                                diff_t4 = mean(.[treat == 5], na.rm = T) - mean(.[treat == 1], na.rm = T),
                                
                                # Calculating confidence intervals
                                conf_t1 = (t.test(.[treat == 2], .[treat == 1])$conf.int[2] - t.test(.[treat == 1], .[treat == 1])$conf.int[1]) / 2,
                                conf_t2 = (t.test(.[treat == 3], .[treat == 1])$conf.int[2] - t.test(.[treat == 1], .[treat == 1])$conf.int[1]) / 2,
                                conf_t3 = (t.test(.[treat == 4], .[treat == 1])$conf.int[2] - t.test(.[treat == 1], .[treat == 1])$conf.int[1]) / 2,
                                conf_t4 = (t.test(.[treat == 5], .[treat == 1])$conf.int[2] - t.test(.[treat == 1], .[treat == 1])$conf.int[1]) / 2
                        ),
                        direct, indirect
                )
        
        diff_summary %>%
                gather(stat, val) %>% # stat = variables, val = values 
                separate(stat, into = c("var", "stat", "treat"), sep = "_") %>% # var = measures, stat = diff or conf, group = treatment status, val = values  
                spread(stat, val) %>% # reorder columns
                mutate(var = replace(var, var == "direct", "Direct bias")) %>% # rename variables 
                mutate(var = replace(var, var == "indirect", "Indirect bias")) 
        
}


boot_dmt <- function(data) {
        
        diff_summary <- data %>%
                
                # Summarize 
                summarise_each(
                        funs(
                                
                                # Different in means 
                                diff_t1 = mean(.[treat == 2], na.rm = T) - mean(.[treat == 1], na.rm = T),
                                diff_t2 = mean(.[treat == 3], na.rm = T) - mean(.[treat == 1], na.rm = T),
                                diff_t3 = mean(.[treat == 4], na.rm = T) - mean(.[treat == 1], na.rm = T),
                                diff_t4 = mean(.[treat == 5], na.rm = T) - mean(.[treat == 1], na.rm = T),
                                
                                # Calculating confidence intervals
                                conf_t1 = (MKinfer::boot.t.test(.[treat == 2], .[treat == 1], R = 1000)$boot.conf.int[2] - MKinfer::boot.t.test(.[treat == 1], .[treat == 1], R = 1000)$boot.conf.int[1]) / 2,
                                conf_t2 = (MKinfer::boot.t.test(.[treat == 3], .[treat == 1], R = 1000)$boot.conf.int[2] - MKinfer::boot.t.test(.[treat == 1], .[treat == 1], R = 1000)$boot.conf.int[1]) / 2,
                                conf_t3 = (MKinfer::boot.t.test(.[treat == 4], .[treat == 1], R = 1000)$boot.conf.int[2] - MKinfer::boot.t.test(.[treat == 1], .[treat == 1], R = 1000)$boot.conf.int[1]) / 2,
                                conf_t4 = (MKinfer::boot.t.test(.[treat == 5], .[treat == 1], R = 1000)$boot.conf.int[2] - MKinfer::boot.t.test(.[treat == 1], .[treat == 1], R = 1000)$boot.conf.int[1]) / 2
                        ),
                        direct, indirect
                )
        
        diff_summary %>%
                gather(stat, val) %>% # stat = variables, val = values 
                separate(stat, into = c("var", "stat", "treat"), sep = "_") %>% # var = measures, stat = diff or conf, group = treatment status, val = values  
                spread(stat, val) %>% # reorder columns
                mutate(var = replace(var, var == "direct", "Direct bias")) %>% # rename variables 
                mutate(var = replace(var, var == "indirect", "Indirect bias")) 
        
}