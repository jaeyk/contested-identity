replace_value <- function(data, var1, var2){
        
        data$nk_attitude[data$nk_attitude == {{var1}}] <- var2
 
        data 
}

replace_party <- function(data){

data$PARTYID[data$PARTYID >= 66] <- NA
data$PARTYID[data$PARTYID >= 77] <- NA
data$PARTYID[data$PARTYID >= 88] <- NA

data$party_f <- ifelse(data$PARTYID == 1 | data$PARTYID == 3 | data$PARTYID == 4 | data$PARTYID == 9, 1, 0)

data 

}

replace_partyid <- function(data){
        
        data$partyid[data$partyid >= 66] <- NA
        data$partyid[data$partyid >= 77] <- NA
        data$partyid[data$partyid >= 88] <- NA
        
        data$party_f <- ifelse(data$partyid == 1 | data$partyid == 3 | data$partyid == 4 | data$partyid == 9, 1, 0)
        
        data 
        
}

analyze_multi_ate <- function(data){
        
treat <- bind_cols(data$par.treat %>% tidy(), data$se.treat %>% tidy())

treat <- treat[,-3]

colnames(treat) <- c("Name", "Estimate", "SE")

control <- bind_cols(data$par.control %>% tidy(), data$se.control %>% tidy())

control <- control[,-3]

colnames(control) <- c("Name", "Estimate", "SE")

result <- bind_rows(mutate(treat, Assignment = "Treatment"),
                    mutate(control, Assignment = "Control"))
result 
}

plot_multi_ate <- function(result){

result %>%
        filter(Name != "(Intercept)") %>%
        ggplot(aes(fct_reorder(Name, Estimate), Estimate, ymax = Estimate + SE, ymin = Estimate - SE)) +
        geom_pointrange() +
        facet_wrap(~Assignment) +
        geom_hline(yintercept = c(0), linetype = "dashed") +
        labs(x = "") +
        ylim(c(-4,4)) +
        coord_flip()
        
}

bind_visualize <- function(df1, df2){
bind_rows(mutate(tidy(df1, conf.int = TRUE), category = "External motivation"),
          mutate(tidy(df2, conf.int = TRUE), category = "Internal motivation")) %>%
        filter(term != "(Intercept)") %>%
        ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
        geom_pointrange() +
        coord_flip() +
        labs(y = "Estimate",
             x ="")+
        facet_wrap(~category) +
        geom_hline(yintercept = c(0), linetype = "dashed")}