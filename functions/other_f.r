
subset_group <- function(df, condition){

        test <- subset(df, treat == 1 | treat == condition)

        test$treat_f <- ifelse(test$treat == condition, 1, 0)

        test 

}

design_test <- function(df, condition){

        test <- subset_group(df, condition)
        
        ict.test(test$indirect, test$treat_f, J = 3, gms = TRUE)

}

normalize <- function(x) {
        return ((x - min(x)) / (max(x) - min(x)))
}

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