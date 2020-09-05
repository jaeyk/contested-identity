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