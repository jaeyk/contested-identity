
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