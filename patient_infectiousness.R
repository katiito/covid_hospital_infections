library(ggplot2)
library(dplyr)
library(tidyr)

library(gridExtra)

# run_hospital_analysis <- function(){
  
  # CAs only infecting the community
  comm2comm <- communityinfections_stayin_community(duration = "indpt")
  # CAs infecting the hospital and community
  comm2hosp <- communityinfections_goto_hospital(duration = "hosp") # OK
  # HAs infecting the hospital and community
  hosp2comm <- hospitalinfections(duration = "indpt") # OK
  # HAs infecting the hospital, community and hospital on readmission for covid
  hosp2hospReadmitted <- hospitalinfections_readmitted(duration = "hosp", hosp_speed = "normal")
  hosp2hospReadmitted_notcovid <- ######
  
  # 1. number days a CA spends in community assuming stays in comm  NONHOSP DURATION
  CAcomm_comm <- comm2comm$inf_total 
  # 2. number of days a CA spends in community or hosp assuming goes to hosp for covid
  CAcomm_hosp <- comm2hosp$inf_total - comm2hosp$infectious_days_hosp
  CAhosp_hosp <- comm2hosp$infectious_days_hosp
  # 3. number of days a CA spends in community or hosp assuming goes to hosp for non-covid NONHOSP DURATION
  CAcomm_nchosp <- runif(num_samples, min = replicate(num_samples,0), max = comm2comm$inf_total) ### could update this with the comm2comm 
  CAhosp_nchosp <-  comm2comm$inf_total - CAcomm_nchosp ### could update this with the comm2comm 
  # 4. number days a HA spends in community or hospital given discharge to community NONHOSP DURATION
  HAcomm_comm <- hosp2comm$inf_total - hosp2comm$infectious_days_hosp #### could update this with the comm2comm
  HAhosp_comm <- hosp2comm$infectious_days_hosp ### could update this with the comm2comm
  # 5. number days a HA spends in community or hospital given readmittance to hosp for covid
  HAhosp_hosp <- hosp2hospReadmitted$infectious_days_hosp
  HAcomm_hosp <- hosp2hospReadmitted$inf_total - HAhosp_hosp
  # 6. number days a HA spends in community or hospital given readmittance to hosp for noncovid NONHOSP DURATION
  HAcomm_nchosp <- runif(num_samples, min = replicate(num_samples,0), max = HAcomm_comm)
  HAhosp_nchosp <- hosp2comm$inf_total - HAcomm_nchosp

  
  # time to readmission = time to hospitalisation
par(mfrow=c(3,4))  
 hist(CAcomm_comm)
 hist(CAcomm_hosp)
 hist(CAhosp_hosp)
 hist(CAcomm_nchosp)
 hist(CAhosp_nchosp)
 hist(HAcomm_comm)
 hist(HAhosp_comm)
 hist(HAcomm_hosp)
 hist(HAhosp_hosp)
 hist(HAcomm_nchosp)
 hist(HAhosp_nchosp)
 plot(HAcomm_nchosp, HAhosp_nchosp)
  
  # infectious days spent in hospital
  infectious_days <- bind_cols("CA_in_hosp" = comm2hosp$infectious_days, 
                               "HA_in_hosp" = hosp2hosp$infectious_days,
                               "CA_overall" = comm2hosp$inf_total,
                               "HA_overall" = hosp2hosp$inf_total)
                          
          
  # fraction of infectious days spent in hospital for community acquired
  infectious_days <- infectious_days %>%
        mutate(CA_spent_in_hosp_frac =  CA_in_hosp / CA_overall) %>%
        mutate(HA_spent_in_hosp_frac = HA_in_hosp / HA_overall) %>%
        mutate(CA_frac_of_hosp_infectious = CA_in_hosp / (CA_in_hosp + HA_in_hosp)) 
  # fraction of infectious days spent in hospital for hospital acquired
  
  
  
  infectious_days_fractions <- infectious_days %>%
      select(CA_spent_in_hosp_frac, HA_spent_in_hosp_frac, CA_frac_of_hosp_infectious)
  
  infectious_days_durations <- infectious_days %>%
    select(CA_in_hosp, HA_in_hosp)
  # infectious_days_in_hosp <- infectious_days %>%
  #           filter(from_comm_in_hosp != 0 & from_hosp_in_hosp != 0)
  # 

  # data_out <- bind_rows("comm_spent_in_hosp_frac" = comm_spent_in_hosp_frac, 
  #                        "hosp_spent_in_hosp_frac" = hosp_spent_in_hosp_frac,
  #                        "comm_frac_of_hosp_infections" = comm_frac_of_hosp_infections,
  #                        .id = "var")
  
  infectious_days_fractions_long <- pivot_longer(infectious_days_fractions, 
                                  cols = names(infectious_days_fractions),
                                  names_to = "variable",
                                  values_to = "fraction")
  infectious_days_durations_long <- pivot_longer(infectious_days_durations, 
                                                 cols = names(infectious_days_durations),
                                                 names_to = "variable",
                                                 values_to = "days")
  
  # given infection is acquired within comm or hosp, number days spent infectious at the hospital
  p <- ggplot(data = infectious_days_durations_long, aes(x = variable, y = days))  + 
          geom_violin()
  # given infection is acquired within comm or hosp, fraction of time spent infectious at the hospital
  # fraction of infections that are in hosp acquired in community
  q <- ggplot(data = infectious_days_fractions_long, aes(x = variable, y = fraction))  + 
    geom_violin()
  
  
# grid.arrange(p,q)
# }



hospitalinfections <- function(duration_type){
  
  
  p <- readParameters(duration_type)
  
  # infection at any point during los -relative to hospital admission
  
  time_of_hosp_infection <- runif(p$num_samples, min = replicate(p$num_samples,0), max = p$duration_hospital_stay)
  time_to_infectious <- time_of_hosp_infection + p$latent_duration
  
  # calculate proportion days of hospital-acquired spend in hosp based on independent distribution
  time_to_not_infectious <- time_to_infectious + p$infectious_duration
  
  days_infectious_in_hosp <- pmax(0, p$duration_hospital_stay - time_to_infectious) - pmax(0, p$duration_hospital_stay - time_to_not_infectious) 
  days_infectious_in_hosp_positive <- days_infectious_in_hosp[days_infectious_in_hosp>0]
  prop_days_inf_in_hosp <- sum(days_infectious_in_hosp) / sum(p$infectious_duration)
  
  
  # Output 
  cat(p$output_message, "\n") 
  cat("Proportion of days spent infectious in hosp = ", round(prop_days_inf_in_hosp,3), "\n\n") 
  return(list("infectious_days_hosp" = days_infectious_in_hosp, "inf_total" = p$infectious_duration))

}


hospitalinfections_readmitted <- function(duration_type, hosp_speed){
  
  p <- readParameters(duration_type)
  
   time_of_hosp_infection <- runif(p$num_samples, min = replicate(p$num_samples,0), max = p$duration_hospital_stay)
   time_to_infectious <- time_of_hosp_infection + p$latent_duration
  # 
  # assuming severe infection are readmitted to hosp on onset symptoms / normal time
  if(hosp_speed=="fast"){
    time_until_readmission <- time_of_hosp_infection + p$latent_duration + p$preclinical_duration
  } else if(hosp_speed=="normal"){
    time_until_readmission <- time_of_hosp_infection + p$latent_duration + p$preclinical_duration + p$hospital_delayfromonset
    
  }
  time_until_discharge <- pmin(time_until_readmission, p$duration_hospital_stay)
  time_until_discharge_second <- time_until_readmission + p$hospital_duration
  
  
  # calculate proportion days of hospital-acquired spend in hosp based on independent distribution
  time_to_not_infectious <- time_to_infectious + p$infectious_duration
  days_infectious_in_hosp_first <- pmax(0, time_until_discharge - time_to_infectious) - pmax(0, time_until_discharge - time_to_not_infectious) 
  days_infectious_in_hosp_second <- pmax(0, pmin(time_until_discharge_second, time_to_not_infectious) - pmax(time_until_readmission, time_to_infectious) )
  days_infectious_in_hosp <- days_infectious_in_hosp_first + days_infectious_in_hosp_second
  days_infectious_in_hosp_positive <- days_infectious_in_hosp[days_infectious_in_hosp>0]
  prop_days_inf_in_hosp <- sum(days_infectious_in_hosp) / sum(p$infectious_duration)
  
 
  
  # Output 
  
  cat(p$output_message, "\n") 
  cat("Proportion of days spent infectious in hosp = ", round(prop_days_inf_in_hosp,3), "\n\n")
  return(list("infectious_days_hosp" = days_infectious_in_hosp, "inf_total" = p$infectious_duration))
  
}


communityinfections_goto_hospital <- function(duration_type){
    
    p <- readParameters(duration_type)
   
    time_to_hospital_admission <- p$latent_duration + p$preclinical_duration + p$hospital_delayfromonset
    time_to_not_infectious <- p$latent_duration + p$infectious_duration
    
    ## Calculate the time infectious after admitted to hosptial (infectious always happens before hosp)
    time_infectious_after_admission_unltd <- time_to_not_infectious - time_to_hospital_admission
    time_infectious_after_admission <- pmax(0,time_infectious_after_admission_unltd)
    days_infectious_in_hosp <- pmin(p$hospital_duration, time_infectious_after_admission)
    days_infectious_in_hosp_positive <- days_infectious_in_hosp[days_infectious_in_hosp>0]
    prob_infectious_in_hospital <- sum(days_infectious_in_hosp>0) / p$num_samples
    prop_infectious_days_in_hospital <- sum(days_infectious_in_hosp) / sum(p$infectious_duration)
   
    
    # Output 
    cat(p$output_message, "\n") 
    cat("Proportion of days spent infectious in hosp = ", round(prop_infectious_days_in_hospital,3), "\n\n") 
    return(list("infectious_days_hosp" = days_infectious_in_hosp, "inf_total" = p$infectious_duration))
} 

communityinfections_stayin_community <- function(duration_type){
  
  p <- readParameters(duration_type)
  days_infectious_in_hosp <- replicate(p$num_samples, 0) 
  prob_infectious_in_hospital <- 0
  prop_infectious_days_in_hospital <- 0
  
  
  # Output 
  cat(p$output_message, "\n") 
  cat("Proportion of days spent infectious in hosp = ", round(prop_infectious_days_in_hospital,3), "\n\n") 
  return(list("infectious_days_hosp" = days_infectious_in_hosp, "inf_total" = p$infectious_duration))
} 
