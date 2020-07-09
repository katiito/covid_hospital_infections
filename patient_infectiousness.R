library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
source("parameters.R")

patientInfectiousness <- function(hosp_detection = "slow"){
          
          
          # num_samples <- 10000
          p <- readParameters(duration = "indpt", hosp_speed = hosp_detection)
        # run_hospital_analysis <- function(){
          
          # CAs only infecting the community
          comm2comm <- communityinfections_stayin_community(duration = "indpt", hosp_speed = hosp_detection)
          # CAs infecting the hospital and community
          comm2hosp <- communityinfections_goto_hospital(duration = "hosp", hosp_speed = hosp_detection)
          # HAs infecting the hospital and community
          hosp2comm <- hospitalinfections(duration = "indpt", hosp_speed = hosp_detection) # OK
          # HAs infecting the hospital, community and hospital on readmission for covid 
          # (severe cases that would usually be hospitalised)
          hosp2hospReadmitted_severe <- hospitalinfections_readmitted(duration = "hosp", hosp_speed = hosp_detection)
          
          
          
          # HAs infecting the hospital, community and hospital on readmission for covid (but are only hospitalised because they are already on ward)
          hosp2hospReadmitted_nonsevere <- hospitalinfections_readmitted(duration = "indpt", hosp_speed = hosp_detection)
          
          # UPDATE WITH stratification - those sent home and those not sent home
          
          
          # hosp2hospReadmitted_notcovid <- ######
          
          # 1. number days a CA spends in community assuming stays in comm  NONHOSP DURATION
          CAcomm_comm <- comm2comm$inf_total 
          
          # 2. number of days a CA spends in community or hosp assuming goes to hosp for covid
          CAcomm_hosp <- comm2hosp$inf_total - comm2hosp$infectious_days_hosp
          CAhosp_hosp <- comm2hosp$infectious_days_hosp
          CAhosp <- CAhosp_hosp / (CAcomm_hosp + CAhosp_hosp)
          
          
          # 3. number of days a CA spends in community or hosp assuming goes to hosp for non-covid NONHOSP DURATION
        #### ISSUE
          
          CAcomm_nchosp <- runif(p$num_samples, min = replicate(p$num_samples,0), max = comm2comm$inf_total) ### could update this with the comm2comm 
          CAhosp_nchosp <-  comm2comm$inf_total - CAcomm_nchosp ### could update this with the comm2comm 
          CAnchosp <- CAhosp_nchosp / (CAhosp_nchosp + CAcomm_nchosp)
          
          # 4. number days a HA spends in community or hospital given discharge to community NONHOSP DURATION
          HAcomm_comm <- hosp2comm$inf_total - hosp2comm$infectious_days_hosp #### could update this with the comm2comm
          HAhosp_comm <- hosp2comm$infectious_days_hosp 
          HAcomm <- HAhosp_comm / (HAhosp_comm + HAcomm_comm)
          
          
          # 5. number days a HA spends in community or hospital given readmittance to hosp for severe covid (first sent to community)
          
          HAhosp_hosp_severe_senttocommunity <- sample(
                        hosp2hospReadmitted_severe$infectious_days_hosp[hosp2hospReadmitted_severe$sent_back_to_community], p$num_samples, replace=TRUE)
          
          communitydays <- hosp2hospReadmitted_severe$inf_total - hosp2hospReadmitted_severe$infectious_days_hosp
          HAcomm_hosp_severe_senttocommunity <- sample(communitydays[hosp2hospReadmitted_severe$sent_back_to_community], p$num_samples, replace=TRUE)
          HAhosp_severe_senttocommunity <- HAhosp_hosp_severe_senttocommunity / (HAhosp_hosp_severe_senttocommunity + HAcomm_hosp_severe_senttocommunity)
          
        
          # 6. number days a HA spends in community or hospital given readmittance to hosp for severe covid (not sent back to community)
          HAhosp_hosp_severe_remaininhosp <- sample(
                        hosp2hospReadmitted_severe$infectious_days_hosp[!hosp2hospReadmitted_severe$sent_back_to_community], p$num_samples, replace = TRUE)
          # HAcomm_hosp_severe <- hosp2hospReadmitted_severe$inf_total - HAhosp_hosp_severe
          HAhosp_severe_remaininhosp <- replicate(p$num_samples, 1)
          
          # 7. number days a HA spends in community or hospital given readmittance to hosp for non-severe covid
          HAhosp_hosp_nonsevere_remaininhosp <- sample(
                        hosp2hospReadmitted_nonsevere$infectious_days_hosp[!hosp2hospReadmitted_nonsevere$sent_back_to_community], p$num_samples, replace = TRUE)
          # HAcomm_hosp_nonsevere <- hosp2hospReadmitted_nonsevere$inf_total - HAhosp_hosp_nonsevere
          HAhosp_nonsevere_remaininhosp <- replicate(p$num_samples, 1)
          # HAhosp_hosp_nonsevere / (HAhosp_hosp_nonsevere + HAcomm_hosp_nonsevere)
          
          
          # 8. number days a HA spends in community or hospital given readmittance to hosp for noncovid NONHOSP DURATION
          HAcomm_nchosp <- runif(p$num_samples, min = replicate(p$num_samples,0), max = round(HAcomm_comm, digits = 5))
          HAhosp_nchosp <- hosp2comm$inf_total - HAcomm_nchosp
          HAnchosp <- HAhosp_nchosp / (HAhosp_nchosp + HAcomm_nchosp)
          
          ## Relative Transmissions
          # how infectious is a CA in hospital relative to in the community?
          
          # how infectious is a CA in community relative to a HA in community? weighting by prob of route
          
          
          # probCovidHospCA <- p$ProbCovidHosp/(1 + p$ProbOtherHosp)
          # probNonCovidHosp <- p$ProbOtherHosp/(1 + p$ProbOtherHosp)
          # 
          # probCovidHospHA <- p$ProbCovidHosp_readm/(1 + p$ProbOtherHosp)
          
          
          # Need to multiply hosp2hospReadmitted_nonsevere$probReadmission by Prob of symptoms
          # detection_prob <- hosp2hospReadmitted_nonsevere$probReadmission * p$ProbSymptomatic
          detection_prob_Mild <- p$ProbSymptomatic * hosp2hospReadmitted_nonsevere$prob_detect_before_discharge
          detection_prob_Severe <-  hosp2hospReadmitted_severe$prob_detect_before_discharge
          
          cat("Mild case detection prob = ", detection_prob_Mild, "\n") 
          cat("Severe case detection prob = ", detection_prob_Severe, "\n") 
          
          
         
          denominator_prob <- (1-p$ProbCovidHosp) * (1-detection_prob_Mild) + p$ProbCovidHosp * (1-detection_prob_Severe)
          CA_infectiousin_comm <- p$ProbCovidHosp*CAcomm_hosp + 
                                    (1-p$ProbCovidHosp) * (1-p$ProbOtherHosp) * CAcomm_comm +
                                    (1-p$ProbCovidHosp) * p$ProbOtherHosp *CAcomm_nchosp
          
          HA_infectiousin_comm <- (1-p$ProbCovidHosp) * (1-detection_prob_Mild) * (1-p$ProbOtherHosp) * HAcomm_comm  / denominator_prob +
                           (1-p$ProbCovidHosp) * (1-detection_prob_Mild) * p$ProbOtherHosp * HAcomm_nchosp  / denominator_prob +
                           p$ProbCovidHosp * (1-detection_prob_Severe) * HAcomm_hosp_severe_senttocommunity / denominator_prob
          
          
          # HA_infectiousin_comm <- (1-p$ProbCovidHosp) * detection_prob * HAcomm_hosp_nonsevere + 
          #                       p$ProbCovidHosp * HAcomm_hosp_severe + 
          #                      (1-p$ProbCovidHosp) * (1-detection_prob) * (1-p$ProbOtherHosp) * HAcomm_comm + 
          #                       (1-p$ProbCovidHosp) * (1-detection_prob) * p$ProbOtherHosp * HAcomm_nchosp
          # 
          # RelativeCAvsHA_community <- CA_infectiousin_comm / HA_infectiousin_comm
          # RelativeHAvsCA_community <- HA_infectiousin_comm / CA_infectiousin_comm
                                    
          # how infectious is a CA in hosp relative to a HA in community? weighting by prob of route 
            
          # probCovidHospCA <- p$ProbCovidHosp/(p$ProbOtherHosp + p$ProbCovidHosp)
          # probCovidHospCA <- p$ProbCovidHosp/((p$ProbCovidHosp) * p$ProbOtherHosp + p$ProbCovidHosp)
          
          # CA_infectiousin_hosp <- probCovidHospCA*CAhosp_hosp + (1-probCovidHospCA)*CAhosp_nchosp
          covid_prob_hosp_weight_CA <- p$ProbCovidHosp / (p$ProbCovidHosp + (1-p$ProbCovidHosp) * p$ProbOtherHosp)
          
          CA_infectiousin_hosp <- covid_prob_hosp_weight_CA * CAhosp_hosp + 
                                       (1 - covid_prob_hosp_weight_CA)* CAhosp_nchosp
          # HA_infectiousin_hosp <- probCovidHospHA*HAhosp_hosp + 
          #                                (1-probCovidHospHA-probNonCovidHosp)*HAhosp_comm + 
          #                               probNonCovidHosp*HAhosp_nchosp
         
          HA_infectiousin_hosp <- 
                      (1-p$ProbCovidHosp) * (1- detection_prob_Mild) * (1 - p$ProbOtherHosp) * HAhosp_comm +
                      (1-p$ProbCovidHosp) * (1- detection_prob_Mild) * p$ProbOtherHosp * HAhosp_nchosp +
                      (1-p$ProbCovidHosp) * detection_prob_Mild * HAhosp_hosp_nonsevere_remaininhosp +
                      p$ProbCovidHosp * (1 - detection_prob_Severe) * HAhosp_hosp_severe_senttocommunity +  
                      p$ProbCovidHosp * detection_prob_Severe * HAhosp_hosp_severe_remaininhosp
                        
          
           # HA_infectiousin_hosp <- (1-p$ProbCovidHosp) * detection_prob * HAhosp_hosp_nonsevere + 
          #                             p$ProbCovidHosp * HAhosp_hosp_severe + 
          #                             (1-p$ProbCovidHosp) * (1-detection_prob) * (1-p$ProbOtherHosp) * HAhosp_comm + 
          #                             (1-p$ProbCovidHosp) * (1-detection_prob) * p$ProbOtherHosp * HAhosp_nchosp
          
          
          # RelativeCAvsHA_hospital <- CA_infectiousin_hosp / HA_infectiousin_hosp
          # RelativeHAvsCA_hospital <- HA_infectiousin_hosp / CA_infectiousin_hosp 
          
          
            days_infectious <- bind_cols("1. CA non-hospitalised COMM" = CAcomm_comm,
                                      "3. CA hospitalised COMM" = CAcomm_hosp,
                                      "3. CA hospitalised for covid HOSP" = CAhosp_hosp,
                                      "2. CA hospitalised for non-covid COMM" = CAcomm_nchosp,
                                      "2. CA hospitalised for non-covid HOSP" = CAhosp_nchosp,
                                      "4. HA not readmitted COMM" = HAcomm_comm,
                                      "4. HA not readmitted HOSP" = HAhosp_comm,
                                      "7. HA readmitted for severe covid (discharge) COMM" = HAcomm_hosp_severe_senttocommunity,
                                      "7. HA readmitted for severe covid (discharge) HOSP" = HAhosp_hosp_severe_senttocommunity,
                                      "8. HA readmitted for severe covid (no discharge) HOSP" = HAhosp_hosp_severe_remaininhosp,
                                      "6. HA readmitted for mild covid (no discharge) HOSP" = HAhosp_hosp_nonsevere_remaininhosp,
                                      "5. HA readmitted for non-covid COMM" = HAcomm_nchosp,
                                      "5. HA readmitted for non-covid HOSP" = HAhosp_nchosp) %>%
                      pivot_longer(everything(), names_to = "route", values_to = "days")
          
            frac_hosp_days <- bind_cols("CA hospitalised for covid" = CAhosp,
                                      "CA hospitalised for non-covid" = CAnchosp,
                                      "HA non-hospitalised" = HAcomm,
                                      "HA readmitted for covid (discharge)" = HAhosp_severe_senttocommunity,
                                      "HA readmitted for severe covid (no discharge)" = HAhosp_severe_remaininhosp,
                                      "HA readmitted for mild covid (no discharge)" = HAhosp_nonsevere_remaininhosp, 
                                      "HA readmitted for non-covid" = HAnchosp) %>%
                      pivot_longer(everything(), names_to = "route", values_to = "proportion")
        
            days_infectious_average_comm <- bind_cols("CA in community" = CA_infectiousin_comm,
                                              "HA in community | comm." = HA_infectiousin_comm) %>%
                      pivot_longer(everything(), names_to = "route", values_to = "days")
            days_infectious_average_hosp <- bind_cols("CA in hospital | hosp." = CA_infectiousin_hosp,
                                                      "HA in hospital" = HA_infectiousin_hosp) %>%
              pivot_longer(everything(), names_to = "route", values_to = "days")
            
            
          # given infection is acquired within comm or hosp, number days spent infectious at the hospital
          p1 <- ggplot(data = days_infectious, aes(x = route, y = days))  + 
                  geom_violin() + 
                  theme(axis.text.x = element_text(angle = 70, hjust=1)) +
                  scale_x_discrete(name ="") + 
                  ggtitle("Infectious days in setting")
          
          p2 <- ggplot(data = frac_hosp_days, aes(x = route, y = proportion))  +
            geom_violin() +
            theme(axis.text.x = element_text(angle = 70, hjust=1)) +
            scale_x_discrete(name ="") +
            ggtitle("Fraction infectious days in hospital")
          
          p3 <- ggplot(data = days_infectious_average_comm, aes(days, after_stat(density)))  + 
            geom_histogram(binwidth=1) + 
            facet_wrap(~route, ncol=1) +
            xlim(-0.5,15) + 
            # theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
            # scale_x_discrete(name ="") + 
            ggtitle("Average infectious days")
          
          p4 <- ggplot(data = days_infectious_average_hosp, aes(days, after_stat(density)))  + 
            geom_histogram() +
            xlim(-0.5,15) + 
            # ylim(0,1.5) + 
            facet_wrap(~route, ncol=1) +
            # theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
            # scale_x_discrete(name ="") + 
            ggtitle("Average infectious days")
          
          
          
          
         P <- grid.arrange(p1,p2,p3,p4, nrow = 2)
         todaysdate <- format(Sys.Date(), "%Y%m%d")
         
         ggsave(
           paste("plotInfectiousness_detect_",hosp_detection, "_", todaysdate, ".pdf", sep=""),
           plot = P,
           width = 11,
           height = 8.5,
           units = "in",
           dpi = 300)
         
        
}


hospitalinfections <- function(duration_type, hosp_speed){
  
  
  p <- readParameters(duration_type, hosp_speed)
  
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
  cat("Hosp. Acquired (no re-admittance): Proportion of days spent infectious in hosp = ", round(prop_days_inf_in_hosp,3), "\n\n") 
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
    
  } else if(hosp_speed=="slow"){
    time_until_readmission <- time_of_hosp_infection + p$latent_duration + p$preclinical_duration + p$hospital_delayfromonset
  }
  time_until_discharge <- pmin(time_until_readmission, p$duration_hospital_stay)
  time_until_discharge_second <- time_until_readmission + p$hospital_duration
  
  # find infections that are sent back to the community
  community_index <- time_until_readmission > time_until_discharge
  prob_detect_before_discharge <- 1 - sum(community_index)/p$num_samples
  
  if(duration_type=="indpt"){
    # if mild case then assume there is no readmission if case trasnferred to community
    scaling_factor_for_mild_cases <- !community_index
    # if(hosp_speed=="slow"){
      
     # community_index <- replicate(TRUE, p$num_samples)
     # probReadmission <- 0
     # if mild case and detection rate is slow, then calculate proportion readmitted readmission
    } else{
       scaling_factor_for_mild_cases <- 1
      # probReadmission <- sum((time_until_readmission < p$duration_hospital_stay))/p$num_samples
    }
  # }else{
  #    scaling_factor_for_mild_cases <- 1
  #    # probReadmission <- 1
  # }
  
  
  
    # calculate proportion days of hospital-acquired spend in hosp based on independent distribution
    time_to_not_infectious <- time_to_infectious + p$infectious_duration
    days_infectious_in_hosp_first <- pmax(0, time_until_discharge - time_to_infectious) - pmax(0, time_until_discharge - time_to_not_infectious) 
     days_infectious_in_hosp_second <- scaling_factor_for_mild_cases * (pmax(0, pmin(time_until_discharge_second, time_to_not_infectious) - pmax(time_until_readmission, time_to_infectious)))
      # days_infectious_in_hosp_second <-  pmax(0, pmin(time_until_discharge_second, time_to_not_infectious) - pmax(time_until_readmission, time_to_infectious))
    days_infectious_in_hosp <- days_infectious_in_hosp_first  + days_infectious_in_hosp_second
    days_infectious_in_hosp_positive <- days_infectious_in_hosp[days_infectious_in_hosp>0]
    
  
    # # if discharge happens before readmission
    # continue_stay <- time_until_readmission < p$duration_hospital_stay
    # cat(sum(continue_stay)/length(continue_stay))
    # # mild cases will not be readmitted if
     
    prop_days_inf_in_hosp <- sum(days_infectious_in_hosp) / sum(p$infectious_duration)
  
  # Output 
  
  cat(p$output_message, "\n") 
  cat("Hosp. acquired (readmitted): Proportion of days spent infectious in hosp = ", round(prop_days_inf_in_hosp,3), "\n\n")
  return(list("infectious_days_hosp" = days_infectious_in_hosp, "inf_total" = p$infectious_duration,
              "sent_back_to_community" = community_index, "prob_detect_before_discharge" = prob_detect_before_discharge))
  
}


communityinfections_goto_hospital <- function(duration_type,hosp_speed){
    
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
    cat("Comm. Acquired (hospitalised): Proportion of days spent infectious in hosp = ", round(prop_infectious_days_in_hospital,3), "\n\n") 
    return(list("infectious_days_hosp" = days_infectious_in_hosp, "inf_total" = p$infectious_duration))
} 

communityinfections_stayin_community <- function(duration_type, hosp_speed){
  
  p <- readParameters(duration_type)
  days_infectious_in_hosp <- replicate(p$num_samples, 0) 
  prob_infectious_in_hospital <- 0
  prop_infectious_days_in_hospital <- 0
  
  
  # Output 
  cat(p$output_message, "\n") 
  cat("Comm. Acquired (non-hospitalised): Proportion of days spent infectious in hosp = ", round(prop_infectious_days_in_hospital,3), "\n\n") 
  return(list("infectious_days_hosp" = days_infectious_in_hosp, "inf_total" = p$infectious_duration))
} 
