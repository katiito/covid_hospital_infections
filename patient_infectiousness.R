library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)


patientInfectiousness <- function(){
          
          # num_samples <- 10000
          p <- readParameters()
        # run_hospital_analysis <- function(){
          
          # CAs only infecting the community
          comm2comm <- communityinfections_stayin_community(duration = "indpt")
          # CAs infecting the hospital and community
          comm2hosp <- communityinfections_goto_hospital(duration = "hosp")
          # HAs infecting the hospital and community
          hosp2comm <- hospitalinfections(duration = "indpt") # OK
          # HAs infecting the hospital, community and hospital on readmission for covid
          hosp2hospReadmitted <- hospitalinfections_readmitted(duration = "hosp", hosp_speed = "normal")
          # hosp2hospReadmitted_notcovid <- ######
          
          # 1. number days a CA spends in community assuming stays in comm  NONHOSP DURATION
          CAcomm_comm <- comm2comm$inf_total 
          
          # 2. number of days a CA spends in community or hosp assuming goes to hosp for covid
          CAcomm_hosp <- comm2hosp$inf_total - comm2hosp$infectious_days_hosp
          CAhosp_hosp <- comm2hosp$infectious_days_hosp
          CAhosp <- CAhosp_hosp / (CAcomm_hosp + CAhosp_hosp)
          
          
          # 3. number of days a CA spends in community or hosp assuming goes to hosp for non-covid NONHOSP DURATION
        #### ISSUE
          
          CAcomm_nchosp <- runif(num_samples, min = replicate(num_samples,0), max = comm2comm$inf_total) ### could update this with the comm2comm 
          CAhosp_nchosp <-  comm2comm$inf_total - CAcomm_nchosp ### could update this with the comm2comm 
          CAnchosp <- CAhosp_nchosp / (CAhosp_nchosp + CAcomm_nchosp)
          
          # 4. number days a HA spends in community or hospital given discharge to community NONHOSP DURATION
          HAcomm_comm <- hosp2comm$inf_total - hosp2comm$infectious_days_hosp #### could update this with the comm2comm
          HAhosp_comm <- hosp2comm$infectious_days_hosp 
          HAcomm <- HAhosp_comm / (HAhosp_comm + HAcomm_comm)
          
          
          # 5. number days a HA spends in community or hospital given readmittance to hosp for covid
          HAhosp_hosp <- hosp2hospReadmitted$infectious_days_hosp
          HAcomm_hosp <- hosp2hospReadmitted$inf_total - HAhosp_hosp
          HAhosp <- HAhosp_hosp / (HAhosp_hosp + HAcomm_hosp)
          
          
          # 6. number days a HA spends in community or hospital given readmittance to hosp for noncovid NONHOSP DURATION
          HAcomm_nchosp <- runif(num_samples, min = replicate(num_samples,0), max = round(HAcomm_comm, digits = 5))
          HAhosp_nchosp <- hosp2comm$inf_total - HAcomm_nchosp
          HAnchosp <- HAhosp_nchosp / (HAhosp_nchosp + HAcomm_nchosp)
          
          ## Relative Transmissions
          # how infectious is a CA in hospital relative to in the community?
          
          # how infectious is a CA in community relative to a HA in community? weighting by prob of being there
          
          
          probCovidHosp <- p$ProbCovidHosp/(1 + p$ProbOtherHosp)
          probNonCovidHosp <- p$ProbOtherHosp/(1 + p$ProbOtherHosp)
          
          CA_infectiousin_comm <- probCovidHosp*CAcomm_hosp + 
                                    (1-probCovidHosp-probNonCovidHosp)*CAcomm_comm +
                                    probNonCovidHosp*CAcomm_nchosp
          HA_infectiousin_comm <- probCovidHosp*HAcomm_hosp + 
                                    (1-probCovidHosp-probNonCovidHosp)*HAcomm_comm + 
                                    probNonCovidHosp*HAcomm_nchosp
          
          # RelativeCAvsHA_community <- CA_infectiousin_comm / HA_infectiousin_comm
          # RelativeHAvsCA_community <- HA_infectiousin_comm / CA_infectiousin_comm
                                    
          # how infectious is a CA in community relative to a HA in community? weighting by prob of being there
            
          probCovidHospCA <- p$ProbCovidHosp/(p$ProbOtherHosp + p$ProbCovidHosp)
          
          CA_infectiousin_hosp <- probCovidHospCA*CAhosp_hosp + (1-probCovidHospCA)*CAhosp_nchosp
          HA_infectiousin_hosp <- probCovidHosp*HAhosp_hosp + 
                                         (1-probCovidHosp-probNonCovidHosp)*HAhosp_comm + 
                                        probNonCovidHosp*HAhosp_nchosp
          
          
          # RelativeCAvsHA_hospital <- CA_infectiousin_hosp / HA_infectiousin_hosp
          # RelativeHAvsCA_hospital <- HA_infectiousin_hosp / CA_infectiousin_hosp 
          
          
            days_infectious <- bind_cols("CA non-hospitalised COMM" = CAcomm_comm,
                                      "CA hospitalised COMM" = CAcomm_hosp,
                                      "CA hospitalised for covid HOSP" = CAhosp_hosp,
                                      "CA hospitalised for non-covid COMM" = CAcomm_nchosp,
                                      "CA hospitalised for non-covid HOSP" = CAhosp_nchosp,
                                      "HA non-hospitalised COMM" = HAcomm_comm,
                                      "HA non-hospitalised HOSP" = HAhosp_comm,
                                      "HA rehospitalised for covid COMM" = HAcomm_hosp,
                                      "HA rehospitalised for covid HOSP" = HAhosp_hosp,
                                      "HA rehospitalised for non-covid COMM" = HAcomm_nchosp,
                                      "HA rehospitalised for non-covid HOSP" = HAhosp_nchosp) %>%
                      pivot_longer(everything(), names_to = "route", values_to = "days")
          
            frac_hosp_days <- bind_cols("CA hospitalised for covid" = CAhosp,
                                      "CA hospitalised for non-covid" = CAnchosp,
                                      "HA non-hospitalised" = HAcomm,
                                      "HA rehospitalised for covid" = HAhosp,
                                      "HA rehospitalised for non-covid" = HAnchosp) %>%
                      pivot_longer(everything(), names_to = "route", values_to = "proportion")
        
            days_infectious_average_comm <- bind_cols("CA in community" = CA_infectiousin_comm,
                                              "HA in community" = HA_infectiousin_comm) %>%
                      pivot_longer(everything(), names_to = "route", values_to = "days")
            days_infectious_average_hosp <- bind_cols("CA in hospital" = CA_infectiousin_hosp,
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
            # theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
            # scale_x_discrete(name ="") + 
            ggtitle("Average infectious days")
          
          p4 <- ggplot(data = days_infectious_average_hosp, aes(days, after_stat(density)))  + 
            geom_histogram() +
            # xlim(0,2) + 
            # ylim(0,1.5) + 
            facet_wrap(~route, ncol=1) +
            # theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
            # scale_x_discrete(name ="") + 
            ggtitle("Average infectious days")
          
          
          
          
         P <- grid.arrange(p1,p2,p3,p4, nrow = 2)
         todaysdate <- format(Sys.Date(), "%Y%m%d")
         
         ggsave(
           paste("plotInfectiousness_", todaysdate, ".pdf", sep=""),
           plot = P,
           width = 11,
           height = 8.5,
           units = "in",
           dpi = 300)
         
        
}


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
  cat("Hosp. acquired (readmitted): Proportion of days spent infectious in hosp = ", round(prop_days_inf_in_hosp,3), "\n\n")
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
    cat("Comm. Acquired (hospitalised): Proportion of days spent infectious in hosp = ", round(prop_infectious_days_in_hospital,3), "\n\n") 
    return(list("infectious_days_hosp" = days_infectious_in_hosp, "inf_total" = p$infectious_duration))
} 

communityinfections_stayin_community <- function(duration_type){
  
  p <- readParameters(duration_type)
  days_infectious_in_hosp <- replicate(p$num_samples, 0) 
  prob_infectious_in_hospital <- 0
  prop_infectious_days_in_hospital <- 0
  
  
  # Output 
  cat(p$output_message, "\n") 
  cat("Comm. Acquired (non-hospitalised): Proportion of days spent infectious in hosp = ", round(prop_infectious_days_in_hospital,3), "\n\n") 
  return(list("infectious_days_hosp" = days_infectious_in_hosp, "inf_total" = p$infectious_duration))
} 
