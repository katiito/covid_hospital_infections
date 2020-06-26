library(ggplot2)
library(dplyr)
library(tidyr)

library(gridExtra)

# run_hospital_analysis <- function(){
  
  comm2comm <- communityinfections_infectious_in_hospital(duration = "indpt")
  comm2hosp <- communityinfections_infectious_in_hospital(duration = "hosp") # OK
  hosp2hosp <- hospitalinfections_infectious_in_hospital(duration = "hosp") # OK
  hosp2comm <- hospitalinfections_infectious_in_hospital(duration = "indpt")
  # something up with this - lower than comm2hosp - should be same or higher
  hosp2hospReadmitted <- hospitalinfections_readmitted_infectious_in_hospital(duration = "hosp", hosp_speed = "normal")
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



hospitalinfections_infectious_in_hospital <- function(duration_type){
  
  
  p <- readParameters(duration_type)
  
  # infection at any point during los -relative to hospital admission
  
  time_of_hosp_infection <- runif(p$num_samples, min = replicate(p$num_samples,0), max = p$duration_hospital_stay)
  time_to_infectious <- time_of_hosp_infection + p$latent_duration
  
  # calculate proportion days of hospital-acquired spend in hosp based on independent distribution
  time_to_not_infectious <- time_to_infectious + p$infectious_duration
  
  days_infectious_in_hosp <- pmax(0, p$duration_hospital_stay - time_to_infectious) - pmax(0, p$duration_hospital_stay - time_to_not_infectious) 
  days_infectious_in_hosp_positive <- days_infectious_in_hosp[days_infectious_in_hosp>0]
  prop_days_inf_in_hosp <- sum(days_infectious_in_hosp) / sum(p$infectious_duration)
  
  # # calculate proportion days of hospital-acquired spend in hosp based on independent distribution
  # time_until_not_infectiousness_indpt <- time_until_infectiousness + infectious_duration
  # days_infectious_in_hosp_indpt <- pmax(0, duration_hospital_stay - time_until_infectiousness) - pmax(0, duration_hospital_stay - time_until_not_infectiousness_indpt) 
  # days_infectious_in_hosp_indpt_positive <- days_infectious_in_hosp_indpt[days_infectious_in_hosp_indpt>0]
  # prop_days_inf_in_hosp_indpt <- sum(days_infectious_in_hosp_indpt) / sum(infectious_duration)
  # 
  # # calculate proportion days of hospital-acquired spend in hosp based on  longer distribution
  # time_until_not_infectiousness_long <- time_until_infectiousness + infectious_duration_long
  # days_infectious_in_hosp_long <- pmax(0, duration_hospital_stay - time_until_infectiousness) - pmax(0, duration_hospital_stay - time_until_not_infectiousness_long) 
  # days_infectious_in_hosp_long_positive <- days_infectious_in_hosp_long[days_infectious_in_hosp_long>0]
  # prop_days_inf_in_hosp_long <- sum(days_infectious_in_hosp_long) / sum(infectious_duration_long)
  # 
  # # calculate proportion days of hospital-acquired spend in hosp based on van Kampen study (hospital patients)
  # time_until_not_infectiousness_hosp <- time_until_infectiousness + infectious_duration_hospital
  # days_infectious_in_hosp_hosp <- pmax(0, duration_hospital_stay - time_until_infectiousness) - pmax(0, duration_hospital_stay - time_until_not_infectiousness_hosp) 
  # days_infectious_in_hosp_hosp_positive <- days_infectious_in_hosp_hosp[days_infectious_in_hosp_hosp>0]
  # prop_days_inf_in_hosp_hosp <- sum(days_infectious_in_hosp_hosp) / sum(infectious_duration_hospital)
  # 
  
  # Output 
  cat(p$output_message, "\n") 
  cat("Proportion of days spent infectious in hosp = ", round(prop_days_inf_in_hosp,3), "\n\n") 

  
  # data_indpt <- bind_rows("hospital stay duration" = as_tibble(duration_hospital_stay), 
  #                         "admittance to infection delay" = as_tibble(time_of_hosp_infection),
  #                         "days infectious in hosp" = as_tibble(days_infectious_in_hosp_indpt),
  #                         "days | infectious in hosp" = as_tibble(days_infectious_in_hosp_indpt_positive), 
  #                         .id="variable")
  # 
  # 
  # data_long <- bind_rows("hospital stay duration" = as_tibble(duration_hospital_stay), 
  #                        "admittance to infection delay" = as_tibble(time_of_hosp_infection),
  #                        "days infectious in hosp" = as_tibble(days_infectious_in_hosp_long),
  #                        "days | infectious in hosp" = as_tibble(days_infectious_in_hosp_long_positive), 
  #                        .id="variable")
  # 
  # data_hosp <- bind_rows("hospital stay duration" = as_tibble(duration_hospital_stay), 
  #                        "admittance to infection delay" = as_tibble(time_of_hosp_infection),
  #                        "days infectious in hosp" = as_tibble(days_infectious_in_hosp_hosp),
  #                        "days | infectious in hosp" = as_tibble(days_infectious_in_hosp_hosp_positive), 
  #                        .id="variable")
  # 
  # data <- bind_rows("independent" = data_indpt, 
  #                   "upper 95%" = data_long,
  #                   "hospital study" = data_hosp, .id = "association")
  # names(data)[names(data) == "value"] <- "days"
  # data$variable <- factor(data$variable, levels = c("hospital stay duration",
  #                                                   "admittance to infection delay",
  #                                                   "days infectious in hosp",
  #                                                   "days | infectious in hosp"))
  # 
  # p <- ggplot(data, aes(x = days, after_stat(density))) +
  #   geom_histogram(binwidth = 0.5) +
  #   ggtitle("Hospital-acquired infections") +
  #   facet_grid(variable ~ association,
  #              scales = "free_y")
  # 
  # # print(p)
  # 
  return(list("infectious_days_hosp" = days_infectious_in_hosp, "inf_total" = p$infectious_duration))
#  id_ret <- data.frame("indpt" = days_infectious_in_hosp_indpt,
#                       "hosp" = days_infectious_in_hosp_hosp,
#                       "long" = days_infectious_in_hosp_long)
#  
#  pd_ret  <- data.frame("indpt" = prop_days_inf_in_hosp_indpt,
#                                "hosp" = prop_days_inf_in_hosp_hosp,
#                                "long" = prop_days_inf_in_hosp_long)
#  
# inf_duration <- data.frame("indpt" = infectious_duration,
#                            "hosp" = infectious_duration_hospital,
#                            "long" = infectious_duration_long)
# if(duration_type == "hosp"){
#   return(list("infectious_days_hosp" = id_ret$hosp, "prop_days" = pd_ret$hosp, "inf_total" = inf_duration$hosp))
# } else if(duration_type == "indpt"){
#   return(list("infectious_days_hosp" = id_ret$indpt, "prop_days" = pd_ret$indpt, "inf_total" = inf_duration$indpt))
# }
  
  
  
  
}


hospitalinfections_readmitted_infectious_in_hospital <- function(duration_type, hosp_speed){
  
  p <- readParameters(duration_type)
  # # estimates of hospital stay non covid
  # length_of_stay_mean = 4
  # length_of_stay_k = 10
  # 
  # # estimates taken from Davies et al. https://www.medrxiv.org/content/10.1101/2020.04.01.20049908v1
  # latent_duration_mean = 4
  # latent_duration_k = 4
  # 
  # # time spent infectious (pre-symptomatic)
  # preclinical_duration_mean = 1.5
  # preclinical_duration_k = 4
  # 
  # # time spent infectious (symptomatic)
  # clinical_duration_mean = 3.5
  # clinical_duration_k = 4
  # 
  # # delay between onset and hospitalisation due to covid
  # hospital_delayfromonset_mean = 7
  # hospital_delayfromonset_k = 7
  # 
  # # covid-specific length of stay
  # hospital_duration_mean = 7
  # hospital_duration_k = 7
  # 
  # # Fitting a Gamma Distribution to the van Kampen hospital study
  # # https://www.medrxiv.org/content/10.1101/2020.06.08.20125310v1.full.pdf
  # g_out <- get.gamma.par(p = c(0.025, 0.5, 0.975), q = c(5, 8, 11),
  #                        show.output = FALSE, plot = FALSE)
  # 
  # 
  # latent_duration <- rgamma(num_samples, scale = latent_duration_mean/latent_duration_k, 
  #                           shape = latent_duration_k)
  # preclinical_duration <- rgamma(num_samples, scale = preclinical_duration_mean/preclinical_duration_k, 
  #                                shape = preclinical_duration_k)
  # clinical_duration <- rgamma(num_samples, scale = clinical_duration_mean/clinical_duration_k, 
  #                             shape = clinical_duration_k)
  # hospital_delayfromonset <- rgamma(num_samples, scale = hospital_delayfromonset_mean/hospital_delayfromonset_k, 
  #                                   shape = hospital_delayfromonset_k)
  # hospital_duration <- rgamma(num_samples, scale = hospital_duration_mean/hospital_duration_k, 
  #                             shape = hospital_duration_k)
  # # infectious_duration <- preclinical_duration + clinical_duration
  # # infectious_duration_hospital <- rgamma(num_samples, scale = 1/g_out["rate"], shape =  g_out["shape"])
  # # 
  # # infectious_duration_long_truncate <- sort(infectious_duration)[(0.95*num_samples):num_samples]
  # # infectious_duration_long <- sample(infectious_duration_long_truncate, num_samples, replace = TRUE)
  # 
  # if(duration_type=="indpt"){
  #   infectious_duration <- preclinical_duration + clinical_duration
  #   output_message <- "Assuming infectious duration is independent of hospitalisation risk"
  # }else if(duration_type=="long"){
  #   infectious_duration_long_truncate <- sort(infectious_duration)[(0.95*num_samples):num_samples]
  #   infectious_duration <- sample(infectious_duration_long_truncate, num_samples, replace = TRUE)
  #   output_message <- "Assuming hospitalised cases are in upper 95% infectious duration"
  # }else if(duration_type=="hosp"){
  #   infectious_duration <- rgamma(num_samples, scale = 1/g_out["rate"], shape =  g_out["shape"])
  #   output_message <- "Assuming hospitalised cases are as estimated in van Kampman"
  # }
  # 
  ## Assuming that infected at hospital severely enough 
  ## to cause increase in length of stay
  
  # # infection at any point during los -relative to first hospital admission
  # duration_hospital_stay <- rgamma(num_samples, scale = length_of_stay_mean/length_of_stay_k, 
  #                                  shape = length_of_stay_k)
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
  # days_infectious_in_hosp_first <- pmax(time_until_discharge - time_to_infectious) - pmax(0, time_until_discharge - time_to_not_infectious) 
  days_infectious_in_hosp_second <- pmax(0, pmin(time_until_discharge_second, time_to_not_infectious) - pmax(time_until_readmission, time_to_infectious) )
  days_infectious_in_hosp <- days_infectious_in_hosp_first + days_infectious_in_hosp_second
  days_infectious_in_hosp_positive <- days_infectious_in_hosp[days_infectious_in_hosp>0]
  prop_days_inf_in_hosp <- sum(days_infectious_in_hosp) / sum(p$infectious_duration)
  
  
  
  # calculate proportion days of hospital-acquired spend in hosp based on  longer distribution
  # time_until_not_infectiousness_long <- time_until_infectiousness + infectious_duration_long
  # days_infectious_in_hosp_first_long <- pmax(0, time_until_discharge - time_until_infectiousness) - pmax(0, time_until_discharge - time_until_not_infectiousness_long) 
  # days_infectious_in_hosp_second_long <- pmax(0, pmin(time_until_discharge_second,time_until_not_infectiousness_long) - pmax(time_until_readmission,time_until_infectiousness) )
  # days_infectious_in_hosp_long <- days_infectious_in_hosp_first_long + days_infectious_in_hosp_second_long
  # days_infectious_in_hosp_long_positive <- days_infectious_in_hosp_long[days_infectious_in_hosp_long>0]
  # prop_days_inf_in_hosp_long <- sum(days_infectious_in_hosp_long) / sum(infectious_duration_long)
  # 
  # 
  # # calculate proportion days of hospital-acquired spend in hosp based on van Kampen study (hospital patients)
  # time_until_not_infectiousness_hosp <- time_until_infectiousness + infectious_duration_hospital
  # days_infectious_in_hosp_first_hosp <- pmax(0, time_until_discharge - time_until_infectiousness) - pmax(0, time_until_discharge - time_until_not_infectiousness_hosp) 
  # days_infectious_in_hosp_second_hosp <- pmax(0, pmin(time_until_discharge_second,time_until_not_infectiousness_hosp) - pmax(time_until_readmission,time_until_infectiousness) )
  # days_infectious_in_hosp_hosp <- days_infectious_in_hosp_first_hosp + days_infectious_in_hosp_second_hosp
  # days_infectious_in_hosp_hosp_positive <- days_infectious_in_hosp_hosp[days_infectious_in_hosp_hosp>0]
  # prop_days_inf_in_hosp_hosp <- sum(days_infectious_in_hosp_hosp) / sum(infectious_duration_hospital)
  # 
  
  # Output 
  
  cat(p$output_message, "\n") 
  cat("Proportion of days spent infectious in hosp = ", round(prop_days_inf_in_hosp,3), "\n\n") 
  
  # cat("Assuming infectious duration is independent of hospitalisation risk", "\n") 
  # cat("Proportion of days spent infectious in hosp = ", round(prop_days_inf_in_hosp_indpt,3), "\n\n") 
  # 
  # cat("Assuming hospitalised cases are in upper 95% infectious duration", "\n") 
  # cat("Proportion of days spent infectious in hosp = ", round(prop_days_inf_in_hosp_long,3), "\n\n") 
  # 
  # cat("Assuming hospitalised cases are as estimated in van Kampman", "\n") 
  # cat("Proportion of days spent infectious in hosp = ", round(prop_days_inf_in_hosp_hosp,3), "\n") 
  # 
  # 
  # data_indpt <- bind_rows("hospital stay duration" = as_tibble(duration_hospital_stay), 
  #                         "admittance to infection delay" = as_tibble(time_of_hosp_infection),
  #                         "days infectious in hosp" = as_tibble(days_infectious_in_hosp_indpt),
  #                         "days | infectious in hosp" = as_tibble(days_infectious_in_hosp_indpt_positive), 
  #                         .id="variable")
  # 
  # 
  # data_long <- bind_rows("hospital stay duration" = as_tibble(duration_hospital_stay), 
  #                        "admittance to infection delay" = as_tibble(time_of_hosp_infection),
  #                        "days infectious in hosp" = as_tibble(days_infectious_in_hosp_long),
  #                        "days | infectious in hosp" = as_tibble(days_infectious_in_hosp_long_positive), 
  #                        .id="variable")
  # 
  # data_hosp <- bind_rows("hospital stay duration" = as_tibble(duration_hospital_stay), 
  #                        "admittance to infection delay" = as_tibble(time_of_hosp_infection),
  #                        "days infectious in hosp" = as_tibble(days_infectious_in_hosp_first_hosp),
  #                        "days | infectious in hosp" = as_tibble(days_infectious_in_hosp_second_hosp), 
  #                        .id="variable")
  # 
  # data <- bind_rows("independent" = data_indpt, 
  #                   "upper 95%" = data_long,
  #                   "hospital study" = data_hosp, .id = "association")
  # names(data)[names(data) == "value"] <- "days"
  # data$variable <- factor(data$variable, levels = c("hospital stay duration",
  #                                                   "admittance to infection delay",
  #                                                   "days infectious in hosp",
  #                                                   "days | infectious in hosp"))
  # 
  # p <- ggplot(data, aes(x = days, after_stat(density))) +
  #   geom_histogram(binwidth = 0.5) +
  #   ggtitle("Hospital-acquired infections") +
  #   facet_grid(variable ~ association,
  #              scales = "free_y")
  # 
  # print(p)
  # 
  # id_ret <- data.frame("indpt" = days_infectious_in_hosp_indpt,
  #                      "hosp" = days_infectious_in_hosp_hosp,
  #                      "long" = days_infectious_in_hosp_long)
  # 
  # pd_ret  <- data.frame("indpt" = prop_days_inf_in_hosp_indpt,
  #                       "hosp" = prop_days_inf_in_hosp_hosp,
  #                       "long" = prop_days_inf_in_hosp_long)
  # 
  # inf_duration <- data.frame("indpt" = infectious_duration,
  #                            "hosp" = infectious_duration_hospital,
  #                            "long" = infectious_duration_long)
  # if(duration_type == "hosp"){
  #   return(list("infectious_days_hosp" = id_ret$hosp, "prop_days" = pd_ret$hosp, "inf_total" = inf_duration$hosp))
  # } else if(duration_type == "indpt"){
  #   return(list("infectious_days_hosp" = id_ret$indpt, "prop_days" = pd_ret$indpt, "inf_total" = inf_duration$indpt))
  # }
  # 
  
  return(list("infectious_days_hosp" = days_infectious_in_hosp, "inf_total" = p$infectious_duration))
  
}


communityinfections_infectious_in_hospital <- function(duration_type){
    
    p <- readParameters(duration_type)
    # # Number of days that a hospitalised person is infectious in the hospital
    # = MIN[ hospitalised duration, 
    #        MIN[0, (time to loss of infectiousness) - (time to hospital admission)]]
    
    
    
    # # estimates taken from Davies et al. https://www.medrxiv.org/content/10.1101/2020.04.01.20049908v1
    #  latent_duration_mean = 4
    #  latent_duration_k = 4
    # #
    # # # time spent infectious (pre-symptomatic)
    #  preclinical_duration_mean = 1.5
    #  preclinical_duration_k = 4
    # #
    # # # time spent infectious (symptomatic)
    #  clinical_duration_mean = 3.5
    #  clinical_duration_k = 4
    # 
    # # delay between onset and hospitalisation due to covid
    #  hospital_delayfromonset_mean = 7
    #  hospital_delayfromonset_k = 7
    # #
    # # # covid-specific length of stay
    #  hospital_duration_mean = 7
    #  hospital_duration_k = 7
    # #
    # # # Fitting a Gamma Distribution to the van Kampen hospital study
    # # # https://www.medrxiv.org/content/10.1101/2020.06.08.20125310v1.full.pdf
    #  g_out <- get.gamma.par(p = c(0.025, 0.5, 0.975), q = c(5, 8, 11),
    #                        show.output = FALSE, plot = FALSE)
    # #
    # #
    #  latent_duration <- rgamma(num_samples, scale = latent_duration_mean/latent_duration_k,
    #                             shape = latent_duration_k)
    #  preclinical_duration <- rgamma(num_samples, scale = preclinical_duration_mean/preclinical_duration_k,
    #                           shape = preclinical_duration_k)
    #  clinical_duration <- rgamma(num_samples, scale = clinical_duration_mean/clinical_duration_k,
    #                                shape = clinical_duration_k)
    #  hospital_delayfromonset <- rgamma(num_samples, scale = hospital_delayfromonset_mean/hospital_delayfromonset_k,
    #                             shape = hospital_delayfromonset_k)
    #  hospital_duration <- rgamma(num_samples, scale = hospital_duration_mean/hospital_duration_k,
    #                             shape = hospital_duration_k)
    #   # infectious_duration <- preclinical_duration + clinical_duration
    # # infectious_duration_hospital <- rgamma(num_samples, scale = 1/g_out["rate"], shape =  g_out["shape"])
    # #
    # # infectious_duration_long_truncate <- sort(infectious_duration)[(0.95*num_samples):num_samples]
    # # infectious_duration_long <- sample(infectious_duration_long_truncate, num_samples, replace = TRUE)
    # #
    # if(duration_type=="indpt"){
    #   infectious_duration <- preclinical_duration + clinical_duration
    #   output_message <- "Assuming infectious duration is independent of hospitalisation risk"
    # }else if(duration_type=="long"){
    #   infectious_duration_long_truncate <- sort(infectious_duration)[(0.95*num_samples):num_samples]
    #   infectious_duration <- sample(infectious_duration_long_truncate, num_samples, replace = TRUE)
    #   output_message <- "Assuming hospitalised cases are in upper 95% infectious duration"
    # }else if(duration_type=="hosp"){
    #   infectious_duration <- rgamma(num_samples, scale = 1/g_out["rate"], shape =  g_out["shape"])
    #   output_message <- "Assuming hospitalised cases are as estimated in van Kampman"
    # }


     time_to_hospital_admission <- p$latent_duration + p$preclinical_duration + p$hospital_delayfromonset
    
    # time_to_hospital_admission <- latent_duration + preclinical_duration + hospital_delayfromonset
    
    time_to_not_infectious <- p$latent_duration + p$infectious_duration
    # time_to_not_infectious <- p$latent_duration + p$infectious_duration
    # time_to_lossofinfectiousness_long <- latent_duration + infectious_duration_long
    # time_to_lossofinfectiousness_hospital <- latent_duration + infectious_duration_hospital
    
    ## 1a. calculate the time infectious after admitted to hosptial
    time_infectious_after_admission_unltd <- time_to_not_infectious - time_to_hospital_admission
    time_infectious_after_admission <- pmax(0,time_infectious_after_admission_unltd)
    
    # time_infectious_after_admission_unltd <- time_to_not_infectious - time_to_hospital_admission
    # time_infectious_after_admission <- pmax(0,time_infectious_after_admission_unltd)
    
    

      # 1b. calculate this time assuming a long infectious period for hosp cases
      # time_infectiousness_after_admission_unltd_long <- time_to_lossofinfectiousness_long - time_to_hospital_admission
      # time_infectiousness_after_admission_long <- pmax(0,time_infectiousness_after_admission_unltd_long)
    
      # 1c. calculate this time assuming an infectious period as estimated for hosp cases
      # time_infectiousness_after_admission_unltd_hosp <- time_to_lossofinfectiousness_hospital - time_to_hospital_admission
      # time_infectiousness_after_admission_hosp <- pmax(0,time_infectiousness_after_admission_unltd_hosp)
      
    
    ## 2a. calcualte number of days infectious within the hospital 
     days_infectious_in_hosp <- pmin(p$hospital_duration, time_infectious_after_admission)
    
    # days_infectious_in_hosp <- pmin(hospital_duration, time_infectious_after_admission)
    days_infectious_in_hosp_positive <- days_infectious_in_hosp[days_infectious_in_hosp>0]
    
      # 2b. now under the assumption of a long duration of infectiousness
      # number_days_infectious_in_hospital_long <- pmin(hospital_duration, time_infectiousness_after_admission_long)
      # number_days_infectious_in_hospital_positive_long <- number_days_infectious_in_hospital_long[number_days_infectious_in_hospital_long>0]
    
      # 2c. now under the assumption of a long duration of infectiousness
      # number_days_infectious_in_hospital_hosp <- pmin(hospital_duration, time_infectiousness_after_admission_hosp)
      # number_days_infectious_in_hospital_positive_hosp <- number_days_infectious_in_hospital_hosp[number_days_infectious_in_hospital_hosp>0]
      
    ## 3a. calcualte statistics
     prob_infectious_in_hospital <- sum(days_infectious_in_hosp>0) / p$num_samples
    
    # prob_infectious_in_hospital <- sum(days_infectious_in_hosp>0) / num_samples
     prop_infectious_days_in_hospital <- sum(days_infectious_in_hosp) / sum(p$infectious_duration)
    # prop_infectious_days_in_hospital <- sum(days_infectious_in_hosp) / sum(infectious_duration)
    
      # 3b. and for long duration infectious
      # prob_infectious_in_hospital_long <- sum(number_days_infectious_in_hospital_long>0) / num_samples
      # prop_infectious_days_in_hospitals_long <- sum(number_days_infectious_in_hospital_long) / sum(infectious_duration_long)
    
      # 3c. and for hosp duration infectious
      # prob_infectious_in_hospital_hosp <- sum(number_days_infectious_in_hospital_hosp>0) / num_samples
      # prop_infectious_days_in_hospitals_hosp <- sum(number_days_infectious_in_hospital_hosp) / sum(infectious_duration_hospital)
      
    # data_indpt <- bind_rows("not infectious delay" = as_tibble(time_to_lossofinfectiousness), 
    #                   "admission delay" = as_tibble(time_to_hospital_admission),
    #                   "days infectious in hosp" = as_tibble(number_days_infectious_in_hospital),
    #                   "days | infectious in hosp" = as_tibble(number_days_infectious_in_hospital_positive), 
    #                   .id="variable")
    # 
    # 
    # data_long <- bind_rows("not infectious delay" = as_tibble(time_to_lossofinfectiousness_long), 
    #                   "admission delay" = as_tibble(time_to_hospital_admission),
    #                   "days infectious in hosp" = as_tibble(number_days_infectious_in_hospital_long),
    #                   "days | infectious in hosp" = as_tibble(number_days_infectious_in_hospital_positive_long), 
    #                   .id="variable")
    # 
    # data_hosp <- bind_rows("not infectious delay" = as_tibble(time_to_lossofinfectiousness_hospital), 
    #                        "admission delay" = as_tibble(time_to_hospital_admission),
    #                        "days infectious in hosp" = as_tibble(number_days_infectious_in_hospital_hosp),
    #                        "days | infectious in hosp" = as_tibble(number_days_infectious_in_hospital_positive_hosp), 
    #                        .id="variable")
    # 
    # data <- bind_rows("independent" = data_indpt, 
    #                   "upper 95%" = data_long,
    #                   "hospital study" = data_hosp, .id = "association")
    # names(data)[names(data) == "value"] <- "days"
    # data$variable <- factor(data$variable, levels = c("not infectious delay",
    #                                         "admission delay",
    #                                         "days infectious in hosp",
    #                                         "days | infectious in hosp"))
    # 
    # 
    # p <- ggplot(data, aes(x = days, after_stat(density))) +
    #       geom_histogram(binwidth = 0.5) +
    #       ggtitle("Community-acquired infections") +
    #       facet_grid(variable ~ association,
    #                  scales = "free_y")
    # 
   # print(p)
    
    # Output 
    cat(p$output_message, "\n") 
    cat("Prob of hospital case being infectious = ", round(prob_infectious_in_hospital,3), "\n") 
    cat("Proportion of days spent infectious in hosp = ", round(prop_infectious_days_in_hospital,3), "\n\n") 
    
    # cat("Assuming infectious duration is independent of hospitalisation risk", "\n") 
    # cat("Prob of hospital case being infectious = ", round(prob_infectious_in_hospital,3), "\n") 
    # cat("Proportion of days spent infectious in hosp = ", round(prop_infectious_days_in_hospitals,3), "\n\n") 
    # 
    # cat("Assuming hospitalised cases are in upper 95% infectious duration", "\n") 
    # cat("Prob of hospital case being infectious = ", round(prob_infectious_in_hospital_long,3), "\n") 
    # cat("Proportion of days spent infectious in hosp = ", round(prop_infectious_days_in_hospitals_long,3), "\n\n") 
    # 
    # cat("Assuming hospitalised cases are as estimated in van Kampman", "\n") 
    # cat("Prob of hospital case being infectious = ", round(prob_infectious_in_hospital_hosp,3), "\n") 
    # cat("Proportion of days spent infectious in hosp = ", round(prop_infectious_days_in_hospitals_hosp,3), "\n") 
    # 
    
    # id_ret <- data.frame("indpt" = number_days_infectious_in_hospital,
    #                          "hosp" = number_days_infectious_in_hospital_hosp,
    #                          "long" = number_days_infectious_in_hospital_long)
    # 
    # pd_ret <- data.frame("indpt" = prop_infectious_days_in_hospitals,
    #                               "hosp" = prop_infectious_days_in_hospitals_hosp,
    #                               "long" = prop_infectious_days_in_hospitals_long)
    # 
    # inf_duration <- data.frame("indpt" = infectious_duration,
    #                            "hosp" = infectious_duration_hospital,
    #                            "long" = infectious_duration_long)
    # 
    # if(duration_type == "hosp"){
    #   return(list("infectious_days_hosp" = id_ret$hosp, "prop_days" = pd_ret$hosp, "inf_total" = inf_duration$hosp))
    # } else if(duration_type == "indpt"){
    #   return(list("infectious_days_hosp" = id_ret$indpt, "prop_days" = pd_ret$indpt, "inf_total" = inf_duration$indpt))
    # }
    return(list("infectious_days_hosp" = days_infectious_in_hosp, "inf_total" = p$infectious_duration))
} 
