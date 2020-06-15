



hospital_infectious <- function(){
    library(ggplot2)
    library(dplyr)
    library(rriskDistributions)
    # # Number of days that a hospitalised person is infectious in the hospital
    # = MIN[ hospitalised duration, 
    #        MIN[0, (time to loss of infectiousness) - (time to hospital admission)]]
    
    num_samples = 100000
    
    # estimates taken from Davies et al. https://www.medrxiv.org/content/10.1101/2020.04.01.20049908v1
    latent_duration_mean = 4
    latent_duration_k=4
    
    preclinical_duration_mean = 1.5
    preclinical_duration_k = 4
    
    clinical_duration_mean = 3.5
    clinical_duration_k = 4
    
    hospital_delayfromonset_mean = 7
    hospital_delayfromonset_k = 7
    
    hospital_duration_mean = 7
    hospital_duration_k = 7
    
    # Fitting a Gamma Distribution to the van Kampen hospital study
    # https://www.medrxiv.org/content/10.1101/2020.06.08.20125310v1.full.pdf
    g_out <- get.gamma.par(p = c(0.025, 0.5, 0.975), q = c(5, 8, 11),
                           show.output = FALSE, plot = FALSE)

    
    latent_duration <- rgamma(num_samples, scale = latent_duration_mean/latent_duration_k, 
                               shape = latent_duration_k)
    preclinical_duration <- rgamma(num_samples, scale = preclinical_duration_mean/preclinical_duration_k, 
                             shape = preclinical_duration_k)
    clinical_duration <- rgamma(num_samples, scale = clinical_duration_mean/clinical_duration_k, 
                                  shape = clinical_duration_k)
    hospital_delayfromonset <- rgamma(num_samples, scale = hospital_delayfromonset_mean/hospital_delayfromonset_k, 
                               shape = hospital_delayfromonset_k)
    hospital_duration <- rgamma(num_samples, scale = hospital_duration_mean/hospital_duration_k, 
                               shape = hospital_duration_k)
    infectious_duration <- preclinical_duration + clinical_duration
    infectious_duration_hospital <- rgamma(num_samples, scale = 1/g_out["rate"], shape =  g_out["shape"])
    
    infectious_duration_long_truncate <- sort(infectious_duration)[(0.95*num_samples):num_samples]
    infectious_duration_long <- sample(infectious_duration_long_truncate, num_samples, replace = TRUE)
    
    
    time_to_hospital_admission <- latent_duration + preclinical_duration + hospital_delayfromonset
    time_to_lossofinfectiousness <- latent_duration + infectious_duration
    time_to_lossofinfectiousness_long <- latent_duration + infectious_duration_long
    time_to_lossofinfectiousness_hospital <- latent_duration + infectious_duration_hospital
    
    ## 1a. calculate the time infectious after admitted to hosptial
    time_infectiousness_after_admission_unltd <- time_to_lossofinfectiousness - time_to_hospital_admission
    time_infectiousness_after_admission <- pmax(0,time_infectiousness_after_admission_unltd)
    
      # 1b. calculate this time assuming a long infectious period for hosp cases
      time_infectiousness_after_admission_unltd_long <- time_to_lossofinfectiousness_long - time_to_hospital_admission
      time_infectiousness_after_admission_long <- pmax(0,time_infectiousness_after_admission_unltd_long)
    
      # 1c. calculate this time assuming an infectious period as estimated for hosp cases
      time_infectiousness_after_admission_unltd_hosp <- time_to_lossofinfectiousness_hospital - time_to_hospital_admission
      time_infectiousness_after_admission_hosp <- pmax(0,time_infectiousness_after_admission_unltd_hosp)
      
    
    ## 2a. calcualte number of days infectious within the hospital 
    number_days_infectious_in_hospital <- pmin(hospital_duration, time_infectiousness_after_admission)
    number_days_infectious_in_hospital_positive <- number_days_infectious_in_hospital[number_days_infectious_in_hospital>0.5]
    
      # 2b. now under the assumption of a long duration of infectiousness
      number_days_infectious_in_hospital_long <- pmin(hospital_duration, time_infectiousness_after_admission_long)
      number_days_infectious_in_hospital_positive_long <- number_days_infectious_in_hospital_long[number_days_infectious_in_hospital_long>0.5]
    
      # 2c. now under the assumption of a long duration of infectiousness
      number_days_infectious_in_hospital_hosp <- pmin(hospital_duration, time_infectiousness_after_admission_hosp)
      number_days_infectious_in_hospital_positive_hosp <- number_days_infectious_in_hospital_hosp[number_days_infectious_in_hospital_hosp>0.5]
      
    ## 3a. calcualte statistics
    prob_infectious_in_hospital <- sum(number_days_infectious_in_hospital>0.5) / num_samples
    prop_infectious_days_in_hospitals <- sum(number_days_infectious_in_hospital) / sum(infectious_duration)
    
      # 3b. and for long duration infectious
      prob_infectious_in_hospital_long <- sum(number_days_infectious_in_hospital_long>0.5) / num_samples
      prop_infectious_days_in_hospitals_long <- sum(number_days_infectious_in_hospital_long) / sum(infectious_duration_long)
    
      # 3c. and for hosp duration infectious
      prob_infectious_in_hospital_hosp <- sum(number_days_infectious_in_hospital_hosp>0.5) / num_samples
      prop_infectious_days_in_hospitals_hosp <- sum(number_days_infectious_in_hospital_hosp) / sum(infectious_duration_hospital)
      
    data_indpt <- bind_rows("not infectious delay" = as_tibble(time_to_lossofinfectiousness), 
                      "admission delay" = as_tibble(time_to_hospital_admission),
                      "days infectious in hosp" = as_tibble(number_days_infectious_in_hospital),
                      "days | infectious in hosp" = as_tibble(number_days_infectious_in_hospital_positive), 
                      .id="variable")
    
    
    data_long <- bind_rows("not infectious delay" = as_tibble(time_to_lossofinfectiousness_long), 
                      "admission delay" = as_tibble(time_to_hospital_admission),
                      "days infectious in hosp" = as_tibble(number_days_infectious_in_hospital_long),
                      "days | infectious in hosp" = as_tibble(number_days_infectious_in_hospital_positive_long), 
                      .id="variable")
    
    data_hosp <- bind_rows("not infectious delay" = as_tibble(time_to_lossofinfectiousness_hospital), 
                           "admission delay" = as_tibble(time_to_hospital_admission),
                           "days infectious in hosp" = as_tibble(number_days_infectious_in_hospital_hosp),
                           "days | infectious in hosp" = as_tibble(number_days_infectious_in_hospital_positive_hosp), 
                           .id="variable")
    
    data <- bind_rows("independent" = data_indpt, 
                      "upper 95%" = data_long,
                      "hospital study" = data_hosp, .id = "association")
    names(data)[names(data) == "value"] <- "days"
    data$variable <- factor(data$variable, levels = c("not infectious delay",
                                            "admission delay",
                                            "days infectious in hosp",
                                            "days | infectious in hosp"))
   
    
    p <- ggplot(data, aes(x = days, after_stat(density))) +
          geom_histogram(binwidth = 0.5) +
          facet_grid(variable ~ association,
                     scales = "free_y")
    
    print(p)
    
    # Output 
    cat("Assuming infectious duration is independent of hospitalisation risk", "\n") 
    cat("Prob of hospital case being infectious = ", round(prob_infectious_in_hospital,3), "\n") 
    cat("Proportion of days spent infectious in hosp = ", round(prop_infectious_days_in_hospitals,3), "\n\n") 
    
    cat("Assuming hospitalised cases are in upper 95% infectious duration", "\n") 
    cat("Prob of hospital case being infectious = ", round(prob_infectious_in_hospital_long,3), "\n") 
    cat("Proportion of days spent infectious in hosp = ", round(prop_infectious_days_in_hospitals_long,3), "\n\n") 

    cat("Assuming hospitalised cases are as estimated in van Kampman", "\n") 
    cat("Prob of hospital case being infectious = ", round(prob_infectious_in_hospital_hosp,3), "\n") 
    cat("Proportion of days spent infectious in hosp = ", round(prop_infectious_days_in_hospitals_hosp,3), "\n") 
    
} 