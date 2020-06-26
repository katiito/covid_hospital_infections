# Parameters for hospital calculation

library(rriskDistributions)

readParameters <- function(duration_type){
    
    num_samples <- 50000
    
    # estimates of hospital stay non covid
    length_of_stay_mean = 4
    length_of_stay_k = 10
    
    # estimates taken from Davies et al. https://www.medrxiv.org/content/10.1101/2020.04.01.20049908v1
    latent_duration_mean = 4
    latent_duration_k = 4
    
    # time spent infectious (pre-symptomatic)
    preclinical_duration_mean = 1.5
    preclinical_duration_k = 4
    
    # time spent infectious (symptomatic)
    clinical_duration_mean = 3.5
    clinical_duration_k = 4
    
    # delay between onset and hospitalisation due to covid
    hospital_delayfromonset_mean = 7
    hospital_delayfromonset_k = 7
    
    # covid-specific length of stay
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
    
    
    
    
    if(duration_type=="indpt"){
      infectious_duration <- preclinical_duration + clinical_duration
      output_message <- "Assuming infectious duration is independent of hospitalisation risk"
    }else if(duration_type=="long"){
      infectious_duration_long_truncate <- sort(infectious_duration)[(0.95*num_samples):num_samples]
      infectious_duration <- sample(infectious_duration_long_truncate, num_samples, replace = TRUE)
      output_message <- "Assuming hospitalised cases are in upper 95% infectious duration"
    }else if(duration_type=="hosp"){
      infectious_duration <- rgamma(num_samples, scale = 1/g_out["rate"], shape =  g_out["shape"])
      output_message <- "Assuming hospitalised cases are as estimated in van Kampman"
    }
    
    ## Assuming that infected at hospital but not severely enough 
    ## to cause increase in length of stay
    
    duration_hospital_stay <- rgamma(num_samples, scale = length_of_stay_mean/length_of_stay_k, 
                                     shape = length_of_stay_k)
    
    outlist <- list("infectious_duration" = infectious_duration, 
                    "latent_duration" = latent_duration, 
                    "preclinical_duration" = preclinical_duration,
                    "hospital_delayfromonset" = hospital_delayfromonset, 
                  "hospital_duration" = hospital_duration, 
                  "duration_hospital_stay" = duration_hospital_stay, 
                  "output_message" = output_message,
                  "num_samples" = num_samples)
                    
    return(outlist)
}