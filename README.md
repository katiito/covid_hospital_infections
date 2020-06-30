# covid_hospital_infections

To run the R function, type:

```R
source("patientInfectiousness.R")
patientInfectiousness(hosp_detection) 
```
where hosp_detection is either "fast" or "slow" depending on whether the readmittance of hosp patients is at onset of symptoms (fast) or at normal hospitalisation time (slow)
Dependencies: The R packages ggplot2, dplyr, rriskDistributions

All parameters provided in:

```R
parameters.R
```

Calculate 
1) infectious days for each route of infection in each setting
2) the proportion of time that route of transmission spends in hospital
3) average numbe rof numbers across all routes for a CA or HA in both community and hospital

Assumptions:

1. Infectious period is dependent on whether cases are hospitalised.
2. Hospitalised cases are readmitted quickly or slower depending on detection (see above)
3. Average infectiousness of community acquired case in commmunity/hosp is calculated as weighted average of the routes of CA case being in community/hosp (same for HA)
