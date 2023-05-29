# Analysing_Hospital_Data_with_GLMs

We investigated how the probability of type of hospital admission changes given some covariates. Specifically, we used a nominal multinomial to model data on hospital admissions using the nnet package in R to determine whether age, race, length of stay and subsequent death can be used to
predict the probability that a patient came to hospital electively (patient choice), as an urgent case (GP referral) or an as emergency (ambulance).
We used the Anova function in the car library to carry out model selection and the effects library to plot any relationships found. 
