# Analysing_Hospital_Data_with_GLMs

We investigated how the probability of type of hospital admission changes given some covariates. Specifically, we used a nominal multinomial to model data on hospital admissions using the nnet package in R to determine whether age, race, length of stay and subsequent death can be used to
predict the probability that a patient came to hospital electively (patient choice), as an urgent case (GP referral) or an as emergency (ambulance).
We carried out model selection and subsequently plotted any relationships found. The dataset included the following variables:

• *provnum* - hospital identification number;

• *died* - logical indicating whether the patient died (1) or not (0);

• *white* - logical indicating whether the patient was white (1) or other (0);

• *los* - Integer value indicating the length of stay in the hospital (in days);

• *age* -
  1. = less than 25;
  2. = 25-44;
  3. = 45-64;
  4. = 65-69;
  5. = 70-74;
  6. = 75-79;
  7. = 80-84;
  8. = 85-89;
  9. = 90 and over;
• *age80* - Logical indicating whether the patient is 80+ years old (age categories 7, 8, 9);

• *admission* - This is the response variable, which contains three categories: Elective, Emergency, and Urgent.
These categories refer to the way the patient enters the hospital.
