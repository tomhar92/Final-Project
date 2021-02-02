install.packages('TMB', type = 'source')
install.packages("glmmTMB")

.libPaths()

library(glmmTMB)

glmm_data <- read.csv(file.choose())

glmm_data$education_level <- as.factor(glmm_data$education_level)
glmm_data$experiment_device <- as.factor(glmm_data$experiment_device)
glmm_data$gender <- as.factor(glmm_data$gender)
glmm_data$mobile_hours <- as.factor(glmm_data$mobile_hours)
glmm_data$pc_hours <- as.factor(glmm_data$pc_hours)
glmm_data$online_flights <- as.factor(glmm_data$online_flights)

glmm_data$user_id <- as.factor(glmm_data$user_id)

# Is_Filler
fit1 <- glmmTMB(answer ~ Is_Filler+Required_to_Switch+age_group+
                  crowded_space+dark_bright_space+device_type_calc+education_level+
                   experiment_device+gender+large_small_space+mobile_hours+
                   noisy_quiet_space+online_flights+open_close_space+pc_hours+Is_Filler*experiment_device+
                   private_public_space+question+(1|user_id), data = glmm_data)

summary(fit1)

# Filler_Inter / Filer_Rel
data_glmm2 <- glmm_data[!is.na(glmm_data$Is_Inter) & !is.na(glmm_data$Is_Rel), ]

fit2 <- glmmTMB(answer ~ Is_Inter+Is_Rel+Required_to_Switch+age_group+
                  crowded_space+dark_bright_space+device_type_calc+education_level+
                  experiment_device+gender+large_small_space+mobile_hours+
                  noisy_quiet_space+online_flights+open_close_space+pc_hours+private_public_space+question+(1|user_id), data = data_glmm2)

summary(fit2)

# Is_Filler - Only Significant
fit3 <- glmmTMB(answer ~ Is_Filler+crowded_space+device_type_calc+education_level+
                  large_small_space+mobile_hours+online_flights+question+(1|user_id), data = glmm_data)

summary(fit3)

# Filler_Inter / Filler_Rel - Only Significant
fit4 <- glmmTMB(answer ~ Is_Inter+Is_Rel+crowded_space+device_type_calc+education_level+
                  large_small_space+online_flights+question+(1|user_id), data = data_glmm2)

summary(fit4)


# Is_Filler
fit5 <- glmmTMB(answer ~ Is_Filler*experiment_device+question +(1|user_id), data = glmm_data)

summary(fit5)

# Filler_Inter
fit6 <- glmmTMB(answer ~ Is_Inter*experiment_device+Is_Rel*experiment_device+question +(1|user_id), data = glmm_data)

summary(fit6)


pwt1_pwt2_pwt3 <- sqldf("select *
                             from glmm_data
                             where question =='pwt1' OR question =='pwt2' OR question =='pwt3'")
print(pwt1_pwt2_pwt3)
print(pwt1_pwt2_pwt3$question)

# Is_Filler
fit7 <- glmmTMB(answer ~ Is_Filler*experiment_device+question +(1|user_id), data = pwt1_pwt2_pwt3)

summary(fit7)

# Filler_Inter
fit8 <- glmmTMB(answer ~ Is_Inter*experiment_device+Is_Rel*experiment_device+question +(1|user_id), data = pwt1_pwt2_pwt3)

summary(fit8)

