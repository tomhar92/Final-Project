library(sqldf)
library(gsubfn)
library(proto)
library(RSQLite)

my_data <- read.csv(file.choose())
install.packages("dplyr")
my_data$Is_Rel[my_data$Is_Rel == '#N/A'] <- NA
my_data$Is_Inter[my_data$Is_Inter == '#N/A'] <- NA

print(anyNA(my_data$Is_Rel))
print(is.na(my_data$Is_Rel))
print(anyNA(my_data$Is_Inter))
print(is.na(my_data$Is_Inter))
# Show a random sample
set.seed(1234)
dplyr::sample_n(my_data, 10)

my_data$device_as_factor <- as.factor(my_data$experiment_device)

# MANOVA Test - Raw Questions
res.man1 <- manova(cbind(fi1, fi2, fi3, fi4, td1, td2, td3, td4, td5, he1, he2, he3, he4) ~ Is_Rel + Is_Inter + device_as_factor, data = my_data)
summary(res.man1)
summary.aov(res.man1)

res.man2 <- manova(cbind(fi1, fi2, fi3, fi4, td1, td2, td3, td4, td5, he1, he2, he3, he4) ~ Is_Filler + device_as_factor, data = my_data)
summary(res.man2)
summary.aov(res.man2)

# MANOVA test - FI, TD, HE
res.man3 <- manova(cbind(Sum_FI, Sum_TD, Sum_HE) ~ Is_Rel + Is_Inter + device_as_factor, data = my_data)
summary(res.man3)
summary.aov(res.man3)

res.man4 <- manova(cbind(Sum_FI, Sum_TD, Sum_HE) ~ Is_Filler + device_as_factor, data = my_data)
summary(res.man4)
summary.aov(res.man4)

# ANOVA test - PWT -  FI, TD, HE
res.aov1 <- aov(Sum_PWT ~ Sum_FI + Sum_TD + Sum_HE, data = my_data)
summary(res.aov1)

res.aov2 <- aov(Sum_PWT ~ fi1 + fi2 + fi3 + fi4 + td1 + td2 + td3 + td4 + td5 + he1 + he2 + he3 + he4, data = my_data)
summary(res.aov2)

# ANOVA test - PWT - REL, INTER ,DEVICE
res.aov3 <- aov(Sum_PWT  ~ Is_Rel + Is_Inter + device_as_factor, data = my_data)
summary(res.aov3)

# MANOVA test - PWT Questions
res.man5 <- manova(cbind(pwt1, pwt2, pwt3) ~ Is_Rel + Is_Inter + device_as_factor, data = my_data)
summary(res.man5)
summary.aov(res.man5)

# MANOVA test - PWT Questions
res.man6 <- manova(cbind(pwt1, pwt2, pwt3) ~ Is_Filler + device_as_factor, data = my_data)
summary(res.man6)
summary.aov(res.man6)

# ANOVA test - PWT - Filler ,DEVICE
res.aov3 <- aov(Sum_PWT  ~  Is_Filler + device_as_factor, data = my_data)
summary(res.aov3)

# GLM - PWT
model1 <- glm(Sum_PWT ~ Is_Rel + Is_Inter + device_as_factor + age_group + as.factor(gender) + as.factor(education_level) + , data = my_data)
summary(model1)

# GLM - PWT
model2 <- glm(Sum_PWT ~ Is_Rel*device_as_factor + Is_Inter*device_as_factor , data = my_data)
summary(model2)

# GLM - PWT
model3 <- glm(Sum_PWT ~ Is_Filler*device_as_factor , data = my_data)
summary(model3)

# GLM - PWT
model4 <- glm(Sum_PWT ~ Is_Inter*device_as_factor , data = my_data)
summary(model4)

# GLM - PWT
model5 <- glm(Sum_PWT ~ Is_Rel*device_as_factor , data = my_data)
summary(model5)

# GLM - PWT
model6 <- glm(Sum_PWT ~ Is_Rel + Is_Inter + device_as_factor  , data = my_data)
summary(model6)


############################################################ Paired T Test ################################################################################

##--------------------------------------------------------------Is_Filler---VS----TD,FI,HE

H1 <- t.test(my_data$Sum_TD ~ my_data$Is_Filler,var.equal = T)
print(H1)


H2 <- t.test(my_data$Sum_FI ~ my_data$Is_Filler,var.equal = T)
print(H2)


H3 <- t.test(my_data$Sum_HE ~ my_data$Is_Filler,var.equal = T)
print(H3)

##----------------------------------------------------------------Is_Rel---VS----TD,FI,HE

H4 <- t.test(my_data$Sum_TD ~ my_data$Is_Rel,var.equal = T)
print(H4)


H5 <- t.test(my_data$Sum_FI ~ my_data$Is_Rel,var.equal = T)
print(H5)


H6 <- t.test(my_data$Sum_HE ~ my_data$Is_Rel,var.equal = T)
print(H6)

##----------------------------------------------------------------Is_Inter---VS----TD,FI,HE

H7 <- t.test(my_data$Sum_TD ~ my_data$Is_Inter,var.equal = T)
print(H7)


H8 <- t.test(my_data$Sum_FI ~ my_data$Is_Inter,var.equal = T)
print(H8)


H9 <- t.test(my_data$Sum_HE ~ my_data$Is_Inter,var.equal = T)
print(H9)

##------------------------------------------device_as_factor_desktop_laptop---VS----TD,FI,HE

device_as_factor_desktop_laptop <- sqldf("select *
                             from my_data
                             where experiment_device =='desktop' OR experiment_device =='laptop'")
print(device_as_factor_desktop_laptop)
print(device_as_factor_desktop_laptop$experiment_device)

H10 <- t.test(device_as_factor_desktop_laptop$Sum_TD ~ device_as_factor_desktop_laptop$experiment_device,var.equal = T)
print(H10)

H11 <- t.test(device_as_factor_desktop_laptop$Sum_FI ~ device_as_factor_desktop_laptop$experiment_device,var.equal = T)
print(H11)

H12 <- t.test(device_as_factor_desktop_laptop$Sum_HE ~ device_as_factor_desktop_laptop$experiment_device,var.equal = T)
print(H12)

##-----------------------------------------------Is_Filler---Is_Rel----Is_Inter----VS----------PWT

H13 <- t.test(my_data$Sum_PWT ~ my_data$Is_Filler,var.equal = T)
print(H13)

H14 <- t.test(my_data$Sum_PWT ~ my_data$Is_Inter,var.equal = T)
print(H14)

H15 <- t.test(my_data$Sum_PWT ~ my_data$Is_Rel,var.equal = T)
print(H15)

##-------------------------------------------------------device_as_factor_desktop_mobile---VS----TD,FI,HE

device_as_factor_desktop_mobile <- sqldf("select *
                             from my_data
                             where experiment_device =='desktop' OR experiment_device =='mobile'")
print(device_as_factor_desktop_mobile)
print(device_as_factor_desktop_mobile$experiment_device)

H16 <- t.test(device_as_factor_desktop_mobile$Sum_TD ~ device_as_factor_desktop_mobile$experiment_device,var.equal = T)
print(H16)

H17 <- t.test(device_as_factor_desktop_mobile$Sum_FI ~ device_as_factor_desktop_mobile$experiment_device,var.equal = T)
print(H17)

H18 <- t.test(device_as_factor_desktop_mobile$Sum_HE ~ device_as_factor_desktop_mobile$experiment_device,var.equal = T)
print(H18)


##------------------------------------------------------device_as_factor_laptop_mobile---VS----TD,FI,HE

device_as_factor_laptop_mobile <- sqldf("select *
                             from my_data
                             where experiment_device =='laptop' OR experiment_device =='mobile'")
print(device_as_factor_laptop_mobile)
print(device_as_factor_laptop_mobile$experiment_device)

H19 <- t.test(device_as_factor_laptop_mobile$Sum_TD ~ device_as_factor_laptop_mobile$experiment_device,var.equal = T)
print(H19)

H20 <- t.test(device_as_factor_laptop_mobile$Sum_FI ~ device_as_factor_laptop_mobile$experiment_device,var.equal = T)
print(H20)

H21 <- t.test(device_as_factor_laptop_mobile$Sum_HE ~ device_as_factor_laptop_mobile$experiment_device,var.equal = T)
print(H21)

##---------------------------------------------------device_as_factor_desktop_laptop_mobile---VS---- PWT

##----------------------device_as_factor_desktop_laptop - PWT
H22 <- t.test(device_as_factor_desktop_laptop$Sum_PWT ~ device_as_factor_desktop_laptop$experiment_device,var.equal = T)
print(H22)

##----------------------device_as_factor_laptop_mobile - PWT
H23 <- t.test(device_as_factor_laptop_mobile$Sum_PWT ~ device_as_factor_laptop_mobile$experiment_device,var.equal = T)
print(H23)

##----------------------device_as_factor_desktop_mobile - PWT
H24 <- t.test(device_as_factor_desktop_mobile$Sum_PWT ~ device_as_factor_desktop_mobile$experiment_device,var.equal = T)
print(H24)

##--------------------------------------------device_as_factor_desktop_laptop_mobile - Is_Filler----VS-----PWT

##----------------------device_as_factor_desktop_laptop - Is_Filler--VS-----PWT
device_as_factor_desktop_laptop_Is_Filler <- sqldf("select *
                                         from my_data
                                         where (experiment_device =='desktop' OR experiment_device =='laptop') AND (Is_Filler == '1')")
print(device_as_factor_desktop_laptop_Is_Filler)

H25 <- t.test(device_as_factor_desktop_laptop_Is_Filler$Sum_PWT ~ device_as_factor_desktop_laptop_Is_Filler$experiment_device,var.equal = T)
print(H25)

##----------------------device_as_factor_laptop_mobile - Is_Filler--VS-----PWT
device_as_factor_laptop_mobile_Is_Filler <- sqldf("select *
                                         from my_data
                                         where (experiment_device =='mobile' OR experiment_device =='laptop') AND (Is_Filler == '1')")
print(device_as_factor_laptop_mobile_Is_Filler)

H26 <- t.test(device_as_factor_laptop_mobile_Is_Filler$Sum_PWT ~ device_as_factor_laptop_mobile_Is_Filler$experiment_device,var.equal = T)
print(H26)

##----------------------device_as_factor_desktop_mobile - Is_Filler--VS-----PWT
device_as_factor_desktop_mobile_Is_Filler <- sqldf("select *
                                         from my_data
                                         where (experiment_device =='mobile' OR experiment_device =='desktop') AND (Is_Filler == '1')")
print(device_as_factor_desktop_mobile_Is_Filler)

H27 <- t.test(device_as_factor_desktop_mobile_Is_Filler$Sum_PWT ~ device_as_factor_desktop_mobile_Is_Filler$experiment_device,var.equal = T)
print(H27)

##--------------------------------------------device_as_factor_desktop_laptop_mobile - Is_Rel----VS-----PWT

##----------------------device_as_factor_desktop_laptop - Is_Rel--VS-----PWT
device_as_factor_desktop_laptop_Is_Rel <- sqldf("select *
                                         from my_data
                                         where (experiment_device =='desktop' OR experiment_device =='laptop') AND (Is_Rel == '1')")
print(device_as_factor_desktop_laptop_Is_Rel)

H28 <- t.test(device_as_factor_desktop_laptop_Is_Rel$Sum_PWT ~ device_as_factor_desktop_laptop_Is_Rel$experiment_device,var.equal = T)
print(H28)

##----------------------device_as_factor_laptop_mobile - Is_Rel--VS-----PWT
device_as_factor_laptop_mobile_Is_Rel <- sqldf("select *
                                         from my_data
                                         where (experiment_device =='mobile' OR experiment_device =='laptop') AND (Is_Rel == '1')")
print(device_as_factor_laptop_mobile_Is_Rel)

H29 <- t.test(device_as_factor_laptop_mobile_Is_Rel$Sum_PWT ~ device_as_factor_laptop_mobile_Is_Rel$experiment_device,var.equal = T)
print(H29)

##----------------------device_as_factor_desktop_mobile - Is_Rel--VS-----PWT
device_as_factor_desktop_mobile_Is_Rel <- sqldf("select *
                                         from my_data
                                         where (experiment_device =='mobile' OR experiment_device =='desktop') AND (Is_Rel == '1')")
print(device_as_factor_desktop_mobile_Is_Rel)

H30 <- t.test(device_as_factor_desktop_mobile_Is_Rel$Sum_PWT ~ device_as_factor_desktop_mobile_Is_Rel$experiment_device,var.equal = T)
print(H30)


##-------------------------------------------device_as_factor_desktop_laptop_mobile - Is_Inter----VS-----PWT

##----------------------device_as_factor_desktop_laptop - Is_Inter--VS-----PWT
device_as_factor_desktop_laptop_Is_Inter <- sqldf("select *
                                                from my_data
                                                where (experiment_device =='desktop' OR experiment_device =='laptop') AND (Is_Inter == '1')")
print(device_as_factor_desktop_laptop_Is_Inter)

H31 <- t.test(device_as_factor_desktop_laptop_Is_Inter$Sum_PWT ~ device_as_factor_desktop_laptop_Is_Inter$experiment_device,var.equal = T)
print(H31)

##----------------------device_as_factor_laptop_mobile - Is_Inter--VS-----PWT
device_as_factor_laptop_mobile_Is_Inter <- sqldf("select *
                                               from my_data
                                               where (experiment_device =='mobile' OR experiment_device =='laptop') AND (Is_Inter == '1')")
print(device_as_factor_laptop_mobile_Is_Inter)

H32 <- t.test(device_as_factor_laptop_mobile_Is_Inter$Sum_PWT ~ device_as_factor_laptop_mobile_Is_Inter$experiment_device,var.equal = T)
print(H32)

##----------------------device_as_factor_desktop_mobile - Is_Inter--VS-----PWT
device_as_factor_desktop_mobile_Is_Inter <- sqldf("select *
                                                from my_data
                                                where (experiment_device =='mobile' OR experiment_device =='desktop') AND (Is_Inter == '1')")
print(device_as_factor_desktop_mobile_Is_Inter)

H33 <- t.test(device_as_factor_desktop_mobile_Is_Inter$Sum_PWT ~ device_as_factor_desktop_mobile_Is_Inter$experiment_device,var.equal = T)
print(H33)





##----------------------------------------------------------------------------------------------------------------

##------------------------------------------device_as_factor_desktop_laptop_mobile-------VS--------------TD,FI,HE

##------------------------------------------device_as_factor_desktop_laptop---VS----TD,FI,HE - Is_Filler

device_as_factor_desktop_laptop_Is_Filler <- sqldf("select *
                                         from my_data
                                                   where (experiment_device =='desktop' OR experiment_device =='laptop') AND (Is_Filler == '1')")
print(device_as_factor_desktop_laptop_Is_Filler)

H34 <- t.test(device_as_factor_desktop_laptop_Is_Filler$Sum_TD ~ device_as_factor_desktop_laptop_Is_Filler$experiment_device,var.equal = T)
print(H34)

H35 <- t.test(device_as_factor_desktop_laptop_Is_Filler$Sum_FI ~ device_as_factor_desktop_laptop_Is_Filler$experiment_device,var.equal = T)
print(H35)

H36 <- t.test(device_as_factor_desktop_laptop_Is_Filler$Sum_HE ~ device_as_factor_desktop_laptop_Is_Filler$experiment_device,var.equal = T)
print(H36)

##-------------------------------------------------------device_as_factor_desktop_mobile---VS----TD,FI,HE - Is_Filler

device_as_factor_desktop_mobile_Is_Filler <- sqldf("select *
                                         from my_data
                                                   where (experiment_device =='mobile' OR experiment_device =='desktop') AND (Is_Filler == '1')")
print(device_as_factor_desktop_mobile_Is_Filler)

H37 <- t.test(device_as_factor_desktop_mobile_Is_Filler$Sum_TD ~ device_as_factor_desktop_mobile_Is_Filler$experiment_device,var.equal = T)
print(H37)

H38 <- t.test(device_as_factor_desktop_mobile_Is_Filler$Sum_FI ~ device_as_factor_desktop_mobile_Is_Filler$experiment_device,var.equal = T)
print(H38)

H39 <- t.test(device_as_factor_desktop_mobile_Is_Filler$Sum_HE ~ device_as_factor_desktop_mobile_Is_Filler$experiment_device,var.equal = T)
print(H39)


##------------------------------------------------------device_as_factor_laptop_mobile---VS----TD,FI,HE - Is_Filler

device_as_factor_laptop_mobile_Is_Filler <- sqldf("select *
                                         from my_data
                                         where (experiment_device =='mobile' OR experiment_device =='laptop') AND (Is_Filler == '1')")
print(device_as_factor_laptop_mobile_Is_Filler)

H40 <- t.test(device_as_factor_laptop_mobile_Is_Filler$Sum_TD ~ device_as_factor_laptop_mobile_Is_Filler$experiment_device,var.equal = T)
print(H40)

H41 <- t.test(device_as_factor_laptop_mobile_Is_Filler$Sum_FI ~ device_as_factor_laptop_mobile_Is_Filler$experiment_device,var.equal = T)
print(H41)

H42 <- t.test(device_as_factor_laptop_mobile_Is_Filler$Sum_HE ~ device_as_factor_laptop_mobile_Is_Filler$experiment_device,var.equal = T)
print(H42)


##-------------------------------------------------------device_as_factor_desktop_mobile---VS----TD,FI,HE - Is_Inter

device_as_factor_desktop_mobile_Is_Inter <- sqldf("select *
                                                   from my_data
                                                   where (experiment_device =='mobile' OR experiment_device =='desktop') AND (Is_Inter == '1')")
print(device_as_factor_desktop_mobile_Is_Inter)

H43 <- t.test(device_as_factor_desktop_mobile_Is_Inter$Sum_TD ~ device_as_factor_desktop_mobile_Is_Inter$experiment_device,var.equal = T)
print(H43)

H44 <- t.test(device_as_factor_desktop_mobile_Is_Inter$Sum_FI ~ device_as_factor_desktop_mobile_Is_Inter$experiment_device,var.equal = T)
print(H44)

H45 <- t.test(device_as_factor_desktop_mobile_Is_Inter$Sum_HE ~ device_as_factor_desktop_mobile_Is_Inter$experiment_device,var.equal = T)
print(H45)

##-------------------------------------------------------Required_to_Switch---VS----PWT

H46 <- t.test(my_data$Sum_PWT ~ my_data$Required_to_Switch ,var.equal = T)
print(H46)
