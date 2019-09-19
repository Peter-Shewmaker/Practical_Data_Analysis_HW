full_data <- read.csv("Wang.csv")
taichi_data <- full_data[full_data$group == 1,]
pt_data <- full_data[full_data$group == 0,]
characteristics <- c("Demographics","Women, no. (%)","Age, years","White, no. (%)","Greater than or equal to high school education, no. (%)","Body mass index, kg/m","Disease condition","Duration of knee pain (on study knee), years","Radiograph score, no. (%)","K/L grade 2","K/L grade 3","K/L grade 4","Knee surgery, no. (%)","Patient VAS (range 0–10 cm)","Physician VAS (study knee; range 0–10 cm)","WOMAC pain (range 0–500 mm)", "WOMAC physical function (range 0–1,700 mm)", "WOMAC stiffness (range 0–200 mm)","Receiving NSAIDs prior to study, no. (%)", "Receiving analgesics prior to study, no. (%)","Self-reported comorbidities, no. (%)","Congestive heart disease","Hypertension","Diabetes mellitus","Health-related quality of life and others","SF-36 PCS (range 0–100)","SF-36 MCS (range 0–100)","CES-D (range 0–60)","Self-efficacy score (range 1–5)","Physical performance","6-minute walk test, yards","Balance score (range 0–5)","Chair stand score, seconds")
baseline_dataframe <- as.data.frame(cbind(characteristics, 0, 0,0), stringsAsFactors = FALSE)
names(baseline_dataframe) <- c("Characteristic", "taichi", "pt", "full")
row.names(baseline_dataframe) <- characteristics
baseline_dataframe <- baseline_dataframe[,-1]


#gender
baseline_dataframe$taichi[2] <- paste(sum(taichi_data$female), " (", round((sum(taichi_data$female)/dim(taichi_data)[1]) * 100), ")", sep ="")
#age
baseline_dataframe$taichi[3] <- paste(round(mean(taichi_data$age),1), " ± ", round(sd(taichi_data$age),1), sep ="")
#white (since no other races included)
baseline_dataframe$taichi[4] <- paste(sum(taichi_data$white), " (", round((sum(taichi_data$white)/dim(taichi_data)[1]) * 100), ")", sep ="")
#high.school.education
baseline_dataframe$taichi[5] <- paste(sum(taichi_data$high.school.education), " (", round((sum(taichi_data$high.school.education)/dim(taichi_data)[1]) * 100), ")", sep ="")
#bmi
baseline_dataframe$taichi[6] <- paste(round(mean(taichi_data$bmi),1), " ± ", round(sd(taichi_data$bmi),1), sep ="")
#duration
baseline_dataframe$taichi[8] <- paste(round(mean(taichi_data$duration,na.rm=TRUE),1), " ± ", round(sd(taichi_data$duration,na.rm=TRUE),1), sep ="")

#radiograph_score
#2
baseline_dataframe$taichi[10] <- paste(sum(taichi_data$radiograph.score == 2), " (", round((sum(taichi_data$radiograph.score == 2)/dim(taichi_data)[1]) * 100), ")", sep ="")

#3
baseline_dataframe$taichi[11] <- paste(sum(taichi_data$radiograph.score == 3), " (", round((sum(taichi_data$radiograph.score == 3)/dim(taichi_data)[1]) * 100), ")", sep ="")

#4
baseline_dataframe$taichi[12] <- paste(sum(taichi_data$radiograph.score == 4), " (", round((sum(taichi_data$radiograph.score == 4)/dim(taichi_data)[1]) * 100), ")", sep ="")

#knee.surgery
baseline_dataframe$taichi[13] <- paste(sum(taichi_data$knee.surgery), " (", round((sum(taichi_data$knee.surgery)/dim(taichi_data)[1]) * 100), ")", sep ="")

#patient VAS
baseline_dataframe$taichi[14] <- paste(round(mean(taichi_data$pt.global.vas.1,na.rm=TRUE),1), " ± ", round(sd(taichi_data$pt.global.vas.1,na.rm=TRUE),1), sep ="")

#physician VAS
baseline_dataframe$taichi[15] <- paste(round(mean(taichi_data$physician.vas.1,na.rm=TRUE),1), " ± ", round(sd(taichi_data$physician.vas.1,na.rm=TRUE),1), sep ="")

#womac pain
baseline_dataframe$taichi[16] <- paste(round(mean(taichi_data$womac.pain.1,na.rm=TRUE),1), " ± ", round(sd(taichi_data$womac.pain.1,na.rm=TRUE),1), sep ="")

#womac phys. function
baseline_dataframe$taichi[17] <- paste(round(mean(taichi_data$womac.phys.func.1,na.rm=TRUE),1), " ± ", round(sd(taichi_data$womac.phys.func.1,na.rm=TRUE),1), sep ="")

#womac stiffness
baseline_dataframe$taichi[18] <- paste(round(mean(taichi_data$pcs.1,na.rm=TRUE),1), " ± ", round(sd(taichi_data$pcs.1,na.rm=TRUE),1), sep ="")

#nsaids
baseline_dataframe$taichi[19] <- paste(sum(taichi_data$nsaids.1), " (", round((sum(taichi_data$nsaids.1)/dim(taichi_data)[1]) * 100), ")", sep ="")

#analgesics
baseline_dataframe$taichi[20] <- paste(sum(taichi_data$analg.1), " (", round((sum(taichi_data$analg.1)/dim(taichi_data)[1]) * 100), ")", sep ="")

#history chd
baseline_dataframe$taichi[22] <- paste(sum(taichi_data$hxchd), " (", round((sum(taichi_data$hxchd)/dim(taichi_data)[1]) * 100), ")", sep ="")

#history hypertension
baseline_dataframe$taichi[23] <- paste(sum(taichi_data$hxhtn), " (", round((sum(taichi_data$hxhtn)/dim(taichi_data)[1]) * 100), ")", sep ="")

#history diabetes
baseline_dataframe$taichi[24] <- paste(sum(taichi_data$hxdm), " (", round((sum(taichi_data$hxdm)/dim(taichi_data)[1]) * 100), ")", sep ="")

#sf-36 pcs
baseline_dataframe$taichi[26] <- paste(round(mean(taichi_data$pcs.1,na.rm=TRUE),1), " ± ", round(sd(taichi_data$pcs.1,na.rm=TRUE),1), sep ="")

#sf-36 mcs
baseline_dataframe$taichi[27] <- paste(round(mean(taichi_data$mcs.1,na.rm=TRUE),1), " ± ", round(sd(taichi_data$mcs.1,na.rm=TRUE),1), sep ="")

#cesd
baseline_dataframe$taichi[28] <- paste(round(mean(taichi_data$cesd.1,na.rm=TRUE),1), " ± ", round(sd(taichi_data$cesd.1,na.rm=TRUE),1), sep ="")

#self efficacy
baseline_dataframe$taichi[29] <- paste(round(mean(taichi_data$self.efficacy.1,na.rm=TRUE),1), " ± ", round(sd(taichi_data$self.efficacy.1,na.rm=TRUE),1), sep ="")

#walk test
baseline_dataframe$taichi[31] <- paste(round(mean(taichi_data$walkyard.1,na.rm=TRUE),1), " ± ", round(sd(taichi_data$walkyard.1,na.rm=TRUE),1), sep ="")

#balance
baseline_dataframe$taichi[32] <- paste(round(mean(taichi_data$balance.1,na.rm=TRUE),1), " ± ", round(sd(taichi_data$balance.1,na.rm=TRUE),1), sep ="")

#chairstand
baseline_dataframe$taichi[33] <- paste(round(mean(taichi_data$chairstand.1,na.rm=TRUE),1), " ± ", round(sd(taichi_data$chairstand.1,na.rm=TRUE),1), sep ="")


#gender
baseline_dataframe$pt[2] <- paste(sum(pt_data$female), " (", round((sum(pt_data$female)/dim(pt_data)[1]) * 100), ")", sep ="")
#age
baseline_dataframe$pt[3] <- paste(round(mean(pt_data$age),1), " ± ", round(sd(pt_data$age),1), sep ="")
#white (since no other races included)
baseline_dataframe$pt[4] <- paste(sum(pt_data$white), " (", round((sum(pt_data$white)/dim(pt_data)[1]) * 100), ")", sep ="")
#high.school.education
baseline_dataframe$pt[5] <- paste(sum(pt_data$high.school.education), " (", round((sum(pt_data$high.school.education)/dim(pt_data)[1]) * 100), ")", sep ="")
#bmi
baseline_dataframe$pt[6] <- paste(round(mean(pt_data$bmi),1), " ± ", round(sd(pt_data$bmi),1), sep ="")
#duration
baseline_dataframe$pt[8] <- paste(round(mean(pt_data$duration,na.rm=TRUE),1), " ± ", round(sd(pt_data$duration,na.rm=TRUE),1), sep ="")

#radiograph_score
#2
baseline_dataframe$pt[10] <- paste(sum(pt_data$radiograph.score == 2), " (", round((sum(pt_data$radiograph.score == 2)/dim(pt_data)[1]) * 100), ")", sep ="")

#3
baseline_dataframe$pt[11] <- paste(sum(pt_data$radiograph.score == 3), " (", round((sum(pt_data$radiograph.score == 3)/dim(pt_data)[1]) * 100), ")", sep ="")

#4
baseline_dataframe$pt[12] <- paste(sum(pt_data$radiograph.score == 4), " (", round((sum(pt_data$radiograph.score == 4)/dim(pt_data)[1]) * 100), ")", sep ="")

#knee.surgery
baseline_dataframe$pt[13] <- paste(sum(pt_data$knee.surgery), " (", round((sum(pt_data$knee.surgery)/dim(pt_data)[1]) * 100), ")", sep ="")

#patient VAS
baseline_dataframe$pt[14] <- paste(round(mean(pt_data$pt.global.vas.1,na.rm=TRUE),1), " ± ", round(sd(pt_data$pt.global.vas.1,na.rm=TRUE),1), sep ="")

#physician VAS
baseline_dataframe$pt[15] <- paste(round(mean(pt_data$physician.vas.1,na.rm=TRUE),1), " ± ", round(sd(pt_data$physician.vas.1,na.rm=TRUE),1), sep ="")

#womac pain
baseline_dataframe$pt[16] <- paste(round(mean(pt_data$womac.pain.1,na.rm=TRUE),1), " ± ", round(sd(pt_data$womac.pain.1,na.rm=TRUE),1), sep ="")

#womac phys. function
baseline_dataframe$pt[17] <- paste(round(mean(pt_data$womac.phys.func.1,na.rm=TRUE),1), " ± ", round(sd(pt_data$womac.phys.func.1,na.rm=TRUE),1), sep ="")

#womac stiffness
baseline_dataframe$pt[18] <- paste(round(mean(pt_data$pcs.1,na.rm=TRUE),1), " ± ", round(sd(pt_data$pcs.1,na.rm=TRUE),1), sep ="")

#nsaids
baseline_dataframe$pt[19] <- paste(sum(pt_data$nsaids.1), " (", round((sum(pt_data$nsaids.1)/dim(pt_data)[1]) * 100), ")", sep ="")

#analgesics
baseline_dataframe$pt[20] <- paste(sum(pt_data$analg.1), " (", round((sum(pt_data$analg.1)/dim(pt_data)[1]) * 100), ")", sep ="")

#history chd
baseline_dataframe$pt[22] <- paste(sum(pt_data$hxchd), " (", round((sum(pt_data$hxchd)/dim(pt_data)[1]) * 100), ")", sep ="")

#history hypertension
baseline_dataframe$pt[23] <- paste(sum(pt_data$hxhtn), " (", round((sum(pt_data$hxhtn)/dim(pt_data)[1]) * 100), ")", sep ="")

#history diabetes
baseline_dataframe$pt[24] <- paste(sum(pt_data$hxdm), " (", round((sum(pt_data$hxdm)/dim(pt_data)[1]) * 100), ")", sep ="")

#sf-36 pcs
baseline_dataframe$pt[26] <- paste(round(mean(pt_data$pcs.1,na.rm=TRUE),1), " ± ", round(sd(pt_data$pcs.1,na.rm=TRUE),1), sep ="")

#sf-36 mcs
baseline_dataframe$pt[27] <- paste(round(mean(pt_data$mcs.1,na.rm=TRUE),1), " ± ", round(sd(pt_data$mcs.1,na.rm=TRUE),1), sep ="")

#cesd
baseline_dataframe$pt[28] <- paste(round(mean(pt_data$cesd.1,na.rm=TRUE),1), " ± ", round(sd(pt_data$cesd.1,na.rm=TRUE),1), sep ="")

#self efficacy
baseline_dataframe$pt[29] <- paste(round(mean(pt_data$self.efficacy.1,na.rm=TRUE),1), " ± ", round(sd(pt_data$self.efficacy.1,na.rm=TRUE),1), sep ="")

#walk test
baseline_dataframe$pt[31] <- paste(round(mean(pt_data$walkyard.1,na.rm=TRUE),1), " ± ", round(sd(pt_data$walkyard.1,na.rm=TRUE),1), sep ="")

#balance
baseline_dataframe$pt[32] <- paste(round(mean(pt_data$balance.1,na.rm=TRUE),1), " ± ", round(sd(pt_data$balance.1,na.rm=TRUE),1), sep ="")

#chairstand
baseline_dataframe$pt[33] <- paste(round(mean(pt_data$chairstand.1,na.rm=TRUE),1), " ± ", round(sd(pt_data$chairstand.1,na.rm=TRUE),1), sep ="")

#gender
baseline_dataframe$full[2] <- paste(sum(full_data$female), " (", round((sum(full_data$female)/dim(full_data)[1]) * 100), ")", sep ="")
#age
baseline_dataframe$full[3] <- paste(round(mean(full_data$age),1), " ± ", round(sd(full_data$age),1), sep ="")
#white (since no other races included)
baseline_dataframe$full[4] <- paste(sum(full_data$white), " (", round((sum(full_data$white)/dim(full_data)[1]) * 100), ")", sep ="")
#high.school.education
baseline_dataframe$full[5] <- paste(sum(full_data$high.school.education), " (", round((sum(full_data$high.school.education)/dim(full_data)[1]) * 100), ")", sep ="")
#bmi
baseline_dataframe$full[6] <- paste(round(mean(full_data$bmi),1), " ± ", round(sd(full_data$bmi),1), sep ="")
#duration
baseline_dataframe$full[8] <- paste(round(mean(full_data$duration,na.rm=TRUE),1), " ± ", round(sd(full_data$duration,na.rm=TRUE),1), sep ="")

#radiograph_score
#2
baseline_dataframe$full[10] <- paste(sum(full_data$radiograph.score == 2), " (", round((sum(full_data$radiograph.score == 2)/dim(full_data)[1]) * 100), ")", sep ="")

#3
baseline_dataframe$full[11] <- paste(sum(full_data$radiograph.score == 3), " (", round((sum(full_data$radiograph.score == 3)/dim(full_data)[1]) * 100), ")", sep ="")

#4
baseline_dataframe$full[12] <- paste(sum(full_data$radiograph.score == 4), " (", round((sum(full_data$radiograph.score == 4)/dim(full_data)[1]) * 100), ")", sep ="")

#knee.surgery
baseline_dataframe$full[13] <- paste(sum(full_data$knee.surgery), " (", round((sum(full_data$knee.surgery)/dim(full_data)[1]) * 100), ")", sep ="")

#patient VAS
baseline_dataframe$full[14] <- paste(round(mean(full_data$pt.global.vas.1,na.rm=TRUE),1), " ± ", round(sd(full_data$pt.global.vas.1,na.rm=TRUE),1), sep ="")

#physician VAS
baseline_dataframe$full[15] <- paste(round(mean(full_data$physician.vas.1,na.rm=TRUE),1), " ± ", round(sd(full_data$physician.vas.1,na.rm=TRUE),1), sep ="")

#womac pain
baseline_dataframe$full[16] <- paste(round(mean(full_data$womac.pain.1,na.rm=TRUE),1), " ± ", round(sd(full_data$womac.pain.1,na.rm=TRUE),1), sep ="")

#womac phys. function
baseline_dataframe$full[17] <- paste(round(mean(full_data$womac.phys.func.1,na.rm=TRUE),1), " ± ", round(sd(full_data$womac.phys.func.1,na.rm=TRUE),1), sep ="")

#womac stiffness
baseline_dataframe$full[18] <- paste(round(mean(full_data$pcs.1,na.rm=TRUE),1), " ± ", round(sd(full_data$pcs.1,na.rm=TRUE),1), sep ="")

#nsaids
baseline_dataframe$full[19] <- paste(sum(full_data$nsaids.1), " (", round((sum(full_data$nsaids.1)/dim(full_data)[1]) * 100), ")", sep ="")

#analgesics
baseline_dataframe$full[20] <- paste(sum(full_data$analg.1), " (", round((sum(full_data$analg.1)/dim(full_data)[1]) * 100), ")", sep ="")

#history chd
baseline_dataframe$full[22] <- paste(sum(full_data$hxchd), " (", round((sum(full_data$hxchd)/dim(full_data)[1]) * 100), ")", sep ="")

#history hypertension
baseline_dataframe$full[23] <- paste(sum(full_data$hxhtn), " (", round((sum(full_data$hxhtn)/dim(full_data)[1]) * 100), ")", sep ="")

#history diabetes
baseline_dataframe$full[24] <- paste(sum(full_data$hxdm), " (", round((sum(full_data$hxdm)/dim(full_data)[1]) * 100), ")", sep ="")

#sf-36 pcs
baseline_dataframe$full[26] <- paste(round(mean(full_data$pcs.1,na.rm=TRUE),1), " ± ", round(sd(full_data$pcs.1,na.rm=TRUE),1), sep ="")

#sf-36 mcs
baseline_dataframe$full[27] <- paste(round(mean(full_data$mcs.1,na.rm=TRUE),1), " ± ", round(sd(full_data$mcs.1,na.rm=TRUE),1), sep ="")

#cesd
baseline_dataframe$full[28] <- paste(round(mean(full_data$cesd.1,na.rm=TRUE),1), " ± ", round(sd(full_data$cesd.1,na.rm=TRUE),1), sep ="")

#self efficacy
baseline_dataframe$full[29] <- paste(round(mean(full_data$self.efficacy.1,na.rm=TRUE),1), " ± ", round(sd(full_data$self.efficacy.1,na.rm=TRUE),1), sep ="")

#walk test
baseline_dataframe$full[31] <- paste(round(mean(full_data$walkyard.1,na.rm=TRUE),1), " ± ", round(sd(full_data$walkyard.1,na.rm=TRUE),1), sep ="")

#balance
baseline_dataframe$full[32] <- paste(round(mean(full_data$balance.1,na.rm=TRUE),1), " ± ", round(sd(full_data$balance.1,na.rm=TRUE),1), sep ="")

#chairstand
baseline_dataframe$full[33] <- paste(round(mean(full_data$chairstand.1,na.rm=TRUE),1), " ± ", round(sd(full_data$chairstand.1,na.rm=TRUE),1), sep ="")

baseline_dataframe[baseline_dataframe == 0] <- ""

names(baseline_dataframe) <- c("Tai Chi (n = 20)", "Attention Control (n = 20)", "Total (n = 40)")