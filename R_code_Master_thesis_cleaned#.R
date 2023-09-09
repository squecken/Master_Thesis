#Required Packages
library("ggplot2")
library("ggsci")
library("tidyverse")
library("dplyr")
library("ltm")
library("ggthemes")  
library("dunn.test")
library("ineq")
library("Hmisc")
library("colorspace")
library("RColorBrewer")
library("gridExtra")
library("grid")
library("papaja")
library('performance')
library('see')
library('rempsyc')
library("stargazer")
library("tidyverse")
library("RNHANES")
library("tableone")
library("labelled")
library("nhanesA")
library("lmtest")
library("broom")
library("sandwich")
library("car")
library("pscl")

##################################################################################################################
#Upload data

#Individual_data
data=read.csv(file.choose(), sep = ';', fileEncoding="UTF-8-BOM")
attach(data)
names(data)
View(data)

#Group_data
group=read.csv(file.choose(), sep = ';', fileEncoding="UTF-8-BOM")
attach(group)
names(group)
View(group)

##Color Scheme
my_colors = c("Baseline" = "steelblue", "Inequality" ="#D95F02")

#########################Diagnostics################################################################

#Indices 

#Individual
#Communication
cor(data$Group_agreed,data$Group_communication) 

df1=data[c("Group_agreed","Group_communication")]
cronbach.alpha(df1) 

#Cooperation
cor(data$Cooperation_self_without_score,data$Group_cooperation_score)

df2=data[c("Cooperation_self_without_score","Group_cooperation_score")]
cronbach.alpha(df2) 

#Group
#Communication

#Group self and experimenter rating 
cor(group$Communication_self_average,group$Communication_experimenter) 
df3=group[c("Communication_self_average","Communication_experimenter")]
cronbach.alpha(df3) 

#Cooperation
#Question and Score
cor(group$Cooperation_self_without_score_average,group$Group_cooperation_score_average) 
df4=group[c("Cooperation_self_without_score_average","Group_cooperation_score_average")]
cronbach.alpha(df4) 

#Self and Experiment Assessment 
cor(group$Cooperation_self_average,group$Cooperation_experimenter) 
df5=group[c("Cooperation_self_average","Cooperation_experimenter")]
cronbach.alpha(df5) 

#Self-efficacy 
df6=data[c("Self_efficacy_Q2","Self_efficacy_Q3","Reverse_self.efficacy_Q1")]
cronbach.alpha(df6) 
cor(df6) 

#######################################Descriptive Statistics###################################################

# Perform Shapiro-Wilk test for each group
group_names = unique(data$Treatment)

#Normality Individual-level
#Age
normality_test_age = lapply(group_names, function(grouping) shapiro.test(data$Age[data$Treatment == grouping]))
names(normality_test_age) = group_names
print(normality_test_age)

#Inequality Aversion
normality_test_inequality_aversion = lapply(group_names, function(grouping) shapiro.test(data$Inequality_aversion[data$Treatment == grouping]))
names(normality_test_inequality_aversion) = group_names
print(normality_test_inequality_aversion)

#Disadvantageous Inequality Aversion
normality_test_dis_inequality_aversion = lapply(group_names, function(grouping) shapiro.test(data$Disadvantageous_inequality_aversion[data$Treatment == grouping]))
names(normality_test_dis_inequality_aversion) = group_names
print(normality_test_dis_inequality_aversion)

#Advantageous Inequality Aversion
normality_test_ad_inequality_aversion = lapply(group_names, function(grouping) shapiro.test(data$Advantageous_inequality_aversion[data$Treatment == grouping]))
names(normality_test_ad_inequality_aversion) = group_names
print(normality_test_ad_inequality_aversion)

#Error_rate
normality_test_error_rate = lapply(group_names, function(grouping) shapiro.test(data$Error_rate[data$Treatment == grouping]))
names(normality_test_error_rate) = group_names
print(normality_test_error_rate)

#Total_payoff
normality_test_total_payoff = lapply(group_names, function(grouping) shapiro.test(data$Total_payoff[data$Treatment == grouping]))
names(normality_test_total_payoff) = group_names
print(normality_test_total_payoff)

#Normality Group-level 
#Age
normality_test_age2 = lapply(group_names, function(grouping) shapiro.test(group$Average_age[group$Treatment == grouping]))
names(normality_test_age2) = group_names
print(normality_test_age2)

#Gender composition
normality_test_gender = lapply(group_names, function(grouping) shapiro.test(group$Gender_composition[group$Treatment == grouping]))
names(normality_test_gender) = group_names
print(normality_test_gender)

#WEIRD Index
normality_test_WEIRD = lapply(group_names, function(grouping) shapiro.test(group$WEIRD_index[group$Treatment == grouping]))
names(normality_test_WEIRD) = group_names
print(normality_test_WEIRD)

#Average_education
normality_test_education = lapply(group_names, function(grouping) shapiro.test(group$Average_education[group$Treatment == grouping]))
names(normality_test_education) = group_names
print(normality_test_education)

#Group_economics
normality_test_economics = lapply(group_names, function(grouping) shapiro.test(group$Group_economics[group$Treatment == grouping]))
names(normality_test_economics) = group_names
print(normality_test_economics)

#Inequality Aversion
normality_test_inequality_aversion2 = lapply(group_names, function(grouping) shapiro.test(group$Inequality_aversion_average[group$Treatment == grouping]))
names(normality_test_inequality_aversion2) = group_names
print(normality_test_inequality_aversion2)

#Disadvantageous Inequality Aversion
normality_test_dis_inequality_aversion2 = lapply(group_names, function(grouping) shapiro.test(group$Disadvantageous_inequality_aversion_average[group$Treatment == grouping]))
names(normality_test_dis_inequality_aversion2) = group_names
print(normality_test_dis_inequality_aversion2)

#Advantageous Inequality Aversion
normality_test_ad_inequality_aversion2 = lapply(group_names, function(grouping) shapiro.test(group$Advantageous_inequality_aversion_average[group$Treatment == grouping]))
names(normality_test_ad_inequality_aversion2) = group_names
print(normality_test_ad_inequality_aversion2)

#Error_rate
normality_test_error_rate2 = lapply(group_names, function(grouping) shapiro.test(group$Error_rate_average[group$Treatment == grouping]))
names(normality_test_error_rate2) = group_names
print(normality_test_error_rate2)

#Average individual payoff 
Average_individual_payoff=group$Total_group_payoff/4
#Add column to dataset 
group=mutate(group,Average_individual_payoff)

normality_test_total_payoff2 = lapply(group_names, function(grouping) shapiro.test(group$Average_individual_payoff[group$Treatment == grouping]))
names(normality_test_total_payoff2) = group_names
print(normality_test_total_payoff2)

#Summary Table Participant Pool individual-level 
summary=data %>% 
  dplyr::select(c("Age","Treatment","Gender","WEIRD_country", "Education","Study_field_economics","Inequality_aversion","Total_payoff","Individual_overexploitation","Disadvantageous_inequality_aversion","Advantageous_inequality_aversion","Error_rate"))

tableone = CreateTableOne(data=summary, vars = c("Age","Gender","WEIRD_country", "Education","Study_field_economics","Total_payoff","Inequality_aversion","Disadvantageous_inequality_aversion","Advantageous_inequality_aversion","Error_rate"),
                           factorVars = c("Gender", "WEIRD_country","Education","Study_field_economics"),
                           strata = "Treatment")

print(tableone,showAllLevels = TRUE, nonnormal = c("Age","Inequality_aversion","Total_payoff","Disadvantageous_inequality_aversion","Advantageous_inequality_aversion","Error_rate"),exact= c("Gender","Education","WEIRD_country"))

#Summary Table Participant Pool group-level 
summary2=group %>% 
  dplyr::select(c("Average_age","Treatment","Gender_composition","WEIRD_index", "Average_education","Group_economics","Inequality_aversion_average","Disadvantageous_inequality_aversion_average","Advantageous_inequality_aversion_average","Error_rate_average","Average_individual_payoff"))

tableone_group = CreateTableOne(data=summary2, vars = c("Average_age","Gender_composition","WEIRD_index", "Average_education","Group_economics","Inequality_aversion_average","Disadvantageous_inequality_aversion_average","Advantageous_inequality_aversion_average","Error_rate_average","Average_individual_payoff"),
                                 strata = "Treatment")

print(tableone_group,showAllLevels = TRUE, nonnormal = c("Average_age","Gender_composition","WEIRD_index","Disadvantageous_inequality_aversion_average","Average_individual_payoff"))

#Double-check

#Age
wilcox.test(group$Average_age~group$Treatment)

#Gender
wilcox.test(group$Gender_composition~group$Treatment)

#WEIRD
wilcox.test(group$WEIRD_index~group$Treatment)

#Education
var.test(group$Average_education~group$Treatment, group, 
         alternative = "two.sided")
t.test(group$Average_education~group$Treatment,var.equal = FALSE)

#Economics
var.test(group$Group_economics~group$Treatment, group, 
         alternative = "two.sided")
t.test(group$Group_economics~group$Treatment,var.equal = TRUE)

#Inequality aversion
var.test(group$Inequality_aversion_average~group$Treatment, group, 
         alternative = "two.sided")
t.test(group$Inequality_aversion_average~group$Treatment,var.equal = TRUE)

#Dis inequality aversion
wilcox.test(group$Disadvantageous_inequality_aversion_average~group$Treatment)

#Ad Inequality aversion
var.test(group$Advantageous_inequality_aversion_average~group$Treatment, group, 
         alternative = "two.sided")
t.test(group$Advantageous_inequality_aversion_average~group$Treatment,var.equal = TRUE)

#Error rate
var.test(group$Error_rate_average~group$Treatment, group, 
         alternative = "two.sided")
t.test(group$Error_rate_average~group$Treatment,var.equal = TRUE)

#Average Payoff
wilcox.test(group$Average_individual_payoff~group$Treatment)

#####################################Comprehension Additional Exploration #####################################################################################

#Checking Error rate difference across groups
t.test(group$Error_rate~group$Treatment)
boxplot(group$Error_rate~group$Treatment) #visual check 

#Hardest Questions 
#Subset Comprehension Questions
comprehension=subset(data,select=c("Treatment","Instructions_Q1_errors","Instructions_Q2_errors","Instructions_Q3_errors","Instructions_Q4_errors","Instructions_Q5_errors"))

#Check Normality 
comprehension2=comprehension[,2:6]
apply(comprehension2,2,shapiro.test)

comprehension_table=CreateTableOne(data=comprehension,strata = "Treatment")
print(comprehension_table,nonnormal = c("Treatment","Instructions_Q1_errors","Instructions_Q2_errors","Instructions_Q3_errors","Instructions_Q4_errors","Instructions_Q5_errors"))

#Check if questions are significantly different from another
#Kruskal-Wallis test as not normally distributed 
kruskal.test(comprehension2)

#Post-hoc test 
dunn_result = dunn.test(comprehension2, method = "bonferroni")

summary_comprehension = data.frame(
  Question = names(comprehension2),
  Mean = apply(comprehension2, 2, mean),
  Median = apply(comprehension2, 2, median),
  Min = apply(comprehension2, 2, min),
  Max = apply(comprehension2, 2, max)
  )
print(summary_comprehension)

############################################Inequality Aversion #########################################################

#Compare if Disadvantageous and advantageous is significantly different 

wilcox.test(group$Disadvantageous_inequality_aversion_average, group$Advantageous_inequality_aversion_average, paired = TRUE,alternative="less")

mean(group$Disadvantageous_inequality_aversion_average)
median(group$Disadvantageous_inequality_aversion_average)

mean(group$Advantageous_inequality_aversion_average)
median(group$Advantageous_inequality_aversion_average)

###########################################Countries where students were from ###############################################
exclude_countries=c("344","446")
filtered_countries = subset(data$Nationality, !(Nationality %in% exclude_countries))
filtered_countries=data_frame(filtered_countries)

summary_countries = table(filtered_countries)
Summary_percentage=prop.table(summary_countries)*100

#Across treatment 
#Grouping
grouping=list('Europe'=c("DEU","GBR","ESP","FRA","IRL","ITA","POL","PRT","ROU","SWE"),'Asia'=c("CHN","IND","IRN","JOR","MYS","NPL","PAK","PHL","RUS","SGP","THA"),'North America'=c("USA","VGB","DMA"),"South America"=c("BRA"),'Australia'=c("AUS"),'Africa'=c("BWA","GHA","MUS","NGA","ZWE"))

Grouped_countries=ifelse(data$Nationality %in% unlist(grouping[["Europe"]]), "Europe",
                         ifelse(data$Nationality %in% unlist(grouping[["Asia"]]), "Asia",ifelse(data$Nationality %in% unlist(grouping[["Africa"]]), "Africa",ifelse(data$Nationality %in% unlist(grouping[["North America"]]), "North America",ifelse(data$Nationality %in% unlist(grouping[["South America"]]), "South America", "Australia")))))
summary_grouped_countries=table(Grouped_countries)
Grouped_countries_percentage=prop.table(summary_grouped_countries)*100
print(Grouped_countries_percentage)

#Visual
Percentage_countries_df=data.frame(Continent=c("Africa","Asia","Australia","Europe","North America","South America"),Percentage=c(3.750,30.00,1.875,61.250,2.500,0.625))
custom_colors = RColorBrewer::brewer.pal(8, "Dark2")

# Color to exclude (e.g., "darkorange")
color_to_exclude = c("#66A61E","#E7298A")

# Remove the color to exclude from the color palette
color_palette = custom_colors[custom_colors != color_to_exclude]

#piechart
ggplot(Percentage_countries_df, aes(x="", y=Percentage, fill=Continent)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+   scale_fill_manual(values = color_palette)  +  
  theme_void()

#Just Inequality 
data_inequality = data[data$Treatment=="Inequality",]
exclude_countries=c("344","446")
filtered_countries2 = subset(data_inequality$Nationality, !(data_inequality$Nationality %in% exclude_countries))
filtered_countries2=data_frame(filtered_countries2)

summary_countries2 = table(filtered_countries2)

Summary_percentage2=prop.table(summary_countries2)*100

#Grouping
grouping=list('Europe'=c("DEU","GBR","ESP","FRA","IRL","ITA","POL","PRT","ROU","SWE"),'Asia'=c("CHN","IND","IRN","JOR","MYS","NPL","PAK","PHL","RUS","SGP","THA"),'North America'=c("USA","VGB","DMA"),"South America"=c("BRA"),'Australia'=c("AUS"),'Africa'=c("BWA","GHA","MUS","NGA","ZWE"))

Grouped_countries2=ifelse(data_inequality$Nationality %in% unlist(grouping[["Europe"]]), "Europe",
                          ifelse(data_inequality$Nationality %in% unlist(grouping[["Asia"]]), "Asia",ifelse(data_inequality$Nationality %in% unlist(grouping[["Africa"]]), "Africa",ifelse(data_inequality$Nationality %in% unlist(grouping[["North America"]]), "North America",ifelse(data_inequality$Nationality %in% unlist(grouping[["South America"]]), "South America", "Australia")))))
summary_grouped_countries2=table(Grouped_countries2)
Grouped_countries_percentage2=prop.table(summary_grouped_countries2)*100
print(Grouped_countries_percentage2)

#Visual
Percentage_countries_df2=data.frame(Continent=c("Africa","Asia","Australia","Europe","North America","South America"),Percentage=c(2.50,26.25,1.25,66.25,2.50,1.25))

#piechart
ggplot(Percentage_countries_df2, aes(x="", y=Percentage, fill=Continent)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+ scale_fill_manual(values = color_palette) +  # Set the colorspace palette
  theme_void()

# Just Baseline 
data_baseline = data[data$Treatment=="Baseline",]
exclude_countries=c("344","446")
filtered_countries = subset(data_baseline$Nationality, !(data_baseline$Nationality %in% exclude_countries))
filtered_countries=data_frame(filtered_countries)

summary_countries = table(filtered_countries)

Summary_percentage=prop.table(summary_countries)*100

#Grouping
grouping=list('Europe'=c("DEU","GBR","ESP","FRA","IRL","ITA","POL","PRT","ROU","SWE"),'Asia'=c("CHN","IND","IRN","JOR","MYS","NPL","PAK","PHL","RUS","SGP","THA"),'North America'=c("USA","VGB","DMA"),"South America"=c("BRA"),'Australia'=c("AUS"),'Africa'=c("BWA","GHA","MUS","NGA","ZWE"))

Grouped_countries3=ifelse(data_baseline$Nationality %in% unlist(grouping[["Europe"]]), "Europe",
                          ifelse(data_baseline$Nationality %in% unlist(grouping[["Asia"]]), "Asia",ifelse(data_baseline$Nationality %in% unlist(grouping[["Africa"]]), "Africa",ifelse(data_baseline$Nationality %in% unlist(grouping[["North America"]]), "North America",ifelse(data_baseline$Nationality %in% unlist(grouping[["South America"]]), "South America", "Australia")))))
summary_grouped_countries3=table(Grouped_countries3)
Grouped_countries_percentage3=prop.table(summary_grouped_countries3)*100
print(Grouped_countries_percentage3)

#Visual
Percentage_countries_df3=data.frame(Continent=c("Africa","Asia","Australia","Europe","North America"),Percentage=c(5.00,33.75,2.50,56.25,2.50))

#piechart
ggplot(Percentage_countries_df3, aes(x="", y=Percentage, fill=Continent)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+ theme_void() + scale_fill_manual(values = "Dark2") +  # Set the colorspace palette
  theme_void()

############################Communication and Cheating##########################################

#Communication 
communicating_data=subset(group,select= c(Treatment,Communicating))
table(communicating_data)

#Cheating
cheating_data=subset(group,select= c(Treatment,Cheating))
table(cheating_data)
#There were 2 groups in the Baseline where cheating occurred and 4 groups in Inequality

#Comparing across groups 
Cheating_table=CreateTableOne(group,var=c("Cheating","Communicating","Group_agreement"),factorVars =c("Cheating","Communicating","Group_agreement"),  strata = "Treatment")
print(Cheating_table,showAllLevels = TRUE, exact = c("Cheating","Communicating"))

#Visualizations
data_counts1 = table(group$Treatment,group$Communicating)
data_percentages1 = prop.table(data_counts1, margin = 1) * 100

# Convert percentages to data frame and change column name
data_percentages1 = as.data.frame(data_percentages1)
colnames(data_percentages1)[2] = "Communicating"

data_percentages1 = data_percentages1 %>%
  mutate(Communicating= case_when(
    Communicating == 0 ~ "Not Communicating",
    Communicating == 1 ~ "Communicating"))

# Create a stacked bar chart with percentages
bar_chart_communicating = ggplot(data_percentages1, aes(x = Var1, y = Freq, fill = Communicating)) +
  geom_bar(stat = "identity", position = position_fill(reverse = F)) + theme_apa() +
  labs( x = "Group", y = "Percentage") +scale_fill_manual(values=c("steelblue","#D95F02"))

print(bar_chart_communicating)

#################################################################Visualization ###########################################################################

#Resource Stock Size over rounds 

#Import Mean data
Resource_size_group=read.csv(file.choose(), sep = ';')
attach(Resource_size_group)
names(Resource_size_group)

#Melting data long format 
Long_resource_size_group = Resource_size_group %>% pivot_longer(cols=c('Stock_size_Baseline', 'Stock_size_Treatment'),
                                                                 names_to='Treatment',
                                                                 values_to='Stock_size',
                                                                 values_drop_na = F)

plot1= ggplot(Long_resource_size_group, aes(x=Round, y=Stock_size)) + 
  geom_line(aes(linetype=Treatment),linewidth=0.8)+ 
  geom_hline(aes(yintercept=20,linetype="Threshold"), color="red")+
  scale_linetype_manual(name = 'Legend',
                        values = c("Stock_size_Baseline" = "solid", "Stock_size_Treatment" = "dashed", "Threshold" = "solid"),
                        labels = c("Baseline", "Inequality", "Threshold"),
                        guide = guide_legend(override.aes = list(color = c("black", "black", "red"),
                                                                 linetype = c("solid", "dashed", "solid")))) +
  scale_color_manual(name = "Legend",
                     values = "black",
                     guide = "none")

plot1 + scale_x_continuous(name="Rounds", limits=c(0, 12),expand = c(0, 0),breaks=seq(0,12,1)) +
  scale_y_continuous(name="Resource stock size", expand = c(0, 0),limits=c(0, 50)) +theme_apa()

#without grey overlay
plot2 = ggplot(Long_resource_size_group, aes(x = Round, y = Stock_size)) + 
  geom_line(aes(linetype = Treatment), color = "black", linewidth = 0.8, show.legend = FALSE) +
  geom_hline(aes(yintercept = 20, linetype = "Threshold"), color = "red") +
  scale_linetype_manual(name = "Legend",
                        values = c("Stock_size_Baseline" = "solid", "Stock_size_Treatment" = "dashed", "Threshold" = "solid"),
                        labels = c("Baseline", "Inequality", "Threshold")) +
  scale_color_manual(name = "Legend",
                     values = "black")

plot2 + scale_x_continuous(name = "Rounds", limits = c(0, 12), expand = c(0, 0), breaks = seq(0, 12, 1)) +
  scale_y_continuous(name = "Resource stock size", expand = c(0, 0), limits = c(0, 50)) +
  theme_apa() +
  guides(linetype = guide_legend(override.aes = list(color = c("black", "black", "red"))),
         color = guide_legend(override.aes = list(linetype = c("solid"))))

par(mfrow = c(1, 2))
#Baseline  Groups
Resource_size_baseline=read.csv(file.choose(), sep = ';')
attach(Resource_size_baseline)
names(Resource_size_baseline)

#Plot 
plot_baseline = ggplot(Resource_size_baseline, aes(x=Round)) +  
  geom_line(aes(y = vhax0sj8), color = "black") +
  geom_line(aes(y = y2seyqgk), color = "black")+geom_line(aes(y = zhyc1dif), color = "black") +
  geom_line(aes(y = el6j4dhp), color = "black")+geom_line(aes(y = hoe6797c), color = "black") +
  geom_line(aes(y = v4kj1iqm), color = "black")+geom_line(aes(y = X7hg7bsd4), color = "black") +
  geom_line(aes(y = pxjeh3z5), color = "black")+geom_line(aes(y = f9ijb5vw), color = "black") +
  geom_line(aes(y = X6cvxvlkf), color = "black")+geom_line(aes(y = scf96kxc), color = "black") +
  geom_line(aes(y = rk5emaoe), color = "black")+geom_line(aes(y = X8tmhnr3b), color = "black") +
  geom_line(aes(y = ha62evlu), color = "black")+geom_line(aes(y = ohb48ify), color = "black") +
  geom_line(aes(y = th0ckpdz), color = "black")+ geom_line(aes(y = peele785), color = "black")+
  geom_line(aes(y = htwbukxr), color = "black")+geom_line(aes(y = rpxz3u06), color = "black") +
  geom_line(aes(y = X7d8nd4o9), color = "black")

plot_baseline=plot_baseline + scale_x_continuous(name="Rounds", limits=c(0, 12),expand = c(0, 0),breaks=seq(0,12,1)) +
  scale_y_continuous(name="Resource stock size", expand = c(0, 0),limits=c(0, 50)) + theme_apa()+ geom_hline(aes(yintercept = 20, color = "Threshold"), linetype = "solid",linewidth=1) +
  scale_color_manual(name = "Legend", values = "red", labels = "Threshold") +
  guides(color = guide_legend(override.aes = list(linetype = "solid"))) +
  ggtitle("Baseline")

print(plot_baseline)

#Inequality Groups
Resource_size_inequality=read.csv(file.choose(), sep = ';')
attach(Resource_size_inequality)
names(Resource_size_inequality)

#Plot 
plot_inequality = ggplot(Resource_size_inequality, aes(x=Round)) +  
  geom_line(aes(y = X5axrge5g), color = "black") +
  geom_line(aes(y = k5h3hrn1), color = "black")+geom_line(aes(y = X3g71r84f), color = "black") +
  geom_line(aes(y = dl30wtp6), color = "black")+geom_line(aes(y = vvnsq32d), color = "black") +
  geom_line(aes(y = X74euqxl0), color = "black")+geom_line(aes(y = X8ef0zzio), color = "black") +
  geom_line(aes(y = wdsjvw37), color = "black")+geom_line(aes(y = htjbqker), color = "black") +
  geom_line(aes(y = wmlws272), color = "black")+geom_line(aes(y = k9w88t9z), color = "black") +
  geom_line(aes(y = xqf23x4v), color = "black")+geom_line(aes(y = jtjonvuv), color = "black") +
  geom_line(aes(y = X8sbx8vub), color = "black")+geom_line(aes(y = hdn0frs8), color = "black") +
  geom_line(aes(y = X19z8vruy), color = "black")+ geom_line(aes(y = jnsnll1l), color = "black")+
  geom_line(aes(y = X7yj991y1), color = "black")+geom_line(aes(y = x9m64eh2), color = "black") +
  geom_line(aes(y = ucowb7uy), color = "black")

plot_inequality=plot_inequality + scale_x_continuous(name="Rounds", limits=c(0, 12),expand = c(0, 0),breaks=seq(0,12,1)) +
  scale_y_continuous(name="Resource stock size", expand = c(0, 0),limits=c(0, 50)) + theme_apa()+ geom_hline(aes(yintercept = 20, color = "Threshold"), linetype = "solid",linewidth=1) +
  scale_color_manual(name = "Legend", values = "red", labels = "Threshold") +
  guides(color = guide_legend(override.aes = list(linetype = "solid")))+
  ggtitle("Inequality")

print(plot_inequality)

# Add a separation line using grid
separation_line = rectGrob(
  x = unit(0.5, "npc"), y = unit(0.5, "npc"),
  width = unit(1, "npc"), height = unit(0, "lines"),
  gp = gpar(lwd = 2))

## Arrange the plots vertically
grid.arrange(plot_baseline, separation_line, plot_inequality, ncol = 1,heights = c(4, 0.2, 4))

#######################Interaction over time###################################################

Resource_size_time=read.csv(file.choose(), sep = ';')

# Reshape the data to long format and infer round numbers from column names
long_dataset = 
  Resource_size_time %>%
  gather(Stock_size, stock_size, starts_with("Stock_size_")) %>%
  mutate(round_number = as.numeric(gsub("Stock_size_", "", Stock_size)))

# Perform linear regression with fixed effects and quadratic term
reg_model = lm(stock_size ~ Treatment + round_number + I(round_number^2) + Treatment:round_number + Treatment:I(round_number^2), data = long_dataset)

# Obtain robust standard errors
robust_se = sqrt(diag(vcovHC(reg_model, type = "HC1")))

# Display the regression results
summary(reg_model)

#######################################################Comparing Averages and Proportions on selected variables across treatment##################

#Subset group data with needed variables 
Averages=group %>%
  dplyr::select(Treatment,Group_average_before_threshold, Cooperation_combined,Communication_combined,Rounds_before_threshold)

#Normality check
#Cooperation combined
normality_Cooperation = lapply(group_names, function(grouping) shapiro.test(group$Cooperation_combined[group$Treatment == grouping]))
names(normality_Cooperation) = group_names
print(normality_Cooperation)

#Communication combined 
normality_Communication = lapply(group_names, function(grouping) shapiro.test(group$Communication_combined[group$Treatment == grouping]))
names(normality_Communication) = group_names
print(normality_Communication)

#Rounds before threshold
normality_rounds = lapply(group_names, function(grouping) shapiro.test(group$Rounds_before_threshold[group$Treatment == grouping]))
names(normality_rounds) = group_names
print(normality_rounds)

Average_func_pvalues = function(Averages) {
  tmp = split(Averages, Averages$Treatment)
  stack(Map(function(x, y) wilcox.test(x, y, exact = FALSE)$p.value, tmp[[1]][-1], tmp[[2]][-1]))
}
p_values2 = Average_func_pvalues(Averages)

Average_func_statistic = function(Averages) {
  tmp = split(Averages, Averages$Treatment)
  stack(Map(function(x, y) wilcox.test(x, y, exact = FALSE)$statistic, tmp[[1]][-1], tmp[[2]][-1]))
}
test_scores2 = Average_func_statistic(Averages)

#Mann-Whitney Tests for all Survey variables that are continuous as all variables are non-normally distributed 

merge(p_values2, test_scores2, by = "ind", suffix = c("_pvalue", "_wilcox_statistic"))

#Test for categorical data 

#Proportions
#Subset group data with needed variables 
Proportions=group%>%
  dplyr::select(Treatment,Threshold_crossed,Depletion,Communicating,Overexploiting)

#Threshold
Table_Threshold=table(Proportions$Treatment,Proportions$Threshold_crossed)
prop.table(Table_Threshold,margin = 1)

#Depletion
Table_Depletion=table(Proportions$Treatment,Proportions$Depletion)
prop.table(Table_Depletion,margin = 1)

#Communicating
Table_Communicating=table(Proportions$Treatment,Proportions$Communicating)
prop.table(Table_Communicating,margin = 1)

#Overexploiting above optimal claim on average before threshold
Table_Overexploiting=table(Proportions$Treatment,Proportions$Overexploiting)
prop.table(Table_Overexploiting,margin = 1)

#Chi-square test

#Threshold Crossing 
chisq.test(Proportions$Treatment, Proportions$Threshold_crossed, correct=FALSE)

#Depletion
chisq.test(Proportions$Treatment, Proportions$Depletion, correct=FALSE)

#Communicating 
chisq.test(Proportions$Treatment, Proportions$Communicating, correct=FALSE)

#Fishers exact test
#Overexploiting
fisher.test(Table_Overexploiting)

#Create a Table one
#Averages and proportion combined 
Average_proportions=group%>%
  dplyr::select(Treatment,Threshold_crossed,Depletion,Communicating,Group_average_before_threshold, Cooperation_combined,Communication_combined,Rounds_before_threshold)

Table_prop_avg=CreateTableOne(data=Average_proportions,factorVars =c("Threshold_crossed","Depletion","Communicating"),strata = "Treatment")

print(Table_prop_avg,nonnorm=c("Group_average_before_threshold", "Cooperation_combined","Communication_combined","Rounds_before_threshold"))

###############################################################Average overexploitation over time #############################################################################

# Reshape the data to long format and filter for positive overexploitation values
positive_overexploitation = group %>%
  gather(Round, Overexploitation, starts_with("Optimal_claim_difference_round")) %>%
  mutate(Round_number = as.numeric(gsub("Optimal_claim_difference_round", "", Round))) %>%
  filter(Overexploitation > 0) %>%
  group_by(Treatment, Round_number) %>%
  summarise(Average_Overexploitation = mean(Overexploitation, na.rm = TRUE))

# Create the bar plot
ggplot(positive_overexploitation, aes(x = Round_number, y = Average_Overexploitation, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Rounds", y = "Average Overexploitation") +
  theme_apa() +
  scale_fill_discrete(name = "Treatment", labels = c("Baseline", "Inequality")) +scale_x_continuous(breaks = 1:12, labels = paste(1:12)) +scale_fill_manual(values = my_colors) 

###########################################################Inferential Statistics######################################################################################

#Hypothesis 1

#Threshold crossing visualizations 
data_counts = table(group$Treatment,group$Threshold_crossed)
data_percentages = prop.table(data_counts, margin = 1) * 100

#Convert percentages to data frame and change column name
data_percentages = as.data.frame(data_percentages)
colnames(data_percentages)[2] = "Threshold"

data_percentages = data_percentages %>%
  mutate(Threshold = case_when(
    Threshold == 0 ~ "Not Crossed",
    Threshold == 1 ~ "Crossed"))

#Create a stacked bar chart with percentages
bar_chart = ggplot(data_percentages, aes(x = Var1, y = Freq, fill = Threshold)) +
  geom_bar(stat = "identity", position = position_fill(reverse = F)) + theme_apa() +
  labs( x = "Group", y = "Percentage") +scale_fill_manual(values=c("#D95F02","steelblue"))

#Chi-square  test 
Proportions=group%>%
  dplyr::select(Treatment,Threshold_crossed,Depletion,Communicating,Overexploiting)

#Threshold
Table_Threshold=table(Proportions$Treatment,Proportions$Threshold_crossed)
prop.table(Table_Threshold,margin = 1)

#Threshold Crossing 
chisq.test(Proportions$Treatment, Proportions$Threshold_crossed, correct=T)

######Check number of rounds##########

#visualization with boxplot ###
ggplot(group,aes(x=Treatment,y=Rounds_before_threshold))+geom_boxplot()+ theme_apa()+labs(y="Number of rounds before threshold crossing", x="Treatment")

#Normality Check 
#Rounds before threshold
normality_rounds = lapply(group_names, function(grouping) shapiro.test(group$Rounds_before_threshold[group$Treatment == grouping]))
names(normality_rounds) = group_names
print(normality_rounds)

# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Rounds_before_threshold ~ Treatment, data = group)

#Median round by treatment 
median_by_treatment = group %>%
  group_by(Treatment) %>%
  summarise(Median = median(Rounds_before_threshold))

#Mean round number by treatment
mean_by_treatment = group %>%
  group_by(Treatment) %>%
  summarise(Median = mean(Rounds_before_threshold))

#########Hypothesis 2 #########################

Data_inequality=data[data$Treatment=="Inequality",]
View(Data_inequality)

#Average harvest before threshold 
#Check normality  
group_names3=unique(Data_inequality$Harvest_restriction)

normality_average_harvest = lapply(group_names3, function(grouping) shapiro.test(Data_inequality$Average_harvest_threshold[Data_inequality$Harvest_restriction == grouping]))
names(normality_average_harvest) = group_names3
print(normality_average_harvest)

#visualization with boxplot
ggplot(Data_inequality,aes(x=Harvest_restriction,y=Average_harvest_threshold))+geom_boxplot()+ theme_apa()+labs(y="Average harvest", x="Treatment")+geom_boxplot(fill =c("#D95F02","steelblue"))
#only non-cooperating
ggplot(data,aes(x=Harvest_restriction,y=Average_harvest_threshold))+geom_boxplot()+ theme_apa()+labs(y="Average harvest", x="Treatment")+geom_boxplot(fill =c("#D95F02","steelblue","lightyellow"))+ggtitle("All Non-Cooperative Groups")+scale_x_discrete(labels = c("Baseline", "HAC", "LAC"))

##Visualization Optimal Claim Difference over rounds  
#reshape data 
Data_inequality_plot = Data_inequality[, c("Harvest_restriction", grep("^Optimal", names(Data_inequality), value = TRUE))]
Data_long = pivot_longer(Data_inequality_plot, cols = starts_with("Optimal"), names_to = "Round", values_to = "Optimal_Claim")

# Calculate the average deviation for each group and round
average_deviations = aggregate(Optimal_Claim ~ Harvest_restriction + Round, Data_long, FUN = mean)
# Convert the "Round" variable to a factor with original levels
average_deviations$Round = factor(average_deviations$Round, levels = unique(Data_long$Round))

round_labels=c("1","2","3","4","5","6","7","8","9","10","11","12")
# Create a line plot with the reordered rounds
ggplot(average_deviations, aes(x = Round, y = Optimal_Claim, group = Harvest_restriction, color = Harvest_restriction)) +
  geom_line() +
  labs(x = "Round", y = "Optimal Claim Difference",color="Adaptive Capacity") +
  scale_color_manual(values = c("LAC" = "steelblue", "HAC" = "#D95F02")) +geom_hline(yintercept = 0, linetype = "dashed", color = "black")+scale_x_discrete(labels = round_labels)+theme_apa()

#Also including Baseline 
Data_plot = data[, c("Harvest_restriction", grep("^Optimal", names(data), value = TRUE))]
Data_long2= pivot_longer(Data_plot, cols = starts_with("Optimal"), names_to = "Round", values_to = "Optimal_Claim")

# Calculate the average deviation for each group and round
average_deviations2 = aggregate(Optimal_Claim ~ Harvest_restriction + Round, Data_long2, FUN = mean)
# Convert the "Round" variable to a factor with original levels
average_deviations2$Round = factor(average_deviations2$Round, levels = unique(Data_long2$Round))

# Update the values in the specified column where the value is 0
average_deviations2[average_deviations2[,"Harvest_restriction"] == 0,"Harvest_restriction"] = "Baseline"

round_labels=c("1","2","3","4","5","6","7","8","9","10","11","12")
# Create a line plot with the reordered rounds
ggplot(average_deviations2, aes(x = Round, y = Optimal_Claim, group = Harvest_restriction, color = Harvest_restriction)) +
  geom_line() +
  labs(x = "Round", y = "Optimal Claim Difference",color="Adaptive Capacity") +
  scale_color_manual(values = c("LAC" = "steelblue", "HAC" = "#D95F02","Baseline"="purple")) +geom_hline(yintercept = 0, linetype = "dashed", color = "black")+scale_x_discrete(labels = round_labels)+theme_apa()

###regression to account for group effects ###

# Fit regression model
reg_inequality_harvest = lm(Average_harvest_threshold~ Harvest_restriction+WEIRD_country+Age+Gender.composition, data = Data_inequality)
summary(reg_inequality_harvest)

#cluster SE for group effects and robust SE
library(sandwich)
vcov_clust = vcovCL(reg_inequality_harvest, cluster = ~ Session)
library(lmtest)
robust_test_inequality_harvest = coeftest(reg_inequality_harvest, vcov_clust)
print(robust_test_inequality_harvest)

#Check assumptions
par(mfrow=c(2,2))
plot(reg_inequality_harvest, which=1:4)
check_model(reg_inequality_harvest)

#1.Linearity
resettest(reg_inequality_harvest)

#2.Homogeneity
bptest(reg_inequality_harvest)

#3.Normality
shapiro.test(rstandard(reg_inequality_harvest))

#4.Independence
durbinWatsonTest(reg_inequality_harvest)

#5.Outliers
par(mfrow=c(1,1))
plot(reg_inequality_harvest,5)

#6.Multicollinearity
vif(reg_inequality_harvest)

##Only round 1
# Fit regression model.
reg_inequality_harvest1 = lm(Harvest_round1~ Harvest_restriction+WEIRD_country+Age+Gender.composition, data = Data_inequality)
summary(reg_inequality_harvest1)

#cluster SE for group effects 
library(sandwich)
vcov_clust = vcovCL(reg_inequality_harvest1, cluster = ~ Session)
library(lmtest)
robust_test_inequality_harvest1 = coeftest(reg_inequality_harvest1, vcov_clust)
print(robust_test_inequality_harvest1)

#Check assumptions
par(mfrow=c(2,2))
plot(reg_inequality_harvest, which=1:4)
check_model(reg_inequality_harvest1)

#1.Linearity
resettest(reg_inequality_harvest1)

#2.Homogeneity
bptest(reg_inequality_harvest1)

#3.Normality
shapiro.test(rstandard(reg_inequality_harvest1))

#4.Independence
durbinWatsonTest(reg_inequality_harvest1)

#5.Outliers
par(mfrow=c(1,1))
plot(reg_inequality_harvest1,5)

#6.Multicollinearity
vif(reg_inequality_harvest1)

#Non-cooperating groups 
non_cooperating= group[group$Cooperation_combined < 4, ] #23 non-cooperating groups in total 
non_cooperating_ids = non_cooperating$Group_session

#Non-Cooperating groups only within inequality 
non_cooperating_data = Data_inequality[Data_inequality$Session %in% non_cooperating_ids, ]

reg_inequality_harvest_coop2 = lm(Average_harvest_threshold~ Harvest_restriction+WEIRD_country+Age+Gender.composition, data = non_cooperating_data)
summary(reg_inequality_harvest_coop2)

#cluster SE for group effects 
library(sandwich)
vcov_clust = vcovCL(reg_inequality_harvest_coop2, cluster = ~ Session)
library(lmtest)
robust_test_inequality_harvest_coop2 = coeftest(reg_inequality_harvest_coop2, vcov_clust)
print(robust_test_inequality_harvest_coop2)

#Check assumptions
par(mfrow=c(2,2))
plot(reg_inequality_harvest_coop2, which=1:4)
check_model(reg_inequality_harvest_coop2)

#1.Linearity
resettest(reg_inequality_harvest_coop2)

#2.Homogeneity
bptest(reg_inequality_harvest_coop2)

#3.Normality
shapiro.test(rstandard(reg_inequality_harvest_coop2))

#4.Independence
durbinWatsonTest(reg_inequality_harvest_coop2)

#5.Outliers
par(mfrow=c(1,1))
plot(reg_inequality_harvest_coop2,5)

#6.Multicollinearity
vif(reg_inequality_harvest_coop2)

#Non-Cooperating groups whole dataset
non_cooperating_data2 = data[data$Session %in% non_cooperating_ids, ]

reg_inequality_harvest_coop3 = lm(Average_harvest_threshold~ Harvest_restriction+WEIRD_country+Age+Gender.composition, data = non_cooperating_data2)
summary(reg_inequality_harvest_coop3)

#cluster SE for group effects 
library(sandwich)
vcov_clust = vcovCL(reg_inequality_harvest_coop3, cluster = ~ Session)
library(lmtest)
robust_test_inequality_harvest_coop3 = coeftest(reg_inequality_harvest_coop3, vcov_clust)
print(robust_test_inequality_harvest_coop3)

#Check assumptions
par(mfrow=c(2,2))
plot(reg_inequality_harvest_coop3, which=1:4)
check_model(reg_inequality_harvest_coop3)

#1.Linearity
resettest(reg_inequality_harvest_coop3)

#2.Homogeneity
bptest(reg_inequality_harvest_coop3)

#3.Normality
shapiro.test(rstandard(reg_inequality_harvest_coop3))

#4.Independence
durbinWatsonTest(reg_inequality_harvest_coop3)

#5.Outliers
par(mfrow=c(1,1))
plot(reg_inequality_harvest_coop3,5)

#6.Multicollinearity
vif(reg_inequality_harvest_coop3)

######################Hypothesis 3 ###############################################
reg_Inequality_aversion_harvest= lm(Average_harvest_threshold ~ Inequality_aversion*Treatment+WEIRD_country+Age+Gender.composition,data = data)
summary(reg_Inequality_aversion_harvest)

#cluster SE for group effects 
library(sandwich)
vcov_clust = vcovCL(reg_Inequality_aversion_harvest, cluster = ~ Session)
library(lmtest)
robust_test_inequality_harvest2 = coeftest(reg_Inequality_aversion_harvest, vcov_clust)
print(robust_test_inequality_harvest2)

#Check assumptions
par(mfrow=c(2,2))
plot(reg_Inequality_aversion_harvest, which=1:4)
check_model(reg_Inequality_aversion_harvest)

#1.Linearity
resettest(reg_Inequality_aversion_harvest)

#2.Homogeneity
bptest(reg_Inequality_aversion_harvest)

#3.Normality
shapiro.test(rstandard(reg_Inequality_aversion_harvest))

#4.Independence
durbinWatsonTest(reg_Inequality_aversion_harvest)

#5.Outliers
par(mfrow=c(1,1))
plot(reg_Inequality_aversion_harvest,5)

#6.Multicollinearity
vif(reg_Inequality_aversion_harvest)

#Visualize instead 
data[data[, "Harvest_restriction"] == 0, "Harvest_restriction"] ="Baseline"

# Basic Interaction Plot
interaction.plot(x.factor = data$Inequality_aversion,
                 trace.factor = data$Harvest_restriction, 
                 response = data$Average_harvest_threshold, fun = median,xlab="Inequality Aversion",
                 ylab="Average harvest", trace.label="Condition",col=c("purple","#D95F02","steelblue"),legend=T, xpd = T, lty = 1,  # Set line type to solid
                 lwd = 1)
axis(1, at = c(1, 2, 3,4,5), labels = c("1", "2", "3","4","5"))
axis(2)

########################Hypothesis 4############################################

#Calculate GINI for treatment with harvest before threshold but this is not a mean measure 
gini_coefficients = tapply(data$Harvest_threshold, data$Treatment, Gini)

#Calculate GINI for each group for harvest before threshold 
gini_coefficients_groups = tapply(data$Harvest_threshold, data$Session, Gini)

#Dataframe
gini_data = data %>%
  group_by(Session) %>%
  summarise(Gini_Coefficient = Gini(Harvest_threshold))

#Add this to the group dataset as a new column called GINI 
names(group)[names(group) == "Group_session"] = "Session"

Group_with_GINI= merge(group, gini_data, by = "Session", all.x = TRUE)

#Check normality
normality_test_GINI = lapply(group_names, function(grouping) shapiro.test(Group_with_GINI$Gini_Coefficient[Group_with_GINI$Treatment == grouping]))
names(normality_test_GINI) = group_names
print(normality_test_GINI)

#One-sided test 
# Perform Wilcoxon rank-sum test for two treatments
group_baseline =Group_with_GINI$Gini_Coefficient[Group_with_GINI$Treatment == "Baseline"]
group_inequality=Group_with_GINI$Gini_Coefficient[Group_with_GINI$Treatment == "Inequality"]

wilcox.test(group_inequality,group_baseline, data = Group_with_GINI, alternative="greater")

#Median
median(group_baseline) 
median(group_inequality) 

#Boxplots
Gini=ggplot(Group_with_GINI,aes(x=Treatment,y=Gini_Coefficient))+geom_boxplot()+ theme_apa()+labs(y="Gini", x="Treatment",title="All groups")+geom_boxplot(fill =c("steelblue","#D95F02"))

####################################only groups that crossed the threshold####################################

#Only threshold crossed
data_threshold=data %>%
  filter(Threshold_crossed=="1")

group_threshold=group %>%
  filter(Threshold_crossed=="1")

names(group_threshold)[names(group_threshold) == "Group_session"] = "Session"

#Calculate GINI for each group for harvest before threshold 
gini_data_threshold = data_threshold %>%
  group_by(Session) %>%
  summarise(Gini_Coefficient_harvest = Gini(Harvest_threshold))

Group_with_GINI_thres= merge(group_threshold, c(gini_data_threshold), by = "Session", all.x = TRUE)

normality_test_GINI_thres = lapply(group_names, function(grouping) shapiro.test(Group_with_GINI_thres$Gini_Coefficient_harvest[Group_with_GINI_thres$Treatment == grouping]))
names(normality_test_GINI_thres) = group_names
print(normality_test_GINI_thres)

#One-sided test 
# Perform Wilcoxon rank-sum test for two treatments
group_baseline_thres =Group_with_GINI_thres$Gini_Coefficient_harvest[Group_with_GINI_thres$Treatment == "Baseline"]
group_inequality_thres=Group_with_GINI_thres$Gini_Coefficient_harvest[Group_with_GINI_thres$Treatment == "Inequality"]

t.test(group_inequality_thres,group_baseline_thres, data = Group_with_GINI_thres, alternative="greater")

#Boxplots
Gini_threshold=ggplot(Group_with_GINI_thres,aes(x=Treatment,y=Gini_Coefficient_harvest))+geom_boxplot()+ theme_apa()+labs(y="Gini", x="Treatment",title="Groups that crossed threshold") +geom_boxplot(fill =c("steelblue","#D95F02"))

#Two boxplots next to another 
# Arrange the boxplots side by side with a separation line
grid.arrange(Gini,Gini_threshold, ncol = 2, widths = c(1, 1), heights = c(1, 1))

#########################################Hypothesis 5##############################################

#Check normality 
normality_communication = lapply(group_names, function(grouping) shapiro.test(group$Communication_combined[group$Treatment== grouping]))
names(normality_communication) = group_names
print(normality_communication)
#both not normally distributed 

#visualization with boxplot
ggplot(group,aes(x=Treatment,y=Communication_combined))+geom_boxplot()+labs(y="Communication index", x="Treatment")+geom_boxplot(fill =c("steelblue","#D95F02"))+theme_apa()

#One-sided
# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Communication_combined~ Treatment, data = group, alternative ="greater")

#Median by condition 
median_communication = group %>%
  group_by(Treatment) %>%
  summarise(Median = median(Communication_combined))

#####Relation to Cooperation and Threshold Crossings########

group_communicating= group %>%
  dplyr::select(Communicating,Cooperation_combined,Complex_dynamics_average,Error_rate_average)
group_communicating$Communicating=as.factor(group_communicating$Communicating)

#Check normality 
normality_cooperation = lapply(group_names, function(grouping) shapiro.test(group$Cooperation_combined[group$Treatment== grouping]))
names(normality_cooperation) = group_names
print(normality_cooperation)
#both not normally distributed 

# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Cooperation_combined~ Communicating, data = group_communicating,alternative="less")

#Median by condition 
median_cooperation = group %>%
  group_by(Communicating) %>%
  summarise(Median = median(Cooperation_combined))

#Chi-square test for threshold
fisher.test(group$Communicating,group$Threshold_crossed)

##Visualizations 
data_counts = table(group$Communicating,group$Threshold_crossed)
data_percentages = prop.table(data_counts, margin = 1) * 100

# Convert percentages to data frame and change column name
data_percentages = as.data.frame(data_percentages)
colnames(data_percentages)[2] = "Threshold"

data_percentages = data_percentages %>%
  mutate(Threshold = case_when(
    Threshold == 0 ~ "Not Crossed",
    Threshold == 1 ~ "Crossed"))

#Create a stacked bar chart with percentages
bar_chart_threshold_comm = ggplot(data_percentages, aes(x = Var1, y = Freq, fill = Threshold)) +
  geom_bar(stat = "identity", position = position_fill(reverse = F)) + theme_apa() +
  labs( x = "Group", y = "Percentage") +scale_fill_manual(values=c("#D95F02","steelblue"))+scale_x_discrete(labels = c("Non-communicating","Communicating"))

#visualization with boxplot
boxplot_comm=ggplot(group_communicating,aes(x=Communicating,y=Cooperation_combined))+geom_boxplot()+ theme_apa()+labs(y="Cooperation index", x="Group")+geom_boxplot(fill =c("steelblue","#D95F02"))+scale_x_discrete(labels = c("Non-communicating","Communicating"))

#Arrange next to another 
grid.arrange(boxplot_comm,bar_chart_threshold_comm, ncol = 2, widths = c(1, 1), heights = c(1, 1))

######################Cooperation and Threshold Crossing##############################################

model_threshold=glm(Threshold_crossed~Cooperation_combined+WEIRD_index+Average_age+Gender_composition, data = group,family = binomial)
summary(model_threshold)

## odds ratios only
exp(coefficients(model_threshold))

## odds ratios and 95% CI
exp(cbind(OR = coef(model_threshold), confint(model_threshold)))

#Model accuracy 
#Visualize with boxplot 
group$Threshold_crossed=as.factor(group$Threshold_crossed)
ggplot(group,aes(x=Threshold_crossed,y=Cooperation_combined))+geom_boxplot()+ theme_apa()+labs(y="Cooperation Score", x="Threshold Crossing")

#Asssumptions 
#Predict probabilities 
probabilities = predict(model_threshold, type = "response")
predicted.classes = ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

#Linearity
group_threshold= group %>%
  dplyr::select(Threshold_crossed,Cooperation_combined) 

# Select only numeric predictors
group_threshold_numeric = group_threshold %>%
  dplyr::select_if(is.numeric) 
predictors = colnames(group_threshold_numeric)
# Bind the logit and tidying the data for plot
group_threshold= group_threshold_numeric %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

#Plot 
ggplot(group_threshold, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#Influential Values
plot(model_threshold, which = 4, id.n = 3)

model.data = augment(model_threshold) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = group$Threshold_crossed), alpha = .5) +
  theme_apa()

vif(model_threshold)

#Pseudo R-squared
#calculate McFadden's R-squared for model
pR2(model_threshold)['McFadden']

############################################################Survey Variables###################################################################

#Descriptive Statistics of Survey Questions

survey=data[ , c(200:268)]  

survey1= subset(survey, select= -c(Nationality,Communication_medium_video,Expecting_long,Expecting_end,Shyness,Language,No_need,Other,Cooperation_self_without_score,Reverse_self.efficacy_Q1))

survey2= lapply(survey1, as.integer) # make integrer 

stargazer(survey1, type = "text", title="Descriptive statistics", digits=2, out="tablesurvey.html",
          covariate.labels=c("Age","Gender","Nationality",
                             "Education","Study Field Economics","How many people from your group did you know from before the start of the experiment",
                             "How many similar experiments have you been part of before","How many similar computer-based experiments have you been part of before","Importance of Instructions",
                             "Importance of Group Discussions","Our group made an agreement", "Our group communicated well","Group communication self-reported","Our group had a leader","It was important for myself to avoid the threshold","It was important for my group to avoid the threshold","Resource Dynamics Complexity","Our group managed to cooperate"," Our group shared the resource","Our group worked together","Group cooperation score","Group cooperation self-rated","Group identification","Trust in group","Importance of Fairness","The group was fair","The harvest restriction was fair","Group shared the harvest equally","I considered past decision of group members","I considered the impact of my choices on other group members","My own monetary gain was more important to me","I believe my action could affect the outcome of the game","I believe my contribution was just a drop in the ocean and insignificant","I believe there was little point in cooperation because others would not", "Self-efficacy score", "I believe that our group could, through joint effort, conserve the resource sustainably", "Outrage", "Guilt","Fear","Envy","Pride","Compassion","Contribution to Budget","Low-income","I only trust people I have known for a while","There are only few people I can trust completely","I think of myself as someone who can be trusted","General trust score","I prioritize individual over communal benefit","I believe that differences in people's living standards should be small","My actions reflect my beliefs regardless of social pressures"))

#Check Normality for Survey Questions
###Individual level
#Survey data with treatment 
survey3=data[ , c(7:268)]  
survey3= subset(survey3, select= -c(2:193))

#Only select numeric variables from dataset
survey_numeric=survey3 %>% dplyr::select(Treatment,where(is.numeric))

#Only Inequality 
survey_inequality = survey_numeric[survey_numeric$Treatment=="Inequality",]
survey_inequality = survey_inequality %>% dplyr::select(-Treatment)

#Only Baseline
survey_baseline = survey_numeric[survey_numeric$Treatment=="Baseline",]
survey_baseline = survey_baseline %>% dplyr::select(-Treatment,-Fair_harvest_restriction)

#Normality test for Survey Variables inequality 
apply(survey_inequality,2,shapiro.test)

#Normality test for Survey Variables baseline  
apply(survey_baseline,2,shapiro.test)

#Comparison of proportion and averages across treatment on Survey variables 
#Individual-level

Survey_table = CreateTableOne(data=survey3,vars = c("Age","Gender","WEIRD_country", "Education","Study_field_economics","Familarity_others","Familarity_experiments","Familarity_computer","Extra rounds","Instruction_feedback","Discussion_important","Group_agreed","Group_communication","Communication_self","Group_leader","Avoid_threshold","Group_avoid_threshold","Complex_dynamics","Managed_cooperation","Group_shared","Group_worked","Group_cooperation_score","Cooperation_self","Sessionentification","Trust_group","Fairness","Fair_group","Fair_shared_harvest","Considered_past","Considered_impact","Considered_money","Self_efficacy_Q1","Self_efficacy_Q2","Self_efficacy_Q3","Self_efficacy","Group_efficacy","Outrage","Guilt","Fear","Envy","Pride","Compassion","Budget","Income","Trust_1","Trust_2","Trust_3","General_trust","Individualism","Inequality","Autonomy"),
                               strata = "Treatment",factorVars = c("WEIRD_country","Study_field_economics"))

print(Survey_table, showAllLevels = T, exact= c("Gender","Education","Income"),nonnormal = c("Age","Familarity_others","Familarity_experiments","Familarity_computer","Instruction_feedback","Discussion_important","Group_agreed","Group_communication","Communication_self","Group_leader","Avoid_threshold","Group_avoid_threshold","Complex_dynamics","Managed_cooperation","Group_shared","Group_worked","Group_cooperation_score","Cooperation_self","Sessionentification","Trust_group","Fairness","Fair_group","Fair_shared_harvest","Considered_past","Considered_impact","Considered_money","Self_efficacy_Q1","Self-efficacy_Q2","Self_efficacy_Q3","Self_efficacy","Group_efficacy","Outrage","Guilt","Fear","Envy","Pride","Compassion","Budget","Trust_1","Trust_2","Trust_3","General_trust","Individualism","Inequality","Autonomy"))

####Double-check using Man-Whitney U test for continuous variables 

#exclude categorical variables 
survey_numeric1=subset(survey_numeric, select= -c(Fair_harvest_restriction))

Map_func_pvalues = function(survey_numeric1) {
  tmp = split(survey_numeric1, survey_numeric1$Treatment)
  stack(Map(function(x, y) wilcox.test(x, y, exact = FALSE)$p.value, tmp[[1]][-1], tmp[[2]][-1]))
}
p_values = Map_func_pvalues(survey_numeric1)

Map_func_statistic = function(survey_numeric1) {
  tmp = split(survey_numeric1, survey_numeric1$Treatment)
  stack(Map(function(x, y) wilcox.test(x, y, exact = FALSE)$statistic, tmp[[1]][-1], tmp[[2]][-1]))
}
test_scores = Map_func_statistic(survey_numeric1)

#Mann-Whitney Tests for all Survey variables that are continuous as all variables are non-normally distributed 

merge(p_values, test_scores, by = "ind", suffix = c("_pvalue", "_wilcox_statistic"))

#Only significant differences at 0.05 significance 
Table_survey_significant=CreateTableOne(data=survey_numeric,vars=c("Discussion_important","Trust_group","Group_efficacy","Envy","Compassion"),strata="Treatment")
print(Table_survey_significant,nonnormal=c("Discussion_important","Trust_group","Group_efficacy","Envy","Compassion" ))

#Mann-Whitney Test for multiple survey variables that are significant 
significant_survey=survey3[,c("Treatment","Discussion_important","Trust_group","Group_efficacy","Envy","Compassion")]

Map_func_pvalues = function(significant_survey) {
  tmp = split(significant_survey, significant_survey$Treatment)
  stack(Map(function(x, y) wilcox.test(x, y, exact = FALSE)$p.value, tmp[[1]][-1], tmp[[2]][-1]))
}
p_values = Map_func_pvalues(significant_survey)

Map_func_statistic = function(significant_survey) {
  tmp = split(significant_survey, significant_survey$Treatment)
  stack(Map(function(x, y) wilcox.test(x, y, exact = FALSE)$statistic, tmp[[1]][-1], tmp[[2]][-1]))
}
test_scores = Map_func_statistic(significant_survey)

merge(p_values, test_scores, by = "ind", suffix = c("_pvalue", "_wilcox_statistic"))

##############################################Group Level#####################################################################

#Check normality 
survey_group=group[ , c(5:203)]  
survey_group= subset(survey_group, select= -c(2:133))

#Only select numeric variables from dataset
survey_numeric_group=survey_group %>% dplyr::select(Treatment,where(is.numeric))
survey_numeric_group=subset(survey_numeric_group, select= -c(Group_familarity,Communication_medium_video,Expecting_end,Expecting_long,Group_agreement,Cheating,Shyness,Language,No_need,Cooperation_self_without_score_average,Reverse_selfefficacy_Q1_average))

#Split into inequality and baseline dataset 
#Inequality
survey_inequality_group= survey_numeric_group[survey_numeric_group$Treatment=="Inequality",]
survey_inequality_group= survey_inequality_group %>% dplyr::select(-Treatment)

#Baseline
survey_baseline_group= survey_numeric_group[survey_numeric_group$Treatment=="Baseline",]
survey_baseline_group= survey_baseline_group %>% dplyr::select(-Treatment,-Fair_random_average)

#Normality test for Survey Variables Inequality
apply(survey_inequality_group,2,shapiro.test)

#Normality test for Survey Variables Baseline 
apply(survey_baseline_group,2,shapiro.test)

names(survey_group)
Survey_table_group = CreateTableOne(data=survey_group,vars = c("Average_age","Gender_composition","WEIRD_index","Group_economics","Group_familarity","Experiments_familarity_average","Computer_experiments_average","Communication_medium_video","Expecting_long","Expecting_end","Instr_feedback_average","Discussion_important_average","Trusted_group_average","Group_agreed_average","Group_communication_average","Communication_self_average","Group_agreement","Cheating","Communication_experimenter","Communication_combined","Group_leader_average","Avoid_threshold_average","Group_avoid_threshold_average","Complex_dynamics_average","Group_managed_average","Group_shared_average","Group_worked_average","Group_cooperation_score_average","Cooperation_self_average","Cooperation_experimenter","Cooperation_combined","Group_relationship_average","Fairness_average","Fair_group_average","Fair_shared_harvest_average","Considered_past_average","Considered_impact_average","Considered_money_average","Groupefficacy_average","Outrage_average","Guilt_average","Fear_average","Envy_average","Pride_average","Compassion_average","Budget_average","Low_income_composition","Trust_Q1_average","Trust_Q2_average","Trust_Q3_average","General_trust_average","Individualism_average","Inequality_average","Autonomy_average"),
                                     strata = "Treatment",factorVars = c("Group_familarity","Communication_medium_video","Expecting_end","Expecting_long","Group_agreement","Cheating"))

print(Survey_table_group, showAllLevels = T, exact= c("Group_familarity","Cheating","Expecting_end"),nonnormal = c("Average_age","Gender_composition","WEIRD_index","Group_economics","Group_familarity","Computer_experiments_average","Instr_feedback_average","Group_agreed_average","Group_communication_average","Communication_self_average","Communication_experimenter","Trusted_group_average","Communication_combined","Complex_dynamics_average","Group_managed_average","Group_shared_average","Group_worked_average","Group_cooperation_score_average","Cooperation_self_average","Cooperation_experimenter","Cooperation_combined","Group_relationship_average","Trust_group_average","Fair_group_average","Fair_shared_harvest_average","Reverse_selfefficacy_Q1_average","Self-efficacy_Q2_average","Self-efficacy","Guilt_average","Fear_average","Envy_average","Pride_average","Low_income_composition"))

#only significant at 0.05%
Survey_table_group_sig1 = CreateTableOne(data=survey_group,vars = c("Groupefficacy_average","Envy_average"),strata="Treatment")
print(Survey_table_group_sig1,nonnormal=c("Envy_average"))

#only significant at 0.1 %
Survey_table_group_sig2 = CreateTableOne(data=survey_group,vars = c("Groupefficacy_average","Envy_average","Discussion_important_average","Trusted_group_average"),strata="Treatment")
print(Survey_table_group_sig2,nonnormal=c("Envy_average","Trusted_group_average","Discussion_important_average","WEIRD_index"))

#Group efficacy
var.test(group$Groupefficacy_average~group$Treatment, group, 
         alternative = "two.sided")
t.test(group$Groupefficacy_average~group$Treatment,var.equal = TRUE)

#Envy
wilcox.test(group$Envy_average~group$Treatment)

#Discusssion Importance 
wilcox.test(group$Discussion_important_average~group$Treatment)
#Trust in group
wilcox.test(group$Trusted_group_average~group$Treatment)

#Export
export = print(Survey_table_group, showAllLevels = T, exact= c("Group_familarity","Cheating","Expecting_end"),nonnormal = c("Average_age","Gender_composition","WEIRD_index","Group_economics","Group_familarity","Computer_experiments_average","Instr_feedback_average","Group_agreed_average","Group_communication_average","Communication_self_average","Communication_experimenter","Trusted_group_average","Communication_combined","Complex_dynamics_average","Group_managed_average","Group_shared_average","Group_worked_average","Group_cooperation_score_average","Cooperation_self_average","Cooperation_experimenter","Cooperation_combined","Group_relationship_average","Trust_group_average","Fair_group_average","Fair_shared_harvest_average","Reverse_selfefficacy_Q1_average","Self-efficacy_Q2_average","Self-efficacy","Guilt_average","Fear_average","Envy_average","Pride_average","Low_income_composition"), quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(export, file = "Survey_differences_group.csv")

#################################Group efficacy, Discussion, Trust############################################

model_group_efficacy = lm(Groupefficacy_average~ Treatment+Cooperation_combined+WEIRD_index+Average_age, data = group)
summary(model_group_efficacy)

# Obtain the coefficients with robust standard errors
robust_se_model_efficacy = coeftest(model_group_efficacy, vcov = sandwich)

# Print the results
print(robust_se_model_efficacy)

#Check model assumptions 
#Check Assumptions 
par(mfrow=c(2,2))
plot(model_group_efficacy, which=1:4)
check_model(model_group_efficacy)

#1.Linearity
resettest(model_group_efficacy)

#2.Homogeneity
bptest(model_group_efficacy)

#3.Normality
shapiro.test(rstandard(model_group_efficacy))

#4.Independence
durbinWatsonTest(model_group_efficacy)

#5.Outliers
par(mfrow=c(1,1))
plot(model_group_efficacy,5)

#6.Multicollinearity
vif(model_group_efficacy)

#Run a regression controlling for group effects to understand effect of harvest restriction 

model_efficacy_2 = lm(Group_efficacy~ Harvest_restriction+Cooperation_self+WEIRD_country+Age+Gender.composition, data = Data_inequality)
summary(model_efficacy_2)

#control for group effects and robust SE
library(sandwich)
vcov_clust = vcovCL(model_efficacy_2, cluster = ~ Session)
library(lmtest)
robust_test_efficacy = coeftest(model_efficacy_2, vcov_clust)
print(robust_test_efficacy)

#Check model assumptions 
#Check Assumptions 
par(mfrow=c(2,2))
plot(model_efficacy_2, which=1:4)
check_model(model_efficacy_2)

#1.Linearity
resettest(model_efficacy_2)
 
#2.Homogeneity
bptest(model_efficacy_2)

#3.Normality
shapiro.test(rstandard(model_efficacy_2))

#4.Independence
durbinWatsonTest(model_efficacy_2)

#5.Outliers
par(mfrow=c(1,1))
plot(model_efficacy_2,5)

#6.Multicollinearity
vif(model_efficacy_2)

####Importance Discussions
model_discussions = lm(Discussion_important_average~ Treatment+Cooperation_combined+WEIRD_index+Average_age, data = group)
summary(model_discussions)

#Obtain the coefficients with robust standard errors
robust_se_model_discussions = coeftest(model_discussions, vcov = sandwich)

#Print the results
print(robust_se_model_discussions)

#Check model assumptions 
#Check Assumptions 
par(mfrow=c(2,2))
plot(model_discussions, which=1:4)
check_model(model_discussions)

#1.Linearity
resettest(model_discussions)

#2.Homogeneity
bptest(model_discussions)

#3.Normality
shapiro.test(rstandard(model_discussions))

#4.Independence
durbinWatsonTest(model_discussions)

#5.Outliers
par(mfrow=c(1,1))
plot(model_discussions,5)

#6.Multicollinearity
vif(model_discussions)

##Trust in group ###

model_trust = lm(Trusted_group_average~ Treatment+Cooperation_combined+Average_age+WEIRD_index, data = group)
summary(model_trust)

# Obtain the coefficients with robust standard errors
robust_se_model_trust = coeftest(model_trust, vcov = sandwich)

# Print the results
print(robust_se_model_trust)

#Check model assumptions 
#Check Assumptions 
par(mfrow=c(2,2))
plot(model_trust, which=1:4)
check_model(model_trust)

#1.Linearity
resettest(model_trust)

#2.Homogeneity
bptest(model_trust)

#3.Normality
shapiro.test(rstandard(model_trust))

#4.Independence
durbinWatsonTest(model_trust)

#5.Outliers
par(mfrow=c(1,1))
plot(model_trust,5)

#6.Multicollinearity
vif(model_trust)

#########################################Emotions################################################

emotions_individual= data %>%
  dplyr::select(Treatment,Outrage,Guilt,Fear,Envy,Pride,Compassion)

emotions_group= group %>%
  dplyr::select(Treatment,Outrage_average,Guilt_average,Fear_average,Envy_average,Pride_average,Compassion_average,)

colnames(emotions_group)=c("Treatment","Outrage","Guilt","Fear","Envy","Pride","Compassion")

#Visual Boxplots
emotions_long = emotions_group %>%
  pivot_longer(-Treatment, names_to = "Emotion", values_to = "Values")

my_colors = c("steelblue", "#7570B3", "#D95F02","#1B9E77","#66A61E","#E6AB02")

ggplot(emotions_long, aes(x = Emotion, y = Values)) +
  geom_boxplot(fill = my_colors) +
  ylab("Average self-rating (group-level)") + theme_apa() 

kw_results=kruskal.test(Values ~ Emotion, data = emotions_long)

#Post-hoc test 
dunn_result = dunn.test(emotions_long$Values, emotions_long$Emotion, method = "bonferroni")

#Visualization
averages = aggregate(Values ~ Emotion + Treatment, data = emotions_long, FUN = mean)

ggplot(averages, aes(x = Emotion, y = Values, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Average self-rating (group-level)") +
  xlab("Emotions") +
  scale_fill_manual(values = c("steelblue","#D95F02")) +
  theme_apa()

#check for significant differences 
# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Compassion_average ~ Treatment, data = group)

# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Envy_average ~ Treatment, data = group)

# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Envy_average ~ Treatment, data = group,alternative="less")

# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Fear_average ~ Treatment, data = group)

# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Guilt_average ~ Treatment, data = group)

# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Outrage_average ~ Treatment, data = group)

# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Pride_average ~ Treatment, data = group)

############################################
#with GINI 
model_envy_Gini = lm(Envy_average~ Treatment+Gini_Coefficient+WEIRD_index +Gender_composition+Average_age, data = Group_with_GINI)
summary(model_envy_Gini)

#Check assumptions
par(mfrow=c(2,2))
plot(model_envy_Gini,which=1:4)
check_model(model_envy_Gini)

#1.Linearity
resettest(model_envy_Gini)

#2.Homogeneity
bptest(model_envy_Gini)

#3.Normality
shapiro.test(rstandard(model_envy_Gini))

#4.Independence
durbinWatsonTest(model_envy_Gini)

#5.Outliers
par(mfrow=c(1,1))
plot(model_envy_Gini,5)

#6.Multicollinearity 
vif(model_envy_Gini)

#Robust SEs to account for heteroskedasticity 
#Gini envy model
coeftest(model_envy_Gini, vcov = vcovHC(model_envy_Gini, type="HC3"))

###Compare Emotions across groups that cross or do not cross threshold 

emotions_threshold= group%>%
  dplyr::select(Threshold_crossed,Outrage_average,Guilt_average,Fear_average,Envy_average,Pride_average,Compassion_average)
colnames(emotions_threshold)=c("Threshold","Outrage","Guilt","Fear","Envy","Pride","Compassion")

emotions_long_threshold = emotions_threshold%>%
  pivot_longer(-c(Threshold), names_to = "Emotion", values_to = "Values")

emotions_long_threshold$Threshold=as.factor(emotions_long_threshold$Threshold)

averages_threshold = aggregate(Values ~ Emotion + Threshold, data = emotions_long_threshold, FUN = mean)

attach(emotions_threshold)

#barplot 
ggplot(averages_threshold, aes(x = Emotion, y = Values, fill = Threshold)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Average self-rating (group-level)") +
  xlab("Emotions") +
  scale_fill_manual(values = c("steelblue","#D95F02"),labels = c("Not Crossed","Crossed")) +
  theme_apa()

#Check normality 
group_names4=unique(group$Threshold_crossed)

#Compassion
normality_test_compassion = lapply(group_names4, function(grouping) shapiro.test(emotions_threshold$Compassion[emotions_threshold$Threshold == grouping]))
names(normality_test_compassion) = group_names4
print(normality_test_compassion)

#Envy
normality_test_envy = lapply(group_names4, function(grouping) shapiro.test(emotions_threshold$Envy[emotions_threshold$Threshold == grouping]))
names(normality_test_envy) = group_names4
print(normality_test_envy)

#Fear
normality_test_fear = lapply(group_names4, function(grouping) shapiro.test(emotions_threshold$Fear[emotions_threshold$Threshold == grouping]))
names(normality_test_fear) = group_names4
print(normality_test_fear)

#Outrage
normality_test_outrage = lapply(group_names4, function(grouping) shapiro.test(emotions_threshold$Outrage[emotions_threshold$Threshold == grouping]))
names(normality_test_outrage) = group_names4
print(normality_test_outrage)
#not normal in not crossed group 

#Pride
normality_test_pride = lapply(group_names4, function(grouping) shapiro.test(emotions_threshold$Pride[emotions_threshold$Threshold == grouping]))
names(normality_test_pride) = group_names4
print(normality_test_pride)

#Guilt 
normality_test_guilt = lapply(group_names4, function(grouping) shapiro.test(emotions_threshold$Outrage[emotions_threshold$Threshold == grouping]))
names(normality_test_guilt) = group_names4
print(normality_test_guilt)

# Perform t-test test for two treatments
var.test(Compassion ~ Threshold, group, 
         alternative = "two.sided")
t.test(Compassion ~ Threshold, data = emotions_threshold,var.equal=F)

# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Envy ~ Threshold, data = emotions_threshold)

# Perform t-test test for two treatments
var.test(Fear ~ Threshold, group, 
         alternative = "two.sided")
t.test(Fear ~ Threshold, data = emotions_threshold,var.equal=F)

# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Guilt~ Threshold, data = emotions_threshold)

# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Outrage ~ Threshold, data = emotions_threshold)

# Perform Wilcoxon rank-sum test for two treatments
wilcox.test(Pride~ Threshold, data = emotions_threshold)

##Table one
Table_one_emotion=CreateTableOne(vars= c("Compassion","Envy", "Fear", "Guilt", "Outrage", "Pride"),strata="Threshold", data=emotions_threshold)
print(Table_one_emotion,nonnormal=c("Envy","Guilt","Outrage","Pride"))

###Only threshold crossed
emotions_only_crossed= emotions_threshold  %>%
  filter (Threshold ==1)

##################################################Exploration Communication#################

#Communicating vs. non-communicating groups - differences on Survey variables 
#Check normality 
survey_group=group[ , c(5:203)]  
survey_group2= subset(survey_group, select= -c(9:133))

#Only select numeric variables from dataset
survey_group2_numeric=subset(survey_group2, select= -c(Depletion,Threshold_crossed,Rounds_before_threshold,Harvest_before_threshold,Group_average_before_threshold,Group_total_harvest,Computer_experiments_average,Communication_medium_video,Expecting_end,Expecting_long,Instr_feedback_average,Discussion_important_average,Group_agreed_average,Group_communication_average,Communication_self_average,Group_agreement,Cheating,Communication_experimenter,Communication_combined,Group_leader_average,Avoid_threshold_average, Group_avoid_threshold_average,Shyness,Language,No_need,Group_managed_average,Group_shared_average,Group_worked_average,Group_cooperation_score_average,Cooperation_self_average,Cooperation_experimenter,Cooperation_self_without_score_average,Fair_random_average, Fair_shared_harvest_average,Considered_past_average,Considered_impact_average,Considered_money_average,Action_effect_belief.average,Selfefficacy_Q2_average,Selfefficacy_Q3_average,Self_efficacy_average,Reverse_selfefficacy_Q1_average,Trust_Q1_average,Trust_Q2_average,Trust_Q3_average,Individualism_average))

#Split into communicating and non-communicating 
#Communicating
survey_communicating= survey_group2_numeric[survey_group2_numeric$Communicating==1,]
survey_communicating= subset(survey_communicating,select=-c(Treatment,Communicating))

#Non-communicating
survey_non_communicating= survey_group2_numeric[survey_group2_numeric$Communicating==0,]
survey_non_communicating= subset(survey_non_communicating,select=-c(Treatment,Communicating))

#Normality test for Survey Variables Communicating
lapply(survey_communicating,shapiro.test)

#Normality test for Survey Variables Non-communicating 
lapply(survey_non_communicating,shapiro.test)

names(survey_group)

Survey_table_group = CreateTableOne(data=survey_group2,vars = c("Treatment","Inequality_aversion_average","Threshold_crossed","Depletion","Average_age","Gender_composition","WEIRD_index", "Average_education","Group_economics","Group_familarity","Experiments_familarity_average","Trusted_group_average","Complex_dynamics_average","Cooperation_combined","Group_relationship_average","Trust_group_average","Fairness_average","Fair_group_average","Groupefficacy_average","Outrage_average","Guilt_average","Fear_average","Envy_average","Pride_average","Compassion_average","Budget_average","Low_income_composition","General_trust_average","Inequality_average","Autonomy_average"),
                                     strata = "Communicating",factorVars = c("Treatment","Group_familarity","Threshold_crossed","Depletion"))

print(Survey_table_group, showAllLevels = T, exact= c("Group_familarity","Threshold_crossed","Depletion"),nonnormal = c("Groupefficacy_average","Average_age","Inequality_aversion_average","Gender_composition","WEIRD_index","Group_economics","Group_familarity","Trusted_group_average","Complex_dynamics_average","Cooperation_combined","Group_relationship_average","Trust_group_average","Fair_group_average","Guilt_average","Outrage_average","Fear_average","Envy_average","Pride_average","Low_income_composition"))

#only significant at 0.05%

Survey_table_group_sig1 = CreateTableOne(data=survey_group,vars = c("Threshold_crossed","Error_rate_average","Depletion","Trusted_group_average","Cooperation_combined","Group_relationship_average","Groupefficacy_average","Fairness_average","Fair_group_average","Pride_average","Compassion_average"),factorVars = c("Threshold_crossed","Depletion"),strata="Communicating")
print(Survey_table_group_sig1,showAllLevels = T,exact= c("Threshold_crossed","Depletion"),nonnormal=c("Error_rate_average","Groupefficacy_average","Trusted_group_average","Cooperation_combined","Group_relationship_average","Groupefficacy_average","Fairness_average","Fair_group_average","Pride_average"))

#only significant at 0.1 %
Survey_table_group_sig2 = CreateTableOne(data=survey_group,vars = c("Inequality_aversion_average","Outrage_average","General_trust_average","Threshold_crossed","Depletion","Trusted_group_average","Cooperation_combined","Group_relationship_average","Groupefficacy_average","Fairness_average","Fair_group_average","Pride_average","Compassion_average"),factorVars = c("Threshold_crossed","Depletion"),strata="Communicating")
print(Survey_table_group_sig2,showAllLevels = T,exact= c("Threshold_crossed","Depletion"),nonnormal=c("Error_rate_average","Inequality_aversion_average","Outrage_average","General_trust_average","Groupefficacy_average","Trusted_group_average","Cooperation_combined","Group_relationship_average","Groupefficacy_average","Fairness_average","Fair_group_average","Pride_average"))
