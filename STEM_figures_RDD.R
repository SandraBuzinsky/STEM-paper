
# WOMEN IN STEM PAPER ------- 
# CREATE RDD FIGURES 



#load data -------------------------------------------

STEM1 <- read.dta13(file.path(project_file_path, "raw_PAE_full19.dta"))



#create  variables for analysis -------------------------------



#make logical version of sex
STEM1$female <- as.logical(STEM1$sex)

#create Y to represent population that has stated their program preference
STEM1$Y <- ifelse(STEM1$preferred_program != "", 1, 0)
 
#gen dummy for whether student has initial law preference
STEM1$prefers_law <- ifelse(STEM1$preferred_program == "LAW", 1, 0)

#gen dummy if the student registers in engineering (outcome var)
STEM1$registered_engineering <- ifelse(STEM1$reg_program == "Ing. Industrial y de Sistemas" | STEM1$reg_program == "Ing. Civil" | STEM1$reg_program == "Ing. Mecánico Eléctrica", 1, 0)

#gen a math score normalized at 600 (for eng), verbal normalized at 500 (for law)
STEM1$maths <- STEM1$math_score - 600
STEM1$verbals <- STEM1$verbal_score -500

#students who have initial non-engineering preferences:
STEM1$NP <- ifelse(STEM1$prefers_engineering != 1 & STEM1$Y== 1, 1, 0)

#student is from piura
STEM1$piura <- ifelse(STEM1$school_region == "PIURA" , 1, 0)

#treatment var 
STEM1$Treatment <- ifelse(STEM1$maths > 0 , 1 ,0)




#limit math score to [-40 , 40]

STEM <- STEM1 %>%
   dplyr::filter(maths <40 & maths >=-40) 
STEM$Treatment <- ifelse(STEM$maths >0, 1, 0)




#create diff filters to look at results by sex and program preferences ---
#STEM- currently filtered maths -40 to 40. 


#females only
STEM_female <- filter(STEM, sex==1)
STEM_female$Treatment <- ifelse(STEM_female$maths >0, 1, 0)

#males only
STEM_male <- filter(STEM, sex==0)
STEM_male$Treatment <- ifelse(STEM_male$maths >0 , 1 , 0)

#prefers eng
STEM_eng <- filter(STEM, prefers_engineering==1)
STEM_eng$Treatment <- ifelse(STEM_eng$maths >0  , 1, 0)
#doesn't prefer eng

STEM_noneng <- filter(STEM, NP==1)
STEM_noneng$Treatment <- ifelse(STEM_noneng$maths >0  , 1, 0)

#females, non eng
STEM_female_noneng <- filter(STEM, NP==1 & sex==1)
STEM_female_noneng$Treatment <- ifelse(STEM_female_noneng$maths >0  , 1, 0)


#males, non eng
STEM_males_noneng <- filter(STEM, NP==1, sex==0)
STEM_males_noneng$Treatment <- ifelse(STEM_males_noneng$maths >0  , 1, 0)


#females, eng
STEM_females_eng <- filter(STEM, prefers_engineering==1, sex==1)
STEM_females_eng$Treatment <- ifelse(STEM_females_eng$maths >0, 1, 0)


#males, eng
STEM_males_eng <- filter(STEM, prefers_engineering==1, sex==0)
STEM_males_eng$Treatment <- ifelse(STEM_males_eng$maths >0 , 1, 0)





# create outputs --------------------------------------------------------------





#FIGURE 1 - The effect of math threshold on UDEP registration


# 1a - full sample
 ggplot(STEM, aes(maths, registered_at_udep, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
  geom_vline(aes(xintercept=0), linetype = "longdash") + 
  #geom_point() +
  geom_smooth( method = "lm") +
  scale_color_brewer(NULL, type = 'qual', palette = 6) +
  xlab("PAE Math Score, Normalized at 600") +
  ylab("Registered at UDEP")  +
 ggtitle("(1a) Full Sample") +
  theme(plot.title = element_text(hjust = 0.5))


 #1b - females
 ggplot(STEM_female, aes(maths, registered_at_udep, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered at UDEP")  +
   ggtitle("(1b) Females Only") +
   theme(plot.title = element_text(hjust = 0.5))
 
 #1c - males
 ggplot(STEM_male, aes(maths, registered_at_udep, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered at UDEP")  +
   ggtitle("(1c) Males Only") +
   theme(plot.title = element_text(hjust = 0.5))
 
 #1d - Non-Eng preference
 ggplot(STEM_noneng, aes(maths, registered_at_udep, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered at UDEP")  +
   ggtitle("(1d) Non-Engineering Preferences") +
   theme(plot.title = element_text(hjust = 0.5))
 
 
 #1e - females, non-Eng preference
 ggplot(STEM_female_noneng, aes(maths, registered_at_udep, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered at UDEP")  +
   ggtitle("(1e) Females, Non-Engineering Preferences") +
   theme(plot.title = element_text(hjust = 0.5))
 
 
 #figure 2- Pre-Treatment Characteristics at 600-math threshold 
 
 #2a - SEX
 ggplot(STEM, aes(maths, sex, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Sex =1 if female")  +
   ggtitle("(2a) Sex") +
   theme(plot.title = element_text(hjust = 0.5))
 
 
 #2b - Verbal score
 ggplot(STEM, aes(maths, verbals, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm", formula = y ~ x) +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Verbal Score")  +
   ggtitle("(2b) Standardized Verbal Score") +
   theme(plot.title = element_text(hjust = 0.5))
 
#2c - students from piura region 
 ggplot(STEM, aes(maths, piura, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Student is from Piura")  +
   ggtitle("(2c) Individual is from Piura Region") +
   theme(plot.title = element_text(hjust = 0.5))
 

 #2d- test year
 ggplot(STEM, aes(maths, test_year, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Test Year")  +
   ggtitle("(2d) PAE Test Year") +
   theme(plot.title = element_text(hjust = 0.5))
 
 
 
 #2e- prefers engineering
 ggplot(STEM, aes(maths, prefers_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Prefers Engineering")  +
   ggtitle("(2e) Individual Prefers Engineering") +
   theme(plot.title = element_text(hjust = 0.5))
 
 
#figure 3-  same as figure two for those who don't prefer eng
 
#3a- SEX, non-eng
 ggplot(STEM_noneng, aes(maths, sex, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Sex =1 if female")  +
   ggtitle("(3a) Sex") +
   theme(plot.title = element_text(hjust = 0.5)) 
 
 #3b- verbals
 ggplot(STEM_noneng, aes(maths, verbals, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Verbal Score")  +
   ggtitle("(3b) Standardized Verbal Score") +
   theme(plot.title = element_text(hjust = 0.5))
 
 
 #3c - piura
 ggplot(STEM_noneng, aes(maths, piura, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Student is from Piura")  +
   ggtitle("(3c) Individual is from Piura Region") +
   theme(plot.title = element_text(hjust = 0.5))
 
 #3d- test year
 
 ggplot(STEM_noneng, aes(maths, test_year, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Test Year")  +
   ggtitle("(3d) PAE Test Year") +
   theme(plot.title = element_text(hjust = 0.5))
 
 
 #3e- prefers law
 
 ggplot(STEM_noneng, aes(maths, prefers_law, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm", formula = y ~ x) +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Prefers Law")  +
   ggtitle("(3e) Individual Prefers Law") +
   theme(plot.title = element_text(hjust = 0.5))
 
 
 
 #figure 5- Effect on enrolling in engineering
 
#all students
ggplot(STEM, aes(maths, registered_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
  geom_vline(aes(xintercept=0), linetype = "longdash") + 
  #geom_point() +
  geom_smooth( method = "lm") +
  scale_color_brewer(NULL, type = 'qual', palette = 6) +
  xlab("PAE Math Score, Normalized at 600") +
  ylab("Registered in Engineering")  +
  ggtitle("(5a) All Test Takers") +
  theme(plot.title = element_text(hjust = 0.5))


#5b prefers engineering
ggplot(STEM_eng, aes(maths, registered_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
  geom_vline(aes(xintercept=0), linetype = "longdash") + 
  #geom_point() +
  geom_smooth( method = "lm") +
  scale_color_brewer(NULL, type = 'qual', palette = 6) +
  xlab("PAE Math Score, Normalized at 600") +
  ylab("Registered in Engineering")  +
  ggtitle("(5b) Individuals who Prefer Engineering") +
  theme(plot.title = element_text(hjust = 0.5))


#5c- doesnt prefer eng
ggplot(STEM_noneng, aes(maths, registered_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
  geom_vline(aes(xintercept=0), linetype = "longdash") + 
  #geom_point() +
  geom_smooth( method = "lm") +
  scale_color_brewer(NULL, type = 'qual', palette = 6) +
  xlab("PAE Math Score, Normalized at 600") +
  ylab("Registered in Engineering")  +
  ggtitle("(5c) Individuals who do not Prefer Engineering") +
  theme(plot.title = element_text(hjust = 0.5))




#figure 6- effects by gender amoung non-eng preferences

#6a- male
ggplot(STEM_males_noneng, aes(maths, registered_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
  geom_vline(aes(xintercept=0), linetype = "longdash") + 
  #geom_point() +
  geom_smooth( method = "lm") +
  scale_color_brewer(NULL, type = 'qual', palette = 6) +
  xlab("PAE Math Score, Normalized at 600") +
  ylab("Registered in Engineering")  +
  ggtitle("(6a) Males Only") +
  theme(plot.title = element_text(hjust = 0.5))


#6b - females
ggplot(STEM_female_noneng, aes(maths, registered_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
  geom_vline(aes(xintercept=0), linetype = "longdash") + 
  #geom_point() +
  geom_smooth( method = "lm") +
  scale_color_brewer(NULL, type = 'qual', palette = 6) +
  xlab("PAE Math Score, Normalized at 600") +
  ylab("Registered in Engineering")  +
  ggtitle("(6b) Females Only") +
  theme(plot.title = element_text(hjust = 0.5))

#figure 7 - scatter plots of registered engineering - males vs females

#7a- Males 
ggplot(STEM_males_noneng, aes(maths, registered_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) +
geom_point() +
scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered in Engineering")  +
   ggtitle("(7a) Males Only") +
   theme(plot.title = element_text(hjust = 0.5))

#7b - females
ggplot(STEM_female_noneng, aes(maths, registered_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) +
   geom_point() +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered in Engineering")  +
   ggtitle("(7b) Females Only") +
   theme(plot.title = element_text(hjust = 0.5))


#figure 8 - no bandwidth , male female non eng

STEM1_males <- filter(STEM1, NP==1, sex==0)
STEM1_females <- filter(STEM1, NP==1 , sex==1)

#8a - males
ggplot(STEM1_males, aes(maths, registered_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
  geom_vline(aes(xintercept=0), linetype = "longdash") + 
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,4)) +
  scale_color_brewer(NULL, type = 'qual', palette = 6) +
  xlab("PAE Math Score, Normalized at 600") +
  ylab("Registered in Engineering")  +
  ggtitle("(8a) Males Only") +
  theme(plot.title = element_text(hjust = 0.5))

#8b- females
ggplot(STEM1_females, aes(maths, registered_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
  geom_vline(aes(xintercept=0), linetype = "longdash") + 
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,4)) +
  scale_color_brewer(NULL, type = 'qual', palette = 6) +
  xlab("PAE Math Score, Normalized at 600") +
  ylab("Registered in Engineering")  +
  ggtitle("(8b) Females Only") +
  theme(plot.title = element_text(hjust = 0.5))


#APPENDIX FIGURES- Same as above except including full sample, quadratic fit
###################################################################################

#create necessary filters based on original data.frame
########################################################

#females only
STEM1_female <- filter(STEM1, sex==1)
STEM1_female$Treatment <- ifelse(STEM1_female$maths >0, 1, 0)

#males only
STEM1_male <- filter(STEM1, sex==0)
STEM1_male$Treatment <- ifelse(STEM1_male$maths >0 , 1 , 0)

#prefers eng
STEM1_eng <- filter(STEM1, prefers_engineering==1)
STEM1_eng$Treatment <- ifelse(STEM1_eng$maths >0  , 1, 0)
#doesn't prefer eng

STEM1_noneng <- filter(STEM1, NP==1)
STEM1_noneng$Treatment <- ifelse(STEM1_noneng$maths >0  , 1, 0)

#females, non eng
STEM1_female_noneng <- filter(STEM1, NP==1 & sex==1)
STEM1_female_noneng$Treatment <- ifelse(STEM1_female_noneng$maths >0  , 1, 0)


#males, non eng
STEM1_males_noneng <- filter(STEM1, NP==1, sex==0)
STEM1_males_noneng$Treatment <- ifelse(STEM1_males_noneng$maths >0  , 1, 0)


#females, eng
STEM1_females_eng <- filter(STEM1, prefers_engineering==1, sex==1)
STEM1_females_eng$Treatment <- ifelse(STEM1_females_eng$maths >0, 1, 0)


#males, eng
STEM1_males_eng <- filter(STEM1, prefers_engineering==1, sex==0)
STEM1_males_eng$Treatment <- ifelse(STEM1_males_eng$maths >0 , 1, 0)


# appendix figures ---------------------------------------------------------------


#appendix 1

#1a
ggplot(STEM1, aes(maths, registered_at_udep, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( ) +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered at UDEP")  +
   ggtitle("(1a) Full Sample") +
   theme(plot.title = element_text(hjust = 0.5))


#1b
ggplot(STEM1_female, aes(maths, registered_at_udep, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth(method = "loess") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered at UDEP")  +
   ggtitle("(1b) Females Only") +
   theme(plot.title = element_text(hjust = 0.5))

#1c
ggplot(STEM1_male, aes(maths, registered_at_udep, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth() +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered at UDEP")  +
   ggtitle("(1c) Males Only") +
   theme(plot.title = element_text(hjust = 0.5))

#1d - Non-Eng preference
ggplot(STEM1_noneng, aes(maths, registered_at_udep, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth(method = "loess" ) +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered at UDEP")  +
   ggtitle("(1d) Non-Engineering Preferences") +
   theme(plot.title = element_text(hjust = 0.5))

#1e - females, non-Eng preference
ggplot(STEM1_female_noneng, aes(maths, registered_at_udep, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth(method = "loess" ) +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered at UDEP")  +
   ggtitle("(1e) Females, Non-Engineering Preferences") +
   theme(plot.title = element_text(hjust = 0.5))


#appendix figure 2

#2a - SEX
ggplot(STEM1, aes(maths, sex, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( ) +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Sex =1 if female")  +
   ggtitle("(2a) Sex") +
   theme(plot.title = element_text(hjust = 0.5))

#2b - Verbal score
ggplot(STEM1, aes(maths, verbals, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth() +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Verbal Score")  +
   ggtitle("(2b) Standardized Verbal Score") +
   theme(plot.title = element_text(hjust = 0.5))


#2c - students from piura region 
ggplot(STEM1, aes(maths, piura, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth() +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Student is from Piura")  +
   ggtitle("(2c) Individual is from Piura Region") +
   theme(plot.title = element_text(hjust = 0.5))

#2d- test year
ggplot(STEM1, aes(maths, test_year, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth() +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Test Year")  +
   ggtitle("(2d) PAE Test Year") +
   theme(plot.title = element_text(hjust = 0.5))

#2e- prefers engineering
ggplot(STEM1, aes(maths, prefers_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( ) +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Prefers Engineering")  +
   ggtitle("(2e) Individual Prefers Engineering") +
   theme(plot.title = element_text(hjust = 0.5))


#appendix figure 3-  same as figure two for those who don't prefer eng

#3a- SEX, non-eng
ggplot(STEM1_noneng, aes(maths, sex, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( ) +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Sex =1 if female")  +
   ggtitle("(3a) Sex") +
   theme(plot.title = element_text(hjust = 0.5)) 

#3b- verbals
ggplot(STEM1_noneng, aes(maths, verbals, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( ) +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Verbal Score")  +
   ggtitle("(3b) Standardized Verbal Score") +
   theme(plot.title = element_text(hjust = 0.5))


#3c - piura
ggplot(STEM1_noneng, aes(maths, piura, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "loess") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Student is from Piura")  +
   ggtitle("(3c) Individual is from Piura Region") +
   theme(plot.title = element_text(hjust = 0.5))

#3d- test year

ggplot(STEM1_noneng, aes(maths, test_year, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth(method = "loess") +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Test Year")  +
   ggtitle("(3d) PAE Test Year") +
   theme(plot.title = element_text(hjust = 0.5))


#3e- prefers law

ggplot(STEM1_noneng, aes(maths, prefers_law, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Prefers Law")  +
   ggtitle("(3e) Individual Prefers Law") +
   theme(plot.title = element_text(hjust = 0.5))




#appendix figure 5- Effect on enrolling in engineering

#all students
ggplot(STEM1, aes(maths, registered_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( ) +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered in Engineering")  +
   ggtitle("(4a) All Test Takers") +
   theme(plot.title = element_text(hjust = 0.5))


#5b prefers engineering
ggplot(STEM1_eng, aes(maths, registered_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth(method = "lm", formula = y ~ poly(x, 4) ) +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered in Engineering")  +
   ggtitle("(4b) Individuals who Prefer Engineering") +
   theme(plot.title = element_text(hjust = 0.5))


#5c- doesnt prefer eng
ggplot(STEM1_noneng, aes(maths, registered_engineering, colour= factor(Treatment, labels = c('Control', 'Treatment')))) + 
   geom_vline(aes(xintercept=0), linetype = "longdash") + 
   #geom_point() +
   geom_smooth( method = "lm", formula = y ~ poly(x, 2)) +
   scale_color_brewer(NULL, type = 'qual', palette = 6) +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Registered in Engineering")  +
   ggtitle("(4c) Individuals who do not Prefer Engineering") +
   theme(plot.title = element_text(hjust = 0.5))


#finally let's try to run a dc density test


ggplot(STEM1, aes(x = maths)) +
   geom_density(color="black", fill="lightblue", alpha = 0.6) +
   geom_vline(xintercept = 0) +
   geom_rug(aes(x = maths, y = 0), position = position_jitter(height = 0)) +
   theme_classic() +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Density")  +
   ggtitle("McCrary Test, Full Sample") +
   theme(plot.title = element_text(hjust = 0.5))



ggplot(STEM1_noneng, aes(x = maths)) +
   geom_density(color="black", fill="lightblue", alpha = 0.6) +
   geom_vline(xintercept = 0) +
   geom_rug(aes(x = maths, y = 0), position = position_jitter(height = 0)) +
   theme_classic() +
   xlab("PAE Math Score, Normalized at 600") +
   ylab("Density")  +
   ggtitle("McCrary Test, Students with Non-Engineering Preferences") +
   theme(plot.title = element_text(hjust = 0.5))




library(UsingR)
densityplot(~ maths,  data = STEM1)