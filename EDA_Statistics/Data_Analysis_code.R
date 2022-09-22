########## Project data analysis ##########

# Q0C: Load any libraries you'll need:
PacMan <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] 
if (length(new.pkg))         
  install.packages(new.pkg, dependencies = TRUE)     
sapply(pkg, library, character.only = TRUE)}
pkgs <- c("foreign", "ggplot2", "dplyr", "apa", "apaTables", "semTools", "car","coin","nortest",
"ez","multcomp","pastecs","rstatix", "reshape","additivityTests")
PacMan(pkgs)

# -----------------------------------------
# ------------- DESCRIPTIVES --------------
# -----------------------------------------

# Q0B: Load the INEGI dataset as a data frame: 
inegi<- read.csv("inegi.csv", header=TRUE)
# View(inegi)

summary(inegi$ing_tri)

# we check % of income to remove outliers 
inegi$porc_ing <- inegi$ing_tri / sum(inegi$ing_tri)

sorted_ingtri <- sort(inegi$porc_ing,decreasing = TRUE)
sorted_ingtri
# we get a plot of income distribution 
# cumporc_ing <- cumsum(sorted_ingtri)
# plot(cumporc_ing)

# we truncate data by quantile, 95th percentile
limit <- quantile(inegi$ing_tri, 0.05)
limit
upper_limit <- 50869.55 
lower_limit <- 4000 

inegi1 <- filter(inegi, ing_tri < upper_limit)
inegi1 <- filter(inegi1, ing_tri > lower_limit)
#View(inegi1)
#inegi <- inegi[inegi$Point < quantile(inegi$ing_tri, 0.999), ]
summary(inegi1$ing_tri)

# Histogram
IngHist <- ggplot(data=inegi1, aes(x=ing_tri)) +
  geom_histogram(position="identity",bins=20)+
  theme_classic()+
  labs(title="Histogram of Quarterly Income", 
       x="Quarterly Income", y="Frequency")
IngHist

# Histogram of age
AgeHist <- ggplot(data=inegi1, aes(x=edad)) +
  geom_histogram(position="identity",bins=20)+
  theme_classic()+
  labs(title="Histogram of Age", 
       x="Age", y="Frequency")
AgeHist


# we create factors for the relevant variables
# inegi1$sexo <- factor(inegi1$sexo)
inegi1$education <- factor(inegi1$education)
inegi1$city_size <- factor(inegi1$city_size)
inegi1$marital_status <- factor(inegi1$marital_status)

# We set up statistics for error bars: education level
IngByEdu <- inegi1 %>% 
  group_by(education) %>%
  summarize(N=n(),
            mean=round(mean(ing_tri), 4),
            sd=round(sd(ing_tri), 4),
            se=round(sd/sqrt(N), 4),
            median=round(median(ing_tri), 2),
            skew_Z=round(skew(ing_tri)[3], 4),
            skew_p=round(skew(ing_tri)[4], 4),
            kurt_Z=round(kurtosis(ing_tri)[3], 4), 
            kurt_p=round(kurtosis(ing_tri)[4], 4))
#View(IngByEdu)
IngByEdu

# ... and make the error bars 95% Confidence Intervals for Ed level
ggplot(IngByEdu, aes(x=education, y=mean, fill=education))+
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-(se*(qt(.95, N-1))),
                    ymax=mean+(se*(qt(.95, N-1)))), width=.2)+
  #scale_fill_brewer(palette="Dark2") +
  theme_classic()+
  labs(title="Income by Education level", 
       subtitle="with 95% CI", 
       x="Education Level", y="Mean Income")+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        plot.title=element_text(hjust=.5),
        plot.subtitle=element_text(hjust=.5))

# We set up statistics for error bars: city size
IngByCity <- inegi1 %>% 
  group_by(city_size) %>%
  summarize(N=n(),
            mean=round(mean(ing_tri), 4),
            sd=round(sd(ing_tri), 4),
            se=round(sd/sqrt(N), 4),
            median=round(median(ing_tri), 2),
            skew_Z=round(skew(ing_tri)[3], 4),
            skew_p=round(skew(ing_tri)[4], 4),
            kurt_Z=round(kurtosis(ing_tri)[3], 4), 
            kurt_p=round(kurtosis(ing_tri)[4], 4))
#View(IngByCity)
IngByCity

# ... and make the error bars 95% Confidence Intervals for Ed level
ggplot(IngByCity, aes(x=city_size, y=mean, fill=city_size))+
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-(se*(qt(.95, N-1))),
                    ymax=mean+(se*(qt(.95, N-1)))), width=.2)+
  #scale_fill_brewer(palette="Dark2") +
  theme_classic()+
  labs(title="Income by City Size", 
       subtitle="with 95% CI", 
       x="City Size", y="Mean Income")+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        plot.title=element_text(hjust=.5),
        plot.subtitle=element_text(hjust=.5))

# income by marital status
# We set up statistics for error bars: marital status
IngByMarit <- inegi1 %>% 
  group_by(marital_status) %>%
  summarize(N=n(),
            mean=round(mean(ing_tri), 4),
            sd=round(sd(ing_tri), 4),
            se=round(sd/sqrt(N), 4),
            median=round(median(ing_tri), 2),
            skew_Z=round(skew(ing_tri)[3], 4),
            skew_p=round(skew(ing_tri)[4], 4),
            kurt_Z=round(kurtosis(ing_tri)[3], 4), 
            kurt_p=round(kurtosis(ing_tri)[4], 4))
#View(IngByMarit)
IngByMarit

# ... and make the error bars 95% Confidence Intervals for Ed level
ggplot(IngByMarit, aes(x=marital_status, y=mean, fill=marital_status))+
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-(se*(qt(.95, N-1))),
                    ymax=mean+(se*(qt(.95, N-1)))), width=.2)+
  #scale_fill_brewer(palette="Dark2") +
  theme_classic()+
  labs(title="Income by Marital Status", 
       subtitle="with 95% CI", 
       x="Marital Status", y="Mean Income")+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        plot.title=element_text(hjust=.5),
        plot.subtitle=element_text(hjust=.5))

# mean income by age
# plot(y=inegi1$ing_tri,x=inegi1$edad)

IngByAge <- inegi1 %>% 
  group_by(edad) %>%
  summarize(N=n(),
            mean=round(mean(ing_tri), 4),
            sd=round(sd(ing_tri), 4),
            se=round(sd/sqrt(N), 4),
            median=round(median(ing_tri), 2),
            skew_Z=round(skew(ing_tri)[3], 4),
            skew_p=round(skew(ing_tri)[4], 4),
            kurt_Z=round(kurtosis(ing_tri)[3], 4), 
            kurt_p=round(kurtosis(ing_tri)[4], 4))
IngByAge

ggplot(IngByAge, aes(x = edad, y = mean)) +
  geom_point()+
  theme_classic()+
  labs(title="Income by Age Scatterplot", 
       x="Age", y="Income")+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        plot.title=element_text(hjust=.5),
        plot.subtitle=element_text(hjust=.5))+
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)



# -----------------------------------------
# ----------- POWER ANALYSIS --------------
# -----------------------------------------
#Power test to obtain estimate of n needed----

# participants: 133404
# Create objects for Salary Mean and SD
IncomeMean <- mean(inegi1$ing_tri)
IncomeSD <- sd(inegi1$ing_tri)
# Create object for effect (difference)
IncomeDiff <- 14755

#
#Perform power test for n needed
#Provide delta (Difference), sd, 
#alpha (sig.level), desired power
power.t.test(delta=IncomeDiff, sd=IncomeSD, 
             sig.level=.05, power=.80, 
             alternative="two.sided")

# To achieve a test with power 0.8 at the 0.05 significance level, 
# and to distinguish a 1% difference in means for quarterly income in MXN,
#   we would need a sample size of 4,691.

#
#Perform power function for obtained power----
# If we wanted to do a "backwards-looking" t-test.
power.t.test(n=103015, delta=IncomeDiff, 
             sd=IncomeSD, sig.level=.05,
             alternative="two.sided")


# -----------------------------------------
# ----------- RESULTS --------------
# -----------------------------------------
# LOG transformation of income

inegi1 = mutate(inegi1,log_inc = log(ing_tri))
# View(inegi1)

# Histogram
LIngHist <- ggplot(data=inegi1, aes(x=log_inc)) +
  geom_histogram(position="identity",bins=8)+
  theme_classic()+
  labs(title="Histogram of Log Quarterly Income", 
       x="Log of Quarterly Income", y="Frequency")
LIngHist

summary(inegi1$log_inc)

qqnorm(inegi1$log_inc)
qqline(inegi1$log_inc)

# we check for normality

lillie.test(inegi1$log_inc)
# lillie.test(rnorm(130000, mean = 5, sd = 3))


# -----------------------------------------------------------------------
# One t-test (one sample, independent samples, or paired samples)

#We first check Homogeneity of Variance
leveneTest(log_inc~city_size, data=inegi1)
# the levene test was significant, therefore we can say the variances are 
# not homogeneous

tEd <- t.test(log_inc~city_size, 
               data=inegi1,
              alternative="two.sided",
               var.equal=TRUE)
tEd

t_apa(tEd, format="docx")
# Q2D: Copy the output from the APA function here:
# RESPONSE: t(103013) = -59.55, p < .001, d = -0.37

# We explore do a non-parametric test, the Wilcoxon test.
# wilcox.test(inegi1$log_inc~inegi1$city_size) 

# RESPONSE: 

# After running a two-sided, independent-samples t-test on log quarterly income 
# by city size, we can reject the null hypothesis,
# as a statistically significant ( t(103013) = -59.55, p < .001, d = -0.37))
# difference in log quarterly income was found between
# city size. In figure xx, we can see the difference in log quarterly income
# by city size and, in figure xxx, the difference between quarterly income 
# by city size, both with a 95% confidence interval. We can appreciate there 
# is a meaningful difference between quarterly income, with the income of 
# those that live in cities with more than 15,000 inhabitants being the
# higher one. There is no overlap between the confidence interval of each, 
# which is another signal there is a significant difference between the two 
# groups. 


# Log Quarterly Income
# 95% confidence interval dataset on quarterly income by city size
ciPercent <- 95
ErrBarData_City <- inegi1 %>% group_by(city_size) %>% 
  summarize(N=n(),mean=mean(log_inc),
            sd=sd(log_inc),se=sd/sqrt(N), 
            ci=se*(qt((95/100)/2+.5, N-1)))
#View(ErrBarData_Gend)

# Create graph of error bars
ggplot(ErrBarData_City, aes(x=city_size, y=mean))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2)+
  geom_point(size=2) +
  theme_classic()+labs(title="Average Log of Quarterly Income per City Size", 
                       subtitle="95% Confidence Interval", x="City Size")+
  theme(axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=.5),
        plot.subtitle=element_text(hjust=.5))+
  coord_cartesian(ylim=c(9, 10))

# Quarterly Income
# 95% confidence interval dataset on quarterly income by city size
ciPercent <- 95
ErrBarData_City_nat <- inegi1 %>% group_by(city_size) %>% 
  summarize(N=n(),mean=mean(ing_tri),
            sd=sd(ing_tri),se=sd/sqrt(N), 
            ci=se*(qt((95/100)/2+.5, N-1)))
#View(ErrBarData_City_nat)

# Create graph of error bars
ggplot(ErrBarData_City_nat, aes(x=city_size, y=mean))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2)+
  geom_point(size=2) +
  theme_classic()+labs(title="Mean Quarterly Income per City Size", 
                       subtitle="95% Confidence Interval", x="City Size")+
  theme(axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=.5),
        plot.subtitle=element_text(hjust=.5))+
  coord_cartesian(ylim=c(13000, 20000))

# -----------------------------------------------------------------------
# One one-way ANOVA (between-subjects, repeated measures)

# Homogeneity of variance 

leveneTest(inegi1$log_inc, inegi1$education)
# Since there is a significant effect, we can say that the homogeneity of 
# variance assumption is not met

#Perform one-way between-subject ANOVA----

# income by education level 
IncByEdu <- aov(log_inc~education, 
                     data=inegi1)
summary.aov(IncByEdu)

anova_apa(IncByEdu, format="docx")


# RESPONSE: 
# education F(3, 103011) = 4084.12, p < .001, eta_p^2 = .11

# Kruskal Wallis Test, non-parametric, done because of homogeneity 
# of variance assumption not met. 
# ks<-kruskal.test(inegi1$log_inc~inegi1$education) 
# ruskal-Wallis chi-squared = 10742, df = 3, p-value < 2.2e-16

# RESPONSE: 
# education F(3, 103011) = 4084.12, p < .001, eta_p^2 = .11


# Write a few sentences about the test you ran, the result, and your
# interpretation (i.e., for a Results section):
# RESPONSE: 

# After running a one-way between-subjects ANOVA of log quarterly income 
# by educational attainment, it is possible to reject the null hypothesis, 
# as there is a statistically significant 
# (F(3, 103011) = 4084.12, p < .001, eta_p^2 = .11) difference.
# We can state there is at least one difference between the means of the groups. 
# After inspecting figure xx, we can detect a difference between the mean of 
# incomes for the different classifications of educational attainment, with no
# overlap of the 95% confidence interval between the means.
# It is possible to observe that higher levels of educational attainment
# indicate a higher mean of quarterly income. 


# Quarterly Income
# 95% confidence interval dataset on quarterly income by city size
ciPercent <- 95
ErrBarData_edu_nat <- inegi1 %>% group_by(education) %>% 
  summarize(N=n(),mean=mean(ing_tri),
            sd=sd(ing_tri),se=sd/sqrt(N), 
            ci=se*(qt((95/100)/2+.5, N-1)))
#View(ErrBarData_City_nat)

# Create graph of error bars
ggplot(ErrBarData_edu_nat, aes(x=education, y=mean))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2)+
  geom_point(size=2) +
  theme_classic()+labs(title="Average of Quarterly Income by Educational Attainment", 
                       subtitle="95% Confidence Interval", x="Educational Attainment")+
  theme(axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=.5),
        plot.subtitle=element_text(hjust=.5))+
  coord_cartesian(ylim=c(10000, 24000))


# Generate CONTRASTS
contrast1 <- c(0.5, 0.5, -0.5, -0.5) # high school and above vs lower than high school

contrasts(inegi1$log_inc) <- contrast1
IncomeByEdu <- aov(log_inc~education, 
                       data=inegi1)
summa_edu <- summary.aov(IncomeByEdu, 
            split=list(education=list("high school+ vs lower HS"=1)))


#                                           Df Sum Sq Mean Sq F value Pr(>F)    
# education                                  3   4026  1341.9    4084 <2e-16 ***
#   education: high school+ vs lower HS      1     43    42.7     130 <2e-16 ***
# Residuals                             103011  33846     0.3    

# We ran a contrast comparing groups of educational levels: high school and above 
# vs lower than high school, the results can be seen in table xx. 
# From these results, can infer from the results that there is enough evidence to 
# state there is a statistically significant (F= 130, p < .001)
# difference in mean log quarterly income
# between the two "Lower than high school" education groups   
# and the two "High school and above" education groups.


# Here, the diff is the difference in means. 
TukeyHSD(IncomeByEdu)
#                                               diff        lwr       upr p adj
# 02_Middle_School-01_Primary_and_less    0.18893396 0.17727046 0.2005975     0
# 03_High_School-01_Primary_and_less      0.27829619 0.26484836 0.2917440     0
# 04_Higher_Education-01_Primary_and_less 0.56307427 0.54984723 0.5763013     0
# 03_High_School-02_Middle_School         0.08936223 0.07586168 0.1028628     0
# 04_Higher_Education-02_Middle_School    0.37414031 0.36085967 0.3874209     0
# 04_Higher_Education-03_High_School      0.28477808 0.26990588 0.2996503     0

# ANALYZE THE TABLE

# Given the post-hoc tests results, which are shown in table xx, 
# we can find a difference in means in all 
# comparison pairs, since their lower and upper limits do not overlap with 
# zero.

# -----------------------------------------------------------------------
# One two-way / factorial ANOVA (between-subjects, repeated measures, or mixed)
# We analyze, two-way interactions between educational attainment and 
# city size. 

# Q3E: Check assumptions by running Levene's Test for the main effects
# and interaction. Then, interpret the results

# RESPONSE: 

leveneTest(inegi1$log_inc, inegi1$education)
# Levene's Test for Homogeneity of Variance (center = median)
#           Df F value    Pr(>F)    
# group      3  169.33 < 2.2e-16 ***
#       103011                  
# Since there is a significant effect, we can say that the homogeneity of 
# variance assumption is not met

leveneTest(inegi1$log_inc, inegi1$city_size)
# Levene's Test for Homogeneity of Variance (center = median)
#           Df F value   Pr(>F)    
# group      1  46.263 1.04e-11 ***
#       103013
# Since there is a significant effect, we can say that the homogeneity of 
# variance assumption is not met

leveneTest(inegi1$log_inc, interaction(inegi1$education, inegi1$city_size))
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   7  0.8378 0.5572
#       191   
# Since there is a significant effect, we can say that the homogeneity of 
# variance assumption is not met.


# Two-way independent-samples ANOVA

EduCS_Income<-aov(log_inc ~ education*city_size, data = inegi1)
Anova(EduCS_Income, type="III")


# Anova Table (Type III tests)
# 
# Response: log_inc
#                        Sum Sq     Df    F value    Pr(>F)    
# (Intercept)         1704939      1 5.2508e+06 < 2.2e-16 ***
#   education              1199      3 1.2313e+03 < 2.2e-16 ***
#   city_size                69      1 2.1209e+02 < 2.2e-16 ***
#   education:city_size      29      3 2.9604e+01 < 2.2e-16 ***
#   Residuals             33446 103007                    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# We ran a two-way independent-samples ANOVA analyzing the role of 
# educational attainment and city size on log quarterly income, the 
# results are in Table 8.
# Significant effects were found for the effects of education (p < .001) , 
# city size (p < .001) , and their interaction (p < .001) . 
# The sum of squares reveals the main source of variability for the main effects
# is educational attainment, followed by city size, and, finally, their interaction.


# Visualization 
boxplot <- ggplot(inegi1, aes(education, ing_tri))
boxplot + geom_boxplot() + facet_wrap(inegi1$city_size) + 
  labs(x = "Educational Attainment", y = "Quarterly Income")

# From figure 9, it does appear that the difference in income depends more 
# on educational attainment than city size. In general, incomes do look higher 
# for larger cities, but this difference is not as pronounced as when 
# comparing different educational attainment groups. Another quality of note is 
# the amount of outlier values is inversely proportional to education level. 
# The dispersion in the higher education group is expressed within the 
# interquartile range, which is wider than for the less educated groups. 


# -----------------------------------------------------------------------
# One advanced statistical test: ANCOVA or MANOVA: use AGE 
# -----------------------------------------------------------------------

# We run an ANCOVA for marital status as predictive variable, 
# using age (edad) as covariate. 

#
#One-way ANOVA of covariate predicting Y----
inegi1$ID <- row.names(inegi1)
# we want it to be NOT significant, no relationship in Covariate.
ez_covariate <- ezANOVA(data=inegi1, 
                 dv=edad, wid=ID, 
                 between=marital_status, type=3, 
                 detailed=TRUE, 
                 return_aov=TRUE)
ez_covariate$ANOVA
# Effect           DFn    DFd       SSn      SSd         F p p<.05       ges
# 1    (Intercept)   1 103011 148725006 20413456 750500.61 0     * 0.8793092
# 2 marital_status   3 103011   6664943 20413456  11210.95 0     * 0.2461350

#One-way ANOVA of marital status predicting log of quarterly income
ez_y <- ezANOVA(data=inegi1, 
                dv=log_inc, wid= ID, 
                between=marital_status, type=3, 
                detailed=TRUE, 
                return_aov=TRUE)
ez_y$ANOVA
# Effect           DFn    DFd          SSn      SSd            F p p<.05        ges
# 1    (Intercept)   1 103011 8029070.9864 37237.07 2.221127e+07 0     * 0.99538363
# 2 marital_status   3 103011     635.0099 37237.07 5.855545e+02 0     * 0.01676723
#
#ANCOVA----
#ANCOVA with interaction <- we run this one just to check if there is interaction
# we want it to NOT be significant. 
ANCOVA_y_Int <- aov(log_inc~marital_status*edad, data=inegi1)
Anova(ANCOVA_y_Int, type=3)

# Response: log_inc
# Sum Sq     Df    F value  Pr(>F)    
# (Intercept)         194485      1 5.5084e+05 < 2e-16 ***
#   marital_status         886      3 8.3649e+02 < 2e-16 ***
#   edad                     2      1 4.5924e+00 0.03212 *  
#   marital_status:edad    553      3 5.2194e+02 < 2e-16 ***
#   Residuals            36368 103007       


# ANCOVA without interaction <- this is the ancova we are 
# interested the most, not the one with interaction
# A significant effect says: our treatment can predict 
# the posttest.  
ANCOVA3_y <- aov(log_inc~edad+marital_status, data=inegi1)
summary.aov(ANCOVA3_y)
anova_apa(ANCOVA3_y, format="docx")
summary(glht(ANCOVA3_y, 
             linfct=mcp(marital_status="Tukey")))


# Multiple Comparisons of Means: Tukey Contrasts
# Fit: aov(formula = log_inc ~ edad + marital_status, data = inegi1)
# 
# Linear Hypotheses:
#                                                  Estimate  Std. Error t value  Pr(>|t|)
# 02_Married - 01_Common_Law_Union == 0            0.092915   0.005270  17.632   <2e-16
# 03_No_Longer_Married - 01_Common_Law_Union == 0 -0.064463   0.006738  -9.567   <2e-16
# 04_Single - 01_Common_Law_Union == 0            -0.140442   0.005871 -23.923   <2e-16
# 03_No_Longer_Married - 02_Married == 0          -0.157378   0.005639 -27.909   <2e-16
# 04_Single - 02_Married == 0                     -0.233357   0.005319 -43.872   <2e-16
# 04_Single - 03_No_Longer_Married == 0           -0.075978   0.006899 -11.013   <2e-16
# 
# 02_Married - 01_Common_Law_Union == 0           ***
#   03_No_Longer_Married - 01_Common_Law_Union == 0 ***
#   04_Single - 01_Common_Law_Union == 0            ***
#   03_No_Longer_Married - 02_Married == 0          ***
#   04_Single - 02_Married == 0                     ***
#   04_Single - 03_No_Longer_Married == 0           ***

# We ran a multiple comparisons of means via Tukey Contrasts for the 
# ancova analysis. 
# There were significant effects found for the comparison pairs of the 
# levels of marital status. We can interpret this variable is able
# to predict differences in quarterly income when controlling for age. 

# Visualization

IncByAge <- inegi1 %>% 
  group_by(edad,marital_status) %>%
  summarize(N=n(),mean=mean(ing_tri))
IncByAge

ggplot(IncByAge, aes(x=edad, y=mean, shape=marital_status)) +
  geom_point(size=2, alpha=.5) +
  theme_classic()+
  labs(title="Mean Quarterly Income by Age", 
       subtitle="Broken by Marital Status", 
       x="Age", y="Mean Quarterly Income")+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        plot.title=element_text(hjust=.5),
        plot.subtitle=element_text(hjust=.5))
