# Maladaptive

install.packages("pastecs")
library(pastecs)

View(PhD_Dataset)

head(PhD_Dataset)

tail(PhD_Dataset)

dat <- PhD_Dataset

dat[452,] # line 452

dat <- dat[-c(452), ] # delete line 452

tail(dat)

View(dat)

dat <- as.factor(dat)

# Descriptive Statistics

att <- table(dat$MALADAPTIVE_ATTACHMENT)

barplot(table(dat$MALADAPTIVE_ATTACHMENT))

pos_exp <- table(dat$MALADAPTIVE_POSITIVE_PREVIOUS_EXPERIENCE)

barplot(table(dat$MALADAPTIVE_POSITIVE_PREVIOUS_EXPERIENCE))

fat <- table(dat$MALADAPTIVE_FATALISM)

barplot(table(dat$MALADAPTIVE_FATALISM))

nat_prep <- table(dat$MALADAPTIVE_NATURAL_PREPAREDNESS)

barplot(table(dat$MALADAPTIVE_NATURAL_PREPAREDNESS))

cost_ben <- table(dat$MALADAPTIVE_COST_BENEFIT)

barplot(table(dat$MALADAPTIVE_COST_BENEFIT))

rel <- table(dat$MALADAPTIVE_RELIGIOUS)

barplot(table(dat$MALADAPTIVE_RELIGIOUS))

plots_combined_cbind <- cbind(att, pos_exp, fat, nat_prep, cost_ben, rel)

plots_combined_cbind

library(RColorBrewer)

par(mfrow=c(1, 1), mar=c(8, 5, 4, 9)) # mar: bottom, left, top, right

barplot(plots_combined_cbind, 
        width = 0.43,
        mgp=c(2.8, .5, 0), # First: xlab and ylab location. Second: Tick-mark labels. Third: Tick marks.
        cex.main = 1.3,
        cex.names = .75,
        legend = c("Not agree at all", 2, 3, 4, "Completely agree", "NA"), 
        xlab = "Maladaptive Responses", 
        ylab = "Number of Responses", 
        main = "Comparative Analysis of Maladaptive Responses",
        las = 1, # 1:hor 2:perpedincular to axis 3:vertical 
        xlim=c(0, ncol(y) + 1),
        col=brewer.pal(nrow(y), "Paired"),
        args.legend=list(
        x=ncol(y) + 2.5,
        y=max(colSums(y)),
        bty = "n"))

plots_combined_rbind <- rbind(att, pos_exp, fat, nat_prep, cost_ben, rel)

barplot(plots_combined_rbind, 
        width = 0.43,
        mgp=c(2.8, .5, 0), # First: xlab and ylab location. Second: Tick-mark labels. Third: Tick marks.
        cex.main = 1.3,
        cex.names = .7,
        legend = c("att", "pos_exp", "fat", "nat_prep", "cost_ben", "rel"), 
        names.arg = c("Not agree at all", 2, 3, 4, "Completely agree", "NA"),
        xlab = "Maladaptive Responses", 
        ylab = "Number of Responses", 
        main = "Comparative Analysis of Maladaptive Responses",
        las = 1, # 1:hor 2:perpedincular to axis 3:vertical 
        xlim=c(0, ncol(y) + 1),
        col=brewer.pal(nrow(y), "Paired"),
        args.legend=list(
          x=ncol(y) + 2.5,
          y=max(colSums(y)),
          bty = "n"))

# Description of Flood Insurance

flood_insurance_2011 <- table(dat$INSURANCE_2011)

barplot(table(dat$INSURANCE_2011))

flood_insurance_2013 <- table(dat$INSURANCE_2013)

barplot(table(dat$INSURANCE_2013))

flood_insurance_never <- table(dat$INSURANCE_NEVER)

barplot(table(dat$INSURANCE_NEVER))

flood_insurance_planning <- table(dat$INSURANCE_PLANNING)

barplot(table(dat$INSURANCE_PLANNING))

flood_insurance_combined <- cbind(flood_insurance_2011, flood_insurance_2013, flood_insurance_never, flood_insurance_planning)

flood_insurance_combined

# Creating Dependent Variable: Flood Insurance with 0 (no) and 1 (yes) 

# Coerce 2 to 1 in flood_insurance_2011

dat1 <- dat

dat1

logitInsurance <- glm(INSURANCE_DEPENDENT ~ 
                        MALADAPTIVE_ATTACHMENT + 
                        MALADAPTIVE_COST_BENEFIT +
                        MALADAPTIVE_FATALISM +
                        MALADAPTIVE_MOBILITY +
                        MALADAPTIVE_NATURAL_PREPAREDNESS +
                        MALADAPTIVE_POSITIVE_PREVIOUS_EXPERIENCE +
                        MALADAPTIVE_RELIGIOUS,
                      data = dat1)

summary(logitInsurance)


logitHouse_Raising <- glm(HOUSE_RAISING_DEPENDENT ~ 
                            MALADAPTIVE_ATTACHMENT + 
                            MALADAPTIVE_COST_BENEFIT +
                            MALADAPTIVE_FATALISM +
                            MALADAPTIVE_MOBILITY +
                            MALADAPTIVE_NATURAL_PREPAREDNESS +
                            MALADAPTIVE_POSITIVE_PREVIOUS_EXPERIENCE +
                            MALADAPTIVE_RELIGIOUS, 
                          data = dat1)

summary(logitHouse_Raising)

logitMeasure <- glm(MEASURE_DEPENDENT ~ 
                      MALADAPTIVE_ATTACHMENT + 
                      MALADAPTIVE_COST_BENEFIT +
                      MALADAPTIVE_FATALISM +
                      MALADAPTIVE_MOBILITY +
                      MALADAPTIVE_NATURAL_PREPAREDNESS +
                      MALADAPTIVE_POSITIVE_PREVIOUS_EXPERIENCE +
                      MALADAPTIVE_RELIGIOUS, 
                    data = dat1)

summary(logitMeasure)

logitRelocation <- glm(RELOCATION_DEPENDENT ~ 
                         MALADAPTIVE_ATTACHMENT + 
                         MALADAPTIVE_COST_BENEFIT +
                         MALADAPTIVE_FATALISM +
                         MALADAPTIVE_MOBILITY +
                         MALADAPTIVE_NATURAL_PREPAREDNESS +
                         MALADAPTIVE_POSITIVE_PREVIOUS_EXPERIENCE +
                         MALADAPTIVE_RELIGIOUS, 
                         data = dat1)
                    

summary(logitRelocation)

# Check Class Bias: Proportions in each dependent variable: Not 50/50 so sample observations in approximately equal proportions

dependent_insurance <- table(dat1$INSURANCE_DEPENDENT)
dependent_house_raising <- table(dat1$HOUSE_RAISING_DEPENDENT)
dependent_measure <- table(dat1$MEASURE_DEPENDENT)
dependent_relocation <- table(dat1$RELOCATION_DEPENDENT)

dependent_combined <- cbind(dependent_insurance, dependent_house_raising, dependent_measure, dependent_relocation)

par(xpd = TRUE)

barplot(dependent_combined, 
        width = 0.53,
        mgp=c(2.5, .6, -.4), # First: xlab and ylab location. Second: Tick-mark labels. Third: Tick marks.
        cex.main = 1.3,
        cex.names = .65,
        legend = c("No", "Yes"),
        xlim = c(2.6, -.45),               
        names.arg = c("Insurance", "Raising", "Home Improv.", "Relocation"),
        ylab = "Frequency", 
        main = "Comparative Analysis of Dependent Variables",
        las = 1,
        args.legend = 
          list("topright",
            bty = "n")) # 1:hor 2:perpendicular to axis 3:vertical
        
library(car)

vif(logitInsurance)
vif(logitRelocation)
vif(logitMeasure)
vif(logitHouse_Raising)

# Physchologically affected

frequency_psychologically_affected <- table(dat$PSYCHOLOGICALLY)
barplot(frequency_psychologically_affected,
        width = .2,
        mgp=c(2.5, .5, 0), # First: xlab and ylab location. Second: Tick-mark labels. Third: Tick marks.
        cex.main = 1,
        cex.names = .75,
        names.arg = c("No", "Yes"),
        xlab = "Psychologically Affected",
        ylab = "Frequency", 
        main = "Number of Residents Psychologically Affected by 2011 Floods",
        las = 1, # 1:hor 2:perpendicular to axis 3:vertical 
        xlim=c(.01, .5),
        col=c("red", "blue"))

fit <- lm(PSYCHOLOGICALLY ~ INSURANCE_DEPENDENT + 
            MEASURE_DEPENDENT + 
            HOUSE_RAISING_DEPENDENT + 
            RELOCATION_DEPENDENT, data = dat)
summary(fit)

# PMT: Dependent Variables

Insurance_cost_descriptive <- table(dat$INSURANCE_COST)
barplot(Insurance_cost_descriptive)

Insurance_simple_descriptive <- table(dat$INSURANCE_SIMPLE)
barplot(Insurance_simple_descriptive)

Insurance_effective_descriptive <- table(dat$INSURANCE_EFFECTIVE)
barplot(Insurance_effective_descriptive)


Insurance_descriptive_combined <- rbind(Insurance_cost_descriptive, Insurance_simple_descriptive, Insurance_effective_descriptive)
barplot(Insurance_descriptive_combined,
        width = 1.07,
        legend = c("Cost", "Simple", "Effective"), 
        xlab = "Respondents' Opinions",
        ylab = "Number of Responses", 
        main = "Comparative Analysis of Insurance",
        names.arg = c("Not agree at all", "2", "3", "4", "Completely agree"),
        cex.names = .9,
        las = 1,
        args.legend = list(
          bty = "n"))

Measure_cost_descriptive <- table(dat$MEASURE_COST)
barplot(Measure_cost_descriptive)

Measure_simple_descriptive <- table(dat$MEASURE_SIMPLE)
barplot(Measure_simple_descriptive)

Measure_effective_descriptive <- table(dat$MEASURE_EFFECTIVE)
barplot(Measure_effective_descriptive)

Measure_descriptive_combined <- rbind(Measure_cost_descriptive, Measure_simple_descriptive, Measure_effective_descriptive)
barplot(Measure_descriptive_combined,
        width = 1.07,
        legend = c("Cost", "Simple", "Effective"), 
        xlab = "Respondents' Opinions",
        ylab = "Number of Responses", 
        main = "Comparative Analysis of Home Improvements (Retrofit)",
        names.arg = c("Not agree at all", "2", "3", "4", "Completely agree"),
        las = 1,
        cex.names = .8,
        args.legend = list(
        bty = "n"))

dat$MEASURE_COST <- round(dat$MEASURE_COST)
dat$MEASURE_SIMPLE <- round(dat$MEASURE_SIMPLE)
dat$MEASURE_EFFECTIVE <- round(dat$MEASURE_EFFECTIVE)

House_raising_cost_descriptive <- table(dat$HOUSE_RAISING_COST)
barplot(House_raising_cost_descriptive)

House_raising_simple_descriptive <- table(dat$HOUSE_RAISING_SIMPLE)
barplot(House_raising_simple_descriptive)

House_raising_effective_descriptive <- table(dat$HOUSE_RAISING_EFFECTIVE)
barplot(House_raising_effective_descriptive)

House_raising_descriptive_combined <- rbind(House_raising_cost_descriptive, House_raising_simple_descriptive, House_raising_effective_descriptive)
barplot(House_raising_descriptive_combined,
        width = 1.07,
        legend = c("Cost", "Simple", "Effective"), 
        xlab = "Respondents' Opinions",
        ylab = "Number of Responses", 
        main = "Comparative Analysis of House Raising",
        names.arg = c("1:Not agree at all", "2", "3", "4", "5:Completely agree"),
        las = 1)   

Insurance_Measure_House_raising_combined <- rbind(Insurance_descriptive_combined, Measure_descriptive_combined, House_raising_descriptive_combined)
barplot(Insurance_Measure_House_raising_combined,
        width = 1.07,
        legend = c("Cost", "Simple", "Effective"), 
        xlab = "Respondents' Opinions",
        ylab = "Number of Responses", 
        main = "Comparative Analysis of Insurance, Home Improvements and House Raising",
        names.arg = c("1:Not agree at all", "2", "3", "4", "5:Completely agree"),
        las = 1)   

Insurance_Measure_House_raising_cost_combined <- table(Insurance_cost_descriptive, House_raising_cost_descriptive, Measure_cost_descriptive)
Insurance_Measure_House_raising_simple_combined <- table(Insurance_simple_descriptive, House_raising_simple_descriptive, Measure_simple_descriptive)
Insurance_Measure_House_raising_effective_combined <- table(Insurance_effective_descriptive, House_raising_effective_descriptive, Measure_effective_descriptive)

Actions_combined <- rbind(Insurance_Measure_House_raising_cost_combined, Insurance_Measure_House_raising_simple_combined, Insurance_Measure_House_raising_effective_combined)
barplot(Actions_combined)

library(ggplot2)

library(lattice)

library(reshape2)

df <- data.frame(dat$INSURANCE_COST, dat$INSURANCE_SIMPLE, dat$INSURANCE_EFFECTIVE)
df

data.m <- melt(df, id.vars = 3)
data.m

ggplot(data.m, aes(dat$INSURANCE_EFFECTIVE, value)) + geom_bar(aes(fill = variable), 
  width = 0.1, position = position_dodge(width=0.1), stat="identity") +  
  theme(legend.position="top", legend.title = 
          element_blank(),axis.title.x=element_blank(), 
        axis.title.y=element_blank())

# Discussion Tables

Insurance_further_costs_barchart <- table(dat$INSURANCE_FURTHER_COSTS)
barplot(Insurance_further_costs_barchart)
barplot(Insurance_further_costs_barchart,
        width = 1.07,
        xlab = "Respondents' Opinions",
        ylab = "Number of Responses", 
        main = "Acceptance to further costs on flood insurance",
        names.arg = c("Not agree at all", "2", "3", "4", "Completely agree"),
        cex.names = .6,
        las = 1)   

Insurance_options_barchart <- table(dat$INSURANCE_OPTIONS)
barplot(Insurance_options_barchart)
barplot(Insurance_options_barchart,
        width = 1.07,
        xlab = "Respondents' Opinions",
        ylab = "Number of Responses", 
        main = "Not enough options of flood insurance in the market",
        names.arg = c("Not agree at all", "2", "3", "4", "Completely agree"),
        cex.names = .6,
        las = 1)   


Measure_effective_barchart <- table(dat$MEASURE_EFFECTIVE)
barplot(Measure_effective_barchart)
barplot(Measure_effective_barchart,
        width = 1.07,
        xlab = "Respondents' Opinions",
        ylab = "Number of Responses", 
        main = "Effectivenes of Retrofitting",
        names.arg = c("Not agree at all", "2", "3", "4", "Completely agree"),
        cex.names = .6,
        las = 1)   

# Creating Tables for logit Outputs

install.packages("xtable")
library(xtable)

Insurance_table <- xtable(logitInsurance)
print.xtable(Insurance_table, type="html", file="PhD_Paper_Maladaptive_Insurance_table.html")

House_raising_table <- xtable(logitHouse_Raising)
print.xtable(House_raising_table, type="html", file="PhD_Paper_Maladaptive_House_raising_table.html")

Measure_table <- xtable(logitMeasure)
print.xtable(Measure_table, type="html", file="PhD_Paper_Maladaptive_Measure_table.html")

Relocation_table <- xtable(logitRelocation)
print.xtable(Relocation_table, type="html", file="PhD_Paper_Maladaptive_Relocation_table.html")
