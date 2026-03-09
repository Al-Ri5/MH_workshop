library(dplyr)
library(effectsize)
library(ggplot2)
library(countrycode)

library(patchwork)

# load data
setwd("C:/Users/riegeraa/Documents/25_09 ChatAsLearning/data")
data_annotations <- read.csv("textual_annotations_gpt.csv", header = TRUE, sep = ";") 
data_all <- read.csv("merged_studies_no_attentionFails.csv", header = TRUE, sep = ",")

data_annotations_filtered <- subset(data_annotations, select = c(userId, justific_coherence, justific_perspectives,
                                                                s1_pro_count, s1_con_count, s2_pro_coun, s2_con_count,         
                                                                s2_pro_new_count, s2_con_new_count, comments))

# create new data_full frames
data_full <- merge(data_full_all, data_full_annotations_filtered, by = "userId")


write.csv(data_full, "merged_studies_annotations.csv", row.names=FALSE)

data_full$CR <- data_full$justific_perspectives + data_full$justific_perspectives
data_full$AE <- data_full$s2_pro_new_count + data_full$s2_con_new_count
data_full$N_post_arguments <- data_full$s2_pro_coun + data_full$s2_con_count

data_full$attitude_change <- ifelse(data_full$preEssay_likertPosition > 4, data_full$postEssay_likertPosition - data_full$preEssay_likertPosition,
                                                  ifelse(data_full$preEssay_likertPosition < 4, data_full$preEssay_likertPosition - data_full$postEssay_likertPosition,
                                                         ifelse(data_full$preEssay_likertPosition == 4 , abs(data_full$postEssay_likertPosition - data_full$preEssay_likertPosition), "MISSING")))

### Recoding textual variables to numerical
# Attitude strength pre study
data_full$attitude_strength <- ifelse(data_full$preEssay_likertPosition == 1 | data_full$preEssay_likertPosition == 7, 3,
                                               ifelse(data_full$preEssay_likertPosition == 2 | data_full$preEssay_likertPosition == 6, 2,
                                                      ifelse(data_full$preEssay_likertPosition == 3 | data_full$preEssay_likertPosition == 5, 1, 0)))

# Intellectual Humility
data_full$IH1 <- ifelse(data_full$sessionExperience_0_questionOwnOpinions == "Slightly like me", 2,
                                          ifelse(data_full$sessionExperience_0_questionOwnOpinions == "Moderately like me", 3,
                                                 ifelse(data_full$sessionExperience_0_questionOwnOpinions == "Quite a bit like me", 4,
                                                        ifelse(data_full$sessionExperience_0_questionOwnOpinions == "Very much like me", 5, 1))))

data_full$IH2 <- ifelse(data_full$sessionExperience_0_reconsiderOpinions == "Slightly like me", 2,
                                   ifelse(data_full$sessionExperience_0_reconsiderOpinions == "Moderately like me", 3,
                                          ifelse(data_full$sessionExperience_0_reconsiderOpinions == "Quite a bit like me", 4,
                                                 ifelse(data_full$sessionExperience_0_reconsiderOpinions == "Very much like me", 5, 1))))

data_full$IH3 <- ifelse(data_full$sessionExperience_0_valueDifferentOpinions == "Slightly like me", 2,
                                   ifelse(data_full$sessionExperience_0_valueDifferentOpinions == "Moderately like me", 3,
                                          ifelse(data_full$sessionExperience_0_valueDifferentOpinions == "Quite a bit like me", 4,
                                                 ifelse(data_full$sessionExperience_0_valueDifferentOpinions == "Very much like me", 5, 1))))

data_full$IH4 <- ifelse(data_full$sessionExperience_0_acceptBeingWrong == "Slightly like me", 2,
                                   ifelse(data_full$sessionExperience_0_acceptBeingWrong == "Moderately like me", 3,
                                          ifelse(data_full$sessionExperience_0_acceptBeingWrong == "Quite a bit like me", 4,
                                                 ifelse(data_full$sessionExperience_0_acceptBeingWrong == "Very much like me", 5, 1))))

data_full$IH5 <- ifelse(data_full$sessionExperience_0_openToChange == "Slightly like me", 2,
                                   ifelse(data_full$sessionExperience_0_openToChange == "Moderately like me", 3,
                                          ifelse(data_full$sessionExperience_0_openToChange == "Quite a bit like me", 4,
                                                 ifelse(data_full$sessionExperience_0_openToChange == "Very much like me", 5, 1))))


data_full$IH_score <- (data_full$IH1 + data_full$IH2 + data_full$IH3 
                                  + data_full$IH4 + data_full$IH5)/5 

# Tool trust
data_full$trust_tool <- ifelse(data_full$sessionExperience_0_tooltTrust == "2 - Slightly", 2,
                                                 ifelse(data_full$sessionExperience_0_tooltTrust == "3 - Moderately", 3,
                                                        ifelse(data_full$sessionExperience_0_tooltTrust == "4 - Mostly", 4,
                                                               ifelse(data_full$sessionExperience_0_tooltTrust == "5 - Completely", 5, 1))))


# Subjective Learning
data_full$subjective_learning <- ifelse(data_full$sessionExperience_0_subjectiveLearning == "Slightly", 2,
                                          ifelse(data_full$sessionExperience_0_subjectiveLearning == "Moderately", 3,
                                                 ifelse(data_full$sessionExperience_0_subjectiveLearning == "Considerably", 4,
                                                        ifelse(data_full$sessionExperience_0_subjectiveLearning == "Very much", 5, 1))))

# Attitude? certainty
data_full$attitude_certainty <- ifelse(data_full$sessionExperience_0_certainty == "Uncertain", 2,
                                          ifelse(data_full$sessionExperience_0_certainty == "Neutral", 3,
                                                 ifelse(data_full$sessionExperience_0_certainty == "Certain", 4,
                                                        ifelse(data_full$sessionExperience_0_certainty == "Very certain", 5, 1))))
# Attitude certainty change
data_full$certainty_change <- ifelse(data_full$sessionExperience_0_certaintyChange == "Became less certain", -1,
                                                 ifelse(data_full$sessionExperience_0_certaintyChange == "No change", 0,
                                                        ifelse(data_full$sessionExperience_0_certaintyChange == "Became more certain", 1, NaN)))
                                                            


# Tool Information Usefulness 
data_full$info_usefulness <- ifelse(data_full$sessionExperience_0_infoUsefulness == "Slightly useful", 2,
                                                  ifelse(data_full$sessionExperience_0_infoUsefulness == "Moderately useful", 3,
                                                         ifelse(data_full$sessionExperience_0_infoUsefulness == "Very useful", 4,
                                                                ifelse(data_full$sessionExperience_0_infoUsefulness == "Extremely useful", 5, 1))))
# Tool Efficiency 
data_full$efficiency <- ifelse(data_full$sessionExperience_0_efficiency == "Inefficient", 1,
                                               ifelse(data_full$sessionExperience_0_efficiency == "Neutral", 2,
                                                      ifelse(data_full$sessionExperience_0_efficiency == "Efficient", 3,
                                                             ifelse(data_full$sessionExperience_0_efficiency == "Very efficient", 4, 0))))
# Info quality
data_full$info_quality <- ifelse(data_full$sessionExperience_0_infoQuality == "Poor", 1,
                                          ifelse(data_full$sessionExperience_0_infoQuality == "Fair", 2,
                                                 ifelse(data_full$sessionExperience_0_infoQuality == "Good", 3,
                                                        ifelse(data_full$sessionExperience_0_infoQuality == "Excellent", 4, 0))))

# Info satisfaction
data_full$info_satisfaction <- ifelse(data_full$sessionExperience_0_satisfaction == "Dissatisfied", 1,
                                            ifelse(data_full$sessionExperience_0_satisfaction == "Neutral", 2,
                                                   ifelse(data_full$sessionExperience_0_satisfaction == "Satisfied", 3,
                                                          ifelse(data_full$sessionExperience_0_satisfaction == "Very Satisfied", 4, 0))))

# Age group
data_full$age_group <- ifelse(data_full$Age <= 25, "18-25",
                                      ifelse(data_full$Age <= 35, "26-35",
                                             ifelse(data_full$Age <= 45, "36-45",
                                                    ifelse(data_full$Age <= 65, "46-65", "66+"))))


### Filter for empty lines, nonsense responses
data <- filter(data_full, comments == "")

write.csv2(data, "data_with_annotations_semicolon.csv", row.names=FALSE, fileEncoding = "UTF-8")

### Sample Descriptives

data %>%
  group_by(Sex) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


data$Region <- country2Region("GEO3", data, Country.of.residence)

data$Region <- countrycode(
  sourcevar = data$Country.of.residence,
  origin = "country.name",
  destination = "region"
)

country <- data %>%
  group_by(Country.of.residence) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

data %>%
  group_by(Region) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

data %>%
  group_by(age_group) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

data %>%
  group_by(sessionExperience_0_educationLevel) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
                                                                                  
## By tool
# AE ANOVA and FIGURES
fit2 <- aov(AE ~ tool, data = data)
summary(fit2)
boxplot(AE ~ tool, data = data, main = "Argument Expansion by Tool",
        xlab = "Tool", ylab = "Argument Expansion", col = "lightblue")

cohens_f(fit2)

# CR ANOVA and FIGURES
fit1 <- aov(CR ~ tool, data = data)
summary(fit1)

cohens_f(fit1)

boxplot(CR ~ attitude_strength*tool, data = data, main = "Critical Reflection by Tool",
        xlab = "Tool", ylab = "Critical Reflection", col = "lightgreen")

## By topic
# AE ANOVA and FIGURES
fit2 <- aov(AE ~ task_topic * tool, data = data)
summary(fit2)
boxplot(AE ~ task_topic + tool, data = data, main = "Argument Expansion by Topic",
        xlab = "Tool", ylab = "Argument Expansion", col = "lightblue")

summary_AE <- data %>%
  group_by(tool, task_topic) %>%
  summarize(
    mean = mean(AE),
    se = sd(AE)/sqrt(n())
  )

# plot
plotAE <- ggplot(summary_AE, aes(x = tool, y = mean, color = task_topic)) +
  geom_point(position=position_dodge(width=0.5), size=3) +
  geom_line(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position=position_dodge(width=0.5), width=0.15, linewidth=0.8) +
  labs(
    title = "Argument Expansion per Tool and Topic",
    x = "Tool",
    y = "Argument Expansion",
    color = "Topic"
  ) +
  scale_color_viridis_d(option = "viridis")+
  guides(color = "none") +
  coord_cartesian(ylim = c(0, 5)) +
  labs(title = NULL) +
  theme_minimal(base_size = 14)+
  theme(panel.border = element_rect(colour = "black", fill = 0, size=1.1))


fit1 <- aov(AE ~ task_topic*tool*attitude_strength, data = data)
summary(fit1)


# CR ANOVA and FIGURES
fit1 <- aov(CR ~ tool, data = data)
summary(fit1)

boxplot(CR ~ tool + task_topic, data = data, main = "Critical Reflection by Topic",
        xlab = "Tool", ylab = "Critical Reflection", col = "lightgreen")

summary_CR <- data %>%
  group_by(tool, task_topic) %>%
  summarize(
    mean = mean(CR),
    se = sd(CR)/sqrt(n())
  )

# plot
plotCR <- ggplot(summary_CR, aes(x = tool, y = mean, color = task_topic)) +
  geom_point(position=position_dodge(width=0.4), size=3) +
  geom_line(position=position_dodge(width=0.4)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position=position_dodge(width=0.4), width=0.15, linewidth=0.8) +
  labs(
    title = "Critical Reasoning per Tool and Topic",
    x = "Tool",
    y = "Critical Reasoning",
    color = "Topic"
  ) +
  scale_color_viridis_d(option = "viridis")+
  coord_cartesian(ylim = c(1, 4)) + 
  labs(title = NULL) +
  theme_minimal(base_size = 14)+
  theme(legend.text = element_text(size = 8))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.1)) 

## merged plots
plotAE + plotCR

## Attitude change by tool and topics
fit1 <- aov(as.numeric(attitude_change) ~ task_topic * tool, data = data)
summary(fit1)

boxplot(as.numeric(attitude_change) ~ task_topic * tool, data = data, main = "Attitude Change by Tool and Topic",
        xlab = "Tool", ylab = "Critical Reflection", col = "lightgreen")


boxplot(CR ~ attitude_strength, data = data, main = "Attitude Change by Tool and Topic",
        xlab = "Tool", ylab = "Critical Reflection", col = "lightgreen")


boxplot(subjective_learning ~ tool, data = data, main = "Subjective Learning by Tool and Topic",
        xlab = "Tool", ylab = "Critical Reflection", col = "lightgreen")


# Factor pre task attitude strength
summary_df <- data %>%
  group_by(attitude_strength) %>%
  summarise(mean = mean(CR, na.rm = TRUE),
            sd   = sd(CR,   na.rm = TRUE),
            n    = n())
summary_df

ggplot(summary_df, aes(x = attitude_strength, y = mean)) +
  geom_col(fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(y = "Mean ± SD") +
  theme_minimal()


ggplot(data, aes(subjective_learning, AE)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1))

cor.test(data$AE, data$subjective_learning)

cor.test(data$CR, data$subjective_learning)
