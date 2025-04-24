# Load dataset
mypersonality <- read.csv("C:/Users/shrey/OneDrive/Desktop/mypersonality.csv", stringsAsFactors = FALSE)

# Inspect dataset
head(mypersonality)
str(mypersonality)
summary(mypersonality)

# Rename the first column if needed
colnames(mypersonality)[1] <- "AUTHID"

# Load dplyr
library(dplyr)

# Clean dataset: remove missing and duplicate rows
mypersonality_clean <- mypersonality %>%
  na.omit() %>%
  distinct()

# Convert appropriate columns to correct types
# Assuming cEXT, cNEU, etc. are categorical (y/n), and sEXT, etc. are numeric
mypersonality_clean <- mypersonality_clean %>%
  mutate(
    cEXT = as.factor(cEXT),
    cNEU = as.factor(cNEU),
    cAGR = as.factor(cAGR),
    cCON = as.factor(cCON),
    cOPN = as.factor(cOPN),
    sEXT = as.numeric(sEXT),
    sNEU = as.numeric(sNEU),
    sAGR = as.numeric(sAGR),
    sCON = as.numeric(sCON),
    sOPN = as.numeric(sOPN)
  )

# Load lubridate for better date parsing
library(lubridate)

# Try parsing date with multiple formats
mypersonality_clean$DATE_clean <- parse_date_time(
  mypersonality_clean$DATE,
  orders = c("mdy HMS p", "mdy HM p", "mdy HM", "dmY HM", "mdY HM", "mdy", "mdY")
)

# Convert to Date
mypersonality_clean$DATE_clean <- as.Date(mypersonality_clean$DATE_clean)

# Create Zodiac sign column
get_zodiac_sign <- function(date) {
  day <- as.integer(format(date, "%d"))
  month <- as.integer(format(date, "%m"))
  
  if ((month == 1 && day >= 20) || (month == 2 && day <= 18)) return("Aquarius")
  if ((month == 2 && day >= 19) || (month == 3 && day <= 20)) return("Pisces")
  if ((month == 3 && day >= 21) || (month == 4 && day <= 19)) return("Aries")
  if ((month == 4 && day >= 20) || (month == 5 && day <= 20)) return("Taurus")
  if ((month == 5 && day >= 21) || (month == 6 && day <= 20)) return("Gemini")
  if ((month == 6 && day >= 21) || (month == 7 && day <= 22)) return("Cancer")
  if ((month == 7 && day >= 23) || (month == 8 && day <= 22)) return("Leo")
  if ((month == 8 && day >= 23) || (month == 9 && day <= 22)) return("Virgo")
  if ((month == 9 && day >= 23) || (month == 10 && day <= 22)) return("Libra")
  if ((month == 10 && day >= 23) || (month == 11 && day <= 21)) return("Scorpio")
  if ((month == 11 && day >= 22) || (month == 12 && day <= 21)) return("Sagittarius")
  if ((month == 12 && day >= 22) || (month == 1 && day <= 19)) return("Capricorn")
}

# Apply zodiac function
mypersonality_clean$zodiac <- sapply(mypersonality_clean$DATE_clean, get_zodiac_sign)
table(mypersonality_clean$zodiac)

# Extra check for Aries and Taurus
aries_dates <- mypersonality_clean$DATE_clean[
  (month(mypersonality_clean$DATE_clean) == 3 & day(mypersonality_clean$DATE_clean) >= 21) |
    (month(mypersonality_clean$DATE_clean) == 4 & day(mypersonality_clean$DATE_clean) <= 19)
]

taurus_dates <- mypersonality_clean$DATE_clean[
  (month(mypersonality_clean$DATE_clean) == 4 & day(mypersonality_clean$DATE_clean) >= 20) |
    (month(mypersonality_clean$DATE_clean) == 5 & day(mypersonality_clean$DATE_clean) <= 20)
]

length(aries_dates)
length(taurus_dates)

# Load ggplot2
library(ggplot2)

# Mean of Big Five traits by zodiac
personality_by_zodiac <- mypersonality_clean %>%
  group_by(zodiac) %>%
  summarise(across(c(sEXT, sNEU, sAGR, sCON, sOPN), mean, na.rm = TRUE))

# Plot: Average Extraversion by zodiac
ggplot(personality_by_zodiac, aes(x = reorder(zodiac, sEXT), y = sEXT)) +
  geom_col(fill = "#69b3a2") +
  labs(title = "Average Extraversion (sEXT) by Zodiac Sign", x = "Zodiac", y = "Extraversion Score") +
  theme_minimal()

# ANOVA Tests
anova_sEXT <- summary(aov(sEXT ~ zodiac, data = mypersonality_clean))
anova_sNEU <- summary(aov(sNEU ~ zodiac, data = mypersonality_clean))
anova_sAGR <- summary(aov(sAGR ~ zodiac, data = mypersonality_clean))
anova_sCON <- summary(aov(sCON ~ zodiac, data = mypersonality_clean))
anova_sOPN <- summary(aov(sOPN ~ zodiac, data = mypersonality_clean))

# Print ANOVA Results
cat("ANOVA Results for sEXT (Extraversion):\n")
print(anova_sEXT)

cat("\nANOVA Results for sNEU (Neuroticism):\n")
print(anova_sNEU)

cat("\nANOVA Results for sAGR (Agreeableness):\n")
print(anova_sAGR)

cat("\nANOVA Results for sCON (Conscientiousness):\n")
print(anova_sCON)

cat("\nANOVA Results for sOPN (Openness):\n")
print(anova_sOPN)

# MANOVA Test using all 5 traits
traits_matrix <- cbind(
  sEXT = mypersonality_clean$sEXT,
  sNEU = mypersonality_clean$sNEU,
  sAGR = mypersonality_clean$sAGR,
  sCON = mypersonality_clean$sCON,
  sOPN = mypersonality_clean$sOPN
)

manova_result <- manova(traits_matrix ~ zodiac, data = mypersonality_clean)
summary(manova_result, test = "Pillai")

# Ensure zodiac is a factor
mypersonality_clean$zodiac <- as.factor(mypersonality_clean$zodiac)

# Multiple Linear Regression for each personality trait
model_sEXT <- lm(sEXT ~ zodiac, data = mypersonality_clean)
model_sNEU <- lm(sNEU ~ zodiac, data = mypersonality_clean)
model_sAGR <- lm(sAGR ~ zodiac, data = mypersonality_clean)
model_sCON <- lm(sCON ~ zodiac, data = mypersonality_clean)
model_sOPN <- lm(sOPN ~ zodiac, data = mypersonality_clean)

# R-squared values (percentage of variance explained)
r2_sEXT <- summary(model_sEXT)$r.squared * 100
r2_sNEU <- summary(model_sNEU)$r.squared * 100
r2_sAGR <- summary(model_sAGR)$r.squared * 100
r2_sCON <- summary(model_sCON)$r.squared * 100
r2_sOPN <- summary(model_sOPN)$r.squared * 100

# Print results
cat("Percentage of variance in each trait explained by zodiac:\n")
cat(sprintf("sEXT (Extraversion): %.2f%%\n", r2_sEXT))
cat(sprintf("sNEU (Neuroticism): %.2f%%\n", r2_sNEU))
cat(sprintf("sAGR (Agreeableness): %.2f%%\n", r2_sAGR))
cat(sprintf("sCON (Conscientiousness): %.2f%%\n", r2_sCON))
cat(sprintf("sOPN (Openness): %.2f%%\n", r2_sOPN))



#graphs
library(ggplot2)

# Prepare the data for plotting
regression_results <- data.frame(
  Trait = c("Extraversion", "Neuroticism", "Agreeableness", "Conscientiousness", "Openness"),
  R2 = c(r2_sEXT, r2_sNEU, r2_sAGR, r2_sCON, r2_sOPN)
)

# Plot the percentage of variance explained by zodiac for each trait
ggplot(regression_results, aes(x = Trait, y = R2)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  geom_text(aes(label = sprintf("%.2f%%", R2 * 100)), vjust = -0.3) +
  labs(title = "Percentage of Variance Explained by Zodiac Sign",
       x = "Personality Traits", y = "Variance Explained (%)") +
  theme_minimal()



#extra
install.packages("caret")

library(caret)

# Convert zodiac to factor if not already
mypersonality_clean$zodiac <- as.factor(mypersonality_clean$zodiac)

# Train a model to predict Extraversion (sEXT) using zodiac
model_data <- mypersonality_clean[, c("zodiac", "sEXT")]

# Train/Test Split
set.seed(123)
train_index <- createDataPartition(model_data$sEXT, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Fit linear regression
lm_model <- train(sEXT ~ zodiac, data = train_data, method = "lm")

# Evaluate
pred <- predict(lm_model, newdata = test_data)
RMSE



# K-means clustering
install.packages("factoextra")
library(factoextra)


traits_only <- mypersonality_clean %>%
  select(sEXT, sNEU, sAGR, sCON, sOPN)

kmeans_result <- kmeans(scale(traits_only), centers = 4)

mypersonality_clean$cluster <- as.factor(kmeans_result$cluster)

# Compare clusters with zodiac
table(mypersonality_clean$cluster, mypersonality_clean$zodiac)

# Visualize clusters
library(cluster)
fviz_cluster(kmeans_result, data = traits_only)

chisq.test(table(mypersonality_clean$cluster, mypersonality_clean$zodiac))

install.packages("rcompanion")  # Install it
library(rcompanion)             # Load it

cramerV(table(mypersonality_clean$cluster, mypersonality_clean$zodiac))


library(ggplot2)

ggplot(mypersonality_clean, aes(x = zodiac, fill = as.factor(cluster))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Distribution of Clusters Across Zodiac Signs",
    x = "Zodiac Sign",
    y = "Proportion",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)

cluster_profiles <- mypersonality_clean %>%
  group_by(cluster) %>%
  summarise(across(starts_with("s"), mean, na.rm = TRUE))

print(cluster_profiles)

