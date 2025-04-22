# Read the translated dataset
divorce_data <- read.csv("C:/Users/shrey/OneDrive/Documents/divorce.csv",stringsAsFactors = FALSE)


# Keep only the DOB columns
divorce_data <- divorce_data[, c("DOB_partner_man", "DOB_partner_woman")]

# Remove rows with missing values
divorce_data <- divorce_data[complete.cases(divorce_data), ]

# Convert DOB to date format (DD/MM/YY)
# Assuming dates are in DD/MM/YY format, use as.Date with %d/%m/%y
divorce_data$DOB_partner_man <- as.Date(divorce_data$DOB_partner_man, format = "%d/%m/%y")
divorce_data$DOB_partner_woman <- as.Date(divorce_data$DOB_partner_woman, format = "%d/%m/%y")

# Extract day and month using format()
divorce_data$day_partner_1 <- as.numeric(format(divorce_data$DOB_partner_man, "%d"))
divorce_data$month_partner_1 <- as.numeric(format(divorce_data$DOB_partner_man, "%m"))
divorce_data$day_partner_2 <- as.numeric(format(divorce_data$DOB_partner_woman, "%d"))
divorce_data$month_partner_2 <- as.numeric(format(divorce_data$DOB_partner_woman, "%m"))

# Drop original DOB columns
divorce_data <- divorce_data[, c("day_partner_1", "month_partner_1", "day_partner_2", "month_partner_2")]

zodiac_sign <- function(day, month) {
  if (month == 12) {
    return(ifelse(day < 22, "Sagittarius", "Capricorn"))
  } else if (month == 1) {
    return(ifelse(day < 20, "Capricorn", "Aquarius"))
  } else if (month == 2) {
    return(ifelse(day < 19, "Aquarius", "Pisces"))
  } else if (month == 3) {
    return(ifelse(day < 21, "Pisces", "Aries"))
  } else if (month == 4) {
    return(ifelse(day < 20, "Aries", "Taurus"))
  } else if (month == 5) {
    return(ifelse(day < 21, "Taurus", "Gemini"))
  } else if (month == 6) {
    return(ifelse(day < 21, "Gemini", "Cancer"))
  } else if (month == 7) {
    return(ifelse(day < 23, "Cancer", "Leo"))
  } else if (month == 8) {
    return(ifelse(day < 23, "Leo", "Virgo"))
  } else if (month == 9) {
    return(ifelse(day < 23, "Virgo", "Libra"))
  } else if (month == 10) {
    return(ifelse(day < 23, "Libra", "Scorpio"))
  } else if (month == 11) {
    return(ifelse(day < 22, "Scorpio", "Sagittarius"))
  }
}

# Replace missing values with default day and month (e.g., January 1st)
divorce_data$day_partner_man[is.na(divorce_data$day_partner_man)] <- 1
divorce_data$month_partner_man[is.na(divorce_data$month_partner_man)] <- 1
divorce_data$day_partner_woman[is.na(divorce_data$day_partner_woman)] <- 1
divorce_data$month_partner_woman[is.na(divorce_data$month_partner_woman)] <- 1


divorce_data$Zod_sign_partner_1 <- mapply(zodiac_sign, divorce_data$day_partner_man, divorce_data$month_partner_man)
divorce_data$Zod_sign_partner_2 <- mapply(zodiac_sign, divorce_data$day_partner_woman, divorce_data$month_partner_woman)



# Keep only zodiac sign columns
divorce_data <- divorce_data[, c("Zod_sign_partner_1", "Zod_sign_partner_2")]

# Bar plot for men's zodiac signs
par(mar = c(5, 4, 4, 2) + 0.1)
barplot(table(divorce_data$Zod_sign_partner_1), 
        main = "Men Zodiac Signs", 
        xlab = "", 
        ylab = "Count of Zodiacs", 
        col = "lightblue", 
        las = 2)

# Bar plot for women's zodiac signs
barplot(table(divorce_data$Zod_sign_partner_2), 
        main = "Women Zodiac Signs", 
        xlab = "", 
        ylab = "Count of Zodiacs", 
        col = "brown", 
        las = 2)

# Create adjacency matrix for zodiac combinations
adjacency_matrix <- table(divorce_data$Zod_sign_partner_1, divorce_data$Zod_sign_partner_2)
print(adjacency_matrix)

# Create zodiac combinations column
divorce_data$Zodiac_combinations <- paste0(divorce_data$Zod_sign_partner_1, divorce_data$Zod_sign_partner_2)

# Bar plot for zodiac combinations
par(mar = c(10, 4, 4, 2) + 0.1)
barplot(table(divorce_data$Zodiac_combinations), 
        main = "Zodiac Signs Combinations", 
        xlab = "", 
        ylab = "Count of Combinations", 
        col = "darkcyan", 
        las = 2)

# Histogram for zodiac combination counts
comb_counts <- table(divorce_data$Zodiac_combinations)
hist(comb_counts, 
     main = "", 
     xlab = "", 
     ylab = "Density", 
     col = "grey", 
     border = "black", 
     freq = FALSE)
curve(dnorm(x, mean = mean(comb_counts), sd = sd(comb_counts)), 
      add = TRUE, 
      col = "red")

# Read compatibility matrix
# Reading the CSV file into the comp_matrix variable
comp_matrix <- read.csv("C:/Users/shrey/OneDrive/compmat.csv")




# Check the structure of the data to ensure it loaded correctly
str(comp_matrix)


# Scatter plot for compatibility rates
par(mar = c(10, 4, 4, 2) + 0.1)
plot(comp_matrix$Compatibility_rate, 
     xaxt = "n", 
     xlab = "", 
     ylab = "Compatibility Rate", 
     pch = 19)
axis(1, at = 1:nrow(comp_matrix), labels = comp_matrix$Zodiac_combination, las = 2)

# Histogram for compatibility rates
hist(comp_matrix$Compatibility_rate, 
     main = "", 
     xlab = "", 
     ylab = "Density", 
     col = "grey", 
     border = "black", 
     freq = FALSE)
curve(dnorm(x, mean = mean(comp_matrix$Compatibility_rate), sd = sd(comp_matrix$Compatibility_rate)), 
      add = TRUE, 
      col = "red")

# Summary statistics for compatibility rates
summary(comp_matrix$Compatibility_rate)

# Categorize compatibility
quantiles <- quantile(comp_matrix$Compatibility_rate, probs = c(0, 0.5, 1))
comp_matrix$Compatibility <- cut(comp_matrix$Compatibility_rate, 
                                 breaks = quantiles, 
                                 labels = c("Bad_fit", "Good_fit"), 
                                 include.lowest = TRUE)

# Count of each compatibility category
table(comp_matrix$Compatibility)

# Rename column for merging
colnames(comp_matrix)[colnames(comp_matrix) == "Zodiac_combination"] <- "Zodiac_combinations"

# Merge datasets
divorce_data <- merge(divorce_data, comp_matrix, by = "Zodiac_combinations", all.x = TRUE)

# Pie chart for compatibility distribution
pie_data <- table(divorce_data$Compatibility)
pie_labels <- paste0(names(pie_data), ": ", round(pie_data / sum(pie_data) * 100, 1), "%")
pie(pie_data, 
    labels = pie_labels, 
    main = "Zodiac Compatibility: Divorce Data (percentage)", 
    col = c("#ff9999", "#66b3ff"))

# Bar plot for compatibility counts
par(mar = c(5, 4, 4, 2) + 0.1)
barplot(table(divorce_data$Compatibility), 
        main = "Zodiac Compatibility: Divorce Data (actual numbers)", 
        xlab = "", 
        ylab = "", 
        col = NA, 
        border = c("black", "darkgrey"))