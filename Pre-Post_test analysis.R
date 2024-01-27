#Import score to R
library(readxl)
Pre_Post_score <- read_excel("Score.xlsx", sheet ="Score")
View(Pre_Post_score)

#####################

# calculate mean and sd
library("dplyr")
group_by(Pre_Post_score, Test) %>%
  summarise(
    count = n(),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE)
  )

#box plot
library("ggpubr")
ggboxplot(Pre_Post_score, x = "Test", y = "Score", 
          color = "Test", palette = c("#00AFBB", "#E7B800"),
          order = c("Pre-test", "Post-test"),
          ylab = "Score", xlab = "Test")

#Normality test (n < 50 use Shapiro-Wilk normality test)
# compute the difference
score_pre <- with(Pre_Post_score,Score[Test == "Pre-test"])
score_pre
score_post <- with(Pre_Post_score,Score[Test == "Post-test"])
score_post
score_diff <- with(Pre_Post_score, 
                   Score[Test == "Pre-test"] - Score[Test == "Post-test"])
score_diff
# Shapiro-Wilk normality test for the differences
shapiro.test(score_pre) # => p-value = 0.003301, reject null hypothesis (not normal distribution)
shapiro.test(score_post) # => p-value = 0.0009104, reject null hypothesis (not normal distribution)
shapiro.test(score_diff) # => p-value = 0.01927, reject null hypothesis (not normal distribution)

# Data is not normal distribution (non-parametric), so paired two sample Wilcoxon test are used
res_diff <- wilcox.test(Score ~ Test, data = Pre_Post_score, 
                   paired = TRUE)
res_diff
res_diff$p.value # => p-value = 0.00231 reject null hypothesis (score of pre-test and post test are different)
res_improve <- wilcox.test(Score ~ Test, data = Pre_Post_score, 
                           paired = TRUE, alternative = "less")
res_improve
res_diff$p.value # => p-value = 0.9991 fail to reject null hypothesis (score of post test is higher than pre-test)
