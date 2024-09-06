library(readr)
library(tidyverse)
library(dplyr)
library(viridis)
library(ggExtra)
library(GGally)
s<- read_csv("C:\\Users\\Shahriar\\Desktop\\StudentsPerformance.csv")
head(s)
s$gender<-as.factor(s$gender)
# Reshape the data from wide to long format
s_long <- s %>%
  pivot_longer(cols = c(`reading score`, `writing score`, `math score`), 
               names_to = "Score_Type", 
               values_to = "Score")

#box plot with flipped coordinates
ggplot(s_long, aes(x = Score_Type, y = Score, fill = gender)) +
  geom_boxplot() +
  labs(title = "Distribution of Scores by Gender", 
       x = "Score Type", y = "Score") +
  theme_classic()+
  ggsave("C:/Users/Shahriar/Documents/Assignment/Score_Distribution_by_Gender.png",
         width = 12,
         height = 4,
         dpi = 150)
#box plot with flipped parental level of education
ggplot(s_long, aes(x = Score_Type, y = Score, fill = `parental level of education`)) +
  geom_boxplot() +
  labs(title = "Distribution of Scores by parental level of education", 
       x = "Score Type", y = "Score") +
  theme_classic()+
  ggsave("C:/Users/Shahriar/Documents/Assignment/Score_Distribution_by_parental level of education.png",
         width = 12,
         height = 4,
         dpi = 150)                        
# Create the main scatter plot
scatter_plot <- ggplot(s, aes(x = `math score`, y = `reading score`)) +
  geom_point(alpha = 0.6) +  # Scatter plot with semi-transparent points
  labs(title = "Joint Plot of Math Score and Reading Score", 
       x = "Math Score", y = "Reading Score") +
  theme_classic()
# Add marginal histograms or density plots
f<- ggMarginal(scatter_plot, type = "histogram", fill = "#55C4FF")
ggsave("C:/Users/Shahriar/Documents/Assignment/Joint_Plot_Math_vs_Reading.png", 
       plot = f, 
       width = 8,
       height = 6,
       dpi = 150)
