library(readr)
library(tidyverse)
library(dplyr)
library(viridis)
d<- read_csv("C:\\Users\\Shahriar\\Desktop\\dm_office_sales.csv")
head(d)
d$`training level`<- as.factor(d$`training level`) # classfication
d$`work experience`<- as.factor(d$`work experience`)
# boxplot
ggplot(d,aes(x=salary,fill = `training level`))+
  geom_boxplot()+
  coord_flip()+
  labs(title="Salary Distribution by Training Level")
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5) # Center the title
  ) +
  ggsave("C:/Users/Shahriar/Documents/Assignment/Salary_Distribution.png")
#level of education
ggplot(d,aes(x=salary,fill = `level of education`))+
  geom_boxplot()+
  coord_flip()+labs(title="Salary Distribution by Level of Education")
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5) # Center the title
  ) +
  ggsave("C:/Users/Shahriar/Documents/Assignment/Salary_Distribution(lo).png")
# Division
ggplot(d,aes(x=salary,fill = division))+
    geom_boxplot()+
    coord_flip()+labs(title="Salary Distribution by Division")
    theme_light() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5) # Center the title
    ) +
    ggsave("C:/Users/Shahriar/Documents/Assignment/Salary_Distribution by Division.png")
# Scatterplot
ggplot(d,aes(x=salary,y=sales,fill = division,colour = `level of education`))+
  geom_point() +
  labs(title = "Relationship Between Sales And Salary")+
  theme(plot.title = element_text(size = 15, hjust = 0.5))+
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  scale_color_viridis_d(option = "viridis", direction = 1) +
  theme_classic()+
  ggsave("C:/Users/Shahriar/Documents/Assignment/Relationship Between Sales And Salary.png")

#histogram salary
ggplot(d,aes(x=salary))+
  geom_histogram(aes(y = ..density..), 
                 position = "identity", alpha = 0.7,
                 fill="#5B3B84")+
  geom_density(alpha=0.8,adjust=1,color="#312D2E")+
  labs(title = "Salary Distribution with Histogram and KDE", 
       x = "Salary",y=" ") +
  theme_classic()+
  ggsave("C:/Users/Shahriar/Documents/Assignment/Salary Distribution with Histogram and KDE.png")

# Histogram Sales
ggplot(d,aes(x=sales))+
  geom_histogram(aes(y = ..density..), 
                 position = "identity", alpha = 0.7,
                 fill="#12A2D7")+
  geom_density(alpha=0.8,adjust=1,color="#312D2E")+
  labs(title = "Sales Distribution with Histogram and KDE", 
       x = "Salary",y=" ") +
  theme_classic()+
  ggsave("C:/Users/Shahriar/Documents/Assignment/Sales Distribution with Histogram and KDE.png")
#COunt plot Division
ggplot(d, aes(x = division)) +
  geom_bar(fill = "#0073C2FF") + # Bar plot with a specific fill color
  labs(title = "Count of Each Division", x = "Division", y = " ") +
  theme_classic()+
  ggsave("C:/Users/Shahriar/Documents/Assignment/Division Count.png")
# Level of Education and Salary 
summary_stats <- d %>%
  group_by(division,`level of education`) %>%
  summarise(
    count = n(),  # Count of level of education
    mean_salary = mean(salary, na.rm = TRUE),  # Mean salary
    sd_salary = sd(salary, na.rm = TRUE)  # Standard deviation of salary
  )
ggplot(summary_stats, aes(x = division, y = mean_salary, fill = `level of education`)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot showing mean salary
  geom_errorbar(aes(ymin = mean_salary - sd_salary, ymax = mean_salary + sd_salary), 
                position = position_dodge(width = 0.9), width = 0.25) +  # Error bars representing standard deviation
  labs(title = "Average Salary by Division and Level of Education", 
       x = "Division", y = "Average Salary", fill = "Level of Education") +
  theme_classic() +
  scale_fill_viridis_d(option = "magma")+
  ggsave("C:/Users/Shahriar/Documents/Assignment/Average Salary by Division and Level of Education.png", 
         width = 12,
         height = 4,
         dpi = 150)