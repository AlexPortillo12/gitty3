birthday_sample <- function(group_size) {
  sample(1:365, group_size, replace = TRUE)
}

birthday_match <- function(birthdays) {
  length(unique(birthdays)) != length(birthdays)
}


many_samples <- function(reps, group_size) {
  matches <- replicate(reps, birthday_match(birthday_sample(group_size)))
  mean(matches)
}

library(ggplot2)


group_sizes <- 2:60
probabilities <- sapply(group_sizes, function(n) {
  many_samples(1000, n)  
})


birthday_data <- data.frame(GroupSize = group_sizes, Probability = probabilities)


ggplot(birthday_data, aes(x = GroupSize, y = Probability)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(title = "Probability of At Least One Shared Birthday by Group Size",
       x = "Number of People in Group",
       y = "Probability (%)",
       subtitle = "Monte Carlo simulation results") +
  theme_minimal()




