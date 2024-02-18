# 1. Use R to illustrate that the probability of getting:
#   
#   (a) a head is 0.5 if a fair coin is tossed repeatedly;
# (b) a red card is 0.5 if cards are drawn repeatedly with replacement from awell-shuffled deck;
# (c) an even number is 0.5 if a fair die is rolled repeatedly.

headsOrTails <- c("H", "T")

a_100 <- sample(headsOrTails, size = 100, replace = TRUE)
a_1000 <- sample(headsOrTails, size = 1000, replace = TRUE)
a_10000 <- sample(headsOrTails, size = 10000, replace = TRUE)
a_100000 <- sample(headsOrTails, size = 100000, replace = TRUE)
a_1000000 <- sample(headsOrTails, size = 1000000, replace = TRUE)
a_10000000 <- sample(headsOrTails, size = 10000000, replace = TRUE)


a_df <- data.frame(
  result = c("Heads", "Tails"),
  freq_100 = c(sum(a_100 == "H"), sum(a_100 == "T")),
  freq_1000 = c(sum(a_1000 == "H"), sum(a_1000 == "T")),
  freq_10000 = c(sum(a_10000 == "H"), sum(a_10000 == "T")),
  freq_100000 = c(sum(a_100000 == "H"), sum(a_100000 == "T")),
  freq_1000000 = c(sum(a_1000000 == "H"), sum(a_1000000 == "T")),
  freq_10000000 = c(sum(a_10000000 == "H"), sum(a_10000000 == "T")),
  rel_freq_100 = c(sum(a_100 == "H")/100, sum(a_100 == "T")/100),
  rel_freq_1000 = c(sum(a_1000 == "H")/1000, sum(a_1000 == "T")/1000),
  rel_freq_10000 = c(sum(a_10000 == "H")/10000, sum(a_10000 == "T")/10000),
  rel_freq_100000 = c(sum(a_100000 == "H")/100000, sum(a_100000 == "T")/100000),
  rel_freq_1000000 = c(sum(a_1000000 == "H")/1000000, sum(a_1000000 == "T")/1000000),
  rel_freq_10000000 = c(sum(a_10000000 == "H")/10000000, sum(a_10000000 == "T")/10000000)
)

library(ggplot2)

a_long_df <- tidyr::pivot_longer(a_df, cols = starts_with("rel_freq"),
                               names_to = "sample_size", values_to = "rel_freq")

a_long_df$sample_size <- gsub("rel_freq_", "", a_long_df$sample_size)
a_long_df$sample_size <- as.numeric(a_long_df$sample_size)

ggplot(a_long_df, aes(x = sample_size, y = rel_freq, color = result)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = paste("N =", sample_size)), vjust = -0.5, check_overlap = TRUE) +
  scale_x_log10() +
  labs(title = "Relative Frequency of Heads or Tails vs. Sample Size",
       x = "Sample Size",
       y = "Relative Frequency",
       color = "Result") +
  theme_minimal() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red")


deckOfCards <- c("R", "B")

b_100 <- sample(deckOfCards, size = 100, replace = TRUE)
b_1000 <- sample(deckOfCards, size = 1000, replace = TRUE)
b_10000 <- sample(deckOfCards, size = 10000, replace = TRUE)
b_100000 <- sample(deckOfCards, size = 100000, replace = TRUE)
b_1000000 <- sample(deckOfCards, size = 1000000, replace = TRUE)
b_10000000 <- sample(deckOfCards, size = 10000000, replace = TRUE)


b_df <- data.frame(
  result = c("Red", "Black"),
  freq_100 = c(sum(b_100 == "R"), sum(b_100 == "B")),
  freq_1000 = c(sum(b_1000 == "R"), sum(b_1000 == "B")),
  freq_10000 = c(sum(b_10000 == "R"), sum(b_10000 == "B")),
  freq_100000 = c(sum(b_100000 == "R"), sum(b_100000 == "B")),
  freq_1000000 = c(sum(b_1000000 == "R"), sum(b_1000000 == "B")),
  freq_10000000 = c(sum(b_10000000 == "R"), sum(b_10000000 == "B")),
  rel_freq_100 = c(sum(b_100 == "R")/100, sum(b_100 == "B")/100),
  rel_freq_1000 = c(sum(b_1000 == "R")/1000, sum(b_1000 == "B")/1000),
  rel_freq_10000 = c(sum(b_10000 == "R")/10000, sum(b_10000 == "B")/10000),
  rel_freq_100000 = c(sum(b_100000 == "R")/100000, sum(b_100000 == "B")/100000),
  rel_freq_1000000 = c(sum(b_1000000 == "R")/1000000, sum(b_1000000 == "B")/1000000),
  rel_freq_10000000 = c(sum(b_10000000 == "R")/10000000, sum(b_10000000 == "B")/10000000)
)

b_long_df <- tidyr::pivot_longer(b_df, cols = starts_with("rel_freq"),
                               names_to = "sample_size", values_to = "rel_freq")

b_long_df$sample_size <- gsub("rel_freq_", "", b_long_df$sample_size)
b_long_df$sample_size <- as.numeric(b_long_df$sample_size)

ggplot(b_long_df, aes(x = sample_size, y = rel_freq, color = result)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = paste("N =", sample_size)), vjust = -0.5, check_overlap = TRUE) +
  scale_x_log10() +
  labs(title = "Relative Frequency of Red or Black vs. Sample Size",
       x = "Sample Size",
       y = "Relative Frequency",
       color = "Result") +
  theme_minimal() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red")



die <- c("E", "O")

c_100 <- sample(die, size = 100, replace = TRUE)
c_1000 <- sample(die, size = 1000, replace = TRUE)
c_10000 <- sample(die, size = 10000, replace = TRUE)
c_100000 <- sample(die, size = 100000, replace = TRUE)
c_1000000 <- sample(die, size = 1000000, replace = TRUE)
c_10000000 <- sample(die, size = 10000000, replace = TRUE)


c_df <- data.frame(
  result = c("Even", "Odd"),
  freq_100 = c(sum(c_100 == "E"), sum(c_100 == "O")),
  freq_1000 = c(sum(c_1000 == "E"), sum(c_1000 == "O")),
  freq_10000 = c(sum(c_10000 == "E"), sum(c_10000 == "O")),
  freq_100000 = c(sum(c_100000 == "E"), sum(c_100000 == "O")),
  freq_1000000 = c(sum(c_1000000 == "E"), sum(c_1000000 == "O")),
  freq_10000000 = c(sum(c_10000000 == "E"), sum(c_10000000 == "O")),
  rel_freq_100 = c(sum(c_100 == "E")/100, sum(c_100 == "O")/100),
  rel_freq_1000 = c(sum(c_1000 == "E")/1000, sum(c_1000 == "O")/1000),
  rel_freq_10000 = c(sum(c_10000 == "E")/10000, sum(c_10000 == "O")/10000),
  rel_freq_100000 = c(sum(c_100000 == "E")/100000, sum(c_100000 == "O")/100000),
  rel_freq_1000000 = c(sum(c_1000000 == "E")/1000000, sum(c_1000000 == "O")/1000000),
  rel_freq_10000000 = c(sum(c_10000000 == "E")/10000000, sum(c_10000000 == "O")/10000000)
)

c_long_df <- tidyr::pivot_longer(c_df, cols = starts_with("rel_freq"),
                                 names_to = "sample_size", values_to = "rel_freq")

c_long_df$sample_size <- gsub("rel_freq_", "", c_long_df$sample_size)
c_long_df$sample_size <- as.numeric(c_long_df$sample_size)

ggplot(c_long_df, aes(x = sample_size, y = rel_freq, color = result)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = paste("N =", sample_size)), vjust = -0.5, check_overlap = TRUE) +
  scale_x_log10() +
  labs(title = "Relative Frequency of Even or Odd vs. Sample Size",
       x = "Sample Size",
       y = "Relative Frequency",
       color = "Result") +
  theme_minimal() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red")




#2. An experiment consists of tossing two fair coins. 
#Use R to simulate this experiment 100 times and obtain the relative frequency of 
#each possible outcome. Hence, estimate the probability of getting one head and 
#one tail in any order.
twoCoins <- c("HH", "TT","HT","TH")
sec_rand <- sample(twoCoins, size = 100, replace = TRUE)
sec_tab <- table(sec_rand)

sec_df <- data.frame(
  result = c("Two Heads", "Two Tails", "Split"),
  freq = c(sum(sec_rand == "HH"), sum(sec_rand == "TT"), sum(sec_rand == "TH") + sum(sec_rand == "HT")),
  rel_freq = c(sum(sec_rand == "HH")/100, sum(sec_rand == "TT")/100, (sum(sec_rand == "TH") + sum(sec_rand == "HT"))/100)
)
sec_df
#create a table wherein it combines the frequency of both HT and TH. Then divide by the total N



#3. An experiment consists of rolling a die. Use R to simulate this experiment 600 times
#and obtain the relative frequency of each possible outcome. Hence, estimate the probability
#of getting each of 1, 2, 3, 4, 5, and 6.
die <- c(1, 2, 3, 4, 5, 6)

third_rand <- sample(die, size = 600, replace = TRUE)
third_df <- data.frame(
  result = c("1", "2", "3", "4", "5", "6"),
  freq = c(sum(third_rand == 1), sum(third_rand == 2), sum(third_rand == 3), sum(third_rand == 4), sum(third_rand == 5), sum(third_rand == "6")),
  rel_freq = c(sum(third_rand == 1)/600, sum(third_rand == 2)/600, sum(third_rand == 3)/600, sum(third_rand == 4)/600, sum(third_rand == 5)/600, sum(third_rand == 6)/600)
)
third_df
#same with 2