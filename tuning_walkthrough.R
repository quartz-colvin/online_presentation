# Load tidyverse
library(tidyverse)
library(here)

dat <- read_csv(here("data", "vocab_learning.csv"))

glimpse(dat)

# vocab size as a function of age and reader time
# start out by plotting
dat |>
  ggplot() +
  aes (x = age, y = vocab, color = reader) +
  geom_point() +
  geom_smooth(method = "lm")

mod1 <- lm(vocab ~ age * reader, data = dat)
summary(mod1)

# centering x
dat <- read_csv(here("data", "vocab_learning.csv")) |>
  mutate(
    age_c = age - mean(age)
  )

glimpse(dat)

# check wheter mean of age_c equals, yes
dat |>
  summarize(
    age_avg = mean(age),
    age_sd = sd(age),
    age_c_avg = mean (age_c),
    age_c_sd = sd(age_c)
  )

mod2 <- lm(vocab ~ age_c * reader, data = dat)
summary(mod2)
# intercept: at the age of mean (average age of the whole data), the value of vocab size
# readerfrequent: the difference of vocab size between frequent and average reader at the age of

# sum coding
dat <- read_csv(here("data", "vocab_learning.csv")) |>
  mutate(
    age_c = age - mean(age),
    reader_sum = if_else(reader == "average", -1, 1)
  )

glimpse(dat)

dat |>
  ggplot() +
  aes (x = age, y = vocab) +
  geom_point() +
  geom_smooth(aes(color = reader), method = "lm") +
  geom_smooth(method = "lm")
# baseline of the sum coding also plotted

mod3 <- lm(vocab ~ age_c * reader_sum, data = dat)
summary(mod3)
