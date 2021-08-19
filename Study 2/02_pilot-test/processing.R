library(tidyverse)
library(ggtext)

df <- read_csv('pilot-test-2.csv', col_names = TRUE) %>%
  slice(3:n()) %>% janitor::clean_names() %>% filter(is.na(q_attention1))


table(df$q7_1, df$q8_2)


df <- read_csv('pilot-test-2-numeric.csv', col_names = TRUE) %>%
  slice(3:n()) %>%
  janitor::clean_names()

# Who did not pass the attention check requiring to not respond
df %>% filter(!is.na(q_attention1)) %>% select(random)

mydata <- df %>% filter(q1_2_1 == 1 & q1_2_2 == 1 & q1_2_3 == 1) %>%
  filter(is.na(q_attention1)) %>%
  mutate(q_recaptcha_score = as.numeric(q_recaptcha_score),
         duration_in_seconds = as.integer(duration_in_seconds)) %>%
  filter(q_recaptcha_score >= 0.5 | is.na(.$q_recaptcha_score))


mydata %>%
  filter(q_recaptcha_score <= 0.8 | duration_in_seconds <= 420) %>%
  select(c(q_recaptcha_score, duration_in_seconds, q7_1, q8_2,
           q4_2:q4_6, q5_2:q5_6, q9_1, q9_2)) %>%
  mutate(q_recaptcha_score = as.numeric(q_recaptcha_score),
         duration_in_seconds = as.integer(duration_in_seconds)) %>%
  print(n = nrow(.))

duration_stat <- mydata %>%
  summarise(mean_duration = mean(duration_in_seconds),
            sd_duration = sd(duration_in_seconds),
            median_duration = median(duration_in_seconds))

theme_set(
  theme_classic(base_size = 15, base_rect_size = .5, base_family = "Roboto") +
    theme(
      axis.line = element_line(size = .5),
      axis.ticks = element_line(size = .5),
      axis.ticks.length.y = unit(.3, "cm"),
      axis.line.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      legend.position = "none"
    )
)


mydata %>%
  ggplot(aes(x = duration_in_seconds)) +
  geom_histogram(colour = "black", fill = "cornflowerblue") +
  geom_richtext(x = 1100, y = 2.7, hjust = 0, family = "Roboto",
                label = paste0("*mean* = ", round(duration_stat$mean_duration, 2)),
                label.color = NA, size = 6.3, fontface = "bold") +
  geom_richtext(x = 1100, y = 2.3, hjust = 0, family = "Roboto",
                label = paste0("*sd* = ", round(duration_stat$sd_duration, 2)),
                label.color = NA, size = 6.3, fontface = "bold") +
  geom_richtext(x = 1100, y = 1.9, hjust = 0, family = "Roboto",
                label = paste0("*median* = ", round(duration_stat$median_duration)),
                label.color = NA, size = 6.3, fontface = "bold") +
  geom_vline(xintercept = mean(duration_stat$mean_duration),
             size = 2, colour = "black", alpha = .5) +
  geom_vline(xintercept = mean(duration_stat$median_duration),
             size = 2, colour = "red", alpha = .5, linetype = "longdash") +
  scale_x_time() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Duration (in minutes)", y = "Count")

mydata <- mydata %>%
  mutate(valence = case_when(
    (!is.na(.$q10_11))|(!is.na(.$q11_11))|(!is.na(.$q12_11)) ~ "Biden",
    (!is.na(.$q13_11))|(!is.na(.$q14_11))|(!is.na(.$q15_11)) ~ "Ducey"
  ),
  media_slant = case_when(
    (!is.na(.$q10_11))|(!is.na(.$q13_11)) ~ "Reuter",
    (!is.na(.$q11_11))|(!is.na(.$q14_11)) ~ "Fox",
    (!is.na(.$q12_11))|(!is.na(.$q15_11)) ~ "MSNBC"
  ),
  agree_t1 = case_when(
    !is.na(.$q10_11) ~ q10_4,
    !is.na(.$q11_11) ~ q11_4,
    !is.na(.$q12_11) ~ q12_4,
    !is.na(.$q13_11) ~ q13_4,
    !is.na(.$q14_11) ~ q14_4,
    !is.na(.$q15_11) ~ q15_4
  ),
  agree_certainty_t1 = case_when(
    !is.na(.$q10_11) ~ q10_5,
    !is.na(.$q11_11) ~ q11_5,
    !is.na(.$q12_11) ~ q12_5,
    !is.na(.$q13_11) ~ q13_5,
    !is.na(.$q14_11) ~ q14_5,
    !is.na(.$q15_11) ~ q15_5
  ),
  belief_t1 = case_when(
    !is.na(.$q10_11) ~ q10_6,
    !is.na(.$q11_11) ~ q11_6,
    !is.na(.$q12_11) ~ q12_6,
    !is.na(.$q13_11) ~ q13_6,
    !is.na(.$q14_11) ~ q14_6,
    !is.na(.$q15_11) ~ q15_6
  ),
  belief_certainty_t1 = case_when(
    !is.na(.$q10_11) ~ q10_7,
    !is.na(.$q11_11) ~ q11_7,
    !is.na(.$q12_11) ~ q12_7,
    !is.na(.$q13_11) ~ q13_7,
    !is.na(.$q14_11) ~ q14_7,
    !is.na(.$q15_11) ~ q15_7
  ),
  agree_t2 = case_when(
    !is.na(.$q10_11) ~ q10_11,
    !is.na(.$q11_11) ~ q11_11,
    !is.na(.$q12_11) ~ q12_11,
    !is.na(.$q13_11) ~ q13_11,
    !is.na(.$q14_11) ~ q14_11,
    !is.na(.$q15_11) ~ q15_11
  ),
  belief_t2 = case_when(
    !is.na(.$q10_11) ~ q10_12,
    !is.na(.$q11_11) ~ q11_12,
    !is.na(.$q12_11) ~ q12_12,
    !is.na(.$q13_11) ~ q13_12,
    !is.na(.$q14_11) ~ q14_12,
    !is.na(.$q15_11) ~ q15_12
  ),
  bias_1 = case_when(
    !is.na(.$q10_11) ~ q10_13,
    !is.na(.$q11_11) ~ q11_13,
    !is.na(.$q12_11) ~ q12_13,
    !is.na(.$q13_11) ~ q13_13,
    !is.na(.$q14_11) ~ q14_13,
    !is.na(.$q15_11) ~ q15_13
  ),
  bias_2 = case_when(
    !is.na(.$q10_11) ~ q10_14,
    !is.na(.$q11_11) ~ q11_14,
    !is.na(.$q12_11) ~ q12_14,
    !is.na(.$q13_11) ~ q13_14,
    !is.na(.$q14_11) ~ q14_14,
    !is.na(.$q15_11) ~ q15_14
  ),
  bias_3 = case_when(
    !is.na(.$q10_11) ~ q10_15_1,
    !is.na(.$q11_11) ~ q11_15_1,
    !is.na(.$q12_11) ~ q12_15_1,
    !is.na(.$q13_11) ~ q13_15_1,
    !is.na(.$q14_11) ~ q14_15_1,
    !is.na(.$q15_11) ~ q15_15_1
  ),
  bias_4 = case_when(
    !is.na(.$q10_11) ~ q10_16_1,
    !is.na(.$q11_11) ~ q11_16_1,
    !is.na(.$q12_11) ~ q12_16_1,
    !is.na(.$q13_11) ~ q13_16_1,
    !is.na(.$q14_11) ~ q14_16_1,
    !is.na(.$q15_11) ~ q15_16_1
  ),
  partisanship = case_when(
    q8_2 == 1 ~ "Republican",
    q8_2 == 2 ~ "Democrat",
    (q8_2 == 3)|(q8_2 == 4) ~ "Moderate"
  ))

mydata <- mydata %>%
  mutate(agree_t1 = as.integer(.$agree_t1),
         belief_t1 = as.integer(.$belief_t1),
         agree_certainty_t1 = as.integer(.$agree_certainty_t1),
         belief_certainty_t1 = as.integer(.$belief_certainty_t1),
         agree_t2 = as.integer(.$agree_t2),
         belief_t2 = as.integer(.$belief_t2),
         bias_1 = as.integer(.$bias_1),
         bias_2 = as.integer(.$bias_2))

mydata <- mydata %>%
  mutate(certainty_agree = case_when(
    agree_certainty_t1 %in% c(1,2) ~ "Certain",
    agree_certainty_t1 %in% c(3,4) ~ "Uncertain"
  ),
  certainty_belief = case_when(
    belief_certainty_t1 %in% c(1,2) ~ "Certain",
    belief_certainty_t1 %in% c(3,4) ~ "Uncertain"
  ))

mydata$agree_diff <- mydata$agree_t2 - mydata$agree_t1
mydata$belief_diff <- mydata$belief_t2 - mydata$belief_t1

mydata %>%
  group_by(partisanship, valence, media_slant) %>%
  summarise(
    n = n(),
    agree_t1 = mean(agree_t1),
    agree_t2 = mean(agree_t2),
    belief_t1 = mean(belief_t1),
    belief_t2 = mean(belief_t2),
    bias_1 = mean(bias_1),
    bias_2 = mean(bias_2)
  )

mydata_democrat <- mydata %>% filter(partisanship == 2)
mydata_republican <- mydata %>% filter(partisanship == 1)
mydata_moderate <- mydata %>% filter(partisanship == 3 | partisanship == 4)

mydata_democrat %>%
  # group_by(valence) %>%
  group_by(valence, media_slant) %>%
  summarise(
    n = n(),
    agree_t1 = mean(agree_t1),
    agree_t2 = mean(agree_t2),
    belief_t1 = mean(belief_t1),
    belief_t2 = mean(belief_t2),
    bias_1 = mean(bias_1),
    bias_2 = mean(bias_2)
  )

mydata_republican %>%
  # group_by(valence) %>%
  group_by(valence, media_slant) %>%
  summarise(
    n = n(),
    agree_t1 = mean(agree_t1),
    agree_t2 = mean(agree_t2),
    belief_t1 = mean(belief_t1),
    belief_t2 = mean(belief_t2),
    bias_1 = mean(bias_1),
    bias_2 = mean(bias_2)
  )

mydata_moderate %>%
  # group_by(valence) %>%
  group_by(valence, media_slant) %>%
  summarise(
    n = n(),
    agree_t1 = mean(agree_t1),
    agree_t2 = mean(agree_t2),
    belief_t1 = mean(belief_t1),
    belief_t2 = mean(belief_t2),
    bias_1 = mean(bias_1),
    bias_2 = mean(bias_2)
  )


mydata_democrat %>%
  group_by(valence, certainty_agree) %>%
  # group_by(valence, media_slant) %>%
  summarise(
    n = n(),
    agree_t1 = mean(agree_t1),
    agree_t2 = mean(agree_t2),
    bias_1 = mean(bias_1),
    bias_2 = mean(bias_2)
    )

mydata_democrat %>%
  group_by(valence, certainty_agree) %>%
  # group_by(valence, media_slant) %>%
  summarise(
    n = n(),
    belief_t1 = mean(belief_t1),
    belief_t2 = mean(belief_t2),
    bias_1 = mean(bias_1),
    bias_2 = mean(bias_2)
  )

mydata_democrat %>%
  group_by(valence, media_slant) %>%
  summarise(
    n = n(),
    agree_t1 = mean(agree_t1),
    agree_t2 = mean(agree_t2),
    belief_t1 = mean(belief_t1),
    belief_t2 = mean(belief_t2),
    bias_1 = mean(bias_1),
    bias_2 = mean(bias_2)
  )

mydata %>% select(media_slant, attention_2, attention_3, attention_4) %>% print(n = nrow(.))
