data %>% glimpse()

data_2 <-
  data %>% filter(gender != "Other") 

data_2 %>% 
  group_by(gender) %>% 
  summarize(mean_depression = mean(dass_depression),
            sd = sd(dass_depression))

data_2 %>% 
  lm(dass_depression ~ gender, .) %>% 
  summary()

t.test(data_2$dass_depression ~ data_2$gender)


data_2 %>% 
  mutate(gender_male = ifelse(gender == "Male", 1, 0)) %>% 
  ggplot(aes(x = gender_male, y = dass_depression)) + 
  geom_jitter(aes(color = gender), height = .8, width = .01, alpha = .5) +
  geom_smooth(color = 'black', method = "lm", se = FALSE) +
  scale_x_continuous(breaks = c(0,1)) +
  theme(
        axis.ticks.x = element_blank(), 
        legend.position = 'bottom')
