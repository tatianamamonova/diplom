library(tidyverse)
library(ggjoy)
library(ggplot2)
library(hrbrthemes)
library(dplyr)

theme_set(theme_bw())

my_vowels_raw <- read_csv("D:/study/gin_vowels.csv")
my_vowels_raw %>%
  mutate(difference = f2 - f1,
         atr = factor(ifelse(str_detect(phoneme, "i|o|u|e|a|ə"), "+ATR", "-ATR")),
         grand_vowel = case_when(
           str_detect(phoneme, "i|ɩ") ~ "I",
           str_detect(phoneme, "o|ɔ") ~ "O",
           str_detect(phoneme, "e|ɛ") ~ "E",
           str_detect(phoneme, "u|ʋ") ~ "U",
           TRUE ~ "transparent"),
         position_vowel = case_when(
           str_detect(poa_next, 'none') ~ 'CV',
           str_detect(poa_prev, 'none') ~ 'VC',
           TRUE ~ 'CVC')) ->
  my_vowels_raw

#картинка с формантым пространством
ggplot(my_vowels_raw, aes(x = f2, y = f1, color = phoneme)) + 
  geom_point() + 
  stat_ellipse(level = 0.67) + 
  scale_x_reverse() + scale_y_reverse()+
  facet_wrap(~person, scales = "free")
  


#normalizing plot
ggplot(my_vowels_raw, aes(x = norm_f2, y = norm_f1, color = phoneme)) + 
  geom_point() + 
  stat_ellipse(level = 0.67) + 
  scale_x_reverse() + scale_y_reverse()
  #facet_wrap(~person, scales = "free")

  
#distr consonants in CVC
my_vowels_raw%>%
  filter(str_detect(position_vowel, "CVC")) -> 
  my_vowels_raw

my_vowels_raw %>% 
  distinct(word, moa_next, poa_next) %>% 
  count(moa_next, poa_next, sort = TRUE) %>% 
  print(distinct(word, moa_next, poa_next))
  ggplot(aes(poa_prev, n, label = n, fill = moa_prev)) +
  geom_col()+
  geom_text(aes(y = n + 1))+
  labs(x = "", y = "")

View(my_vowels_raw)

my_vowels_raw %>% 
  print(word) 

#poa_prev
ggplot(my_vowels_raw, aes(f2, f1, color = phoneme))+
  geom_point()+
  facet_grid(person~poa_prev, scales = "free")+
  stat_ellipse()+
  scale_x_reverse() + scale_y_reverse()+
  theme(legend.position = "bottom")+
  labs(x = "F2 (Hz)", y = "F1 (Hz)")

#moa_prev
ggplot(my_vowels_raw, aes(f2, f1, color = phoneme))+
  geom_point()+
  facet_grid(person~moa_prev, scales = "free")+
  stat_ellipse()+
  scale_x_reverse() + scale_y_reverse()+
  theme(legend.position = "bottom")+
  labs(x = "F2 (Hz)", y = "F1 (Hz)")

#the same for next context

#poa_next
ggplot(my_vowels_raw, aes(f2, f1, color = phoneme))+
  geom_point()+
  facet_grid(person~poa_next, scales = "free")+
  stat_ellipse()+
  scale_x_reverse() + scale_y_reverse()+
  theme(legend.position = "bottom")+
  labs(x = "F2 (Hz)", y = "F1 (Hz)")

#moa_next
ggplot(my_vowels_raw, aes(f2, f1, color = phoneme))+
  geom_point()+
  facet_grid(person~moa_next, scales = "free")+
  stat_ellipse()+
  scale_x_reverse() + scale_y_reverse()+
  theme(legend.position = "bottom")+
  labs(x = "F2 (Hz)", y = "F1 (Hz)")


ggplot(my_vowels_raw, aes(x = f2, y = f1, color = poa_prev)) + 
  geom_point() + 
  stat_ellipse(level = 0.67) + 
  scale_x_reverse() + scale_y_reverse()+
  labs(x = "distribution between i and ɩ in all contexts, Hz", y = "")+
  facet_wrap(~person, scales = "free")


#i and ɩ 
my_vowels_raw%>%
  filter(str_detect(grand_vowel, "I")) -> 
  my_vowels_raw

#all contexts
ggplot(my_vowels_raw, aes(x = f2, y = f1, color = phoneme)) + 
  geom_point() + 
  stat_ellipse(level = 0.67) + 
  scale_x_reverse() + scale_y_reverse()+
  labs(x = "distribution between i and ɩ in all contexts, Hz", y = "")+
  facet_wrap(~person, scales = "free")

#filter for CVC
my_vowels_raw%>%
  filter(!str_detect(poa_prev, "none")) -> 
  my_vowels_raw
my_vowels_raw%>%
  filter(!str_detect(poa_next, "none")) -> 
  my_vowels_raw

View(my_vowels_raw)

#distr in CVC
ggplot(my_vowels_raw, aes(x = f2, y = f1, color = phoneme)) + 
  geom_point() + 
  stat_ellipse(level = 0.67) + 
  scale_x_reverse() + scale_y_reverse()+
  labs(x = "distribution between i and ɩ in CVC contexts, Hz", y = "")+
  facet_wrap(~person, scales = "free")

#filter for VC and CV
filter(my_vowels_raw, poa_next == "none" | poa_prev == "none")-> 
  my_vowels_raw
  
ggplot(my_vowels_raw, aes(x = f2, y = f1, color = phoneme)) + 
  geom_point() + 
  stat_ellipse(level = 0.67) + 
  scale_x_reverse() + scale_y_reverse()+
  labs(x = "distribution between i and ɩ in VC and CV contexts, Hz", y = "")+
  facet_wrap(~person, scales = "free")



#distr in CV or VC
ggplot(my_vowels_raw, aes(x = f2, y = f1, color = phoneme)) + 
  geom_point() + 
  stat_ellipse(level = 0.67) + 
  scale_x_reverse() + scale_y_reverse()+
  labs(x = "distribution between i and ɩ in CV and VC contexts, Hz", y = "")+
  facet_wrap(~person, scales = "free")

#i in different position
my_vowels_raw%>%
  filter(str_detect(phoneme, "ɩ")) -> 
  my_vowels_raw

ggplot(my_vowels_raw, aes(x = f2, y = f1, color = position_vowel)) + 
  geom_point() + 
  stat_ellipse(level = 0.67) + 
  scale_x_reverse() + scale_y_reverse()+
  labs(x = "distribution of ɩ in CV, VC and CVC contexts, Hz", y = "")+
  facet_wrap(~person, scales = "free")


#look what is in prefix
my_vowels_raw%>%
  filter(str_detect(part_word, "pref")) -> 
  my_vowels_raw
View(my_vowels_raw)

#картинка с формантым пространством
ggplot(my_vowels_raw, aes(x = f2, y = f1, color = phoneme)) + 
  geom_point() + 
  stat_ellipse(level = 0.67) + 
  scale_x_reverse() + scale_y_reverse()+
  labs(x = "distribution between i and ɩ in all contexts", y = "")+
  facet_wrap(~person, scales = "free")



