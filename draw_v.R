library(tidyverse)
library(psycho)
library(ggjoy)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(vowels)
library(MLeval)
library(phonR)
library(lme4)
library(lmerTest)
library(phonTools)
library(ROCit)
library(pROC)


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
           TRUE ~ 'CVC'),) ->
  my_vowels_raw

my_vowels_raw$left_cons = paste(my_vowels_raw$poa_prev,
                                my_vowels_raw$moa_prev)
my_vowels_raw$right_cons = paste(my_vowels_raw$poa_next,
                                my_vowels_raw$moa_next)


my_vowels_raw %>%
  mutate(left = case_when(
    str_detect(left_cons, "labial stop") ~ "b,p",
    str_detect(left_cons, "labial nasal") ~ "m",
    str_detect(left_cons, "labial glide") ~ "w",
    str_detect(left_cons, "dental stop") ~ "d,t",
    str_detect(left_cons, "dental lateral") ~ "l",
    str_detect(left_cons, "labial fricative") ~ "f",
    str_detect(left_cons, "dental thrill") ~ "r",
    str_detect(left_cons, "dental nasal") ~ "n",
    str_detect(left_cons, "dental fricative") ~ "s",
    str_detect(left_cons, "velar stop") ~ "g,k",
    str_detect(left_cons, "palatal affricate") ~ "c,j",
    str_detect(left_cons, "palatal glide") ~ "y",
    str_detect(left_cons, "palatal nasal") ~ "ɲ",
    str_detect(left_cons, "velar nasal") ~ "ŋ",
    str_detect(left_cons, "labiovelar stop") ~ "kp",
    str_detect(left_cons, "labiovelar nasal") ~ "ŋm",
    TRUE ~ "non"),
    right = case_when(str_detect(right_cons, "labial stop") ~ "b|p",
    str_detect(right_cons, "labial nasal") ~ "m",
    str_detect(right_cons, "labial glide") ~ "w",
    str_detect(right_cons, "dental stop") ~ "d,t",
    str_detect(right_cons, "dental lateral") ~ "l",
    str_detect(right_cons, "labial fricative") ~ "f",
    str_detect(right_cons, "dental thrill") ~ "r",
    str_detect(right_cons, "dental nasal") ~ "n",
    str_detect(right_cons, "dental fricative") ~ "s",
    str_detect(right_cons, "velar stop") ~ "g,k",
    str_detect(right_cons, "palatal affricate") ~ "c,j",
    str_detect(right_cons, "palatal glide") ~ "y",
    str_detect(right_cons, "palatal nasal") ~ "ɲ",
    str_detect(right_cons, "velar nasal") ~ "ŋ",
    str_detect(right_cons, "labiovelar stop") ~ "kp",
    str_detect(right_cons, 'glottal stop') ~ 'ʔ',
    str_detect(right_cons, "labiovelar nasal") ~ "ŋm",
    TRUE ~ "non"),
    height = case_when(str_detect(phoneme, 'i') ~ 'close',
                       str_detect(phoneme, 'ɩ') ~ 'close',
                       str_detect(phoneme, 'u') ~ 'close',
                       str_detect(phoneme, 'ʋ') ~ 'close',
                       str_detect(phoneme, 'e') ~ 'mid',
                       str_detect(phoneme, 'o') ~ 'mid',
                       str_detect(phoneme, 'ɔ') ~ 'mid-open',
                       str_detect(phoneme, 'ɛ') ~ 'mid-open',
                       TRUE ~ 'open'),
    backness =case_when(str_detect(phoneme, 'i') ~ 'front',
                        str_detect(phoneme, 'ɩ') ~ 'front',
                        str_detect(phoneme, 'u') ~ 'back',
                        str_detect(phoneme, 'ʋ') ~ 'back',
                        str_detect(phoneme, 'e') ~ 'front',
                        str_detect(phoneme, 'o') ~ 'back',
                        str_detect(phoneme, 'ɔ') ~ 'back',
                        str_detect(phoneme, 'ɛ') ~ 'front',
                        TRUE ~ 'central'),) ->
  my_vowels_raw

my_vowels_raw %>% 
  group_by(person) %>% 
  mutate(f1lobanov = normLobanov(f1), 
         f2lobanov = normLobanov(f2))->
  my_vowels_raw

my_vowels_raw %>% 
  group_by(person) %>% 
  mutate(f1nearey = normNearey2(f1), 
         f2nearey = normNearey2(f2))->
  my_vowels_raw


my_vowels_raw %>% 
  ggplot(aes(backness, f2nearey, fill = moa_next))+
  geom_boxplot()+
  labs(x = "Influence of the manner of articulation of
       the right context", y = "")+
  facet_wrap(~position_vowel)

my_vowels_raw %>% 
  ggplot(aes(f2lobanov, f1lobanov, fill = backness))+
  geom_boxplot()+
#  facet_grid(rows = left_cons, cols = right_cons, margins = TRUE)+
  labs(x = "Influence of the manner of articulation of
       left context", y = "")+
  facet_wrap(~position_vowel)


#distr consonants in CVC
my_vowels_raw%>%
  filter(str_detect(position_vowel, "CVС")) -> 
  my_vowels_raw
View(my_vowels_raw)
my_vowels_raw%>%
  filter(str_detect(left_cons, "velar stop")) -> 
  my_vowels_raw_g

my_vowels_raw%>%
  filter(str_detect(position_vowel, 'CVC')) -> 
  my_vowels_raw

my_vowels_raw_a%>%
  filter(!str_detect(poa_prev, 'nasal')) -> 
  my_vowels_raw_a

my_vowels_raw_iatr%>%
  filter(str_detect(left_cons, 'velar stop')) -> 
  my_vowels_raw_iatr


my_vowels_raw%>%
  filter(str_detect(right_cons, 'non')) ->
  my_vowels_raw_cv
my_vowels_raw_oa%>%
  filter(str_detect(right_cons, 'velar')) ->
  my_vowels_raw_oa

my_vowels_raw_a %>%
  ggplot(aes(f2nearey, f1nearey, color = position_vowel, label = phoneme)) +
  geom_text(alpha = 0.7) +
  stat_ellipse()+
  #geom_density2d()+
  scale_y_reverse() +
  scale_x_reverse() +
  theme(axis.text.x = element_text(angle=90), text=element_text(family="Times", face="bold", size=8)) +
  labs(x = "F2", y = "F1", color = "Vowel space for /a/")
  #facet_wrap(~person)


medians <- my_vowels_raw_a %>%
  group_by(phoneme, person, left_cons, right_cons)%>%
  summarise(median_f1 = median(f1),
            median_f2 = median(f2))%>%
  print(medians)

vc <- my_vowels_raw_cv %>% 
  distinct(word, poa_prev, moa_prev) %>% 
  count(word, poa_prev, moa_prev, sort = TRUE) %>% 
  print(40)


sds <- my_vowels_raw_a %>%
  group_by(phoneme, person, left_cons, right_cons)%>%
  summarise(sd_f1 = sd(f1),
            sd_f2 = sd(f2))%>%
  print(sds)




my_vowels_raw_u%>%
  filter(!str_detect(f2, "1968")) -> 
  my_vowels_raw_u


my_vowels_raw_a%>%
  filter(!str_detect(right, "non")) -> 
  my_vowels_raw_a

my_vowels_raw%>%
  filter(!str_detect(left, "g")) -> 
  my_vowels_raw

normed_vow = subset(my_vowels_raw, select = c(person, phoneme, row, f1, f2))
normed_vow[,"gl.F1"] <- NA
normed_vow[,"gl.F2"] <- NA
normed_vow = subset(normed_vow, select = c(person, phoneme, row, f1, f2, gl.F1, gl.F2))
names(normed_vow)[names(normed_vow) == 'phoneme'] <- 'vowel.frame'
names(normed_vow)[names(normed_vow) == 'row'] <- 'context'
normed_vow <- as.data.frame(normed_vow)
t = scalevowels(normed_vow, 'no.f3s' == T)

ggplot(my_vowels_raw, aes(x = f2, y = f1, color = poa_next, label = )) + 
  geom_text(alpha = 0.8) + 
  stat_ellipse() + 
  scale_x_reverse() + scale_y_reverse()+
  labs(x = "Formant space for /ɛ/, Hz", y = "")+
  facet_wrap(~person, scales = "free")

ggplot(my_vowels_raw, aes(x = f2lobanov, y = f1lobanov, color = poa_next, label = left)) + 
  geom_point(alpha = 0.8) + 
  stat_ellipse() + 
  scale_x_reverse() + scale_y_reverse()+
  labs(x = "Formant space for /i/", y = "")+
  facet_wrap(~phoneme, scales = "free")



my_vowels_raw%>%
  mutate(poa_prev = factor(poa_prev, levels = c("labial", "dental",
                                              'palatal', 'velar', 'labiovelar', 'glottal'))) -> 
  my_vowels_raw



my_vowels_raw%>%
  mutate(poa_next = factor(poa_next, levels = c("labial", "dental",
                                                'palatal', 'velar', 'labiovelar', 'glottal'))) -> 
  my_vowels_raw

fit_1 <- lmer(f1nearey ~ left_cons + right_cons + (1|utter), 
              data=my_vowels_raw)
summary(fit_1)
vcov(fit_1)

fit_1 <- lmer(f1nearey ~ right_cons + (1|utter), 
              data=my_vowels_raw_i)
summary(fit_1)

fit_2 <- lmer(f1nearey ~ poa_prev + poa_next + (1|utter), 
              data=my_vowels_raw_i)
fit_3 <- aov(f1nearey ~ poa_prev + poa_next , 
              data=my_vowels_raw_i)
plot(fit_1)

anova(fit_1, fit_2, fit_3)
vcov(fit_1)
fixef(fit_1)
ranef(fit_1)

ggplot(my_vowels_raw_o, aes(x = f2nearey, y = f1nearey, color = phoneme)) + 
  #mutate(poa_prev = factor(poa_prev, levels = c("labial", "dental",'palatal', 'velar', 'labiovelar')))+
  facet_grid(moa_prev ~ moa_next, margins = T )+
  geom_point(alpha = 0.8) + 
  stat_ellipse(level = 0.67) + 
  scale_x_reverse() + scale_y_reverse()+
  labs(x = "Formant space for /o/. Manner of articulation", y = "")
  #facet_wrap(~person, scales = "free")

ggplot(my_vowels_raw_a, aes(x = f2, y = f1, color = moa_next, label = moa_prev)) + 
  geom_text(alpha = 0.6) + 
  stat_ellipse(level = 0.67) + 
  scale_x_reverse() + scale_y_reverse()+
  labs(x = "Formant space for /a/. Various manner of articulation", y = "")+
  facet_wrap(~person, scales = "free")


ggplot(my_vowels_raw_u, aes(x = f2, y = f1, color = position_vowel)) + 
  geom_point(alpha = 0.7) + 
  stat_ellipse(level = 0.67) + 
  scale_x_reverse() + scale_y_reverse()+
  labs(x = "distribution between CV, VC and CVC contexts, Hz", y = "")+
  facet_wrap(~person, scales = "free")


my_vowels_raw_oa%>%
  filter(!str_detect(f2, '1968.244')) -> 
  my_vowels_raw_oa


my_vowels_raw_i%>%
  filter(str_detect(left_cons, "dental thrill")) -> 
  my_vowels_raw_i

my_vowels_raw%>%
  filter(str_detect(phoneme, 'ɔ')) ->
  my_vowels_raw_at




my_vowels_raw$phoneme <- as.factor(my_vowels_raw$phoneme)
my_vowels_raw$poa_prev <- as.factor(my_vowels_raw$poa_prev)
my_vowels_raw$moa_prev <- as.factor(my_vowels_raw$moa_prev)
my_vowels_raw$poa_next <- as.factor(my_vowels_raw$poa_next)
my_vowels_raw$moa_next <- as.factor(my_vowels_raw$moa_next)
my_vowels_raw$right <- as.factor(my_vowels_raw$right)
my_vowels_raw$word <- as.factor(my_vowels_raw$word)
my_vowels_raw$person <- as.factor(my_vowels_raw$person)
my_vowels_raw$utter <- as.factor(my_vowels_raw$utter)



fit_unnorm <- glmer(phoneme ~ f1+f2 + (1+f1+f2|utter) 
               + (1+f1+f2|person), data = my_vowels_raw, 
               family = 'binomial')
summary(fit_unnorm)

fit_nearey <- glmer(phoneme ~ f1nearey+f2nearey + (1+f1nearey+f2nearey|utter) 
                  , data = my_vowels_raw, 
                    family = 'binomial')
summary(fit_nearey)
a

plot(ggpredict(fit_unnorm, terms = c("f2 [sample = 8]", "f1 [sample = 8]")))



fit_norm_l <- glmer(phoneme ~ f1lobanov+f2lobanov + (1+f1lobanov+f2lobanov|utter), data = my_vowels_raw, 
                    family = 'binomial')

fit_norm_n <- glmer(phoneme ~ f1nearey+f2nearey + (1+f1nearey+f2nearey|utter), data = my_vowels_raw, 
                  family = 'binomial')

anova(fit_norm_l, fit_unnorm, fit_nearey)
summary(fit_norm_n)
plot(fit_norm)
plot(fit_unnorm)

fit_poa <- glmer(right ~ f1+f2 + (1+f1+f2|utter) 
                 + (1+f1+f2|person), data = my_vowels_raw_i, 
               family = 'binomial')
summary(fit_poa)

tapply(f1, phoneme, mean)

summary(aov(word~phoneme, data = my_vowels_raw_i))
summary(aov(f2lobanov~phoneme, data = my_vowels_raw))
summary(aov(f2~right, data = my_vowels_raw_i))
summary(aov(f2~poa_next, data = my_vowels_raw_i))

summaryBy(weightgain ~ type + source, data = weightgain,
          FUN = c(mean, sd, length))

t<- lmer(f2 ~ poa_next*poa_prev + (1|utter) + (1|person), data = my_vowels_raw)
summary(t)


fit_poa <- glmer(f1+f2 ~ poa_prev + moa_prev + poa_next + moa_next + (1|utter) 
               + (1|person), data = my_vowels_raw_i, 
               family = 'binomial')
summary(fit_poa)




fit_4 <- glmer(phoneme ~ norm_f1+norm_f2 + (1+norm_f1+norm_f2|utter) 
               , data = my_vowels_raw_i, 
               family = 'binomial')
summary(fit_4)

library(ggeffects)
plot(ggpredict(fit_3, terms = c("f1[all]", "f2[all]")))
ggpredict(fit_3, terms = c('f1[all]', 'f2[all]'))

library(ggeffects)

predictions_norm_f2 <-ggpredict(fit_norm, terms = c("f2lobanov [sample = 9]", "f1lobanov [sample = 9]"))
plot(predictions_norm_f2)
predictions_norm_f1 <-ggpredict(fit_norm, terms = c("f1lobanov [sample = 9]", "f2lobanov [sample = 9]"))
pROC_obj_norm <- roc(predictions_norm_f2$predicted, predictions_norm_f2$x, 
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(pROC_obj_norm)
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")



predictions_unnorm_f2 <-ggpredict(fit_unnorm, terms = c("f2 [sample = 9]", "f1 [sample = 9]"))
predictions_unnorm_f1 <-ggpredict(fit_unnorm, terms = c("f1 [sample = 9]", "f2 [sample = 9]"))

print(predictions_unnorm_f2)
pROC_obj_unnorm <- roc(predictions_unnorm_f2$predicted, predictions_unnorm_f2$x, 
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(pROC_obj_unnorm)
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")






plot(ggpredict(fit_unnorm, terms = c("f2 [sample = 9]", "f1 [sample = 9]")))

print(ggpredict(fit_norm, terms = c("f2lobanov", "f1lobanov")))
plot(ggeffect(fit_unnorm, terms = c("f2lobanov [sample = 9]", "f1lobanov [sample = 9]")))

my_vowels_raw_a%>%
  filter(str_detect(phoneme, "a")) -> 
  my_vowels_raw_a



library(ggpubr)

my_vowels_raw_i_for_poa%>%
  ggplot(aes(f2, f1, color = poa_next, label = left)) +
  geom_text() +
  scale_y_reverse() +
  scale_x_reverse() +
  theme(axis.text.x = element_text(angle=90), text=element_text(family="Times", face="bold", size=10)) +
  labs(x = "F2 (Hz)", y = "F1 (Hz)", color = "Village")+
  facet_wrap(~person, scales = "free")


my_vowels_raw_a %>%
  ggplot(aes(f2, f1, color = right, label = left)) +
  geom_text() +
  scale_y_reverse() +
  scale_x_reverse() +
  theme(axis.text.x = element_text(angle=90), text=element_text(family="Times", face="bold", size=10)) +
  labs(x = "F2 (Hz)", y = "F1 (Hz)", color = "Manner of articulation")+
  facet_wrap(~person, scales = "free")


#  facet_wrap(~person, scales = "free")

