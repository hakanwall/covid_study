library(tidyverse)
library(lme4)
library(kableExtra)


#Explore transition from any sports betting to online casino games before --> during, during --> second wave
data_analyses <- data_analyses %>% mutate(any_online_casino_b = if_else((game_b1 == 1 | game_b2 == 1 | game_b3 == 1),1,0),
                 any_sports_betting_b =if_else((game_b7 == 1 | game_b8 == 1 | game_b9 == 1),1,0),
                 any_online_casino_d = if_else((game_d1 == 1 | game_d2 == 1 | game_d3 == 1),1,0),
                 any_sports_betting_d =if_else((game_d7 == 1 | game_d8 == 1 | game_d9 == 1),1,0),
                 any_online_casino_s = if_else((game_s1 == 1 | game_s2 == 1 | game_s3 == 1),1,0),
                 any_sports_betting_s =if_else((game_s7 == 1 | game_s8 == 1 | game_s9 == 1),1,0))

# Tabulate game types before, during and second wave

data_analyses %>% count(across(starts_with(c("any_")))) %>% kableExtra::kable(digits = 2, ) %>% kableExtra::kable_styling()

# Check prop which started gambling on any online casino games
data_analyses %>% count(any_online_casino_b == 0 & any_sports_betting_b == 1 & any_sports_betting_d == 0 & any_online_casino_d == 1) %>% kableExtra::kable(digits = 2, ) %>% kableExtra::kable_styling()


data_analyses %>% count(any_online_casino_b == 0 & (any_online_casino_d == 1 | any_online_casino_s == 1)) %>% kableExtra::kable(digits = 2, ) %>% kableExtra::kable_styling()

# binary variable, diff in level of gambling problems during and before, more = 1, same or less = 0

data_analyses <- covid %>% mutate(diff_gamprob_d_b = if_else(gampro_d - gampro_b > 0,1,0))



## Logistic regression, PGSI 5+ as outcome

fit_model_1 <- glm(formula = pgsi_cut_off_five ~ age + sex + neg_corona_rest_iso + neg_corona_rest_ec + any_riskgame  + worry1_ec + worry1_ph + worry1_me, data = data_analyses, family = binomial)

summary(fit_model_1)

round(exp(fit_model_1$coefficients),2) %>% kableExtra::kable() %>% kableExtra::kable_styling()

round(exp(confint(fit_model_1)),2) %>% kableExtra::kable() %>% kableExtra::kable_styling()

round(coef(summary(fit_model_1))[,4],5) %>% kableExtra::kable() %>% kableExtra::kable_styling()



#Characteristics of those increasing, maintaining and decreasing gambling
# First wave
data_analyses %>% filter(!is.na(bet_max_diff_d_b)) %>% mutate(cat_bet = cut(bet_max_diff_d_b, breaks = c(-Inf,-1,1,Inf), labels = c("minskade","oförändrat","ökade"))) %>% group_by(cat_bet) %>% summarise(mean(na.omit(sex)), mean(na.omit(age)), mean(na.omit(sum_pgsi)), mean(na.omit(started_riskgame_d)))

# Second wave
data_analyses %>% filter(!is.na(bet_max_diff_s_d)) %>% mutate(cat_bet = cut(bet_max_diff_s_d, breaks = c(-Inf,-1,1,Inf), labels = c("minskade","oförändrat","ökade"))) %>% group_by(cat_bet) %>% summarise(mean(na.omit(sex)), mean(na.omit(age)), mean(na.omit(sum_pgsi)), mean(na.omit(started_riskgame_s)))

#Check how first and second wave differs

data_analyses %>% mutate(first_wave_only = if_else(!is.na(date1) & is.na(date2),TRUE,FALSE)) %>% group_by(first_wave_only) %>% summarise(mean(na.omit(sum_pgsi)), mean(na.omit(sex)), mean(na.omit(age)))


data_analyses %>% mutate(first_wave_only = if_else(!is.na(date1) & is.na(date2),TRUE,FALSE)) %>% group_by(first_wave_only) %>% count(civil) %>% mutate(f = n/sum(n)*100)

data_analyses %>% mutate(first_wave_only = if_else(!is.na(date1) & is.na(date2),TRUE,FALSE)) %>% mutate(main_occ = cut(main_occ, breaks=c(-Inf,1,2,4,Inf), labels = c("1","2","3-4","5"))) %>% count(main_occ) %>% mutate(f = n/sum(n)*100)

# explore patterns among wave two participants

data_analyses %>% filter(!is.na(freq_max_diff_s_d)) %>% mutate(freq_max_diff_s_d = cut(freq_max_diff_s_d, breaks=c(-Inf,-1,0,Inf), labels = c("reduced","no change","increased"))) %>% group_by(freq_max_diff_s_d) %>% summarise(mean(na.omit(sex)), mean(na.omit(started_riskgame_s)), mean(na.omit(age)), mean(na.omit(stopped_riskgame_s)), mean(na.omit(continued_riskgame_s)), mean(na.omit(sum_pgsi)), n= n()) %>% mutate(f = n/sum(n)*100)

data_analyses %>% mutate(gamprob_diff = (gamprob_s - gampro_d), gamprob_diff = cut(gamprob_diff, breaks=c(-Inf,-1,0,Inf), labels = c("reduced","no change","increased"))) %>% filter(!is.na(gamprob_diff)) %>% group_by(gamprob_diff) %>% summarise(mean(na.omit(sex)), mean(na.omit(started_riskgame_s)), mean(na.omit(age)), mean(na.omit(stopped_riskgame_s)), mean(na.omit(continued_riskgame_s)), mean(na.omit(sum_pgsi)), n= n()) %>% mutate(f = n/sum(n)*100)


#Logistic regressions
# Max freq
model_logit_freq <- glm(formula = freq_diff ~ age + sex + neg_corona_rest_iso + neg_corona_rest_ec + any_riskgame + worry1_ec + worry1_ph + worry1_me, data = data_analyses, family =binomial)
summary(model_logit_freq)

#test for overdispersion, , if p <= 0.05, use quasibinominal link 

model_logit_freq_od <- glm(formula = freq_diff ~ age + sex + neg_corona_rest_iso + neg_corona_rest_ec + any_riskgame + worry1_ec + worry1_ph + worry1_me, data = data_analyses, family =quasibinomial())
pchisq(summary(model_logit_freq_od)$dispersion * model_logit_freq$df.residual, model_logit_freq$df.residual, lower = F)

round(exp(model_logit_freq$coefficients),2) %>% kableExtra::kable() %>% kableExtra::kable_styling()
round(exp(confint(model_logit_freq)),2) %>% kableExtra::kable() %>% kableExtra::kable_styling()
round(coef(summary(model_logit_freq))[,4],5) %>% kableExtra::kable() %>% kableExtra::kable_classic()


# Gamprob
model_logit_prob <- glm(formula = gampro_d_gt_one ~ age + sex + neg_corona_rest_iso + neg_corona_rest_ec + any_riskgame + worry1_ec + worry1_ph + worry1_me + gampro_b_gt_one, data = data_analyses, family =binomial)

#test for overdispersion, if p <= 0.05, use quasibinominal link 
model_logit_prob_od <- glm(formula = gampro_d_gt_one ~ age + sex + neg_corona_rest_iso + neg_corona_rest_ec + any_riskgame + worry1_ec + worry1_ph + worry1_me + gampro_b_gt_one, data = data_analyses, family =quasibinomial())
pchisq(summary(model_logit_prob_od)$dispersion * model_logit_prob$df.residual, model_logit_prob$df.residual, lower = F)

#Get coefficients
summary(model_logit_prob_od)
round(exp(model_logit_prob_od$coefficients),2) %>% kableExtra::kable() %>% kableExtra::kable_styling()
round(exp(confint(model_logit_prob_od)),2) %>% kableExtra::kable() %>% kableExtra::kable_styling()
round(coef(summary(model_logit_prob_od))[,4],5) %>% kableExtra::kable() %>% kableExtra::kable_styling()



## Prepare dataset for longitudinal analyses

bg_data <- data_analyses %>% dplyr::select(1,337,8:44)
baseline_measures <- data_analyses %>% dplyr::select(1,62:123,188,305,308,317)
measures_first_wave <- data_analyses %>% dplyr::select(1,3,4,45:61,124:187,189,190,191,306,309,318,324,336,started_riskgame_d,stopped_riskgame_d,continued_riskgame_d,neg_corona_rest_iso,neg_corona_rest_ec,max_freq_b) %>% mutate(spelpaus = if_else(spelpaus1>1,1,0))
measures_first_wave <- measures_first_wave %>% mutate(max_freq_d = max_freq_d - max_freq_b) %>% mutate(max_freq_d = if_else(max_freq_d >0,1,0))



measures_second_wave <- covid %>% dplyr::select(1,202,203,209:292,307,310,319,started_riskgame_s,stopped_riskgame_s,continued_riskgame_s,spelpaus_2, neg_corona_rest_iso_2,neg_corona_rest_ec_2,max_freq_d) %>% mutate(sum_pgsi = NA, pgsi_cut_off_five = NA, spelpaus = if_else(spelpaus2>1,1,0))
measures_second_wave <- measures_second_wave %>% mutate(max_freq_s = max_freq_s - max_freq_d) %>% mutate(max_freq_s = if_else(max_freq_s >0,1,0))


measures_first_wave <- measures_first_wave %>% rename_with(~gsub("_d", "_", .x, fixed = TRUE)) %>% rename_with(~gsub("1_", "_", .x, fixed = TRUE)) %>% rename(med = med1, max_freq = max_freq_, aggr_bet = aggr_bet_, date = date1, gamcom = gamcom1, gamprob = gampro_, started_riskgame = started_riskgame_, stopped_riskgame = stopped_riskgame_, continued_riskgame = continued_riskgame_)
measures_second_wave <- measures_second_wave %>% rename_with(~gsub("_s", "_", .x, fixed = TRUE))%>% rename_with(~gsub("2_", "_", .x, fixed = TRUE)) %>% rename(med = med2, max_freq = max_freq_, aggr_bet = aggr_bet_, date = date2, gamcom = gamcom2, gamprob = gamprob_, started_riskgame = started_riskgame_, stopped_riskgame = stopped_riskgame_, continued_riskgame = continued_riskgame_, neg_corona_rest_iso = neg_corona_rest_iso_2, neg_corona_rest_ec = neg_corona_rest_ec_2)
data_analyses_second_wave <- bind_rows(measures_first_wave,measures_second_wave)

data_analyses_second_wave <- data_analyses_second_wave %>% left_join(bg_data, by="X1") %>% group_by(X1) %>% arrange(X1,date)
data_analyses_second_wave <- data_analyses_second_wave %>% left_join(baseline_measures, by="X1") %>% group_by(X1) %>% arrange(X1,date)

data_analyses_second_wave <- data_analyses_second_wave %>% filter(X1 %in% covid$X1[covid$include_gambl_analyses_second_wave_bet == TRUE]) %>% mutate(timepoint = row_number())

data_analyses_second_wave <- data_analyses_second_wave %>% group_by(X1) %>% mutate(timepoint = row_number())

data_analyses_second_wave <- data_analyses_second_wave %>% mutate(gamprob = if_else(gamprob >1,1,0), gampro_b = if_else(gampro_b >1,1,0)) 

data_analyses_second_wave <- data_analyses_second_wave %>% mutate(any_riskgame = if_else((started_riskgame ==1 | continued_riskgame == 1),1,0))



# Longitudinal analyses

# Gambling problems past month
model_second_wave_gamprob <- glmer(formula = gamprob ~ timepoint + sex + any_riskgame + age + worry_me + worry_ec + worry_ph + neg_corona_rest_iso + neg_corona_rest_ec + gampro_b + (1|X1), data=data_analyses_second_wave, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
summary(model_second_wave_gamprob)

tmp <- as_tibble(round(coef(summary(model_second_wave_gamprob))[,],5))
tmp %>% mutate(EST = round(exp(Estimate),2), lw_CI = round(exp(Estimate - 1.96*`Std. Error`),2), hi_CI = round(exp(Estimate + 1.96*`Std. Error`),2), p = round(`Pr(>|z|)`,2)) %>% select(2:7) %>% kable() %>% kable_styling()

# increased gambling frequency 
model_second_wave_max_freq <- glmer(formula = max_freq ~ timepoint + sex + any_riskgame + age + worry_me + worry_ec + worry_ph + neg_corona_rest_iso + neg_corona_rest_ec + (1|X1), data=data_analyses_second_wave, family = binomial,control = glmerControl(optimizer = "bobyqa"), nAGQ=0)
summary(model_second_wave_max_freq)

tmp <- as_tibble(round(coef(summary(model_second_wave_max_freq))[,],5))

tmp %>% mutate(EST = exp(round(Estimate,2)), lw_CI = exp(round(Estimate - 1.96*`Std. Error`,2)), hi_CI = exp(round(Estimate + 1.96*`Std. Error`,2)), p = round(`Pr(>|z|)`,2)) %>% select(2:7) %>% kable() %>% kable_styling()


