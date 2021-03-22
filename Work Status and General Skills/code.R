###########################################################
# Work status ~ generals skills
# Econometrics project - M1 S2 - Feb 2021
###########################################################



# Set up environment ------------------------------------------------------

rm(list = ls())
required_libs = c('dplyr', 'ggplot2', 'readr', 'stringr', 'corrplot',
                  'MCMCpack', 'coda', 'tidyr', 'nnet')
load_pack = function(lib = required_libs)
{
   if(!require(lib, character.only = TRUE)) install.packages(lib)
   library(lib, character.only = TRUE)
}
lapply(required_libs, load_pack); rm(required_libs)



# Load Data ---------------------------------------------------------------

"locally" # the file must be in the same directory
piaac_fr = read_csv("data/prgfrap1.csv")

"from the internet"
# piaac_fr = fread("https://webfs.oecd.org/piaac/puf-data/CSV/prgfrap1.csv")



# Explore raw data ------------------------------------------------------------

'dimensions'
dim(piaac_fr) # 6993 people from France participated

'missing values'
# count missing values per column
# mv = apply(piaac_fr, 2, function(x) return(mean(is.na(x)))) %>%
#    tibble::as_tibble()  
# mv %>% .$value %>% hist(breaks = 50) 
mean(mv$value == 0)



# Choose variables ---------------------------------------------------------

vars_of_interest = c("C_D09", "GENDER_R", "AGE_R", "J_Q04a", "EDCAT6", "LNG_L1",
                     "PARED", "FNFAET12", "I_Q08", "DISP_CIBQ", 
                     paste0("PVNUM", 1:10), paste0("PVLIT", 1:10),
                     "READYTOLEARN", "READHOME", "WRITHOME", "ICTHOME", "NUMHOME")
explicit_names = c("work_status", "gender", "age", "native", "educ", "mother_tongue",
                   "parents_educ", "aet_last_y", "selfrep_health", "disabilities", 
                   paste0("numeracy_", 1:10), paste0("literacy_", 1:10),
                   "READYTOLEARN", "READHOME", "WRITHOME", "ICTHOME", "NUMHOME") %>% tolower

data = piaac_fr[, vars_of_interest]
colnames(data) = explicit_names
head(data)



# Prepare final data ------------------------------------------------------

piaac = data %>% 
   # keep job status of interest
   filter(work_status %in% paste(1:4)) %>%
   mutate(work_status = as.numeric(work_status)) %>%
   # change the the gender to female
   mutate(female = ifelse(gender == 2, 1, 0)) %>%
   select(-gender) %>%
   # choose a range for ages - is it relevant to include young people ???
   filter(age > 0) %>%
   # clean native
   filter(native %in% c('1', '2')) %>%
   mutate(native = ifelse(native == '1', 1, 0)) %>% 
   # clean educ 1: collège 2: lycée 3: diplome post-bac 4: master/doctorat
   filter(educ %in% paste(1:6)) %>%
   mutate(educ = ifelse(educ == 4 | educ == 5, 3, educ)) %>%
   mutate(educ = ifelse(educ == 6, 4, educ)) %>%
   # clean mother_tongue, create french
   filter(!is.na(mother_tongue)) %>%
   mutate(fr_mother_tongue = ifelse(mother_tongue == 'fra', 1, 0)) %>%
   select(-mother_tongue) %>%
   # clean parents_educ, create parents tertiary
   filter(parents_educ %in% paste(1:3)) %>%
   mutate(parents_tertiary = ifelse(parents_educ == 3, 1, 0)) %>%
   select(-parents_educ) %>%
   # clean AET (adult education training) students get 0 obviously, again consider excluding them
   filter(aet_last_y %in% c("0", "1", "A")) %>%
   mutate(aet_last_y = ifelse(aet_last_y == "1", 1, 0)) %>%
   # clean health
   filter(selfrep_health %in% paste(1:5)) %>%
   mutate(selfrep_health = as.numeric(selfrep_health)) %>%
   # modify disabilities : not if 1, 7/8/9 are language, reading&writing and mental disabilities
   mutate(disabilities = ifelse(disabilities == 1, 0, 1)) %>%
   # clean skills practice
   mutate_at(names(data)[31:35], function(x) {ifelse(x == "N", "0", x)}) %>%
   mutate_at(names(data)[31:35], as.numeric) %>%
   # clean introduced NAs and NAs in scores (same for each row since they are estimates of the same data)
   na.omit() 

# compute scores
numeracy = apply(piaac[, paste0("numeracy_", 1:10)], 1, mean)
literacy = apply(piaac[, paste0("literacy_", 1:10)], 1, mean)
piaac[, "numeracy"] = numeracy
piaac[, "literacy"] = literacy
piaac = piaac %>% 
   select(-paste0("numeracy_", 1:10)) %>%
   select(-paste0("literacy_", 1:10))

## We see that the few people having disabilities did not pass assessments
## Thus disabilities can be removed
piaac = piaac %>% select(-disabilities)

rm(data, piaac_fr, explicit_names, vars_of_interest)
write_csv(piaac, 'data/piaac_cleaned.csv')



# Exploratory Data Analysis -----------------------------------------------

piaac = read_csv('data/piaac_cleaned.csv')

## Corrplot
piaac_cor = piaac[, sapply(piaac, is.numeric)]
colnames(piaac_cor) = colnames(piaac_cor) %>% substr(1, 5)
for(i in 2:ncol(piaac_cor)) {
   if(colnames(piaac_cor)[i] %in% colnames(piaac_cor)[1:i-1]) {
      colnames(piaac_cor)[i] = colnames(piaac_cor)[i] %>%
         paste0("*")
   }
}
corrplot(cor(piaac_cor), type = 'upper', order = 'original', 
         method = 'circle', tl.pos = "d", tl.col = 9)
corrplot(cor(piaac_cor), add = TRUE, type = "lower", method = "number", 
         order = "original", diag = F, tl.pos = "n", cl.pos = "n")


## Distributions
# Work Statuses vs gender
piaac %>% mutate(gender = ifelse(female == 1, "female", "male")) %>%
   mutate(work = factor(work_status, levels = 1:4, 
            labels =  c('Working', 'ST unemployed', 'LT unemployed', 'Never worked'))) %>% 
   group_by(work, gender) %>%
   summarise(n = n()) %>%
      ggplot(aes(gender, n, fill = work)) +
      geom_bar(stat = 'identity', position = position_dodge2()) +
      ggthemes::theme_calc() +
      geom_text(aes(label=n), vjust=1.6, color="white", size=3.5, position = position_dodge2(0.9)) +
      scale_fill_brewer(palette="Set1") +
      ggtitle('Distribution of work statuses by gender') + ylab('') + xlab('') +
      theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
# work statuses vs origin
piaac %>% mutate(native = ifelse(native == 1, "native", "foreigner")) %>%
   mutate(work = factor(work_status, levels = 1:4, 
                        labels =  c('Working', 'ST unemployed', 'LT unemployed', 'Never worked'))) %>% 
   group_by(work, native) %>%
   summarise(n = n()) %>%
   ggplot(aes(native, n, fill = work)) +
   geom_bar(stat = 'identity', position = position_dodge2()) +
   ggthemes::theme_calc() +
   geom_text(aes(label=n), vjust=1.6, color="white", size=3.5, position = position_dodge2(0.9)) +
   scale_fill_brewer(palette="Set1") +
   ggtitle('Distribution of work statuses of natives and foreigners (log2 scale)') + ylab('') + xlab('') +
   theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
   scale_y_continuous(trans = 'log2')
# work statuses vs educational level
piaac %>% 
   mutate(work = factor(work_status, levels = 1:4, 
                        labels =  c('Working', 'ST unemployed', 'LT unemployed', 'Never worked'))) %>% 
   group_by(work, educ) %>%
   summarise(n = n()) %>%
   ggplot(aes(educ, n, fill = work)) +
   geom_bar(stat = 'identity', position = position_dodge2()) +
   ggthemes::theme_calc() +
   geom_text(aes(label=n), vjust=1.6, color="white", size=3.5, position = position_dodge2(0.9)) +
   scale_fill_brewer(palette="Set1") +
   ggtitle('Distribution of work statuses by educational level (log2 scale)') + ylab('') + 
   xlab('Educational level') +
   theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
   scale_y_continuous(trans = 'log2')
# Education
piaac %>% 
   group_by(educ) %>%
   summarise(n = n()) %>%
   ggplot(aes(educ, n)) +
   geom_bar(stat = 'identity', position = position_dodge2(), fill = 'turquoise4') +
   ggthemes::theme_calc() +
   geom_text(aes(label=n), vjust=1.6, color="white", size=3.5, position = position_dodge2(0.9)) +
   scale_fill_brewer(palette="Set1") +
   ggtitle('Distribution of educational levels') + ylab('') + xlab('') +
   theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) 


head(piaac)

## Stats
summary(piaac)
mean(piaac$work_status == 4)
mean(piaac$female == 1)
mean(piaac$native == 0)
mean(piaac$fr_mother_tongue == 1)
mean(piaac$fr_mother_tongue == 0 & piaac$native == 0)
mean(piaac$parents_tertiary == 1)
mean(piaac$educ == 4)
fBasics::basicStats(piaac$literacy)
mean(piaac$numeracy > 272)
mean(piaac$literacy > 275)
mean(piaac$numeracy > mean(piaac$literacy) - 2*sd(piaac$literacy))
mean(piaac$selfrep_health > 3)
fBasics::basicStats(piaac)



# Multinomial ordered probit ------------------------------------------------

piaac = read_csv('data/piaac_cleaned.csv')

head(piaac)
attach(piaac)


"Ordered probit 1"
# piaac_ordered_probit_fit_1 = MCMCoprobit(work_status ~ numeracy + literacy)
# save(piaac_ordered_probit_fit_1, file = "data/oprobit1.RData")
load("data/oprobit1.RData")
piaac_ordered_probit_fit_1 %>% summary %>% .[[1]] 
piaac_ordered_probit_fit_1 %>% summary %>% .[[1]] %>% as_tibble() %>%
   mutate(t = Mean/SD, p = pnorm(-abs(t)),
          var = piaac_ordered_probit_fit_1 %>% summary %>% .[[1]] %>% rownames()) 

'Probs at mean probit 1'
# get coefs
cf = piaac_ordered_probit_fit_1 %>% summary %>% .$statistics %>% .[,1]; cf
# mean values
avgs = sapply(piaac, mean)[15:16]; avgs
# compute c thresholds
c1 = -cf[1]
c2 = cf[4] - cf[1] 
c3 = cf[5] - cf[1]
# probs
p_y1 = pnorm(c1 - t(cf[2:3]) %*% avgs)
p_y2 = pnorm(c2 - t(cf[2:3]) %*% avgs) - pnorm(c1 - t(cf[2:3]) %*% avgs)
p_y3 = pnorm(c3 - t(cf[2:3]) %*% avgs) - pnorm(c2 - t(cf[2:3]) %*% avgs)
p_y4 = 1 - pnorm(c3 - t(cf[2:3]) %*% avgs)
# check
p_y1+p_y2+p_y3+p_y4
p_y1; p_y2; p_y3; p_y4

'Marginal effects probit 1'
me_y1 = -p_y1 %*% cf[2:3]; colnames(me_y1) = names(avgs); me_y1
me_y2 = -p_y2 %*% cf[2:3]; colnames(me_y2) = names(avgs); me_y2
me_y3 = -p_y3 %*% cf[2:3]; colnames(me_y3) = names(avgs); me_y3
me_y4 = (-p_y4+1) %*% cf[2:3]; colnames(me_y4) = names(avgs); me_y4



"Ordered probit 2"
# piaac_ordered_probit_fit_2 = MCMCoprobit(work_status ~ numeracy + literacy + numhome + icthome +
#                                           writhome + readhome + readytolearn)
# save(piaac_ordered_probit_fit_2, file = "data/oprobit2.RData")
load("data/oprobit2.RData")
piaac_ordered_probit_fit_2 %>% summary %>% .[[1]] 
piaac_ordered_probit_fit_2 %>% summary %>% .[[1]] %>% as_tibble() %>%
   mutate(t = Mean/SD, p = pnorm(-abs(t)),
          var = piaac_ordered_probit_fit_2 %>% summary %>% .[[1]] %>% rownames()) 

'Probs at mean probit 2'
# get coefs
cf = piaac_ordered_probit_fit_2 %>% summary %>% .$statistics %>% .[,1]; cf
# mean values
avgs = sapply(piaac, mean)[c(15, 16, 11, 10, 9, 8, 7)]; avgs
# compute c thresholds
c1 = -cf[1]
c2 = cf[9] - cf[1] 
c3 = cf[10] - cf[1]
# probs
p_y1 = pnorm(c1 - t(cf[2:8]) %*% avgs)
p_y2 = pnorm(c2 - t(cf[2:8]) %*% avgs) - pnorm(c1 - t(cf[2:8]) %*% avgs)
p_y3 = pnorm(c3 - t(cf[2:8]) %*% avgs) - pnorm(c2 - t(cf[2:8]) %*% avgs)
p_y4 = 1 - pnorm(c3 - t(cf[2:8]) %*% avgs)
# check
p_y1+p_y2+p_y3+p_y4
p_y1; p_y2; p_y3; p_y4

'Marginal effects probit 2'
me_y1 = -p_y1 %*% cf[2:8]; colnames(me_y1) = names(avgs); me_y1
me_y2 = -p_y2 %*% cf[2:8]; colnames(me_y2) = names(avgs); me_y2
me_y3 = -p_y3 %*% cf[2:8]; colnames(me_y3) = names(avgs); me_y3
me_y4 = (-p_y4+1) %*% cf[2:8]; colnames(me_y4) = names(avgs); me_y4






"Ordered probit 3"
# piaac_ordered_probit_fit_3 = MCMCoprobit(work_status ~ numeracy + literacy + numhome + icthome +
#                                             writhome + readhome + readytolearn + age + I(age^2) +
#                                             native + I(native*educ) + fr_mother_tongue + 
#                                             female + I(female*educ) + educ + parents_tertiary +
#                                             aet_last_y + selfrep_health)
# save(piaac_ordered_probit_fit_3, file = "data/oprobit3.RData")
load("data/oprobit3.RData")
piaac_ordered_probit_fit_3 %>% summary %>% .[[1]]
piaac_ordered_probit_fit_3 %>% summary %>% .[[1]] %>% as_tibble() %>%
   mutate(t = Mean/SD, p = pnorm(-abs(t)),
          var = piaac_ordered_probit_fit_3 %>% summary %>% .[[1]] %>% rownames())  %>%
   View

'Probs at mean probit 3'
# get coefs
cf = piaac_ordered_probit_fit_3 %>% summary %>% .$statistics %>% .[,1]; cf
# mean values
avgs = c()
avgs[c(1:8, 10, 12:13, 15:18)] = sapply(piaac, mean)[c(15, 16, 11, 10, 9, 8, 7, 2, 3, 13, 12, 4, 14, 5, 6)]
avgs[c(9, 11, 14)] = c(mean(age^2), mean(native*educ), mean(female*educ))
names(avgs) = names(cf)[2:19]; avgs
# compute c thresholds
c1 = -cf[1]
c2 = cf[20] - cf[1] 
c3 = cf[21] - cf[1]
# probs
p_y1 = pnorm(c1 - t(cf[2:19]) %*% avgs)
p_y2 = pnorm(c2 - t(cf[2:19]) %*% avgs) - pnorm(c1 - t(cf[2:19]) %*% avgs)
p_y3 = pnorm(c3 - t(cf[2:19]) %*% avgs) - pnorm(c2 - t(cf[2:19]) %*% avgs)
p_y4 = 1 - pnorm(c3 - t(cf[2:19]) %*% avgs)
# check
p_y1+p_y2+p_y3+p_y4
p_y1; p_y2; p_y3; p_y4

'Marginal effects probit 3'
me_y1 = -p_y1 %*% cf[2:19]; colnames(me_y1) = names(avgs); me_y1
me_y2 = -p_y2 %*% cf[2:19]; colnames(me_y2) = names(avgs); me_y2
me_y3 = -p_y3 %*% cf[2:19]; colnames(me_y3) = names(avgs); me_y3
me_y4 = (-p_y4+1) %*% cf[2:19]; colnames(me_y4) = names(avgs); me_y4

"Limite ordered ? tjr le même signe des effets marginaux 1-j-1, donc les femmes ont toujours 
une proba plus faible d'appartenir à une classe"


'Sexe + Scores'
# men
vals = avgs[3:18]; vals[c(11,12)] = 0; qs = seq(0.01, 1, 0.01); vals
quantiles = sapply(data.frame(numeracy, literacy), quantile, probs = seq(0.01, 1, 0.01))
vals = cbind(quantiles, sapply(vals, rep, 100))
men = pnorm(c1 - t(cf[2:19]) %*% t(vals))
# women
vals = avgs[3:18]; vals[11] = 1
vals = cbind(quantiles, sapply(vals, rep, 100))
women = pnorm(c1 - t(cf[2:19]) %*% t(vals))
# plot
data.frame(t(men), t(women), qs) %>%
   setNames(c('men', 'women', 'q')) %>%
   pivot_longer(cols = c('women', 'men')) %>%
   group_by(name) %>%
   ggplot(aes(q, value, color = name)) +
   geom_point(alpha = 0.3) + geom_smooth(se = F) +
   labs(x = 'Scores quantile', y = 'P(Y=1)', title = 'Probability to have a job by scores and sex') +
   ggthemes::theme_clean() +
   theme(plot.title = element_text(hjust = 0.5), legend.position = 'none') +
   geom_text(label = 'Females', x = 0.6, y = 0.65, col = 'turquoise3', size = 5) +
   geom_text(label = 'Males', x = 0.65, y = 0.77, col = 'tomato1', size = 5)
   
'Educ + scores'
# collège
vals = avgs[3:18]; vals[13] = 1; 
vals[9] = mean(1*native); vals[12] = mean(1*female); vals
vals = cbind(quantiles, sapply(vals, rep, 100))
educ1 = pnorm(c1 - t(cf[2:19]) %*% t(vals))
# bac
vals = avgs[3:18]; vals[13] = 2; 
vals[9] = mean(2*native); vals[12] = mean(2*female); vals
vals = cbind(quantiles, sapply(vals, rep, 100))
educ2 = pnorm(c1 - t(cf[2:19]) %*% t(vals))
# bac+3
vals = avgs[3:18]; vals[13] = 3; 
vals[9] = mean(3*native); vals[12] = mean(3*female); vals
vals = cbind(quantiles, sapply(vals, rep, 100))
educ3 = pnorm(c1 - t(cf[2:19]) %*% t(vals))
# bac+5/8
vals = avgs[3:18]; vals[13] = 4; 
vals[9] = mean(4*native); vals[12] = mean(4*female); vals
vals = cbind(quantiles, sapply(vals, rep, 100))
educ4 = pnorm(c1 - t(cf[2:19]) %*% t(vals))
# plot
data.frame(t(educ1), t(educ2), t(educ3), t(educ4), qs) %>%
   setNames(c('educ1', 'educ2', 'educ3', 'educ4', 'q')) %>%
   pivot_longer(cols = c('educ1', 'educ2', 'educ3', 'educ4')) %>%
   group_by(name) %>%
   ggplot(aes(q, value, color = name)) +
   geom_point(alpha = 0.3) + geom_smooth(se = F) +
   labs(x = 'Scores quantile', y = 'P(Y=1)', title = 'Probability to have a job by scores and education') +
   ggthemes::theme_clean() +
   theme(plot.title = element_text(hjust = 0.5), legend.position = 'none') +
   geom_text(label = 'lower secondary', x = 0.45, y = 0.59, col = 'tomato1') +
   geom_text(label = 'upper secondary', x = 0.5, y = 0.68, col = 'olivedrab') +
   geom_text(label = 'bachelor', x = 0.55, y = 0.765, col = 'turquoise3') +
   geom_text(label = 'master / PhD', x = 0.6, y = 0.835, col = 'purple')



# Multinomial independent logit ------------------------------------------------

piaac = read_csv('data/piaac_cleaned.csv')

head(piaac)
attach(piaac)

'logit 1'
piaac_logit_1 = multinom(work_status ~ numeracy + literacy, data = piaac)
piaac_logit_1 %>% summary
# Probas
piaac_logit_1 %>% predict(data.frame(numeracy = mean(numeracy),
                                     literacy = mean(literacy)),
                          "probs")

'logit 2'
piaac_logit_2 = multinom(work_status ~ numeracy + literacy + numhome + icthome +
                         writhome + readhome + readytolearn, data = piaac)
s = piaac_logit_2 %>% summary; s
(-((s$coefficients / s$standard.errors) %>% abs) %>% pnorm) %>% round(4)
# mean values
avgs = sapply(piaac, mean)[c(15, 16, 11, 10, 9, 8, 7)] %>% t() %>%
   data.frame(); avgs
# Probas
piaac_logit_2 %>% predict(avgs, "probs")

'logit 3'
piaac_logit_3 = multinom(work_status ~ numeracy + literacy + numhome + icthome +
                        writhome + readhome + readytolearn + age + I(age^2) +
                        native + I(native*educ) + fr_mother_tongue +
                        female + I(female*educ) + educ + parents_tertiary +
                        aet_last_y + selfrep_health, data = piaac)
s = piaac_logit_3 %>% summary; s
((-((s$coefficients / s$standard.errors) %>% abs) %>% pnorm) * 2) %>% round(4)
# mean values
avgs = c()
avgs[c(1:8, 10, 12:13, 15:18)] = sapply(piaac, mean)[c(15, 16, 11, 10, 9, 8, 7, 2, 3, 13, 12, 4, 14, 5, 6)]
avgs[c(9, 11, 14)] = c(mean(age^2), mean(native*educ), mean(female*educ))
avgs = t(avgs) %>% data.frame(); colnames(avgs) = names(cf)[2:19]
# Probas
piaac_logit_3 %>% predict(avgs, "probs")



j = 1

'Score + sex'
# men
vals = avgs[3:18]; vals[c(11,12)] = 0; qs = seq(0.01, 1, 0.01); vals
quantiles = sapply(data.frame(numeracy, literacy), quantile, probs = seq(0.01, 1, 0.01))
vals = cbind(quantiles, sapply(vals, rep, 100)) %>% as.data.frame()
men = predict(piaac_logit_3, vals, "probs")
# women
vals = avgs[3:18]; vals[11] = 1
vals = cbind(quantiles, sapply(vals, rep, 100)) %>% as.data.frame()
women = predict(piaac_logit_3, vals, "probs")
# plot
data.frame(y = women[,j], x = qs) %>% ggplot(aes(x,y)) +
   geom_point(col = 'red', alpha = 0.3) + geom_smooth(se = F, col = 'red') +
   geom_point(aes(x, y), data = data.frame(y = men[,j], x = qs), col = 'blue', alpha = 0.3) +
   geom_smooth(aes(x, y), data = data.frame(y = men[,j], x = qs), col = 'blue', se = F) +
   ggthemes::theme_economist_white() +
   labs(x = 'Scores quantile', y = 'P(Y=1)', title = 'Probability to have a job by scores and sex') +
   theme(plot.title = element_text(hjust = 0.5, vjust = 3), axis.title.y = element_text(vjust = 3),
         axis.title.x = element_text(vjust = -1)) +
   geom_text(label = 'Females', x = 0.62, y = 0.885, col = 'red') +
   geom_text(label = 'Males', x = 0.68, y = 0.926, col = 'blue')



j = 1

'Educ + scores'
# collège
vals = avgs[3:18]; vals[13] = 1; 
vals[9] = mean(1*native); vals[12] = mean(1*female); vals
vals = cbind(quantiles, sapply(vals, rep, 100)) %>% as.data.frame()
educ1 = predict(piaac_logit_3, vals, "probs")
# bac
vals = avgs[3:18]; vals[13] = 2; 
vals[9] = mean(2*native); vals[12] = mean(2*female); vals
vals = cbind(quantiles, sapply(vals, rep, 100)) %>% as.data.frame()
educ2 = predict(piaac_logit_3, vals, "probs")
# bac+3
vals = avgs[3:18]; vals[13] = 3; 
vals[9] = mean(3*native); vals[12] = mean(3*female); vals
vals = cbind(quantiles, sapply(vals, rep, 100)) %>% as.data.frame()
educ3 = predict(piaac_logit_3, vals, "probs")
# bac+5/8
vals = avgs[3:18]; vals[13] = 4; 
vals[9] = mean(4*native); vals[12] = mean(4*female); vals
vals = cbind(quantiles, sapply(vals, rep, 100)) %>% as.data.frame()
educ4 = predict(piaac_logit_3, vals, "probs")
# plot 
data.frame(y = educ1[,j], x = qs) %>% ggplot(aes(x,y)) +
   geom_point(col = 'red', alpha = 0.3) + geom_smooth(se = F, col = 'red') +
   geom_point(aes(x, y), data = data.frame(y = educ2[,j], x = qs), col = 'blue', alpha = 0.3) +
   geom_smooth(aes(x, y), data = data.frame(y = educ2[,j], x = qs), col = 'blue', se = F) +
   geom_point(aes(x, y), data = data.frame(y = educ3[,j], x = qs), col = 'forestgreen', alpha = 0.3) +
   geom_smooth(aes(x, y), data = data.frame(y = educ3[,j], x = qs), col = 'forestgreen', se = F) +
   geom_point(aes(x, y), data = data.frame(y = educ4[,j], x = qs), col = 'black', alpha = 0.3) +
   geom_smooth(aes(x, y), data = data.frame(y = educ4[,j], x = qs), col = 'black', se = F) +
   ggthemes::theme_economist_white() +
   labs(x = 'Scores quantile', y = 'P(Y=1)', title = 'Probability to have a job by scores and education') +
   theme(plot.title = element_text(hjust = 0.5, vjust = 3), axis.title.y = element_text(vjust = 3),
         axis.title.x = element_text(vjust = -1)) +
   geom_text(label = 'lower secondary', x = 0.45, y = 0.855, col = 'red') +
   geom_text(label = 'upper secondary', x = 0.5, y = 0.89, col = 'blue') +
   geom_text(label = 'bachelor', x = 0.55, y = 0.918, col = 'forestgreen') +
   geom_text(label = 'master / PhD', x = 0.6, y = 0.938, col = 'black')



'Score + native'
# men
vals = avgs[3:18]; vals[c(8,9)] = 0; qs = seq(0.01, 1, 0.01); vals
quantiles = sapply(data.frame(numeracy, literacy), quantile, probs = seq(0.01, 1, 0.01))
vals = cbind(quantiles, sapply(vals, rep, 100)) %>% as.data.frame()
foreigner = predict(piaac_logit_3, vals, "probs")
# women
vals = avgs[3:18]; vals[8] = 1
vals = cbind(quantiles, sapply(vals, rep, 100)) %>% as.data.frame()
native = predict(piaac_logit_3, vals, "probs")
# plot
j = 1
data.frame(y = native[,j], x = qs) %>% ggplot(aes(x,y)) +
   geom_point(col = 'red', alpha = 0.3) + geom_smooth(se = F, col = 'red') +
   geom_point(aes(x, y), data = data.frame(y = foreigner[,j], x = qs), col = 'blue', alpha = 0.3) +
   geom_smooth(aes(x, y), data = data.frame(y = foreigner[,j], x = qs), col = 'blue', se = F) +
   ggthemes::theme_economist_white() +
   labs(x = 'Scores quantile', y = 'P(Y=1)', title = 'Probability to have a job by scores and origin') +
   theme(plot.title = element_text(hjust = 0.5, vjust = 3), axis.title.y = element_text(vjust = 3),
         axis.title.x = element_text(vjust = -1)) +
   geom_text(label = 'Natives', x = 0.65, y = 0.92, col = 'red') +
   geom_text(label = 'Others', x = 0.6, y = 0.905, col = 'blue')
j = 2
data.frame(y = native[,j], x = qs) %>% ggplot(aes(x,y)) +
   geom_point(col = 'red', alpha = 0.3) + geom_smooth(se = F, col = 'red') +
   geom_point(aes(x, y), data = data.frame(y = foreigner[,j], x = qs), col = 'blue', alpha = 0.3) +
   geom_smooth(aes(x, y), data = data.frame(y = foreigner[,j], x = qs), col = 'blue', se = F) +
   ggthemes::theme_economist_white() +
   labs(x = 'Scores quantile', y = 'P(Y=2)', title = 'Probability of short unemployment by scores and origin') +
   theme(plot.title = element_text(hjust = 0.5, vjust = 3), axis.title.y = element_text(vjust = 3),
         axis.title.x = element_text(vjust = -1)) +
   geom_text(label = 'Natives', x = 0.62, y = 0.041, col = 'red') +
   geom_text(label = 'Others', x = 0.6, y = 0.034, col = 'blue')
j = 3
data.frame(y = native[,j], x = qs) %>% ggplot(aes(x,y)) +
   geom_point(col = 'red', alpha = 0.3) + geom_smooth(se = F, col = 'red') +
   geom_point(aes(x, y), data = data.frame(y = foreigner[,j], x = qs), col = 'blue', alpha = 0.3) +
   geom_smooth(aes(x, y), data = data.frame(y = foreigner[,j], x = qs), col = 'blue', se = F) +
   ggthemes::theme_economist_white() +
   labs(x = 'Scores quantile', y = 'P(Y=3)', title = 'Probability of long unemployment by scores and origin') +
   theme(plot.title = element_text(hjust = 0.5, vjust = 3), axis.title.y = element_text(vjust = 3),
         axis.title.x = element_text(vjust = -1)) +
   geom_text(label = 'Natives', x = 0.5, y = 0.044, col = 'red') +
   geom_text(label = 'Others', x = 0.6, y = 0.054, col = 'blue')
j = 4
data.frame(y = native[,j], x = qs) %>% ggplot(aes(x,y)) +
   geom_point(col = 'red', alpha = 0.3) + geom_smooth(se = F, col = 'red') +
   geom_point(aes(x, y), data = data.frame(y = foreigner[,j], x = qs), col = 'blue', alpha = 0.3) +
   geom_smooth(aes(x, y), data = data.frame(y = foreigner[,j], x = qs), col = 'blue', se = F) +
   ggthemes::theme_economist_white() +
   labs(x = 'Scores quantile', y = 'P(Y=4)', title = 'Never have worked probability by scores and origin') +
   theme(plot.title = element_text(hjust = 0.5, vjust = 3), axis.title.y = element_text(vjust = 3),
         axis.title.x = element_text(vjust = -1)) +
   geom_text(label = 'Natives', x = 0.45, y = 0.004, col = 'red') +
   geom_text(label = 'Others', x = 0.6, y = 0.009, col = 'blue')






