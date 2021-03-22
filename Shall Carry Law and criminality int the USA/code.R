###########################################################
# Econometric analysis of a Crime Panel Data
# Econometrics project - M1 S2 - MAr 2021
###########################################################

rm(list = ls())
required_libs = c('dplyr', 'ggplot2', 'AER', 'plm', 'corrplot')
load_pack = function(lib = required_libs)
{
   if(!require(lib, character.only = TRUE)) install.packages(lib)
   library(lib, character.only = TRUE)
}
lapply(required_libs, load_pack); rm(required_libs)




# Data description
?AER::Guns

# Load data
data("Guns")
guns = Guns
rm(Guns)

# Explore
head(guns)
summary(guns)

# Panel data
guns = pdata.frame(guns, index = c('state', 'year'), drop.index=TRUE, row.names=TRUE)
guns %>% head




'Because all variables are expressed as rates of population we can ignore the population variable'
'We also focus on violent crimes in general and thus exclude murders and robbery variables'
guns = guns[, c(1,4,5,6,7,9,10,11)]

# Check NAs
sapply(guns, function(x) sum(is.na(x)))

# Make law a dummy variable
guns[,8] = ifelse(guns[,8] == 'yes', 1, 0) %>% as.logical()




# Look at relationships
attach(guns)
# lin-lin
sapply(colnames(guns)[-1], function(var) {
   eval(parse(text = paste0(
      "plot(violent ~ ", var, ")"
   )))
})
# log-lin
sapply(colnames(guns)[-1], function(var) {
   eval(parse(text = paste0(
      "plot(log(violent) ~ ", var, ")"
   )))
})
# log-log
sapply(colnames(guns)[-1], function(var) {
   eval(parse(text = paste0(
      "plot(log(violent) ~ log(", var, "))"
   )))
})

'violent seems to be linear in some variables but we detect potential heteroskdasticity'
'log log seems to be good excepting for cauc and law'




# Correlations
guns_cor = guns
colnames(guns_cor) = colnames(guns_cor) %>% substr(1, 7)
for(i in 2:ncol(guns_cor)) {
   if(colnames(guns_cor)[i] %in% colnames(guns_cor)[1:i-1]) {
      colnames(guns_cor)[i] = colnames(guns_cor)[i] %>%
         paste0("*")
   }
}
corrplot(cor(guns_cor), type = 'upper', order = 'original', 
         method = 'circle', tl.pos = "d", tl.col = 9)
corrplot(cor(guns_cor), add = TRUE, type = "lower", method = "number", 
         order = "original", diag = F, tl.pos = "n", cl.pos = "n")
rm(guns_cor)

'all are correlated, male and law a bit less'




model = log(violent) ~ log(prisoners) + log(afam) + log(cauc) + log(male) + log(income) + log(density) + law

# OLS
ols.fit = lm(model, data = guns)
summary(ols.fit)

# Between
btw_i.fit = plm(model, data = guns, model = 'between', effect = 'indiv')
summary(btw_i.fit)
btw_t.fit = plm(model, data = guns, model = 'between', effect = 'time')
summary(btw_t.fit)

# within
within_i.fit = plm(model, data = guns, model = 'within', effect = 'indiv')
summary(within_i.fit)
within_t.fit = plm(model, data = guns, model = 'within', effect = 'time')
summary(within_t.fit)
within_it.fit = plm(model, data = guns, model = 'within', effect = 'twoways')
summary(within_it.fit)

# GLS (effet aléatoire)
gls_i.fit = plm(model, data = guns, model = 'random', effect = 'indiv')
summary(gls_i.fit)
gls_t.fit = plm(model, data = guns, model = 'random', effect = 'time')
summary(gls_t.fit)
gls_it.fit = plm(model, data = guns, model = 'random', effect = 'twoways')
summary(gls_it.fit)



# Tests effets infividuels
pFtest(model, data = guns, effect = "indiv")
plmtest(model, data = guns, effect = "indiv", type = "bp")

# Tests effets temporels
pFtest(model, data = guns, effect = "time")
plmtest(model, data = guns, effect = "time", type = "bp")

# Tests effets individuels et temporels
pFtest(model, data = guns, effect = "twoways")
plmtest(model, data = guns, effect = "twoways", type = "bp")

'-> les deux effets sont significatifs donc Within i+t et GLS plus appropriés'

# Test d'Hausman
phtest(within_i.fit, gls_i.fit)
phtest(within_t.fit, gls_t.fit)
phtest(within_it.fit, gls_it.fit)

'-> les effets sont non indépendants : corrélés aux variables explicatives
Donc within est le meilleur estimateur à considérer'


'les effets'


# Plots
attach(guns)
data(Guns)
'mean violent evo in time'
'violent boxplot vs law'
boxplot(violent)
plot(Guns$state, violent)

Guns %>% group_by(year) %>%
   summarise(avg = mean(violent)) %>%
   ggplot(aes(year, avg)) +
   geom_point(size = 2) +
   ggthemes::theme_clean() +
   xlab('') + ylab('Average Rate') +
   ggtitle('Evolution of the Average Violent Crime Rates in the U.S.') 

Guns %>% group_by(state) %>% 
   ggplot(aes(reorder(state, violent, median), violent)) +
   geom_boxplot() +
   ggthemes::theme_clean() +
   xlab('States') + ylab('Crime Rate') + 
   theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
   ggtitle('Distribution of Violent Crime Rates by State') 



