################################################################################
# ECONOMETRICS MASTER 1 APE S1                       
# Happiness and Alcohol Consumption across Countries
# Corentin Lobet                                     
# 13/12/2020                               
# Github link : https://github.com/FractalySyn/Alcohol_and_Happiness
################################################################################


# Load packages -----------------------------------------------------------
rm(list = ls())
required_libs = c('dplyr', 'ggplot2', 'corrplot', 'knitr', 'lmtest', 'readr', 'sandwich')
load_pack = function(lib = required_libs)
{
   if(!require(lib, character.only = TRUE)) install.packages(lib)
   library(lib, character.only = TRUE)
}
lapply(required_libs, load_pack); rm(required_libs)


# Download and clean Data --------------------------------------------------------------------

### Download and Explore data
gini_all = read_csv('https://raw.githubusercontent.com/FractalySyn/Master-Works/main/Alcohol%20effect%20on%20aggregate%20happiness/data/Gini_WB.csv', skip= 4) 
hp_alc_2016 = read_csv('https://github.com/FractalySyn/Master-Works/raw/main/Alcohol%20effect%20on%20aggregate%20happiness/data/wh2016.csv')
head(hp_alc_2016)



### Clean gini
# Fix different country names issue in the gini dataset 
wrong_ctry = c('Slovak Republic', 'Kyrgyz Republic', 'North Macedonia', 'Egypt, Arab Rep.',
               'Korea, Dem. People’s Rep.', 'Venezuela, RB', 'Congo, Rep.', 'Congo, Dem. Rep.',
               'Syrian Arab Republic')
right_ctry = c('Slovakia', 'Kyrgyzstan', 'Macedonia', 'Egypt', 'South Korea', 'Venezuela',
               'Rep. Congo', 'Dem. Rep. Congo', 'Syria')
for(i in 1:length(right_ctry))
{
   gini_all[which(gini_all$`Country Name` == wrong_ctry[i]), "Country Name"] = right_ctry[i]
}
# Replace gini index by the mean of 2010-2019, also allows to get more observations since some 2016 data are missing
both_ctry = inner_join(gini_all %>% rename(Country = 'Country Name'), hp_alc_2016, by = 'Country') %>% .$'Country'
gini_all = gini_all[which(gini_all$`Country Name` %in% both_ctry),] %>% .[, c('Country Name', as.character(2010:2019))]
gini_all[, 'GI'] = gini_all[, as.character(2010:2019)] %>% apply(1, mean, na.rm = T)
# Remove NAs and keep useful variables
gini = gini_all %>% dplyr::select('Country Name', 'GI') %>%
   rename(Country = 'Country Name') %>%
   filter(!is.na(GI))



### Clean HP ALC data
hp_alc = hp_alc_2016 %>% 
   mutate(AC = Beer_PerCapita + Wine_PerCapita*2.5 + Spirit_PerCapita*8) %>%
   select(Country, Region, HappinessScore, HDI, GDP_PerCapita, AC)
# Transform regions to binary variables
regions = hp_alc_2016 %>% dplyr::select(Region) %>% unique() %>%
   as.data.frame() %>% .[,1]
for(i in 1:length(regions))
{
   hp_alc = hp_alc %>% mutate(col = ifelse(Region == regions[i], 1, 0)) 
   colnames(hp_alc)[which(colnames(hp_alc) == 'col')] = regions[i]
}
# Clean
hp_alc = hp_alc %>% rename(HS = HappinessScore, GDP = GDP_PerCapita, WE = `Western Europe`,
                           NAm = `North America`, ANZ = `Australia and New Zealand`, 
                           MENA = `Middle East and Northern Africa`, LAC = `Latin America and Caribbean`,
                           SA = `Southeastern Asia`, CEE = `Central and Eastern Europe`, 
                           EA = `Eastern Asia`, SSA = `Sub-Saharan Africa`) %>%
   select(-Region)



### Merge data
data = inner_join(hp_alc, gini, by = 'Country')
rm(gini, gini_all, hp_alc, hp_alc_2016, both_ctry, i,
   regions, right_ctry, wrong_ctry)

# Some Countries gdp haven't been divided by 100
data = data %>% mutate(GDP = ifelse(GDP > 150, GDP/1000, GDP))

# Look at dummy regions variables
data[, 6:14] %>% apply(2, sum)
'We can see that some of these regions are not very common in the dataset, we could merge some'
mean_gdp = c(); for(i in 6:14){
   mean_gdp[i-5] = (sum(data[,i] * data$GDP) / sum(data[, i])) 
}
mean_gdp %>% data.frame(mean_gdp = ., region = data[, 6:14] %>% colnames())
# developped countries : Western Europe, North America, Asutralia and New Zealand
# developping countries : Middle East and Northern Africa, Latin America and Caribbean, Eastern Asia, Central and Eastern Europe
# poor countries : Sub-Saharan Africa, Southeastern Asia
data = data %>% mutate(Rich = WE + NAm + ANZ,
                       Medium = MENA + LAC + EA + CEE,
                       Poor = SSA + SA)

'Keeping Rich and Poor will suffice as they are excluding'
merged = data %>% select(Country, HS, AC, GDP, HDI, GI, Rich, Poor)

write.csv(merged, 'data/merged.csv', row.names = F)


# Correlations ------------------------------------------------------------

continuous = merged %>% select(HS, AC, GDP, HDI, GI)
corrplot(cor(continuous), type = 'upper', order = 'alphabet', 
         method = 'circle', tl.pos = "d", tl.col = 9)
corrplot(cor(continuous), add = TRUE, type = "lower", 
         method = "number", order = "alphabet", diag = F, tl.pos = "n", cl.pos = "n")
rm(continuous, mean_gdp)


# Functions ---------------------------------------------------------------

format_data = function(data, y_name, x_names)
{
   dta = data[, c(y_name,x_names)]; names = c()
   for(l in 2:ncol(dta)) names[l-1] = paste0('x', l-1)
   colnames(dta) = c('y', names)
   dta = as.matrix(dta)
   return(dta)
}

ols = function(matrix_data, x_names)
{
   x = cbind(1, matrix_data[, 2:ncol(matrix_data)])
   y = matrix_data[, 1]
   
   # Compute OLS "beta = (X'X)^-1 * (X'Y)"
   beta = solve(t(x)%*%x) %*% t(x)%*%y
   
   # Compute residuals
   res = y - x%*%beta
   
   # Compute standard errors
   "VCM = S^2(e) * (X'X)^-1 = (1/(n-k)) * (e'e) * (X'X)^-1"
   n = nrow(x); k = ncol(x)
   vcm = (1/(n-k)) * as.numeric(t(res) %*% res) * solve(t(x)%*%x)
   se = sqrt(diag(vcm))
   
   # Compute t-tests
   t_tests = beta / se
   
   # Compute p-values
   pvals = 2*pt(abs(beta/se), df = n-k, lower.tail = FALSE)
   signif = rep('***', length(pvals))
   for(i in 1:length(pvals))
   {
      if(pvals[i] > 0.1) signif[i] = ''
      if(between(pvals[i], 0.05, 0.1)) signif[i] = '*'
      if(between(pvals[i], 0.01, 0.05)) signif[i] = '**'
   }
   
   # Results
   results = data.frame(Beta = round(beta, 3), SE = round(se, 3), t = round(t_tests, 1),
                        'p-value' = pvals, Significance = signif)
   rownames(results) = c('Intercept', x_names)
   
   # R-squared
   rss = t(res)%*%res / (n-1); tss = sum((y - mean(y))^2) / (n-1)
   r_sq = 1 - (rss / tss)
   adj_r = 1 - (rss / tss) * (n-1) / (n-k)
   
   # Fisher test "(R1-R0)/(1-R1)*(n-k)/J" J : betas (k-1), R0 R² with constraints 
   r_non_constraint = 0 # no features
   f_stat = (r_sq - r_non_constraint) / (1 - r_sq) * (n-k) / (k-1)
   f_pval = pf(f_stat, k-1, n-k, lower.tail = FALSE)
   
   # Stats
   stats = data.frame('R²' = r_sq, 'adj-R²' = adj_r,
                      'Fisher' = f_stat, 'p-value' = f_pval)
   
   return(list(residuals = res,
               Coefficients = results,
               Stats = stats))
}

ols_robust = function(matrix_data, x_names)
{
   x = cbind(1, matrix_data[, 2:ncol(matrix_data)])
   y = matrix_data[, 1]
   
   # Compute OLS "beta = (X'X)^-1 * (X'Y)"
   beta = solve(t(x)%*%x) %*% t(x)%*%y
   
   # Compute residuals
   res = y - x%*%beta
   
   # Compute HC-robust standard errors
   omega <- matrix(0, length(res), length(res))
   for (i in 1:length(res)) omega[i,i] <- res[i]^2 
   "VCM = S^2(e) * (X'X)^-1 = (1/(n-k)) * (e'e) * (X'X)^-1"
   n = nrow(x); k = ncol(x)
   vcm = solve(t(x)%*%x) %*% t(x)%*%omega%*%x %*% solve(t(x)%*%x)
   se = sqrt(diag(vcm))
   
   # Compute t-tests
   t_tests = beta / se
   
   # Compute p-values
   pvals = 2*pt(abs(beta/se), df = n-k, lower.tail = FALSE)
   signif = rep('***', length(pvals))
   for(i in 1:length(pvals))
   {
      if(pvals[i] > 0.1) signif[i] = ''
      if(between(pvals[i], 0.05, 0.1)) signif[i] = '*'
      if(between(pvals[i], 0.01, 0.05)) signif[i] = '**'
   }
   
   # Results
   results = data.frame(Beta = round(beta, 3), SE = round(se, 3), t = round(t_tests, 1),
                        'p-value' = pvals, Significance = signif)
   rownames(results) = c('Intercept', x_names)
   
   # R-squared
   rss = t(res)%*%res / (n-1); tss = sum((y - mean(y))^2) / (n-1)
   r_sq = 1 - (rss / tss)
   adj_r = 1 - (rss / tss) * (n-1) / (n-k)
   
   # Fisher test "(R1-R0)/(1-R1)*(n-k)/J" J : betas (k-1), R0 R² with constraints 
   r_non_constraint = 0 # no features
   f_stat = (r_sq - r_non_constraint) / (1 - r_sq) * (n-k) / (k-1)
   f_pval = pf(f_stat, k-1, n-k, lower.tail = FALSE)
   
   # Stats
   stats = data.frame('R²' = r_sq, 'adj-R²' = adj_r,
                      'Fisher' = f_stat, 'p-value' = f_pval)
   
   return(list(residuals = res,
               Coefficients = results,
               Stats = stats))
}

test_HC = function(matrix_data, type = 'BP', fit) # type = BP|white, returns TRUE if heteroskedasticity
{
   names = c(); for(l in 2:ncol(matrix_data)) names[l-1] = paste0('x', l-1)
   dta = matrix_data; colnames(dta) = c('y', names); dta = as.matrix(dta) 
   temp = dta
   
   # Add squared and crossed features, the loop computes every cross-product among Xs and add them to dta
   if(type == 'white')
   {
      for(i in 2:ncol(temp))
      {
         for(j in i:ncol(temp))
         {
            dta = dta %>% cbind(temp[, names[i-1]] * temp[, names[j-1]])
            colnames(dta)[ncol(dta)] = paste0('x', i+2*j)
         }
      }
   }
   
   # Auxiliary regressions for HC tests
   sq_res = fit$residuals ^ 2
   res_data = cbind(sq_res, dta[, 2:ncol(dta)])
   colnames(res_data) = c('sq_res', colnames(dta)[-1])
   aux_fit = ols(res_data, colnames(dta)[-1])
   
   # Compute the test
   n = nrow(dta)
   k = ifelse(type == 'BP', ncol(temp), ncol(dta))
   statistic = n * aux_fit$Stats$`R²`
   pvalue = 1 - pchisq(statistic, k-1)
   
   # Results
   hc_results = data.frame('Auxiliary R²' = statistic/n, 'N' = n, 
                           'Test Statistic' = statistic, 'p-value' = pvalue)
   conclusion = ifelse(pvalue < 0.05, TRUE, FALSE)
   
   return(list('HC Test' = hc_results, 'Conclusion' = conclusion))
}

econometric_analysis = function(data, y_name, x_names, hc_test_type = 'BP')
{
   new_data = format_data(data, y_name, x_names)
   ols_fit = ols(new_data, x_names)
   hc = test_HC(new_data, type = hc_test_type, ols_fit)
   ols_hc = ols_robust(new_data, x_names)
   return(list('OLS Estimates' = ols_fit$Coefficients, 
               'OLS Goodness of fit' = ols_fit$Stats, 
               'Heteroskedasticity test' = hc, 
               'OLS with Heteroskedasticity robust SEs' = ols_hc$Coefficients))
}

# Test functions with Boston
Boston = MASS::Boston
econometric_analysis(Boston, 'medv', c('lstat', 'crim'), hc_test_type = 'white')
econometric_analysis(Boston, 'medv', 'lstat')




# Econometric Analysis ----------------------------------------------------

### Visualize relationships
'Alcohol, increasing relationship'
merged %>% ggplot(aes(AC, HS)) + geom_point() + geom_smooth(se = F, method = 'lm')
'GDP, strong positive correlation, note that it is used for the Hap score computation'
merged %>% ggplot(aes(log(GDP), HS)) + geom_point() + geom_smooth(se = F, method = 'lm')
'HDI, strong positive correlation'
merged %>% ggplot(aes(HDI, HS)) + geom_point() + geom_smooth(se = F, method = 'lm')
'Gini'
merged %>% ggplot(aes(GI, HS)) + geom_point() + geom_smooth(se = F, method = 'lm')

merged = merged %>% mutate(lGDP = log(GDP)) %>% select(-GDP)

### Model
'Specification : HS = a + b1.AC + b2.ln(GDP) + b3.HDI + b4.GI + DummyRegions'
econometric_analysis(merged, 'HS', c('AC', 'lGDP', 'HDI', 'GI', 'Rich', 'Poor'), 'BP')
lm(HS~., data = merged[, -1]) %>% coeftest(x=., vcov = vcovHC, type = 'HC1')










