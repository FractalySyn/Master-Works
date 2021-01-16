###########################################################
# GAME THEORY : Common Value Auctions and Winner's Curse                          
# CVA simulations
# Corentin Lobet                                     
# 02/12/2020                               
###########################################################

# Load packages -----------------------------------------------------------

rm(list = ls())
required_libs = c('ggplot2', 'dplyr', 'gridExtra', 'ggthemes')
load_pack = function(lib = required_libs)
{
   if(!require(lib, character.only = TRUE)) install.packages(lib)
   library(lib, character.only = TRUE)
}
lapply(required_libs, load_pack); rm(required_libs)


# Game characteristics --------------------------------------------------------------------

### Computation functions
# Risk-Neutral Nash Equilibrium
RNNE = function(s, eps, N, V_range)
{
   if(s < min(V_range) + eps)
   {
      return(min(V_range) - (-s+min(V_range)-eps) / (N+1))
   }
   # else if(s > max(V_range) - eps)
   # {
   #    return(s + eps - ((max(V_range)-RNNE(max(V_range)-eps, eps, N, V_range)) * (exp(log(1) + N*0)) + 
   #                         2*N*eps*integrate(function(x){exp(log(1-x^N)+N*integrate(function(x){1/(1-x^N)}, -Inf, Inf))},
   #                                           0, (s-max(V_range)+eps)/(2*eps))))
   # }
   else
   {
      return(s - eps + (2*eps/(N+1)) * exp(-(N/(2*eps)) * (s - min(V_range) - eps)))
   }
}
# Discount rate
discount_rate = function(s, eps, b)
{
   return((s-b) / eps)
}
# Winner's Curse threshold
curse_value = function(s, eps, N)
{
   return(s - eps * (N-1) / (N+1))
}


# Game simulation ---------------------------------------------------------

cva_simulation = function(n_players = 2, value_range, signal_noise, wealth, reps,
                          exp_w_l = c(0,0), obs_w_l = c(0,0), dr_sd = 0.3)
{
   # n, noise, range, wealth
   N = n_players; V_range = value_range; eps = signal_noise
   w = list()
   
   dr = list(); payoffs = list(); req_dr = list(); inexp = c();
   curse_thres = list(); hyp_payoffs = list(); b = list(); rnne = c()
   for(per in 1:reps)
   {
      # true value
      V = sample(seq(V_range[1]+2*eps, V_range[2]-2*eps, by = 0.01), 1)
      
      # signals
      s = numeric(N)
      for(i in 1:N)
      {
         s[i] = sample(seq(V-eps, V+eps, by = 0.01), 1)
      }
      
      # discount rates, heterogeneous pouplation
      if(per == 1) dr[[per]] = rnorm(n = N, 0, dr_sd)
      else {
         roc = 100*payoffs[[per-1]] / V
         hyp_roc = 100*hyp_payoffs[[per-1]] / V
         dr[[per]] = dr[[per-1]]  - roc*ifelse(payoffs[[per-1]] >= 0, exp_w_l[1], exp_w_l[2]) -
                hyp_roc*ifelse(hyp_payoffs[[per-1]] >= 0, obs_w_l[1], obs_w_l[2])
      }
                  
      # winner's curse
      curse_thres[[per]] = sapply(s, curse_value, eps = eps, N = N)
      
      # RNNE discount rate
      req_dr[[per]] = sapply(curse_thres[[per]], discount_rate, s=s, eps=eps)[1,]
      
      # bids
      b[[per]] = -(dr[[per]]*eps) + s
      winner = which.max(b[[per]])
      
      # RNNE
      rnne[per] = sapply(s, RNNE, eps, N, V_range) %>% mean()
      
      # payoffs and wealth evo
      payoffs[[per]] = rep(0, N)
      hyp_payoffs[[per]] = V - (-(dr[[per]]*eps) + max(s))
      payoffs[[per]][winner] = hyp_payoffs[[per]][winner]
      for(k in 1:N) 
      {
         if(per == 1) w[[per]] = rep(wealth, N) + payoffs[[per]]
         else w[[per]] = w[[per-1]] + payoffs[[per]]
      }
      
      # Inexperience bidders prop
      inexp[per] = data.frame(bid = b[[per]], curse = curse_thres[[per]]) %>%
         mutate(inexp = ifelse(bid > curse, 1, 0)) %>%
         pull(inexp) %>% mean()
   }
   
   # Plots
   plt_wealth = sapply(w, mean) %>% data.frame(wealth = ., per = 1:reps) %>% 
      ggplot(aes(per, wealth)) +
      geom_line(col = 'blue', size = 2) +
      labs(title = 'Wealths Evolution', x = '', y = '')+
      ggthemes::theme_economist()
   plt_dr = sapply(dr, mean) %>% data.frame(discount_rate = ., per = 1:reps) %>% 
      ggplot(aes(per, discount_rate)) +
      geom_line(col = 'blue', size = 2)+
      labs(title = 'DR Evolution', x = '', y = '')+
      ggthemes::theme_economist()
   plt_b = data.frame(dif = 100*(sapply(b, mean)-rnne)/rnne, per = 1:reps,
                      dif2 = 100*(sapply(b, mean)-sapply(curse_thres, mean))/sapply(curse_thres, mean)) %>% 
      ggplot(aes(per, dif, col = 'blue')) +
      geom_line(size = 1.2)+
      geom_line(aes(per, dif2, col = 'red'), size = 1.2)+
      labs(title = 'Bids Evolution', x = '', y = '')+
      ggthemes::theme_economist() +
      theme(legend.title = element_text(vjust = 0), legend.text=element_text(size=10))+
      scale_color_identity(name = "", breaks = c("blue", "red"),
                           labels = c('Relative to RNNE', 'Relative to the Curse'),
                           guide = "legend")
   plt_inexp = data.frame(inexp = inexp*100, per = 1:reps) %>%
      ggplot(aes(per, inexp)) +
      geom_line(col = 'blue', size = 1.5)+
      labs(title = 'Inexperienced Proportion Evolution', x = '', y = '') +
      ggthemes::theme_economist() + ylim(c(0,100))
   for(i in 1:n_players)
   {
      plt_wealth = plt_wealth +
         geom_line(aes(x, y), col = i,
                   data = data.frame(y = as.data.frame(w) %>% t() %>% .[,i],
                                     x = 1:reps))
      plt_dr = plt_dr +
         geom_line(aes(x, y), col = i,
                   data = data.frame(y = as.data.frame(dr) %>% t() %>% .[,i],
                                     x = 1:reps))
   }
   grid.arrange(plt_wealth, plt_dr, plt_b, plt_inexp, nrow = 2)
}
cva_simulation(n_players = 10, value_range = c(1000, 2000), signal_noise = 100, 
               wealth = 5000, reps = 100, dr_sd = 0.2,
               exp_w_l = c(0.01, 0.08), obs_w_l = c(0.01, 0.07)) 
cva_simulation(n_players = 10, value_range = c(1000, 2000), signal_noise = 100, 
               wealth = 5000, reps = 100, dr_sd = 0.2,
               exp_w_l = c(0.01, 0.08), obs_w_l = c(0, 0)) 
"'Adding a bit of obs win effect reverses the curve so easily !!!'
'Bids evo plot shows that the curse tresh is below the RNNE with 10 players and low signal noise,
which is not the case with fewer players and lower signal noise. Thus to avoid the winner's curse, 
players should sometimes bid less than the RNNE"


cva_simulation(n_players = 5, value_range = c(50, 550), signal_noise = 12, 
               wealth = 20, reps = 30, dr_sd = 0.2,
               exp_w_l = c(0.01, 0.08), obs_w_l = c(0, 0.07)) 


# First plots --------------------------------------------------------------

"Refaire avec d'autres valeurs, signal moyen en référence, N>2 en référence"

V_range = c(200, 5000)
'RNNE'
# RNNE ~ N, by 0.1 to smooth the curve
sapply(seq(2, 10, by = 0.1), RNNE, s = 1000, eps = 200, V_range) %>%
   data.frame(RNNE = ., N = seq(2, 10, by = 0.1)) %>% 
   ggplot(aes(N, RNNE)) + geom_line() + 
   labs(x = 'Number of Players', title = 'Equilibrium vs Players')
# RNNE ~ eps
sapply(seq(5, 1000, by = 5), RNNE, s = 1000, N = 2, V_range) %>%
   data.frame(RNNE = ., eps = seq(5, 1000, by = 5)) %>% 
   ggplot(aes(eps, RNNE)) + geom_line() +
   labs(x = 'Signal Noise', title = 'Equilibrium vs Signal Noise')
# RNNE ~ s
sapply(seq(400, 5000, by = 25), RNNE, eps = 200, N = 2, V_range) %>%
   data.frame(RNNE = ., s = seq(400, 5000, by = 25)) %>% 
   ggplot(aes(s, RNNE)) + geom_line() +
   labs(x = 'Signal', title = 'Equilibrium vs Signal')

'Winners Curse'
# WC ~ N
sapply(seq(2, 100, by = 0.1), curse_value, s = 1000, eps = 200) %>%
   data.frame(wc = ., N = seq(2, 100, by = 0.1)) %>% 
   ggplot(aes(N, wc)) + geom_line() +
   scale_x_log10() + 
   labs(x = 'Number of Players (log)', y = "Winner's Curse Threshold", 
        title = "Winner's Curse Threshold vs Players") 
# WC ~ eps
sapply(seq(5, 1000, by = 5), curse_value, s = 1000, N = 2) %>%
   data.frame(wc = ., eps = seq(5, 1000, by = 5)) %>% 
   ggplot(aes(eps, wc)) + geom_line() +
   labs(x = 'Signal Noise', y = "Winner's Curse Threshold", 
        title = "Winner's Curse Threshold vs Signal Noise") 
# WC ~ s
sapply(seq(0, 5000, by = 20), curse_value, eps = 200, N = 2) %>%
   data.frame(wc = ., s = seq(0, 5000, by = 20)) %>% 
   ggplot(aes(s, wc)) + geom_line() +
   labs(x = 'Signal', y = "Winner's Curse Threshold", 
        title = "Winner's Curse Threshold vs Signal")

'Discount rate'
# DR ~ N, tend vers un DR max
sapply(sapply(seq(2, 30, by = 0.1), RNNE, s = 500, eps = 200, V_range),
       discount_rate, s = 1000, eps = 200) %>%
   data.frame(DR = ., N = seq(2, 30, by = 0.1)) %>%
   ggplot(aes(N, DR)) + geom_line() +
   scale_x_log10() + 
   labs(x = 'Number of Players (log)', y = "Discount Rate", 
        title = "Discount Rate vs Players") 















