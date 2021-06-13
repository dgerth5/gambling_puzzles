library(ggplot2)

comb = function(n, x) {
  factorial(n) / factorial(n-x) / factorial(x)
}


# win probabilities
tigers_win_prob = seq(0,1,0.05)


tigers_2way = function(prob, odds){
  
  # probabilities
  sweep = comb(4,0)*prob^4
  tig3 = comb(4,1)*prob^3*(1-prob)
  tie = comb(4,2)*prob^2*(1-prob)^2
  tig1 = comb(4,1)*prob*(1-prob)^3
  loseall = comb(4,0)*(1-prob)^4
  
  win = sweep + tig3 
  lose = tig1 + loseall
  
  # kelly size
  kelly = (odds*(win+.5*tie) - (1-win-.5*tie))/odds
  return(kelly)

}

(two_way_br_size = tigers_2way(tigers_win_prob,2))

tigers_3way = function(prob, odds_win, odds_tie){
  
  sweep = comb(4,0)*prob^4
  tig3 = comb(4,1)*prob^3*(1-prob)
  tie = comb(4,2)*prob^2*(1-prob)^2
  tig1 = comb(4,1)*prob*(1-prob)^3
  loseall = comb(4,0)*(1-prob)^4
  
  win = sweep + tig3 
  lose = tig1 + loseall
  
  # kelly size
  win_kelly = (odds_win*win - (1-win))/odds_win
  tie_kelly = (odds_tie*tie - (1-tie))/odds_tie

  df = data.frame(win_kelly,tie_kelly)
  return(df)

}

(three_way_br_size = tigers_3way(tigers_win_prob, 8.5,3.15))

# plot
df0 = data.frame(three_way_br_size,two_way_br_size, tigers_win_prob)
colnames(df0)[1] <- "win_3way"
colnames(df0)[2] <- "tie"
colnames(df0)[3] <- "win_2way"

ggplot(data = df0, aes(x = tigers_win_prob)) + 
  geom_line(aes(y = win_3way), color = "blue")+
  geom_line(aes(y = win_2way), color = "red")+
  geom_line(aes(y = tie), color = "black")+
  labs(x="Probability of Team Winning", y = "BR Percentage Risked")
