starting_br = as.integer(readline(prompt = "Bankroll: "))
offer = as.integer(readline(prompt = "Sportsbook Offer: "))
init_bet = as.integer(readline(prompt = "Initial Bet: "))
profit = offer - init_bet
br_growth = profit / starting_br

odds = as.integer(readline(prompt = "What are the original fractional odds?: "))
estimated_prob = readline(prompt = "What is your estimated probability?: ")
estimated_prob = as.numeric(estimated_prob)
estimated_loss = 1 - estimated_prob
fn = function(x){estimated_loss*log(1-x) + estimated_prob*log(1+15*x)}
opt = optimize(fn, interval = c(0,1), maximum = 1)
f = fn(opt$maximum)
est_br_growth = exp(f) 


