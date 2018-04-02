print_trials <- function(trial, prize_door, chosen_door) {
  print(paste0("Trial ", trial, ":"))
  print(paste("Prize door:", prize_door))
  print(paste("Initial chosen door:", chosen_door))
  
  if (chosen_door == prize_door) {
    print("Staying would win here")
  } else {
    print("Switching would win here")
  }
  
  print("==========")
}

simulate_trials <- function(num_trials = 10000, print_details = FALSE){
  switch_wins <- 0
  stay_wins <- 0
  
  for (trial in 1:num_trials) {
    
    possibilities <- 1:3
    prize_door <- sample(possibilities, 1)
    chosen_door <- sample(possibilities, 1)
    
    if (prize_door == chosen_door) {
      host_opens <- sample(possibilities[-prize_door], 1)
    } else if (prize_door != chosen_door) {
      host_opens <- possibilities[-c(prize_door, chosen_door)]
    }
    
    switch_door <- possibilities[-c(host_opens, chosen_door)]
    if (print_details) {
      print_trials(trial, prize_door, chosen_door)
    }
    
    switch_wins <- switch_wins + (prize_door == switch_door)
    stay_wins <- stay_wins + (prize_door == chosen_door)
  }
  
  plot_data <- round(c(stay_wins, switch_wins)/num_trials * 100, 2)
  b <- barplot(plot_data,
               main = "Monty Hall Problem",
               xlab = "Stay/Switch Choice", ylab = "Percent Wins (%)")
  axis(1, at = b, c("Stay", "Switch"))
  text(b, 3, paste0(plot_data, "%"))
  
  print(paste("Results for", num_trials, "trials:"))
  print(paste0("Switching would result in ", switch_wins, " wins (", 
               round(switch_wins/num_trials * 100, 2),  "%)"))
  print(paste0("Staying would result in ", stay_wins, " wins (", 
               round(stay_wins/num_trials * 100, 2),  "%)"))
}