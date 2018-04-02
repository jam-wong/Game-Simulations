tile_count <- rep(0, 40)
tile_position <- 1
max_tile_position <- 40
num_turns <- 100000
doubles_rolled <- 0
in_jail <- FALSE
freedom_attempts <- 0

for (turn in seq(num_turns)) {
  doubles_rolled <- 0
  
  if (!in_jail) {
    repeat {
      dice_roll <- sample(6, 2, replace = TRUE)
      
      print(paste('turn', turn))
      print(paste('dice roll:', dice_roll))
      
      if (dice_roll[1] == dice_roll[2]) {
        doubles_rolled <- doubles_rolled + 1
      }
      
      if (doubles_rolled > 2) {
        tile_position <- 11
        in_jail <- TRUE
        
        print(paste('position:', tile_position))
        tile_count[tile_position] <- tile_count[tile_position] + 1
        
        print("doubles rolled too many times --> in jail")
        break
      }
      
      tile_position <- tile_position + sum(dice_roll)
      
      if (tile_position == 31) {
        tile_count[31] <- tile_count[31] + 1
        tile_position <- 11
        in_jail <- TRUE
        
        print(paste('position:', tile_position))
        tile_count[tile_position] <- tile_count[tile_position] + 1
        
        print("landed on jail tile --> in jail")
        break
      } else if (tile_position > 40) {
        tile_position <- tile_position - 40
      }
      
      
      print(paste('position:', tile_position))
      tile_count[tile_position] <- tile_count[tile_position] + 1
      
      if (dice_roll[1] != dice_roll[2]) {
        
        print("rolled no doubles")
        break
      }
    }
  } else if (in_jail) {
    
    print ('in jail in jail in jail')
    repeat {
      dice_roll <- sample(6, 2, replace = TRUE)
      
      print(paste('turn', turn))
      print(paste('dice roll:', dice_roll))
      
      if (dice_roll[1] == dice_roll[2]) {
        tile_position <- tile_position + sum(dice_roll)
        in_jail <- FALSE
        freedom_attempts <- 0
        
        print(paste('position:', tile_position))
        tile_count[tile_position] <- tile_count[tile_position] + 1
        
        print("rolled doubles in jail --> freedom")
        break
      } else if (dice_roll[1] != dice_roll[2]) {
        
        print(paste('position:', tile_position))
        
        freedom_attempts <- freedom_attempts + 1
      }
      
      if (freedom_attempts > 2) {
        tile_position <- tile_position + sum(dice_roll)
        in_jail <- FALSE
        freedom_attempts <- 0
        
        print(paste('position:', tile_position))
        tile_count[tile_position] <- tile_count[tile_position] + 1
        
        print("3 freedom attempts --> freedom")
        break
      }
    }
  }
  print ("=======================")
}

tile_count
plot(tile_count)
