library(glue)

# [tool, win, lose] mean if you chose rock [rock, win scissors, lose paper]
tools <- list(rock = c(1, 3, 2), # 1 = rock 
              paper = c(2, 1, 3), # 2 = paper
              scissors = c(3, 2, 1)) # 3 = scissors

# keep all scores in game
score <- list(win = 0, 
              lose = 0, 
              draw = 0, 
              user_score = 0,
              bot_score = 0)

# converting a numeric become to a symbol to show the weapon that use in each match
convert_weapon <- function(weapon) {
  convert <- factor(weapon, 
                        levels = c(1, 2, 3), 
                        labels = c("rock", "paper", "scissors"))
}

# principles of the game
condition <- function(user, bot) {
  # if the bot's weapon can with the user weapon refer to tools object
  if (tools[[bot]][1] == tools[[user]][3]) {
    score$lose <<- score$lose + 1
    score$bot_score <<- score$bot_score + 1
    print("You lose!")
    # user win bot
  } else {
    if (tools[[bot]][1] == tools[[user]][2]){
      score$win <<- score$win + 1
      score$user_score <<- score$user_score + 1
      print("You win!")
      # the both is draw
    } else {
      score$draw <<- score$draw + 1
      print("You draw!")
    }
  }
  print(paste("Your score is:", score$user_score))
  print(paste("The enemy score is:", score$bot_score))
}


# Welcome function
welcome <- function() {
  print("Welcome to Pao Ying Chub GAME")
  user_name <- readline("What's your name?: ")
  
  print(paste("Hi", user_name))
  print("GAME START")
}


# Start game
pao_ying_chub <- function() {
  # call the function's name "welcome"
  welcome()

  while (TRUE){
    # ask user to choose rock, paper or scissors
    tool_user <- as.numeric(readline('Choose your weapon to fight, 1 for Rock, 2 for Paper and 3 for Scissor or 4 for exit to end the game: '))
    
    # if user wanna stop the game add 4
    if (tool_user == 4) {
      print(glue("Your score is: {score$user_score}
            The enemy's score is: {score$bot_score}
            Wins is: {score$win}
            Loses is: {score$lose}
            Draws is: {score$draw}
             "))
      break
    # if user added something out of 1, 2, 3. Try again!  
    } else { if (class(tool_user) != "numeric") {
      print("No choices you chose please try again!")
    # play game 
    } else {
      # random bot weapon
      tool_bot <- as.numeric(sample(1:3, 1))
      print(paste("The enemy chose:", convert_weapon(tool_bot)))
      print(paste("You chose:", convert_weapon(tool_user)))
      
      # call the function's name condition
      condition(tool_user, tool_bot)
    }
    }
  }
}

pao_ying_chub()
