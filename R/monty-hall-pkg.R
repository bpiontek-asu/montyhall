#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Contestant selects a door in the Monty Hall Problem game.
#'   
#' @description
#'   `select_door` allows the contestant to select one of the three doors 
#'   created via the `create_game` function
#'   
#' @details
#'   This is the portion of the game wherein the contestant selects an
#'   initial door. 
#'   
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a number indicating the door that the contestant
#'   has chosen. What is behind the door is not indicated, although the person
#'   running the game can determine that from the character vector returned by
#'   the previous function.
#'   
#' @examples
#'   select_door()
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host opens a door with a goat behind it in the Monty Hall Problem Game. 
#'   
#' @description
#'   `open_goal_door` allows the host of the game to open one of the remaining 
#'   doors, not previously selected by the contestant, to reveal a goat.
#'   
#' @details
#'   This is the portion of the game where the host opens one of the remaining
#'   doors, which will have a goat behind it, which sets the contestant up to 
#'   make the choice between staying with the door they chose in the previous 
#'   step, or changing to the other remaining unopened door. 
#'   
#' @param The arguments 'game' and 'a.pick' are used by the function.
#' 
#' @return The function returns a number indicating the door that the host
#'   opens. What is behind the door is not indicated, although the person
#'   running the game can determine that it is a goat from the character vector 
#'   returned by the first function.
#'   
#' @examples
#'   open_goat_door(game, a.pick)
#'   
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'  Contestant decides whether to 'stay' with or 'switch' the door they 
#'  originally selected in the Monty Hall Problem game.
#'  
#' @description
#'   `change_door` allows the contestant continue on through the game using the 
#'   door they originally selected, or to switch their door to the remaining
#'   unopened door. 
#'   
#' @details
#'   This is the portion of the game where the contestant decides whether to
#'   continue by using the door they originally selected, or to change their 
#'   door choice to the remaining unopened door, knowing that the host has 
#'   opened a door with a goat behind it, so one of the remaining two doors has
#'   the other goat behind it, while the other has a car behind it. 
#'   
#' @param The arguments 'stay=T', 'opened.door', and 'a.pick' are used by the 
#'   function.
#' 
#' @return The function returns a number indicating the door that the contestant
#'   ultimately chooses. What is behind the door is not indicated, although the 
#'   person running the game can determine that it is a goat from the character 
#'   vector returned by the first function.
#'   
#' @examples
#'   change_door(stay=T, opened.door, a.pick)
#'   
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   The simulation indicates whether the contestant's final pick results in a 
#'   win or a loss of the Monty Hall Problem game.
#'   
#' @description
#'   `change_door` allows the contestant continue on through the game using the 
#'   door they originally selected, or to switch their door to the remaining
#'   unopened door. 
#'   
#' @details
#'   This is the portion of the game where the contestant decides whether to
#'   continue by using the door they originally selected, or to change their 
#'   door choice to the remaining unopened door, knowing that the host has 
#'   opened a door with a goat behind it, so one of the remaining two doors has
#'   the other goat behind it, while the other has a car behind it. 
#'   
#' @param The arguments 'final.pick' and 'game' are used by the function.
#' 
#' @return The function returns a string indicating whether the door that the 
#'   contestant ultimately chose results in a win or loss of the game.
#'   
#' @examples
#'   determine_winner(final.pick, game)
#'   
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   A wrapper function is created to allow all of the simulation's previously
#'   created functions to run in a single step. 
#'   
#' @description
#'   `play_game` allows the entire simulation, including all previously defined 
#'   functions, to be run in a single step. 
#'   
#' @details
#'   All of the functions defined in previous portions of the simulation are 
#'   combined (wrapped) into a single function, allowing the game-runner to play
#'   the game in a single step.
#'  
#' @param ... no arguments are used by the function, although the previously 
#'   indicated arguments are used by the previously defined functions within 
#'   this function.
#'   
#' @return The function returns a table with two rows and two columns, 
#'   indicating the outcomes of the game (win/lose) if the contestant decides to
#'   stay with the door they originally chose or decides to switch to the door
#'   remaining after the host opens the goat door. 
#'   
#' @examples
#'   play_game()
#'   
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   The game is run on a loop.
#'   
#' @description
#'   `play_n_games` allows the entire simulation, including all previously 
#'   defined functions, to be run repeatedly, 'n' number of times, in a single 
#'   step. 
#'   
#' @details
#'   The simulation is run 'n' number of times to determine the proportion of 
#'   times that the contestant will win and lose the Monty Hall Problem game 
#'   based on their decision to stay with the door that they originally chose or
#'   switch to the remaining door after the host opens a door with a goat behind
#'   it.
#'   
#' @param ... no arguments are used by the function, although the previously 
#'   indicated arguments are used by the previously defined functions within 
#'   this function.
#'   
#' @return The function returns a table with two rows and three columns, 
#'   indicating the proportion of times each game strategy (stay/switch) results
#'   in a given outcomes of the game (win/lose) depending on whether the 
#'   contestant decides to stay with the door they originally chose or decides 
#'   to switch to the door remaining after the host opens the goat door. 
#'   
#' @examples
#'   play_n_games()
#'   
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
