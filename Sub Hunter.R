###########################
### Review of last time ###
###########################

### How do we create a vector ###
1:10
c(1:10)
seq(2,20, by=2)

### store a vector or variable <- ###
s<-seq(2,20, by=2)
s

########################################
#######     1D Sub Hunter Game    ######
########################################

###############
### Concept ###
###############

### Player A is a Submarine and Player B is a Sub Hunter with X mines (Assume 5) ###
### Player B can place 1 mine each turn anywhere on the board ###
### Player A can only move 1 space each turn ###
### Player A cannot move through player B's mines ###

##########################
### Winner is declared ###
##########################

### Player A wins if Player B runs out of mines ###
### Player B wins if Player A cannot move ###
### player B wins if Player A is hit with a mine ###

####################
### Create Board ###
####################

### Create Vector of length n ###
n<-10
b<-vector(mode="numeric",length=n)

### Another way to create the board ###
b<-c(1:10*0)
b

### Player A move 1 ###
i<-4  # the i will help you rememeber where you are
b[i]<-1
b

### Player B move 1 ###
b[5]<-2
b

### Player A move 2 (Remove move 1 and Perform move 2) ###
b
b[i]<-0
b
i<-i-1
b[i]<-1


### Player B move 2 ###
b[8]<-2
b

### Player A move 3 ###
b
b[i]<-0
b
i<-i-1
b[i]<-1
b

### Player B move 3 ###
b[3]<-2
b

### Player A move 4 ###
b
b[i]<-0
b
i<-i-1
b[i]<-1
b

### Player B move 4 ###
b[1]<-2
b

### Game Over Player B wins ###
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################

################################################
###                                          ###
###       Building 1D Sub Hunter Game        ###
###                                          ###  
################################################

######################################################################
### Problem: There is too much typing and mental rule checking     ###
### We want be able to play using just few charcters worth of code ###
######################################################################

################################
###  List of Game functions  ###
################################

#############################################
### c(x) creates vector board of length x ###
### m(x) places sub on board              ###
### b(x) places a bomb on the board       ###
### clear() clears the screen             ###
#############################################

### To clear screen type clear() ###
clear<-function()
{
  cat("\014")
}

############################################
### Function to build our game board     ###
### c(x) to create board of length x     ###
############################################

create<-function(x)
{
  ### board is the variable name for the game board ###
  board<<-vector(mode="numeric",length=x)  
  
  ### Some useful stuff for later ###  
  counter<<-0
  spot<<-0
}

#################################################################
### Sub Movement function m(x) where x is location to move to ###
### Example: m(5) places the sub on spot                      ### 
### If you forget your last spot type: spot to find out       ###
################################################################# 

m<-function(x)
{
  if(counter %% 2 != 0)
  ### determine who's turn is next ###
  {
    return("Not Your Move...It Is Sub Hunter's Turn")
  } else
  
  ### is x a move within the board limits ###  
  if(x < 1 | x > length(board))
  {
    return("Out of Bounds Move! Please try again!")
  } else
  
  ### For 2nd Move and beyond ###
  if(counter>0)
  {
      ### Sub cannot stay still: Perform check ### 
      if(spot==x)
      {
        return("Must Move!")
      } else
      
      ### Sub can only move 1 unit at a time                ###
      ### Check: Is previous spot and new spot distance > 1 ### 
      if(abs(spot-x)>1)
      {
        return("Can't Move That Far!")
      } else
      
      if(board[x]==2)
      {
        return("BOOM!  Sub Hunter Wins Game.")
        board<<-c(length(board))
        counter<<-0
        spot<-0
      } else
      
      ### All Logic checks are sound   ###
      ### Assign new sub spot x on board remove old spot ###  
      board[x]<<-1
      board[spot]<<-0
      
      ### increment counter and assign spot ###
      counter<<-counter+1
      spot<<-x
      
      if(counter>=round(length(board)))
      {
        board<<-c(length(board))
        counter<<-0
        spot<-0
        return("Game Over: Sub Wins!")
      }
  } else
  
  ### Initialize Sub's first move ###
  if(counter==0)
  {
    board[x]<<-1
    spot<<-x
    counter<<-counter+1    
  }
  
  ### Clear movement selection from screen ###
  cat("\014")
}

#################################################################
### Bomber function b(x) where x is location to place bomb    ###
### Example: b(5) places a bomb on spot 5                     ### 
### If you forget your bomb spot type: bomb to find out       ###
#################################################################

b<-function(x)
{
  ### determine who's turn it is      ###
  ### Bomber goes when counter is odd ###
  if(counter %% 2 == 0)
  {
    return("Not Your Move...It Is Sub Hunter's Turn")
  } else
    
  ### End the game if half board length of bombs are placed with no Boom ###
  if(counter>=round(length(board)))
  {
    board<<-c(length(board))
    counter<<-0
    spot<-0
    return("Game Over: Sub Wins!")
  } else 
  
  ### End game if direct hit ###
  if(board[x]==1)
  {
    return("Game Over: Direct Hit Hunter Wins!")
    board<<-c(length(board))
    counter<<-0
    spot<-0
  } else
  
  ### End game if a suicide bomber ###  
  if(board[x]==2)
  {
    return("Game Over: Bombed Self, Good Job. Sub Wins!")
    board<<-c(length(board))
    counter<<-0
    spot<-0
  } else    
  
  ### place bomb and increment counter ###  
  board[x]<<-2
  counter<<-counter+1
  y<-board
  y[spot]<-0
  plot(c(1:length(board)),vector(length=length(board), mode="numeric")+2,
       pch=19,cex=2,col="blue",yaxt="n",ylab="",xlab="Bomb Location", ylim=c(0,6),
       main="Sub Hunter Bomb Placement")
  p<<-which(board==2)
  points(p,vector(length=length(p), mode="numeric")+2,pch=19,cex=2,col="red")
  legend(5,6,c("No Bomb","Bomb"), pch=c(19,19), col=c("blue","red"))
}