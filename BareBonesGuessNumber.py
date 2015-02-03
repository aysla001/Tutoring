## Game to play guess the number in an array.
## Barebones logic of subhunter
#http://www.codeskulptor.org/#user38_IQuJGXx3dW_3.py

## libraries for python
import simplegui
import random
import math

number_to_guess = 0 #initialize guess to 0

def init():
    create_array()

def create_array():
    global number_to_guess #make our random number global
    global current_number_guesses
    number_to_guess = random.randrange(0,10) # eventually update to number dimensioned
    current_number_guesses = 0
    

def input_guess(guess):
    guess = int(guess)
    global number_to_guess
    global current_number_guesses
    
    current_number_guesses = current_number_guesses + 1
    
    if guess == number_to_guess:
        print "Correct. Game Restart"
        create_array()
    #else:
    #    print number_to_guess

#create window(s)
f = simplegui.create_frame ("Guess the number", 200,200)

# create control elements for window
#f.add_button("Range is [0,100)", create_array,200)
f.add_input("Enter a guess", input_guess, 200)

#initialize
init()