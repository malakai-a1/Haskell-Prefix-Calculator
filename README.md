# Haskell-Prefix-Calculator

This Haskell project implements a simple prefix notation calculator. 
The calculator supports basic arithmetic operations and maintains a history of results that can be referenced in subsequent calculations.

## Features

**Basic Arithmetic Operations**: Addition, Subtraction, Multiplication, and Division.

**History Reference**: Ability to reference previous results using a history index.
    -- the entire history will print after each expression. In case you forget what you typed.
    
**Interactive Loop**: Continuously prompts the user for expressions until the user types "quit".

## Files

**prefixCalculator.hs**: Main source code for the prefix calculator.

## Running the Calculator

### To run the prefix calculator follow these steps:
  - save the .hs file into a directory of your choosing.
  - open a terminal window and navigate to the directory where the file was saved.
  - type in the following command to compile the .hs file:

          ghc -o prefixCalc prefixCalculator.hs
  - after compiling type in the following command to run the program

          ./prefixCalc

  - when running the program, you **MUST** put spaces between every operator and value

            e.g: Enter an expression: + + 3 2 1
            Enter an expression: * * $3 $2 $1  
            Enter an expression: / / 3 2 1

  - when you are done using the calculator type in the following command:

            quit
    

