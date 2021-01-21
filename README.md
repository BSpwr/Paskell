# Paskell (Pascal Interpreter in Haskell)

## Project 3 for COP4020

## Name
Boris Ermakov-Spektor and Austin Kee

## Build and Test

The build script requires that stack be installed and properly configured with ghc.

A script to build the project and run all tests is in the root folder.

To execute: Run `bash ./runTests.bash` from the same directory this file is in.

To manually run the interpreter, execute:

- `stack run path_to_pascal_source.pas`

## Features
Not working:
- Scope

Fully working:
- while-do and for-do loops
- break and continue keywords inside loops
- user-defined procedures and functions
- formal parameter passing in procedures/functions
- everything from P1

## Bonus Features
Fully working:
- Readln input

## Extra Features (Not on assignment sheet)
Fully working:
- string types
- boolean types (and boolean arithmetic)