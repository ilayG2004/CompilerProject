# CompilerProject
Experimental Project to Compile Python into low-level stack language
Ilay Guler 2023 -- 

12/23/2023:
  Creating operational semantics & formal grammars for Stack Language
  Implemented helper functions for parsing. Not complete

12/24/2023:
  Mostly completed parser. Will convert a string into a list of stack commands, although the parser needs to be further polished. Negative numbers,
  as well as semicolons between commands, need to be dealt with.

12/25/2023:
  Finished parsing for all current commands and constants of the stack language:
  Push, Pop, Add, Sub, Mul, Div, And, Or, Not, Lt, Gt, Eq, Neq, If, Jmp, Jz, Call, Ret, Swap, Over, Rot
  Takes user input of commands in the form of a string, then returns a list of commands (program). 
  If the user inputs a non-existent command or constant an error will be raised.
  

