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
  Push, Dup, Pop, Add, Sub, Mul, Div, And, Or, Not, Lt, Gt, Eq, Neq, If, Fun, Jmp, Jz, Call, Ret, Swap, Over, Rot
  Takes user input of commands in the form of a string, then returns a list of commands (program). 
  If the user inputs a non-existent command or constant an error will be raised.

12/26/2023:
  Fixed parsing issues with If, and Fun. Added command Bind and Lookup. Removed command Neq because it was redundant.
  Began work on implementing the execute function, which will run through a stack program, and produce an output represented by when trace is called.
  Trace is a command that will take the top value off the stack and represents it in string form for our output. (More can be found on pdf of operational semantics)
  Binary operators, Push, Dup, and Pop have been completed for execute function.
  

