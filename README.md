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

12/27/2023:
  Implemented support for executing If-Statements, trace, bind, and lookup. Trace, and Bind are unique because they return a tuple containing the edited stack and output or variable environment respectively. 
  
12/30/2023:
  Fixed parsing bug with nested If-Statements. The parser would read the 'Else' or 'End' of the inner if-statement, and then prematurely stop parsing both, leading to a parse error. Tested stack language with recursive functions such as Factorial and I received the correct output. I will likely test more tomorrow with more complex recursive functions before moving on. I will also change how symbols parse tomorrow. Currently, they can only consist of alphabetical characters, but I would like for them to be alphanumeric.

  1/1/2024:
    Added documentation file which describes the operational semantics of the stack language. Included examples for passing cases and error cases of individual commands, as well as describing the nature of the interpreter.
I included some functions which are common in higher-level languages, but translated into low-level stack language. 
    These are common Math functions you would find in Python such as:
      -Math.abs(n)
      -Math.factorial(n)
      -Math.pow(x,y)
