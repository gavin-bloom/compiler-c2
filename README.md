# C2 Compiler
Compiler for a small C-like language C2. Built with OCaml 4.11.0. 
Uses a makefile to compile the project. 
The usage for the outputted file c2 is `c2 [-c] [-e] filename` where flags `c` and `e` are for compile and evaluate respectively.

# Process
The compiler makes use of ocamllex and ocamlyacc for generating a lexer and a parser respectively. 
Next the AST (abstract syntax tree) is typechecked and if it passes the AST is converted to a CFG (control flow graph).
Next the register allocator takes the CFG, which may contain non-physical "registers" (like Var 3, etc), and should return a CFG which
only contains physical registers (like RAX, etc.) Spilling is done if necessary. 
Finally the CFG is converted to an X86 instruction list and output for assembly.





