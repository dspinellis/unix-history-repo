/* Declarations for insn-output.c.  These functions are defined in recog.c.
   Copyright (C) 1987 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Operand-predicate functions.  */
int general_operand ();
int push_operand ();
int memory_operand ();
int indirect_operand ();
int immediate_operand ();
int register_operand ();
int address_operand ();
int nonmemory_operand ();
int nonimmediate_operand ();

int offsettable_address_p ();
rtx adj_offsettable_operand ();

/* Output a string of assembler code.
   Defined in final.c.  */
void output_asm_insn ();

/* When outputting assembler code, indicates which alternative
   of the constraints was actually satisfied.  */
extern int which_alternative;

/* When outputting delayed branch sequences, this rtx holds the
   sequence being output.  It is null when no delayed branch
   sequence is being output, so it can be used as a test in the
   insn output code.

   This variable is defined  in final.c.  */
extern rtx final_sequence;

/* Nonzero if function being compiled pops its args on return.
   May affect compilation of return insn or of function epilogue.  */

extern int current_function_pops_args;

/* Nonzero if function being compiled needs to be given an address
   where the value should be stored.  */

extern int current_function_returns_struct;

/* Nonzero if function being compiled needs to
   return the address of where it has put a structure value.  */

extern int current_function_returns_pcc_struct;

/* Nonzero if function being compiled needs to be passed a static chain.  */

extern int current_function_needs_context;

/* Nonzero if function being compiled can call setjmp.  */

extern int current_function_calls_setjmp;

/* Nonzero if function being compiled can call alloca,
   either as a subroutine or builtin.  */

extern int current_function_calls_alloca;

/* Nonzero if the current function returns a pointer type */

extern int current_function_returns_pointer;

/* If function's args have a fixed size, this is that size, in bytes.
   Otherwise, it is -1.
   May affect compilation of return insn or of function epilogue.  */

extern int current_function_args_size;

/* # bytes the prologue should push and pretend that the caller pushed them.
   The prologue must do this, but only if parms can be passed in registers.  */

extern int current_function_pretend_args_size;

/* Name of function now being compiled.  */

extern char *current_function_name;
