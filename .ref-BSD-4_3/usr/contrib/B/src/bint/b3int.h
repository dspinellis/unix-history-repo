/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3int.h,v 1.4 85/08/22 16:44:13 timo Exp $
*/

/* Interpreter */

value evalthread();
/* Procedure execthread(); */

value pop();
/* Procedure push(); */

extern bool tracing;

#define Thread(t) *Branch(t, Nbranches(t)) /* Next instruction */
#define Thread2(t) *Branch(t, Nbranches(t)+1) /* Alternate next instr. or flag */

#define Stop ((parsetree)zero) /* Legal stop */
#define Halt NilTree /* Illegal stop (loose end of code) */

extern int call_level;
