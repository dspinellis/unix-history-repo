/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3sta.h,v 1.4 85/08/22 16:48:00 timo Exp $
*/

#undef Procedure
#define Procedure int

Procedure v_formal(); 
Procedure l_formal();

extern parsetree pc; /* 'Program counter', current parsetree node */
extern parsetree next; /* Next parsetree node (changed by jumps) */
extern bool report; /* 'Condition code register', outcome of last test */

extern bool noloc; /* Set while evaluating (as opposed to locating)
			formal parameters of HOW'TOs */
