/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2tcU.h,v 1.4 85/08/22 16:43:30 timo Exp $
*/

/* unification of polytypes */

/* Procedure unify(); */ 	/* polytype a, b, &u; bool &bad */

bool contains(); 	/* polytype u, a */
bool equal_vars(); 	/* polytype s, a */
