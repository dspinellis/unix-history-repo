/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3err.h,v 1.4 85/08/22 16:43:46 timo Exp $
*/

/* berr.h: B error message handling */

extern intlet errlino; extern value erruname;
extern parsetree curline;
extern value curlino;
extern context how_context, act_context;
extern bool still_ok, interrupted;
extern bool tracing;
extern Procedure bye();

/* Procedure syserr(); */
/* Procedure memexh(); */
/* Procedure error(); */
/* Procedure parerr(); */
/* Procedure pprerr(); */
/* Procedure checkerr(); */
/* Procedure debug(); */
/* Procedure trace(); */
/* Procedure int_signal(); */
