/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1mem.h,v 1.4 85/08/22 16:41:48 timo Exp $
*/

/* bmem.h: B memory management */

typedef char *ptr;
#define Nil ((ptr) 0)

#define getmem get_mem

ptr getmem();

/* Procedure regetmem(); */
/* Procedure freemem(); */
/* Procedure prgr(); */
/* Procedure initmem(); */
