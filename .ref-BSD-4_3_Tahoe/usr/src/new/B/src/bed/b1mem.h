/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */

/*
  $Header: b1mem.h,v 1.1 85/08/22 15:44:29 timo Exp $
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
extern value notel; /*TEMPORARY*/
extern bool noting; /*TEMPORARY*/
