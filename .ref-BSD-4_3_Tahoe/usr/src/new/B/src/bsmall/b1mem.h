/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b1mem.h,v 1.1 84/06/28 00:48:42 timo Exp $ */

/* bmem.h: B memory management */

typedef char *ptr;
#define Nil ((ptr) 0)

ptr getmem();
/* Procedure regetmem(); */
/* Procedure freemem(); */
/* Procedure prgr(); */
/* Procedure xtndtex(); */
/* Procedure xtndlt(); */
/* Procedure initmem(); */
extern value notel; /*TEMPORARY*/
extern bool noting; /*TEMPORARY*/
