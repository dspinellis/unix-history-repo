/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2err.h,v 1.1 84/06/28 00:48:45 timo Exp $ */

/* berr.h: B error message handling */

extern intlet errlino; extern value erruname; extern literal errutype;
extern jmp_buf main_loop;
extern bool skipping;
extern bool tracing;

/* Procedure syserr(); */
/* Procedure memexh(); */
/* Procedure error(); */
/* Procedure parerr(); */
/* Procedure pprerr(); */
/* Procedure checkerr(); */
/* Procedure debug(); */
/* Procedure trace(); */
/* Procedure int_signal(); */
/* Procedure bye(); */
