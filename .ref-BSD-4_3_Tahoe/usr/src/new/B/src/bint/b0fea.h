/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b0fea.h,v 1.4 85/08/22 16:41:18 timo Exp $
*/

#ifdef CWI
#define CONVERSION
#endif

#ifdef unix
#define PRMNVFILE ".prmnv"
#define SAVEPRMNVFILE ".prmnv_save"
#define SIGNAL
#define SETJMP
#define KILL
#define TIMING
#define ISATTY
#endif unix

/* ********************************************************************	*/

#ifndef unix

#define RENAME

#endif !unix

/* ********************************************************************	*/

#ifdef vax

#define BSD_SELECT

#endif vax

/* ********************************************************************	*/

#ifdef IBMPC

#define LATTICE
#define SIGNAL
#define START_MESSAGE
#define NO_ABS
#define SETJMP
#define ISATTY

#endif IBMPC

/* ********************************************************************	*/

#ifndef IBMPC

/* #define EXT_COMMAND */
#define TYPE_CHECK
#define PRINT_APPROX

#endif !IBMPC

