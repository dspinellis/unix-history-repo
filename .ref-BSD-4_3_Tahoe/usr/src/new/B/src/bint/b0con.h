/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b0con.h,v 1.4 85/08/26 10:42:23 timo Exp $
*/

/* Configuration file: some easy changes to the system                      */
/* As much as possible, this is done automatically these days               */
/* You only need to change this file under rare circumstances               */

/* At the end, this file #includes another file, config.h, which is       */
/* generated automatically (by running mkconfig).  Most machine-dependent   */
/* changes are put there.                                                   */

/* VOID is used to keep lint quiet(er)                                      */
/* (This could be moved to "b.h", as it is never necessary to change it)    */
#ifdef lint
#define VOID (void)
#else
#define VOID /*empty*/
#endif

/* some un*xes demand that you reset stdin in some way if you get eof, and  */
/* want to read from it still. If yours doesn't, delete "clearerr(stdin)"   */
/* Actually, it never harms, so why should you want to delete it?           */
#define CLEAR_EOF clearerr(stdin)


/* Miscellaneous definitions*/
typedef int expint;		/*The 2nd argument of frexp points to this */
				/*(see manual page frexp(3)).              */
				/*On some 68K systems must be short (foo!) */

#define Maxtrig 1e16		/*Max x for sin(x), cos(x), tan(x)         */
				/*(Can anybody find a way to compute this  */
				/*automatically?)                          */

#ifdef IBMPC
#define Maxrefcnt 255
#else
#define Maxrefcnt Maxintlet
#endif

#define MaxSmallInt (BASE-1) /* This must be so! */
#define MinSmallInt (-BASE) /* This must be so!!! */

#ifndef INTEGRATION
#define CMBUFSIZE 1000		/*Buffer used for commands*/
#define RDBUFSIZE 1000		/*Buffer used for reading*/
#else
#define CMBUFSIZE 200		/*Buffer used for commands*/
#define RDBUFSIZE 100		/*Buffer used for reading*/
#endif

#ifdef unix
#define SEED getpid()		/*Any suitable random int (eg date or time) */
				/*to start the random number generator with */
#else
#define SEED getseed()
#endif

#include "config.h"		/* Chain to real machine dependencies       */
