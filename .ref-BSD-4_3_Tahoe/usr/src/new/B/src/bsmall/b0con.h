/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b0con.h,v 1.1 84/07/03 20:11:59 timo Exp $ */

/* Configuration file: some easy changes to the system                      */

/* VOID is used to keep lint quiet(er)                                      */
/* if your compiler doesn't accept (void) delete "(void)" from the next line*/
#define VOID

/* some un*xes demand that you reset stdin in some way if you get eof, and  */
/* want to read from it still. If yours doesn't, delete "clearerr(stdin)"   */
#define CLEAR_EOF clearerr(stdin)

/* Default editor: end the string with <space>+ if it accepts a line number */
/* of the form +<number> before the file name when it is called		    */
/* eg vi +10 myunit 							    */

/* temporary file to save an edited file in                                 */
#define SAVEFILE ".b_temp"

/* 'Small' number package.  (Also used by output conversion of 'Big' pkg.)  */
#define BIG 72057594037927935.0 /*Largest integral real number              */
#define LONG 9999999999999999.5 /*Largest power of 10 less than BIG         */
#define MAXNUMDIG 16		/*The number of 9's in LONG                 */
#define MINNUMDIG  6		/*Don't change; this is here for consistency*/

/* Unbounded number package                                                 */
/* BASE must be set for your machine so that BASE*BASE can be computed      */
/* exactly as a double, and (BASE+BASE) and (-BASE-BASE) are computable     */
/* with long integers. It must equal 10**tenlogBASE.                        */
/* Maxreal, Maxexpo, Minexpo and Dblbits define properties of floating      */
/* point arithmetic on your machine                                         */
#define tenlogBASE 8	/*Change this and the next line together!*/
#define BASE 100000000l
#define Maxreal 1.7E38
#define Maxexpo 127
#define Minexpo (-128)
#define Dblbits 56		/*Number of bits used for double precision */

/*Other definitions*/
typedef int expint;		/*The 2nd argument of frexp points to this */
				/*(see manual page frexp(3)).              */
				/*On some 68K systems must be short (foo!) */

#define Maxgonio 1e16		/*Max x for sin(x), cos(x), tan(x)         */

#define Maxintlet ((1<<15)-1)	/*Largest short*/
#define Maxint    ((1<<15)-1)	/*Largest int*/

#define RDBUFSIZE 500		/*Buffer used for read commands*/
#define TXDBUFSIZE 100		/*Text displays*/

#define SEED getpid()		/*Any suitable random int (eg date or time) */
				/*to start the random number generator with */
