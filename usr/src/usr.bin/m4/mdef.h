/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ozan Yigit.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)mdef.h	5.1 (Berkeley) %G%
 */

/*
 * mdef.h
 * Facility: m4 macro processor
 * by: oz
 */

#ifndef unix
#define unix 0
#endif 

#ifndef vms
#define vms 0
#endif

#if vms

#include stdio
#include ctype
#include signal

#else 

#include <stdio.h>
#include <ctype.h>
#include <signal.h>

#endif

/*
 *
 * m4 constants..
 *
 */
 
#define MACRTYPE        1
#define DEFITYPE        2
#define EXPRTYPE        3
#define SUBSTYPE        4
#define IFELTYPE        5
#define LENGTYPE        6
#define CHNQTYPE        7
#define SYSCTYPE        8
#define UNDFTYPE        9
#define INCLTYPE        10
#define SINCTYPE        11
#define PASTTYPE        12
#define SPASTYPE        13
#define INCRTYPE        14
#define IFDFTYPE        15
#define PUSDTYPE        16
#define POPDTYPE        17
#define SHIFTYPE        18
#define DECRTYPE        19
#define DIVRTYPE        20
#define UNDVTYPE        21
#define DIVNTYPE        22
#define MKTMTYPE        23
#define ERRPTYPE        24
#define M4WRTYPE        25
#define TRNLTYPE        26
#define DNLNTYPE        27
#define DUMPTYPE        28
#define CHNCTYPE        29
#define INDXTYPE        30
#define SYSVTYPE        31
#define EXITTYPE        32
#define DEFNTYPE        33
 
#define STATIC          128

/*
 * m4 special characters
 */
 
#define ARGFLAG         '$'
#define LPAREN          '('
#define RPAREN          ')'
#define LQUOTE          '`'
#define RQUOTE          '\''
#define COMMA           ','
#define SCOMMT          '#'
#define ECOMMT          '\n'

/*
 * definitions of diversion files. If the name of
 * the file is changed, adjust UNIQUE to point to the
 * wildcard (*) character in the filename.
 */

#if unix
#define DIVNAM  "/tmp/m4*XXXXXX"        /* unix diversion files    */
#define UNIQUE          7               /* unique char location    */
#else
#if vms
#define DIVNAM  "sys$login:m4*XXXXXX"   /* vms diversion files     */
#define UNIQUE          12              /* unique char location    */
#else
#define DIVNAM	"\M4*XXXXXX"		/* msdos diversion files   */
#define	UNIQUE	    3			/* unique char location    */
#endif
#endif

/*
 * other important constants
 */

#define EOS             (char) 0
#define MAXINP          10              /* maximum include files   */
#define MAXOUT          10              /* maximum # of diversions */
#define MAXSTR          512             /* maximum size of string  */
#define BUFSIZE         4096            /* size of pushback buffer */
#define STACKMAX        1024            /* size of call stack      */
#define STRSPMAX        4096            /* size of string space    */
#define MAXTOK          MAXSTR          /* maximum chars in a tokn */
#define HASHSIZE        199             /* maximum size of hashtab */
 
#define ALL             1
#define TOP             0
 
#define TRUE            1
#define FALSE           0
#define cycle           for(;;)

#ifdef VOID
#define void            int             /* define if void is void. */
#endif

/*
 * m4 data structures
 */
 
typedef struct ndblock *ndptr;
 
struct ndblock {                /* hastable structure         */
        char    *name;          /* entry name..               */
        char    *defn;          /* definition..               */
        int     type;           /* type of the entry..        */
        ndptr   nxtptr;         /* link to next entry..       */
};
 
#define nil     ((ndptr) 0)
 
struct keyblk {
        char    *knam;          /* keyword name */
        int     ktyp;           /* keyword type */
};

typedef union {			/* stack structure */
	int	sfra;		/* frame entry  */
	char 	*sstr;		/* string entry */
} stae;

/*
 * macros for readibility and/or speed
 *
 *      gpbc()  - get a possibly pushed-back character
 *      min()   - select the minimum of two elements
 *      pushf() - push a call frame entry onto stack
 *      pushs() - push a string pointer onto stack
 */
#define gpbc() 	 (bp > buf) ? *--bp : getc(infile[ilevel])
#define min(x,y) ((x > y) ? y : x)
#define pushf(x) if (sp < STACKMAX) mstack[++sp].sfra = (x)
#define pushs(x) if (sp < STACKMAX) mstack[++sp].sstr = (x)

/*
 *	    .				   .
 *	|   .	|  <-- sp		|  .  |
 *	+-------+			+-----+
 *	| arg 3 ----------------------->| str |
 *	+-------+			|  .  |
 *	| arg 2 ---PREVEP-----+ 	   .
 *	+-------+	      |
 *	    .		      |		|     |
 *	+-------+	      | 	+-----+
 *	| plev	|  PARLEV     +-------->| str |
 *	+-------+			|  .  |
 *	| type	|  CALTYP		   .
 *	+-------+
 *	| prcf	---PREVFP--+
 *	+-------+  	   |
 *	|   .	|  PREVSP  |
 *	    .	   	   |
 *	+-------+	   |
 *	|	<----------+
 *	+-------+
 *
 */
#define PARLEV  (mstack[fp].sfra)
#define CALTYP  (mstack[fp-1].sfra)
#define PREVEP	(mstack[fp+3].sstr)
#define PREVSP	(fp-3)
#define PREVFP	(mstack[fp-2].sfra)
