/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ozan Yigit at York University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mdef.h	5.9 (Berkeley) %G%
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

#ifdef msdos
#define system(str)	(-1)
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
 *      pushf() - push a call frame entry onto stack
 *      pushs() - push a string pointer onto stack
 */
#define gpbc() 	 (bp > bufbase) ? *--bp : getc(infile[ilevel])
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
