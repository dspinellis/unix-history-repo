/* ckcker.h -- Symbol and macro definitions for C-Kermit */

/*
 Author: Frank da Cruz (SY.FDC@CU20B),
 Columbia University Center for Computing Activities, January 1985.
 Copyright (C) 1985, Trustees of Columbia University in the City of New York.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as it is not sold for profit, provided this
 copyright notice is retained. 
*/

/* Mnemonics for ASCII characters */

#define NUL	   000	    	/* ASCII Null */
#define SOH	   001	    	/* ASCII Start of header */
#define BEL        007		/* ASCII Bell (Beep) */
#define BS         010		/* ASCII Backspace */
#define LF         012	    	/* ASCII Linefeed */
#define CR         015		/* ASCII Carriage Return */
#define XON	   021	    	/* ASCII XON */
#define SP	   040		/* ASCII Space */
#define DEL	   0177		/* ASCII Delete (Rubout) */

/* Kermit parameters and defaults */

#define MAXPACK	   94		/* Maximum packet size */
#define RBUFL	   200 	    	/* Receive buffer length */
#define CTLQ	   '#'		/* Control char prefix I will use */
#define MYEBQ	   '&'		/* 8th-Bit prefix char I will use */
#define MYRPTQ	   '~'		/* Repeat count prefix I will use */

#define MAXTRY	    10	  	/* Times to retry a packet */
#define MYPADN	    0	  	/* How many padding chars I need */
#define MYPADC	    '\0'  	/* Which padding character I need */

#define DMYTIM	    7	  	/* Default timeout interval to use. */
#define URTIME	    10	  	/* Timeout interval to be used on me. */

#define DEFTRN	    0           /* Default line turnaround handshake */
#define DEFPAR	    0           /* Default parity */
#define MYEOL	    CR          /* End-Of-Line character I need on packets. */

#define DRPSIZ	    90	        /* Default incoming packet size. */
#define DSPSIZ	    90	        /* Default outbound packet size. */

#define DDELAY      5		/* Default delay. */
#define DSPEED	    9600 	/* Default line speed. */

/* Files */

#define ZCTERM      0	    	/* Console terminal */
#define ZSTDIO      1		/* Standard input/output */
#define ZIFILE	    2		/* Current input file */
#define ZOFILE      3	    	/* Current output file */
#define ZDFILE      4	    	/* Current debugging log file */
#define ZTFILE      5	    	/* Current transaction log file */
#define ZPFILE      6	    	/* Current packet log file */
#define ZSFILE      7		/* Current session log file */
#define ZSYSFN	    8		/* Input from a system function */
#define ZNFILS      9	    	/* How many defined file numbers */

/* Screen functions */

#define SCR_FN 1    	/* filename */
#define SCR_AN 2    	/* as-name */
#define SCR_FS 3 	/* file-size */
#define SCR_XD 4    	/* x-packet data */
#define SCR_ST 5      	/* File status: */
#define   ST_OK   0   	/*  Transferred OK */
#define   ST_DISC 1 	/*  Discarded */
#define   ST_INT  2     /*  Interrupted */
#define   ST_SKIP 3 	/*  Skipped */
#define   ST_ERR  4 	/*  Fatal Error */
#define SCR_PN 6    	/* packet number */
#define SCR_PT 7    	/* packet type or pseudotype */
#define SCR_TC 8    	/* transaction complete */
#define SCR_EM 9    	/* error message */
#define SCR_WM 10   	/* warning message */
#define SCR_TU 11	/* arbitrary undelimited text */
#define SCR_TN 12   	/* arbitrary new text, delimited at beginning */
#define SCR_TZ 13   	/* arbitrary text, delimited at end */
#define SCR_QE 14	/* quantity equals (e.g. "foo: 7") */

/* Macros */

#define tochar(ch)  ((ch) + SP )	/* Number to character */
#define unchar(ch)  ((ch) - SP )	/* Character to number */
#define ctl(ch)     ((ch) ^ 64 )	/* Controllify/Uncontrollify */
#define unpar(ch)   ((ch) & 127)	/* Clear parity bit */
