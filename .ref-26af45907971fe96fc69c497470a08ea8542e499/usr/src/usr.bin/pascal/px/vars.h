/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vars.h	5.4 (Berkeley) %G%
 */

#include <stdio.h>

/*
 * px - Berkeley Pascal interpreter
 *
 * Version 4.0, January 1981
 *
 * Original version by Ken Thompson
 *
 * Substantial revisions by Bill Joy and Chuck Haley
 * November-December 1976
 *
 * Rewritten for VAX 11/780 by Kirk McKusick
 * Fall 1978
 *
 * Rewritten in ``C'' using libpc by Kirk McKusick
 * Winter 1981
 *
 * Px is described in detail in the "PX 4.0 Implementation Notes"
 * The source code for px is in several major pieces:
 *
 *	int.c		C main program which reads in interpreter code
 *	interp.c	Driver including main interpreter loop and
 *			the interpreter instructions grouped by their
 *			positions in the interpreter table.
 *	utilities.c	Interpreter exit, backtrace, and runtime statistics.
 *
 * In addition there are several headers defining mappings for panic
 * names into codes, and a definition of the interpreter transfer
 * table. These are made by the script make.ed1 in this directory and 
 * the routine opc.c from ${PASCALDIR}. (see the makefile for details)
 */
#define PXPFILE		"pmon.out"
#define	BITSPERBYTE	8
#define	BITSPERLONG	(BITSPERBYTE * sizeof(long))
#define HZ		100
#define NAMSIZ		76
#define MAXFILES	32
#define PREDEF		2
#ifdef ADDR32
#ifndef tahoe
#define STDLVL		((struct iorec *)(0x7ffffff1))
#define GLVL		((struct iorec *)(0x7ffffff0))
#else tahoe
#define STDLVL		((struct iorec *)(0xbffffff1))
#define GLVL		((struct iorec *)(0xbffffff0))
#endif tahoe
#endif ADDR32
#ifdef ADDR16
#define STDLVL		((struct iorec *)(0xfff1))
#define GLVL		((struct iorec *)(0xfff0))
#endif ADDR16
#define FILNIL		((struct iorec *)(0))
#define INPUT		((struct iorec *)(&input))
#define OUTPUT		((struct iorec *)(&output))
#define ERR		((struct iorec *)(&_err))
#define	PX	0	/* normal run of px */
#define	PIX	1	/* load and go */
#define	PIPE	2	/* bootstrap via a pipe */
#define	PDX	3	/* invoked by the debugger "pdx" */
#define releq 0
#define relne 2
#define rellt 4
#define relgt 6
#define relle 8
#define relge 10
typedef enum {FALSE, TRUE} bool;

/*
 * interrupt and allocation routines
 */
extern long createtime;
extern char *PALLOC();
extern char *malloc();
extern long time();
extern intr();
extern memsize();
extern syserr();
extern liberr();

/*
 * stack routines and structures
 */
struct sze8 {
	char element[8];
};

/*
 * emulated pc types
 */
union progcntr {
	char *cp;
	unsigned char *ucp;
	short *sp;
	unsigned short *usp;
	long *lp;
	double *dbp;
	struct hdr *hdrp;
	struct sze8 *s8p;
};

/*
 * THE RUNTIME DISPLAY
 *
 * The entries in the display point to the active static block marks.
 * The first entry in the display is for the global variables,
 * then the procedure or function at level one, etc.
 * Each display entry points to a stack frame as shown:
 *
 *		base of stack frame
 *		  ---------------
 *		  |		|
 *		  | block mark  |
 *		  |		|
 *		  ---------------  <-- display entry "stp" points here
 *		  |             |  <-- display entry "locvars" points here
 *		  |   local	|
 *		  |  variables  |
 *		  |		|
 *		  ---------------
 *		  |		|
 *		  |  expression |
 *		  |  temporary  |
 *		  |  storage	|
 *		  |		|
 *		  - - - - - - - -
 *
 * The information in the block mark is thus at positive offsets from
 * the display.stp pointer entries while the local variables are at negative
 * offsets from display.locvars. The block mark actually consists of
 * two parts. The first part is created at CALL and the second at entry,
 * i.e. BEGIN. Thus:
 *
 *		-------------------------
 *		|			|
 *		|  Saved lino		|
 *		|  Saved lc		|
 *		|  Saved dp		|
 *		|			|
 *		-------------------------
 *		|			|
 *		|  Saved (dp)		|
 *		|			|
 *		|  Pointer to current 	|
 *		|   routine header info	|
 *		|			|
 *		|  Saved value of	|
 *		|   "curfile"		|
 *		|			|
 *		|  Empty tos value	|
 *		|			|
 *		-------------------------
 */

/*
 * program variables
 */
extern union display	_display;	/* runtime display */
extern struct dispsave	*_dp;		/* ptr to active frame */
extern long		_lino;		/* current line number */
extern int		_argc;		/* number of passed args */
extern char		**_argv;	/* values of passed args */
extern bool		_nodump;	/* TRUE => no post mortum dump */
extern long		_runtst;	/* TRUE => runtime tests */
extern long		_mode;		/* execl by PX, PIPE, or PIX */
extern long		_stlim;		/* statement limit */
extern long		_stcnt;		/* statement count */
extern long		_seed;		/* random number seed */
extern char		*_maxptr;	/* maximum valid pointer */
extern char		*_minptr;	/* minimum valid pointer */
extern long		*_pcpcount;	/* pointer to pxp buffer */
extern long		_cntrs;		/* number of counters */
extern long		_rtns;		/* number of routine cntrs */

/*
 * The file i/o routines maintain a notion of a "current file".
 * A pointer to this file structure is kept in "curfile".
 *
 * file structures
 */
struct iorechd {
	char		*fileptr;	/* ptr to file window */
	long		lcount;		/* number of lines printed */
	long		llimit;		/* maximum number of text lines */
	FILE		*fbuf;		/* FILE ptr */
	struct iorec	*fchain;	/* chain to next file */
	struct iorec	*flev;		/* ptr to associated file variable */
	char		*pfname;	/* ptr to name of file */
	short		funit;		/* file status flags */
	short		fblk;		/* index into active file table */
	long		fsize;		/* size of elements in the file */
	char		fname[NAMSIZ];	/* name of associated UNIX file */
};

struct iorec {
	char		*fileptr;	/* ptr to file window */
	long		lcount;		/* number of lines printed */
	long		llimit;		/* maximum number of text lines */
	FILE		*fbuf;		/* FILE ptr */
	struct iorec	*fchain;	/* chain to next file */
	struct iorec	*flev;		/* ptr to associated file variable */
	char		*pfname;	/* ptr to name of file */
	short		funit;		/* file status flags */
	short		fblk;		/* index into active file table */
	long		fsize;		/* size of elements in the file */
	char		fname[NAMSIZ];	/* name of associated UNIX file */
	char		buf[BUFSIZ];	/* I/O buffer */
	char		window[1];	/* file window element */
};

/*
 * unit flags
 */
#define	FDEF	0x80	/* 1 => reserved file name */
#define	FTEXT	0x40	/* 1 => text file, process EOLN */
#define	FWRITE	0x20	/* 1 => open for writing */
#define	FREAD	0x10	/* 1 => open for reading */
#define	TEMP	0x08	/* 1 => temporary file */
#define	SYNC	0x04	/* 1 => window is out of sync */
#define	EOLN	0x02	/* 1 => at end of line */
#define	EOFF	0x01	/* 1 => at end of file */

/*
 * file routines
 */
extern struct iorec	*GETNAME();
extern char		*MKTEMP();

/*
 * file record variables
 */
extern struct iorechd	_fchain;	/* head of active file chain */
extern struct iorec	*_actfile[];	/* table of active files */
extern long		_filefre;	/* last used entry in _actfile */

/*
 * standard files
 */
extern struct iorechd	input;
extern struct iorechd	output;
extern struct iorechd	_err;

/*
 * Px execution profile array
 */
#ifdef PROFILE
#define	NUMOPS 256
extern long _profcnts[NUMOPS];
#endif PROFILE
