/* Copyright (c) 1979 Regents of the University of California */

/* sccsid[] = "@(#)h00vars.h 1.11 4/7/87"; */

#include <stdio.h>
#include "whoami.h"

#define PXPFILE		"pmon.out"
#define	BITSPERBYTE	8
#define	BITSPERLONG	(BITSPERBYTE * sizeof(long))
#define LG2BITSBYTE	03
#define MSKBITSBYTE	07
#define LG2BITSLONG	05
#define MSKBITSLONG	037
#define HZ		60
#define	MAXLVL		20
#define MAXERRS		75
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
typedef enum {FALSE, TRUE} bool;

/*
 * runtime display structure
 */
struct display {
	char	*ap;
	char	*fp;
};

/*
 * formal routine structure
 */
struct formalrtn {
	long		(*fentryaddr)();	/* formal entry point */
	long		fbn;			/* block number of function */
	struct display	fdisp[ MAXLVL ];	/* saved at first passing */
};

/*
 * program variables
 */
extern struct display	_disply[MAXLVL];/* runtime display */
extern int		_argc;		/* number of passed args */
extern char		**_argv;	/* values of passed args */
extern long		_stlim;		/* statement limit */
extern long		_stcnt;		/* statement count */
extern long		_seed;		/* random number seed */
extern char		*_maxptr;	/* maximum valid pointer */
extern char		*_minptr;	/* minimum valid pointer */
extern long		_pcpcount[];	/* pxp buffer */

/*
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
	unsigned short	fblk;		/* index into active file table */
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
	unsigned short	fblk;		/* index into active file table */
	long		fsize;		/* size of elements in the file */
	char		fname[NAMSIZ];	/* name of associated UNIX file */
	char		buf[BUFSIZ];	/* I/O buffer */
	char		window[1];	/* file window element */
};

/*
 * unit flags
 */
#define SPEOLN	0x100	/* 1 => pseudo EOLN char read at EOF */
#define	FDEF	0x080	/* 1 => reserved file name */
#define	FTEXT	0x040	/* 1 => text file, process EOLN */
#define	FWRITE	0x020	/* 1 => open for writing */
#define	FREAD	0x010	/* 1 => open for reading */
#define	TEMP	0x008	/* 1 => temporary file */
#define	SYNC	0x004	/* 1 => window is out of sync */
#define	EOLN	0x002	/* 1 => at end of line */
#define	EOFF	0x001	/* 1 => at end of file */

/*
 * file routines
 */
extern struct iorec	*GETNAME();
extern char		*MKTEMP();
extern char		*PALLOC();

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
 * seek pointer struct for TELL, SEEK extensions
 */
struct seekptr {
	long	cnt;
};
