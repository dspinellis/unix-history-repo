/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)objfmt.h	5.4 (Berkeley) %G%
 */

/*
 * The size of the display.
 */
#define DSPLYSZ 20

/*
 *	The structure of the runtime display
 */
#ifdef OBJ
struct dispsave {
	char *locvars;		/* pointer to local variables */
	struct blockmark *stp;	/* pointer to local stack frame */
};
	/*
	 * The following union allows fast access to
	 * precomputed display entries
	 */
union display {
	struct dispsave frame[DSPLYSZ];
	char *raw[2*DSPLYSZ];
} display;
#endif OBJ
#ifdef PC
#ifdef vax
	/*
	 *	the display is made up of saved AP's and FP's.
	 *	FP's are used to find locals,
	 *	and AP's are used to find parameters.
	 *	FP and AP are untyped pointers,
	 *	but are used throughout as (char *).
	 *	the display is used by adding AP_OFFSET or FP_OFFSET to the 
	 *	address of the approriate display entry.
	 */
    struct dispsave {
	char	*savedAP;
	char	*savedFP;
    } display[ DSPLYSZ ];

#   define	AP_OFFSET	( 0 )
#   define	FP_OFFSET	( sizeof (char *) )
#endif vax
#ifdef mc68000
	/*
	 *	the display is just the saved a6.
	 *	arguments are at positive offsets,
	 *	locals are at negative offsets.
	 *	there are no offsets within the saved display structure.
	 */
    struct dispsave {
	char	*saveda6;
    } display[ DSPLYSZ ];

#   define	AP_OFFSET	(0)
#   define	FP_OFFSET	(0)
#endif mc68000
#ifdef tahoe
	/*
	 *	the display is just the saved FP.
	 *	arguments are at positive offsets,
	 *	locals are at negative offsets.
	 *	there are no offsets within the saved display structure.
	 */
    struct dispsave {
	char	*savedFP;
    } display[ DSPLYSZ ];

#   define	AP_OFFSET	0
#   define	FP_OFFSET	0
#endif tahoe
#endif PC

    /*
     *	the structure below describes the block mark used by the architecture.
     *	this is the space used by the machine between the arguments and the
     *	whatever is used to point to the arguments.
     */
#ifdef OBJ
struct blockmark {
	char *tos;		/* pointer to top of stack frame */
	struct iorec *file;	/* pointer to active file name */
	struct hdr {
		long framesze;	/* number of bytes of local vars */
		long nargs;	/* number of bytes of arguments */
		long tests;	/* TRUE => perform runtime tests */
		short offset;	/* offset of procedure in source file */
		char name[1];	/* name of active procedure */
	} *entry;
	struct dispsave odisp;	/* previous display value for this level */
	struct dispsave *dp;	/* pointer to active display entry */
	char *pc;		/* previous location counter */
	long lino;		/* previous line number */
};
#endif OBJ
#ifdef PC
#ifdef vax
	/*
	 *	since we have the ap pointing to the number of args:
	 */
    struct blockmark {
        long	nargs;
    };
#endif vax
#ifdef mc68000
	/*
	 *	there's the saved pc (from the jsr)
	 *	and the saved a6 (from the link a6).
	 */
    struct blockmark {
	char	*savedpc;
	char	*saveda6;
    };
#endif mc68000
#ifdef tahoe
	/*
	 *	since we have the fp pointing to its predecessor
	 */
    struct blockmark {
	long	savedfp;
    };
#endif tahoe
#endif PC

    /*
     *	formal routine structure:
     */
struct formalrtn {
	long		(*fentryaddr)();	/* formal entry point */
	long		fbn;			/* block number of function */
	struct dispsave	fdisp[ DSPLYSZ ];	/* saved at first passing */
};
#ifndef PC
#ifndef OBJ
struct formalrtn	frtn;
#endif
#endif

#define	FENTRYOFFSET	0
#define FBNOFFSET	( FENTRYOFFSET + sizeof frtn.fentryaddr )
#define	FDISPOFFSET	( FBNOFFSET + sizeof frtn.fbn )

#ifdef OBJ
	/*
	 *	the creation time, the size and the magic number of the obj file
	 */
    struct pxhdr {
	    long	maketime;
	    long	objsize;
	    long	symtabsize;
	    short	magicnum;
    };

/*
 *	START defines the beginning of the text space.
 *	This should be the defined external label "start",
 *	however there is no way to access externals from C
 *	whose names do not begin with an "_".
 */
#ifdef vax
#   define HEADER_BYTES	2048			/* the size of px_header */
#   define START 0x0				/* beginning of text */
#endif vax
#ifdef tahoe
#   define HEADER_BYTES	2560			/* the size of px_header */
#   define START 0x0				/* beginning of text */
#endif tahoe
#ifdef mc68000
#   define HEADER_BYTES	3072			/* the size of px_header */
#   define START 0x8000				/* beginning of text */
#endif mc68000
#   define INDX 1				/* amt to shift display index */
#endif OBJ

	    /*
	     *	these are because of varying sizes of pointers
	     */
#ifdef ADDR16
#	define PTR_AS O_AS2
#	define PTR_RV O_RV2
#	define PTR_IND O_IND2
#	define PTR_CON O_CON2
#	define PTR_DUP O_SDUP2
#	define CON_INT O_CON2
#	define INT_TYP (nl + T2INT)
#	define PTR_DCL char *
#	define TOOMUCH 50000
#	define SHORTADDR 65536
#	define MAXSET 65536		/* maximum set size */
#endif ADDR16
#ifdef ADDR32
#	define PTR_AS O_AS4
#	define PTR_RV O_RV4
#	define PTR_IND O_IND4
#	define PTR_CON O_CON4
#	define PTR_DUP O_SDUP4
#	define CON_INT O_CON24
#	define INT_TYP (nl + T4INT)
#	define PTR_DCL unsigned long		/* for pointer variables */
#	define SHORTADDR 32768			/* maximum short address */
#	define TOOMUCH 65536			/* maximum variable size */
#	define MAXSET 65536			/* maximum set size */
#endif ADDR32
	/*
	 * Offsets due to the structure of the runtime stack.
	 * DPOFF1	is the amount of fixed storage in each block allocated
	 * 		as local variables for the runtime system.
	 *		since locals are allocated negative offsets,
	 *		-DPOFF1 is the last used implicit local offset.
	 * DPOFF2	is the size of the block mark.
	 *		since arguments are allocated positive offsets,
	 *		DPOFF2 is the end of the implicit arguments.
	 *		for obj, the first argument has the highest offset
	 *		from the stackpointer.  and the block mark is an
	 *		implicit last parameter.
	 *		for pc, the first argument has the lowest offset
	 *		from the argumentpointer.  and the block mark is an
	 *		implicit first parameter.
	 */
#	ifdef OBJ
#	    ifdef ADDR32
#		define MAGICNUM 0403	/* obj magic number */
#		define DPOFF1		0
#		define DPOFF2		(sizeof (struct blockmark))
#		define INPUT_OFF	-8	/* offset of `input' */
#		define OUTPUT_OFF	-4	/* offset of `output' */
#	    endif ADDR32
#	    ifdef ADDR16
#		define MAGICNUM 0404
#		define DPOFF1		0
#		define DPOFF2		(sizeof (struct blockmark))
#		define INPUT_OFF	-2
#		define OUTPUT_OFF	-4
#	    endif ADDR16
#	endif OBJ
#	ifdef	PC
#	    define DPOFF1	( sizeof (struct rtlocals) )
#	    define DPOFF2	( sizeof (struct blockmark) )
#	    define INPUT_OFF	0
#	    define OUTPUT_OFF	0
#	endif PC
