/* Copyright (c) 1979 Regents of the University of California */

/* static	char sccsid[] = "@(#)objfmt.h 1.2 10/4/80"; */

#ifdef OBJ
	/*
	 *	the creation time, the size and the magic number of the obj file
	 */
    struct pxhdr {
	    int	maketime;
	    int	objsize;
	    short	magicnum;
    };

#   define	HEADER_BYTES	1024		/* the size of px_header */
#   define PX_HEADER "/usr/lib/px_header"	/* px_header's name */
#   define PX_INTRP "/usr/ucb/px"		/* the interpreter's name */
#endif OBJ

    /*
     *	the file of error messages created by mkstr
     */
#ifdef OBJ
#   define	ERR_STRNGS	"/usr/lib/pi2.0strings"
#   define	ERR_PATHLEN	9
#endif OBJ
#ifdef PC
#   define	ERR_STRNGS	"/usr/lib/pc2.0strings"
#   define	ERR_PATHLEN	9
#endif PC

	    /*
	     *	these are because of varying sizes of pointers
	     */
#ifdef VAX
#	define INDX 2				/* log2 of sizeof( * ) */
#	define PTR_AS O_AS4
#	define PTR_RV O_RV4
#	define PTR_IND O_IND4
#	define PTR_DCL unsigned long		/* for pointer variables */
#	define SHORTADDR 32768			/* maximum short address */
#	define TOOMUCH 65536			/* maximum variable size */
#	define MAXSET 65536			/* maximum set size */
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
#	    define DPOFF1		0
#	    define DPOFF2		32
#	    define INPUT_OFF		-8	/* offset of `input' */
#	    define OUTPUT_OFF		-4	/* offset of `output' */
#	endif OBJ
#	ifdef	PC
#	    define DPOFF1	( sizeof rtlocs - sizeof rtlocs.unwind )
#	    define DPOFF2	( sizeof (long) )
#	    define INPUT_OFF	0
#	    define OUTPUT_OFF	0
#	endif PC
#	define MAGICNUM 0403			/* obj magic number */
#endif VAX

#ifdef PDP11
#	define INDX 1
#	define PTR_AS O_AS2
#	define PTR_RV O_RV2
#	define PTR_IND O_IND2
#	define PTR_DCL char *
#	define TOOMUCH 50000
#	define SHORTADDR 65536
#	define MAXSET 65536			/* maximum set size */
#	define DPOFF2 16
#	define INPUT_OFF -2
#	define OUTPUT_OFF -4
#	define MAGICNUM 0404
#endif PDP11
