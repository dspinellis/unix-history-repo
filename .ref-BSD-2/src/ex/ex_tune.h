/* Copyright (c) 1979 Regents of the University of California */
/*
 * Definitions of editor parameters and limits
 */

/*
 * Pathnames.
 *
 * Only exstrings is looked at "+4", i.e. if you give
 * "/usr/lib/..." here, "/lib" will be tried only for strings.
 */
#define	EXRECOVER	"/usr/lib/ex2.0recover"
#define	EXPRESERVE	"/usr/lib/ex2.0preserve"
#define	EXSTRINGS	"/usr/lib/ex2.0strings"
#define	EXHELPDIR	"/usr/lib/how_ex"
#define	MASTERTAGS	"/usr/lib/tags"

/*
 * If your system believes that tabs expand to a width other than
 * 8 then your makefile should cc with -DTABS=whatever, otherwise we use 8.
 */
#ifndef TABS
#define	TABS	8
#endif

/*
 * Maximums
 *
 * The definition of LBSIZE should be the same as BUFSIZ (512 usually).
 * Most other defitions are quite generous.
 */
/* FNSIZE is also defined in expreserve.c */
#define	FNSIZE		128		/* File name size */
#define	LBSIZE		512		/* Line length */
#define	ESIZE		128		/* Size of compiled re */
#define	RHSSIZE		256		/* Size of rhs of substitute */
#define	NBRA		9		/* Number of re \( \) pairs */
#define	TAGSIZE		32		/* Tag length */
#define	ONMSZ		32		/* Option name size */
#define	GBSIZE		256		/* Buffer size */
#define	UXBSIZE		128		/* Unix command buffer size */
#define	VBSIZE		128		/* Partial line max size in visual */
/* LBLKS is also defined in expreserve.c */
#define	LBLKS		125		/* Line pointer blocks in temp file */
#define	MAXDIRT		12		/* Max dirtcnt before sync tfile */

/*
 * These are a ridiculously small due to the
 * lousy arglist processing implementation which fixes core
 * proportional to them.  Argv (and hence NARGS) is really unnecessary,
 * and argument character space not needed except when
 * arguments exist.  Argument lists should be saved before the "zero"
 * of the incore line information and could then
 * be reasonably large.
 */
#define	NARGS	100		/* Maximum number of names in "next" */
#define	NCARGS	512		/* Maximum arglist chars in "next" */

/*
 * Note: because the routine "alloca" is not portable, TUBESIZE
 * bytes are allocated on the stack each time you go into visual
 * and then never freed by the system.  Thus if you have not terminals
 * which are larger than 24 * 80 you may well want to make TUBESIZE
 * smaller.  TUBECOLS should stay at 160 since this defines the maximum
 * length of opening on hardcopies and allows two lines of open on
 * terminals like adm3's (glass tty's) where it switches to pseudo
 * hardcopy mode when a line gets longer than 80 characters.
 */
#define	TUBELINES	36	/* Number of screen lines for visual */
#define	TUBECOLS	160	/* Number of screen columns for visual */
#define	TUBESIZE	2880	/* Maximum screen size for visual */

/*
 * Output column (and line) are set to this value on cursor addressible
 * terminals when we lose track of the cursor to force cursor
 * addressing to occur.
 */
#define	UKCOL		-20	/* Prototype unknown column */

/*
 * Attention is the interrupt character (normally 0177 -- delete).
 * Quit is the quit signal (normally FS -- control-\) and quits open/visual.
 */
#define	ATTN	0177
#define	QUIT	('\\' & 037)
