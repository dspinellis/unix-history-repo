/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ex_tune.h	7.12 (Berkeley) %G%
 */

#include "pathnames.h"

/*
 * Definitions of editor parameters and limits
 */

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
 * Most other definitions are quite generous.
 */
/* FNSIZE is also defined in expreserve.c */
#define	FNSIZE		128		/* File name size */
#ifdef VMUNIX
#define	LBSIZE		1024
#define	ESIZE		512
#define CRSIZE		1024
#else
#ifdef u370
#define LBSIZE		4096
#define ESIZE		512
#define CRSIZE		4096
#else
#ifdef vms
#define LBSIZE		1024
#define	ESIZE		512
#define	CRSIZE		1024
#else
#define	LBSIZE		512		/* Line length */
#define	ESIZE		128		/* Size of compiled re */
#define CRSIZE		512
#endif
#endif
#endif
#define	RHSSIZE		256		/* Size of rhs of substitute */
#define	NBRA		9		/* Number of re \( \) pairs */
#define	TAGSIZE		128		/* Tag length */
#define	ONMSZ		64		/* Option name size */
#define	GBSIZE		256		/* Buffer size */
#define	UXBSIZE		128		/* Unix command buffer size */
#define	VBSIZE		128		/* Partial line max size in visual */
/* LBLKS is also defined in expreserve.c */
#ifndef VMUNIX
#define	LBLKS		125		/* Line pointer blocks in temp file */
#define	HBLKS		1		/* struct header fits in BUFSIZ*HBLKS */
#else
#define	LBLKS		900
#define	HBLKS		2
#endif
#define	MAXDIRT		12		/* Max dirtcnt before sync tfile */
#define TCBUFSIZE	2048		/* Max entry size in termcap, see
					   also termlib and termcap */

/*
 * Except on VMUNIX, these are a ridiculously small due to the
 * lousy arglist processing implementation which fixes core
 * proportional to them.  Argv (and hence NARGS) is really unnecessary,
 * and argument character space not needed except when
 * arguments exist.  Argument lists should be saved before the "zero"
 * of the incore line information and could then
 * be reasonably large.
 */
#ifndef	vms
#undef NCARGS
#endif
#ifndef VMUNIX
#define	NARGS	100		/* Maximum number of names in "next" */
#define	NCARGS	LBSIZE		/* Maximum arglist chars in "next" */
#else
#define	NCARGS	5120
#define	NARGS	(NCARGS/6)
#endif

/*
 * Note: because the routine "alloca" is not portable, TUBESIZE
 * bytes are allocated on the stack each time you go into visual
 * and then never freed by the system.  Thus if you have no terminals
 * which are larger than 24 * 80 you may well want to make TUBESIZE
 * smaller.  TUBECOLS should stay at 160 since this defines the maximum
 * length of opening on hardcopies and allows two lines of open on
 * terminals like adm3's (glass tty's) where it switches to pseudo
 * hardcopy mode when a line gets longer than 80 characters.
 */
#ifndef VMUNIX
#define	TUBELINES	60	/* Number of screen lines for visual */
#define	TUBECOLS	160	/* Number of screen columns for visual */
#define	TUBESIZE	5000	/* Maximum screen size for visual */
#else
#define	TUBELINES	100
#define	TUBECOLS	160
#define	TUBESIZE	(TUBELINES * 100)
#endif

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
#define	ATTN	(-2)	/* mjm: (char) ??  */
#define	QUIT	('\\' & 037)
