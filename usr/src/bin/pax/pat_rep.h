/*-
 * Copyright (c) 1992 Keith Muller.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pat_rep.h	1.1 (Berkeley) %G%
 */

/*
 * data structure for storing user supplied replacement strings (-s)
 */
typedef struct replace {
	char		*nstr;	/* the new string we will substitute with */
#	ifdef NET2_REGEX
	regexp		*rcmp;	/* compiled regular expression used to match */
#	else
	regex_t		rcmp;	/* compiled regular expression used to match */
#	endif
	int		flgs;	/* print conversions? global in operation?  */
#define	PRNT		0x1
#define	GLOB		0x2
	struct replace	*fow;	/* pointer to next pattern */
} REPLACE;
