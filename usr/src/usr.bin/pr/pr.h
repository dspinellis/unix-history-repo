/*-
 * Copyright (c) 1991 Keith Muller.
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego.
 *
 * %sccs.include.redist.c%
 *
 *      @(#)pr.h	5.2 (Berkeley) %G%
 */

/*
 * parameter defaults
 */
#define	CLCNT		1
#define	INCHAR		'\t'
#define	INGAP		8
#define	OCHAR		'\t'
#define OGAP		8
#define	LINES		66
#define	NMWD		5
#define	NMCHAR		'\t'
#define	SCHAR		'\t'
#define	PGWD		72
#define SPGWD		512

/*
 * misc default values
 */
#define	HDFMT		"%s %s Page %d\n\n\n"
#define	HEADLEN		5
#define	TAILLEN		5
#define	TIMEFMT		"%b %e %H:%M %Y"
#define	FNAME		""
#define	LBUF		8192
#define	HDBUF		512

/*
 * structure for vertical columns. Used to balance cols on last page
 */
struct vcol {
	char *pt;		/* ptr to col */
	int cnt;		/* char count */
};
