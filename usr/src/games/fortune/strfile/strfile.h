/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ken Arnold.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)strfile.h	5.8 (Berkeley) %G%
 */

#define	STR_ENDSTRING(line,tbl) \
	((line)[0] == (tbl).str_delim && (line)[1] == '\n')

typedef struct {				/* information table */
#define	VERSION		1
	unsigned long	str_version;		/* version number */
	unsigned long	str_numstr;		/* # of strings in the file */
	unsigned long	str_longlen;		/* length of longest string */
	unsigned long	str_shortlen;		/* length of shortest string */
#define	STR_RANDOM	0x1			/* randomized pointers */
#define	STR_ORDERED	0x2			/* ordered pointers */
#define	STR_ROTATED	0x4			/* rot-13'd text */
	unsigned long	str_flags;		/* bit field for flags */
	unsigned char	stuff[4];		/* long aligned space */
#define	str_delim	stuff[0]		/* delimiting character */
} STRFILE;
