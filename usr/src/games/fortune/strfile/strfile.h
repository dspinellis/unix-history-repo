/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ken Arnold.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)strfile.h	5.7 (Berkeley) %G%
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
