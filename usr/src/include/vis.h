/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
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
 *	@(#)vis.h	5.3 (Berkeley) %G%
 */


/*
 * vis flags
 */

/*
 * to select alternate encoding format
 */
#define	VIS_OCTAL	0x01	/* use octal \ddd format */
#define	VIS_CSTYLE	0x02	/* use \[nrft...] where appropiate */
/*
 * to alter set of characters encoded
 * (default is to encode all non-graphic except space, tab, and newline)
 */
#define	VIS_TAB		0x04	/* also encode tab */
#define	VIS_WHITE	0x08	/* also encode space, tab, and newline */
#define	VIS_SAFE	0x10	/* only encode "unsafe" characters */
/*
 * other
 */
#define	VIS_NOSLASH	0x20	/* inhibit printing '\' */
#define	VIS_NEXTC	0x40	/* nextc character is valid */

extern char *vis();

/*
 * unvis flags
 */

#define	UNVIS_HAT	0x01	/* treat ^ as special */
#define	UNVIS_END	0x02	/* reset decode machine */

/*
 * return codes
 */
#define	UNVIS_NEEDMORE	0	/* not done, feed another char */
#define	UNVIS_OK		1	/* done, character ready */
#define	UNVIS_OKPUSH	2	/* done, push back passed char */
#define	UNVIS_NOCHAR	3	/* done, no character ready (hidden newline) */
#define	UNVIS_SYNBAD	-1	/* unrecognized escape sequence */
