/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vis.h	8.1 (Berkeley) %G%
 */

#ifndef _VIS_H_
#define	_VIS_H_

/*
 * to select alternate encoding format
 */
#define	VIS_OCTAL	0x01	/* use octal \ddd format */
#define	VIS_CSTYLE	0x02	/* use \[nrft0..] where appropiate */

/*
 * to alter set of characters encoded (default is to encode all
 * non-graphic except space, tab, and newline).
 */
#define	VIS_SP		0x04	/* also encode space */
#define	VIS_TAB		0x08	/* also encode tab */
#define	VIS_NL		0x10	/* also encode newline */
#define	VIS_WHITE	(VIS_SP | VIS_TAB | VIS_NL)
#define	VIS_SAFE	0x20	/* only encode "unsafe" characters */

/*
 * other
 */
#define	VIS_NOSLASH	0x40	/* inhibit printing '\' */

/*
 * unvis return codes
 */
#define	UNVIS_VALID	 1	/* character valid */
#define	UNVIS_VALIDPUSH	 2	/* character valid, push back passed char */
#define	UNVIS_NOCHAR	 3	/* valid sequence, no character produced */
#define	UNVIS_SYNBAD	-1	/* unrecognized escape sequence */
#define	UNVIS_ERROR	-2	/* decoder in unknown state (unrecoverable) */

/*
 * unvis flags
 */
#define	UNVIS_END	1	/* no more characters */

#include <sys/cdefs.h>

__BEGIN_DECLS
char	*vis __P((char *, int, int, int));
int	strvis __P((char *, const char *, int));
int	strvisx __P((char *, const char *, size_t, int));
int	strunvis __P((char *, const char *));
int	unvis __P((char *, int, int *, int));
__END_DECLS

#endif /* !_VIS_H_ */
