/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)general.h	1.5 (Berkeley) 6/28/90
 */

/*
 * Some general definitions.
 */


#define	numberof(x)	(sizeof x/sizeof x[0])
#define	highestof(x)	(numberof(x)-1)

#if	defined(unix)
#define	ClearElement(x)		bzero((char *)&x, sizeof x)
#define	ClearArray(x)		bzero((char *)x, sizeof x)
#else	/* defined(unix) */
#define	ClearElement(x)		memset((char *)&x, 0, sizeof x)
#define	ClearArray(x)		memset((char *)x, 0, sizeof x)
#endif	/* defined(unix) */

#if	defined(unix)		/* Define BSD equivalent mem* functions */
#define	memcpy(dest,src,n)	bcopy(src,dest,n)
#define	memmove(dest,src,n)	bcopy(src,dest,n)
#define	memset(s,c,n)		if (c == 0) { \
				    bzero(s,n); \
				} else { \
				    register char *src = s; \
				    register int count = n; \
					\
				    while (count--) { \
					*src++ = c; \
				    } \
				}
#define	memcmp(s1,s2,n)		bcmp(s1,s2,n)
#endif	/* defined(unix) */
