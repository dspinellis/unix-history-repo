/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)general.h	5.2 (Berkeley) %G%
 */

/*
 * Some general definitions.
 */


#define	numberof(x)	(sizeof x/sizeof x[0])
#define	highestof(x)	(numberof(x)-1)

#define	ClearElement(x)		memset((char *)&x, 0, sizeof x)
#define	ClearArray(x)		memset((char *)x, 0, sizeof x)
