/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pagsiz.h	5.1 (Berkeley) %G%
 */

#define	NBPG	512
#define	PGOFSET	511
#define	CLSIZE	2
#define	CLOFSET	1023
#define	PAGSIZ	(NBPG*CLSIZE)
#define	PAGRND	((PAGSIZ)-1)
