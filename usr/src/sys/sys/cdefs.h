/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cdefs.h	7.3 (Berkeley) %G%
 */

/*
 * This file is designed to ease the porting from standard C to ANSI C.
 * It will eventually go away.
 * Questions to K. Bostic.
 */

#if __STDC__ || c_plusplus
#define	CONCAT(x,y)	x ## y
#define	PROTOTYPE(p)	p
#define	STRING(x)	#x
#else
#define	const
#define	volatile
#define	signed
#define	CONCAT(x,y)	x/**/y
#define	PROTOTYPE(p)	()
#define	STRING(x)	"x"
#endif
