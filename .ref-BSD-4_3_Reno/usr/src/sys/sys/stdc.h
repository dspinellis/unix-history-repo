/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)stdc.h	7.3 (Berkeley) 6/28/90
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
