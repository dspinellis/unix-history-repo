/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)floor.s	5.1 (Berkeley) 5/17/90
 */

	.text
	.globl	_floor,_ceil,_rint

| floor(x)
| the largest integer no larger than x
_floor:
	fmovel	fpcr,d0		| save old FPCR
	fmoved	sp@(4),fp0	| get argument
	fbun	Lret		| if NaN, return NaN
	fboge	Lrtz		| >=0, round to zero
	fmovel	#0x20,fpcr	| <0, round to -inf
	jra	Ldoit

| ceil(x)
| -floor(-x), for all real x
_ceil:
	fmovel	fpcr,d0		| save old FPCR
	fmoved	sp@(4),fp0	| get argument
	fbun	Lret		| if NaN, return NaN
	fbolt	Lrtz		| <0, round to zero
	fmovel	#0x30,fpcr	| >=0, round to inf
	jra	Ldoit

Lrtz:
	fmovel	#0x10,fpcr
Ldoit:
	fintd	sp@(4),fp0	| truncate
	fmovel	d0,fpcr		| restore old FPCR
Lret:
	fmoved	fp0,sp@-
	movel	sp@+,d0
	movel	sp@+,d1
	rts

| rint(x)
| delivers integer nearest x in direction of prevailing rounding mode
_rint:
	fintd	sp@(4),fp0	| use prevailing rounding mode
	jra	Lret
