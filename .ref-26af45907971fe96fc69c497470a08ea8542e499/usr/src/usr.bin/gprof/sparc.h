/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sparc.h	5.1 (Berkeley) %G%
 */

/*
 * offset (in bytes) of the code from the entry address of a routine.
 * (see asgnsamples for use and explanation.)
 */
#define OFFSET_OF_CODE	0
#define	UNITS_TO_CODE	(OFFSET_OF_CODE / sizeof(UNIT))

enum opermodes { dummy };
typedef enum opermodes	operandenum;
