/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)bit.h	5.1 (Berkeley) 4/26/85
 */

/*
 * Bit matrix manipulations for font editor.
 *
 * General structure of a bit matrix: each row is packed into as few
 * bytes as possible, taking the bits from left to right within bytes.
 * The matrix is a sequence of such rows, i.e. up to 7 bits are wasted
 * at the end of each row.
 */

#include <stdio.h>
typedef char *	bitmat;
#ifdef TRACE
	FILE *trace;
#endif

#define max(x,y)	((x) > (y) ?   (x)  : (y))
#define min(x,y)	((x) < (y) ?   (x)  : (y))
