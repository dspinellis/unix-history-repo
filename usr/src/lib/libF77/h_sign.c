/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)h_sign.c	5.1	6/7/85
 */

short h_sign(a,b)
short *a, *b;
{
short x;
x = (*a >= 0 ? *a : - *a);
return( *b >= 0 ? x : -x);
}
