/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)i_dim.c	5.1	6/7/85
 */

long int i_dim(a,b)
long int *a, *b;
{
return( *a > *b ? *a - *b : 0);
}
