/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)besyn_.c	5.2 (Berkeley) 4/12/91";
#endif /* not lint */

double yn();

float besyn_(n, x)
long *n; float *x;
{
	return((float)yn((int)*n, (double)*x));
}
