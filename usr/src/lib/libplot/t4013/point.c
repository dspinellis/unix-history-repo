/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)point.c	8.1 (Berkeley) %G%";
#endif /* not lint */

point(xi,yi){
	move(xi,yi);
	cont(xi,yi);
}
