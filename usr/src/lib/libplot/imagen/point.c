/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)point.c	5.2 (Berkeley) %G%";
#endif /* not lint */

point(xi,yi){
	line(xi,yi,xi,yi);
}
