/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)circle.c	5.2 (Berkeley) %G%";
#endif /* not lint */

circle(x,y,r){
	arc(x,y,x+r,y,x+r,y);
}
