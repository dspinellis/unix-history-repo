/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)circle.c	5.1 (Berkeley) 9/21/85";
#endif not lint

circle(x,y,r){
	arc(x,y,x+r,y,x+r,y);
}
