/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)scale.c	5.1 (Berkeley) 9/21/85";
#endif not lint

extern float scalex;
extern float scaley;
extern int scaleflag;
scale(i,x,y)
char i;
float x,y;
{
	switch(i) {
	default:
		return;
	case 'c':
		x *= 2.54;
		y *= 2.54;
	case 'i':
		x /= 200;
		y /= 200;
	case 'u':
		scalex = 1/x;
		scaley = 1/y;
	}
	scaleflag = 1;
}
