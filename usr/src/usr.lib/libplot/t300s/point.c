#ifndef lint
static char sccsid[] = "@(#)point.c	4.1 (Berkeley) 6/27/83";
#endif

#include "con.h"
point(xi,yi){
		move(xi,yi);
		label(".");
		return;
}
