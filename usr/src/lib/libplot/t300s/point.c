#ifndef lint
static char sccsid[] = "@(#)point.c	4.1 (Berkeley) %G%";
#endif

#include "con.h"
point(xi,yi){
		move(xi,yi);
		label(".");
		return;
}
