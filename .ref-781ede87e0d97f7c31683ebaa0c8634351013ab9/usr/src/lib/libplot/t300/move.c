#ifndef lint
static char sccsid[] = "@(#)move.c	4.1 (Berkeley) %G%";
#endif

move(xi,yi){
		movep(xconv(xsc(xi)),yconv(ysc(yi)));
		return;
}
