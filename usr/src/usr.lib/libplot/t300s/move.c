#ifndef lint
static char sccsid[] = "@(#)move.c	4.1 (Berkeley) 6/27/83";
#endif

move(xi,yi){
		movep(xconv(xsc(xi)),yconv(ysc(yi)));
		return;
}
