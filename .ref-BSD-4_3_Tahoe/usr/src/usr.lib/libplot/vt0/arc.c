#ifndef lint
static char sccsid[] = "@(#)arc.c	4.1 (Berkeley) 6/27/83";
#endif

extern vti;
arc(xi,yi,x0,y0,x1,y1){
	char c;
	c = 6;
	write(vti,&c,1);
	write(vti,&xi,12);
}
