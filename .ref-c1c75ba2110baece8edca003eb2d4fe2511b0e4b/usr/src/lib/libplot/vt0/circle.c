#ifndef lint
static char sccsid[] = "@(#)circle.c	4.1 (Berkeley) %G%";
#endif

extern vti;
circle(x,y,r){
	char c;
	c = 5;
	write(vti,&c,1);
	write(vti,&x,6);
}
