#ifndef lint
static char sccsid[] = "@(#)line.c	4.1 (Berkeley) 6/27/83";
#endif

extern vti;
extern xnow,ynow;
line(x0,y0,x1,y1){
	struct{char x,c; int x0,y0,x1,y1;} p;
	p.c = 3;
	p.x0 = xsc(x0);
	p.y0 = ysc(y0);
	p.x1 = xnow = xsc(x1);
	p.y1 = ynow = ysc(y1);
	write(vti,&p.c,9);
}
cont(x0,y0){
	line(xnow,ynow,xsc(x0),ysc(y0));
	return;
}
