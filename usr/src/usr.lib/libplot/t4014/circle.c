#ifndef lint
static char sccsid[] = "@(#)circle.c	4.1 (Berkeley) 6/27/83";
#endif

circle(x,y,r){
	arc(x,y,x+r,y,x+r,y);
}
