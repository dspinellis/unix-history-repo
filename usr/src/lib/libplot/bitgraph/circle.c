#ifndef lint
static char sccsid[] = "@(#)circle.c	4.1 (Berkeley) %G%";
#endif

circle (xc,yc,r)
int xc,yc,r;
{
	arc(xc,yc, xc+r,yc, xc-r,yc);
	arc(xc,yc, xc-r,yc, xc+r,yc);
}
