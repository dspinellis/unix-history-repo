#ifndef lint
static char sccsid[] = "@(#)box.c	1.1 (Berkeley) %G%";
#endif
box_(x0, y0, x1, y1)
int *x0, *y0, *x1, *y1;
{
	box(*x0, *y0, *x1, *y1);
}
