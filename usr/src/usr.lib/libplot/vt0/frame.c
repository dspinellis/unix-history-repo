#ifndef lint
static char sccsid[] = "@(#)frame.c	4.1 (Berkeley) 6/27/83";
#endif

frame(n)
{
	extern vti;
	n=n&0377 | 02000;
	write(vti,&n,2);
}
