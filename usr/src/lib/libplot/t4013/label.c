#ifndef lint
static char sccsid[] = "@(#)label.c	1.1 (Berkeley) %G%";
#endif

label(s)
char *s;
{
	register i,c;
	putch(037);	/* alpha mode */
	for(i=0; c=s[i]; i++)
		putch(c);
}
