/*	@(#)strucmp.c	4.1	(Melbourne)	82/02/21	*/

strucmp(p, q, s)
register char *p, *q;
register s;
{
	while (--s >= 0)
		if (*p++ != *q++)
			return(1);
	return(0);
}
