static char Sccsid[] "@(#)xpipe	2.1";
/*
	Interface to pipe(II) which handles all error conditions.
	Returns 0 on success,
	fatal() on failure.
*/

xpipe(t)
int *t;
{
	static char p[] "pipe";

	if (pipe(t) == 0)
		return(0);
	return(xmsg(p,p));
}
