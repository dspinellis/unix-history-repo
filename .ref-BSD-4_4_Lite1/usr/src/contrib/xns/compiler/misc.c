#ifndef lint
static char RCSid[] = "$Header: misc.c,v 2.1 86/10/11 15:47:55 jqj Exp $";
#endif

/* $Log:	misc.c,v $
 * Revision 2.1  86/10/11  15:47:55  jqj
 * in stringtocard(), if passed a null pointer return 0. (per Liebman)
 * 
 * Revision 2.0  85/11/21  07:21:41  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.3  85/03/11  16:39:50  jqj
 * *** empty log message ***
 * 
 * Revision 1.3  85/03/11  16:39:50  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/02/21  11:05:34  jqj
 * alpha test version
 * 
 * Revision 1.1  85/02/15  13:55:34  jqj
 * Initial revision
 * 
 */

#include "compiler.h"

/*
 * String allocation.
 */
char *
copy(s)
	char *s;
{
	char *p;
	extern char *malloc();

	if ((p = malloc(strlen(s) + 1)) == NULL) {
		error(FATAL, "out of string space");
	}
	(void) strcpy(p, s);
	return (p);
}


/*
 * like atoi, convert a string to an integer.  Accept
 * 1/ numeric string, e.g. "34"
 * 2/ hex string, e.g. "0x22"
 * 3/ octal string, e.g. "042"
 * Handles only positive integers.
 */
int
stringtocard(str)
	char *str;
{
	int val;

	if (str == (char *) NULL)
		return(0);	/* sanity check */
	if (
	    sscanf(str, " -0x%x", &val) > 0 ||
	    sscanf(str, " -0%o", &val) > 0)
		return( -val);
	if (
	    sscanf(str, " 0x%x", &val) > 0 ||
	    sscanf(str, " 0%o", &val) > 0 ||
	    sscanf(str, " %d", &val) > 0 
	   )
		return(val);
	return(0);
}


/*
 * Lisp operations.
 */
list
cons(a, b)
	list a, b;
{
	list p;

	if ((p = New(struct cons)) == NIL) {
		error(FATAL,"Out of list space.");
	}
	car(p) = a;
	cdr(p) = b;
	return (p);
}

length(p)
	list p;
{
	int n;

	for (n = 0; p != NIL; p = cdr(p), n++)
		;
	return (n);
}

list
nconc(p, q)
	list p, q;
{
	list pp;

	pp = p;
	if (p == NIL)
		return (q);
	while (cdr(p) != NIL)
		p = cdr(p);
	cdr(p) = q;
	return (pp);
}

