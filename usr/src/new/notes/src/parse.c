#include	"dump.h"
#include	<stdio.h>

#ifdef	RCSIDENT
static char *RCSid = "$Header: parse.c,v 1.7 85/01/18 15:42:07 notes Rel $";
#endif	RCSIDENT

/*
 *	given a header line in the basic RFC-822 format,
 *	this routine scans the name and determines a variable
 *	name for it.
 *
 *	Ray Essick, March 1984
 *		Stolen from the code I did for the BA 456 class....
 */

rfcparse (line, varlist)
char   *line;
struct dump_f  *varlist;
{
    char    name[BUFSIZ];				/* hold name */

    if (sscanf (line, "%[^:]:", name) != 1)		/* get field name */
    {
	return (-1);					/* bogus line */
    }

    for (; varlist -> du_name[0]; varlist++)		/* empty string at end */
    {
	if (!strcmp (name, varlist -> du_name))		/* matched */
	    return (varlist -> du_number);		/* which is it */
    }
    return (-1);					/* never reached */
}
