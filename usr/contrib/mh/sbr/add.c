/* add.c - concatenate two strings in managed memory */

#include "../h/mh.h"
#include <stdio.h>


char   *add (this, that)
register char  *this,
               *that;
{
    register char  *cp;

    if (!this)
	this = "";
    if (!that)
	that = "";
    if ((cp = malloc ((unsigned) (strlen (this) + strlen (that) + 1))) == NULL)
	adios (NULLCP, "unable to allocate string storage");

    (void) sprintf (cp, "%s%s", that, this);
    if (*that)
	free (that);
    return cp;
}
