/* getcpy.c - copy a string in managed memory */

#include "../h/mh.h"
#include <stdio.h>


char   *getcpy (str)
register char  *str;
{
    register char  *cp;

    if ((cp = malloc ((unsigned) (strlen (str) + 1))) == NULL)
	adios (NULLCP, "unable to allocate string storage");

    (void) strcpy (cp, str);
    return cp;
}
