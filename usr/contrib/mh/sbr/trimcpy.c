/* trimcpy.c - strip [lt]wsp and replace newlines with spaces */

#include "../h/mh.h"
#include <ctype.h>
#include <stdio.h>


char *trimcpy (cp)
register char *cp;
{
    register char  *sp;

    while (isspace (*cp))
	cp++;
    for (sp = cp + strlen (cp) - 1; sp >= cp; sp--)
	if (isspace (*sp))
	    *sp = NULL;
	else
	    break;
    for (sp = cp; *sp; sp++)
	if (isspace (*sp))
	    *sp = ' ';

    return getcpy (cp);
}
