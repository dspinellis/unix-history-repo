/* concat.c - concatenate a bunch of strings in managed memory */

#include "../h/mh.h"
#include <stdio.h>
#include <varargs.h>


/* VARARGS */

char   *concat (va_alist)
va_dcl
{
    register char  *cp,
                   *dp,
                   *sp;
    register unsigned   len;
    register    va_list list;

    len = 1;
    va_start (list); 
    while (cp = va_arg (list, char *))
	len += strlen (cp);
    va_end (list);

    dp = sp = malloc (len);
    if (dp == NULL)
	adios (NULLCP, "unable to allocate string storage");

    va_start (list); 
    while (cp = va_arg (list, char *))
	sp = copy (cp, sp);
    va_end (list);

    return dp;
}
