static char *sccsid = "@(#)check.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
/*
 *	check.c - checks the arguement supplied. If there are any
 *	.'s or /'s in the name a -1 is returned. 
 *	If the string is . and / free, then a zero is returned.
 *
 *	Ray Essick	23-nov-1981
 */

chkpath (p)
char   *p;
{
    int     count;
    char   *q;

    if (*p == '.') {
	return(-1);			/* hidden is bad */
    }
    count = 0;
    q = p;				/* save the head of the string */
    while (*p && (*p != '/') && (*p != ' ') && (*p != ':')) {
	count++;
	p++;
    }
    if (count > WDLEN) {			/* truncate the name */
	*(q + WDLEN) = '\0';		/* lop for WDLEN character file names */
    }
    if (count > NNLEN) {
	return(-1);			/* name too long */
    }
    if (*p == 0) {
	return(0);
    } else {
	return(-1);
    }
}

/*
 *	patcheck - look for a pattern character. These are the shell
 *	meta-characters ?, [, and *
 *	Return 0 if non exist in the string
 */
patcheck (p)
char   *p;
{
    register char  *q;
    q = p;
    while (*q && (*q != '?') && (*q != '[') && (*q != '*')) {
	q++;
    }
    return(*q != '\0');		/* return 0 if no pattern */
}
