/*  $Revision: 1.3 $
**
*/
#include <stdio.h>
#include <sys/types.h>
#include "configdata.h"
#include "clibrary.h"
#include "patchlevel.h"

    /* Length of string plus trailing space. */
#define S(x)	sizeof(x)

    /* Buffer size. */
#define SIZE	S("INN") + S(RELEASE) + 1 + S(PATCHLEVEL) \
		+ S(DATE) + 1 + S(LOCAL_STRING) + 1



/*
**  Return the version string.
*/
char *
INNVersion()
{
    static char		buff[SIZE + 1];
    register char	*p;
    STRING		q;

    if (buff[0] == '\0') {
	p = buff;
	p += strlen(strcpy(p, "INN"));
	*p++ = ' ';
	p += strlen(strcpy(p, RELEASE));
	*p++ = '.';
	p += strlen(strcpy(p, PATCHLEVEL));
	*p++ = ' ';
	p += strlen(strcpy(p, DATE));
	q = LOCAL_STRING;
	if (*q) {
	    *p++ = ' ';
	    *p++ = '(';
	    p += strlen(strcpy(p, q));
	    *p++ = ')';
	}
    }

    return buff;
}
