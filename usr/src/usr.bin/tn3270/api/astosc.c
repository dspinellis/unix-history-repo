#include <ctype.h>

#include "../general/general.h"

#include "../ctlr/function.h"

#include "astosc.h"

struct astosc astosc[256] = {
#include "astosc.out"
};

/* compare two strings, ignoring case */

static
ustrcmp(string1, string2)
register char *string1;
register char *string2;
{
    register int c1, c2;

    while ((c1 = (unsigned char) *string1++) != 0) {
	if (isupper(c1)) {
	    c1 = tolower(c1);
	}
	if (isupper(c2 = (unsigned char) *string2++)) {
	    c2 = tolower(c2);
	}
	if (c1 < c2) {
	    return(-1);
	} else if (c1 > c2) {
	    return(1);
	}
    }
    if (*string2) {
	return(-1);
    } else {
	return(0);
    }
}


/*
 * This routine takes a string and returns an integer.  It may return
 * -1 if there is no other integer which corresponds to the
 * string.  -1 implies an error.
 */

int
ascii_to_index(string)
register char *string;
{
    register struct astosc *this;

    for (this = astosc; this <= &astosc[highestof(astosc)]; this++) {
	if (ustrcmp(this->name, string) == 0) {
	    return this-astosc;
	}
    }
    return -1;
}
