#include "astosc.h"
#include "state.h"

#include "../ctlr/function.h"

struct astosc astosc[256] = {
#include "astosc.out"
};


/*
 * This routine takes a string and returns an integer.  It may return
 * STATE_NULL if there is no other integer which corresponds to the
 * string.  STATE_NULL implies an error.
 */

int
ascii_to_index(string)
register char *string;
{
    register struct astosc *this;

    for (this = astosc; this <= &astosc[highestof(astosc)]; this++) {
	if ((this->name[0] == string[0]) && (strcmp(this->name, string) == 0)) {
	    return this-astosc;
	}
    }
    return STATE_NULL;
}
