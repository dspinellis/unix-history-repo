#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * 	Error - Default non-fatal xwm error reporting routine.  Called when
 * 	an random xwm error is encountered.
 *
 */
#ifndef lint
static char *rcsid_Error_c = "$Header: Error.c,v 10.3 86/02/01 16:09:23 tony Rel $";
#endif

#include "xwm.h"

Error(string)
	char *string;	/* Error description string. */
{
	fprintf(stderr, "\nxwm: %s", string);
	fprintf(stderr, "\n\n");
	exit(1);
}
