#ifndef lint
static char sccsid[] = "@(#)reset.c	1.1 (Berkeley/CCI) 7/5/86";
#endif

#include	"vdfmt.h"

/*
**
*/

reset()
{
	indent();
	if(get_yes_no("Confirm delete all operations and reset variables")) {
		print("All operations specified have been deleted.\n");
		exdent(-1);
		_longjmp(reset_environ, 1);
	}
	exdent(1);
}

