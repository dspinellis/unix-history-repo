/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)1.main.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include "def.h"
int endbuf;

mkgraph()
	{
	if (!parse())
		return(FALSE);
	hash_check();
	hash_free();
	fingraph();
	return(TRUE);
	}
