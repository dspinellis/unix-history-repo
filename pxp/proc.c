#
/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
#include "0.h"
#include "tree.h"

proc(r)
	int *r;
{

	call(r[2], r[3]);
}
