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

/*
 * A function call
 */
funccod(r)
	int *r;
{

	call(r[1], r[2]);
}
