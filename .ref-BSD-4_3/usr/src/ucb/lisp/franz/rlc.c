#ifndef lint
static char *rcsid =
   "$Header: rlc.c,v 1.4 85/05/22 07:53:51 sklower Exp $";
#endif

/*					-[Sat Jan 29 13:32:26 1983 by jkf]-
 * 	rlc.c				$Locker:  $
 * relocator for data space 
 *
 * (c) copyright 1982, Regents of the University of California
 */

#define TRUE 1
#include "h/global.h"
#if vax_4_2 | vax_4_3
#define brk(p) syscall(17,p)
#endif
extern char holend[], end[];
extern int usehole;
extern char *curhbeg;

rlc()
{
	char *cp, *dp;
	
	brk(end);
	dp = holend;
	cp = dp - HOLE;
	while (dp < end)
		*dp++ = *cp++;
	curhbeg = holend - HOLE;	/* set up the hole */
	usehole = TRUE;
}
