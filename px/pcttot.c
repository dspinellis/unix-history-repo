#
#include "0x.h"
#include "E.h"

/*
 * Constant set constructor (yechh!)
 */
pcttot(uprbp, lwrb, n, av)
{
	register *set;
	register l;
	int *ap, h;

	ap = &av;
	set = &ap[2 * n];
	while(--n >= 0) {
		if ((l = *ap++ - lwrb) < 0 || l > uprbp ||
		    (h = *ap++ - lwrb) < 0 || h > uprbp)
			error(ECTTOT);
		while (l <= h) {
			set[l >> 4] =| 1 << (l & 017);
			l++;
		}
	}
	return(set);
}
