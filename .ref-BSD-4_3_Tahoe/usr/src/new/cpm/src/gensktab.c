/*	gensktab.c	1.4	83/05/13	*/
#include <stdio.h>
#include "cpmio.h"

int	skew;
int	*skewtab;

/*
 * Generate a skew factor table in skewtab according to the
 * global parameters given by skew and sectrk.
 * This routine must be called before any disk (or virtual disk)
 * accesses are attempted.
 */

gen_sktab()
{

	int	*i, *j;
	char	*malloc();

	if (( skewtab = (int *) malloc(sectrk*4)) == NULL) {
		fprintf(stderr, 
			"can't allocate memory for skew sector table\n");
		exit(1);
	}
	*skewtab = 1;
	for (i=skewtab+1; i<skewtab+ sectrk; i++) {
		*i = *(i-1) + skew;
		if (*i > sectrk) {
			*i -= sectrk;
L1:
			for (j=skewtab; j<i; j++) {
				if (*j == *i) 
					break;
			}
			if (j < i ) { 
				++*i; 
				goto L1; 
			}
		}
	}
}
