/*	cfillbuf.c	1.6	83/05/13	*/

#include <stdio.h>
#include "cpmio.h"
#include "cpmfio.h"

c_fillbuf(fptr)
	register C_FILE *fptr;
{

	int	getnext(), nsect;

	if (++fptr->c_blk == 16)
		if (fptr->c_dirp->blkcnt ==  (char) 0x80) {
			/* find next extent (if it exists) */
			if (getnext(fptr) == NULL)
				return (EOF);
		}
	else 
		return (EOF);
	nsect = (fptr->c_seccnt>blksiz/seclth) ? blksiz/seclth : fptr->c_seccnt;
	if (nsect == 0) 
		return (EOF);
	fptr->c_seccnt -= nsect;
	if (getblock(0xff & ((int)fptr->c_dirp->pointers[fptr->c_blk]),
		 fptr->c_base, nsect) == EOF)
		return (EOF);
	fptr->c_buf = fptr->c_base;
	fptr->c_cnt = nsect*seclth -1;
	return (*fptr->c_buf++&0xff);
}
