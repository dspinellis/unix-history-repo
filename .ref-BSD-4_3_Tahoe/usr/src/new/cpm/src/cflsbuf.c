/*	cflsbuf.c	1.10	83/05/13	*/

#include <stdio.h>
#include "cpmio.h"
#include "cpmfio.h"

/*
 * Flush a full block to floppy disk file
 * (these routines are never called unless a full block is
 * to be written)
 * Create a new extent if required
 */

c_flush(fptr)
	register C_FILE *fptr;
{

	int	it; 
	char	alloc();

	if (!(fptr->c_flag & WRITE)) {
		fprintf(stderr, "no write access");
		return (EOF);
	}
	fptr->c_seccnt += blksiz/seclth;
	if (putblock(0xff & (int)fptr->c_dirp->pointers[fptr->c_blk++],
				 fptr->c_base, -1) == EOF)
		return (EOF);
	if (fptr->c_blk == 16) {
		fptr->c_dirp->blkcnt =  (char) 0x80;
		savedir();
		/* create new extent */
		if ((it = creext(fptr->c_ext)) == NULL) {
			fprintf(stderr,"can't create new extent, current: %d\n",
				fptr->c_ext);
			return (EOF);
		}
		fptr->c_dirp = dirbuf+it;
		fptr->c_ext  = it;
		fptr->c_blk  = 0;
		fptr->c_seccnt = 0;
		fptr->c_extno++;
		fptr->c_dirp->extno= fptr->c_extno;
	}
	fptr->c_buf = fptr->c_base;
	fptr->c_cnt = blksiz;
	if ((fptr->c_dirp->pointers[fptr->c_blk] = alloc()) == '\0') {
		fprintf(stderr, "disk full\n");
		return (EOF);
	}
	return (0);
}

c_flsbuf(c, fptr)
	register C_FILE *fptr;
{
	if (c_flush(fptr) == EOF)
		return (EOF);
	*(fptr->c_buf++) = c;
	fptr->c_cnt--;
	return (c);
}

/*
 * move the contents of 'buf' into the cpm block buffer,
 * flush the buffer if full (for binary file transfers)
 */

c_write(fptr, buf, cnt)
	register C_FILE *fptr;
	char *buf;
{
	int i = cnt;

	while (i-- > 0) {
		*(fptr->c_buf++) = *(buf++);
		if (--fptr->c_cnt == 0)
			if (c_flush(fptr) == EOF)
				return (EOF);
	}
	return (cnt);
}
