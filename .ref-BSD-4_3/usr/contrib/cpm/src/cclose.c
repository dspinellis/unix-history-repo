/*	cclose.c	1.10	83/05/13	*/
#include <stdio.h>
#include "cpmio.h"
#include "cpmfio.h"

/*
 * Close cp/m file
 * If the file was opened for write, flush 
 * buffer to disk and update directory 
 */

c_close(fptr)
	register C_FILE *fptr;
{
	int	scts = 0;

#ifdef DEBUG
	printf("c_close: cnt: %d\n", fptr->c_cnt);
#endif
	if (fptr->c_flag & WRITE) {
		scts = (blksiz - fptr->c_cnt)/seclth + 1;
		if (fptr->c_cnt%seclth == 0)
			scts--;
		fptr->c_dirp->blkcnt = scts + fptr->c_seccnt;
		if (fptr->c_cnt < blksiz) {
			/*
		 	 * zero fill to end, and flush 
		 	 * the block to disk
		 	 */
			if ((fptr->c_flag&BINARY) == 0) {
				/*
			 	 * add cp/m eof mark (ctrl-z) if the file
			 	 * is not a binary file
			 	 */
				*(fptr->c_buf++) = '\032';
				fptr->c_cnt--;
			}
			while (fptr->c_cnt-- > 0)
				*(fptr->c_buf++) = '\0';
		}
		if (putblock(0xff&(int)fptr->c_dirp->pointers[fptr->c_blk],
						fptr->c_base, scts) == EOF)
			return (EOF);
	}
	savedir();

	/* deallocate buffer memory and file descriptor */
	fptr->c_flag = 0;
	free(fptr->c_base);
	return (NULL);
}
