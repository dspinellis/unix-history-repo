/*	extent.c	1.7	83/05/13	*/

#include <stdio.h>
#include "cpmio.h"
#include "cpmfio.h"

/*
 * Allocate a new extent to the file pointed to by curext,
 * or, if curext < 0, return the index of the first free
 * directory slot.
 * Return a negative pointer if no directory space, otherwise
 * the index to the new extent.
 */

creext(curext)
	int curext;
{

	int i, j;

	for (i=0; i < maxdir; i++)
		if ((dirbuf + i)->status == (char) 0xe5) 
			break;
	if (i == maxdir) 
		return (EOF);
	if (curext >= 0) 
		*(dirbuf+i) = *(dirbuf+curext);
	
	/* clear all file pointers */
	for (j=0; j<16; j++)
		(dirbuf+i)->pointers[j] = '\0';

#ifdef DEBUG
	printf("extent allocated: %d (old: %d)\n", i, curext);
	printf("extent data: 0x%x, name: %s\n", (dirbuf+i)->status,
			(dirbuf+i)->name);
#endif
	return(i);
}


/*
 * Find next extent of the file pointed to by file pointer 'current',
 * return the new extent's index if found, otherwise NULL.
 */

getnext(cur)
	C_FILE	*cur;
{

	int	ind;

	cur->c_extno++;
	for (ind = 0; ind < maxdir; ind++) 
		if ((strncmp(cur->c_dirp->name,(dirbuf+ind)->name,8)==0) &&
		    (strncmp((dirbuf+ind)->ext, cur->c_dirp->ext,3)==0) &&
		    ((dirbuf+ind)->extno == cur->c_extno)) {
			cur->c_ext=ind;
			cur->c_seccnt = 0xff & (int)(dirbuf+ind)->blkcnt;
			cur->c_dirp = dirbuf + ind;
			cur->c_blk = 0;
#ifdef DEBUG
			printf("getnext: dir. index: %d\n",ind); 
#endif
			return (ind);
		}
	return (NULL);
}
