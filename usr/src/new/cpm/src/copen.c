/*	copen.c	1.8	85/03/24	*/

#include <stdio.h>
#include "cpmio.h"
#include "cpmfio.h"

/*
 * Open cp/m file with the given file name and extension, return
 * file pointer. A null pointer is returned if the file could not
 * be opened. mode tells whether the file is to be read, written
 * or both.
 */

C_FILE *
c_open(name, ext, mode)
	char *name, *ext;
{

	register int	i, index, scnt;
	register C_FILE *fptr;
	char *malloc(), *fixname();

	if ((index = searchdir(name, ext)) == -1) {
		fprintf(stderr, "file not found: %s\n", fixname(name, ext));
		return (NULL);
	}
	/* test for legal mode */
	if (!(mode & RW)) {
		fprintf(stderr, "open: illegal mode - %d\n", mode);
		return (NULL);
	}
#ifdef DEBUG
	printf("directory index: %d\n", index); 
#endif
	for ((i = 0, fptr=c_iob); i < C_NFILE; i++,fptr++)
		if (!(fptr->c_flag)) 
			break;
	if (i == C_NFILE) {
		fprintf(stderr, "too many open files\n");
		return (NULL);
	}

/*
 * Free file descriptor slot found, initialize field, allocate
 * memory and read first block.
 */
	if ((fptr->c_buf = malloc(blksiz)) == NULL) {
		printf("c_open: no memory!\n");
		return (NULL);
	}
	fptr->c_extno = 0;
	fptr->c_base = fptr->c_buf;
	fptr->c_flag = mode;
	fptr->c_blk = 0;
	fptr->c_ext = index;
	fptr->c_dirp = dirbuf+index;
	fptr->c_seccnt = 0xff &(dirbuf+index)->blkcnt;
	scnt = (fptr->c_seccnt > blksiz/seclth) ? blksiz/seclth:fptr->c_seccnt;
#ifdef DEBUG
	printf("c_open: scnt=%d\n",scnt); 
#endif
	if (getblock(0xff &(int)fptr->c_dirp->pointers[0], fptr->c_buf, scnt)
								== EOF)
		return (NULL);
	fptr->c_cnt = seclth*scnt;
	fptr->c_seccnt -= scnt;
	return (fptr);
}
