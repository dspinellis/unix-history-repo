/*	ccreat.c	1.9	83/05/13	*/
#include <stdio.h>
#include "cpmio.h"
#include "cpmfio.h"

/*
 * Create cp/m file with the given file name and extension, return
 * file pointer. A null pointer is returned if the file could not
 * be created or if the file already exists.
 */

C_FILE *
c_creat(name, ext, flag)
	char *name, *ext;
{

	int	i, index;
	register C_FILE *fptr;
	char *malloc(), alloc();

	if (searchdir(name, ext) != -1) {
		fprintf(stderr, "file already exists: %s %s\n",name, ext);
		return(NULL);
	}
	if ((index = creext(-1)) < NULL) {
		fprintf(stderr, "c_creat: no directory space\n");
		return (NULL);
	}
#ifdef DEBUG
	printf("directory index: %d\n", index);
#endif

	/* find free slot for file descriptor */
	for ((i = 0, fptr=c_iob); i < C_NFILE; i++,fptr++) {
		if (!(fptr->c_flag)) 
			break;
	}
	if (i == C_NFILE) {
		fprintf(stderr,"c_creat: too many open files\n");
		return (NULL);
	}

	/*
	 * Free file descriptor slot found, initialize and allocate buffer
	 * memory 
	 */
	if ((fptr->c_buf=malloc(blksiz)) == NULL) {
		fprintf(stderr, "c_creat: no memory!\n");
		return (NULL);
	}
	fptr->c_dirp = dirbuf+index;
	if ((fptr->c_dirp->pointers[0] = alloc()) == '\0') {
		fprintf(stderr, "c_creat: disk full\n");
		return (NULL);
	}
	fptr->c_dirp->status = '\0';
	fptr->c_dirp->extno  = '\0';
	fptr->c_dirp->notused[0]  = '\0';
	fptr->c_dirp->notused[1]  = '\0';
	strncpy(fptr->c_dirp->name, name, 8);
	strncpy(fptr->c_dirp->ext, ext, 3);
	fptr->c_dirp->blkcnt = '\0';
	fptr->c_blk = 0;
	fptr->c_base = fptr->c_buf;
	fptr->c_flag = WRITE | flag;
	fptr->c_ext = index;
	fptr->c_seccnt = 0;
	fptr->c_cnt = blksiz;
	fptr->c_extno = 0;
	return (fptr);
}
