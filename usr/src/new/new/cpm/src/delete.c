/*	delete.c	1.6	83/05/13	*/

#include <stdio.h>
#include "cpmio.h"
#include "cpmfio.h"

/*
 * Delete cp/m file
 */

delete(cmdline)
char *cmdline;

{

	char name[9], ext[4];
	C_FILE *cio;

	if (!(namesep(cmdline,name,ext))) return;
	if (searchdir(name, ext) != -1) {
		cio = c_open(name, ext, READ);
		cio->c_dirp->status = (char) 0xe5;
		while(cio->c_dirp->blkcnt == (char) 0x80 && getnext(cio) != 0)
			cio->c_dirp->status = (char) 0xe5;
	}
	else {
		printf("File not found: %s %s\n", name, ext);
		return;
	}
	savedir();
	c_close(cio);
	/* 
	 * rebuild the bitmap completely instead of recovering
         * each block as they are deleted
	 */
	build_bmap(); 
	if (ext[0] == ' ') 
		ext[0] = '\0';
	printf("%s %s deleted\n",name,ext);
}
