/*	delete.c	1.7	85/03/24	*/

#include <stdio.h>
#include "cpmio.h"
#include "cpmfio.h"

/*
 * Delete cp/m file
 */

delete(cmdline)
	char *cmdline;
{

	char name[9], ext[4], *fixname();
	register C_FILE *cio;

	if (!(namesep(cmdline, name, ext))) 
		return;
	if (searchdir(name, ext) == -1) {
		fprintf(stderr, "File not found: %s\n", fixname(name, ext));
		return;
	}
	cio = c_open(name, ext, READ);
	cio->c_dirp->status = (char) 0xe5;
	while(cio->c_dirp->blkcnt == (char) 0x80 && getnext(cio) != 0)
		cio->c_dirp->status = (char) 0xe5;
	savedir();
	c_close(cio);
	/* 
	 * rebuild the bitmap completely instead of recovering
         * each block as they are deleted
	 */
	build_bmap(); 
	fprintf(stderr, "%s deleted\n", fixname(name, ext));
}
