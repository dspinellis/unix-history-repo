/*	dirhdl.c	1.7	83/05/13	*/

#include <stdio.h>
#include "cpmio.h"

/* Display cp/m directory on stdout */

dispdir()
{

	int cnt;
	int filecnt = 0;
	int blkcnt ;

	for (cnt=0; cnt<maxdir; cnt++) {
		if ((dirbuf+cnt)->status != (char) 0xe5) {
		   if ((dirbuf+cnt)->extno == '\0' ) {
			printf("%.8s %.3s",(dirbuf+cnt)->name,
				(dirbuf+cnt)->ext);
			if (++filecnt%4 == 0) 
				printf("\n");
			else
				printf("   :   ");
		   }
		}
	}
	blkcnt = blks_used();
	if (filecnt%4 > 0) printf("\n");
	if (filecnt == 0) 
		printf("No files\n");
	else	
		printf(
		"Total of %d files. %d blocks used, %d blocks free.\n"
		,filecnt, blkcnt,seclth*sectrk*(tracks-2)/blksiz - blkcnt);
	return;
}

getdir()

{

	int bl, blks;
	int offset = 0;

	blks = maxdir*32/blksiz;
	if ((maxdir*32)%blksiz > 0) 
		++blks;
	for (bl = 0; blks > 0; bl++, blks--) {
		if (getblock(bl,dirbuf+offset,-1) == EOF) {
			fprintf(stderr, "getdir: fatal error\n");
			exit (0);
		}
		offset += blksiz/32;
	}
}


savedir()

{

	int bl, blks;
	int offset = 0;

	blks = maxdir*32/blksiz;
	if ((maxdir*32)%blksiz > 0) 
		++blks;
	for (bl = 0; blks > 0; bl++, blks--) {
		if (putblock(bl,dirbuf+offset,-1) == EOF) {
			fprintf(stderr, "savedir: fatal error\n");
			exit (0);
		}
		offset += blksiz/32;
	}
}

/* Search the cp/m directory for the file given by the input
 * parameters, return -1 if not found,
 * directory index to the file's first extent is
 * returned if found.
 */

searchdir(name,ext)
char *name, *ext;

{
	int ind;

	for (ind = 0; ind < maxdir; ++ind) 
	{
		if ((dirbuf+ind)->status == (char) 0xe5) continue;
		if ((strncmp(name,(dirbuf+ind)->name,8) == 0) &&
		    (strncmp(ext, (dirbuf+ind)->ext, 3) == 0) &&
		    ((dirbuf+ind)->extno == '\0')) return(ind);
	}
	return(-1);
}
