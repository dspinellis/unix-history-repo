/*	rename.c	1.10	85/03/24	*/

#include <stdio.h>
#include "cpmio.h"
#include "cpmfio.h"

/*
 * Rename a cp/m file
 * returns: 1 = failure, 0 = success 
 */

rename(cmdline)
	char *cmdline;

{

	C_FILE *cio;
	char oldname[9], oldext[4], newname[9], newext[4];
	char *secarg, *index(), *fixname();


	if ((secarg = index(cmdline, ' ')) == NULL) {
		printf("rename: too few arguments\n");
		return (1);
	}
	*secarg++ = '\0';
	if (!(namesep(cmdline, oldname, oldext))) 
		return (1);
	if (!(namesep(secarg, newname, newext))) 
		return (1);

	if (searchdir(oldname, oldext) == -1) {
		fprintf(stderr, "file not found: %s\n", 
			fixname(oldname, oldext));
		return (1);
	}
	if ((strncmp(oldname, newname, 8) == 0) && (strncmp(oldext, newext, 3)
		== 0)) {
		fprintf(stderr, "%s and %s are identical\n", 
			fixname(oldname, oldext), fixname(newname, newext));
		return (1);
	}
	cio = c_open(oldname, oldext, READ);
	do {
		strncpy(cio->c_dirp->name, newname, 8);
		strncpy(cio->c_dirp->ext, newext, 3);
	} while (getnext(cio) != NULL);
	savedir();
	c_close(cio);
	/* two calls to fprintf here because fixname is brain damaged */
	fprintf(stderr, "renamed %s to ", fixname(oldname, oldext)); 
	fprintf(stderr, "%s\n", fixname(newname, newext));
	return (0);
}
