/*	rename.c	1.8	83/05/13	*/

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
	char *secarg, *index();


	if ((secarg = index(cmdline, ' ')) == NULL) {
		printf("rename: too few arguments\n");
		return (1);
	}
	*secarg++ = '\0';
	if (!(namesep(cmdline, oldname, oldext))) 
		return (1);
	if (!(namesep(secarg, newname, newext))) 
		return (1);

	if (searchdir(oldname, oldext) != -1) {
		cio = c_open(oldname, oldext, READ);
		do {
			strncpy(cio->c_dirp->name, newname, 8);
			strncpy(cio->c_dirp->ext, newext, 3);
		} while (getnext(cio) != NULL);
	} else {
		fnfound(oldname, oldext);
		return (1);
	}
	if (newext[0] == ' ') 
		newext[0] = '\0';
	if (oldext[0] == ' ') 
		oldext[0] = '\0';
	printf("renamed %s %s to %s %s\n", oldname, oldext, newname, newext);
	savedir();
	c_close(cio);
	return (0);
}
