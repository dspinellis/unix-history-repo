#include "uucp.h"
#include <sys/types.h>
#include <sys/dir.h>


/*******
 *	gnamef(p, filename)	get next file name from directory
 *	FILE *p;
 *	char *filename;
 *
 *	return codes:
 *		0  -  end of directory read
 *		1  -  returned name
 */


gnamef(p, filename)
FILE *p;
char *filename;
{
	static struct direct dentry;
	int i;
	char *s;

	while (1) {
		if (fread(&dentry,  sizeof(dentry), 1, p) != 1)
			return(0);
		if (dentry.d_ino != 0)
			break;
	}

	for (i = 0, s = dentry.d_name; i < DIRSIZ; i++)
		if ((filename[i] = *s++) == '\0')
			break;
	filename[DIRSIZ] = '\0';
	return(1);
}

