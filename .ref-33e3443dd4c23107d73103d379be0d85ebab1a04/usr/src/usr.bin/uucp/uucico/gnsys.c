#ifndef lint
static char sccsid[] = "@(#)gnsys.c	5.4 (Berkeley) %G%";
#endif

#include "uucp.h"
#ifdef	NDIR
#include "ndir.h"
#else
#include <sys/dir.h>
#endif

#define LSIZE 128	/* number of systems to store */
#define WSUFSIZE 6	/* work file name suffix size */

/*LINTLIBRARY*/

/*
 *	this routine will return the next system name which has work to be done.
 *	"sname" is a string of size DIRSIZ - WSUFSIZE.
 *	"pre" is the prefix for work files.
 *	"dir" is the directory to search.
 *
 *	return codes:
 *		1  -  name returned in sname
 *		SUCCESS  -  no more names
 *		FAIL  -  bad directory
 */

gnsys(sname, dir, pre)
char *sname, *dir, pre;
{
	register char *s, *p1, *p2;
	char px[3];
	static char *list[LSIZE];
	static int nitem=0, n=0, base=0;
	char systname[NAMESIZE], filename[NAMESIZE];
	DIR *dirp;

retry:
	px[0] = pre;
	px[1] = '.';
	px[2] = '\0';
	if (nitem == base) {
		/* get list of systems with work */
		int i;
		dirp = opendir(subdir(dir,pre));
		ASSERT(dirp != NULL, "BAD DIRECTORY", dir, 0);
		for (i = base; i < LSIZE; i++)
			list[i] = NULL;
		while (gnamef(dirp, filename) != 0) {
			if (!prefix(px, filename))
				continue;
			p2 = filename + strlen(filename)
				- WSUFSIZE;
			p1 = filename + strlen(px);
			for(s = systname; p1 <= p2; p1++)
				*s++ = *p1;
			*s = '\0';
			if (systname[0] == '\0')
				continue;
			nitem = srchst(systname, list, nitem);
			if (LSIZE <= nitem) break;
		}

		closedir(dirp);
	}

	if (nitem == base) {
		for (n = 0; n < nitem; n++)
			if (list[n] != NULL)
				free(list[n]);
		return SUCCESS;
	}
	while(nitem > n) {
		/* We only have at most a SYSNSIZE character site name encoded
		 * in the file. However, we would like to use the full sitename
		 * if possible. If the number of chars in list[n] is < SYSNSIZE
		 * then the sitename could not have been truncated and
		 * we don't bother to check. Otherwise, we scan SYSFILE
		 * looking for the fullname and return it if we find it
		 */
		strcpy(sname, list[n++]);
		if (strlen(sname) >= SYSNSIZE) {
			register FILE *fp;
			register char *p;
			char line[MAXFULLNAME];
			fp = fopen(SYSFILE, "r");
			ASSERT(fp != NULL, CANTOPEN, SYSFILE, 0);
			while (cfgets(line, sizeof(line), fp) != NULL) {
				p = index(line, ' ');
				if (p)
					*p = '\0';
				if (strncmp(sname, line, SYSNSIZE) == SAME) {
					strncpy(sname, line, MAXBASENAME);
					break;
				}
			}
			fclose(fp);
		}
		if (callok(sname) == 0)
			return 1;
	}
	base = n = nitem;
	goto retry;
}

/*
 *	this routine will do a linear search of list (list) to find name (name).
 *	If the name is not found, it is added to the list.
 *	The number of items in the list (n) is returned (incremented if a
 *	name is added).
 *
 *	return codes:
 *		n - the number of items in the list
 */

srchst(name, list, n)
char *name;
register char **list;
int n;
{
	register int i;
	register char *p;

	for (i = 0; i < n; i++)
		if (strcmp(name, list[i]) == 0)
			break;
	if (i >= n) {
		if ((p = calloc((unsigned)strlen(name) + 1, sizeof (char)))
			== NULL)
			return n;
		strcpy(p, name);
		list[n++] = p;
	}
	return n;
}
