#ifndef lint
static char sccsid[] = "@(#)gnsys.c	5.2 (Berkeley) %G%";
#endif

/*
 * Mods:
 * The "retry" code below prevents uucico from calling
 * a site which it has called earlier.
 * Also, uucico does callok() only once for each system.
 * Done by unc!smb
 */

#include "uucp.h"
#include <sys/types.h>
#ifdef	NDIR
#include "ndir.h"
#else
#include <sys/dir.h>
#endif


#define LSIZE 100	/* number of systems to store */
#define WSUFSIZE 6	/* work file name suffix size */

/*******
 *	gnsys(sname, dir, pre)
 *	char *sname, *dir, pre;
 *
 *	gnsys  -  this routine will return the next
 *	system name which has work to be done.
 *	"pre" is the prefix for work files.
 *	"dir" is the directory to search.
 *	"sname" is a string of size DIRSIZ - WSUFSIZE.
 *
 *	return codes:
 *		0  -  no more names
 *		1  -  name returned in sname
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
		dirp = opendir(subdir(dir,pre), "r");
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
		return(0);
	}
	while(nitem > n) {
		strcpy(sname, list[n++]);
		if (callok(sname) == 0)
			return(1);
	}
	base = n = nitem;
	goto retry;
}

/***
 *	srchst(name, list, n)
 *	char *name, **list;
 *	int n;
 *
 *	srchst  -  this routine will do a linear search
 *	of list (list) to find name (name).
 *	If the name is not found, it is added to the
 *	list.
 *	The number of items in the list (n) is
 *	returned (incremented if a name is added).
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
			return(n);
		strcpy(p, name);
		list[n++] = p;
	}
	return(n);
}
