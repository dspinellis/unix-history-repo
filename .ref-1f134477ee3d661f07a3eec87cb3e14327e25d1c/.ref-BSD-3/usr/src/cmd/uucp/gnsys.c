#include "uucp.h"


#define LSIZE 30	/* number of systems to store */
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
	char *s, *p1, *p2;
	char px[3];
	static char *list[LSIZE];
	static int nitem=0, n=0;
	char sysname[NAMESIZE], filename[NAMESIZE];
	FILE *fp;

	px[0] = pre;
	px[1] = '.';
	px[2] = '\0';
	if (nitem == 0) {
		/* get list of systems with work */
		int i;
		fp = fopen(dir, "r");
		ASSERT(fp != NULL, "BAD DIRECTRY %s\n", dir);
		for (i = 0; i < LSIZE; i++)
			list[i] = NULL;
		while (gnamef(fp, filename) != 0) {
			if (!prefix(px, filename))
				continue;
			p2 = filename + strlen(filename)
				- WSUFSIZE;
			p1 = filename + strlen(px);
			for(s = sysname; p1 <= p2; p1++)
				*s++ = *p1;
			*s = '\0';
			if (sysname[0] == '\0')
				continue;
			nitem = srchst(sysname, list, nitem);
			if (LSIZE <= nitem) break;
		}

		fclose(fp);
	}

	if (nitem == 0)
		return(0);
	while(nitem > n) {
		strcpy(sname, list[n++]);
		if (callok(sname) == 0)
			return(1);
	}
	for (n = 0; n < nitem; n++)
		if (list[n] != NULL)
			free(list[n]);
	nitem = n = 0;
	return(0);
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
char *name, **list;
int n;
{
	int i;
	char *p;
	extern char *calloc();

	for (i = 0; i < n; i++)
		if (strcmp(name, list[i]) == 0)
			break;
	if (i >= n) {
		if ((p = calloc(strlen(name) + 1, sizeof (char)))
			== NULL)
			return(n);
		strcpy(p, name);
		list[n++] = p;
	}
	return(n);
}
