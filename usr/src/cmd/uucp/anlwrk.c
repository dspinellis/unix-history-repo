	/*  anlwrk 2.1  5/22/79  09:59:09  */
#include "uucp.h"
#include <sys/types.h>
#include <sys/stat.h>

static char SiD[] = "@(#)anlwrk	2.1";


#define LLEN 20
#define SAME 0

/*******
 *	anlwrk(file, wvec)	create a vector of command arguments
 *	char *file, **wvec;
 *
 *	return codes:
 *		0  -  no more work in this file
 *		positive number  -  number of arguments
 */

anlwrk(file, wvec)
char *file, **wvec;
{
	static char str[BUFSIZ];
	static FILE *fp = NULL;

	if (file[0] == '\0')
		return(0);
	if (fp == NULL) {
		fp = fopen(file, "r");
		if (fp == NULL)
			return(0);
	}

	if (fgets(str, BUFSIZ, fp) == NULL) {
		fclose(fp);
		unlink(file);
		file[0] = '\0';
		fp = NULL;
		return(0);
	}

	return(getargs(str, wvec));
}


/***
 *	iswrk(file, reqst, dir, pre)
 *	char *file, *reqst, *dir, *pre;
 *
 *	iswrk  -  this routine will check the work list (list).
 *	If it is empty or the present work is exhausted, it
 *	will call gtwrk to generate a new list.
 *	The "reqst" field will be the string "chk" or "get" to
 *	check for work, or get the next work file respectively.
 *
 *	return codes:
 *		0  -  no more work (or some error)
 *		1  -  there is work
 */

iswrk(file, reqst, dir, pre)
char *file, *reqst, *dir, *pre;
{
	static char **listp, *list[LLEN];

	if (listp == NULL || *listp == NULL || listp > (list + LLEN)
	  || !prefix(pre, *listp)) {
		int i;
		for (i = 0, listp = list; i < LLEN; i++) {
			if (*listp != NULL)
				free(*listp);
			*listp++ = NULL;
		}
		if (gtwrk(dir, pre, listp = list, LLEN) != 0)
			/* alloc error */
			return(0);
	}

	if (*listp == NULL)
		return(0);

	if (strcmp(reqst, "get") == SAME)
		sprintf(file, "%s/%s", dir, *listp++);
	return(1);
}


/***
 *	gtwvec(file, dir, wkpre, wrkvec)	get work vector 
 *	char *file, *dir, *wkpre, **wrkvec;
 *
 *	return codes:
 *		positive number  -  number of arguments
 *		0 -  no arguments - fail
 */

gtwvec(file, dir, wkpre, wrkvec)
char *file, *dir, *wkpre, **wrkvec;
{
	int nargs;

	while ((nargs = anlwrk(file, wrkvec)) == 0) {
		if (!iswrk(file, "get", dir, wkpre))
			return(0);
	}
	return(nargs);
}


/***
 *	gtwrk(dir, pre, list, llen)
 *	char *dir, *pre, **list;
 *	int llen;
 *
 *	gtwrk  -  this routine will build a sorted list
 *	of files in a directory.
 *	"dir" is the directory name to search for file names
 *	beginning with the prefix (pre).
 *	"list" is the pointer to the list and "llen" is the
 *	length of the list.
 *
 *	return codes:  0  |  FAIL
 */

gtwrk(dir, pre, list, llen)
char *dir, *pre, **list;
int llen;
{
	struct stat s;
	char filename[NAMESIZE], *p;
	char **first, **last;
	FILE *pdir;
	extern int compar();
	extern char *calloc();

	first = last = list;
	if ((pdir = fopen(dir, "r")) == NULL)
		return(FAIL);
	while (gnamef(pdir, filename) && (last - first) < llen) {
		if (!prefix(pre, filename))
			continue;
		if (stat(filename, &s) == -1)
			continue;
		if ((s.st_mode & ANYREAD) == 0)
			continue;
		if ((p = calloc(strlen(filename) + 1, sizeof (char))) == NULL)
			return(FAIL);

		strcpy(p, filename);
		*last++ = p;
	}

	fclose(pdir);
	qsort(first, last - first, sizeof *last, compar);
	return(0);
}


/***
 *	compar(p1, p2)
 *	char **p1, **p2;
 *
 *	compar  -  this routine is used by qsort.
 *
 */

compar(p1, p2)
char **p1, **p2;
{
	return(strcmp(*p1, *p2));
}
