/* tmpnam.c : return a temporary file name */
/* written by Eric R. Smith and placed in the public domain */
/**
 *  - modified for gawk needs - pattern /$$XXXXXX from the original
 *    code creates names which are hard to remove when somethig
 *    goes wrong
 *  - retuned name can be passed outside via system(); other programs
 *    may not dig '/' as a path separator
 *  - somehow more frugal in a memory use
 *    (mj - October 1990)
 **/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern char *	getenv(const char *);
extern char *	mktemp(char *);
char *          tempnam(const char *path, const char *base);
static char pattern[] = "\\gwkXXXXX";

char *tmpnam(buf)
	char *buf;
{
	char *tmpdir;

	if (!(tmpdir = getenv("TEMP")) && !(tmpdir = getenv("TMPDIR")))
		tmpdir = ".";

	if (!buf) {
		size_t blen;
		
		blen = strlen (tmpdir) + sizeof(pattern);
		if (NULL == (buf = malloc(blen)))
			return NULL;
	}
	(void) strcat(strcpy(buf, tmpdir), pattern);
	return(mktemp(buf));
}

/* used by gawk_popen() */
char *tempnam(path, base)
const char *path, *base;	/* ignored */
{
	return tmpnam(NULL);
}
