/*  Author: Mark Moraes <moraes@csri.toronto.edu> */

/*LINTLIBRARY*/

#include "defs.h"
#include "globals.h"
#include "trace.h"

RCSID("$Id: _memalign.c,v 1.8 1993/05/23 03:38:27 moraes Exp $")

univptr_t
__memalign(alignment, size, fname, linenum)
size_t alignment, size;
const char *fname;
int linenum;
{
	univptr_t cp;
	
	PRTRACE(sprintf(_malloc_statsbuf, "%s:%d:", fname, linenum));
	cp = memalign(alignment, size);
	RECORD_FILE_AND_LINE(cp, fname, linenum);
	return(cp);
}

univptr_t
__valloc(size, fname, linenum)
size_t size;
const char *fname;
int linenum;
{
	univptr_t cp;
	
	PRTRACE(sprintf(_malloc_statsbuf, "%s:%d:", fname, linenum));
	cp = valloc(size);
	RECORD_FILE_AND_LINE(cp, fname, linenum);
	return(cp);
}
