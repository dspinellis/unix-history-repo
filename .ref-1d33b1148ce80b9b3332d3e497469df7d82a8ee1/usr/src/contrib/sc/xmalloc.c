/*
 * A safer saner malloc, for careless programmers
 * $Revision: 6.8 $
 */

#include <stdio.h>
#include <curses.h>
#include "sc.h"

extern char *malloc();

#ifdef SYSV3
extern void free();
extern void exit();
#endif

char *
xmalloc(n)
unsigned n;
{
register char *ptr;

if ((ptr = malloc(n + sizeof(double))) == NULL)
    fatal("xmalloc: no memory");
*((int *) ptr) = 12345;		/* magic number */
return(ptr + sizeof(double));
}

xfree(p)
char *p;
{
if (p == NULL)
    fatal("xfree: NULL");
p -= sizeof(double);
if (*((int *) p) != 12345)
    fatal("xfree: storage not malloc'ed");
free(p);
}

fatal(str)
char *str;
{
    deraw();
    (void) fprintf(stderr,"%s\n", str);
    exit(1);
}
