#include <stdio.h>

subout(varn, offset, procn, line)
char *varn, *procn;
long int offset;
int line;
{
register int i;

fprintf(stderr, "Subscript out of range on line %d of procedure ", line);
for(i = 0 ; i < 8 && *procn!='_' ; ++i)
	putc(*procn++, stderr);
fprintf(stderr, ".\nAttempt to access the %ld-th element of variable ", offset+1);
for(i = 0 ; i < 6  && *varn!=' ' ; ++i)
	putc(*varn++, stderr);
fprintf(stderr, ".\n");
_cleanup();
abort();
}
