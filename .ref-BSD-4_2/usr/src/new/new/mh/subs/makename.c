#include "mh.h"
#include <stdio.h>

char *makename(prefix,suffix)
char *prefix, *suffix;
{
	static char tmpname[15];
	register char *cp1, *cp2;
	register int pid;

	pid = getpid();
	cp1 = tmpname;
	for (cp2 = prefix; *cp1++ = *cp2++; );
	cp1--;
	do *cp1++ = pid%10 + '0'; while (pid /= 10);
	for (cp2 = suffix; *cp1++ = *cp2++; );
	if (cp1 >= &tmpname[15]) error("strs too long to makename");
	return (tmpname);
}
