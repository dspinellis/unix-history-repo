#include "mh.h"

char *concat(args)
char *args;
{
	register char **a;
	register char *cp;
	register int  len;
	register char *ret;

	len = 1;
	for(a = &args; *a; )
		len += strlen(*a++);
	ret = cp = (char *) malloc(len);
	for(a = &args; *a; )
		cp = copy(*a++, cp);
	return(ret);
}
