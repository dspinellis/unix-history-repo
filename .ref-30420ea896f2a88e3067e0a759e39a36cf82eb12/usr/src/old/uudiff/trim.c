#ifndef lint
static char sccsid[] = "@(#)trim.c	4.1 (Berkeley) %G%";
#endif

# if interdata
# include "stdio.h"
# endif
main(argc,argv)
	char *argv[];
{
char *nm, *p;
p = nm = argv[1];
while (*p) p++;
while (*p != '/') p--;
if (p==nm) p++;
*p = 0;
printf("%s\n", nm);
exit(0);
}
