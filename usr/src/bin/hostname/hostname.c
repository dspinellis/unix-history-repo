static char *sccsid = "@(#)hostname.c	1.1 1.1 82/04/08"; 
/*
 * hostname -- get (or set hostname)
 */
#include <stdio.h>

char hostname[32];
extern int errno;

main(argc,argv)
char *argv[];
{
	argc--;
	argv++;
	if(argc) {
		if(sethostname(*argv,strlen(*argv)+1))
			perror("sethostname");
	} else {
		gethostname(hostname,sizeof(hostname));
		printf("%s\n",hostname);
	}
	return(errno);
}
