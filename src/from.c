/* Copyright (c) 1979 Regents of the University of California */
#include <retrofit.h>
#include <stdio.h>
#include <pwd.h>

struct	passwd *getpwuid();

main()
{
	char lbuf[BUFSIZ];
	register struct passwd *pp;

	if (chdir("/usr/spool/mail") < 0)
		exit(1);
	pp = getpwuid(getuid());
	if (pp == 0) {
		fprintf(stderr, "Who are you?\n");
		exit(1);
	}
	if (freopen(pp->pw_name, "r", stdin) == NULL)
		exit(0);
	while(fgets(lbuf, sizeof lbuf, stdin) != NULL)
		if (lbuf[0] == 'F' && lbuf[1] == 'r' && lbuf[2] == 'o' && lbuf[3] == 'm')
			printf("%s", lbuf);
	exit(0);
}
