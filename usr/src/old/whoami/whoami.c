/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)whoami.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <pwd.h>

main()
{
	struct passwd *p, *getpwuid();
	uid_t uid;

	uid = geteuid();
	if (!(p = getpwuid(uid))) {
		printf("whoami: no login associated with uid %u.\n", uid);
		exit(1);
	}
	printf("%s\n", p->pw_name);
	exit(0);
}
