/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)whoami.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <pwd.h>

main()
{
	struct passwd *p;
	uid_t uid;

	uid = geteuid();
	if (!(p = getpwuid(uid))) {
		printf("whoami: no login associated with uid %u.\n", uid);
		exit(1);
	}
	printf("%s\n", p->pw_name);
	exit(0);
}
