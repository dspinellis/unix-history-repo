/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkdate.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#ifdef IRIS
#   include <time.h>
#else
#   include <sys/time.h>
#endif

main()
{
    struct tm *t;
    long clock;
    char name[100];
    int namelen;

    printf("char *date = \"");
    clock = time(0);
    t = localtime(&clock);
    printf("%d/%d/%d ", t->tm_mon + 1, t->tm_mday, t->tm_year % 100);
    printf("%d:%02d", t->tm_hour, t->tm_min);
#   ifndef IRIS
	gethostname(name, &namelen);
	printf(" (%s)", name);
#   endif
    printf("\";\n");
    exit(0);
}
