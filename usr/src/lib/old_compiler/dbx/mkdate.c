/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)mkdate.c	5.3 (Berkeley) 1/12/88";
#endif not lint

static char rcsid[] = "$Header: mkdate.c,v 1.2 87/03/26 19:56:22 donn Exp $";

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
