/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

static char sccsid[] = "@(#)mkdate.c 5.1 %G%";

static char rcsid[] = "$Header: mkdate.c,v 1.5 84/12/26 10:40:30 linton Exp $";

#include <stdio.h>
#include <sys/time.h>

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
    gethostname(name, &namelen);
    printf(" (%s)", name);
    printf("\";\n");
}
