/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)mkdate.c	5.2 (Berkeley) %G%";
#endif not lint

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
    DoVersionNumber();
    exit(0);
}

DoVersionNumber()
{
    FILE *f;
    int n;

    f = fopen("version", "r");
    if (f == NULL) {
	n = 1;
    } else {
	fscanf(f, "%d", &n);
	n = n + 1;
	fclose(f);
    }
    f = fopen("version", "w");
    if (f != NULL) {
	fprintf(f, "%d\n", n);
	fclose(f);
    }
    printf("int versionNumber = %d;\n", n);
}
