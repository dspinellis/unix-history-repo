/* Copyright (c) 1982 Regents of the University of California */

static char rcsid[] = "$Header: mkdate.c,v 1.3 84/03/27 10:21:59 linton Exp $";

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
