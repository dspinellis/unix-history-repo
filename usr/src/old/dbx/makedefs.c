/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)makedefs.c	5.1 (Berkeley) %G%";
#endif not lint

static char rcsid[] = "$Header: makedefs.c,v 1.4 84/12/26 10:40:22 linton Exp $";

/*
 * Create a definitions file (e.g. .h) from an implementation file (e.g. .c).
 *
 * Usage is "makedefs source.c source.h" where source.h is to be created.
 *
 * Lines beginning with "public" or within a "#ifndef public ... #endif"
 * block are copied to the new file.  Initializations (e.g. "int x = 3") are
 * omitted ("int x;" is output).
 *
 * Normally a temporary definitions file is created and compared to
 * the given destination.  If they are different, the temporary file
 * is copied on top of the destination.  This is so that dependencies
 * when using "make" are not triggered.
 *
 * The "-f" option overrides this and forces the destination file to be created.
 */

#include "defs.h"
#include <signal.h>

#define procedure void

Boolean force;
Boolean copytext;

String tmpname;
String modulename();
procedure abnorm();

main(argc, argv)
int argc;
String argv[];
{
    extern String mktemp();
    String name;
    File tmp;
    Integer r;
    Integer index;

    if (streq(argv[1], "-f")) {
	force = true;
	index = 2;
    } else {
	force = false;
	index = 1;
    }
    if (argc - index > 2) {
	fatal("usage: makedefs [ -f ] file.c [ file.h ]\n");
    }
    tmp = nil;
    if (freopen(argv[index], "r", stdin) == NULL) {
	fatal("can't read %s", argv[index]);
    }
    signal(SIGINT, abnorm);
    signal(SIGQUIT, abnorm);
    if (index + 1 < argc) {
	if (force) {
	    tmpname = argv[index + 1];
	} else {
	    tmpname = mktemp("/tmp/makedefsXXXXXX");
	}
	tmp = freopen(tmpname, "w", stdout);
	if (tmp == nil) {
	    fatal("can't write %s", tmpname);
	}
    }
    copytext = false;
    name = modulename(argv[index]);
    printf("#ifndef %s\n", name);
    printf("#define %s\n", name);
    copy();
    printf("#endif\n");
    if (tmp != NULL and not force) {
	fclose(tmp);
	r = call("cmp", stdin, stderr, "-s", tmpname, argv[2], nil);
	if (r != 0) {
	    r = call("cp", stdin, stderr, tmpname, argv[2], nil);
	    if (r != 0) {
		fprintf(stderr, "can't create %s\n", argv[2]);
	    }
	}
	unlink(tmpname);
    }
    quit(0);
}

String modulename(s)
String s;
{
    String r, i, j;
    static char buf[256];

    strcpy(buf, s);
    i = rindex(buf, '/');
    if (i == nil) {
	i = buf;
    }
    for (j = i; *j != '.'; j++);
    *j++ = '_';
    *j++ = 'h';
    *j = '\0';
    return buf;
}

copy()
{
    register char *p;
    char line[1024];

    while (gets(line) != NULL) {
	if (strncmp(line, "#ifndef public", 14) == 0) {
	    copytext = true;
	} else if (strncmp(line, "#endif", 6) == 0) {
	    copytext = false;
	} else if (strncmp(line, "public", 6) == 0) {
	    copydef(line);
	} else if (copytext) {
	    printf("%s\n", line);
	}
    }
}

copydef(s)
String s;
{
    register char *p;
    register Boolean isproc;

    isproc = false;
    for (p = &s[7]; *p != '\0' and *p != '='; p++) {
	if (*p == '(') {
	    isproc = true;
	    printf("(/* ");
	} else if (*p == ')' and isproc and *(p+1) == '\0') {
	    printf(" */)");
	} else {
	    putchar(*p);
	}
    }
    if (isproc or *p == '=') {
	putchar(';');
    }
    putchar('\n');
}

/*
 * Terminate program.
 */

procedure abnorm(signo)
int signo;
{
    unlink(tmpname);
    quit(signo);
}

quit(r)
int r;
{
    exit(r);
}

/*
 * No special error recovery strategy.
 */

erecover()
{
}
