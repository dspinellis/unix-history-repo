/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)makedefs.c	5.6 (Berkeley) 6/1/90";
#endif /* not lint */

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
#include "pathnames.h"

#define procedure void

#define streqn(s1, s2, n) (strncmp(s1, s2, n) == 0)

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
	    tmpname = mktemp(_PATH_TMP);
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
    } else {
	++i;
    }
    for (j = i; *j; j++) {
	if (*j == '.') {
	    *j = '_';
	}
    }
    if (j > i && *--j == 'c') {
	*j = 'h';
    }
    return i;
}

copy()
{
    register char *p;
    integer nesting;
    char line[1024];

    while (gets(line) != NULL) {
	if (streqn(line, "#ifndef public", 14)) {
	    copytext = true;
	    nesting = 1;
	} else if (streqn(line, "public", 6)) {
	    copydef(line);
	} else if (copytext) {
	    if (streqn(line, "#ifdef", 6) or streqn(line, "#ifndef", 7)) {
		++nesting;
		printf("%s\n", line);
	    } else if (streqn(line, "#endif", 6)) {
		--nesting;
		if (nesting <= 0) {
		    copytext = false;
		} else {
		    printf("%s\n", line);
		}
	    } else {
		printf("%s\n", line);
	    }
	} else if (
	    streqn(line, "#ifdef", 6) or
	    streqn(line, "#ifndef", 7) or
	    streqn(line, "#else", 5) or
	    streqn(line, "#endif", 6)
	) {
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
