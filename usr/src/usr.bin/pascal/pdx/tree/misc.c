/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Miscellaneous commands "edit" and "help".
 * Also, output redirection routine "setout" and "unsetout".
 */

#include "defs.h"
#include "tree.h"
#include "command.h"
#include "object.h"
#include "mappings.h"
#include "sym.h"
#include "symtab.h"

extern char *getenv();

#define DEF_EDITOR	"vi"

/*
 * Invoke an editor on the given file.  Which editor to use might change
 * installation to installation.  For now, we use "vi".  In any event,
 * the environment variable "EDITOR" overrides any default.
 */

edit(filename)
char *filename;
{
	char *ed;
	FILE *fp;
	SYM *s;
	ADDRESS addr;
	char buff[10];

	if ((ed = getenv("EDITOR")) == NIL) {
		ed = DEF_EDITOR;
	}
	fp = fopen(filename, "r");
	if (fp == NIL) {
		s = st_lookup(symtab, filename);
		if (s == NIL) {
			error("can't read \"%s\"", filename);
		}
		s = which(s);
		if (!isblock(s)) {
			error("can't read \"%s\"", filename);
		}
		addr = firstline(s);
		filename = srcfilename(addr);
		sprintf(buff, "+%d", srcline(addr));
		call(ed, stdin, stdout, buff, filename, NIL);
	} else {
		fclose(fp);
		call(ed, stdin, stdout, filename, NIL);
	}
}

/*
 * Send some nasty mail to the current pdx support person.
 */

gripe()
{
	char *maintainer = "4bsd-bugs@Berkeley.EDU";

	puts("Type control-D to end your message.  Be sure to include");
	puts("your name and the name of the file you are debugging.");
	putchar('\n');
	call("Mail", stdin, stdout, maintainer, NIL);
	puts("Thank you.");
}

/*
 * Give the user some help.
 */

help()
{
	puts("pdx command subset summary:");
	putchar('\n');
	puts("run                    - begin execution of the program");
	puts("cont                   - continue execution");
	puts("step                   - single step one line");
	puts("next                   - step to next line (skip over calls)");
	puts("trace <line#>          - trace execution of the line");
	puts("trace <proc>           - trace calls to the procedure");
	puts("trace <var>            - trace changes to the variable");
	puts("trace <exp> at <line#> - print <exp> when <line> is reached");
	puts("stop at <line>         - suspend execution at the line");
	puts("stop in <proc>         - suspend execution when <proc> is called");
	puts("status                 - print trace/stop's in effect");
	puts("delete <number>        - remove trace or stop of given number");
	puts("call <proc>            - call the procedure");
	puts("where                  - print currently active procedures");
	puts("print <exp>            - print the value of the expression");
	puts("whatis <name>          - print the declaration of the name");
	puts("list <line>, <line>    - list source lines");
	puts("edit <proc>            - edit file containing <proc>");
	puts("gripe                  - send mail to the person in charge of pdx");
	puts("quit                   - exit pdx");
}

/*
 * Divert output to the given file name.
 * Cannot redirect to an existing file.
 */

LOCAL int so_fd;
LOCAL BOOLEAN notstdout;

setout(filename)
char *filename;
{
	FILE *fp;

	if ((fp = fopen(filename, "r")) != NIL) {
		fclose(fp);
		error("%s: file already exists", filename);
	} else {
		so_fd = dup(1);
		close(1);
		if (creat(filename, 0666) == NIL) {
			unsetout();
			error("can't create %s", filename);
		}
		notstdout = TRUE;
	}
}

/*
 * Revert output to standard output.
 */

unsetout()
{
	fflush(stdout);
	close(1);
	if (dup(so_fd) != 1) {
		panic("standard out dup failed");
	}
	close(so_fd);
	notstdout = FALSE;
}

BOOLEAN isredirected()
{
	return(notstdout);
}
