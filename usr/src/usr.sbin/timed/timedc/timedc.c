/*-
 * Copyright (c) 1985, 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1985, 1993 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)timedc.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#ifdef sgi
#ident "$Revision: 1.6 $"
#endif

#include "timedc.h"
#include <strings.h>
#include <signal.h>
#include <ctype.h>
#include <setjmp.h>
#include <unistd.h>
#include <stdlib.h>
#include <syslog.h>

int trace = 0;
FILE *fd = 0;
int	margc;
int	fromatty;
char	*margv[20];
char	cmdline[200];
jmp_buf	toplevel;
static struct cmd *getcmd __P((char *));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register struct cmd *c;

	openlog("timedc", LOG_ODELAY, LOG_AUTH);

	/*
	 * security dictates!
	 */
	if (priv_resources() < 0) {
		fprintf(stderr, "Could not get privileged resources\n");
		exit(1);
	}
	(void) setuid(getuid());

	if (--argc > 0) {
		c = getcmd(*++argv);
		if (c == (struct cmd *)-1) {
			printf("?Ambiguous command\n");
			exit(1);
		}
		if (c == 0) {
			printf("?Invalid command\n");
			exit(1);
		}
		if (c->c_priv && getuid()) {
			printf("?Privileged command\n");
			exit(1);
		}
		(*c->c_handler)(argc, argv);
		exit(0);
	}

	fromatty = isatty(fileno(stdin));
	if (setjmp(toplevel))
		putchar('\n');
	(void) signal(SIGINT, intr);
	for (;;) {
		if (fromatty) {
			printf("timedc> ");
			(void) fflush(stdout);
		}
		if (fgets(cmdline, sizeof(cmdline), stdin) == 0)
			quit();
		if (cmdline[0] == 0)
			break;
		makeargv();
		if (margv[0] == 0)
			continue;
		c = getcmd(margv[0]);
		if (c == (struct cmd *)-1) {
			printf("?Ambiguous command\n");
			continue;
		}
		if (c == 0) {
			printf("?Invalid command\n");
			continue;
		}
		if (c->c_priv && getuid()) {
			printf("?Privileged command\n");
			continue;
		}
		(*c->c_handler)(margc, margv);
	}
	return 0;
}

void
intr(signo)
	int signo;
{
	if (!fromatty)
		exit(0);
	longjmp(toplevel, 1);
}


static struct cmd *
getcmd(name)
	char *name;
{
	register char *p, *q;
	register struct cmd *c, *found;
	register int nmatches, longest;
	extern int NCMDS;

	longest = 0;
	nmatches = 0;
	found = 0;
	for (c = cmdtab; c < &cmdtab[NCMDS]; c++) {
		p = c->c_name;
		for (q = name; *q == *p++; q++)
			if (*q == 0)		/* exact match? */
				return(c);
		if (!*q) {			/* the name was a prefix */
			if (q - name > longest) {
				longest = q - name;
				nmatches = 1;
				found = c;
			} else if (q - name == longest)
				nmatches++;
		}
	}
	if (nmatches > 1)
		return((struct cmd *)-1);
	return(found);
}

/*
 * Slice a string up into argc/argv.
 */
void
makeargv()
{
	register char *cp;
	register char **argp = margv;

	margc = 0;
	for (cp = cmdline; *cp;) {
		while (isspace(*cp))
			cp++;
		if (*cp == '\0')
			break;
		*argp++ = cp;
		margc += 1;
		while (*cp != '\0' && !isspace(*cp))
			cp++;
		if (*cp == '\0')
			break;
		*cp++ = '\0';
	}
	*argp++ = 0;
}

#define HELPINDENT (sizeof ("directory"))

/*
 * Help command.
 */
void
help(argc, argv)
	int argc;
	char *argv[];
{
	register struct cmd *c;

	if (argc == 1) {
		register int i, j, w;
		int columns, width = 0, lines;
		extern int NCMDS;

		printf("Commands may be abbreviated.  Commands are:\n\n");
		for (c = cmdtab; c < &cmdtab[NCMDS]; c++) {
			int len = strlen(c->c_name);

			if (len > width)
				width = len;
		}
		width = (width + 8) &~ 7;
		columns = 80 / width;
		if (columns == 0)
			columns = 1;
		lines = (NCMDS + columns - 1) / columns;
		for (i = 0; i < lines; i++) {
			for (j = 0; j < columns; j++) {
				c = cmdtab + j * lines + i;
				printf("%s", c->c_name);
				if (c + lines >= &cmdtab[NCMDS]) {
					printf("\n");
					break;
				}
				w = strlen(c->c_name);
				while (w < width) {
					w = (w + 8) &~ 7;
					putchar('\t');
				}
			}
		}
		return;
	}
	while (--argc > 0) {
		register char *arg;
		arg = *++argv;
		c = getcmd(arg);
		if (c == (struct cmd *)-1)
			printf("?Ambiguous help command %s\n", arg);
		else if (c == (struct cmd *)0)
			printf("?Invalid help command %s\n", arg);
		else
			printf("%-*s\t%s\n", (int)HELPINDENT,
				c->c_name, c->c_help);
	}
}
