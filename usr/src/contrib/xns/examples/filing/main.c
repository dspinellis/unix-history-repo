#ifndef lint
static char RCSid[] = "$Header: main.c,v 2.1 86/12/11 06:12:14 jqj Exp $";
#endif

/*
 * FTP User Program -- Command Interface.
 */
/* $Log:	main.c,v $
 * Revision 2.1  86/12/11  06:12:14  jqj
 * Eliminated form, mode, and struct commands.  Started adding support for
 * more file types.
 * 
 * Revision 2.0  85/11/21  07:22:49  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/05/27  06:31:05  jqj
 * Initial revision
 * 
 * Revision 1.1  85/05/27  06:31:05  jqj
 * Initial revision
 * 
 * Based on Berkeley tcp/ftp
 */
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

#include <xnscourier/except.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <pwd.h>

#include "ftp_var.h"

int	intr();
int	lostpeer();
extern	char *home;

main(argc, argv)
	char *argv[];
{
	register char *cp;
	int top;
	struct passwd *pw;
	char homedir[MAXPATHLEN];

	doglob = 1;
	interactive = 1;
	autologin = 1;
	argc--, argv++;
	while (argc > 0 && **argv == '-') {
		for (cp = *argv + 1; *cp; cp++)
			switch (*cp) {

			case 'd':
				debug++;
				break;
			
			case 'v':
				verbose++;
				break;

			case 't':
				trace++;
				break;

			case 'i':
				interactive = 0;
				break;

			case 'n':
				autologin = 0;
				break;

			case 'g':
				doglob = 0;
				break;

			default:
				fprintf(stderr,
				  "ftp: %c: unknown option\n", *cp);
				exit(1);
			}
		argc--, argv++;
	}
	fromatty = isatty(fileno(stdin));
	/*
	 * Set up defaults for FTP.
	 */
	strcpy(typename, "ascii"), typevalue = TYPE_A;
	if (fromatty)
		verbose++;
	/*
	 * Set up the home directory in case we're globbing.
	 */
	pw = getpwnam(getlogin());
	if (pw == NULL)
		pw = getpwuid(getuid());
	if (pw != NULL) {
		home = homedir;
		strcpy(home, pw->pw_dir);
	}
	signal(SIGINT, intr);
	signal(SIGPIPE, lostpeer);
	DURING {
		if (argc > 0)
		  setpeer(argc + 1, argv - 1);
	} HANDLER {
		FilingErrMsg(Exception.Code, Exception.Message);
		exit(0);
	} END_HANDLER;
	for (;;) {
		DURING {
			for (;;)
			  cmdscanner();
		} HANDLER {
			FilingErrMsg(Exception.Code, Exception.Message);
			if (connected != (CourierConnection*)0) {
				DURING
				  probe();	/* reset alarm */
				HANDLER {	/* can't?  Lost peer */
				  connected = (CourierConnection*) 0;
				} END_HANDLER;
			}
		} END_HANDLER;
	}
}

intr()
{
	extern probe();

	printf("\n");
	raise(0, (char *)0);
}

lostpeer()
{
	if (connected != (CourierConnection*)0) {
		/* CourierClose(connected); */	/* probably won't work */
		connected = (CourierConnection*) 0;
	}
	raise(EPIPE, "lost peer");
}

char *
tail(filename)
	char *filename;
{
	register char *s;
	
	while (*filename) {
		s = rindex(filename, '/');
		if (s == NULL)
			break;
		if (s[1])
			return (s + 1);
		*s = '\0';
	}
	return (filename);
}

/*
 * Command parser.
 */
cmdscanner()
{
	register struct cmd *c;
	struct cmd *getcmd();
	extern struct cmd cmdtab[];
	extern int help();

	for (;;) {
		if (fromatty) {
			printf("xnsftp> ");
			fflush(stdout);
		}
		if (gets(line) == 0) {
			if (feof(stdin)) {
				clearerr(stdin);
				putchar('\n');
			}
			break;
		}
		if (line[0] == 0)
			break;
		makeargv();
		c = getcmd(margv[0]);
		if (c == (struct cmd *)-1) {
			printf("?Ambiguous command\n");
			continue;
		}
		if (c == 0) {
			printf("?Invalid command\n");
			continue;
		}
		if (c->c_conn && !connected) {
			printf ("Not connected.\n");
			continue;
		}
		(*c->c_handler)(margc, margv);
		if (bell && c->c_bell)
			putchar(CTRL(g));
		if (c->c_handler != help)
			break;
	}
}

struct cmd *
getcmd(name)
	register char *name;
{
	register char *p, *q;
	register struct cmd *c, *found;
	register int nmatches, longest;

	longest = 0;
	nmatches = 0;
	found = 0;
	for (c = cmdtab; p = c->c_name; c++) {
		for (q = name; *q == *p++; q++)
			if (*q == 0)		/* exact match? */
				return (c);
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
		return ((struct cmd *)-1);
	return (found);
}

/*
 * Slice a string up into argc/argv.
 */
makeargv()
{
	char **argp;
	char *slurpstring();

	margc = 0;
	argp = margv;
	stringbase = line;		/* scan from first of buffer */
	argbase = argbuf;		/* store from first of buffer */
	while (*argp++ = slurpstring())
		margc++;
}

/*
 * Parse string into argbuf;
 * implemented with FSM to
 * handle quoting and strings
 */
char *
slurpstring()
{
	int got_one = 0;
	register char *sb = stringbase;
	register char *ap = argbase;
	char *tmp = argbase;		/* will return this if token found */

	if (*sb == '!') {		/* recognize ! as a token for shell */
		stringbase++;
		return ("!");
	}
S0:
	switch (*sb) {

	case '\0':
		goto OUT;

	case ' ':
	case '\t':
		sb++; goto S0;

	default:
		goto S1;
	}

S1:
	switch (*sb) {

	case ' ':
	case '\t':
	case '\0':
		goto OUT;	/* end of token */

	case '\\':
		sb++; goto S2;	/* slurp next character */

	case '"':
		sb++; goto S3;	/* slurp quoted string */

	default:
		*ap++ = *sb++;	/* add character to token */
		got_one = 1;
		goto S1;
	}

S2:
	switch (*sb) {

	case '\0':
		goto OUT;

	default:
		*ap++ = *sb++;
		got_one = 1;
		goto S1;
	}

S3:
	switch (*sb) {

	case '\0':
		goto OUT;

	case '"':
		sb++; goto S1;

	default:
		*ap++ = *sb++;
		got_one = 1;
		goto S3;
	}

OUT:
	if (got_one)
		*ap++ = '\0';
	argbase = ap;			/* update storage pointer */
	stringbase = sb;		/* update scan pointer */
	if (got_one)
		return(tmp);
	return((char *)0);
}

#define HELPINDENT (sizeof ("directory"))

/*
 * Help command.
 * Call each command handler with argc == 0 and argv[0] == name.
 */
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
			printf("%-*s\t%s\n", HELPINDENT,
				c->c_name, c->c_help);
	}
}

/*
 * Call routine with argc, argv set from args (terminated by 0).
 */
/* VARARGS2 */
call(routine, args)
	int (*routine)();
	int args;
{
	register int *argp;
	register int argc;

	for (argc = 0, argp = &args; *argp++ != 0; argc++)
		;
	(*routine)(argc, &args);
}

