/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mark Nudleman.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Mark Nudleman.\n\
@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)lesskey.c	5.1 (Berkeley) %G%";
#endif /* not lint */

/*
 *	lesskey [-o output] [input]
 *
 *	Make a .less file.
 *	If no input file is specified, standard input is used.
 *	If no output file is specified, $HOME/.less is used.
 *
 *	The .less file is used to specify (to "less") user-defined
 *	key bindings.  Basically any sequence of 1 to MAX_CMDLEN
 *	keystrokes may be bound to an existing less function.
 *
 *	The input file is an ascii file consisting of a 
 *	sequence of lines of the form:
 *		string <whitespace> action <newline>
 *
 *	"string" is a sequence of command characters which form
 *		the new user-defined command.  The command
 *		characters may be:
 *		1. The actual character itself.
 *		2. A character preceeded by ^ to specify a
 *		   control character (e.g. ^X means control-X).
 *		3. Any character (other than an octal digit) preceeded by
 *		   a \ to specify the character itself (characters which
 *		   must be preceeded by \ include ^, \, and whitespace.
 *		4. A backslash followed by one to three octal digits
 *		   to specify a character by its octal value.
 *	"action" is the name of a "less" action, from the table below.
 *
 *	Blank lines and lines which start with # are ignored.
 *
 *
 *	The output file is a non-ascii file, consisting of
 *	zero or more byte sequences of the form:
 *		string <0> <action>
 *
 *	"string" is the command string.
 *	"<0>" is one null byte.
 *	"<action>" is one byte containing the action code (the A_xxx value).
 *
 *
 *		Revision history
 *
 *	v1: Initial version.					10/13/87  mark
 */

#include <stdio.h>
#include "less.h"
#include "cmd.h"

char usertable[MAX_USERCMD];

struct cmdname
{
	char *cn_name;
	int cn_action;
} cmdnames[] = 
{
	"back-line",		A_B_LINE,
	"back-screen",		A_B_SCREEN,
	"back-scroll",		A_B_SCROLL,
	"back-search",		A_B_SEARCH,
	"debug",		A_DEBUG,
	"display-flag",		A_DISP_OPTION,
	"display-option",	A_DISP_OPTION,
	"end",			A_GOEND,
	"examine",		A_EXAMINE,
	"first-cmd",		A_FIRSTCMD,
	"firstcmd",		A_FIRSTCMD,
	"flush-repaint",	A_FREPAINT,
	"forw-line",		A_F_LINE,
	"forw-screen",		A_F_SCREEN,
	"forw-scroll",		A_F_SCROLL,
	"forw-search",		A_F_SEARCH,
	"goto-end",		A_GOEND,
	"goto-line",		A_GOLINE,
	"goto-mark",		A_GOMARK,
	"help",			A_HELP,
	"invalid",		A_NOACTION,
	"next-file",		A_NEXT_FILE,
	"noaction",		A_NOACTION,
	"percent",		A_PERCENT,
	"prev-file",		A_PREV_FILE,
	"quit",			A_QUIT,
	"repaint",		A_REPAINT,
	"repaint-flush",	A_FREPAINT,
	"repeat-search",	A_AGAIN_SEARCH,
	"set-mark",		A_SETMARK,
	"shell",		A_SHELL,
	"status",		A_STAT,
	"toggle-flag",		A_TOGGLE_OPTION,
	"toggle-option",	A_TOGGLE_OPTION,
	"version",		A_VERSION,
	"visual",		A_VISUAL,
	NULL,			0
};

main(argc, argv)
	int argc;
	char *argv[];
{
	char *p;		/* {{ Can't be register since we use &p }} */
	register char *up;	/* Pointer into usertable */
	FILE *desc;		/* Description file (input) */
	FILE *out;		/* Output file */
	int linenum;		/* Line number in input file */
	char *currcmd;		/* Start of current command string */
	int errors;
	int i;
	char line[100];
	char *outfile;

	extern char *getenv();

	/*
	 * Process command line arguments.
	 */
	outfile = NULL;
	while (--argc > 0 && **(++argv) == '-')
	{
		switch (argv[0][1])
		{
		case 'o':
			outfile = &argv[0][2];
			if (*outfile == '\0')
			{
				if (--argc <= 0)
					usage();
				outfile = *(++argv);
			}
			break;
		default:
			usage();
		}
	}
	if (argc > 1)
		usage();


	/*
	 * Open the input file, or use standard input if none specified.
	 */
	if (argc > 0)
	{
		if ((desc = fopen(*argv, "r")) == NULL)
		{
			perror(*argv);
			exit(1);
		}
	} else
		desc = stdin;

	/*
	 * Read the input file, one line at a time.
	 * Each line consists of a command string,
	 * followed by white space, followed by an action name.
	 */
	linenum = 0;
	errors = 0;
	up = usertable;
	while (fgets(line, sizeof(line), desc) != NULL)
	{
		++linenum;

		/*
		 * Skip leading white space.
		 * Replace the final newline with a null byte.
		 * Ignore blank lines and comment lines.
		 */
		p = line;
		while (*p == ' ' || *p == '\t')
			++p;
		for (i = 0;  p[i] != '\n' && p[i] != '\0';  i++)
			;
		p[i] = '\0';
		if (*p == '#' || *p == '\0')
			continue;

		/*
		 * Parse the command string and store it in the usertable.
		 */
		currcmd = up;
		do
		{
			if (up >= usertable + MAX_USERCMD)
			{
				fprintf(stderr, "too many commands, line %d\n",
					linenum);
				exit(1);
			}
			if (up >= currcmd + MAX_CMDLEN)
			{
				fprintf(stderr, "command too long on line %d\n",
					linenum);
				errors++;
				break;
			}

			*up++ = tchar(&p);

		} while (*p != ' ' && *p != '\t' && *p != '\0');

		/*
		 * Terminate the command string with a null byte.
		 */
		*up++ = '\0';

		/*
		 * Skip white space between the command string
		 * and the action name.
		 * Terminate the action name if it is followed
		 * by whitespace or a # comment.
		 */
		if (*p == '\0')
		{
			fprintf(stderr, "missing whitespace on line %d\n",
				linenum);
			errors++;
			continue;
		}
		while (*p == ' ' || *p == '\t')
			++p;
		for (i = 0;  p[i] != ' ' && p[i] != '\t' && 
			     p[i] != '#' && p[i] != '\0';  i++)
			;
		p[i] = '\0';

		/*
		 * Parse the action name and store it in the usertable.
		 */
		for (i = 0;  cmdnames[i].cn_name != NULL;  i++)
			if (strcmp(cmdnames[i].cn_name, p) == 0)
				break;
		if (cmdnames[i].cn_name == NULL)
		{
			fprintf(stderr, "unknown action <%s> on line %d\n",
				p, linenum);
			errors++;
			continue;
		}
		*up++ = cmdnames[i].cn_action;
	}

	if (errors > 0)
	{
		fprintf(stderr, "%d errors; no output produced\n", errors);
		exit(1);
	}

	/*
	 * Write the output file.
	 * If no output file was specified, use "$HOME/.less"
	 */
	if (outfile == NULL)
	{
		p = getenv("HOME");
		if (p == NULL)
		{
			fprintf(stderr, "cannot find $HOME\n");
			exit(1);
		}
		strcpy(line, p);
		strcat(line, "/.less");
		outfile = line;
	}
	if ((out = fopen(outfile, "w")) == NULL)
		perror(outfile);
	else
		fwrite((char *)usertable, 1, up-usertable, out);
}

/*
 * Parse one character of the command string.
 */
tchar(pp)
	char **pp;
{
	register char *p;
	register char ch;
	register int i;

	p = *pp;
	switch (*p)
	{
	case '\\':
		if (*++p >= '0' && *p <= '7')
		{
			/*
			 * Parse an octal number.
			 */
			ch = 0;
			i = 0;
			do
				ch = 8*ch + (*p - '0');
			while (*++p >= '0' && *p <= '7' && ++i < 3);
			*pp = p;
			return (ch);
		}
		/*
		 * Backslash followed by a char just means that char.
		 */
		*pp = p+1;
		return (*p);
	case '^':
		/*
		 * Carat means CONTROL.
		 */
		*pp = p+2;
		return (CONTROL(p[1]));
	}
	*pp = p+1;
	return (*p);
}

usage()
{
	fprintf(stderr, "usage: lesskey [-o output] [input]\n");
	exit(1);
}
