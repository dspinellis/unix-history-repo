/*
 * Copyright (c) 1988, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988, 1990 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	1.15 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <string.h>

#include "ring.h"
#include "externs.h"
#include "defines.h"

/*
 * Initialize variables.
 */
void
tninit()
{
	init_terminal();
	init_network();
	init_telnet();
	init_sys();
	init_3270();
}

int	autologin;

/*
 * main.  Parse arguments, invoke the protocol or command parser.
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	int ch;
	char *user;

	tninit();		/* Clear out things */
#ifdef CRAY
	_setlist_init();	/* Work around compiler bug */
#endif
	TerminalSaveState();

	if (prompt = rindex(argv[0], '/'))
		++prompt;
	else
		prompt = argv[0];

	user = NULL;
	autologin = 0;
	while ((ch = getopt(argc, argv, "ade:l:n:")) != EOF)
		switch(ch) {
		case 'a':
			autologin = 1;
			break;
		case 'd':
			debug = 1;
			break;
		case 'e':
			set_escape_char(optarg);
			break;
		case 'l':
			autologin = 1;
			user = optarg;
			break;
		case 'n':
#if defined(TN3270) && defined(unix)
			/* distinguish between "-n oasynch" and "-noasynch" */
			if (argv[optind - 1][0] == '-' && argv[optind - 1][1]
			    == 'n' && argv[optind - 1][2] == 'o') {
				if (!strcmp(optarg, "oasynch")) {
					noasynchtty = 1;
					noasynchnet = 1;
				} else if (!strcmp(optarg, "oasynchtty"))
					noasynchtty = 1;
				} else if (!strcmp(optarg, "oasynchnet"))
					noasynchnet = 1;
				}
			} else
#endif	/* defined(TN3270) && defined(unix) */
				SetNetTrace(optarg);
			break;
#if defined(TN3270) && defined(unix)
		case 't':
			transcom = tline;
			(void)strcpy(transcom, optarg);
			break;
#endif
		case '?':
		default:
			usage();
			/* NOTREACHED */
		}
	argc -= optind;
	argv += optind;

	if (argc) {
		char *args[7], **argp = args;

		if (argc > 2)
			usage();
		*argp++ = prompt;
		if (user) {
			*argp++ = "-l";
			*argp++ = user;
		}
		*argp++ = argv[0];		/* host */
		if (argc > 1)
			*argp++ = argv[1];	/* port */
		*argp = 0;

		if (setjmp(toplevel) != 0)
			Exit(0);
		if (tn(argp - args, args) == 1)
			return (0);
		else
			return (1);
	}
	(void)setjmp(toplevel);
	for (;;)
#ifdef TN3270
		if (shell_active)
			shell_continue();
		else
#endif
			command(1, 0, 0);
}

usage()
{
	fprintf(stderr, "usage: %s [-a] [ [-l user] host-name [port] ]\n",
	    prompt);
	exit(1);
}
