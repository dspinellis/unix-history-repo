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
static char sccsid[] = "@(#)main.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

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

#if defined(TN3270)
    init_3270();
#endif
}

	void
usage()
{
	fprintf(stderr, "Usage: %s %s%s%s%s\n",
	    prompt,
#ifdef	AUTHENTICATION
	    "[-8] [-E] [-K] [-L] [-S tos] [-X atype] [-a] [-c] [-d] [-e char]",
	    "\n\t[-k realm] [-l user] [-n tracefile] ",
#else
	    "[-8] [-E] [-L] [-S tos] [-a] [-c] [-d] [-e char] [-l user]",
	    "\n\t[-n tracefile]",
#endif
#if defined(TN3270) && defined(unix)
# ifdef AUTHENTICATION
	    "[-noasynch] [-noasynctty]\n\t[-noasyncnet] [-r] [-t transcom] ",
# else
	    "[-noasynch] [-noasynctty] [-noasyncnet] [-r]\n\t[-t transcom]",
# endif
#else
	    "[-r] ",
#endif
#ifdef	ENCRYPTION
	    "[-x] [host-name [port]]"
#else	/* ENCRYPTION */
	    "[host-name [port]]"
#endif	/* ENCRYPTION */
	);
	exit(1);
}

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
	char *user, *strrchr();

	tninit();		/* Clear out things */
#if	defined(CRAY) && !defined(__STDC__)
	_setlist_init();	/* Work around compiler bug */
#endif

	TerminalSaveState();

	if (prompt = strrchr(argv[0], '/'))
		++prompt;
	else
		prompt = argv[0];

	user = NULL;

	rlogin = (strncmp(prompt, "rlog", 4) == 0) ? '~' : _POSIX_VDISABLE;
	autologin = -1;

	while ((ch = getopt(argc, argv, "8EKLS:X:acde:k:l:n:rt:x")) != EOF) {
		switch(ch) {
		case '8':
			eight = 3;	/* binary output and input */
			break;
		case 'E':
			rlogin = escape = _POSIX_VDISABLE;
			break;
		case 'K':
#ifdef	AUTHENTICATION
			autologin = 0;
#endif
			break;
		case 'L':
			eight |= 2;	/* binary output only */
			break;
		case 'S':
		    {
#ifdef	HAS_GETTOS
			extern int tos;

			if ((tos = parsetos(optarg, "tcp")) < 0)
				fprintf(stderr, "%s%s%s%s\n",
					prompt, ": Bad TOS argument '",
					optarg,
					"; will try to use default TOS");
#else
			fprintf(stderr,
			   "%s: Warning: -S ignored, no parsetos() support.\n",
								prompt);
#endif
		    }
			break;
		case 'X':
#ifdef	AUTHENTICATION
			auth_disable_name(optarg);
#endif
			break;
		case 'a':
			autologin = 1;
			break;
		case 'c':
			skiprc = 1;
			break;
		case 'd':
			debug = 1;
			break;
		case 'e':
			set_escape_char(optarg);
			break;
		case 'k':
#if defined(AUTHENTICATION) && defined(KRB4)
		    {
			extern char *dest_realm, dst_realm_buf[], dst_realm_sz;
			dest_realm = dst_realm_buf;
			(void)strncpy(dest_realm, optarg, dst_realm_sz);
		    }
#else
			fprintf(stderr,
			   "%s: Warning: -k ignored, no Kerberos V4 support.\n",
								prompt);
#endif
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
				else if (!strcmp(optarg, "oasynchnet"))
					noasynchnet = 1;
			} else
#endif	/* defined(TN3270) && defined(unix) */
				SetNetTrace(optarg);
			break;
		case 'r':
			rlogin = '~';
			break;
		case 't':
#if defined(TN3270) && defined(unix)
			transcom = tline;
			(void)strcpy(transcom, optarg);
#else
			fprintf(stderr,
			   "%s: Warning: -t ignored, no TN3270 support.\n",
								prompt);
#endif
			break;
		case 'x':
#ifdef	ENCRYPTION
			encrypt_auto(1);
			decrypt_auto(1);
#else	/* ENCRYPTION */
			fprintf(stderr,
			    "%s: Warning: -x ignored, no ENCRYPT support.\n",
								prompt);
#endif	/* ENCRYPTION */
			break;
		case '?':
		default:
			usage();
			/* NOTREACHED */
		}
	}
	if (autologin == -1)
		autologin = (rlogin == _POSIX_VDISABLE) ? 0 : 1;

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
	for (;;) {
#ifdef TN3270
		if (shell_active)
			shell_continue();
		else
#endif
			command(1, 0, 0);
	}
}
