/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char *copyright =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char *sccsid = "@(#)main.c	5.4 (Berkeley) %G%";
#endif not lint

#include "rcv.h"
#include <sys/stat.h>

/*
 * Mail -- a mail program
 *
 * Startup -- interface with user.
 */

jmp_buf	hdrjmp;

/*
 * Find out who the user is, copy his mail file (if exists) into
 * /tmp/Rxxxxx and set up the message pointers.  Then, print out the
 * message headers and read user commands.
 *
 * Command line syntax:
 *	Mail [ -i ] [ -r address ] [ -h number ] [ -f [ name ] ]
 * or:
 *	Mail [ -i ] [ -r address ] [ -h number ] people ...
 */

main(argc, argv)
	char **argv;
{
	register char *ef;
	register int i, argp;
	int mustsend, hdrstop(), (*prevint)(), f;
	struct sgttyb tbuf;

	/*
	 * Set up a reasonable environment.
	 * Figure out whether we are being run interactively, set up
	 * all the temporary files, buffer standard output, and so forth.
	 */

#ifdef	GETHOST
	inithost();
#endif	GETHOST
	mypid = getpid();
	intty = isatty(0);
	outtty = isatty(1);
	if (outtty) {
		gtty(1, &tbuf);
		baud = tbuf.sg_ospeed;
	} else
		baud = B9600;
	image = -1;

	/*
	 * Now, determine how we are being used.
	 * We successively pick off instances of -r, -h, -f, and -i.
	 * If called as "rmail" we note this fact for letter sending.
	 * If there is anything left, it is the base of the list
	 * of users to mail to.  Argp will be set to point to the
	 * first of these users.
	 */

	ef = NOSTR;
	argp = -1;
	mustsend = 0;
	if (argc > 0 && **argv == 'r')
		rmail++;
	for (i = 1; i < argc; i++) {

		/*
		 * If current argument is not a flag, then the
		 * rest of the arguments must be recipients.
		 */

		if (*argv[i] != '-') {
			argp = i;
			break;
		}
		switch (argv[i][1]) {
		case 'r':
			/*
			 * Next argument is address to be sent along
			 * to the mailer.
			 */
			if (i >= argc - 1) {
				fprintf(stderr, "Address required after -r\n");
				exit(1);
			}
			mustsend++;
			rflag = argv[i+1];
			i++;
			break;

		case 'T':
			/*
			 * Next argument is temp file to write which
			 * articles have been read/deleted for netnews.
			 */
			if (i >= argc - 1) {
				fprintf(stderr, "Name required after -T\n");
				exit(1);
			}
			Tflag = argv[i+1];
			if ((f = creat(Tflag, 0600)) < 0) {
				perror(Tflag);
				exit(1);
			}
			close(f);
			i++;
			break;

		case 'u':
			/*
			 * Next argument is person to pretend to be.
			 */
			if (i >= argc - 1) {
				fprintf(stderr, "Missing user name for -u\n");
				exit(1);
			}
			strcpy(myname, argv[i+1]);
			i++;
			break;

		case 'i':
			/*
			 * User wants to ignore interrupts.
			 * Set the variable "ignore"
			 */
			assign("ignore", "");
			break;

		case 'd':
			debug++;
			break;

		case 'h':
			/*
			 * Specified sequence number for network.
			 * This is the number of "hops" made so
			 * far (count of times message has been
			 * forwarded) to help avoid infinite mail loops.
			 */
			if (i >= argc - 1) {
				fprintf(stderr, "Number required for -h\n");
				exit(1);
			}
			mustsend++;
			hflag = atoi(argv[i+1]);
			if (hflag == 0) {
				fprintf(stderr, "-h needs non-zero number\n");
				exit(1);
			}
			i++;
			break;

		case 's':
			/*
			 * Give a subject field for sending from
			 * non terminal
			 */
			if (i >= argc - 1) {
				fprintf(stderr, "Subject req'd for -s\n");
				exit(1);
			}
			mustsend++;
			sflag = argv[i+1];
			i++;
			break;

		case 'f':
			/*
			 * User is specifying file to "edit" with Mail,
			 * as opposed to reading system mailbox.
			 * If no argument is given after -f, we read his
			 * mbox file in his home directory.
			 */
			if (i >= argc - 1)
				ef = mbox;
			else
				ef = argv[i + 1];
			i++;
			break;

		case 'n':
			/*
			 * User doesn't want to source /usr/lib/Mail.rc
			 */
			nosrc++;
			break;

		case 'N':
			/*
			 * Avoid initial header printing.
			 */
			noheader++;
			break;

		case 'v':
			/*
			 * Send mailer verbose flag
			 */
			assign("verbose", "");
			break;

		case 'I':
			/*
			 * We're interactive
			 */
			intty = 1;
			break;

		default:
			fprintf(stderr, "Unknown flag: %s\n", argv[i]);
			exit(1);
		}
	}

	/*
	 * Check for inconsistent arguments.
	 */

	if (ef != NOSTR && argp != -1) {
		fprintf(stderr, "Cannot give -f and people to send to.\n");
		exit(1);
	}
	if (mustsend && argp == -1) {
		fprintf(stderr, "The flags you gave make no sense since you're not sending mail.\n");
		exit(1);
	}
	tinit();
	setscreensize();
	input = stdin;
	rcvmode = argp == -1;
	if (!nosrc)
		load(MASTER);
	load(mailrc);
	if (argp != -1) {
		mail(&argv[argp]);

		/*
		 * why wait?
		 */

		exit(senderr);
	}

	/*
	 * Ok, we are reading mail.
	 * Decide whether we are editing a mailbox or reading
	 * the system mailbox, and open up the right stuff.
	 */

	if (ef != NOSTR) {
		char *ename;

		edit++;
		ename = expand(ef);
		if (ename != ef) {
			ef = malloc((unsigned) strlen(ename) + 1);
			strcpy(ef, ename);
		}
		editfile = ef;
		strcpy(mailname, ef);
	}
	if (setfile(mailname, edit) < 0) {
		if (edit)
			perror(mailname);
		else
			fprintf(stderr, "No mail for %s\n", myname);
		exit(1);
	}
	if (!noheader && value("noheader") == NOSTR) {
		if (setjmp(hdrjmp) == 0) {
			if ((prevint = signal(SIGINT, SIG_IGN)) != SIG_IGN)
				signal(SIGINT, hdrstop);
			announce(!0);
			fflush(stdout);
			signal(SIGINT, prevint);
		}
	}
	if (!edit && msgCount == 0) {
		printf("No mail\n");
		fflush(stdout);
		exit(0);
	}
	commands();
	if (!edit) {
		signal(SIGHUP, SIG_IGN);
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		quit();
	}
	exit(0);
}

/*
 * Interrupt printing of the headers.
 */
hdrstop()
{

	fflush(stdout);
	fprintf(stderr, "\nInterrupt\n");
	longjmp(hdrjmp, 1);
}

/*
 * Compute what the screen size should be.
 * We use the following algorithm for the height:
 *	If baud rate < 1200, use  9
 *	If baud rate = 1200, use 14
 *	If baud rate > 1200, use 24 or ws_row
 * Width is either 80 or ws_col;
 */
setscreensize()
{
#ifdef	TIOCGWINSZ
	struct winsize ws;

	if (ioctl(fileno(stdout), TIOCGWINSZ, (char *) &ws) < 0)
		ws.ws_col = ws.ws_row = 0;
#endif
	if (baud < B1200)
		screenheight = 9;
	else if (baud == B1200)
		screenheight = 14;
#ifdef	TIOCGWINSZ
	else if (ws.ws_row != 0)
		screenheight = ws.ws_row;
#endif
	else
		screenheight = 24;
#ifdef TIOCGWINSZ
	if (ws.ws_col != 0)
		screenwidth = ws.ws_col;
	else
#endif
		screenwidth = 80;
}
