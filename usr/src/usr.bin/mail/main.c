/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifdef notdef
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* notdef */

#ifdef notdef
static char sccsid[] = "@(#)main.c	5.8 (Berkeley) %G%";
#endif /* notdef */

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
	register char *ef, opt;
	register int i;
	struct name *to, *cc, *bcc, *smopts;
	int  mustsend, hdrstop(), (*prevint)(), f;
	struct sgttyb tbuf;
	extern int getopt(), optind, opterr;
	extern char *optarg;

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
	to = NULL;
	cc = NULL;
	bcc = NULL;
	smopts = NULL;
	mustsend = 0;
	if (argc > 0 && **argv == 'r')
		rmail++;
	while ((opt = getopt(argc, argv, "INT:b:c:dfh:inr:s:u:v")) != EOF) {
		switch (opt) {
		case 'r':
			/*
			 * Next argument is address to be sent along
			 * to the mailer.
			 */
			mustsend++;
			rflag = optarg;
			break;
		case 'T':
			/*
			 * Next argument is temp file to write which
			 * articles have been read/deleted for netnews.
			 */
			Tflag = optarg;
			if ((f = creat(Tflag, 0600)) < 0) {
				perror(Tflag);
				exit(1);
			}
			close(f);
			break;
		case 'u':
			/*
			 * Next argument is person to pretend to be.
			 */
			strcpy(myname, optarg);
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
			mustsend++;
			hflag = atoi(optarg);
			if (hflag == 0) {
				fprintf(stderr, "-h needs non-zero number\n");
				exit(1);
			}
			break;
		case 's':
			/*
			 * Give a subject field for sending from
			 * non terminal
			 */
			mustsend++;
			sflag = optarg;
			break;
		case 'f':
			/*
			 * User is specifying file to "edit" with Mail,
			 * as opposed to reading system mailbox.
			 * If no argument is given after -f, we read his
			 * mbox file in his home directory.
			 *
			 * getopt() can't handle optional arguments, so here
			 * is an ugly hack to get around it.
			 */
			if ((argv[optind]) && (argv[optind][0] != '-'))
				ef = argv[optind++];
			else
				ef = mbox;
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
			assign("noheader", "");
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
		case 'c':
			/*
			 * Get Carbon Copy Recipient list
			 */
			cc = cat(cc, nalloc(optarg));
			mustsend++;
			break;
		case 'b':
			/*
			 * Get Blind Carbon Copy Recipient list
			 */
			bcc = cat(bcc, nalloc(optarg));
			mustsend++;
			break;
		case '?':
			fputs("\
Usage: mail [-iInv] [-s subject] [-c cc-addr] [-b bcc-addr] to-addr ...\n\
            [- sendmail-options ...]\n\
       mail [-iInNv] -f [name]\n\
       mail [-iInNv] [-u user]\n",
				stderr);
			exit(1);
		}
	}
	for (i = optind; (argv[i]) && (*argv[i] != '-'); i++)
		to = cat(to, nalloc(argv[i]));
	for (; argv[i]; i++)
		smopts = cat(smopts, nalloc(argv[i]));
	/*
	 * Check for inconsistent arguments.
	 */
	if (!to && (cc || bcc)) {
		fputs("You must also specify direct recipients of mail.\n", stderr);
		exit(1);
	}
	if ((ef != NOSTR) && to) {
		fprintf(stderr, "Cannot give -f and people to send to.\n");
		exit(1);
	}
	if (mustsend && !to) {
		fprintf(stderr, "The flags you gave make no sense since you're not sending mail.\n");
		exit(1);
	}
	tinit();
	setscreensize();
	input = stdin;
	rcvmode = !to;
	if (!nosrc)
		load(MASTER);
	load(mailrc);
	if (!rcvmode) {
		mail(to, cc, bcc, smopts);

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
	if (setjmp(hdrjmp) == 0) {
		if ((prevint = signal(SIGINT, SIG_IGN)) != SIG_IGN)
			signal(SIGINT, hdrstop);
		announce(!0);
		fflush(stdout);
		signal(SIGINT, prevint);
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
