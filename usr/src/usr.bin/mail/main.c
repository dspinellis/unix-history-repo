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
static char sccsid[] = "@(#)main.c	5.15 (Berkeley) %G%";
#endif /* notdef */

#include "rcv.h"
#include <sys/stat.h>

/*
 * Mail -- a mail program
 *
 * Startup -- interface with user.
 */

jmp_buf	hdrjmp;

main(argc, argv)
	char **argv;
{
	register int i;
	struct name *to, *cc, *bcc, *smopts;
	char *subject;
	char *ef;
	int hdrstop(), (*prevint)();
	extern int getopt(), optind, opterr;
	extern char *optarg;

	/*
	 * Set up a reasonable environment.
	 * Figure out whether we are being run interactively, set up
	 * all the temporary files, buffer standard output, and so forth.
	 */
	mypid = getpid();
	if (isatty(0))
		assign("interactive", "");
	image = -1;
	/*
	 * Now, determine how we are being used.
	 * We successively pick off - flags.
	 * If there is anything left, it is the base of the list
	 * of users to mail to.  Argp will be set to point to the
	 * first of these users.
	 */
	ef = NOSTR;
	to = NIL;
	cc = NIL;
	bcc = NIL;
	smopts = NIL;
	subject = NOSTR;
	while ((i = getopt(argc, argv, "INT:b:c:dfins:u:v")) != EOF) {
		switch (i) {
		case 'T':
			/*
			 * Next argument is temp file to write which
			 * articles have been read/deleted for netnews.
			 */
			Tflag = optarg;
			if ((i = creat(Tflag, 0600)) < 0) {
				perror(Tflag);
				exit(1);
			}
			close(i);
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
		case 's':
			/*
			 * Give a subject field for sending from
			 * non terminal
			 */
			subject = optarg;
			break;
		case 'f':
			/*
			 * User is specifying file to "edit" with Mail,
			 * as opposed to reading system mailbox.
			 * If no argument is given after -f, we read his
			 * mbox file.
			 *
			 * getopt() can't handle optional arguments, so here
			 * is an ugly hack to get around it.
			 */
			if ((argv[optind]) && (argv[optind][0] != '-'))
				ef = argv[optind++];
			else
				ef = "&";
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
			assign("interactive", "");
			break;
		case 'c':
			/*
			 * Get Carbon Copy Recipient list
			 */
			cc = cat(cc, nalloc(optarg, GCC));
			break;
		case 'b':
			/*
			 * Get Blind Carbon Copy Recipient list
			 */
			bcc = cat(bcc, nalloc(optarg, GBCC));
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
		to = cat(to, nalloc(argv[i], GTO));
	for (; argv[i]; i++)
		smopts = cat(smopts, nalloc(argv[i], 0));
	/*
	 * Check for inconsistent arguments.
	 */
	if (to == NIL && (subject != NOSTR || cc != NIL || bcc != NIL)) {
		fputs("You must specify direct recipients with -s, -c, or -b.\n", stderr);
		exit(1);
	}
	if (ef != NOSTR && to != NIL) {
		fprintf(stderr, "Cannot give -f and people to send to.\n");
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
		mail(to, cc, bcc, smopts, subject);
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
	if (ef != NOSTR)
		edit++;
	else
		ef = "%";
	if ((ef = expand(ef)) == NOSTR)
		exit(1);		/* error already reported */
	if (setfile(ef, edit) < 0) {
		if (edit)
			perror(ef);
		else
			fprintf(stderr, "No mail for %s\n", myname);
		exit(1);
	}
	if (setjmp(hdrjmp) == 0) {
		extern char *version;

		if ((prevint = signal(SIGINT, SIG_IGN)) != SIG_IGN)
			signal(SIGINT, hdrstop);
		if (value("quiet") == NOSTR)
			printf("Mail version %s.  Type ? for help.\n",
				version);
		announce();
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
 * Compute what the screen size for printing headers should be.
 * We use the following algorithm for the height:
 *	If baud rate < 1200, use  9
 *	If baud rate = 1200, use 14
 *	If baud rate > 1200, use 24 or ws_row
 * Width is either 80 or ws_col;
 */
setscreensize()
{
	struct sgttyb tbuf;
#ifdef	TIOCGWINSZ
	struct winsize ws;

	if (ioctl(1, TIOCGWINSZ, (char *) &ws) < 0)
#endif
		ws.ws_col = ws.ws_row = 0;
	if (gtty(1, &tbuf) < 0)
		tbuf.sg_ospeed = B9600;
	if (tbuf.sg_ospeed < B1200)
		screenheight = 9;
	else if (tbuf.sg_ospeed == B1200)
		screenheight = 14;
	else if (ws.ws_row != 0)
		screenheight = ws.ws_row;
	else
		screenheight = 24;
	if ((realscreenheight = ws.ws_row) == 0)
		realscreenheight = 24;
	if ((screenwidth = ws.ws_col) == 0)
		screenwidth = 80;
}
