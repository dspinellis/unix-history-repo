#

#include "rcv.h"
#include <sys/stat.h>

/*
 * Mail -- a mail program
 *
 * Startup -- interface with user.
 */

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
	int mustsend, uflag;
	FILE *ibuf;
	extern char tempMesg[], _sobuf[];

#ifdef signal
	Siginit();
#endif

	/*
	 * Set up a reasonable environment.  We clobber the last
	 * element of argument list for compatibility with version 6,
	 * figure out whether we are being run interactively, set up
	 * all the temporary files, buffer standard output, and so forth.
	 */

	uflag = 0;
	argv[argc] = (char *) -1;
	mypid = getpid();
	intty = isatty(0);
	outtty = isatty(1);
	image = -1;
	setbuf(stdout, _sobuf);

	/*
	 * Now, determine how we are being used.
	 * We successively pick off instances of -r, -h, -f, and -i.
	 * If there is anything left, it is the base of the list
	 * of users to mail to.  Argp will be set to point to the
	 * first of these users.
	 */

	ef = NOSTR;
	argp = -1;
	mustsend = 0;
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

		case 'u':
			/*
			 * Next argument is person to pretend to be.
			 */
			uflag++;
			if (i >= argc - 1) {
				fprintf(stderr, "You obviously dont know what you're doing\n");
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
	if (argp != -1) {
		commands();
		mail(&argv[argp]);

		/*
		 * why wait?
		 */

		exit(0);
	}

	/*
	 * Ok, we are reading mail.
	 * Decide whether we are editing a mailbox or reading
	 * the system mailbox, and open up the right stuff.
	 */

	rcvmode++;
	if (ef != NOSTR) {
		edit++;
		editfile = mailname = ef;
		if ((ibuf = fopen(mailname, "r")) == NULL) {
			perror(mailname);
			exit(1);
		}
		if ((i = open(mailname, 1)) < 0)
			printf("Warning: \"%s\" not writable.\n", mailname);
		else
			close(i);
	}
	else {
		if ((ibuf = fopen(mailname, "r")) == NULL) {
			if (uflag)
				printf("No mail for %s\n", myname);
			else
				printf("No mail.\n");
			exit(0);
		}
	}

	/*
	 * Copy the messages into /tmp
	 * and set pointers.
	 */

	mailsize = fsize(ibuf);
	if ((otf = fopen(tempMesg, "w")) == NULL) {
		perror(tempMesg);
		exit(1);
	}
	if ((itf = fopen(tempMesg, "r")) == NULL) {
		perror(tempMesg);
		exit(1);
	}
	remove(tempMesg);
	setptr(ibuf);
	fclose(ibuf);

	/*
	 * print headings and accept user commands.
	 */

	if (msgCount == 0) {
		if (uflag)
			printf("No mail for %s\n", myname);
		else
			printf("No messages.\n");
		exit(1);
	}
	commands();
	if (!edit)
		quit();
	exit(0);
}
