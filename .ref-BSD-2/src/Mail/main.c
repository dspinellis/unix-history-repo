/* Copyright (c) 1979 Regents of the University of California */
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
 */

main(argc, argv)
	char **argv;
{
	register char *ef;
	register int i;
	FILE *ibuf;
	extern char tempMesg[], _sobuf[];

	argv[argc] = (char *) -1;
	mypid = getpid();
	intty = isatty(0);
	outtty = isatty(1);
	setbuf(stdout, _sobuf);
	tinit();
	ef = NOSTR;
	for (i = 1; i < argc; i++) {
		if (equal(argv[i], "-f")) {
			ef = argv[i+1];
			break;
		}
		if (equal(argv[i], "-n")) {
			demail();
			exit(0);
		}
	}
	if (ef == NOSTR && argc > 1) {
		commands();
		i = 1;
		if (equal(argv[1], "-i")) {
			assign("ignore", "");
			i++;
		}
		mail(&argv[i]);

		/*
		 * why wait?
		 */

		exit(0);
	}
	rcvmode++;
	if (ef != NOSTR) {
		edit++;
		if (ef == (char *) -1)
			ef = mbox;
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
			printf("No mail.\n");
			exit(0);
		}
	}

	/*
	 * Copy the mudder into /tmp
	 * and set pointers.
	 * Announce the presence of this funny file.
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
	unlink(tempMesg);
	setptr(ibuf);
	fclose(ibuf);

	/*
	 * print headings and accept user commands. */

	if (msgCount == 0) {
		printf("No messages.\n");
		exit(1);
	}
	commands();
	if (!edit)
		quit();
	exit(0);
}
