/* Copyright (c) 1979 Regents of the University of California */
#

#include "rcv.h"

/*
 * Mail -- a mail program
 *
 * Give names to all the temporary files that we will need.
 */

char	tempMail[14];
char	tempQuit[14];
char	tempEdit[14];
char	tempSet[14];
char	tempResid[14];
char	tempMesg[14];

tinit()
{
	register char *cp, *cp2;
	char uname[9];

	copy("/tmp/RsXXXXX", tempMail);
	copy("/tmp/RqXXXXX", tempResid);
	copy("/tmp/RmXXXXX", tempQuit);
	copy("/tmp/ReXXXXX", tempEdit);
	copy("/tmp/RxXXXXX", tempSet);
	copy("/tmp/RxXXXXX", tempMesg);
	mktemp(tempMail);
	mktemp(tempResid);
	mktemp(tempQuit);
	mktemp(tempEdit);
	mktemp(tempSet);
	mktemp(tempMesg);

	uid = getuid() & UIDMASK;
	if (getname(uid, uname) < 0) {
		printf("Who are you!?\n");
		exit(1);
	}
	copy(uname, myname);
	mailname = mailspace;
	cp = value("HOME");
	if (cp == NOSTR)
		cp = ".";
	copy(cp, homedir);
	findmail();
	cp = copy(homedir, mbox);
	copy("/mbox", cp);
	cp = copy(homedir, mailrc);
	copy("/.mailrc", cp);
	cp = copy(homedir, deadletter);
	copy("/dead.letter", cp);
}
