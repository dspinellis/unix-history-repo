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
	register int err = 0;
	register int pid;

	pid = getpid();
	sprintf(tempMail, "/tmp/Rs%05d", pid);
	sprintf(tempResid, "/tmp/Rq%05d", pid);
	sprintf(tempQuit, "/tmp/Rm%05d", pid);
	sprintf(tempEdit, "/tmp/Re%05d", pid);
	sprintf(tempSet, "/tmp/Rx%05d", pid);
	sprintf(tempMesg, "/tmp/Rx%05d", pid);

	if (strlen(myname) != 0) {
		uid = getuserid(myname);
		if (uid == -1)
			goto youlose;
	}
	else {
		uid = getuid() & UIDMASK;
		if (username(uid, uname) < 0) {
			copy("ubluit", myname);
youlose:
			err++;
			if (rcvmode) {
				printf("Who are you!?\n");
				exit(1);
			}
		}
		copy(uname, myname);
	}
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
	if (debug) {
		printf("uid = %d, user = %s, mailname = %s\n",
		    uid, myname, mailname);
		printf("deadletter = %s, mailrc = %s, mbox = %s\n",
		    deadletter, mailrc, mbox);
	}
}
