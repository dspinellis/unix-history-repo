/*	hunt.c	4.6	83/06/15	*/
#include "tip.h"
#include <sys/stat.h>

#define RD	04
#define EX	01

static char *sccsid = "@(#)hunt.c	4.6 %G%";
extern char *getremote();
extern char *rindex();

int deadfl;

dead()
{
	deadfl = 1;
}

hunt(name)
	char *name;
{
	register char *cp;
	char string[100];
	struct stat	statbuf;

	deadfl = 0;
	signal(SIGALRM, dead);
	while (cp = getremote(name)) {
		uucplock = rindex(cp, '/')+1;
		if(strncmp(uucplock,"ttyd",4) == 0){
			/* reverse dialin line */
			if((stat(cp,&statbuf) != 0) ||
			((statbuf.st_mode & EX) == 0))
				continue;
			sprintf(string,"/usr/lib/uucp/disable %s",uucplock);
			if(system(string)) continue;
			sleep(5); /* insure that phone line is dropped */
		}
		if (mlock(uucplock) < 0) {
			delock(uucplock);
			continue;
		}
		/*
		 * Straight through call units, such as the BIZCOMP,
		 * VADIC and the DF, must indicate they're hardwired in
		 *  order to get an open file descriptor placed in FD.
		 * Otherwise, as for a DN-11, the open will have to
		 *  be done in the "open" routine.
		 */
		if (!HW)
			break;
		alarm(10);
		if ((FD = open(cp, 2)) >= 0){
			alarm(0);
			if (!deadfl) {
				ioctl(FD, TIOCEXCL, 0);
				ioctl(FD, TIOCHPCL, 0);
				signal(SIGALRM, SIG_DFL);
				return ((int)cp);
			}
		}
		alarm(0);
		signal(SIGALRM, dead);
		delock(uucplock);
	}
	signal(SIGALRM, SIG_DFL);
	return (deadfl ? -1 : (int)cp);
}
