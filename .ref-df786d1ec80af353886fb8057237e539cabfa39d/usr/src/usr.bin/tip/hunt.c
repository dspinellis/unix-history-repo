#ifndef lint
static char sccsid[] = "@(#)hunt.c	4.7 (Berkeley) %G%";
#endif

#include "tip.h"

extern char *getremote();
extern char *rindex();

static	jmp_buf deadline;
static	int deadfl;

dead()
{

	deadfl = 1;
	longjmp(deadline, 1);
}

hunt(name)
	char *name;
{
	register char *cp;
	int (*f)();

	f = signal(SIGALRM, dead);
	deadfl = 0;
	while (cp = getremote(name)) {
		uucplock = rindex(cp, '/')+1;
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
		if (setjmp(deadline) == 0) {
			alarm(10);
			FD = open(cp, O_RDWR);
		}
		alarm(0);
		if (!deadfl) {
			ioctl(FD, TIOCEXCL, 0);
			ioctl(FD, TIOCHPCL, 0);
			signal(SIGALRM, SIG_DFL);
			return ((int)cp);
		}
		delock(uucplock);
	}
	signal(SIGALRM, f);
	return (deadfl ? -1 : (int)cp);
}
