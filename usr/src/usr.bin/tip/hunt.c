/*	hunt.c	4.1	81/05/09	*/
#include "tip.h"

#define RD	04

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

	deadfl = 0;
	signal(SIGALRM, dead);
	while(cp = getremote(name)){
		if (access(cp, RD))
			continue;
		uucplock = rindex(cp, '/')+1;
		if (mlock(uucplock) < 0) {
			delock(uucplock);
			continue;
		}
		alarm(10);
		if((FD = open(cp, 2)) >= 0){
			alarm(0);
			if(deadfl)
				continue;
			ioctl(FD, TIOCEXCL, 0);
			break;
		}
		alarm(0);
		signal(SIGALRM, dead);
	}
	alarm(0);
	signal(SIGALRM, SIG_DFL);
	if(deadfl)
		return(-1);
	return((int)cp);
}
