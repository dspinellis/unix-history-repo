/*	dn11.c	4.3	81/06/16	*/

#if DN11
/*
 * Routines for dialing up on DN-11
 */
#include "tip.h"
#include <setjmp.h>
#include <errno.h>

int dn_abort();

int alarmtr();

static jmp_buf jmpbuf;

dn_dialer(num, acu)
char *num, *acu;
{
	extern errno;
	char *p, *q, phone[40];
	int child = -1, dn, lt, nw, connected = 1;
	register int timelim;

	if ((dn = open(acu, 1)) < 0) {
		if (errno == ENXIO)
			printf("line busy\n");
		else
			printf("acu open error\n");
		return(0);
	}
	if (setjmp(jmpbuf)) {
		printf("dn11 write error...");
		kill(child, SIGKILL);
		close(dn);
		return(0);
	}
	signal(SIGALRM, alarmtr);
	timelim = 5 * strlen(num);
	alarm(timelim < 30 ? 30 : timelim);
	if ((child = fork()) == 0) {
		signal(SIGALRM, SIG_IGN);
		sleep(2);
		nw = write(dn, num, lt = strlen(num));
		if (nw != lt) {
			printf("dn11 write failed...");
			exit(1);
		}
		exit(0);
	}
	/*  open line - will return on carrier */
	FD = open(DV, 2);
	if (FD < 0) {
		if (errno == EIO)
			printf("lost carrier...");
		else
			printf("dialup line open failed...");
		alarm(0);
		kill(child, SIGKILL);
		close(dn);
		return(0);
	}
	ioctl(dn, TIOCHPCL, 0);
	signal(SIGALRM, SIG_DFL);
	while ((nw = wait(&lt)) != child && nw != -1)
		;
	alarm(0);
	fflush(stdout);
	if (lt != 0) {
		close(FD);
		close(dn);
		return(0);
	}
	return(1);
}

alarmtr()
{
	alarm(0);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	longjmp(jmpbuf, 1);
}

/*
 * Insurance, for some reason we don't seem to be
 *  hanging up...
 */
dn_disconnect()
{
#ifdef VMUNIX
	if (FD > 0)
		ioctl(FD, TIOCCDTR, 0);
#endif
	close(FD);
}

dn_abort()
{
#ifdef VMUNIX
	if (FD > 0)
		ioctl(FD, TIOCCDTR, 0);
#endif
	close(FD);
}
#endif
