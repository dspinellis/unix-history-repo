/*	dn11.c	4.1	81/05/09	*/
/*
 * Routines for dialing up on DN-11
 */
#include "tip.h"

int dn_abort();

#if DN11
dn_dialer(num, acu)
char *num, *acu;
{
	extern errno;
	char *p, *q, b[30];
	int child = -1, dn, t, connected = 1;

	if ((dn = open(acu, 1)) < 0) {
		if (errno == 6)
			printf("line busy\n");
		return(0);
	}
	if ((child = fork()) == -1) {
		printf("can't fork\n");
		return(0);
	}
	if (child == 0) {
		signal(SIGALRM, SIG_IGN);
		pause();
	}
	sleep(2);
	/*
	 * copy phone #, assure EON
	 */
	for (p = b, q = num; *p = *q; p++, q++)
		;
	if (*(p-1) != '<') {
		if (*(p-1) != '-')
			*p++ = '-';
		*p++ = '<';
	}
	close(FD);
	t = p-b;
	signal(SIGALRM, dn_abort);
	alarm(5*t);
	t = write(dn, b, t);
	alarm(0);
	if (t < 0) {
		printf("dn11 write error\n");
		connected = 0;
		goto error;
	}
	alarm(40);		/* was 5; sometimes missed carrier */
	FD = open(DV, 2);
	alarm(0);
	if (FD < 0) {
		if (errno == 4)
			printf("lost carrier\n");
		connected = 0;
		goto error;
	}
	ioctl(FD, TIOCEXCL, 0);
	ioctl(FD, TIOCHPCL, 0);
error: 
	kill(child, SIGKILL);
	alarm(10);
	while ((t = wait((int *)NULL)) != -1 && t != child)
		;
	alarm(0);
	signal(SIGALRM, SIG_DFL);
	return(connected);
}

dn_disconnect() { }

dn_abort() { }
#endif
