/* signal.c: a Hugh-approved signal handler. */

#include <signal.h>
#include <setjmp.h>
#include "rc.h"
#include "sigmsgs.h"
#include "jbwrap.h"

Jbwrap slowbuf;
volatile SIG_ATOMIC_T slow, interrupt_happened;
void (*sighandlers[NUMOFSIGNALS])(int);

static volatile SIG_ATOMIC_T sigcount, caught[NUMOFSIGNALS];

extern void catcher(int s) {
	if (forked)
		exit(1); /* exit unconditionally on a signal in a child process */
	if (caught[s] == 0) {
		sigcount++;
		caught[s] = 1;
	}
	signal(s, catcher);
	interrupt_happened = TRUE;
	if (slow)
		longjmp(slowbuf.j, 1);
}

extern void sigchk() {
	void (*h)(int);
	int s, i;
	if (sigcount == 0)
		return; /* ho hum; life as usual */
	if (forked)
		exit(1); /* exit unconditionally on a signal in a child process */
	for (i = 0, s = -1; i < NUMOFSIGNALS; i++)
		if (caught[i] != 0) {
			s = i;
			--sigcount;
			caught[s] = 0;
			break;
		}
	if (s == -1)
		panic("all-zero sig vector with nonzero sigcount");
	if ((h = sighandlers[s]) == SIG_DFL)
		panic("caught signal set to SIG_DFL");
	if (h == SIG_IGN)
		panic("caught signal set to SIG_IGN");
	(*h)(s);
}

extern void (*rc_signal(int s, void (*h)(int)))(int) {
	void (*old)(int);
	SIGCHK;
	old = sighandlers[s];
	if (h == SIG_DFL || h == SIG_IGN) {
		signal(s, h);
		sighandlers[s] = h;
	} else {
		sighandlers[s] = h;
		signal(s, catcher);
	}
	return old;
}

extern void initsignal() {
	void (*h)(int);
	int i;
	for (i = 1; i < NUMOFSIGNALS; i++) {
		if ((h = signal(i, SIG_DFL)) != SIG_DFL)
			signal(i, h);
		sighandlers[i] = h;
	}
}
