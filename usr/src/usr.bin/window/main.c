#ifndef lint
static	char *sccsid = "@(#)main.c	1.1 83/07/12";
#endif

#include "ww.h"
#include <signal.h>

struct ww *w1, *w2;

main()
{
	register n;
	register char *p;
	int wwchild();
	int imask;
	char buf[512];
	char escape = 0;

	w1 = wwopen(12, 80, 0, 0);
	w2 = wwopen(12, 80, 12, 0);
	wwsetcurrent(w1);
	noecho();
	crmode();
	(void) signal(SIGCHLD, wwchild);
	switch (wwfork(w1)) {
	case -1:
		perror("wwfork");
		goto bad;
	case 0:
		wwexecl(w1, "/bin/csh", "csh", 0);
		perror("wwexecl(w1)");
		goto bad;
	}
	switch (wwfork(w2)) {
	case -1:
		perror("wwfork");
		goto bad;
	case 0:
		wwexecl(w2, "/bin/csh", "csh", 0);
		perror("wwexecl(w2)");
		goto bad;
	}
	for (;;) {
		wwflushall();
		if (!wwhaschildren())
			break;
		imask = 1<<0;
		while (wwforce(&imask) < 0)
			;
		if (imask & 1<<0) {
			n = read(0, buf, sizeof buf);
			for (p = buf; n-- > 0; p++) {
				*p &= 0x7f;
				if (escape) {
					escape = 0;
					switch (*p) {
					case '\\':
						write(_wwcurrent->ww_pty, p, 1);
						break;
					case '1':
						wwsetcurrent(w1);
						break;
					case '2':
						wwsetcurrent(w2);
						break;
					default:
						write(_wwcurrent->ww_pty,
							"\\", 1);
						write(_wwcurrent->ww_pty, p, 1);
					}
				} else {
					if (*p == '\\')
						escape++;
					else
						write(_wwcurrent->ww_pty, p, 1);
				}
			}
		}
	}
bad:
	echo();
	nocrmode();
	wwend();
	return 0;
}
