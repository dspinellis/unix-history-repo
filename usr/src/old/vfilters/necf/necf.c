/*	necf.c	1.3	81/06/01	*/
#include <stdio.h>
#include <sgtty.h>
#include <signal.h>

#define PAGESIZE	66

struct sgttyb tty;

main()
{
	extern char _sobuf[BUFSIZ];
	extern char *rindex();
	char line[256];
	register char c, *cp;
	register lnumber;

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGHUP, SIG_IGN);

	tty.sg_ispeed = tty.sg_ospeed = B9600;
	tty.sg_erase = tty.sg_kill = -1;
	tty.sg_flags = (ANYP|XTABS|LDECCTQ);
	if (ioctl(1, TIOCSETP, (char *)&tty) < 0)
		exit (2);
	setbuf(stdout, _sobuf);
#ifdef SHEETFEEDER
	printf("\033=\033\033\033O\f");
#else
	printf("\033=");
#endif
	lnumber = 0;
	while (fgets(line, sizeof(line), stdin) != NULL) {
#ifdef SHEETFEEDER
		if (lnumber == PAGESIZE-1) {
			putchar('\f');
			lnumber = 0;
		}
		if (lnumber >= 2) {
#endif
			if ((cp = rindex(line, '\n')) != NULL)
				*cp = '\r';
			printf("%s", line);
#ifdef SHEETFEEDER
		}
		lnumber++;
#endif
	}
	fflush (stdout);
}
