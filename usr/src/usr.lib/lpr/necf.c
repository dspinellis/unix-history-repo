/*	necf.c	1.5	83/01/05	*/
#include <stdio.h>
#include <sgtty.h>
#include <signal.h>

#define PAGESIZE	66

#ifdef TTY
#ifndef BAUDRATE
#	define BAUDRATE	B300
#endif

struct sgttyb tty;
#endif

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

#ifdef TTY
	tty.sg_ispeed = tty.sg_ospeed = BAUDRATE;
	tty.sg_erase = tty.sg_kill = -1;
	tty.sg_flags = (ANYP|XTABS|LDECCTQ);
	if (ioctl(1, TIOCSETP, (char *)&tty) < 0)
		exit (2);
#endif
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
#ifdef TTY
			if ((cp = rindex(line, '\n')) != NULL)
				*cp = '\r';
#endif
			printf("%s", line);
#ifdef SHEETFEEDER
		}
		lnumber++;
#endif
	}
	fflush (stdout);
}
