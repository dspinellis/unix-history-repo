/*	necf.c	1.1	81/05/09	*/
#include <stdio.h>
#include <sgtty.h>
#include <signal.h>

struct sgttyb tty;

main()
{
	extern char _sobuf[BUFSIZ];
	register char c;

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGHUP, SIG_IGN);
	signal(SIGTERM, SIG_IGN);

	tty.sg_ispeed = tty.sg_ospeed = B4800;
	tty.sg_erase = tty.sg_kill = -1;
	tty.sg_flags = (ANYP|XTABS|CRMOD|FLCTRL);
	if (ioctl(1, TIOCSETP, (char *)&tty) < 0)
		exit (2);
	setbuf(stdout, _sobuf);
	printf ("\033=\r");
	while ((c = getchar()) != EOF)
		putchar (c);
	printf ("\033=\r");
	fflush (stdout);
}
