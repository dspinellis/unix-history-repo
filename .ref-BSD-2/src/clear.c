/* Copyright (c) 1979 Regents of the University of California */
#include <retrofit.h>
#include <sgtty.h>
/*
 * clear - clear the screen
 */

#include <stdio.h>

char	*getenv();
char	*tgetstr();
char	PC;
short	ospeed;
#undef	putchar
int	putchar();

main()
{
	char *cp = getenv("TERM");
	char clbuf[20];
	char pcbuf[20];
	char *clbp = clbuf;
	char *pcbp = pcbuf;
	char *clear;
	char buf[BUFSIZ];
	char *pc;
	struct sgttyb tty;

	gtty(1, &tty);
	ospeed = tty.sg_ospeed;
	if (cp == (char *) 0)
		exit(1);
	if (tgetent(buf, cp) != 1)
		exit(1);
	pc = tgetstr("pc", &pcbp);
	if (pc)
		PC = *pc;
	clear = tgetstr("cl", &clbp);
	if (clear)
		tputs(clear, tgetnum("li"), putchar);
	exit (clear != (char *) 0);
}
