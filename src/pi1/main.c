/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"

#define	INTR	2

int	onintr();

#ifdef DEBUG
main(argc)
	int argc;
#else
main()
#endif
{
	int intr;

#ifdef DEBUG
	hp21mx = argc > 1;
#endif
	intr = signal(INTR, 1);
	if (intr == 0)
		signal(INTR, onintr);
#ifdef DEBUG
	dprintf("PI1 initialized\n");
#endif
	yymain();
	/* no return */
}

/*
 * Buffer for putchar
 */
char	pcbuf[128];
char	*pcbp pcbuf;

/*
 * Line buffered putchar for pi.
 */
putchar(c)
	char c;
{

	*pcbp++ = c;
 	if (c == '\n' || pcbp == &pcbuf[sizeof pcbuf-1]) { 
		write(1, &pcbuf, pcbp-pcbuf);
		pcbp = pcbuf;
	}
}

char	ugh[]	"Fatal error in pi1\n";
/*
 * Exit from the Pascal system.
 */
pexit(c)
	int c;
{

	if (c == DIED)
		write(2, ugh, sizeof ugh);
	exit(c);
}

onintr()
{

	signal(2, 1);
	pexit(NOSTART);
}

/*
 * Get an error message from the error message file
 */
geterr(seekpt, buf)
	int seekpt;
	char *buf;
{

	if (seek(efil, seekpt, 0) || read(efil, buf, 256) <= 0)
		perror(errfile), pexit(DIED);
}
