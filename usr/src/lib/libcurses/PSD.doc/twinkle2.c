/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)twinkle2.c	5.1 (Berkeley) %G%";
#endif not lint

main() {

	reg char	*sp;
	char		*getenv();
	int		_putchar(), die();

	srand(getpid());		/* initialize random sequence */

	if (isatty(0)) {
	       gettmode();
	       if (sp=getenv("TERM"))
		       setterm(sp);
		signal(SIGINT, die);
	}
	else {
		printf("Need a terminal on %d\n", _tty_ch);
		exit(1);
	}
	_puts(TI);
	_puts(VS);

	noecho();
	nonl();
	tputs(CL, NLINES, _putchar);
	for (;;) {
		makeboard();		/* make the board setup */
		puton('*');		/* put on '*'s */
		puton(' ');		/* cover up with ' 's */
	}
}

/*
 * _putchar defined for tputs() (and _puts())
 */
_putchar(c)
reg char	c; {

	putchar(c);
}

puton(ch)
char	ch; {

	static int	lasty, lastx;
	reg LOCS	*lp;
	reg int		r;
	reg LOCS	*end;
	LOCS		temp;

	end = &Layout[Numstars];
	for (lp = Layout; lp < end; lp++) {
		r = rand() % Numstars;
		temp = *lp;
		*lp = Layout[r];
		Layout[r] = temp;
	}

	for (lp = Layout; lp < end; lp++)
			/* prevent scrolling */
		if (!AM || (lp->y < NLINES - 1 || lp->x < NCOLS - 1)) {
			mvcur(lasty, lastx, lp->y, lp->x);
			putchar(ch);
			lasty = lp->y;
			if ((lastx = lp->x + 1) >= NCOLS)
				if (AM) {
					lastx = 0;
					lasty++;
				}
				else
					lastx = NCOLS - 1;
		}
}
