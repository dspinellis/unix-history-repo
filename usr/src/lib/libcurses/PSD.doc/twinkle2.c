/*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)twinkle2.c	6.2 (Berkeley) %G%";
#endif /* not lint */

extern int	_putchar();

main()
{
	reg char	*sp;

	srand(getpid());		/* initialize random sequence */

	if (isatty(0)) {
	       gettmode();
	       if ((sp = getenv("TERM")) != NULL)
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

puton(ch)
char	ch;
{
	reg LOCS	*lp;
	reg int		r;
	reg LOCS	*end;
	LOCS		temp;
	static int	lasty, lastx;

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
