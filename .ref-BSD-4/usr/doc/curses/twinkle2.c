main() {

	reg char	*sp;
	char		*getenv();
	int		fputchar(), die();

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

	noecho();
	tputs(CL, NLINES, fputchar);
	for (;;) {
		makeboard();		/* make the board setup */
		puton('*');		/* put on '*'s */
		puton(' ');		/* cover up with ' 's */
	}
}

/*
 * fputchar defined for tputs()
 */
fputchar(c)
reg char	c; {

	putchar(c);
}

die() {

	signal(SIGINT, SIG_IGN);
	mvcur(0, COLS-1, LINES-1, 0);
	endwin();
	exit(0);
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
