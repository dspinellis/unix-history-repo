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
static char sccsid[] = "@(#)life.c	6.2 (Berkeley) %G%";
#endif /* not lint */

# include	<curses.h>
# include	<signal.h>

/*
 *	Run a life game.  This is a demonstration program for
 * the Screen Updating section of the -lcurses cursor package.
 */

typedef struct lst_st {			/* linked list element */
	int		y, x;		/* (y, x) position of piece */
	struct lst_st	*next, *last;	/* doubly linked */
} LIST;

LIST	*Head;			/* head of linked list */

int	die();

main(ac, av)
int	ac;
char	*av[];
{
	evalargs(ac, av);		/* evaluate arguments */

	initscr();			/* initialize screen package */
	signal(SIGINT, die);		/* set to restore tty stats */
	cbreak();			/* set for char-by-char */
	noecho();			/*	input */
	nonl();				/* for optimization */

	getstart();			/* get starting position */
	for (;;) {
		prboard();		/* print out current board */
		update();		/* update board position */
	}
}

/*
 * This is the routine which is called when rubout is hit.
 * It resets the tty stats to their original values.  This
 * is the normal way of leaving the program.
 */
die()
{
	signal(SIGINT, SIG_IGN);		/* ignore rubouts */
	mvcur(0, COLS - 1, LINES - 1, 0);	/* go to bottom of screen */
	endwin();				/* set terminal to good state */
	exit(0);
}

/*
 * Get the starting position from the user.  They keys u, i, o, j, l,
 * m, ,, and . are used for moving their relative directions from the
 * k key.  Thus, u move diagonally up to the left, , moves directly down,
 * etc.  x places a piece at the current position, " " takes it away.
 * The input can also be from a file.  The list is built after the
 * board setup is ready.
 */
getstart()
{
	reg char	c;
	reg int		x, y;
	auto char	buf[100];

	box(stdscr, '|', '_');		/* box in the screen */
	move(1, 1);			/* move to upper left corner */

	for (;;) {
		refresh();		/* print current position */
		if ((c = getch()) == 'q')
			break;
		switch (c) {
		  case 'u':
		  case 'i':
		  case 'o':
		  case 'j':
		  case 'l':
		  case 'm':
		  case ',':
		  case '.':
			adjustyx(c);
			break;
		  case 'f':
			mvaddstr(0, 0, "File name: ");
			getstr(buf);
			readfile(buf);
			break;
		  case 'x':
			addch('X');
			break;
		  case ' ':
			addch(' ');
			break;
		}
	}

	if (Head != NULL)			/* start new list */
		dellist(Head);
	Head = malloc(sizeof (LIST)); 

	/*
	 * loop through the screen looking for 'x's, and add a list
	 * element for each one
	 */
	for (y = 1; y < LINES - 1; y++)
		for (x = 1; x < COLS - 1; x++) {
			move(y, x);
			if (inch() == 'x')
				addlist(y, x);
		}
}

/*
 * Print out the current board position from the linked list
 */
prboard() {

	reg LIST	*hp;

	erase();			/* clear out last position */
	box(stdscr, '|', '_');		/* box in the screen */

	/*
	 * go through the list adding each piece to the newly
	 * blank board
	 */
	for (hp = Head; hp; hp = hp->next)
		mvaddch(hp->y, hp->x, 'X');

	refresh();
}
