/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek and Darren F. Provine.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tetris.c	5.2 (Berkeley) %G%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1992 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

/*
 * Tetris (or however it is spelled).
 */

#include <sys/time.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "input.h"
#include "scores.h"
#include "screen.h"
#include "tetris.h"

/*
 * Set up the initial board.
 * The bottom display row is completely set,
 * along with another (hidden) row underneath that.
 * Also, the left and right edges are set.
 */
static void
setup_board()
{
	register int i;
	register cell *p;

	p = board;
	for (i = B_SIZE; i; i--)
#ifndef mips
		*p++ = i <= (2 * B_COLS) || (i % B_COLS) < 2;
#else /* work around compiler bug */
		*p++ = i <= (2 * B_COLS) || (i % B_COLS) < 2 ? 1 : 0;
#endif
}

/*
 * Elide any full active rows.
 */
static void
elide()
{
	register int i, j, base;
	register cell *p;

	for (i = A_FIRST; i < A_LAST; i++) {
		base = i * B_COLS + 1;
		p = &board[base];
		for (j = B_COLS - 2; *p++ != 0;) {
			if (--j <= 0) {
				/* this row is to be elided */
				bzero(&board[base], B_COLS - 2);
				scr_update();
				tsleep();
				while (--base != 0)
					board[base + B_COLS] = board[base];
				scr_update();
				tsleep();
				break;
			}
		}
	}
}

int
main(argc, argv)
	int argc;
	char **argv;
{
	register int pos, c;
	register struct shape *curshape;
	register char *keys;
	register int level = 2;
	char key_write[6][10];
	int i, j;

	keys = "jkl pq";

	if (argc > 3)
		goto usage;

	while (argc-- >= 2)
		switch (argv[argc][0]) {

		case '-':
			keys = argv[argc];
			keys++;
			if (strlen(keys) < 6) {
	usage:
				(void)fprintf(stderr,
				    "usage: %s [level] [-keys]\n",
				    argv[0]);
				exit(1);
			}
			break;

		default:
			level = atoi(argv[argc]);
			if (level < MINLEVEL) {
				showscores(level);
				exit(0);
			}
			if (level > MAXLEVEL)
				level = MAXLEVEL;
		}

	fallrate = 1000000 / level;

	for (i = 0; i <= 5; i++) {
		for (j = i+1; j <= 5; j++) {
			if (keys[i] == keys[j]) {
				(void)fprintf(stderr,
				    "%s: Duplicate command keys specified.\n",
				    argv[0]);
				exit (1);
			}
		}
		if (keys[i] == ' ')
			strcpy(key_write[i], "<space>");
		else {
			key_write[i][0] = keys[i];
			key_write[i][1] = '\0';
		}
	}

	sprintf(key_msg,
"%s - left   %s - rotate   %s - right   %s - drop   %s - pause   %s - quit",
		key_write[0], key_write[1], key_write[2], key_write[3],
		key_write[4], key_write[5]);

	scr_init();
	setup_board();

	srandom(getpid());
	scr_set();

	pos = A_FIRST*B_COLS + (B_COLS/2)-1;
	curshape = randshape();

	scr_msg(key_msg, 1);

	for (;;) {
		place(curshape, pos, 1);
		scr_update();
		place(curshape, pos, 0);
		c = tgetchar();
		if (c < 0) {
			/*
			 * Timeout.  Move down if possible.
			 */
			if (fits_in(curshape, pos + B_COLS)) {
				pos += B_COLS;
				continue;
			}

			/*
			 * Put up the current shape `permanently',
			 * bump score, and elide any full rows.
			 */
			place(curshape, pos, 1);
			score++;
			elide();

			/*
			 * Choose a new shape.  If it does not fit,
			 * the game is over.
			 */
			curshape = randshape();
			pos = A_FIRST*B_COLS + (B_COLS/2)-1;
			if (!fits_in(curshape, pos))
				break;
			continue;
		}

		/*
		 * Handle command keys.
		 */
		if (c == keys[5]) {
			/* quit */
			break;
		}
		if (c == keys[4]) {
			static char msg[] =
			    "paused - press RETURN to continue";

			place(curshape, pos, 1);
			do {
				scr_update();
				scr_msg(key_msg, 0);
				scr_msg(msg, 1);
				(void) fflush(stdout);
			} while (rwait((struct timeval *)NULL) == -1);
			scr_msg(msg, 0);
			scr_msg(key_msg, 1);
			place(curshape, pos, 0);
			continue;
		}
		if (c == keys[0]) {
			/* move left */
			if (fits_in(curshape, pos - 1))
				pos--;
			continue;
		}
		if (c == keys[1]) {
			/* turn */
			struct shape *new = &shapes[curshape->rot];

			if (fits_in(new, pos))
				curshape = new;
			continue;
		}
		if (c == keys[2]) {
			/* move right */
			if (fits_in(curshape, pos + 1))
				pos++;
			continue;
		}
		if (c == keys[3]) {
			/* move to bottom */
			while (fits_in(curshape, pos + B_COLS)) {
				pos += B_COLS;
				score++;
			}
			continue;
		}
		if (c == '\f')
			scr_clear();
	}

	scr_clear();
	scr_end();

	(void)printf("Your score:  %d point%s  x  level %d  =  %d\n",
	    score, score == 1 ? "" : "s", level, score * level);
	savescore(level);

	printf("\nHit RETURN to see high scores, ^C to skip.\n");

	while ((i = getchar()) != '\n')
		if (i == EOF)
			break;

	showscores(level);

	exit(0);
}
