.\" Copyright (c) 1992 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"     @(#)ex1.c	1.2 (Berkeley) %G%
.\"
#include <sys/types.h>
#include <curses.h>
#include <stdio.h>
#include <signal.h>


#define YSIZE 10 
#define XSIZE 20

int quit();

main()
{
	int i, j, c;
	size_t len;
	char id[100];
	FILE *fp;
	char *s;

	initscr();			/* Always call initscr() first */
	signal(SIGINT, quit);		/* Make sure wou have a 'cleanup' fn */
	crmode();			/* We want cbreak mode */
	noecho();			/* We want to have control of chars */
	delwin(stdscr);			/* Create our own stdscr */
	stdscr = newwin(YSIZE, XSIZE, 10, 35); 
	flushok(stdscr, TRUE);		/* Enable flushing of stdout */
	scrollok(stdscr, TRUE);		/* Enable scrolling */
	erase();			/* Initially, clear the screen */

	standout();
	move(0,0);
	while (1) {
		c = getchar();
		switch(c) {
		case 'q':		/* Quit on 'q' */
			quit();
			break;
		case 's':		/* Go into standout mode on 's' */
			standout();
			break;
		case 'e':		/* Exit standout mode on 'e' */
			standend();
			break;
		case 'r':		/* Force a refresh on 'r' */
			wrefresh(curscr);
			break;
		default:		/* By default output the character */
			addch(c);
			refresh();
		}
	}
}


int
quit()
{
	erase();		/* Terminate by erasing the screen */
	refresh();
	endwin();		/* Always end with endwin() */
	delwin(curscr);		/* Return storage */
	delwin(stdscr);
	putchar('\n');
	exit(0);
}

				
	
	
