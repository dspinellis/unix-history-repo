/* $Header: /a/guest/moore/talk/RCS/io.c,v 1.3 83/07/06 00:17:15 moore Exp $ */

/* this file contains the I/O handling and the exchange of 
   edit characters. This connection itself is established in
   ctl.c
 */

#include "talk.h"
#include <stdio.h>
#include <errno.h>
#include <sys/time.h>

#define A_LONG_TIME 10000000
#define STDIN_MASK (1<<fileno(stdin))	/* the bit mask for standard
					   input */
extern int errno;

/*
 * The routine to do the actual talking
 */

talk()
{
    register int read_template, sockt_mask;
    int read_set, nb;
    char buf[BUFSIZ];
    struct timeval wait;

    message("Connection established\007\007\007");
    current_line = 0;

    sockt_mask = (1<<sockt);

	/*
	 * wait on both the other process (sockt_mask) and 
	 * standard input ( STDIN_MASK )
	 */

    read_template = sockt_mask | STDIN_MASK;

    forever {

	read_set = read_template;

	wait.tv_sec = A_LONG_TIME;
	wait.tv_usec = 0;

	nb = select(32, &read_set, 0, 0, &wait);

	if (nb <= 0) {

		/* We may be returning from an interupt handler */

	    if (errno == EINTR) {
		read_set = read_template;
		continue;
	    } else {
		    /* panic, we don't know what happened */
		p_error("Unexpected error from select");
		quit();
	    }
	}

	if ( read_set & sockt_mask ) { 

		/* There is data on sockt */
	    nb = read(sockt, buf, sizeof buf);

	    if (nb <= 0) {
		message("Connection closed. Exiting");
		quit();
	    } else {
		display(&his_win, buf, nb);
	    }
	}
	
	if ( read_set & STDIN_MASK ) {

		/* we can't make the tty non_blocking, because
		   curses's output routines would screw up */

	    ioctl(0, FIONREAD, (struct sgttyb *) &nb);
	    nb = read(0, buf, nb);

	    display(&my_win, buf, nb);
	    write(sockt, buf, nb);	/* We might lose data here
					   because sockt is non-blocking
					 */

	}
    }
}

extern int	errno;
extern int	sys_nerr;
extern char	*sys_errlist[];

    /* p_error prints the system error message on the standard location
       on the screen and then exits. (i.e. a curses version of perror)
     */

p_error(string) 
char *string;
{
    char *sys;

    sys = "Unknown error";
    if(errno < sys_nerr) {
	sys = sys_errlist[errno];
    }


    wmove(my_win.x_win, current_line%my_win.x_nlines, 0);
    wprintw(my_win.x_win, "[%s : %s (%d)]\n", string, sys, errno);
    wrefresh(my_win.x_win);
    move(LINES-1, 0);
    refresh();
    quit();
}

    /* display string in the standard location */

message(string)
char *string;
{
    wmove(my_win.x_win, current_line%my_win.x_nlines, 0);
    wprintw(my_win.x_win, "[%s]\n", string);
    wrefresh(my_win.x_win);
}
