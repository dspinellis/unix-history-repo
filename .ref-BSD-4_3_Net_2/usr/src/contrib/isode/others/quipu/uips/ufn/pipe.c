/* pipe.c - ufn talks to dish */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/ufn/RCS/pipe.c,v 7.1 91/02/22 09:33:13 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/uips/ufn/RCS/pipe.c,v 7.1 91/02/22 09:33:13 mrose Interim $
 *
 *
 * $Log:	pipe.c,v $
 * Revision 7.1  91/02/22  09:33:13  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/06/13  18:52:42  mrose
 * *** empty log message ***
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */



#include <stdio.h>
#include <signal.h>
#include "internet.h"

/*    DISH */

int	dish (command)
char   *command;
{
    int	    cc,
	    n;
    char    buffer[BUFSIZ];
    static int sd;
    static int dish_running = NOTOK;
    static struct sockaddr_in sin;

    if (dish_running == NOTOK) {
	if (get_dish_sock (&sin, 0, 1) != 0) {
		(void) fprintf (stderr,"can't get dish socket\n");
bye_bye:;
		(void) fprintf (stderr,"Can't invoke '%s'\n",command);
		exit (-1);
	}

	dish_running = OK;
    }

    if ((sd = start_tcp_client ((struct sockaddr_in *) 0, 0)) == NOTOK) {
	(void) fprintf (stderr,"Unable to start tcp client\n");
	goto bye_bye;
    }
    if (join_tcp_server (sd, &sin) == NOTOK) {
	(void) close_tcp_socket (sd);
	(void) fprintf (stderr,"unable to join to tcp socket\n");
	goto bye_bye;
    }

    n = send (sd, command, cc = strlen (command), 0);

    if (n != cc) {
	    (void) fprintf (stderr,"write to DUA failed\n");
	    goto bye_bye;
    }

    for (;;) {
	if ((cc = recv (sd, buffer, sizeof buffer - 1, 0)) == NOTOK) {
	    (void) fprintf (stderr,"read from DUA failed\n");
	    goto bye_bye;
	}

	buffer[cc] = NULL;
	if (cc == OK) 
	    break;

	switch (buffer[0]) {
	    case '1':
	    case '2':
	    case '3':
		(void) write (1,buffer + 1, cc - 1);
			    
		while ((cc = recv (sd, buffer, sizeof buffer - 1, 0)) > OK)
			(void) write (1,buffer + 1, cc - 1);

		break;

	    default:
		(void) fprintf (stderr, "Dish is asking difficult questions (%s)!!!",buffer + 1);
		break;
	}
	break;
    }

    (void) close_tcp_socket (sd);
}

