/* socket.c - dish -pipe support */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/dish/RCS/socket.c,v 7.1 91/02/22 09:30:30 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/uips/dish/RCS/socket.c,v 7.1 91/02/22 09:30:30 mrose Interim $
 *
 *
 * $Log:	socket.c,v $
 * Revision 7.1  91/02/22  09:30:30  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:36:06  mrose
 * Release 6.0
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
#include "quipu/util.h"
#include "tailor.h"
#include "general.h"

#ifdef SOCKETS   	/* USE INTERNET SOCKETS */

#include "internet.h"

get_dish_sock (isock, pid, islocal)
struct sockaddr_in *isock;
int	pid,
	islocal;
{
    int    myppid;
char * getenv ();
char * ptr, * prnt;
static char buffer [BUFSIZ];
static char parent [BUFSIZ];
int     portno;
char   *dp;
register struct hostent *hp;

	if ((myppid = pid) == 0)
	    myppid = getppid ();

        if (pid != 0 || (ptr = getenv ("DISHPROC")) == NULLCP) {
		char	*cp;

		portno = (myppid & 0xffff) | 0x8000;
		if (!islocal) {
		    if ((hp = gethostbystring (cp = getlocalhost ()))
			    == NULL) {
            		(void) fprintf (stderr,"%s: unknown host", cp);
			return (-1);
		    }
		    (void) sprintf (buffer, "%s %d",
				    inet_ntoa (*(struct in_addr *)
								hp -> h_addr),
				    portno);
		}
		else
		    (void) sprintf (buffer, "127.0.0.1 %d", portno);

		(void) setenv ("DISHPROC", ptr = buffer);
        }

        if (pid !=0 || (prnt = getenv ("DISHPARENT")) == NULLCP) {
		(void) sprintf (parent, "%d", myppid);
		(void) setenv ("DISHPARENT", prnt = parent);
	}


	if (sscanf (prnt, "%d", &pid) != 1) {
		(void) fprintf (stderr,"DISHPARENT malformed");
		return (-1);
	}

	if ((dp = index (ptr, ' ')) == NULLCP || sscanf (dp + 1, "%d", &portno) != 1) {
		(void) fprintf (stderr,"DISHPROC malformed");
		return (-1);
	}
	*dp = NULL;

	if ((hp = gethostbystring (ptr)) == NULL) {
		(void) fprintf (stderr,"%s: unknown host in DISHPROC", ptr);
		return (-1);
	}
	*dp = ' ';

	bzero ((char *) isock, sizeof *isock);
	isock -> sin_family = hp -> h_addrtype;
	isock -> sin_port = htons ((u_short) portno);
	inaddr_copy (hp, isock);

	return (0);

}

#else	/* USE UNIX NAMED PIPES */


void dummy ()
{
;
}

#endif
