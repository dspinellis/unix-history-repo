/* isore.c - help out ISODE TSAP programs */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/support/RCS/isore.c,v 7.1 91/02/22 09:46:46 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/support/RCS/isore.c,v 7.1 91/02/22 09:46:46 mrose Interim $
 *
 *
 * $Log:	isore.c,v $
 * Revision 7.1  91/02/22  09:46:46  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:27:34  mrose
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
#include <signal.h>
#include "manifest.h"

/*    MAIN */

SFD	EMTser ();


/* ARGSUSED */

main (argc, argv, envp)
int     argc;
char  **argv,
      **envp;
{
    int	    fd,
	    mask,
    	    nfds,
    	    ppid;
    fd_set  ifds,
    	    rfds;

    if (argc != 4)
	exit (1);
    if ((nfds = atoi (argv[1])) < 0
	    || sscanf (argv[2], "0x%x", &mask) != 1
	    || (ppid = atoi (argv[3])) < 0)
	exit (2);

    FD_ZERO (&rfds);
    for (fd = 0; fd < nfds; fd++)
	if (mask & (1 << fd))
	    FD_SET (fd, &rfds);

    (void) signal (SIGEMT, EMTser);

    for (;;) {
	ifds = rfds;
	switch (xselect (nfds, &ifds, NULLFD, NULLFD, NOTOK)) {
	    case NOTOK: 
		fprintf (stderr, "NOTOK\n");
		break;

	    case OK: 
		fprintf (stderr, "OK\n");
		break;

	    default: 
		(void) kill (ppid, SIGEMT);
		sigpause (0);
		break;
	}
    }
}

/*    SIGNALS */

/* ARGSUSED */

static  SFD EMTser (sig, code, sc)
int	sig;
long	code;
struct sigcontext *sc;
{
#ifndef	BSDSIGS
    (void) signal (SIGEMT, EMTser);
#endif
}
