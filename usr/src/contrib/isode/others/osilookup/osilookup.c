/* osilookup.c - convert entry in /etc/osi.hosts to isoentities format */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/osilookup/RCS/osilookup.c,v 7.1 91/02/22 09:28:48 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/osilookup/RCS/osilookup.c,v 7.1 91/02/22 09:28:48 mrose Interim $
 *
 * Contributed by John A. Scott, the MITRE Corporation
 *
 * N.B.:	I whipped up this code quickly to fill a need I had.  I
 *		do not, it any way, shape, or form, warrant its output.
 *
 *
 * $Log:	osilookup.c,v $
 * Revision 7.1  91/02/22  09:28:48  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:01:00  mrose
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


#include "config.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#ifndef	SUNLINK_6_0
#include <sys/ieee802.h>
#else
#include <net/if_ieee802.h>
#endif
#include <netosi/osi.h>
#include <netosi/osi_addr.h>
#include <sunosi/mapds_user.h>

/*  */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int	    len,
	    paddr_type;
    char   *prefix,
	   *service,
	    buf[BUFSIZ],
	    buf2[BUFSIZ];
    OSI_ADDR p_addr;

    if (argc < 2) {
	fprintf (stderr,"usage: %s host [service]\n", argv[0]);
	exit (0);
    }
    service = (argc > 2) ? argv[2] : "FTAM";

    /* SUNLink OSI directory lookup */
    mds_lookup (argv[1], service, &p_addr);

    /* SUNLink function to slice out SAP bytes from full address */
    paddr_type = 0;
    if ((len = osi_get_sap (&p_addr, buf, sizeof buf, OSI_NSAP, &paddr_type))
	    <= 0) {
	fprintf (stderr, "no entry for %s %s\n", argv[1], service);
	exit (1);
    }

    buf2[explode (buf2, (u_char *) buf, len)]= NULL;
    switch (paddr_type) {
	case AF_NBS:
	    prefix = "49";
	    break;

	case AF_OSINET:
	    prefix = "470004";
	    break;

	default:
	    prefix = "";
	    break;
    }
    printf ("\t\t\t\tNS+%s%s\n\n", prefix, buf2);

    exit (0);
}

/* so we don't have to load libisode.a */

static char nib2hex[0x10] = {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
};


static int  explode (a, b, n)
register char  *a;
register u_char *b;
register int    n;
{
    register int    i;
    register u_char c;

    for (i = 0; i < n; i++) {
	c = *b++;
	*a++ = nib2hex[(c & 0xf0) >> 4];
	*a++ = nib2hex[(c & 0x0f)];
    }
    *a = NULL;

    return (n * 2);
}
