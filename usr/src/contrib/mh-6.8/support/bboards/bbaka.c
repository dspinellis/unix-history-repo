/* bbaka.c - generate an alias list for BBoards */
#ifndef	lint
static char ident[] = "@(#)$Id: bbaka.c,v 2.4 1992/12/15 00:20:22 jromine Exp $";
#endif	lint

#include "../h/strings.h"
#include "../zotnet/bboards.h"
#include <stdio.h>
#include "../zotnet/mts.h"


static int  systemn = 0;
static char domain[BUFSIZ] = "";

static	aka(), process();

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    struct bboard  *bb;

    if (argc > 1 && strcmp (argv[1], "system") == 0)
	systemn++;

    mts_init (argv[0]);
    make_lower (domain, bb_domain);

    (void) setbbent (SB_STAY);
    while (bb = getbbent ())
	process (bb);
    (void) endbbent ();

    exit (0);
}

/*  */

static  process (bb)
struct bboard  *bb;
{
    char  **ap,
            bbaddr[BUFSIZ],
            buffer[BUFSIZ],
            ldaddr[BUFSIZ],
            result[BUFSIZ];

    if (systemn) {
#ifndef	MHMTS
	(void) sprintf (bbaddr, "%s@bboards", bb -> bb_name, LocalName ());
#else	/* MHMTS */
	(void) strcpy (bbaddr, BBOARDS);
#endif	/* MHMTS */

	if (strcmp (bb -> bb_name, bb -> bb_addr) == 0)
	    aka (bb -> bb_name, bbaddr);
	if (domain[0]) {
	    (void) sprintf (buffer, "%s-%s", domain, bb -> bb_name);
	    if (bb -> bb_relay && *bb -> bb_relay) {
		(void) sprintf (result, "%s%s@%s", DISTADR, bb -> bb_name,
			bb -> bb_relay);
		aka (buffer, result);
	    }
	    else
		aka (buffer, bbaddr);
	}
	(void) sprintf (buffer, "%s%s", DISTADR, bb -> bb_name);
	aka (buffer, bbaddr);

	(void) sprintf (ldaddr, "%s@%s", *bb -> bb_leader, LocalName ());
	for (ap = bb -> bb_leader, ap++; *ap; ap++)
	    (void) sprintf (ldaddr + strlen (ldaddr), ",%s@%s", *ap,
		    LocalName ());
	if (domain[0]) {
	    if (bb -> bb_relay && *bb -> bb_relay
		    && strcmp (*bb -> bb_leader, BBOARDS) == 0
		    && --ap == bb -> bb_leader)
		(void) sprintf (ldaddr, "%s-%s-request@%s",
			domain, bb -> bb_name, bb -> bb_relay);
	}
	if (strcmp (bb -> bb_request, *bb -> bb_leader) == 0) {
	    (void) sprintf (buffer, "%s-request", bb -> bb_name);
	    aka (buffer, ldaddr);
	}
	if (domain[0]) {
	    (void) sprintf (buffer, "%s-%s-request", domain, bb -> bb_name);
	    if (bb -> bb_relay && *bb -> bb_relay) {
		(void) sprintf (result, "%s@%s", buffer, bb -> bb_relay);
		aka (buffer, result);
	    }
	    else
		aka (buffer, ldaddr);
	}
	(void) sprintf (buffer, "local-%s-request", bb -> bb_name);
	aka (buffer, ldaddr);
    }
    else {
	if (strcmp (bb -> bb_name, bb -> bb_addr))
	    aka (bb -> bb_name, bb -> bb_addr);

	(void) sprintf (buffer, "%s-request", bb -> bb_name);
	if (strcmp (bb -> bb_request, *bb -> bb_leader))
	    aka (buffer, bb -> bb_request);
    }
}

/*  */

static  aka (field, value)
char   *field,
       *value;
{
#ifndef	MHMTS
    printf ("%s: %s\n", field, value);
#else	/* MHMTS */
    printf ("%s%s %s\n", field, systemn ? ":" : ";", value);
#endif	/* MHMTS */
}
