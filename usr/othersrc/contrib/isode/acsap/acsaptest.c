/* acsaptest.c - test out -lacsap */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/acsaptest.c,v 7.7 91/02/22 09:14:21 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/acsaptest.c,v 7.7 91/02/22 09:14:21 mrose Interim $
 *
 *
 * $Log:	acsaptest.c,v $
 * Revision 7.7  91/02/22  09:14:21  mrose
 * Interim 6.8
 * 
 * Revision 7.6  91/02/19  09:18:25  mrose
 * update
 * 
 * Revision 7.5  90/12/11  10:51:58  mrose
 * lock-and-load
 * 
 * Revision 7.4  90/11/04  19:14:41  mrose
 * update
 * 
 * Revision 7.3  90/07/27  08:41:44  mrose
 * update
 * 
 * Revision 7.2  90/07/09  14:30:43  mrose
 * sync
 * 
 * Revision 7.1  90/07/01  21:02:11  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:22:01  mrose
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
#define	ACSE
#include "acpkt.h"
#include "isoservent.h"
#include "tailor.h"
#include "psap.h"
#include "DSE-types.h"
#include "UNIV-types.h"


#define	NULLIE	((struct isoentity *) 0)


char   *macro2str ();
struct TSAPaddr *ta2norm ();

/*  */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    AEI	    aei;
    register struct PSAPaddr *pa;
    register struct isoentity  *ie;
    register struct isobject  *io;
    register struct isoservent  *is;

    isodetailor (argv[0], 1);
    
    argc--, argv++;
    switch (argc) {
	case 1:
	    if (strcmp (*argv, "-version") == 0) {
		printf ("%s\n", acsapversion);
		break;
	    }
	    if (!(pa = str2paddr (*argv)))
		goto you_lose;
	    printent (NULLIE, NULLAEI, pa);
	    break;

	case 2:
	case 3:
	    if (strcmp (argv[0], "-macro") == 0) {
		char   *cp;

		if ((cp  = macro2str (argv[1])) == NULL)
		    goto you_lose;
		printf ("%s\n", cp);
		break;
	    }

#undef	UFN
#ifdef	UFN
	    {
#include "quipu/ufn.h"

		ufn_notify = 1;

		(void) set_lookup_ufn (0);
	    }
#endif

	    if (!(aei = _str2aei (argv[0], *argv[1] ? argv[1] : NULLCP,
				  argv[2], isatty (fileno (stdin)), NULLCP,
				  NULLCP))) {
		fprintf (stderr, "name translation failed: %s\n", PY_pepy);
		goto you_lose;
	    }
	    printent (NULLIE, aei, NULLPA);

	    if (!(pa = aei2addr (aei))) {
		fprintf (stderr, "address translation failed\n");
		goto you_lose;
	    }
	    printent (NULLIE, NULLAEI, pa);
	    break;

	default:
	    printf ("ISO Entities Database\n");
	    while (ie = getisoentity ())
		printent (ie, oid2aei (&ie -> ie_identifier), &ie -> ie_addr);

	    printf ("\nISO Objects Database\n");
	    while (io = getisobject ())
		printobj (io);

	    printf ("\nISO Services Database\n");
	    while (is = getisoservent ())
		printsrv (is);
	    break;
    }

    exit (0);			/* NOTREACHED */

you_lose: ;
    fprintf (stderr, "no such luck\n");
    exit (1);			/* NOTREACHED */
}

/*  */

static	printent (ie, aei, pa)
register struct isoentity  *ie;
AEI	aei;
register struct PSAPaddr *pa;
{
    if (ie)
	printf ("Entity:  %s (%s)\n", ie -> ie_descriptor,
		oid2ode (&ie -> ie_identifier));

    if (aei)
	printf ("AE info: %s\n", sprintaei (aei));

    if (pa) {
	struct PSAPaddr pas;
	register struct TSAPaddr *ta = &pa -> pa_addr.sa_addr;
	PE	    pe;

	printf ("Address: %s\n", paddr2str (pa, NULLNA));

	pe = NULLPE;
	if (build_DSE_PSAPaddr (&pe, 1, NULL, NULLCP, (char *) pa) == NOTOK) {
	    printf ("build of PSAPaddr failed: %s\n", PY_pepy);
	    goto dont_touch;
	}

	bzero ((char *) &pas, sizeof pas);
	if (parse_DSE_PSAPaddr (pe, 1, NULLIP, NULLVP, (char *) &pas) ==NOTOK){
	    printf ("parse of PSAPaddr failed: %s\n", PY_pepy);
	    goto dont_touch;
	}

	(void) print_DSE_PSAPaddr (pe, 1, NULLIP, NULLVP, NULLCP);

	if (bcmp ((char *) pa, (char *) &pas, sizeof pas)) {
	    printf ("*** NOT EQUAL ***\n");
	    printf ("\told %s\n", paddr2str (pa, NULLNA));
	    printf ("\tnew %s\n", paddr2str (&pas, NULLNA));
	}

dont_touch: ;
	if (pe)
	    pe_free (pe);

	{
	    struct TSAPaddr *tz = ta2norm (ta);

	    if (tz) {
		if (bcmp ((char *) ta, (char *) tz, sizeof *tz))
		    printf ("NORM:    %s\n", taddr2str (tz));
	    }
	    else
		printf ("*** ta2norm FAILED ***\n");
	}

    }

    if (ie || aei || pa)
	printf ("\n");
}

/*  */

static	printobj (io)
register struct isobject *io;
{
    printf ("ODE: \"%s\"\nOID: %s\n\n", io -> io_descriptor,
	    sprintoid (&io -> io_identity));
}

/*  */

static	printsrv (is)
register struct isoservent *is;
{
    register int    n = is -> is_tail - is -> is_vec - 1;
    register char **ap = is -> is_vec;

    printf ("ENT: \"%s\" PRV: \"%s\" SEL: %s\n",
	    is -> is_entity, is -> is_provider,
	    sel2str (is -> is_selector, is -> is_selectlen, 1));

    for (; n >= 0; ap++, n--)
	printf ("\t%d: \"%s\"\n", ap - is -> is_vec, *ap);
    printf ("\n");
}
