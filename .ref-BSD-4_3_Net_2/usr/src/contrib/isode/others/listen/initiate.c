/* initiate.c -- initiator for listen demo */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/listen/RCS/initiate.c,v 7.1 91/02/22 09:27:24 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/listen/RCS/initiate.c,v 7.1 91/02/22 09:27:24 mrose Interim $
 *
 *
 * $Log:	initiate.c,v $
 * Revision 7.1  91/02/22  09:27:24  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:00:17  mrose
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
#include "listen.h"

/*  */

static char *mycontext = "isode listen demo";

/*  */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    register struct SSAPaddr *sz;
    struct SSAPconnect scs;
    register struct SSAPconnect *sc = &scs;
    struct SSAPdata sxs;
    register struct SSAPdata   *sx = &sxs;
    struct SSAPrelease  srs;
    register struct SSAPrelease *sr = &srs;
    struct SSAPindication sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort *sa = &si -> si_abort;
    AEI	    aei;
    struct PSAPaddr *pa;
    struct sblk outgoing;
    SB	    sbo = &outgoing;

    reportailor (argv[0]);

    if (argc != 2)
	adios (NULLCP, "usage: %s \"host\"", argv[0]);

    if ((aei = str2aei (argv[1], mycontext)) == NULLAEI)
	adios (NULLCP, "%s-%s: unknown application-entity",
		argv[1], mycontext);
    if ((pa = aei2addr (aei)) == NULLPA)
	adios (NULLCP, "address translation failed");
    sz = &pa -> pa_addr;

    bzero ((char *) sbo, sizeof *sbo);
    sbo -> sb_requirements = SR_BASUBSET;
    sbo -> sb_settings = 0;
#define dotoken(requires,shift,bit,type) \
{ \
    if (sbo -> sb_requirements & requires) \
	    sbo -> sb_settings |= ST_INIT_VALUE << shift; \
}
    dotokens ();
#undef  dotoken
    sbo -> sb_isn = SERIAL_NONE;

    if (SConnRequest (&sbo -> sb_connect, NULLSA, sz, sbo -> sb_requirements,
	    sbo -> sb_settings, sbo -> sb_isn, NULLCP, 0, NULLQOS, sc, si)
	    == NOTOK)
	adios (NULLCP, "S-CONNECT.REQUEST: %s", SErrString (sa -> sa_reason));
    if (sc -> sc_result != SC_ACCEPT)
	adios (NULLCP, "connection rejected by peer: %s",
		SErrString (sc -> sc_result));
    advise (LLOG_NOTICE, NULLCP,
	    "S-CONNECT.CONFIRMATION: <%d, %s, %s, %s, %ld, %d>",
	    sc -> sc_sd, sprintref (&sc -> sc_connect),
	    saddr2str (&sc -> sc_responding),
	    sprintb (sc -> sc_requirements, RMASK), sc -> sc_isn,
	    sc -> sc_ssdusize);

    sbo -> sb_sd = sc -> sc_sd;
    sbo -> sb_requirements = sc -> sc_requirements;
    sbo -> sb_settings = sc -> sc_settings;
#define dotoken(requires,shift,bit,type) \
{ \
    if (sbo -> sb_requirements & requires) \
	if ((sbo -> sb_settings & (ST_MASK << shift)) == ST_INIT_VALUE) \
	    sbo -> sb_owned |= bit; \
}
    dotokens ();
#undef  dotoken
    sbo -> sb_ssn = sbo -> sb_isn = sc -> sc_isn;

    SCFREE (sc);

/* do work here */

    if (SRelRequest (sbo -> sb_sd, NULLCP, 0, NOTOK, sr, si) == NOTOK)
	adios (NULLCP, "S-RELEASE.REQUEST: %s", SErrString (sa -> sa_reason));
    if (sr -> sr_affirmative) {
	exit(0);
	/* NOTREACHED */
    } else {
        switch (SReadRequest (sbo -> sb_sd, sx, NOTOK, si)) {
	    case NOTOK:
	        adios (NULLCP, "S-READ.REQUEST: %s", SErrString (sa -> sa_reason));
    
	    case OK:
	        adios (NULLCP, "not expecting DATA indication 0x%x", sx -> sx_type);
    
	    case DONE:
	        if (si -> si_type != SI_FINISH)
		    adios (NULLCP, "not expecting indication 0x%x", si -> si_type);
	        if (SRelResponse (sbo -> sb_sd, SC_ACCEPT, NULLCP, 0, si)
			== NOTOK)
		    adios (NULLCP, "S-RELEASE.RESPONSE: %s",
			   SErrString (sa -> sa_reason));
	        break;
        }
    }

    exit (1);
}
