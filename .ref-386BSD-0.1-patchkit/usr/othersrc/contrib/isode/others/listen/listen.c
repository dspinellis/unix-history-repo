/* listen.c -- responder for listen demo */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/listen/RCS/listen.c,v 7.1 91/02/22 09:27:25 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/listen/RCS/listen.c,v 7.1 91/02/22 09:27:25 mrose Interim $
 *
 *
 * $Log:	listen.c,v $
 * Revision 7.1  91/02/22  09:27:25  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:00:18  mrose
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
    int	    secs,
	    vecp;
    char   *vec[4];
    struct TSAPdisconnect tds;
    struct TSAPdisconnect *td = &tds;
    struct SSAPstart sss;
    register struct SSAPstart *ss = &sss;
    struct SSAPdata sxs;
    register struct SSAPdata   *sx = &sxs;
    struct SSAPindication sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort *sa = &si -> si_abort;
    AEI	    aei;
    struct PSAPaddr *pa;
    struct sblk incoming;
    SB	    sbi = &incoming;

    reportailor (argv[0]);

    if (argc > 2)
	adios (NULLCP, "usage: %s [secs]", argv[0]);

    secs = (argc == 2) ? atoi(argv[1]) : 15; /* Wait 15s after release
						by default */

    if ((aei = str2aei (PLocalHostName(), mycontext)) == NULLAEI)
	adios (NULLCP, "%s-%s: unknown application-entity",
		PLocalHostName (), mycontext);

    if ((pa = aei2addr (aei)) == NULLPA)
	adios (NULLCP, "address translation failed");

    if (TNetListen (&pa -> pa_addr.sa_addr, td) == NOTOK)
	if (td -> td_cc > 0)
	    adios (NULLCP, "TNetListen: [%s] %*.*s", TErrString (td -> td_reason),
		   td -> td_cc, td -> td_cc, td -> td_data);
	else
	    adios (NULLCP, "TNetListen: [%s]", TErrString (td -> td_reason));

	/* now wait for incoming call */
    for (;;) {
	if (TNetAccept (&vecp, vec, 0, NULLFD, NULLFD, NULLFD, NOTOK, td)
		== NOTOK) {
	    if (td -> td_cc > 0)
		adios (NULLCP, "TNetAccept: [%s] %*.*s", TErrString (td -> td_reason),
			td -> td_cc, td -> td_cc, td -> td_data);
	    else
		adios (NULLCP, "TNetAccept: [%s]", TErrString (td -> td_reason));
	}

	if (vecp > 0)
	    break;
    }

    if (SInit (vecp, vec, ss, si) == NOTOK)
	adios (NULLCP, "S-CONNECT.INDICATION: %s", SErrString (sa -> sa_reason));
    advise (LLOG_NOTICE, NULLCP,
	    "S-CONNECT.INDICATION: <%d, %s, %s, %s, %s, %ld, %d>",
	    ss -> ss_sd, sprintref (&ss -> ss_connect),
	    saddr2str (&ss -> ss_calling), saddr2str (&ss -> ss_called),
	    sprintb (ss -> ss_requirements, RMASK), ss -> ss_isn,
	    ss -> ss_ssdusize);

	/* stop listening, we have what we want */
    (void) TNetClose (NULLTA, td);

    bzero ((char *) sbi, sizeof *sbi);
    sbi -> sb_sd = ss -> ss_sd;
    sbi -> sb_connect = ss -> ss_connect;	/* struct copy */    
    sbi -> sb_requirements = ss -> ss_requirements & SR_BASUBSET;
    sbi -> sb_settings = ss -> ss_settings;
#define dotoken(requires,shift,bit,type) \
{ \
    if (sbi -> sb_requirements & requires) \
	switch (sbi -> sb_settings & (ST_MASK << shift)) { \
	    case ST_CALL_VALUE << shift: \
		sbi -> sb_settings &= ~(ST_MASK << shift); \
		sbi -> sb_settings |= ST_INIT_VALUE << shift; \
		break; \
 \
	    case ST_INIT_VALUE: \
		break; \
 \
	    case ST_RESP_VALUE: \
		sbi -> sb_owned |= bit; \
		break; \
 \
	    default: \
		adios (NULLCP, "%s token: reserved", type); \
		break; \
	} \
}
    dotokens ();
#undef	dotoken
    sbi -> sb_ssn = sbi -> sb_isn = ss -> ss_isn;

    SSFREE (ss);

    if (SConnResponse (sbi -> sb_sd, &sbi -> sb_connect, NULLSA, SC_ACCEPT,
	    sbi -> sb_requirements, sbi -> sb_settings, sbi -> sb_isn,
	    NULLCP, 0, si) == NOTOK)
	adios (NULLCP, "S-CONNECT.RESPONSE: %s", SErrString (sa -> sa_reason));

/* do work here */

    switch (SReadRequest (sbi -> sb_sd, sx, secs, si)) {
	case NOTOK:
	    adios (NULLCP, "S-READ.REQUEST: %s", SErrString (sa -> sa_reason));

	case OK:
	    adios (NULLCP, "not expecting DATA indication 0x%x", sx -> sx_type);

	case DONE:
	    if (si -> si_type != SI_FINISH)
		adios (NULLCP, "not expecting indication 0x%x", si -> si_type);
	    if (SRelResponse (sbi -> sb_sd, SC_ACCEPT, NULLCP, 0, si) == NOTOK)
		adios (NULLCP, "S-RELEASE.RESPONSE: %s", SErrString (sa -> sa_reason));
	    break;
    }

    exit (0);
}
