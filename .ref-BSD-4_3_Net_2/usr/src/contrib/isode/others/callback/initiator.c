/* initiator.c -- initiator for callback demo */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/callback/RCS/initiator.c,v 7.1 91/02/22 09:26:38 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/callback/RCS/initiator.c,v 7.1 91/02/22 09:26:38 mrose Interim $
 *
 *
 * $Log:	initiator.c,v $
 * Revision 7.1  91/02/22  09:26:38  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:58:11  mrose
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
#include "callback.h"

/*  */

static char *mycontext = "isode callback demo";

/*  */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int	    vecp;
    char   *taddr,
	   *vec[4];
    struct TSAPaddr *ta;
    struct TSAPdisconnect tds;
    struct TSAPdisconnect *td = &tds;
    register struct SSAPaddr *sz;
    struct SSAPconnect scs;
    register struct SSAPconnect *sc = &scs;
    struct SSAPstart sss;
    register struct SSAPstart *ss = &sss;
    struct SSAPdata sxs;
    register struct SSAPdata   *sx = &sxs;
    struct SSAPrelease  srs;
    register struct SSAPrelease *sr = &srs;
    struct SSAPindication sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort *sa = &si -> si_abort;
    AEI	    aei;
    struct PSAPaddr *pa;
    struct sblk incoming,
		outgoing;
    SB	    sbi = &incoming,
	    sbo = &outgoing;

    reportailor (argv[0]);

    if (argc != 2)
	adios (NULLCP, "usage: %s \"host\"", argv[0]);

/* really should postpone this until after the session connect so we
   can bind to the same network address (in the case of a multi-homed ES) */
    if ((aei = str2aei (PLocalHostName (), mycontext)) == NULLAEI)
	adios (NULLCP, "%s-%s: unknown application-entity",
	       PLocalHostName (), mycontext);
    if ((pa = aei2addr (aei)) == NULLPA)
	adios (NULLCP, "address translation failed");
    ta = &pa -> pa_addr.sa_addr;
    if (TNetUnique (ta, td) == NOTOK) {
	if (td -> td_cc > 0)
	    adios (NULLCP, "TNetUnique: [%s] %*.*s", TErrString (td -> td_reason),
		    td -> td_cc, td -> td_cc, td -> td_data);
	else
	    adios (NULLCP, "TNetUnique: [%s]", TErrString (td -> td_reason));
    }
    if ((taddr = taddr2str (ta)) == NULLCP)
	adios (NULLCP, "taddr2str failed");

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

	/* we pass the transport address we are listening on in the initial
	   user data */
    if (SConnRequest (&sbo -> sb_connect, NULLSA, sz, sbo -> sb_requirements,
		      sbo -> sb_settings, sbo -> sb_isn, taddr, strlen (taddr),
		      NULLQOS, sc, si) == NOTOK)
	adios (NULLCP, "S-CONNECT.REQUEST: %s", SErrString (sa -> sa_reason));
    if (sc -> sc_result != SC_ACCEPT)
	adios (NULLCP, "connection rejected by peer: %s",
		SErrString (sc -> sc_result));
    advise (LLOG_NOTICE, NULLCP,
	    "S-CONNECT.RESPONSE: <%d, %s, %s, %s, %ld, %d>",
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

	/* now wait for the other end to call us back */
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
	adios (NULLCP, "S-CONNECT.INDICATION: %s",
	       SErrString (sa -> sa_reason));
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

    if (SRelRequest (sbo -> sb_sd, NULLCP, 0, NOTOK, sr, si) == NOTOK)
	adios (NULLCP, "S-RELEASE.REQUEST: %s", SErrString (sa -> sa_reason));
    if (!sr -> sr_affirmative)
	adios (NULLCP, "release rejected by peer");

    switch (SReadRequest (sbi -> sb_sd, sx, NOTOK, si)) {
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
