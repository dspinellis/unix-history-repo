/* ftamd.c - FTAM -- responder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftamd.c,v 7.1 91/02/22 09:24:04 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftamd.c,v 7.1 91/02/22 09:24:04 mrose Interim $
 *
 *
 * $Log:	ftamd.c,v $
 * Revision 7.1  91/02/22  09:24:04  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:54:36  mrose
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
#include <varargs.h>
#include "ftamsystem.h"

/*    DATA */

int	ftamfd = NOTOK;

int	cflag = 0;
int	debug = 0;
char   *myname = "ftamd";

/*    MAIN */

#define	SMASK	"\020\01UNCONS\02MANAGE\03TRANSFER\04TM\05ACCESS"

#define	UMASK	"\020\01READ\02WRITE\03ACCESS\04LIMITED\05ENHANCED\06GROUPING\
\07FADULOCK\08RECOVERY\00RESTART"

#define	AMASK	"\020\01STORAGE\02SECURITY\03PRIVATE"

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int     result;
    char   *ap;
    struct FTAMstart    ftss;
    register struct FTAMstart  *fts = &ftss;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;

    if (myname = rindex (argv[0], '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = argv[0];

    isodetailor (myname, 0);
    if (debug == 0 && !(debug = isatty (fileno (stderr)))) {
	ftam_log -> ll_stat &= ~LLOGCLS;
	ll_hdinit (ftam_log, myname);
    }
    else
	ll_dbinit (ftam_log, myname);

    advise (LLOG_NOTICE, NULLCP, "starting");

    if (FInit (argc, argv, fts, debug ? FTraceHook : NULLIFP, fti) == NOTOK)
	ftam_adios (&fti -> fti_abort, "initialization");
    advise (LLOG_NOTICE, NULLCP,
	    "F-INITIALIZE.INDICATION: <%d, %s, %s, %s, %s>",
	    fts -> fts_sd, oid2ode (fts -> fts_context),
	    sprintb (fts -> fts_class, SMASK),
	    fts -> fts_initiator ? fts -> fts_initiator : "",
	    fts -> fts_account ? fts -> fts_account : "");
#ifdef	DEBUG
    {
	register int    i;
	register struct FTAMcontent *fx;

	advise (LLOG_DEBUG, NULLCP,
		" called AE title: %s, called PSAP address: %s",
		sprintaei (&fts -> fts_calledtitle),
		paddr2str (&fts -> fts_calledaddr, NULLNA));
	advise (LLOG_DEBUG, NULLCP,
		" calling AE title: %s, calling PSAP address: %s",
		sprintaei (&fts -> fts_callingtitle),
		paddr2str (&fts -> fts_callingaddr, NULLNA));

	advise (LLOG_DEBUG, NULLCP, " manage: %d, ftam-QoS: %d",
		fts -> fts_manage, fts -> fts_fqos);
	advise (LLOG_DEBUG, NULLCP, " units: %s",
		sprintb (fts -> fts_units, UMASK));
	advise (LLOG_DEBUG, NULLCP, " attrs: %s",
		sprintb (fts -> fts_attrs, AMASK));

	for (fx = fts -> fts_contents.fc_contents, i = 0;
		i < fts -> fts_contents.fc_ncontent;
		fx++, i++)
	    advise (LLOG_DEBUG, NULLCP, " cnt %d: %s %d %d",
		i, sprintoid (fx -> fc_dtn), fx -> fc_id, fx -> fc_result);
    }
#endif

    for (argv++; ap = *argv; argv++)
	if (*ap == '-')
	    while (*++ap)
		switch (*ap) {
		    case 'c':
			cflag++;
			break;

		    case 'd':
			debug++;
			ll_dbinit (ftam_log, myname);
			(void) FHookRequest (fts -> fts_sd, FTraceHook, fti);
			break;

		    default:
			adios (NULLCP, "unknown switch -%c", *ap);
		}
    	else
	    advise (LLOG_NOTICE, NULLCP, "unknown argument \"%s\"", ap);    

    ftam_start (fts);

    FTSFREE (fts);

    for (;;)
	switch (result = FWaitRequest (ftamfd, NOTOK, fti)) {
	    case NOTOK: 
	    case OK: 
	    case DONE: 
		ftam_indication (fti);
		break;

	    default: 
		adios (NULLCP, "unknown return from FWaitRequest=%d", result);
	}
}

/*  */

void	ftam_adios (fta, event)
struct FTAMabort *fta;
char   *event;
{
    struct FTAMindication   ftis;

    ftam_advise (fta, event);

    if (fta -> fta_action != FACTION_PERM && ftamfd != NOTOK)
	(void) FUAbortRequest (ftamfd, FACTION_PERM,
		(struct FTAMdiagnostic *) 0, 0, &ftis);

    closewtmp ();

    _exit (1);
}


void	ftam_advise (fta, event)
register struct FTAMabort *fta;
char   *event;
{
    advise (LLOG_NOTICE, NULLCP, "%s: failed", event);
    ftam_diag (fta -> fta_diags, fta -> fta_ndiag);

    if (fta -> fta_action == FACTION_PERM) {
	closewtmp ();

	_exit (1);
    }
}

/*  */

static char *entity[] = {
    "UNK", "IFSU", "IFPM", "VFS", "RFPM", "RFSU"
};


void	ftam_diag (diag, ndiag)
struct FTAMdiagnostic diag[];
int	ndiag;
{
    register int    i;
    register char  *cp;
    char    buffer[BUFSIZ];
    register struct FTAMdiagnostic *dp;

    for (dp = diag, i = ndiag - 1; i >= 0; dp++, i--) {
	cp = buffer;
	(void) sprintf (cp, "%s", FErrString (dp -> ftd_identifier));

	if (dp -> ftd_cc > 0) {
	    cp += strlen (cp);
	    (void) sprintf (cp, ": %*.*s", dp -> ftd_cc, dp -> ftd_cc,
		    dp -> ftd_data);
	}

	advise (LLOG_NOTICE, NULLCP, "%s", buffer);

	cp = buffer;
	(void) sprintf (cp, "    type ");
	cp += strlen (cp);

	switch (dp -> ftd_type) {
	    case DIAG_INFORM: 
		(void) sprintf (cp, "informative");
		break;

	    case DIAG_TRANS: 
		(void) sprintf (cp, "transient");
		break;

	    case DIAG_PERM: 
		(void) sprintf (cp, "permanent");
		break;

	    default: 
		(void) sprintf (cp, "%d", dp -> ftd_type);
		break;
	}
	cp += strlen (cp);

	(void) sprintf (cp, ", observer ");
	cp += strlen (cp);

	switch (dp -> ftd_observer) {
	    case EREF_IFSU: 
	    case EREF_IFPM: 
	    case EREF_RFPM: 
	    case EREF_RFSU: 
		(void) sprintf (cp, "%s", entity[dp -> ftd_observer]);
		break;

	    default: 
		(void) sprintf (cp, "%d", dp -> ftd_observer);
		break;
	}
	cp += strlen (cp);

	(void) sprintf (cp, ", source ");
	cp += strlen (cp);

	switch (dp -> ftd_source) {
	    case EREF_NONE: 
	    case EREF_IFSU: 
	    case EREF_IFPM: 
	    case EREF_SERV: 
	    case EREF_RFPM: 
	    case EREF_RFSU: 
		(void) sprintf (cp, "%s", entity[dp -> ftd_source]);
		break;

	    default: 
		(void) sprintf (cp, "%d", dp -> ftd_source);
		break;
	}

	if (dp -> ftd_delay != DIAG_NODELAY) {
	    cp += strlen (cp);
	    (void) sprintf (cp, ", suggested-delay %d", dp -> ftd_delay);
	}

	advise (LLOG_NOTICE, NULLCP, "%s", buffer);
    }
}

/*  */

#ifndef	lint
void	adios (va_alist)
va_dcl
{
    struct FTAMindication   ftis;
    va_list ap;

    va_start (ap);

    (void) _ll_log (ftam_log, LLOG_FATAL, ap);

    va_end (ap);

    if (ftamfd != NOTOK)
	(void) FUAbortRequest (ftamfd, FACTION_PERM,
		(struct FTAMdiagnostic *) 0, 0, &ftis);

    closewtmp ();

    _exit (1);
}
#else
/* VARARGS */

void	adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
void	advise (va_alist)
va_dcl
{
    int	    code;
    va_list ap;

    va_start (ap);

    code = va_arg (ap, int);

    (void) _ll_log (ftam_log, code, ap);

    va_end (ap);
}
#else
/* VARARGS */

void	advise (code, what, fmt)
char   *what,
       *fmt;
int	code;
{
    advise (code, what, fmt);
}
#endif
