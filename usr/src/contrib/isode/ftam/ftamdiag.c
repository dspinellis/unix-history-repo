/* ftamdiag.c - FPM: encode/decode diagnostics */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamdiag.c,v 7.2 91/02/22 09:22:47 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamdiag.c,v 7.2 91/02/22 09:22:47 mrose Interim $
 *
 *
 * $Log:	ftamdiag.c,v $
 * Revision 7.2  91/02/22  09:22:47  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/01/11  18:35:40  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  21:53:32  mrose
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


/* LINTLIBRARY */

#include <stdio.h>
#include "fpkt.h"

/*  */

struct type_FTAM_Diagnostic *diag2fpm (fsb, magic, diag, ndiag, fti)
register struct ftamblk *fsb;
int	magic;
struct FTAMdiagnostic diag[];
int	ndiag;
struct FTAMindication *fti;
{
    register int    i;
    register struct FTAMdiagnostic *dp;
    struct type_FTAM_Diagnostic *fpmp;
    register struct type_FTAM_Diagnostic  *fpm,
					 **fpc;
    register struct diag_element *f3;

    fpmp = NULL, fpc = &fpmp;
    for (dp = diag, i = ndiag - 1; i >= 0; dp++, i--) {
	if ((fpm = (struct type_FTAM_Diagnostic *) calloc (1, sizeof *fpm))
		== NULL) {
no_mem: ;
	    (void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP, "out of memory");
out: ;
	    if (fpmp)
		free_FTAM_Diagnostic (fpmp);
	    return NULL;
	}
	*fpc = fpm;

	if ((f3 = (struct diag_element *) calloc (1, sizeof *f3)) == NULL)
	    goto no_mem;
	fpm -> diagnostic = f3;

	switch (dp -> ftd_type) {
	    case DIAG_INFORM: 
	    case DIAG_TRANS: 
	    case DIAG_PERM: 
		break;

	    default: 
	bad_dp: ;
		(void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad diagnostic type/observer/source (%d/%d/%d) at slot %d",
			dp -> ftd_type, dp -> ftd_observer, dp -> ftd_source,
			ndiag - i - 1);
		goto out;
	}
	f3 -> diagnostic__type = dp -> ftd_type;

	f3 -> error__identifier = dp -> ftd_identifier;

	switch (dp -> ftd_observer) {
	    case EREF_IFSU: 
	    case EREF_IFPM: 
		if (!(fsb -> fsb_flags & FSB_INIT))
		    goto bad_dp;
		break;

	    case EREF_RFSU: 
	    case EREF_RFPM: 
		if (fsb -> fsb_flags & FSB_INIT)
		    goto bad_dp;
		break;

	    default:
		if (!magic)
		    goto bad_dp;
		break;
	}
	if ((f3 -> error__observer =
			(struct type_FTAM_Entity__Reference *)
				calloc (1, sizeof *f3 -> error__observer))
	        == NULL)
	    goto no_mem;
	f3 -> error__observer -> parm = dp -> ftd_observer;

	switch (dp -> ftd_source) {
	    case EREF_NONE: 
	    case EREF_IFSU: 
	    case EREF_IFPM: 
	    case EREF_SERV: 
	    case EREF_RFPM: 
	    case EREF_RFSU: 
		break;

	    default: 
		goto bad_dp;
	}
	if ((f3 -> error__source =
			(struct type_FTAM_Entity__Reference *)
				calloc (1, sizeof *f3 -> error__source))
	        == NULL)
	    goto no_mem;
	f3 -> error__source -> parm = dp -> ftd_source;

	if (dp -> ftd_delay != DIAG_NODELAY) {
	    f3 -> optionals |= opt_FTAM_diag_element_suggested__delay;
	    f3 -> suggested__delay = dp -> ftd_delay;
	}

	if (dp -> ftd_cc > FTD_SIZE)
	    goto bad_dp;
	else
	    if (dp -> ftd_cc > 0
		    && (f3 -> further__details = str2qb (dp -> ftd_data,
							 dp -> ftd_cc, 1))
			    == NULL)
		goto no_mem;

	fpc = &fpm -> next;
    }

    return fpmp;
}

/*  */

int	fpm2diag (fsb, fpm, diag, ndiag, fti)
register struct ftamblk *fsb;
register struct type_FTAM_Diagnostic *fpm;
struct FTAMdiagnostic diag[];
int   *ndiag;
struct FTAMindication *fti;
{
    register int    i;
    register struct FTAMdiagnostic *dp;
    register struct diag_element *f3;

    *ndiag = 0;

    dp = diag, i = 0;
    for (; fpm; fpm = fpm -> next) {
	if (i >= NFDIAG)
	    return ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
			     "too many diagnostics");

	f3 = fpm -> diagnostic;
	dp -> ftd_type = f3 -> diagnostic__type;
	dp -> ftd_identifier = f3 -> error__identifier;
	dp -> ftd_observer = f3 -> error__observer -> parm;
	dp -> ftd_source = f3 -> error__source -> parm;
	if (f3 -> optionals & opt_FTAM_diag_element_suggested__delay)
	    dp -> ftd_delay = f3 -> suggested__delay;
	else
	    dp -> ftd_delay = DIAG_NODELAY;
	if (f3 -> further__details) {
	    register char   *cp;

	    if ((cp = qb2str (f3 -> further__details)) == NULL)
		return ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
				 "out of memory");
	    (void) strncpy (dp -> ftd_data, cp, sizeof dp -> ftd_data);
	    dp -> ftd_data[sizeof dp -> ftd_data - 1] = NULL;
	    dp -> ftd_cc = strlen (dp -> ftd_data);
	    free (cp);
	}
	else
	    dp -> ftd_cc = 0;

	dp++, i++;
    }

    *ndiag = i;

    return OK;
}
