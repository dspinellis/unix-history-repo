/* ftamacl.c - FPM: encode/decode access control lists (SETs of AC elements) */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamacl.c,v 7.3 91/02/22 09:22:36 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamacl.c,v 7.3 91/02/22 09:22:36 mrose Interim $
 *
 *
 * $Log:	ftamacl.c,v $
 * Revision 7.3  91/02/22  09:22:36  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/08/14  14:27:59  mrose
 * oops
 * 
 * Revision 7.1  90/03/23  10:53:55  mrose
 * update
 * 
 * Revision 7.0  89/11/23  21:53:22  mrose
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

struct type_FTAM_Access__Control__List *acl2fpm (fsb, fe, fti)
register struct ftamblk *fsb;
register struct FTAMacelement *fe;
struct FTAMindication *fti;
{
    struct type_FTAM_Access__Control__List *fpmp;
    register struct type_FTAM_Access__Control__List  *fpm,
						    **fpc;
    register struct type_FTAM_Access__Control__Element *ace;

    fpmp = NULL, fpc = &fpmp;
    for (; fe; fe = fe -> fe_next) {
	if ((fpm = (struct type_FTAM_Access__Control__List *)
	     		calloc (1, sizeof *fpm)) == NULL) {
no_mem: ;
	    (void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP, "out of memory");
out: ;
	    if (fpmp)
		free_FTAM_Access__Control__List (fpmp);
	    return NULL;
	}
	*fpc = fpm, fpc = &fpm -> next;

	if ((ace = (struct type_FTAM_Access__Control__Element *)
			calloc (1, sizeof *ace)) == NULL)
	    goto no_mem;
	fpm -> Access__Control__Element = ace;

	if (fe -> fe_actions & FA_PERM_TRAVERSAL) {
	    (void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			     "bad value for action-list");
	    goto out;
	}
	if ((ace -> action__list = bits2fpm (fsb, frequested_pairs,
					     fe -> fe_actions, fti)) == NULL)
	    goto out;

	if (conctl_present (&fe -> fe_concurrency)
		&& (ace -> concurrency__access =
		    		conacc2fpm (fsb, &fe -> fe_concurrency, fti))
	    		== NULL)
	    goto out;

	if (fe -> fe_identity
	        && (ace -> identity = str2qb (fe -> fe_identity,
					      strlen (fe -> fe_identity), 1))
	    		== NULL)
	    goto no_mem;

	if (passes_present (&fe -> fe_passwords)
		&& (ace -> passwords = pass2fpm (fsb, &fe -> fe_passwords, fti))
	    		== NULL)
	    goto out;

	if (fe -> fe_aet) {
	    if ((ace -> location = (struct type_ACS_AE__title *)
					calloc (1, sizeof *ace -> location))
		    == NULL)
		goto no_mem;
	    if (ace -> location -> title = fe -> fe_aet -> aei_ap_title)
		ace -> location -> title -> pe_refcnt++;
	    if (ace -> location -> qualifier = fe -> fe_aet -> aei_ae_qualifier)
		ace -> location -> qualifier -> pe_refcnt++;
	}
    }

    return fpmp;
}

/*  */

int	fpm2acl (fsb, fpm, fe, fti)
register struct ftamblk *fsb;
register struct type_FTAM_Access__Control__List *fpm;
register struct FTAMacelement **fe;
struct FTAMindication *fti;
{
    register struct FTAMacelement *fc,
				 **fl;
    register struct type_FTAM_Access__Control__Element *ace;

    *(fl = fe) = NULL;

    for (; fpm; fpm = fpm -> next) {
	ace = fpm -> Access__Control__Element;

	if ((fc = (struct FTAMacelement *) calloc (1, sizeof *fc)) == NULL) {
no_mem: ;
	    (void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP, "out of memory");
out: ;
	    if (fc = *fl)
		FEFREE (fc);
	    return NOTOK;
	}
	*fe = fc, fe = &fc -> fe_next;

	if (fpm2bits (fsb, frequested_pairs, ace -> action__list,
		      &fc -> fe_actions, fti) == NOTOK)
	    goto out;

	FCINIT (&fc -> fe_concurrency);
	if (ace -> concurrency__access
	        && fpm2conacc (fsb, ace -> concurrency__access,
			       &fc -> fe_concurrency, fti) == NOTOK)
	    goto out;

	if (ace -> identity
	        && (fc -> fe_identity = qb2str (ace -> identity)) == NULL)
	    goto no_mem;

	if (ace -> passwords
		&& fpm2pass (fsb, ace -> passwords, &fc -> fe_passwords, fti)
			== NOTOK)
	    goto out;

	if (ace -> location) {
	    if ((fc -> fe_aet = (AEI) calloc (1, sizeof *fc -> fe_aet))
		    == NULL)
		goto no_mem;
	    if (ace -> location -> title
		    && (fc -> fe_aet -> aei_ap_title
			        = pe_cpy (ace -> location -> title)) == NULLPE)
		goto no_mem;
	    if (ace -> location -> qualifier
		    && (fc -> fe_aet -> aei_ae_qualifier
				= pe_cpy (ace -> location -> qualifier))
			    == NULLPE)
		goto no_mem;
	}
    }

    return OK;
}
