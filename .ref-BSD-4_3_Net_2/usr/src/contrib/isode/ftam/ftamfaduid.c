/* ftamfaduid.c - FPM: encode/decode FADU identities */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamfaduid.c,v 7.1 91/02/22 09:22:51 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamfaduid.c,v 7.1 91/02/22 09:22:51 mrose Interim $
 *
 *
 * $Log:	ftamfaduid.c,v $
 * Revision 7.1  91/02/22  09:22:51  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:53:36  mrose
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

struct type_FTAM_FADU__Identity *faduid2fpm (fsb, fa, fti)
register struct ftamblk *fsb;
register struct FADUidentity *fa;
struct FTAMindication *fti;
{
    register int    n;
    register char **ap;
    register struct type_FTAM_FADU__Identity *fpm;
    register struct name_element **f4;

    if ((fpm = (struct type_FTAM_FADU__Identity *) calloc (1, sizeof *fpm))
	    == NULL) {
no_mem: ;
	(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP, "out of memory");
out: ;
	if (fpm)
	    free_FTAM_FADU__Identity (fpm);
	return NULL;
    }

    switch (fa -> fa_type) {
	case FA_FIRSTLAST:
	    fpm -> offset = type_FTAM_FADU__Identity_first__last;
	    switch (fa -> fa_firstlast) {
		case FA_FIRST:
		case FA_LAST:
		    break;

		default:
bad_value:;
		    (void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
				     "bad value in FADU identity");
		    goto out;
	    }
	    fpm -> un.first__last = fa -> fa_firstlast;
	    break;

	case FA_RELATIVE:
	    fpm -> offset = type_FTAM_FADU__Identity_relative;
	    switch (fa -> fa_relative) {
		case FA_PREVIOUS:
		case FA_CURRENT:
		case FA_NEXT:
		    break;

		default:
		    goto bad_value;
	    }
	    fpm -> un.relative = fa -> fa_relative;
	    break;

	case FA_BEGINEND:
	    fpm -> offset = type_FTAM_FADU__Identity_begin__end;
	    switch (fa -> fa_beginend) {
		case FA_BEGIN:
		case FA_END:
		    break;

		default:
		    goto out;
	    }
	    fpm -> un.begin__end = fa -> fa_beginend;
	    break;

	case FA_SINGLE:
	    if (!fa -> fa_singlename)
		goto bad_value;
	    fpm -> offset = type_FTAM_FADU__Identity_single__name;
	    if ((fpm -> un.single__name =
			str2qb (fa -> fa_singlename,
				strlen (fa -> fa_singlename),
				1)) == NULL)
		goto no_mem;
	    break;

	case FA_NAMELIST:
	    f4 = &fpm -> un.name__list;
	    for (n = fa -> fa_nname - 1, ap = fa -> fa_names;
		     n >= 0;
		     ap++, n--) {
		if (!*ap)
		    goto bad_value;
		if ((*f4 = (struct name_element *) calloc (1, sizeof **f4))
			    == NULL
		        || ((*f4) -> Node__Name =
			        str2qb (*ap, strlen (*ap), 1)) == NULL)
		    goto no_mem;
	        f4 = &((*f4) -> next);
	    }
	    break;

	case FA_FADUNUMBER:
	    fpm -> offset = type_FTAM_FADU__Identity_fadu__number;
	    fpm -> un.fadu__number = fa -> fa_fadunumber;
	    break;

	default:
	    (void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			     "bad type for FADU identity");
	    goto out;
    }

    return fpm;
}

/*  */

int	fpm2faduid (fsb, fpm, fa, fti)
register struct ftamblk *fsb;
register struct type_FTAM_FADU__Identity *fpm;
register struct FADUidentity *fa;
struct FTAMindication *fti;
{
    register int    n;
    register char **ap;
    register struct name_element *f4;

    bzero ((char *) fa, sizeof *fa);

    switch (fpm -> offset) {
	case type_FTAM_FADU__Identity_first__last:
	    fa -> fa_type = FA_FIRSTLAST;
	    fa -> fa_firstlast = fpm -> un.first__last;
	    break;

	case type_FTAM_FADU__Identity_relative:
	    fa -> fa_type = FA_RELATIVE;
	    fa -> fa_relative = fpm -> un.relative;
	    break;

	case type_FTAM_FADU__Identity_begin__end:
	    fa -> fa_type = FA_BEGINEND;
	    fa -> fa_beginend = fpm -> un.begin__end;
	    break;

	case type_FTAM_FADU__Identity_single__name:
	    fa -> fa_type = FA_SINGLE;
	    if ((fa -> fa_singlename = qb2str (fpm -> un.single__name))
		    == NULL) {
no_mem: ;
		FUFREE (fa);
		return ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
				 "out of memory");
	    }
	    break;

	case type_FTAM_FADU__Identity_name__list:
	    fa -> fa_type = FA_NAMELIST;
	    ap = fa -> fa_names, n = NANAME;
	    for (f4 = fpm -> un.name__list; f4; f4 = f4 -> next) {
		if (n-- <= 0) {
		    FUFREE (fa);
		    return ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
				     "too many nodes in FADU name");
		}
		if ((*ap++ = qb2str (f4 -> Node__Name)) == NULL)
		    goto no_mem;
	    }
	    break;

	case type_FTAM_FADU__Identity_fadu__number:
	    fa -> fa_type = FA_FADUNUMBER;
	    fa -> fa_fadunumber = fpm -> un.fadu__number;
	    break;
    }

    return OK;
}
