/* ftamattr.c - FPM: encode/decode attributes */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamattr.c,v 7.3 91/02/22 09:22:37 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamattr.c,v 7.3 91/02/22 09:22:37 mrose Interim $
 *
 *
 * $Log:	ftamattr.c,v $
 * Revision 7.3  91/02/22  09:22:37  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/23  20:41:27  mrose
 * update
 * 
 * Revision 7.1  90/03/23  10:53:58  mrose
 * update
 * 
 * Revision 7.0  89/11/23  21:53:23  mrose
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

struct type_FTAM_Read__Attributes *attr2fpm (fsb, fa, fti)
register struct ftamblk *fsb;
register struct FTAMattributes *fa;
struct FTAMindication *fti;
{
    register int    i;
    register char  *cp,
		  **ap;
    register struct type_FTAM_Read__Attributes *fpm;

    if ((fpm = (struct type_FTAM_Read__Attributes *) calloc (1, sizeof *fpm))
	    == NULL) {
no_mem: ;
	(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP, "out of memory");
out: ;
	if (fpm)
	    free_FTAM_Read__Attributes (fpm);
	return NULL;
    }

    if (fa -> fa_present & FA_FILENAME) {
	register struct type_FTAM_Filename__Attribute *fn,
						     **fc;

	if (fa -> fa_novalue & FA_FILENAME) {
	    (void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			     "filename not present");
	    goto out;
	}

	if (fa -> fa_nfile > NFFILE) {
	    (void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			     "too many filenames");
	    goto out;
	}

	fc = &fpm -> filename;
	for (ap = fa -> fa_files, i = fa -> fa_nfile - 1; i >= 0; ap++, i--) {
	    if (*ap == NULL) {
		(void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
				 "empty filename at slot %d",
				 fa -> fa_nfile - i - 1);
		goto out;
	    }

	    if ((fn = (struct type_FTAM_Filename__Attribute *)
				calloc (1, sizeof *fpm -> filename))
	            == NULL)
		goto no_mem;
	    *fc = fn;

	    if ((fn -> GraphicString = str2qb (*ap, strlen (*ap), 1)) == NULL)
		goto no_mem;

	    fc = &((*fc) -> next);
	}
    }

    if (fa -> fa_present & FA_ACTIONS) {
	if (fa -> fa_novalue & FA_ACTIONS) {
	    (void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			     "permitted-actions not present");
	    goto out;
	}

	if ((fpm -> permitted__actions = bits2fpm (fsb, fpermitted_pairs,
						   fa -> fa_permitted, fti))
	        == NULL)
	    goto out;
    }

    if (fa -> fa_present & FA_CONTENTS) {
	if (fa -> fa_novalue & FA_CONTENTS) {
	    (void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			     "contents-type not present");
	    goto out;
	}
	if (fa -> fa_contents == NULLOID) {
	    (void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			     "missing contents-type");
	    goto out;
	}
	if ((fpm -> contents__type =
		    (struct type_FTAM_Contents__Type__Attribute *)
	     		    calloc (1, sizeof *fpm -> contents__type)) == NULL
		|| (fpm -> contents__type -> document__type__name =
		    	    oid_cpy (fa -> fa_contents)) == NULL)
	    goto no_mem;
	if (fpm -> contents__type -> parameter = fa -> fa_parameter)
	    fpm -> contents__type -> parameter -> pe_refcnt++;
    }

    if (fa -> fa_present & FA_ACCOUNT) {
	if (fa -> fa_account == NULL) {
	    (void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			     "missing account");
	    goto out;
	}
	if ((fpm -> storage__account =
		    (struct type_FTAM_Account__Attribute *)
			    calloc (1, sizeof *fpm -> storage__account))
	        == NULL)
	    goto no_mem;
	if (fa -> fa_novalue & FA_ACCOUNT)
	    fpm -> storage__account -> offset =
		    type_FTAM_Account__Attribute_no__value__available;
	else {
	    fpm -> storage__account -> offset =
		    type_FTAM_Account__Attribute_actual__values;
	    if ((fpm -> storage__account -> un.actual__values =
		       str2qb (fa -> fa_account, strlen (fa -> fa_account), 1))
		    == NULL)
		goto no_mem;
	}
    }

#define	dodate(flag,field,tag) \
    if (fa -> fa_present & flag) { \
	if ((fpm -> tag = (struct type_FTAM_Date__and__Time__Attribute *) \
			        calloc (1, sizeof *fpm -> tag)) == NULL) \
	    goto no_mem; \
	if (fa -> fa_novalue & flag) \
	    fpm -> tag -> offset = \
		    type_FTAM_Date__and__Time__Attribute_no__value__available;\
	else { \
	    fpm -> tag -> offset = \
		    type_FTAM_Date__and__Time__Attribute_actual__values; \
	    if ((cp = gent2str (field)) == NULL \
		    || (fpm -> tag -> un.actual__values = \
				str2qb (cp, strlen (cp), 1)) == NULL) \
		goto no_mem; \
	} \
    }
    dodate (FA_DATE_CREATE, &fa -> fa_date_create,
	    date__and__time__of__creation);
    dodate (FA_DATE_MODIFY, &fa -> fa_date_modify,
	    date__and__time__of__last__modification);
    dodate (FA_DATE_READ, &fa -> fa_date_read,
	    date__and__time__of__last__read__access);
    dodate (FA_DATE_ATTR, &fa -> fa_date_attribute,
	    date__and__time__of__last__attribute__modification);
#undef	dodate

#define	douser(flag,field,tag,name) \
    if (fa -> fa_present & flag) { \
	if ((fpm -> tag = (struct type_FTAM_User__Identity__Attribute *) \
				calloc (1, sizeof *fpm -> tag)) == NULL) \
	    goto no_mem; \
	if (fa -> fa_novalue & flag) \
	    fpm -> tag -> offset = \
		    type_FTAM_User__Identity__Attribute_no__value__available; \
	else { \
	    if (field == NULL) { \
		(void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP, "missing %s", \
				 name); \
		goto out; \
	    } \
	    fpm -> tag -> offset = \
		    type_FTAM_User__Identity__Attribute_actual__values; \
	    if ((fpm -> tag -> un.actual__values = str2qb (field, \
							  strlen (field), 1)) \
		    == NULL) \
		goto no_mem; \
	} \
    }
    douser (FA_ID_CREATE, fa -> fa_id_create,
	    identity__of__creator, "identity-of-creator");
    douser (FA_ID_MODIFY, fa -> fa_id_modify,
	    identity__of__last__modifier, "identity-of-last-modifier");
    douser (FA_ID_READ, fa -> fa_id_read,
	    identity__of__last__reader, "identity-of-last-reader");
    douser (FA_ID_ATTR, fa -> fa_id_attribute,
	    identity__of__last__attribute__modifier,
	    "identity-of-last-attribute-modifier");
#undef	douser

    if (fa -> fa_present & FA_AVAILABILITY) {
	if ((fpm -> file__availability =
		    (struct type_FTAM_File__Availability__Attribute *)
			    calloc (1, sizeof *fpm -> file__availability))
	        == NULL)
	    goto no_mem;
	if (fa -> fa_novalue & FA_AVAILABILITY)
	    fpm -> file__availability -> offset =
		type_FTAM_File__Availability__Attribute_no__value__available;
	else {
	    fpm -> file__availability -> offset =
		type_FTAM_File__Availability__Attribute_actual__values;
	    switch (fa -> fa_availability) {
		case FA_AVAIL_IMMED:
		case FA_AVAIL_DEFER:
		    fpm -> file__availability -> un.actual__values =
				fa -> fa_availability;
		    break;

		default:
		    (void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
				     "bad value for file-availability");
		    goto out;
	    }
	}
    }

#define	dosize(flag,field,tag) \
    if (fa -> fa_present & flag) { \
	if ((fpm -> tag = (struct type_FTAM_Filesize__Attribute *) \
			    calloc (1, sizeof *fpm -> tag)) == NULL) \
	    goto no_mem; \
	if (fa -> fa_novalue & flag) \
	    fpm -> tag -> offset = \
		type_FTAM_Filesize__Attribute_no__value__available; \
	else { \
	    fpm -> tag -> offset = \
		type_FTAM_Filesize__Attribute_actual__values; \
	    fpm -> tag -> un.actual__values = field; \
	} \
    }
    dosize (FA_FILESIZE, fa -> fa_filesize, filesize);
    dosize (FA_FUTURESIZE, fa -> fa_futuresize, future__filesize);
#undef	dosize

    if (fa -> fa_present & FA_CONTROL) {
	if ((fpm -> access__control =
		    (struct type_FTAM_Access__Control__Attribute *)
			    calloc (1, sizeof *fpm -> access__control))
	        == NULL)
	    goto no_mem;
	if (fa -> fa_novalue & FA_CONTROL)
	    fpm -> access__control -> offset =
		type_FTAM_Access__Control__Attribute_no__value__available;
	else {
	    fpm -> access__control -> offset =
		type_FTAM_Access__Control__Attribute_actual__values;
	    if ((fpm -> access__control -> un.actual__values =
			    acl2fpm (fsb, fa -> fa_control, fti)) == NULL)
		goto out;
	}
    }

    if (fa -> fa_present & FA_LEGAL) {
	if ((fpm -> legal__qualification =
			(struct type_FTAM_Legal__Qualification__Attribute *)
			       calloc (1, sizeof *fpm -> legal__qualification))
		== NULL)
	    goto no_mem;
	if (fa -> fa_novalue & FA_LEGAL)
	    fpm -> legal__qualification -> offset =
		type_FTAM_Legal__Qualification__Attribute_no__value__available;
	else {
	    if (fa -> fa_legal == NULL) {
		(void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
				 "missing legal-qualification");
		goto out;
	    }
	    fpm -> legal__qualification -> offset =
		type_FTAM_Legal__Qualification__Attribute_actual__values;
	    if ((fpm -> legal__qualification -> un.actual__values =
			str2qb (fa -> fa_legal, strlen (fa -> fa_legal), 1))
		    == NULL)
		goto no_mem;
	}
    }
	
    /*
     * Added private use attribute functionality for Retix NBS9 interworking.
     * No value should always be set because we don't support private use.
     * pmk.
     */
    if (fa -> fa_present & FA_PRIVATE) {
	(void) ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			 "private-use attribute not supported");
	goto out;
    }

    return fpm;
}

/*  */

int	fpm2attr (fsb, fpm, fa, fti)
register struct ftamblk *fsb;
register struct type_FTAM_Read__Attributes *fpm;
register struct FTAMattributes *fa;
struct FTAMindication *fti;
{
    register char   *cp;
    register UTC     u;

    bzero ((char *) fa, sizeof *fa);

    if (fpm -> filename) {
	register int	n;
	register char **ap;
	register struct type_FTAM_Filename__Attribute *fn;

	fa -> fa_present |= FA_FILENAME;

	ap = fa -> fa_files, n = NFFILE;
	for (fn = fpm -> filename; fn; fn = fn -> next) {
	    if (n-- <= 0) {
		(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
				 "too many names in filename");
out: ;
		FAFREE (fa);
		return NOTOK;
	    }
	    if ((*ap++ = qb2str (fn -> GraphicString)) == NULL) {
no_mem: ;
		(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
				 "out of memory");
		goto out;
	    }
	    fa -> fa_nfile++;
	}
    }

    if (fpm -> permitted__actions) {
	fa -> fa_present |= FA_ACTIONS;

	if (fpm2bits (fsb, fpermitted_pairs, fpm -> permitted__actions,
		      &fa -> fa_permitted, fti) == NOTOK)
	    goto out;
    }

    if (fpm -> contents__type) {
	fa -> fa_present |= FA_CONTENTS;

	fa -> fa_contents = fpm -> contents__type -> document__type__name;
	fpm -> contents__type -> document__type__name = NULLOID;
	if (fpm -> contents__type -> parameter
	        && (fa -> fa_parameter =
		    	    pe_cpy (fpm -> contents__type -> parameter))
	    		== NULLPE)
	    goto no_mem;
    }

    if (fpm -> storage__account) {
	fa -> fa_present |= FA_ACCOUNT;

	if (fpm -> storage__account -> offset
	        == type_FTAM_Account__Attribute_no__value__available)
	    fa -> fa_novalue |= FA_ACCOUNT;
	else {
	    if ((fa -> fa_account =
			qb2str (fpm -> storage__account -> un.actual__values))
		    == NULL)
		goto no_mem;
	    if (*fa -> fa_account == NULL) {	/* CDC: some other vendor... */
		fa -> fa_present &= ~FA_ACCOUNT;

		free (fa -> fa_account);
		fa -> fa_account = NULL;
	    }
	}
    }

#define	dodate(flag,field,tag) \
    if (fpm -> tag) { \
	fa -> fa_present |= flag; \
 \
	if (fpm -> tag -> offset \
	        == type_FTAM_Date__and__Time__Attribute_no__value__available) \
	    fa -> fa_novalue |= flag; \
	else { \
	    if ((cp = qb2str (fpm -> tag -> un.actual__values)) == NULL) \
		goto no_mem; \
	    u = str2gent (cp, strlen (cp)); \
	    free (cp); \
	    if (u == NULLUTC) \
		goto no_mem; \
	    field = *u; \
	} \
    }
    dodate (FA_DATE_CREATE, fa -> fa_date_create,
	    date__and__time__of__creation);
    dodate (FA_DATE_MODIFY, fa -> fa_date_modify,
	    date__and__time__of__last__modification);
    dodate (FA_DATE_READ, fa -> fa_date_read,
	    date__and__time__of__last__read__access);
    dodate (FA_DATE_ATTR, fa -> fa_date_attribute,
	    date__and__time__of__last__attribute__modification);
#undef	dodate

#define	douser(flag,field,tag,name) \
    if (fpm -> tag) { \
	fa -> fa_present |= flag; \
 \
	if (fpm -> tag -> offset \
		== type_FTAM_User__Identity__Attribute_no__value__available) \
	    fa -> fa_novalue |= flag; \
	else \
	    if ((field = qb2str (fpm -> tag -> un.actual__values)) == NULL) \
		goto no_mem; \
    }
    douser (FA_ID_CREATE, fa -> fa_id_create,
	    identity__of__creator, "identity-of-creator");
    douser (FA_ID_MODIFY, fa -> fa_id_modify,
	    identity__of__last__modifier, "identity-of-last-modifier");
    douser (FA_ID_READ, fa -> fa_id_read,
	    identity__of__last__reader, "identity-of-last-reader");
    douser (FA_ID_ATTR, fa -> fa_id_attribute,
	    identity__of__last__attribute__modifier,
	    "identity-of-last-attribute-modifier");
#undef	douser

    if (fpm -> file__availability) {
	fa -> fa_present |= FA_AVAILABILITY;

	if (fpm -> file__availability -> offset ==
		type_FTAM_File__Availability__Attribute_no__value__available)
	    fa -> fa_novalue |= FA_AVAILABILITY;
	else
	    fa -> fa_availability =
			fpm -> file__availability -> un.actual__values;
    }

#define	dosize(flag,field,tag) \
    if (fpm -> tag) { \
	fa -> fa_present |= flag; \
 \
	if (fpm -> tag -> offset \
		== type_FTAM_Filesize__Attribute_no__value__available) \
	    fa -> fa_novalue |= flag; \
	else \
	    field = fpm -> tag -> un.actual__values; \
    }
    dosize (FA_FILESIZE, fa -> fa_filesize, filesize);
    dosize (FA_FUTURESIZE, fa -> fa_futuresize, future__filesize);
#undef	dosize

    if (fpm -> access__control) {
	fa -> fa_present |= FA_CONTROL;

	if (fpm -> access__control -> offset
	        == type_FTAM_Access__Control__Attribute_no__value__available)
	    fa -> fa_novalue |= FA_CONTROL;
	else
	    if (fpm2acl (fsb, fpm -> access__control -> un.actual__values,
			 &fa -> fa_control, fti) == NOTOK)
	    goto out;
    }

    if (fpm -> legal__qualification) {
	fa -> fa_present |= FA_LEGAL;

	if (fpm -> legal__qualification -> offset ==
	        type_FTAM_Legal__Qualification__Attribute_no__value__available)
	    fa -> fa_novalue |= FA_LEGAL;
	else
	    if ((fa -> fa_legal =
		     qb2str (fpm -> legal__qualification -> un.actual__values))
		    == NULL)
		goto no_mem;
    }

    /* Added private use functionality pmk */
    if (fpm -> private__use) {
	fa -> fa_present |= FA_PRIVATE;
	/* Set no value, regardless of what is there we don't support it */
	fa -> fa_novalue |= FA_PRIVATE;
    }

    return OK;
}

/*  */

void	FAFREE (fa)
register struct FTAMattributes *fa;
{
    register int FAI;

    for (FAI = (fa) -> fa_nfile - 1; FAI >= 0; FAI--)
	if ((fa) -> fa_files[FAI])
	    free ((fa) -> fa_files[FAI]), (fa) -> fa_files[FAI] = NULL;
    (fa) -> fa_nfile = 0;
    if ((fa) -> fa_contents)
	oid_free ((fa) -> fa_contents), (fa) -> fa_contents = NULLOID;
    if ((fa) -> fa_parameter)
	pe_free ((fa) -> fa_parameter), (fa) -> fa_parameter = NULLPE;
    if ((fa) -> fa_account)
	free ((fa) -> fa_account), (fa) -> fa_account = NULL;
    if ((fa) -> fa_id_create)
	free  ((fa) -> fa_id_create), (fa) -> fa_id_create = NULL;
    if ((fa) -> fa_id_modify)
	free  ((fa) -> fa_id_modify), (fa) -> fa_id_modify = NULL;
    if ((fa) -> fa_id_read)
	free  ((fa) -> fa_id_read), (fa) -> fa_id_read = NULL;
    if ((fa) -> fa_id_attribute)
	free  ((fa) -> fa_id_attribute), (fa) -> fa_id_attribute = NULL;
    if (fa -> fa_control) {
	FEFREE (fa -> fa_control);
	fa -> fa_control = NULL;
    }
    if ((fa) -> fa_legal)
	free ((fa) -> fa_legal), (fa) -> fa_legal = NULL;
    if ((fa) -> fa_private)
      free ((fa) -> fa_private), (fa) -> fa_private = NULL;
}
