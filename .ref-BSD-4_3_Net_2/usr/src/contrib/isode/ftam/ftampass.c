/* ftampass.c - FPM: encode/decode access passwords */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftampass.c,v 7.1 91/02/22 09:23:03 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftampass.c,v 7.1 91/02/22 09:23:03 mrose Interim $
 *
 *
 * $Log:	ftampass.c,v $
 * Revision 7.1  91/02/22  09:23:03  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:53:44  mrose
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

struct type_FTAM_Access__Passwords *pass2fpm (fsb, fp, fti)
register struct ftamblk *fsb;
register struct FTAMpasswords *fp;
struct FTAMindication *fti;
{
    register struct type_FTAM_Access__Passwords *fpm;

    if ((fpm = (struct type_FTAM_Access__Passwords *)
		    calloc (1, sizeof *fpm)) == NULL) {
no_mem: ;
	(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP, "out of memory");
	if (fpm)
	    free_FTAM_Access__Passwords (fpm);
	return NULL;
    }

#define	dopass(s,t,u) \
{ \
    if ((fpm -> s = (struct type_FTAM_Password *) \
			    calloc (1, sizeof *fpm -> s)) \
	    == NULL) \
	goto no_mem; \
    fpm -> s -> offset = type_FTAM_Password_binary; \
    if ((fpm -> s -> un.binary = str2qb (fp -> t, fp -> u, 1)) == NULL) \
	goto no_mem; \
}

    dopass (read__password, fp_read, fp_readlen);
    dopass (insert__password, fp_insert, fp_insertlen);
    dopass (replace__password, fp_replace, fp_replacelen);
    dopass (extend__password, fp_extend, fp_extendlen);
    dopass (erase__password, fp_erase, fp_eraselen);
    dopass (read__attribute__password, fp_readattr, fp_readattrlen);
    dopass (change__attribute__password, fp_chngattr, fp_chngattrlen);
    dopass (delete__password, fp_delete, fp_deletelen);

#undef	dopass

    return fpm;
}

/*  */

int	fpm2pass (fsb, fpm, fp, fti)
register struct ftamblk *fsb;
register struct type_FTAM_Access__Passwords *fpm;
register struct FTAMpasswords *fp;
struct FTAMindication *fti;
{
    register struct qbuf *qb;

    bzero ((char *) fp, sizeof *fp);

/* both choices of this structure are qbuf's, so... */
#define	dopass(s,t,u) \
{ \
    qb = fpm -> s -> un.graphic; \
 \
    if ((fp -> t = qb2str (qb)) == NULL) \
	goto no_mem; \
    fp -> u = qb -> qb_len; \
}

    dopass (read__password, fp_read, fp_readlen);
    dopass (insert__password, fp_insert, fp_insertlen);
    dopass (replace__password, fp_replace, fp_replacelen);
    dopass (extend__password, fp_extend, fp_extendlen);
    dopass (erase__password, fp_erase, fp_eraselen);
    dopass (read__attribute__password, fp_readattr, fp_readattrlen);
    dopass (change__attribute__password, fp_chngattr, fp_chngattrlen);
    dopass (delete__password, fp_delete, fp_deletelen);

#undef	dopass

    return OK;

no_mem: ;
    FPFREE (fp);
    return ftamlose (fti, FS_GEN (fsb), 1, NULLCP, "out of memory");
}
