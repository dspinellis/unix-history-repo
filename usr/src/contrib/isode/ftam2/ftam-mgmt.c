/* ftam-mgmt.c - interactive initiator FTAM -- file management */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftam-mgmt.c,v 7.2 91/02/22 09:23:46 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftam-mgmt.c,v 7.2 91/02/22 09:23:46 mrose Interim $
 *
 *
 * $Log:	ftam-mgmt.c,v $
 * Revision 7.2  91/02/22  09:23:46  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/21  11:30:30  mrose
 * sun
 * 
 * Revision 7.0  89/11/23  21:54:23  mrose
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
#include "ftamuser.h"

/*  */

int	f_mv (vec)
char  **vec;
{
#ifdef	BRIDGE
    register char *src;
#else
    int	sglobbed;
    register char  *bp,
		  **gp,
		  **src;
    char   *freedst = NULL,
	    buffer[BUFSIZ];
#endif
    register char  *dst;

    if (*++vec == NULL) {
#ifdef	BRIDGE
	return NOTOK;
#else
	if (getline ("source: ", buffer) == NOTOK || str2vec (buffer, vec) < 1)
	    return OK;
	dst = NULL;
#endif
    }
#ifdef	BRIDGE
    src = *vec;
    if (*++vec == NULL)
	return NOTOK;
    dst = *vec;
#else
    else {
	register char **ap;

	for (ap = vec; *ap; ap++)
	    continue;
	if (--ap != vec)
	    dst = *ap, *ap = NULL;
	else
	    dst = NULL;
    }
#endif
#ifndef	BRIDGE
    if (!(src = xglob (vec, 1)))
	return OK;
    sglobbed = xglobbed;

    if (dst == NULL) {
	if (getline ("destination: ", buffer) == NOTOK) {
	    blkfree (src);
	    return OK;
	}
	switch (str2vec (buffer, vec)) {
	    case 0: 
		switch (realstore) {
		    case RFS_UNIX:
			dst = ".";
			break;

		    default:
			advise (NULLCP, "need a destination");
			goto out;
		}
		break;

	    case 1: 
		dst = *vec;
		break;

	    default: 
		advise (NULLCP, "too many destinations");
		goto out;
	}
    }
    if (!(dst = freedst = xglob1val (dst, 1)))
	goto out;

    if (src[1] == NULL) {
	if (interrupted)
	    goto out;
	    
	switch (realstore) {
	    case RFS_UNIX:
		if (isdir (dst, NULLCP, 1) == NOTOK)
		    break;
#ifdef apollo
                if (*dst == '/')
		    (void) sprintf (bp = buffer, "%s", dst);
                else
#endif
		(void) sprintf (bp = buffer, "%s/", dst);
		bp += strlen (bp);
		if (dst = rindex (*src, '/'))
		    dst++;
		if (dst == NULL || *dst == NULL)
		    dst = *src;
		(void) strcpy (bp, dst);
		dst = buffer;

		switch (ask ("mv %s %s", *src, dst)) {
		    case NOTOK: 
			goto out;

		    case OK: 
		    default: 
			break;

		    case DONE: 
			goto out;
		}
		break;

	    default:
		break;
	}

	(void) mv (*src, dst, 0);
	goto out;
    }

    switch (realstore) {
	case RFS_UNKNOWN: 
	    advise (NULLCP, "%s", rs_unknown);
	    goto out;

	case RFS_UNIX: 
#ifdef apollo
            if (*dst == '/')
                (void) sprintf (bp = buffer, "%s", dst);
            else
#endif
	    (void) sprintf (bp = buffer, "%s/", dst);
	    bp += strlen (bp);
	    break;

	default: 
	    advise (NULLCP, "%s", rs_support);
	    goto out;
    }

    if (isdir (dst, NULLCP, 0) == NOTOK)
	goto out;

    for (gp = src; *gp; gp++) {
	switch (realstore) {
	    case RFS_UNIX:
		if (dst = rindex (*gp, '/'))
		    dst++;
		if (dst == NULL || *dst == NULL)
		    dst = *gp;
		break;

	    default:
		dst = *gp;
	}
	(void) strcpy (bp, dst);
	dst = str2file (buffer);

	if (sglobbed) {
	    if (query)
		switch (ask ("mv %s %s", *gp, dst)) {
		    case NOTOK:
		        continue;

		    case OK:
		    default:
			break;

		    case DONE:
			goto out;
		}
	    else
		advise (NULLCP, "mv %s %s", *gp, dst);
	}

	(void) mv (*gp, dst, 1);

	if (ftamfd == NOTOK)
	    break;
    }
#else
    return mv (src, dst, 0);
#endif

#ifndef	BRIDGE
out: ;
    blkfree (src);
    if (freedst)
	free (freedst);

    return OK;
#endif
}

/*  */

static int  mv (src, dst, multi)
char   *src,
       *dst;
int	multi;
{
    struct FTAMgroup    ftgs;
    register struct FTAMgroup  *ftg = &ftgs;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;

    bzero ((char *) ftg, sizeof *ftg);
    ftg -> ftg_flags |= FTG_BEGIN | FTG_END;
    ftg -> ftg_threshold = 0;

    ftg -> ftg_flags |= FTG_SELECT;
    {
	register struct FTAMselect *ftse = &ftg -> ftg_select;
	register struct FTAMattributes *fa = &ftse -> ftse_attrs;

	fa -> fa_present = FA_FILENAME;
	fa -> fa_nfile = 0;
	fa -> fa_files[fa -> fa_nfile++] = src;

	ftse -> ftse_access = FA_PERM_CHNGATTR;
	FCINIT (&ftse -> ftse_conctl);
    }
    ftg -> ftg_threshold++;

    ftg -> ftg_flags |= FTG_CHATTR;
    {
	register struct FTAMchngattr   *ftca = &ftg -> ftg_chngattr;
	register struct FTAMattributes *fa = &ftca -> ftca_attrs;

	fa -> fa_present = FA_FILENAME;
	fa -> fa_nfile = 0;
	fa -> fa_files[fa -> fa_nfile++] = dst;
    }
    ftg -> ftg_threshold++;

    ftg -> ftg_flags |= FTG_DESELECT;
    ftg -> ftg_threshold++;

    if (FManageRequest (ftamfd, ftg, fti) == NOTOK) {
	ftam_advise (&fti -> fti_abort, "F-MANAGE.REQUEST");
	return NOTOK;
    }

    ftg = &fti -> fti_group;

    if (ftg -> ftg_flags & FTG_SELECT) {
	register struct FTAMselect *ftse = &ftg -> ftg_select;

	if (multi && ftse -> ftse_state != FSTATE_SUCCESS)
	    printf ("%s\n", src);
	ftam_diag (ftse -> ftse_diags, ftse -> ftse_ndiag, 1, 
		ftse -> ftse_action);
	if (ftse -> ftse_state != FSTATE_SUCCESS)
	    goto you_lose;
    }

    if (ftg -> ftg_flags & FTG_CHATTR) {
	register struct FTAMchngattr   *ftca = &ftg -> ftg_chngattr;

	ftam_diag (ftca -> ftca_diags, ftca -> ftca_ndiag, 1,
		ftca -> ftca_action);
    }

    if (ftg -> ftg_flags & FTG_DESELECT) {
	register struct FTAMdeselect   *ftde = &ftg -> ftg_deselect;

	ftam_diag (ftde -> ftde_diags, ftde -> ftde_ndiag, 1,
		ftde -> ftde_action);
	ftam_chrg (&ftde -> ftde_charges);
    }

    FTGFREE (ftg);
    return OK;

you_lose: ;
    FTGFREE (ftg);
    return NOTOK;
}

/*  */

int	f_rm (vec)
char  **vec;
{
#ifndef	BRIDGE
    int     multi;
    char    buffer[BUFSIZ];
#endif

    if (*++vec == NULL) {
#ifdef	BRIDGE
	return NOTOK;
#else
	if (getline ("file: ", buffer) == NOTOK || str2vec (buffer, vec) < 1)
	    return OK;
#endif
    }

#ifdef	BRIDGE
    return rm (*vec, 0);
#else
    if (vec = xglob (vec, 1)) {
	register char **gp;

	multi = vec[1] ? 1 : 0;

	for (gp = vec; *gp && !interrupted; gp++) {
	    if (xglobbed && query)
		switch (ask ("rm %s", *gp)) {
		    case NOTOK:
			continue;

		    case OK:
		    default:
			break;

		    case DONE:
			goto out;
		}
		    
	    (void) rm (*gp, multi);

	    if (ftamfd == NOTOK)
		break;
	}

out: ;
	blkfree (vec);
    }

    return OK;
#endif
}

/*  */

static int  rm (file, multi)
char   *file;
int	multi;
{
    struct FTAMgroup    ftgs;
    register struct FTAMgroup  *ftg = &ftgs;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;

    bzero ((char *) ftg, sizeof *ftg);
    ftg -> ftg_flags |= FTG_BEGIN | FTG_END;
    ftg -> ftg_threshold = 0;

    ftg -> ftg_flags |= FTG_SELECT;
    {
	register struct FTAMselect *ftse = &ftg -> ftg_select;
	register struct FTAMattributes *fa = &ftse -> ftse_attrs;

	fa -> fa_present = FA_FILENAME;
	fa -> fa_nfile = 0;
	fa -> fa_files[fa -> fa_nfile++] = file;

	ftse -> ftse_access = FA_PERM_DELETE;
	FCINIT (&ftse -> ftse_conctl);
    }
    ftg -> ftg_threshold++;

    ftg -> ftg_flags |= FTG_DELETE;
    ftg -> ftg_threshold++;

    if (FManageRequest (ftamfd, ftg, fti) == NOTOK) {
	ftam_advise (&fti -> fti_abort, "F-MANAGE.REQUEST");
	return NOTOK;
    }

    ftg = &fti -> fti_group;

    if (ftg -> ftg_flags & FTG_SELECT) {
	register struct FTAMselect *ftse = &ftg -> ftg_select;

	if (multi && ftse -> ftse_state != FSTATE_SUCCESS)
	    printf ("%s\n", file);
	ftam_diag (ftse -> ftse_diags, ftse -> ftse_ndiag, 1,
		ftse -> ftse_action);
	if (ftse -> ftse_state != FSTATE_SUCCESS)
	    goto you_lose;
    }

    if (ftg -> ftg_flags & FTG_DELETE) {
	register struct FTAMdelete   *ftxe = &ftg -> ftg_delete;

	ftam_diag (ftxe -> ftxe_diags, ftxe -> ftxe_ndiag, 1,
		ftxe -> ftxe_action);
	ftam_chrg (&ftxe -> ftxe_charges);
    }

    FTGFREE (ftg);
    return OK;

you_lose: ;
    FTGFREE (ftg);
    return NOTOK;
}

/*  */

#ifndef	BRIDGE
int	f_chgrp (vec)
char  **vec;
{
    int     multi;
    char    group[8 + 1];
    char    buffer[BUFSIZ];

    if (!(attrs & FATTR_STORAGE)) {
	advise (NULLCP, "no support for storage attributes");
	return OK;
    }

    if (*++vec == NULL) {
	if (getline ("group: ", buffer) == NOTOK || str2vec (buffer, vec) < 1)
	    return OK;
    }

    (void) strcpy (group, vec[0]);

    if (*++vec == NULL) {
	if (getline ("file: ", buffer) == NOTOK || str2vec (buffer, vec) < 1)
	    return OK;
    }

    if (vec = xglob (vec, 1)) {
	register char **gp;

	multi = vec[1] ? 1 : 0;

	for (gp = vec; *gp && !interrupted; gp++) {
	    if (xglobbed && query)
		switch (ask ("chgrp %s %s", group, *gp)) {
		    case NOTOK:
			continue;

		    case OK:
		    default:
			break;

		    case DONE:
			goto out;
		}
		    
	    (void) chgrp (group, *gp, multi);

	    if (ftamfd == NOTOK)
		break;
	}

out: ;
	blkfree (vec);
    }

    return OK;
}

/*  */

static int  chgrp (group, file, multi)
char   *group,
       *file;
int	multi;
{
    struct FTAMgroup    ftgs;
    register struct FTAMgroup  *ftg = &ftgs;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;

    bzero ((char *) ftg, sizeof *ftg);
    ftg -> ftg_flags |= FTG_BEGIN | FTG_END;
    ftg -> ftg_threshold = 0;

    ftg -> ftg_flags |= FTG_SELECT;
    {
	register struct FTAMselect *ftse = &ftg -> ftg_select;
	register struct FTAMattributes *fa = &ftse -> ftse_attrs;

	fa -> fa_present = FA_FILENAME;
	fa -> fa_nfile = 0;
	fa -> fa_files[fa -> fa_nfile++] = file;

	ftse -> ftse_access = FA_PERM_CHNGATTR;
	FCINIT (&ftse -> ftse_conctl);
    }
    ftg -> ftg_threshold++;

    ftg -> ftg_flags |= FTG_CHATTR;
    {
	register struct FTAMchngattr   *ftca = &ftg -> ftg_chngattr;
	register struct FTAMattributes *fa = &ftca -> ftca_attrs;

	fa -> fa_present = FA_ACCOUNT;
	fa -> fa_account = group;
    }
    ftg -> ftg_threshold++;

    ftg -> ftg_flags |= FTG_DESELECT;
    ftg -> ftg_threshold++;

    if (FManageRequest (ftamfd, ftg, fti) == NOTOK) {
	ftam_advise (&fti -> fti_abort, "F-MANAGE.REQUEST");
	return NOTOK;
    }

    ftg = &fti -> fti_group;

    if (ftg -> ftg_flags & FTG_SELECT) {
	register struct FTAMselect *ftse = &ftg -> ftg_select;

	if (multi && ftse -> ftse_state != FSTATE_SUCCESS)
	    printf ("%s\n", file);
	ftam_diag (ftse -> ftse_diags, ftse -> ftse_ndiag, 1, 
		ftse -> ftse_action);
	if (ftse -> ftse_state != FSTATE_SUCCESS)
	    goto you_lose;
    }

    if (ftg -> ftg_flags & FTG_CHATTR) {
	register struct FTAMchngattr   *ftca = &ftg -> ftg_chngattr;

	ftam_diag (ftca -> ftca_diags, ftca -> ftca_ndiag, 1,
		ftca -> ftca_action);
    }

    if (ftg -> ftg_flags & FTG_DESELECT) {
	register struct FTAMdeselect   *ftde = &ftg -> ftg_deselect;

	ftam_diag (ftde -> ftde_diags, ftde -> ftde_ndiag, 1,
		ftde -> ftde_action);
	ftam_chrg (&ftde -> ftde_charges);
    }

    FTGFREE (ftg);
    return OK;

you_lose: ;
    FTGFREE (ftg);
    return NOTOK;
}
#endif

/*  */

int	f_mkdir (vec)
char  **vec;
{
#ifndef	BRIDGE
    int	    multi;
    register char  *dir;
    char    buffer[BUFSIZ];
#endif

    if (*++vec == NULL) {
#ifdef	BRIDGE
	return NOTOK;
#else
	if (getline ("directory: ", buffer) == NOTOK
		|| str2vec (buffer, vec) < 1)
	    return OK;

	(void) makedir (str2file (*vec), 0);
#endif
    }
#ifdef	BRIDGE
    return makedir (str2file (*vec));
#else
    else {
	multi = vec[1] ? 1 : 0;

	while (dir = *vec++)
	    (void) makedir (str2file (dir), multi);
    }

    return OK;
#endif
}

/*  */

static int  makedir (dir, multi)
char   *dir;
int	multi;
{
    struct FTAMgroup    ftgs;
    register struct FTAMgroup  *ftg = &ftgs;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;
    struct vfsmap *vf = &vfs[VFS_FDF];

    if (vf -> vf_oid == NULLOID) {
	advise (NULLCP, "no support for %ss", vf -> vf_text);
	return NOTOK;
    }

    bzero ((char *) ftg, sizeof *ftg);
    ftg -> ftg_flags |= FTG_BEGIN | FTG_END;
    ftg -> ftg_threshold = 0;

    ftg -> ftg_flags |= FTG_CREATE;
    {
	register struct FTAMcreate *ftce = &ftg -> ftg_create;
	register struct FTAMattributes *fa = &ftce -> ftce_attrs;

	ftce -> ftce_override = FOVER_FAIL;

	fa -> fa_present = FA_FILENAME;
	fa -> fa_nfile = 0;
	fa -> fa_files[fa -> fa_nfile++] = dir;

	fa -> fa_present |= FA_ACTIONS;
/*for NBS Phase 2 creation of directories is not		*/
/*allowed; so the access mode and the permitted actions */
/*below which includes "EXTEND" will have to be checked */
/*once creation of NSB-9 dir. file type is defined      */
	fa -> fa_permitted = FA_PERM_READ | FA_PERM_EXTEND
					| FA_PERM_READATTR | FA_PERM_CHNGATTR
	    			| FA_PERM_DELETE | FA_PERM_TRAV;

	fa -> fa_present |= FA_CONTENTS;
	fa -> fa_contents = vf -> vf_oid;

	ftce -> ftce_access = FA_PERM_EXTEND;
	FCINIT (&ftce -> ftce_conctl);
    }
    ftg -> ftg_threshold++;

    ftg -> ftg_flags |= FTG_DESELECT;
    ftg -> ftg_threshold++;

    if (FManageRequest (ftamfd, ftg, fti) == NOTOK) {
	ftam_advise (&fti -> fti_abort, "F-MANAGE.REQUEST");
	return NOTOK;
    }

    ftg = &fti -> fti_group;

    if (ftg -> ftg_flags & FTG_CREATE) {
	register struct FTAMcreate *ftce = &ftg -> ftg_create;

	if (multi && ftce -> ftce_state != FSTATE_SUCCESS)
	    printf ("%s\n", dir);
	ftam_diag (ftce -> ftce_diags, ftce -> ftce_ndiag, 1, 
		ftce -> ftce_action);
	if (ftce -> ftce_state != FSTATE_SUCCESS)
	    goto you_lose;
    }

    if (ftg -> ftg_flags & FTG_DESELECT) {
	register struct FTAMdeselect   *ftde = &ftg -> ftg_deselect;

	ftam_diag (ftde -> ftde_diags, ftde -> ftde_ndiag, 1,
		ftde -> ftde_action);
	ftam_chrg (&ftde -> ftde_charges);
    }

    FTGFREE (ftg);
    return OK;

you_lose: ;
    FTGFREE (ftg);
    return NOTOK;
}
