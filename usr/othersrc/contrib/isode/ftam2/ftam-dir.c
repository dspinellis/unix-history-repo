/* ftam-dir.c - interactive initiator FTAM -- directory management */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftam-dir.c,v 7.1 91/02/22 09:23:39 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftam-dir.c,v 7.1 91/02/22 09:23:39 mrose Interim $
 *
 *
 * $Log:	ftam-dir.c,v $
 * Revision 7.1  91/02/22  09:23:39  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:54:16  mrose
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

/*    DATA */

#ifndef	BRIDGE
static char *lcwd = NULL;
#endif


int	fdffnx ();


#ifndef	SYS5
char   *getcwd ();
#endif

/*  */

#ifndef	BRIDGE
int	f_lcd (vec)
char  **vec;
{
    char   *cp,
            cwd[MAXPATHLEN];

    if (*++vec == NULL)
	cp = strdup (myhome);
    else
	if ((cp = xglob1val (*vec, 0)) == NULL)
	    return OK;

    if (chdir (cp) == NOTOK)
	advise (cp, "unable to change to");
    else {
	if (lcwd)
	    free (lcwd);

	if (getcwd (cwd, MAXPATHLEN) && strcmp (cp, cwd))
#ifdef	apollo
	    printf ("/%s\n", lcwd = strdup (cwd));	/* network root */
#else
	    printf ("%s\n", lcwd = strdup (cwd));
#endif
	else
	    lcwd = cp, cp = NULL;
    }

    if (cp)
	free (cp);

    return OK;
}
#endif

/*  */

int	f_cd (vec)
char  **vec;
{
    int	    silent;
    char   *cp,
	    cwd[MAXPATHLEN];

    silent = strcmp (*vec, "sd") == 0;

    cp = *++vec;

    switch (realstore) {
	case RFS_UNKNOWN: 
	    if (!silent)
		advise (NULLCP, rs_unknown);
#ifndef	BRIDGE
	    else
		if (rcwd) {
		    free (rcwd);
		    rcwd = NULL;
		}
#endif
	    return OK;

	case RFS_UNIX: 
	    break;

	default: 
	    if (!silent)
		advise (NULLCP, "%s", rs_support);
	    return OK;
    }

    if (cp) {
	if ((cp = xglob1val (cp, 1)) == NULL
		|| isdir (cp, cwd, silent) == NOTOK)
	    return OK;

	if (cwd[0]) {
	    free (cp);
	    cp = strdup (cwd);
	}
    }

    if (rcwd)
	free (rcwd);
    rcwd = cp;

    return OK;
}

/*  */

int	isdir (dir, dp, silent)
char   *dir,
       *dp;
int	silent;
{
    int	    result;
    struct FTAMgroup    ftgs;
    register struct FTAMgroup  *ftg = &ftgs;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;
    struct vfsmap *vf = &vfs[VFS_FDF];

    result = OK;
    if (dp)
	*dp = NULL;

    if (!vf -> vf_oid || !(units & FUNIT_LIMITED))
	return result;

    bzero ((char *) ftg, sizeof *ftg);
    ftg -> ftg_flags |= FTG_BEGIN | FTG_END;
    ftg -> ftg_threshold = 0;

    ftg -> ftg_flags |= FTG_SELECT;
    {
	register struct FTAMselect *ftse = &ftg -> ftg_select;
	register struct FTAMattributes *fa = &ftse -> ftse_attrs;

	fa -> fa_present = FA_FILENAME;
	fa -> fa_nfile = 0;
	fa -> fa_files[fa -> fa_nfile++] = dir;

	ftse -> ftse_access = FA_PERM_READATTR;
	FCINIT (&ftse -> ftse_conctl);
    }
    ftg -> ftg_threshold++;

    ftg -> ftg_flags |= FTG_RDATTR;
    {
	register struct FTAMreadattr   *ftra = &ftg -> ftg_readattr;

	ftra -> ftra_attrnames = FA_FILENAME | FA_CONTENTS;
    }
    ftg -> ftg_threshold++;

    ftg -> ftg_flags |= FTG_DESELECT;
    ftg -> ftg_threshold++;

    if (FManageRequest (ftamfd, ftg, fti) == NOTOK) {
	if (!silent)
	    ftam_advise (&fti -> fti_abort, "F-MANAGE.REQUEST");
	return NOTOK;
    }

    ftg = &fti -> fti_group;

    if (ftg -> ftg_flags & FTG_SELECT) {
	register struct FTAMselect *ftse = &ftg -> ftg_select;

	if (!silent)
	    ftam_diag (ftse -> ftse_diags, ftse -> ftse_ndiag, 1,
			ftse -> ftse_action);
	if (ftse -> ftse_state != FSTATE_SUCCESS)
	    goto you_lose;
    }

    if (ftg -> ftg_flags & FTG_RDATTR) {
	register struct FTAMreadattr   *ftra = &ftg -> ftg_readattr;
	register struct FTAMattributes *fa = &ftra -> ftra_attrs;

	if (!silent)
	    ftam_diag (ftra -> ftra_diags, ftra -> ftra_ndiag, 1,
		ftra -> ftra_action);
	if (ftra -> ftra_action != FACTION_SUCCESS)
	    fa -> fa_present = 0;

	if ((fa -> fa_present & FA_FILENAME) && fa -> fa_nfile == 1 && dp)
	    (void) strcpy (dp, fa -> fa_files[0]);

	if (fa -> fa_present & FA_CONTENTS)
	    if (oid_cmp (vf -> vf_oid, fa -> fa_contents)) {
		if (!silent)
		    advise (NULLCP, "not a directory");
		goto you_lose;
	    }
	    else
		result = DONE;
    }

    if (ftg -> ftg_flags & FTG_DESELECT) {
	register struct FTAMdeselect   *ftde = &ftg -> ftg_deselect;

	if (!silent) {
	    ftam_diag (ftde -> ftde_diags, ftde -> ftde_ndiag, 1,
		ftde -> ftde_action);
	    ftam_chrg (&ftde -> ftde_charges);
	}
    }

    FTGFREE (ftg);
    return result;

you_lose: ;
    FTGFREE (ftg);
    return NOTOK;
}

/*  */

/* ARGSUSED */

int	f_pwd (vec)
char  **vec;
{
#ifndef	BRIDGE
    char    cwd[MAXPATHLEN];

    if (lcwd == NULL)
	lcwd = strdup (getcwd (cwd, MAXPATHLEN) ? cwd : ".");
#ifdef	apollo
    printf ("local directory: /%s\n", lcwd);	/* network root */
#else
    printf ("local directory: %s\n", lcwd);
#endif
#endif

    if (rcwd)
#ifndef	BRIDGE
	printf ("virtual filestore directory: %s\n", rcwd);
#else
	(void) sprintf (ftam_error, "virtual filestore directory: %s\n", rcwd);
#endif

    return OK;
}

/*  */

char   *str2file (s)
char   *s;
{
    register char  *bp;
    static int  i = 0;
    static char buffer1[BUFSIZ],
                buffer2[BUFSIZ];

    if (!rcwd)
	return s;

    bp = (i++ % 2) ? buffer1 : buffer2;

    switch (realstore) {
	case RFS_UNIX:
	    if (*s == '/' || *s == '~')
		return s;

#ifdef apollo
            if (strcmp (rcwd, "/") == 0 || strcmp (rcwd, "//") == 0)
		(void) sprintf (bp, "%s%s", rcwd, s);
            else
#endif                
	    (void) sprintf (bp, "%s/%s", rcwd, s);
	    break;

#ifdef	BRIDGE
	default:
	    return s;
#endif
    }

    return bp;
}
