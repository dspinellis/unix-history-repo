/* ftam-get.c - interactive initiator FTAM -- "get" */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftam-get.c,v 7.6 91/02/22 09:23:40 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftam-get.c,v 7.6 91/02/22 09:23:40 mrose Interim $
 *
 *
 * $Log:	ftam-get.c,v $
 * Revision 7.6  91/02/22  09:23:40  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/12/23  18:39:59  mrose
 * update
 * 
 * Revision 7.4  90/11/21  11:30:23  mrose
 * sun
 * 
 * Revision 7.3  90/09/07  11:13:59  mrose
 * update
 * 
 * Revision 7.2  90/07/01  21:03:08  mrose
 * pepsy
 * 
 * Revision 7.1  90/01/11  23:48:39  mrose
 * lint
 * 
 * Revision 7.0  89/11/23  21:54:17  mrose
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


#include <errno.h>
#include <stdio.h>
#include "ftamuser.h"

/*  */

static int effector;

int	ubffnx ();
struct vfsmap *findvf ();

/*  */

int	f_get (vec)
char  **vec;
{
#ifndef	BRIDGE
    int     sglobbed;
    register char  *bp;
    register char  *dst,
                  **gp,
                  **src;
    char   *freedst = NULL,
	    buffer[BUFSIZ];
    struct stat st;
#endif
    struct FADUidentity faduids;
    register struct FADUidentity   *faduid = &faduids;
    struct vfsmap  *vf = &vfs[tmode];

    if (vf == &vfs[VFS_DEF]
	    && (!(units & FUNIT_LIMITED)
		    || (class != FCLASS_MANAGE && class != FCLASS_TM))) {
	advise (NULLCP,
		"unable to use \"default\" document type on this association");
	return OK;
    }

#ifdef	BRIDGE
    if (*++vec == NULL)
	return NOTOK;
#else
    if (*++vec == NULL) {
	if (getline ("source: ", buffer) == NOTOK || str2vec (buffer, vec) < 1)
	    return OK;
	dst = NULL;
    }
    else {
	register char **ap;

	for (ap = vec; *ap; ap++)
	    continue;
	if (--ap != vec)
	    dst = *ap, *ap = NULL;
	else
	    dst = NULL;
    }
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
		break;

	    case 1:
		dst = *vec;
		break;

	    default:
		advise (NULLCP, "too many destinations");
		goto out;
	}
    }
    if (dst && !(dst = freedst = xglob1val (dst, 0)))
	goto out;
#endif

    faduid -> fa_type = FA_FIRSTLAST;
    faduid -> fa_firstlast = FA_FIRST;

#ifdef	BRIDGE
    return getvf (*vec, *vec, faduid, vf, ubffnx);
#else
    if (src[1] == NULL) {
	if (interrupted)
	    goto out;

	if (dst == NULL) {
	    switch (realstore) {
		case RFS_UNIX: 
		    if (dst = rindex (*src, '/'))
			dst++;
		    if (dst == NULL || *dst == NULL)
			dst = *src;
		    break;

		default: 
		    dst = *src;
		    break;
	    }

ask_it: ;
	    if (query)
		switch (ask ("get %s %s", *src, dst)) {
		    case NOTOK:
			goto out;

		    case OK:
		    default:
			break;

		    case DONE:
			goto out;
		}
	}
	else
	    if (stat (dst, &st) != NOTOK
		    && (st.st_mode & S_IFMT) == S_IFDIR) {
#ifdef apollo
                if (*dst == '/')
		    (void) sprintf (bp = buffer, "%s", dst);
                else
#endif
		(void) sprintf (bp = buffer, "%s/", dst);
		bp += strlen (bp);
		switch (realstore) {
		    case RFS_UNIX:
			if (dst = rindex (*src, '/'))
			    dst++;
			if (dst == NULL || *dst == NULL)
			    dst = *src;
			break;

		    default:
			break;
		}
		(void) strcpy (bp, dst);
		dst = buffer;
		goto ask_it;
	    }

	if (check_get (dst) != NOTOK)
	    (void) getvf (*src, dst, faduid, vf, ubffnx);
	goto out;
    }

    switch (realstore) {
	case RFS_UNKNOWN:
	    advise (NULLCP, "%s", rs_unknown);
	    goto out;

	case RFS_UNIX:
#ifdef apollo
            if (dst && dst[strlen (dst) - 1] == '/')
		(void) sprintf (bp = buffer, "%s", dst);
            else
#endif
	    (void) sprintf (bp = buffer, "%s/", dst ? dst : ".");
	    bp += strlen (bp);
	    break;

	default:
	    advise (NULLCP, "%s", rs_support);
	    goto out;
    }

    if (stat (buffer, &st) == NOTOK) {
	advise (dst, "unable to stat");
	goto out;
    }
    if ((st.st_mode & S_IFMT) != S_IFDIR) {
	advise (NULLCP, "%s: not a directory");
	goto out;
    }

    for (gp = src; *gp && !interrupted; gp++) {
	switch (realstore) {
	    case RFS_UNIX:
		if (dst = rindex (*gp, '/'))
		    dst++;
		if (dst == NULL || *dst == NULL)
		    dst = *gp;
		break;

	    default:
		dst = *gp;
		break;
	}
	(void) strcpy (bp, dst);
	dst = buffer;

	if (sglobbed) {
	    if (query)
		switch (ask ("get %s %s", *gp, dst)) {
		    case NOTOK:
		        continue;

		    case OK:
		    default:
			break;

		    case DONE:
			goto out;
		}
	    else
		advise (NULLCP, "get %s %s", *gp, dst);
	}

	if (check_get (dst) == NOTOK)
	    break;
	(void) getvf (*gp, dst, faduid, vf, ubffnx);

	if (ftamfd == NOTOK)
	    break;
    }

out: ;
    blkfree (src);
    if (freedst)
	free (freedst);

    return OK;
#endif
}

/*  */

#ifndef	BRIDGE
static int  check_get (dst)
char   *dst;
{
    int	    result;
    register char  *cp;

    if ((result = access (dst, W_OK)) == NOTOK && errno == ENOENT)
	if (cp = rindex (dst, '/')) {
	    *cp = NULL;
	    result = access (*dst ? dst : "/", W_OK);
	    *cp = '/';
	}
	else
	    result = access (".", W_OK);
    if (result == OK)
	return result;

    advise (dst, "unable to write");

    return result;
}
#endif

/*  */

int	getvf (src, dst, faduid, vf, wfnx)
char   *src,
       *dst;
register struct FADUidentity *faduid;
register struct vfsmap *vf;
IFP	wfnx;
{
    int	    fd,
	    result;
#ifdef	BRIDGE
    int     ftp_result;
#endif
    PE	    param;
    struct FTAMgroup    ftgs;
    register struct FTAMgroup  *ftg = &ftgs;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;
    register struct FTAMabort *fta = &fti -> fti_abort;

    if (vf == &vfs[VFS_DEF]) {
	if (!(vf = findvf (src))) {
	    vf = &vfs[VFS_UBF];
	    advise (NULLCP,
		"unable to determine document type associated with %s", src);
	    /* most likely reason: the file isn't there... */

	    if (vf -> vf_oid == NULLOID || !(vf -> vf_flags & VF_OK))
		return NOTOK;
	    advise (NULLCP, "proposing %s transfer", vf -> vf_text);
	}
	else
	    if (vf == &vfs[VFS_FDF]) {
		advise (NULLCP, "%s is a %s", src, vf -> vf_text);
		return NOTOK;
	    }
    }
    else
	if (vf -> vf_oid == NULLOID || !(vf -> vf_flags & VF_OK)) {
	    advise (NULLCP, "no support for %ss", vf -> vf_text);
	    return NOTOK;
	}

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

	ftse -> ftse_access = FA_PERM_READ;
	FCINIT (&ftse -> ftse_conctl);
    }
    ftg -> ftg_threshold++;

    param = NULLPE;

    ftg -> ftg_flags |= FTG_OPEN;
    {
	register struct FTAMopen *ftop = &ftg -> ftg_open;

	ftop -> ftop_mode = FA_PERM_READ;
	ftop -> ftop_contents = vf -> vf_oid;
	if (vf -> vf_parameter) {
	    if (enc_f (vf -> vf_number, &_ZDOCS_mod, &param, 1, 0, NULLCP,
		      vf -> vf_parameter) == NOTOK) {
		advise (NULLCP, "unable to build document type parameter: %s",
			PY_pepy);
		return NOTOK;
	    }
	    ftop -> ftop_parameter = param;
	}	
	FCINIT (&ftop -> ftop_conctl);
	if (concurrency)
	    ftop -> ftop_conctl.fc_readlock = FLOCK_SHARED;
    }
    ftg -> ftg_threshold++;

    result = FBulkBeginRequest (ftamfd, ftg, fti);

    if (param)
	pe_free (param);

    if (result == NOTOK) {
	ftam_advise (&fti -> fti_abort, "F-BULK-BEGIN.REQUEST");
	return NOTOK;
    }

    ftg = &fti -> fti_group;

    if (ftg -> ftg_flags & FTG_SELECT) {
	register struct FTAMselect *ftse = &ftg -> ftg_select;

	ftam_diag (ftse -> ftse_diags, ftse -> ftse_ndiag, 1,
		ftse -> ftse_action);
	if (ftse -> ftse_state != FSTATE_SUCCESS)
	    goto you_lose;
    }

    if (ftg -> ftg_flags & FTG_OPEN) {
	register struct FTAMopen *ftop = &ftg -> ftg_open;

	ftam_diag (ftop -> ftop_diags, ftop -> ftop_ndiag, 1,
		ftop -> ftop_action);
	if (ftop -> ftop_state != FSTATE_SUCCESS)
	    goto you_lose;

	for (myvf = vfs; myvf -> vf_entry; myvf++)
	    if (oid_cmp (myvf -> vf_oid, ftop -> ftop_contents) == 0)
		break;
	switch (myvf - vfs) {
	    case VFS_UBF:
	    case VFS_UTF:
	        effector = 1;
	        if (ftop -> ftop_parameter && myvf -> vf_number >= 0) {
		    caddr_t parm = NULL;

		    if (dec_f (myvf -> vf_number, &_ZDOCS_mod,
			      ftop -> ftop_parameter, 1, NULLIP, NULLVP,
			       (char **) &parm) == NOTOK)
			advise (NULLCP,
				"unable to parse document type parameter: %s",
				PY_pepy);
		    else
			switch (myvf - vfs) {
			    case VFS_UTF:
				{
				    PElementID	id;
				    register struct type_DOCS_FTAM__1__Parameters *p1 =
					(struct type_DOCS_FTAM__1__Parameters *)
			    				parm;

				    if (p1 -> optionals
					    & opt_DOCS_FTAM__1__Parameters_universal__class__number)
					id = (PElementID)
						p1 -> universal__class__number;
				    else
					id = PE_DEFN_GFXS;
				    switch (id) {
					case PE_DEFN_GFXS:
					    if (getenv ("HP-FTAM")) {
						effector = 1;
						break;
					    }	/* else fall... */
					case PE_DEFN_PRTS:
					case PE_DEFN_VISS:
					    effector = 0;
					    break;

				        case PE_DEFN_T61S:
					case PE_DEFN_VTXS:
					case PE_DEFN_IA5S:
					case PE_DEFN_GENS:
					    effector = 1;
					    break;

					default:
					    break;
				    }
				}
				break;

			    case VFS_UBF:
			    default:
			        break;
			}
		    if (parm)
			(void) fre_obj (parm,
					_ZDOCS_mod.md_dtab[myvf -> vf_number],
					&_ZDOCS_mod, 1);
		}
		if (debug)
		    advise (NULLCP, "effector=%d", effector);
		/* and fall... */
	    case VFS_FDF:
		if (myvf != vf || watch) {
		    advise (NULLCP, "%s transfer", myvf -> vf_text);
		    vf = myvf;
		}
		break;

	    default:
		vf = &vfs[VFS_UBF];
		advise (NULLCP, "document type mismatch; assuming %s (%s)",
				vf -> vf_text, vf -> vf_entry);
		break;
	}
    }
    myvf = vf;

    FTGFREE (ftg);

    if (FReadWriteRequest (ftamfd, FA_OPS_READ, faduid, myvf -> vf_context,
		NOTOK, 0, fti) == NOTOK) {
	ftam_advise (fta, "F-READWRITE.REQUEST");
	return NOTOK;
    }

    if (dst) {
#ifdef	BRIDGE
	if ((fd = dataconn(dst)) == NOTOK) {
#else
	if ((fd = open (dst, O_WRONLY | O_CREAT | O_TRUNC, 0666)) == NOTOK) {
#endif
	    struct FTAMdiagnostic   diags[NFDIAG];
	    register struct FTAMdiagnostic *dp = diags;

	    advise (dst, "unable to write");
#ifdef	BRIDGE
	    ftp_result = DONE;
#endif

	    dp -> ftd_type = DIAG_PERM;
	    dp -> ftd_identifier = FS_ACC_LCL;
	    dp -> ftd_observer = dp -> ftd_source = EREF_IFSU;
	    dp -> ftd_delay = DIAG_NODELAY;
	    (void) sprintf (dp -> ftd_data, "unable to write %s: %s",
		    dst, sys_errname (errno));
	    dp -> ftd_cc = strlen (dp -> ftd_data);
	    dp++;

	    if (FCancelRequest (ftamfd, FACTION_PERM, NULLPE, diags,
				dp - diags, fti) == NOTOK) {
		ftam_advise (fta, "F-CANCEL.REQUEST");
		if (fd != NOTOK)
		    (void) close (fd);
		return NOTOK;
	    }

	    if (fti -> fti_type == FTI_CANCEL) {
		register struct FTAMcancel *ftcn = &fti -> fti_cancel;

		ftam_diag (ftcn -> ftcn_diags, ftcn -> ftcn_ndiag, 1,
			ftcn -> ftcn_action);
		FTCNFREE (ftcn);
	    }

	    goto done_transfer;
	}
    }
    else
	fd = NOTOK;

    if (fd != NOTOK)
	(*wfnx) (fd, (struct PSAPdata *) 0, OK);

    result = getloop (fd, dst, wfnx);

    if (fd != NOTOK) {
	(*wfnx) (fd, (struct PSAPdata *) 0, DONE);
	(void) close (fd);
    }
    
    switch (result) {
	case NOTOK: 
	    return NOTOK;

	case OK: 
	default: 
	    break;

	case DONE: 
	    goto done_transfer;
    }

    if (FTransEndRequest (ftamfd, NULLPE, fti) == NOTOK) {
	ftam_advise (fta, "F-TRANSFER-END.REQUEST");
	return NOTOK;
    }

    switch (fti -> fti_type) {
	case FTI_TRANSEND:
	    {
		register struct FTAMtransend *ftre = &fti -> fti_transend;

		ftam_diag (ftre -> ftre_diags, ftre -> ftre_ndiag, 1,
			   ftre -> ftre_action);
		FTREFREE (ftre);
	    }
	    break;

	case FTI_CANCEL:
	    {
		register struct FTAMcancel *ftcn = &fti -> fti_cancel;
		
		advise (NULLCP, "data transfer cancelled!");
		ftam_diag (ftcn -> ftcn_diags, ftcn -> ftcn_ndiag, 1, 
			ftcn -> ftcn_action);
		FTCNFREE (ftcn);

		if (FCancelResponse (ftamfd, FACTION_SUCCESS, NULLPE, 
			    (struct FTAMdiagnostic *) 0, 0, fti)
			== NOTOK) {
		    ftam_advise (fta, "F-CANCEL.RESPONSE");
		    return NOTOK;
		}
	    }
	    break;

	default:
 	    adios (NULLCP, "unexpected indication type=%d", fti -> fti_type);
    }
#ifdef	BRIDGE
    ftp_result = OK;
#endif

done_transfer: ;
    ftg = &ftgs;
    bzero ((char *) ftg, sizeof *ftg);
    ftg -> ftg_flags |= FTG_BEGIN | FTG_END;
    ftg -> ftg_threshold = 0;

    ftg -> ftg_flags |= FTG_CLOSE;
    ftg -> ftg_threshold++;

    ftg -> ftg_flags |= FTG_DESELECT;
    ftg -> ftg_threshold++;

    if (FBulkEndRequest (ftamfd, ftg, fti) == NOTOK) {
	ftam_advise (fta, "F-BULK-END.REQUEST");
	return NOTOK;
    }

    ftg = &fti -> fti_group;

    if (ftg -> ftg_flags & FTG_CLOSE) {
	register struct FTAMclose     *ftcl = &ftg -> ftg_close;

	ftam_diag (ftcl -> ftcl_diags, ftcl -> ftcl_ndiag, 1,
		ftcl -> ftcl_action);
    }

    if (ftg -> ftg_flags & FTG_DESELECT) {
	register struct FTAMdeselect   *ftde = &ftg -> ftg_deselect;

	ftam_diag (ftde -> ftde_diags, ftde -> ftde_ndiag, 1,
		ftde -> ftde_action);
	ftam_chrg (&ftde -> ftde_charges);
    }

    FTGFREE (ftg);
#ifdef	BRIDGE
    return ftp_result;
#else
    return OK;
#endif

you_lose: ;
    FTGFREE (ftg);
    return NOTOK;
}

/*  */

static struct vfsmap *findvf (file)
char   *file;
{
    struct FTAMgroup    ftgs;
    register struct FTAMgroup  *ftg = &ftgs;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;
    struct vfsmap *vf;

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
	ftam_advise (&fti -> fti_abort, "F-MANAGE.REQUEST");
	return NULL;
    }

    ftg = &fti -> fti_group;

    if (ftg -> ftg_flags & FTG_SELECT) {
	register struct FTAMselect *ftse = &ftg -> ftg_select;

	if (debug)
	    ftam_diag (ftse -> ftse_diags, ftse -> ftse_ndiag, 1,
		    ftse -> ftse_action);
	if (ftse -> ftse_state != FSTATE_SUCCESS)
	    goto you_lose;
    }

    vf = NULL;
    if (ftg -> ftg_flags & FTG_RDATTR) {
	register struct FTAMreadattr   *ftra = &ftg -> ftg_readattr;
	register struct FTAMattributes *fa = &ftra -> ftra_attrs;

	if (debug)
	    ftam_diag (ftra -> ftra_diags, ftra -> ftra_ndiag, 1,
		    ftra -> ftra_action);
	if (ftra -> ftra_action != FACTION_SUCCESS)
	    fa -> fa_present = 0;

	if (fa -> fa_present & FA_CONTENTS) {
	    for (vf = vfs; vf -> vf_entry; vf++)
		if (vf -> vf_oid
			&& (vf -> vf_flags & VF_OK)
			&& oid_cmp (vf -> vf_oid, fa -> fa_contents) == 0) {
		    if (fa -> fa_parameter && vf -> vf_number >= 0) {
			if (vf -> vf_parameter && (vf -> vf_flags & VF_PARM))
			    (void) fre_obj (vf -> vf_parameter,
					    _ZDOCS_mod.md_dtab[vf
								-> vf_number],
					    &_ZDOCS_mod, 1);
			vf -> vf_parameter = NULL, vf -> vf_flags &= ~VF_PARM;
			if (dec_f (vf -> vf_number, &_ZDOCS_mod,
				   fa -> fa_parameter, 1, NULLIP, NULLVP,
				   &vf -> vf_parameter) == NOTOK) {
			    advise (NULLCP,
				 "unable to parse document type parameter: %s",
				    PY_pepy);
			    vf = NULL;
			}
			else
			    if (vf -> vf_check
				    && (*vf -> vf_check) (vf -> vf_parameter,
							  PY_pepy) == NOTOK) {
				advise (NULLCP, "%s", PY_pepy);
				vf = NULL;
			    }
		    }
		    break;
		}
	    if (!vf -> vf_entry) {
		advise (NULLCP,
			"unknown document type %s associated with %s",
			sprintoid (fa -> fa_contents), file);
		vf = NULL;
	    }
	}
    }

    if (ftg -> ftg_flags & FTG_DESELECT) {
	register struct FTAMdeselect   *ftde = &ftg -> ftg_deselect;

	if (debug) {
	    ftam_diag (ftde -> ftde_diags, ftde -> ftde_ndiag, 1,
		ftde -> ftde_action);
	    ftam_chrg (&ftde -> ftde_charges);
	}
    }

    FTGFREE (ftg);
    return vf;

you_lose: ;
    FTGFREE (ftg);
    return NULL;
}

/*  */

static int  getloop (fd, dst, wfnx)
int	fd;
char   *dst;
IFP	wfnx;
{
    int	    reason,
	    result;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;
    register struct FTAMabort  *fta = &fti -> fti_abort;

    for (;;) {
	if (!interrupted) {
	    int     nfds;
	    fd_set  rfds;

	    nfds = 0;
	    FD_ZERO (&rfds);
					/* interrupt causes EINTR */
	    if (FSelectMask (ftamfd, &rfds, &nfds, fti) == OK)
		(void) xselect (nfds, &rfds, NULLFD, NULLFD, NOTOK);
	}

	if (interrupted) {
	    advise (NULLCP, "cancelling transfer");
	    reason = FS_GEN_INITIATOR;
	    errno = EINTR;
	    goto do_cancel;
	}
	
	switch (result = FWaitRequest (ftamfd, NOTOK, fti)) {
	    case NOTOK: 
		ftam_advise (&fti -> fti_abort, "F-WAIT.REQUEST");
		return NOTOK;

	    case OK: 
	    case DONE: 
		break;

	    default: 
		adios (NULLCP, "unknown return from FWaitRequest=%d",
			result);
	}

	switch (fti -> fti_type) {
	    case FTI_DATA: 
		if ((*wfnx) (fd, &fti -> fti_data, OK) == NOTOK) {
		    struct FTAMdiagnostic   diags[NFDIAG];
		    register struct FTAMdiagnostic *dp;

		    advise (dst, "error writing");
		    reason = FS_ACC_WRITE;

do_cancel: ;
		    dp = diags;

		    dp -> ftd_type = DIAG_PERM;
		    dp -> ftd_identifier = reason;
		    dp -> ftd_observer = dp -> ftd_source = EREF_IFSU;
		    dp -> ftd_delay = DIAG_NODELAY;
		    (void) strcpy (dp -> ftd_data, sys_errname (errno));
		    dp -> ftd_cc = strlen (dp -> ftd_data);
		    dp++;

		    if (FCancelRequest (ftamfd, FACTION_PERM, NULLPE, diags,
			    dp - diags, fti) == NOTOK) {
			ftam_advise (fta, "F-CANCEL.REQUEST");
			return NOTOK;
		    }

		    if (fti -> fti_type == FTI_CANCEL) {
			register struct FTAMcancel *ftcn = &fti -> fti_cancel;

			ftam_diag (ftcn -> ftcn_diags, ftcn -> ftcn_ndiag, 1,
				    ftcn -> ftcn_action);
			FTCNFREE (ftcn);
		    }

		    return DONE;
		}
		break;

	    case FTI_DATAEND: 
		return OK;

	    case FTI_CANCEL: 
		{
		    register struct FTAMcancel *ftcn = &fti -> fti_cancel;
		
		    advise (NULLCP, "data transfer cancelled!");
		    ftam_diag (ftcn -> ftcn_diags, ftcn -> ftcn_ndiag, 1,
			    ftcn -> ftcn_action);
		    FTCNFREE (ftcn);

		    if (FCancelResponse (ftamfd, FACTION_SUCCESS, NULLPE, 
				(struct FTAMdiagnostic *) 0, 0, fti)
			    == NOTOK) {
			ftam_advise (fta, "F-CANCEL.RESPONSE");
			return NOTOK;
		    }
		}
		return DONE;

	    default: 
		adios (NULLCP, "unexpected indication type=%d",
			fti -> fti_type);
	}
    }
}

/*  */

static int  ubffnx (fd, px, status)
int	fd;
register struct PSAPdata *px;
int	status;
{
    register int    i,
		    n;
    register PE	    pe,
		   *pep;
    static int	    cc;

    if (px == NULL) {
	switch (status) {
	    case OK:
	    default:
		cc = 0;
		if (verbose)
		    timer (cc, NULLCP);
		if (hash)
		    marks = BUFSIZ - 1;
		break;

	    case DONE:
		if (verbose)
		    timer (cc, "received");
		break;
	}

	return OK;
    }

    for (pep = px -> px_info, i = px -> px_ninfo - 1; i >= 0; pep++, i--) {
	if ((pe = *pep) == NULLPE)
	    continue;

	switch (myvf - vfs) {
	    case VFS_UBF:
	    default:
		if (debug)
		    WATCHP (DOCS_FTAM__3__Datatype1, pe, 1);
		n = de2fd (fd, pe, 0, 0);
		break;

	    case VFS_UTF:
		if (debug)
		    WATCHP (DOCS_FTAM__1__Datatype1, pe, 1);
		n = de2fd (fd, pe, 1, effector);
		break;
	}
	if (n == NOTOK)
	    break;

	if (verbose || hash)
	    cc += n;
	if (hash) {
	    if (hash > 1)
		printf ("%d\r", cc);
	    else
		for (; marks < cc; marks += BUFSIZ)
		    (void) putchar ('#');
	    (void) fflush (stdout);
	}
    }

    PXFREE (px);

    return n;
}
