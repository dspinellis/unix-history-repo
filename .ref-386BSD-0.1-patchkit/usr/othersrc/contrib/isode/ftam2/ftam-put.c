/* ftam-put.c - interactive initiator FTAM -- "put" */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftam-put.c,v 7.9 91/02/22 09:23:48 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftam-put.c,v 7.9 91/02/22 09:23:48 mrose Interim $
 *
 *
 * $Log:	ftam-put.c,v $
 * Revision 7.9  91/02/22  09:23:48  mrose
 * Interim 6.8
 * 
 * Revision 7.8  90/12/23  18:40:03  mrose
 * update
 * 
 * Revision 7.7  90/11/21  11:30:33  mrose
 * sun
 * 
 * Revision 7.6  90/09/07  11:14:04  mrose
 * update
 * 
 * Revision 7.5  90/08/29  15:02:20  mrose
 * fixes
 * 
 * Revision 7.4  90/08/14  14:28:31  mrose
 * T1
 * 
 * Revision 7.3  90/07/01  21:03:16  mrose
 * pepsy
 * 
 * Revision 7.2  90/01/16  22:37:17  mrose
 * very last time
 * 
 * Revision 7.1  90/01/11  18:35:43  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  21:54:24  mrose
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

int	f_put (vec)
char  **vec;
{
    int     append;
#ifdef	BRIDGE
    int     result;
    register char  *dst;
#else
    int	    sglobbed;
    register char  *bp,
                   *dst,
                  **gp,
                  **src;
    char   *freedst = NULL,
	    buffer[BUFSIZ];
#endif

    append = strcmp (*vec, "append") == 0;

    if (*++vec == NULL) {
#ifdef	BRIDGE
	return NOTOK;
#else
	if (getline ("source: ", buffer) == NOTOK || str2vec (buffer, vec) < 1)
	    return OK;
	dst = NULL;
#endif
    }
    else {
#ifdef	BRIDGE
	dst = *vec;
#else
	register char **ap;

	for (ap = vec; *ap; ap++)
	    continue;
	if (--ap != vec)
	    dst = *ap, *ap = NULL;
	else
	    dst = NULL;
#endif
    }
#ifndef	BRIDGE
    if (!(src = xglob (vec, 0)))
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
	    dst = str2file (dst);

ask_it: ;
	    if (query)
		switch (ask ("%s %s %s", append ? "append" : "put", *src,
			dst)) {
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
		    goto ask_it;

		default:
		    break;
	    }

	dst = str2file (dst);
	(void) put (*src, dst, append);
	goto out;
    }

    switch (realstore) {
	case RFS_UNKNOWN: 
	    advise (NULLCP, "%s", rs_unknown);
	    goto out;

	case RFS_UNIX: 
	    if (dst)
#ifdef apollo
                if (*(bp = str2file (dst)) == '/') {
		    (void) strcpy (buffer, bp);
		    bp = buffer;
		}
                else
#endif
		(void) sprintf (bp = buffer, "%s/", str2file (dst));
	    else
		if (rcwd)
		    (void) sprintf (bp = buffer, "%s", str2file (""));
		else
		    (void) strcpy (bp = buffer, "./");
	    bp += strlen (bp);
	    break;

	default: 
	    advise (NULLCP, "%s", rs_support);
	    goto out;
    }

    if (isdir (str2file (buffer), NULLCP, 0) == NOTOK)
	goto out;

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
	dst = str2file (buffer);

	if (sglobbed) {
	    if (query)
		switch (ask ("%s %s %s", append ? "append" : "put", *gp, dst)){
		    case NOTOK:
		        continue;

		    case OK:
		    default:
			break;

		    case DONE:
			goto out;
		}
	    else
		advise (NULLCP, "%s %s %s", append ? "append" : "put", *gp,
			dst);
	}

	(void) put (*gp, dst, append);

	if (ftamfd == NOTOK)
	    break;
    }

out: ;
    blkfree (src);
    if (freedst)
	free (freedst);

    return OK;
#else
    result = put (dst, append);
    return result;
#endif
}

/*  */

#ifdef	BRIDGE
static int  put (dst, append)
char   *dst;
#else
static int  put (src, dst, append)
char   *src,
       *dst;
#endif
int	append;
{
    int     bsize,
            fd,
	    magic,
            result,
            size;
    PE	    pe;
#ifndef	BRIDGE
    struct stat st;
#endif
    struct vfsmap  *vf;

#ifdef	BRIDGE
    if ((fd = dataconn (dst)) == NOTOK) {
	advise (dst, "unable to open");
	return NOTOK;
    }
#else
    if ((fd = open (src, O_RDONLY)) == NOTOK) {
	advise (src, "unable to open");
	return NOTOK;
    }
#endif

#ifndef	BRIDGE
    if (fstat (fd, &st) == NOTOK) {
	advise (src, "unable to fstat");
you_lose: ;
	(void) close (fd);
	return NOTOK;
    }
    if ((st.st_mode & S_IFMT) != S_IFREG) {
	advise (NULLCP, "%s: not a regular file", src);
	goto you_lose;
    }
#endif

#ifdef	BRIDGE
    vf = &vfs[tmode];
#else
    if ((vf = st2vfs (fd, src, &st, vfs[tmode].vf_oid, ftamfd)) == NULL) {
	advise (NULLCP, "unable to determine document type for %s", src);
	goto you_lose;
    }
    if (vf == &vfs[VFS_FDF]) {
	advise (NULLCP, "%s is a %s", src, vf -> vf_text);
	goto you_lose;
    }
#endif
    if (tmode != vf - vfs && tmode != VFS_DEF)
	advise (NULLCP, "negotiating %s transfer", vf -> vf_text);

    if ((pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_OCTS))
	    == NULLPE) {
	advise (NULLCP, "out of memory");
#ifdef	BRIDGE
you_lose: ;
	(void) close (fd);
	return NOTOK;
#else
	goto you_lose;
#endif
    }

    switch (vf - vfs) {
	case VFS_UTF: 
	    if ((magic = fadusize - MAGIC_OCTET1) < 0)
		magic = 0;
	    break;

	case VFS_UBF:
	default:
	    if ((magic = fadusize - MAGIC_SINGLE) < 0)
		magic = 0;
	    break;
    }

    if (magic > 6 * 1024)	/* FTAM profile T1 or A/111 limits  to 7K */
	magic = 6 * 1024;
#ifdef	BRIDGE
    bsize = BUFSIZ << 2;
    size = magic >= bsize ? magic : bsize;
    if (size > bsize)
	size -= size % bsize;
#else
#ifndef	MAXBSIZE
    bsize = BUFSIZ;
#else
    bsize = st.st_blksize > 0 ? st.st_blksize : BUFSIZ;
#endif
    size = (1024 <= magic && magic < bsize) ? magic : bsize;
#endif
    if (watch) {
#ifndef	BRIDGE
	printf ("Selecting FADU size of %d\n", size);
	printf ("based on blksize of %d and estimated integral FADU size of %d\n",
		bsize, magic);
#endif
    }

    if ((pe -> pe_prim = PEDalloc (pe -> pe_len = size)) == NULLPED) {
	advise (NULLCP, "out of memory");
	pe_free (pe);
	goto you_lose;
    }

#ifdef	BRIDGE
    result = putaux (dst, append, fd, pe, vf, size);
#else
    result = putaux (src, dst, append, fd, pe, vf, size);
#endif

    pe_free (pe);

    (void) close (fd);

    return result;
}

/*  */

#ifdef	BRIDGE
static int  putaux (dst, append, fd, pe, vf, size)
char   *dst;
#else
static int  putaux (src, dst, append, fd, pe, vf, size)
char   *src,
       *dst;
#endif
int	append,
	fd;
PE      pe;
struct vfsmap  *vf;
int	size;
{
    register int    n;
    int     cc,
	    effector,
	    gd,
	    nc,
            reason,
	    result;
    PE	    de,
	    param;
#ifdef	BRIDGE
    char line[BUFSIZ];
#endif
    struct FADUidentity faduids;
    register struct FADUidentity   *faduid = &faduids;
    struct FTAMgroup    ftgs;
    register struct FTAMgroup  *ftg = &ftgs;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;
    register struct FTAMabort  *fta = &fti -> fti_abort;
    FILE *fp;

    pe -> pe_context = vf -> vf_id;

    param = NULLPE;
    if (vf -> vf_parameter
	    && enc_f (vf -> vf_number, &_ZDOCS_mod, &param, 1, 0, NULLCP,
		      vf -> vf_parameter) == NOTOK) {
	advise (NULLCP, "unable to build document type parameter: %s",
		PY_pepy);
	return NOTOK;
    }

    bzero ((char *) ftg, sizeof *ftg);
    ftg -> ftg_flags |= FTG_BEGIN | FTG_END;
    ftg -> ftg_threshold = 0;

    if (omode == FOVER_SELECT)
	append = 1;
    if (units & FUNIT_LIMITED) {
	ftg -> ftg_flags |= FTG_CREATE;
	{
	    register struct FTAMcreate *ftce = &ftg -> ftg_create;
	    register struct FTAMattributes *fa = &ftce -> ftce_attrs;

	    ftce -> ftce_override = append ? FOVER_SELECT : omode;

	    fa -> fa_present = FA_FILENAME;
	    fa -> fa_nfile = 0;
	    fa -> fa_files[fa -> fa_nfile++] = dst;

	    fa -> fa_present |= FA_ACTIONS;
	    fa -> fa_permitted = FA_PERM_READ | FA_PERM_REPLACE
					| FA_PERM_EXTEND | FA_PERM_READATTR
					| FA_PERM_CHNGATTR | FA_PERM_DELETE
					| FA_PERM_TRAV;

	    fa -> fa_present |= FA_CONTENTS;
	    fa -> fa_contents = vf -> vf_oid;
	    fa  -> fa_parameter = param;

	    ftce -> ftce_access = append ? FA_PERM_EXTEND : FA_PERM_REPLACE;
	    FCINIT (&ftce -> ftce_conctl);
	}
    }
    else {
	ftg -> ftg_flags |= FTG_SELECT;
	{
	    register struct FTAMselect *ftse = &ftg -> ftg_select;
	    register struct FTAMattributes *fa = &ftse -> ftse_attrs;

	    if (!append && omode == FOVER_FAIL) {
		advise (NULLCP,
			"lack of limited-file-management conflicts with setting of \"override\" variable");
		return NOTOK;
	    }

	    fa -> fa_present = FA_FILENAME;
	    fa -> fa_nfile = 0;
	    fa -> fa_files[fa -> fa_nfile++] = dst;

	    ftse -> ftse_access = append ? FA_PERM_EXTEND : FA_PERM_REPLACE;
	    FCINIT (&ftse -> ftse_conctl);
	}
    }
    
    ftg -> ftg_threshold++;

    ftg -> ftg_flags |= FTG_OPEN;
    {
	register struct FTAMopen   *ftop = &ftg -> ftg_open;

	ftop -> ftop_contents = vf -> vf_oid;
	ftop -> ftop_parameter = param;
	FCINIT (&ftop -> ftop_conctl);
	if (append) {
	    ftop -> ftop_mode = FA_PERM_EXTEND;
            if (concurrency)
		ftop -> ftop_conctl.fc_extendlock = FLOCK_EXCLUSIVE;
	}
	else {
	    ftop -> ftop_mode = FA_PERM_REPLACE;
            if (concurrency)
		ftop -> ftop_conctl.fc_replacelock = FLOCK_EXCLUSIVE;
	}
    }
    ftg -> ftg_threshold++;

    result = FBulkBeginRequest (ftamfd, ftg, fti);

    if (param)
	pe_free (param);

    if (result == NOTOK) {
	ftam_advise (fta, "F-BULK-BEGIN.REQUEST");
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
    else
	if (ftg -> ftg_flags & FTG_CREATE) {
	    register struct FTAMcreate *ftce = &ftg -> ftg_create;

	    ftam_diag (ftce -> ftce_diags, ftce -> ftce_ndiag, 1,
		       ftce -> ftce_action);
	    if (ftce -> ftce_state != FSTATE_SUCCESS)
		goto you_lose;
	}

    if (ftg -> ftg_flags & FTG_OPEN) {
	register struct FTAMopen   *ftop = &ftg -> ftg_open;

	ftam_diag (ftop -> ftop_diags, ftop -> ftop_ndiag, 1,
		ftop -> ftop_action);
	if (ftop -> ftop_state != FSTATE_SUCCESS)
	    goto you_lose;

	for (myvf = vfs; myvf -> vf_entry; myvf++)
	    if (oid_cmp (myvf -> vf_oid, ftop -> ftop_contents) == 0)
		break;
	switch (myvf - vfs) {
	    case VFS_UTF: 
		pe -> pe_id = (PElementID)
		    		    ((struct type_DOCS_FTAM__1__Parameters *)
				     			myvf -> vf_parameter)
					    -> universal__class__number;
		/* and fall... */
	    case VFS_UBF: 
	        effector = 1;
	        if (ftop -> ftop_parameter && myvf -> vf_number >= 0) {
		    caddr_t parm = NULL;

		    if (dec_f (myvf -> vf_number, &_ZDOCS_mod,
			       ftop -> ftop_parameter, 1, NULLIP, NULLVP,
			       &parm) == NOTOK)
			advise (NULLCP,
				"unable to parse document type parameter: %s",
				PY_pepy);
		    else
			switch (myvf - vfs) {
			    case VFS_UTF:
				{
				    register struct type_DOCS_FTAM__1__Parameters *p1 =
					(struct type_DOCS_FTAM__1__Parameters *)
			    				parm;

				    if (p1 -> optionals
					    & opt_DOCS_FTAM__1__Parameters_universal__class__number)
					pe -> pe_id = (PElementID)
						p1 -> universal__class__number;
				    else
					pe -> pe_id = PE_DEFN_GFXS;
				    switch (pe -> pe_id) {
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
		    advise (NULLCP, "effector=%d id=0x%x",
			    effector, pe -> pe_id);
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

    faduid -> fa_type = FA_FIRSTLAST;
    faduid -> fa_firstlast = FA_FIRST;
    if (FReadWriteRequest (ftamfd, append ? FA_OPS_EXTEND : FA_OPS_REPLACE,
		faduid, myvf -> vf_context, NOTOK, 0, fti) == NOTOK) {
	ftam_advise (fta, "F-READWRITE.REQUEST");
	return NOTOK;
    }

    switch (myvf - vfs) {
	case VFS_UTF:
	    if ((gd = dup (fd)) == NOTOK || (fp = fdopen (gd, "r")) == NULL) {
		if (gd != NOTOK)
		    (void) close (gd);

#ifdef	BRIDGE
		advise (dst, gd != NOTOK ? "fdopen failed" : "unable to dup");
#else
		advise (src, gd != NOTOK ? "fdopen failed on"
					 : "unable to dup");
#endif
		reason = FS_ACC_LCL;
		goto do_cancel;
	    }
	    break;

	case VFS_UBF:
	default: 
	    fp = NULL;
	    break;
    }

    cc = 0;
    if (verbose)
	timer (cc, NULLCP);
    if (hash)
	marks = BUFSIZ - 1;

#ifdef	BRIDGE
    line[0] = '\0';
#endif

    for (;;) {
	register char  *bp,
	               *ep;

	if (!interrupted) {
	    int	    nfds;
	    fd_set  wfds;

	    nfds = 0;
	    FD_ZERO (&wfds);
					/* interrupt causes EINTR */
	    if (FSelectMask (ftamfd, &wfds, &nfds, fti) == OK)
		(void) xselect (nfds, NULLFD, &wfds, NULLFD, NOTOK);
	}

	if (interrupted) {
	    advise (NULLCP, "cancelling transfer");

	    reason = FS_GEN_INITIATOR;
	    errno = EINTR;
	    goto do_cancel;
	}

	for (ep = (bp = (char *) pe -> pe_prim) + size - (fp ? 2 : 0), nc = 0;
		bp < ep; ) {
	    if (fp) {
		register char  *cp;

#ifdef	BRIDGE
		if (strlen (line) || fgets (line, BUFSIZ, fp)) {
		    if ((strlen(line) + 1) < (ep - bp - 1)) {
			(void) strcpy (bp, line);
			line[0] = NULL;
		    }
		    else
			break;
		}
		else {
#else
		if (fgets (bp, ep - bp + 1, fp) == NULL) {
#endif
		    n = (ferror (fp) && !feof (fp)) ? NOTOK : OK;
		    break;
		}
		cp = bp + strlen (bp) - 1;
		if (!effector) {
		    if (*cp == '\n') {
#ifndef	BRIDGE
			*cp = NULL;
#else
			if (cp > bp) {
			    if (*--cp == '\r')
				*cp = NULL;
			    else
				*++cp = NULL;
			}
			else
			    *cp = NULL;
#endif
			n = cp - bp;
			bp = cp;
		    }
		    else {			/* XXX: losing! */
			n = cp - bp + 1;
			bp = cp + 1;
		    }
		}
		else {
		    if (*cp == '\n') {
#ifndef	BRIDGE
			*cp++ = '\r';
#endif
			*cp++ = '\n';
			n = cp - bp;
			bp = cp;
			nc++;
			continue;
		    }

		    n = cp - bp + 1;
		    bp = cp + 1;
		}
	    }
	    else {
#ifdef	BRIDGE
		switch (n = read (fd, bp, ep - bp)) {
#else
		switch (n = read (fd, bp, ep - bp)) {
#endif
		    case NOTOK: 
		    case OK: 
			break;

		    default: 
			bp += n;
			continue;
		}
	    }
	    break;
	}
	if (n == NOTOK || (n = bp - (char *) pe -> pe_prim) == 0)
	    break;
	pe -> pe_len = n;

	if (fp && !effector) {
	    if ((de = pe_cpy (pe)) == NULLPE) {
		reason = FS_ACC_LCL;
		errno = ENOMEM;
		goto do_cancel;
	    }
	}
	else
	    de = pe;

	if (debug) {
	    if (fp) {
		WATCHP (DOCS_FTAM__1__Datatype1, de, 0);
	    }
	    else
		WATCHP (DOCS_FTAM__3__Datatype1, de, 0);
	}

	switch (de2fadu (de, pe != de ? 1 : 0)) {
	    case NOTOK: 
		if (fp)
		    (void) fclose (fp);
		return NOTOK;

	    case OK: 
	    default: 
		if (verbose || hash)
		    cc += (n - nc), nc = 0;
		if (hash) {
		    if (hash > 1)
			printf ("%d\r", cc);
		    else
			for (; marks < cc; marks += BUFSIZ)
			    (void) putchar ('#');
		    (void) fflush (stdout);
		}
		break;

	    case DONE:
		(void) de2fadu (NULLPE, 0);
		if (fp)
		    (void) fclose (fp);
		goto done_transfer;
	}
    }

    if (verbose)
	timer (cc, "sent");

    if (fp)
	(void) fclose (fp);

    if (n == NOTOK) {
	struct FTAMdiagnostic   diags[NFDIAG];
	register struct FTAMdiagnostic *dp;

#ifdef	BRIDGE
	advise (dst, "error reading");
#else
	advise (src, "error reading");
#endif
	reason = FS_ACC_LCLDEV;

do_cancel: ;
	dp = diags;

	dp -> ftd_type = DIAG_PERM;
	dp -> ftd_identifier = reason;
	dp -> ftd_observer = dp -> ftd_source = EREF_IFSU;
	dp -> ftd_delay = DIAG_NODELAY;
	(void) strcpy (dp -> ftd_data, sys_errname (errno));
	dp -> ftd_cc = strlen (dp -> ftd_data);
	dp++;

	(void) de2fadu (NULLPE, 0);

	if (FCancelRequest (ftamfd, FACTION_PERM, NULLPE, diags, dp - diags,
			    fti) == NOTOK) {
	    ftam_advise (fta, "F-CANCEL.REQUEST");
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

    if (n == OK)
	n = de2fadu (NULLPE, 1);

    if (FDataEndRequest (ftamfd, FACTION_SUCCESS, (struct FTAMdiagnostic *) 0,
	    0, fti) == NOTOK) {
	ftam_advise (fta, "F-DATA-END.REQUEST");
	return NOTOK;
    }

    if (FTransEndRequest (ftamfd, NULLPE, fti) == NOTOK) {
	ftam_advise (fta, "F-TRANSFER-END.REQUEST");
	return NOTOK;
    }

    switch (fti -> fti_type) {
	case FTI_TRANSEND: 
	    {
		register struct FTAMtransend   *ftre = &fti -> fti_transend;

		ftam_diag (ftre -> ftre_diags, ftre -> ftre_ndiag, 1,
			ftre -> ftre_action);
		FTREFREE (ftre);
	    }
	    break;

	case FTI_CANCEL: 
	    {
		register struct FTAMcancel *ftcn = &fti -> fti_cancel;

		advise (NULLCP, "data transfer canceled!");
		ftam_diag (ftcn -> ftcn_diags, ftcn -> ftcn_ndiag, 1,
			ftcn -> ftcn_action);
		FTCNFREE (ftcn);

		if (FCancelResponse (ftamfd, FACTION_SUCCESS, NULLPE,
			    (struct FTAMdiagnostic *) 0, 0, fti) == NOTOK) {
		    ftam_advise (fta, "F-CANCEL.RESPONSE");
		    return NOTOK;
		}
	    }
	    break;

	default: 
	    adios (NULLCP, "unexpected indication type=%d", fti -> fti_type);
    }

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
	register struct FTAMclose  *ftcl = &ftg -> ftg_close;

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
    return OK;

you_lose: ;
    FTGFREE (ftg);
    return NOTOK;
}

/*  */

int	de2fadu (pe, concat)
PE	pe;
int	concat;
{
    int	    result;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;
    register struct FTAMabort  *fta = &fti -> fti_abort;
    static int ninfo = 0;
    static int size = 0;
    static PE info[NPDATA];

    if (pe == NULLPE) {
	result = OK;
	if (concat
		&& ninfo > 0
		&& FDataRequest (ftamfd, info, ninfo, fti) == NOTOK) {
	    ftam_advise (fta, "F-DATA.REQUEST");
	    result = NOTOK;
	}

	while (ninfo > 0)
	    pe_free (info[--ninfo]);
	size = 0;

	return result;
    }

    if (concat) {
	int	flush,
		n;

	if (size + (n = ps_get_abs (pe) + MAGIC_OCTET2) >= fadusize
	        && ninfo > 0) {
	    if (debug)
		advise (NULLCP,
			"de2fadu flushing on %d FADUs, estimated size %d/%d",
			ninfo, size, fadusize);

	    if ((result = de2fadu (NULLPE, 1)) != OK)
		return result;
	    flush = 1;
	}
	else
	    flush = 0;
	
	info[ninfo++] = pe;
	size += n;

	if (ninfo < NPDATA && size < fadusize) {
	    if (!flush)
		return OK;
	}
	else {
	    if ((result = FDataRequest (ftamfd, info, ninfo, fti)) == NOTOK)
		ftam_advise (fta, "F-DATA.REQUEST");

	    while (ninfo > 0)
		pe_free (info[--ninfo]);
	    size = 0;

	    if (result == NOTOK)
		return result;
	}
    }
    else
	if (FDataRequest (ftamfd, &pe, 1, fti) == NOTOK) {
	    ftam_advise (fta, "F-DATA.REQUEST");
	    return NOTOK;
	}

    if (FWaitRequest (ftamfd, OK, fti) == NOTOK) {
	if (fta -> fta_peer
		|| fta -> fta_action != FACTION_TRANS
		|| fta -> fta_ndiag < 1
		|| fta -> fta_diags[0].ftd_type != DIAG_TRANS
		|| fta -> fta_diags[0].ftd_identifier != FS_PRO_TIMEOUT) {
	    ftam_advise (fta, "F-WAIT.REQUEST");
	    return NOTOK;
	}

	return OK;
    }

    if (fti -> fti_type == FTI_CANCEL) {
	register struct FTAMcancel *ftcn = &fti -> fti_cancel;

	advise (NULLCP, "data transfer cancelled!");
	ftam_diag (ftcn -> ftcn_diags, ftcn -> ftcn_ndiag, 1,
		   ftcn -> ftcn_action);
	FTCNFREE (ftcn);

	if (FCancelResponse (ftamfd, FACTION_SUCCESS, NULLPE, 
		    (struct FTAMdiagnostic *) 0, 0, fti) == NOTOK) {
	    ftam_advise (fta, "F-CANCEL.RESPONSE");
	    return NOTOK;
	}
    }

    return DONE;
}

