/* ftamd-trans.c - FTAM responder -- transfer */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftamd-trans.c,v 7.8 91/02/22 09:23:58 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftamd-trans.c,v 7.8 91/02/22 09:23:58 mrose Interim $
 *
 *
 * $Log:	ftamd-trans.c,v $
 * Revision 7.8  91/02/22  09:23:58  mrose
 * Interim 6.8
 * 
 * Revision 7.7  91/01/13  12:26:59  mrose
 * NBS
 * 
 * Revision 7.6  90/11/21  11:30:46  mrose
 * sun
 * 
 * Revision 7.5  90/11/11  10:01:15  mrose
 * touch-up
 * 
 * Revision 7.4  90/11/05  13:29:50  mrose
 * nist
 * 
 * Revision 7.3  90/08/14  14:28:28  mrose
 * T1
 * 
 * Revision 7.2  90/07/01  21:03:28  mrose
 * pepsy
 * 
 * Revision 7.1  90/01/16  20:43:01  mrose
 * last check-out
 * 
 * Revision 7.0  89/11/23  21:54:33  mrose
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
#include "FTAM-types.h"
#include "ftamsystem.h"
#if	defined(SYS5) && !defined(HPUX)
#include <sys/times.h>
#define	TMS
#endif

/*    DATA */

static int  nbytes;

long	lseek ();


/*    TRANSFER */

int	ftam_bulkbeginindication (ftg)
struct FTAMgroup *ftg;
{
    int	    state;
    struct FTAMgroup    ftms;
    struct FTAMgroup   *ftm = &ftms;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;

    ftam_selection (ftg, ftm);

    if (ftm -> ftg_flags & FTG_SELECT)
	state = ftm -> ftg_select.ftse_state;
    else
	state = ftm -> ftg_create.ftce_state;
    if ((state != FSTATE_SUCCESS
		|| ((ftm -> ftg_flags & FTG_OPEN)
			&& ftm -> ftg_open.ftop_state != FSTATE_SUCCESS))
	    && myfd != NOTOK) {
#ifndef	BRIDGE
	unlock ();
#endif
	(void) close (myfd);
	myfd = NOTOK;
    }

    if (FBulkBeginResponse (ftamfd, ftm, fti) == NOTOK)
	ftam_adios (&fti -> fti_abort, "F-BULK-BEGIN.RESPONSE");

    FTGFREE (ftg);
}

/*  */

/* we really pay the price here for not keeping more constraint set
   information in the vfs structure...

   regular files have the unstructured constraint set
	access context US

   directory files have the sequential flat constraint set
	access context UA (is forced)

   Hence, when FADUs are transmitted, only data elements of type

	File-Contents-Data-Element

   are sent.
*/


int	ftam_readwriteindication (ftrw)
struct FTAMreadwrite *ftrw;
{
    int	    result;
    register struct FADUidentity *fa = &ftrw -> ftrw_identity;
    struct FTAMdiagnostic   diags[NFDIAG];
    struct FTAMdiagnostic *dp = diags;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;

    mylocation = *fa;		/* struct copy */
    mylevel = ftrw -> ftrw_level;
    
#ifdef	BRIDGE
    myoperation = ftrw -> ftrw_operation; 
    if (ftp_type (myvf - vfs) == NOTOK
	    && myoperation == FA_OPS_READ
	    && (mycontext = ftrw -> ftrw_context) != myvf -> vf_context) {
#else
    if ((myoperation = ftrw -> ftrw_operation) == FA_OPS_READ
	    && (mycontext = ftrw -> ftrw_context) != myvf -> vf_context) {
#endif
	dp -> ftd_type = DIAG_PERM;
	dp -> ftd_identifier = FS_ACC_CTXSUPRT;
	dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	dp -> ftd_delay = DIAG_NODELAY;
	dp -> ftd_cc = 0;
	dp++;

	goto do_cancel;
    }

    switch (myvf - vfs) {
	case VFS_UBF: 
	case VFS_UTF: 
	default: 
	    if (fa -> fa_type != FA_FIRSTLAST
		    || fa -> fa_firstlast != FA_FIRST) {
		dp -> ftd_type = DIAG_PERM;
		dp -> ftd_identifier = FS_ACC_FADULOC;
		dp -> ftd_observer = EREF_RFSU, dp -> ftd_source = EREF_IFSU;
		dp -> ftd_delay = DIAG_NODELAY;
		dp -> ftd_cc = 0;
		dp++;

		goto do_cancel;
	    }
	    break;
    }

    switch (myoperation) {
	case FA_OPS_READ:
	default:
	    advise (LLOG_NOTICE, NULLCP, "read %s", myfile);
	    break;

	case FA_OPS_INSERT:	/* not valid on an FDF */
	    dp -> ftd_type = DIAG_PERM;
	    dp -> ftd_identifier = FS_ACC_INSERT;
	    dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	    dp -> ftd_delay = DIAG_NODELAY;
	    dp -> ftd_cc = 0;
	    dp++;
	    goto do_cancel;

	case FA_OPS_REPLACE:
#ifdef	BRIDGE
	    if ((myfd = ftp_write (myfile)) == NOTOK) {
#else
	    if (statok && myst.st_size == 0)
		goto replace;
#ifdef	SUNOS4
	    if (ftruncate (myfd, (off_t) 0) == NOTOK) {
#else
	    if (ftruncate (myfd, 0) == NOTOK) {
#endif
#endif
bad_operation: ;
		dp -> ftd_type = DIAG_PERM;
		dp -> ftd_identifier = myoperation == FA_OPS_REPLACE
					    ? FS_ACC_REPLACE : FS_ACC_INSERT;
		dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
		dp -> ftd_delay = DIAG_NODELAY;
#ifdef	BRIDGE
		(void) strcpy (dp -> ftd_data, ftp_error);
#else
		(void) strcpy (dp -> ftd_data, sys_errname (errno));
#endif
		dp -> ftd_cc = strlen (dp -> ftd_data);
		dp++;
		goto do_cancel;
	    }
#ifndef	BRIDGE
replace: ;
#endif
	    advise (LLOG_NOTICE, NULLCP, "replace %s", myfile);
	    break;

	case FA_OPS_EXTEND:
#ifdef	BRIDGE
	    if ((myfd = ftp_append (myfile)) == NOTOK)
#else
	    if (lseek (myfd, 0L, L_XTND) == (long) NOTOK)
#endif
		goto bad_operation;
	    advise (LLOG_NOTICE, NULLCP, "extend %s", myfile);
	    break;
    }

    FTRWFREE (ftrw);

    timer (nbytes = 0, NULLCP);
    if (myoperation != FA_OPS_READ)
	return;

    switch (myvf - vfs) {
	case VFS_UTF:
	case VFS_UBF:
	default:
	    result = uxfget (&dp);
	    break;

	case VFS_FDF:
	    result = fdfget (&dp);
	    break;
    }

    if (result != NOTOK)
	return;

do_cancel: ;
    FTRWFREE (ftrw);

    if (FCancelRequest (ftamfd, FACTION_PERM, NULLPE, diags, dp - diags, fti)
	    == NOTOK)
	ftam_adios (&fti -> fti_abort, "F-CANCEL-REQUEST");

    if (fti -> fti_type == FTI_CANCEL) {
	register struct FTAMcancel *ftcn = &fti -> fti_cancel;

	advise (LLOG_NOTICE, NULLCP, "F-CANCEL.RESPONSE: %d",
		    ftcn -> ftcn_action);
	ftam_diag (ftcn -> ftcn_diags, ftcn -> ftcn_ndiag);
	FTCNFREE (ftcn);
    }
}

/*  */

static	uxfget (diags)
register struct FTAMdiagnostic **diags;
{
    register int    n;
    int	    bsize,
	    effector,
	    gd,
	    magic,
	    nc,
	    size,
    	    cancelled = OK;
    PE	    pe,
	    de;
    register struct FTAMdiagnostic *dp = *diags;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;
    register struct FTAMabort *fta = &fti -> fti_abort;
    register struct type_DOCS_FTAM__1__Parameters *p1;
    register struct type_DOCS_FTAM__3__Parameters *p3;
    FILE   *fp;
#ifdef	BRIDGE
    char line[BUFSIZ];
#endif

#ifdef	BRIDGE
    /* try opening file for ftp read */
    if ((myfd = ftp_read (myfile)) == NOTOK){
	dp -> ftd_type = DIAG_PERM;
	dp -> ftd_identifier = FS_ACC_LCL;
	dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	dp -> ftd_delay = DIAG_NODELAY;
	(void) strcpy (dp -> ftd_data, "failed file access on remote host");
	dp -> ftd_cc = strlen (dp -> ftd_data);
	dp++;

	*diags = dp;
	return NOTOK;
    }
#endif

    effector = 1;
    switch (myvf - vfs) {
	case VFS_UTF: 
	    p1 = (struct type_DOCS_FTAM__1__Parameters *) myparam;
	    if ((gd = dup (myfd)) == NOTOK
		    || (fp = fdopen (gd, "r")) == NULL) {
		if (gd != NOTOK)
		    (void) close (gd);
		if (myfd != NOTOK){
#ifdef	BRIDGE
			(void) close (myfd);
			myfd = NOTOK;
			(void) ftp_reply ();
#endif
		}
		goto no_mem;
	    }

	    {
		PElementID    id;

		if (p1
		        && (p1 -> optionals
			        & opt_DOCS_FTAM__1__Parameters_universal__class__number))
		    id = (PElementID) p1 -> universal__class__number;
		else
		    id = PE_DEFN_GFXS;

		pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, id);

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
	    if ((magic = fadusize - MAGIC_OCTET1) < 0)
		magic = 0;
	    break;

	case VFS_UBF:
	    p3 = (struct type_DOCS_FTAM__3__Parameters *) myparam;
	    /* and fall */
	default:
	    fp = NULL;

	    pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_OCTS);
	    if ((magic = fadusize - MAGIC_SINGLE) < 0)
		magic = 0;
	    break;
    }
    if (pe == NULLPE)
	goto no_mem;
    pe -> pe_context = myvf -> vf_id;

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
    bsize = myst.st_blksize > 0 ? myst.st_blksize : BUFSIZ;
#endif
    size = (1024 <= magic && magic < bsize) ? magic : bsize;
#endif
    switch (myvf - vfs) {
	case VFS_UTF:
	    if (p1
		    && (p1 -> optionals
		            & opt_DOCS_FTAM__1__Parameters_maximum__string__length)
		    && p1 -> maximum__string__length > 0)		    
		size = p1 -> maximum__string__length;
	    break;

	case VFS_UBF:
	    if (p3
		    && (p3 -> optionals
		            & opt_DOCS_FTAM__3__Parameters_maximum__string__length)
		    && p3 -> maximum__string__length > 0)
		size = p3 -> maximum__string__length;
	    /* and fall */
	default:
	    break;
    }

    if (debug)
	advise (LLOG_DEBUG, NULLCP, "effector=%d id=0x%x size=%d",
		effector, pe -> pe_id, size);

    if ((pe -> pe_prim = PEDalloc (pe -> pe_len = size)) == NULLPED) {
no_mem: ;
	dp -> ftd_type = DIAG_PERM;
	dp -> ftd_identifier = FS_ACC_LCL;
	dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	dp -> ftd_delay = DIAG_NODELAY;
	(void) strcpy (dp -> ftd_data, "out of memory");
	dp -> ftd_cc = strlen (dp -> ftd_data);
	dp++;

error_return: ;
	(void) de2fadu (NULLPE, 0);
	if (pe)
	    pe_free (pe);
	if (fp)
	    (void) fclose (fp);
	*diags = dp;
	return NOTOK;
    }

    for (;;) {
	register char  *bp,
	               *ep;

	for (ep = (bp = (char *) pe -> pe_prim) + size - (fp ? 2 : 0), nc = 0;
		bp < ep; ) {
	    if (fp) {
		register char *cp;

#ifdef	BRIDGE
		if (strlen (line) || fgets (line, BUFSIZ, fp)) {
		    if ((strlen (line) + 1) < (ep - bp + 1)) {
			(void) strcpy (bp, line);
			line[0] = NULL;
		    }
		    else
			break;
		}
		else {
		    n = (ferror (fp) && !feof (fp)) ? NOTOK : OK;
		    break;
		}
#else
		if (fgets (bp, ep - bp + 1, fp) == NULL) {
		    n = (ferror (fp) && !feof (fp)) ? NOTOK : OK;
		    break;
		}
#endif
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
		switch (n = read (myfd, bp, ep - bp)) {
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
	    if ((de = pe_cpy (pe)) == NULLPE)
		goto no_mem;
	}
	else
	    de = pe;

	if (debug)
	    if (fp) {
		WATCHP (DOCS_FTAM__1__Datatype1, de, 0);
	    }
	    else
		WATCHP (DOCS_FTAM__3__Datatype1, de, 0);


	switch (cancelled = de2fadu (de, pe != de ? 1 : 0)) {
	    case NOTOK:
	        dp -> ftd_type = DIAG_PERM;
		dp -> ftd_identifier = FS_ACC_LCL;
		dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
		dp -> ftd_delay = DIAG_NODELAY;
		dp -> ftd_cc = 0;
		dp++;
	        goto error_return;

	    case OK:
	    default:
		nbytes += (n - nc), nc = 0;
		continue;

	    case DONE:
		break;
	}
	break;
    }

    pe_free (pe);

    if (fp)
	(void) fclose (fp);

    if (n == DONE || cancelled == DONE)
	return DONE;

    if (n == NOTOK) {
	dp -> ftd_type = DIAG_PERM;
	dp -> ftd_identifier = FS_ACC_LCLDEV;
	dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	dp -> ftd_delay = DIAG_NODELAY;
	(void) strcpy (dp -> ftd_data, sys_errname (errno));
	dp -> ftd_cc = strlen (dp -> ftd_data);
	dp++;

	*diags = dp;

	(void) de2fadu (NULLPE, 0);
	return NOTOK;
    }

    n = de2fadu (NULLPE, 1);

    if (n != DONE) {
	timer (nbytes, "sent");

	if (FDataEndRequest (ftamfd, FACTION_SUCCESS,
			     (struct FTAMdiagnostic *) 0, 0, fti) == NOTOK)
	    ftam_adios (fta, "F-DATA-END.REQUEST");
    }

    return n;
}

/*  */

static	fdfget (diags)
register struct FTAMdiagnostic **diags;
{
    int	    names,
	    len,
	    n;
    char   *pp,
	    path[MAXPATHLEN];
    PE	    pe;
#ifdef	BRIDGE
    int	    dd;
    FILE   *df;
    char   *ptr,
	    line[BUFSIZ];
#else
    register struct dirent *d;
    register DIR *dd;
#endif
    struct stat st;
    register struct FTAMdiagnostic *dp = *diags;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;
    register struct FTAMabort *fta = &fti -> fti_abort;
    register struct type_DOCS_NBS__9__Parameters *p9;
    struct type_DOCS_NBS__9__Datatype1 *d9;

    if (strcmp (myfile, ".") == 0) {
	pp = path;
	*pp = NULL;
	len = 0;
    }
    else 
#ifdef apollo
        if (strcmp (myfile, "/") == 0 || strcmp (myfile, "//") == 0) {
            (void) sprintf (pp = path, "%s", myfile);
            pp += (len = strlen (pp));
        }
        else
#endif                
    {
	(void) sprintf (pp = path, "%s/", myfile);
	pp += (len = strlen (pp));
    }

    pe = NULLPE;

#ifdef	BRIDGE
    if ((dd = ftp_ls (myfile)) == NOTOK || (df = fdopen (dd,"r")) == NULL) {
#else
    if ((dd = opendir (myfile)) == NULL) {
#endif
no_mem: ;
	dp -> ftd_type = DIAG_PERM;
	dp -> ftd_identifier = FS_ACC_LCL;
	dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	dp -> ftd_delay = DIAG_NODELAY;
	(void) strcpy (dp -> ftd_data, "out of memory");
	dp -> ftd_cc = strlen (dp -> ftd_data);
	dp++;

error_return: ;
	(void) de2fadu (NULLPE, 0);
	if (pe)
	    pe_free (pe);
#ifdef	BRIDGE
	if (dd) {
	    (void) close (dd);
	    (void) ftp_reply ();
        }
#else
	if (dd)
	    (void) closedir (dd);
#endif
	*diags = dp;
	return NOTOK;
    }


    p9 = (struct type_DOCS_NBS__9__Parameters *) myparam;
    if (fdf_p2names (ftamfd, p9, &names, fti) == NOTOK) {
	register struct FTAMdiagnostic *d2 = fti -> fti_abort.fta_diags;

	dp = d2;	/* struct copy */
	dp++;
	goto error_return;
    }

    n = OK;
#ifdef	BRIDGE
    while (fgets(line, BUFSIZ, df)) {
#else
    for (errno = 0; d = readdir (dd); errno = 0) {
#endif
	struct FTAMattributes fas;
	register struct FTAMattributes *fa = &fas;
#ifndef	BRIDGE
	register struct vfsmap *vf;
#endif

#ifdef	BRIDGE
	if (ptr = rindex(line,'\r'))
	    *ptr = '\0';
#endif

	if (debug)
#ifdef	BRIDGE
	    advise (LLOG_DEBUG, NULLCP, "len=%d name=\"%s\"",
			strlen(line), line);
#else
	    advise (LLOG_DEBUG, NULLCP, "ino=%ld len=%d name=\"%s\"",
		    d -> d_ino, strlen (d -> d_name), d -> d_name);
#endif

#ifndef	BRIDGE
	if (!d -> d_ino)
	    continue;
#endif

#ifdef	BRIDGE
	if (len + strlen (line) >= MAXPATHLEN)
#else
	if (len + strlen (d -> d_name) >= MAXPATHLEN)
#endif
	    continue;

#ifdef	BRIDGE
	(void) strcpy (pp, line);
#else
	(void) strcpy (pp, d -> d_name);
#endif

#ifndef	BRIDGE
	if (stat (path, &st) == NOTOK
		|| (vf = st2vfs (NOTOK, path, &st, NULLOID, ftamfd)) == NULL)
	    continue;
#endif

	bzero ((char *) fa, sizeof *fa);
	*diags = dp;
#ifdef	BRIDGE
	(void) readattrs (names, fa, vfs[VFS_UTF].vf_oid, NULLPE, path, &st,
			  diags);
#else
	(void) readattrs (names, fa, vf -> vf_oid, NULLPE, path, &st, diags);
#endif
	dp = *diags;

	if (fdf_attrs2d (ftamfd, fa, &d9, fti) == NOTOK) {
	    register struct FTAMdiagnostic *d2 = fti -> fti_abort.fta_diags;

	    dp = d2;	/* struct copy */
	    dp++;
	    goto error_return;
	}

	if (encode_DOCS_NBS__9__Datatype1 (&pe, 1, 0, NULLCP, d9) == NOTOK) {
	    dp -> ftd_type = DIAG_PERM;
	    dp -> ftd_identifier = FS_ACC_LCL;
	    dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	    dp -> ftd_delay = DIAG_NODELAY;
	    (void) sprintf (dp -> ftd_data, "error encoding Datatype1: %s",
			    PY_pepy);
	    dp -> ftd_cc = strlen (dp -> ftd_data);
	    dp++;

	    free_DOCS_NBS__9__Datatype1 (d9);
	    goto error_return;
	}
	pe -> pe_context = myvf -> vf_id;

	if (debug)
	    WATCHP (DOCS_NBS__9__Datatype1, pe, 0);

	n = de2fadu (pe, 1), pe = NULLPE;
	switch (n) {
	    case NOTOK:
		goto no_mem;

	    case OK:
	    default:
		continue;

	    case DONE:
		break;
	}
	break;
    }

#ifdef	BRIDGE
    (void) fclose (df);
    (void) close (dd);
    (void) ftp_reply ();
#else
    if (errno != 0) {
	dp -> ftd_type = DIAG_PERM;
	dp -> ftd_identifier = FS_ACC_LCL;
	dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	dp -> ftd_delay = DIAG_NODELAY;
	(void) sprintf (dp -> ftd_data, "%s: %s", myfile, sys_errname (errno));
	dp -> ftd_cc = strlen (dp -> ftd_data);
	dp++;

	goto error_return;
	
    }
    (void) closedir (dd);
#endif

    if (n == OK)
	n = de2fadu (NULLPE, 1);

    switch (n) {
	case DONE:
	    return DONE;

	case OK:
	default:
	    if (FDataEndRequest (ftamfd, FACTION_SUCCESS,
			(struct FTAMdiagnostic *) 0, 0, fti) == NOTOK)
		ftam_adios (fta, "F-DATA-END.REQUEST");
	    return OK;
    }
}

/*  */

static int  de2fadu (pe, concat)
PE	pe;
int	concat;
{
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;
    register struct FTAMabort  *fta = &fti -> fti_abort;
    static int ninfo = 0;
    static int size = 0;
    static PE info[NPDATA];

    if (pe == NULLPE) {
	if (concat
		&& ninfo > 0
		&& FDataRequest (ftamfd, info, ninfo, fti) == NOTOK)
	    ftam_adios (fta, "F-DATA.REQUEST");

	while (ninfo > 0)
	    pe_free (info[--ninfo]);
	size = 0;

	return OK;
    }

    if (concat) {
	int	flush,
		n;

	if (size + (n = ps_get_abs (pe) + MAGIC_OCTET2) >= fadusize
	        && ninfo > 0) {
	    if (debug)
		advise (LLOG_DEBUG, NULLCP,
			"de2fadu flushing on %d FADUs, estimated size %d/%d",
			ninfo, size, fadusize);

	    (void) de2fadu (NULLPE, 1);
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
	    if (FDataRequest (ftamfd, info, ninfo, fti) == NOTOK)
		ftam_adios (fta, "F-DATA.REQUEST");

	    while (ninfo > 0)
		pe_free (info[--ninfo]);
	    size = 0;
	}
    }
    else
	if (FDataRequest (ftamfd, &pe, 1, fti) == NOTOK)
	    ftam_adios (fta, "F-DATA.REQUEST");

    if (FWaitRequest (ftamfd, OK, fti) == NOTOK) {
	if (fta -> fta_peer
		|| fta -> fta_action != FACTION_TRANS
		|| fta -> fta_ndiag < 1
		|| fta -> fta_diags[0].ftd_type != DIAG_TRANS
		|| fta -> fta_diags[0].ftd_identifier != FS_PRO_TIMEOUT)
	    ftam_adios (fta, "F-WAIT.REQUEST");

	return OK;
    }

    if (fti -> fti_type == FTI_CANCEL) {
	register struct FTAMcancel *ftcn = &fti -> fti_cancel;

	advise (LLOG_NOTICE, NULLCP, "F-CANCEL.INDICATION: %d",
		ftcn -> ftcn_action);
	ftam_diag (ftcn -> ftcn_diags, ftcn -> ftcn_ndiag);
	FTCNFREE (ftcn);

	if (FCancelResponse (ftamfd, FACTION_SUCCESS, NULLPE, 
		    (struct FTAMdiagnostic *) 0, 0, fti) == NOTOK)
	    ftam_adios (fta, "F-CANCEL.RESPONSE");
    }

    return DONE;
}

/*  */

int	ftam_dataindication (px)
register struct PSAPdata *px;
{
    register int    i;
    int	    effector,
	    n;
    register PE     pe,
		   *pep;
    struct FTAMdiagnostic   diags[NFDIAG];
    register struct FTAMdiagnostic *dp = diags;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;

    effector = 1;
    switch (myvf - vfs) {
	case VFS_UBF:
	default:
	    break;

	case VFS_UTF:
	    {
		PElementID    id;
		register struct type_DOCS_FTAM__1__Parameters *p1 =
			    (struct type_DOCS_FTAM__1__Parameters *) myparam;

		if (p1
		        && (p1 -> optionals
			        & opt_DOCS_FTAM__1__Parameters_universal__class__number))
		    id = (PElementID) p1 -> universal__class__number;
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
    }

    for (pep = px -> px_info, i = px -> px_ninfo - 1; i >= 0; pep++, i--) {
	if ((pe = *pep) == NULLPE)
	    continue;

	switch (myvf - vfs) {
	    case VFS_UBF:
	    default:
		if (debug)
		    WATCHP (DOCS_FTAM__3__Datatype1, pe, 1);
		n = de2fd (myfd, pe, 0, 0);
		break;

	    case VFS_UTF:
		if (debug)
		    WATCHP (DOCS_FTAM__1__Datatype1, pe, 1);
		n = de2fd (myfd, pe, 1, effector);
		break;
	}

	if (n != NOTOK) {
	    nbytes += n;
	    continue;
	}

	dp -> ftd_type = DIAG_PERM;
	switch (errno) {
	    case ENOSPC: 
		dp -> ftd_identifier = FS_ACC_LCLSPACE;
		dp -> ftd_cc = 0;
		break;

	    case EIO: 
	    case ENXIO: 
		dp -> ftd_identifier = FS_ACC_LCLDEV;
		dp -> ftd_cc = 0;
		break;

	    default: 
		dp -> ftd_identifier = FS_ACC_WRITE;
		(void) strcpy (dp -> ftd_data, sys_errname (errno));
		dp -> ftd_cc = strlen (dp -> ftd_data);
		break;
	}
	dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	dp -> ftd_delay = DIAG_NODELAY;
	dp++;

	if (FCancelRequest (ftamfd, FACTION_PERM, NULLPE, diags, dp - diags,
			    fti) == NOTOK)
	    ftam_adios (&fti -> fti_abort, "F-CANCEL-REQUEST");

	if (fti -> fti_type == FTI_CANCEL) {
	    register struct FTAMcancel *ftcn = &fti -> fti_cancel;

	    advise (LLOG_NOTICE, NULLCP, "F-CANCEL.RESPONSE: %d",
		    ftcn -> ftcn_action);
	    ftam_diag (ftcn -> ftcn_diags, ftcn -> ftcn_ndiag);
	    FTCNFREE (ftcn);
	}
	break;
    }

    PXFREE (px);
}

/*  */

/* ARGSUSED */

int	ftam_dataendindication (ftda)
struct FTAMdataend *ftda;
{
    timer (nbytes, "received");

#ifndef	SYS5
    if (ftda -> ftda_action == FACTION_SUCCESS)
	(void) fsync (myfd);
#endif
}

/*  */

int	ftam_cancelindication (ftcn)
register struct FTAMcancel *ftcn;
{
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;

    advise (LLOG_NOTICE, NULLCP, "F-CANCEL.INDICATION: %d",
	    ftcn -> ftcn_action);
    ftam_diag (ftcn -> ftcn_diags, ftcn -> ftcn_ndiag);
    FTCNFREE (ftcn);

    if (FCancelResponse (ftamfd, FACTION_SUCCESS, NULLPE,
		(struct FTAMdiagnostic *) 0, 0, fti) == NOTOK)
	ftam_adios (&fti -> fti_abort, "F-CANCEL.RESPONSE");
}

/*  */

/* ARGSUSED */

int	ftam_transendindication (ftre)
struct FTAMtransend *ftre;
{
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;

    if (FTransEndResponse (ftamfd, FACTION_SUCCESS, NULLPE, 
		(struct FTAMdiagnostic *) 0, 0, fti) == NOTOK)
	ftam_adios (&fti -> fti_abort, "F-TRANSFER-END.RESPONSE");
}

/*  */

int	ftam_bulkendindication (ftg)
struct FTAMgroup *ftg;
{
    struct FTAMgroup    ftms;
    struct FTAMgroup   *ftm = &ftms;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;

    ftam_selection (ftg, ftm);

    if (myfd != NOTOK) {
#ifdef	BRIDGE
	(void) close (myfd);
	(void) ftp_reply ();
#else
	unlock ();
	(void) close (myfd);
#endif
	myfd = NOTOK;
    }

    if (FBulkEndResponse (ftamfd, ftm, fti) == NOTOK)
	ftam_adios (&fti -> fti_abort, "F-BULK-END.RESPONSE");

    FTGFREE (ftg);
}

/*  */

#ifndef	NBBY
#define	NBBY	8
#endif


#ifndef	TMS
timer (cc, action)
int     cc;
char   *action;
{
    long    ms;
    float   bs;
    struct timeval  stop,
                    td;
    static struct timeval   start;

    if (cc == 0) {
	(void) gettimeofday (&start, (struct timezone *) 0);
	return;
    }
    else
	(void) gettimeofday (&stop, (struct timezone  *) 0);

    tvsub (&td, &stop, &start);
    ms = (td.tv_sec * 1000) + (td.tv_usec / 1000);
    bs = (((float) cc * NBBY * 1000) / (float) (ms ? ms : 1)) / NBBY;

    advise (LLOG_NOTICE, NULLCP,
	    "%d bytes %s in %d.%02d seconds (%.2f Kbytes/s)",
	    cc, action, td.tv_sec, td.tv_usec / 10000, bs / 1024);
}


static  tvsub (tdiff, t1, t0)
register struct timeval *tdiff,
			*t1,
			*t0;
{

    tdiff -> tv_sec = t1 -> tv_sec - t0 -> tv_sec;
    tdiff -> tv_usec = t1 -> tv_usec - t0 -> tv_usec;
    if (tdiff -> tv_usec < 0)
	tdiff -> tv_sec--, tdiff -> tv_usec += 1000000;
}

#else
#ifndef	HZ
#define	HZ	60
#endif


long	times ();


static	timer (cc, action)
int	cc;
char   *action;
{
    long    ms;
    float   bs;
    long    stop,
	    td,
	    secs,
	    msecs;
    struct tms tm;
    static long start;

    if (cc == 0) {
	start = times (&tm);
	return;
    }
    else
	stop = times (&tm);

    td = stop - start;
    secs = td / HZ, msecs = (td % HZ) * 1000 / HZ;
    ms = (secs * 1000) +  msecs;
    bs = (((float) cc * NBBY * 1000) / (float) (ms ? ms : 1)) / NBBY;
    
    advise (LLOG_NOTICE, NULLCP,
	    "%d bytes %s in %d.%02d seconds (%.2f Kbytes/s)",
	    cc, action, secs, msecs / 10, bs / 1024);
}
#endif
