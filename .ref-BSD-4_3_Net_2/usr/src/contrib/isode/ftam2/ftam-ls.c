/* ftam-ls.c - interactive initiator FTAM -- "ls" */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftam-ls.c,v 7.5 91/02/22 09:23:45 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftam-ls.c,v 7.5 91/02/22 09:23:45 mrose Interim $
 *
 *
 * $Log:	ftam-ls.c,v $
 * Revision 7.5  91/02/22  09:23:45  mrose
 * Interim 6.8
 * 
 * Revision 7.4  91/01/13  12:26:56  mrose
 * NBS
 * 
 * Revision 7.3  90/11/21  11:30:26  mrose
 * sun
 * 
 * Revision 7.2  90/11/05  13:29:46  mrose
 * nist
 * 
 * Revision 7.1  90/07/01  21:03:12  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:54:21  mrose
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


#include <signal.h>
#include <stdio.h>
#include "FTAM-types.h"
#include "ftamuser.h"

/*     DATA */

static int dashl;
static int didrecurse;
static int silent;


static long  now;
static long  longtimeago;


int	toomany;

int	nfilent = 0;
struct filent *filents = NULL;

int	filcmp ();


#ifdef	BRIDGE
FILE	*fdopen();
#endif
static  FILE *lsfp = stdout;


long	time ();
char   *ctime ();
FILE   *popen ();

/*  */

#ifndef	BRIDGE
int	f_fls (vec)
char  **vec;
{
    int	    doingpipe,
	    result;
    SFP	    pstat;    
    char   *cp,
	   *pp,
	    buffer[BUFSIZ];
    FILE   *fp;

    pp = vec[0];
    if (*++vec == NULL)  {
	if (getline ("output to file/program: ", buffer) == NOTOK
		|| str2vec (buffer, vec) < 1)
	    return OK;
    }
	
    cp = vec[0];
    if (*cp == '|') {
	if ((fp = popen (cp + 1, "w")) == NULL) {
	    advise (cp + 1, "unable to start");
	    return OK;
	}

	doingpipe = 1;
	pstat = signal (SIGPIPE, SIG_IGN);
    }
    else {
	if ((cp = xglob1val (cp, 0)) == NULL)
	    return OK;

	if ((fp = fopen (cp, "w")) == NULL) {
	    advise (cp, "unable to write");
	    free (cp);
	    return OK;
	}

	doingpipe = 0;
    }
    vec[0] = pp + 1;

    lsfp = fp;

    result = f_ls (vec);

    if (doingpipe) {
	(void) pclose (fp);
	(void) signal (SIGPIPE, pstat);
    }
    else {
	free (cp);
	(void) fclose (fp);
    }

    lsfp = stdout;

    return result;
}
#endif

/*  */

int	f_ls (vec)
char  **vec;
{
    int     invis,
            multi,
	    result;
#ifdef	BRIDGE
    int	    fd;
#else
    char    buffer[BUFSIZ];
#endif

    if (dashl = strcmp (*vec, "dir") == 0) {
	if (!(attrs & FATTR_STORAGE)) {
	    advise (NULLCP, "no support for storage attributes");
	    return OK;
	}

	(void) time (&now);
	longtimeago = now - 6L * 30L * 24L * 60L * 60L;
    }

    if (*++vec == NULL) {
#ifdef	BRIDGE
	return NOTOK;
#else
	switch (realstore) {
	    case RFS_UNIX: 
		*vec++ = ".";
		*vec-- = NULL;
		invis = !dashl;
		break;

	    default: 
		if (getline ("file: ", buffer) == NOTOK
			|| str2vec (buffer, vec) < 1)
		    return OK;
		invis = 0;
		break;
	}
#endif
    }
    else
	invis = 0;

#ifdef	BRIDGE
    if ((fd = dataconn ("LIST")) == NOTOK
	    || (lsfp = fdopen (fd, "w")) == NULL) {
	(void) sprintf (ftam_error, "out of memory");
	if (fd != NOTOK) {
	    (void) close (fd);
	    return NOTOK;
	}
        return DONE;
    }
#endif

    result = OK;
    if (vec = xglob (vec, 1)) {
	register char **gp;

	didrecurse = 0;
	multi = vec[1] ? 1 : 0;

	for (gp = vec; *gp && !interrupted; gp++) {
	    result = ls (*gp, *gp, 1, gp == vec, gp[1] == NULL, invis, multi);

	    if (ftamfd == NOTOK || result == NOTOK)
		break;
	}

	blkfree (vec);
    }
#ifdef	BRIDGE
    (void) fclose (lsfp);
    (void) close (fd);
#endif

    return result;
}

/*  */

static int  ls (file, entry, top, first, last, invis, multi)
char   *file,
       *entry;
int	top,
	first,
	last,
	invis,
	multi;
{
    int	    recurse;
    long    mtime;
    char   *s;
    UTC	    ut;
    struct FTAMgroup    ftgs;
    register struct FTAMgroup  *ftg = &ftgs;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;
    struct vfsmap *vf = &vfs[VFS_FDF];

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
	if (dashl)
#ifdef	DEBUG
	    ftra -> ftra_attrnames |= FA_STORAGE
			| (attrs & FATTR_SECURITY ? FA_SECURITY : 0);
#else
	    ftra -> ftra_attrnames |= FA_ID_CREATE | FA_DATE_MODIFY
			| FA_ACCOUNT | FA_FILESIZE;
#endif
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
	register struct FTAMattributes *fa = &ftse -> ftse_attrs;

	if (multi && ftse -> ftse_state != FSTATE_SUCCESS)
	    printf ("%s\n", fa -> fa_files[0]);
	ftam_diag (ftse -> ftse_diags, ftse -> ftse_ndiag, 1,
		ftse -> ftse_action);
	if (ftse -> ftse_state != FSTATE_SUCCESS)
	    goto you_lose;

	file = fa -> fa_files[0];
    }

    recurse = 0;

    if (ftg -> ftg_flags & FTG_RDATTR) {
	register struct FTAMreadattr   *ftra = &ftg -> ftg_readattr;
	register struct FTAMattributes *fa = &ftra -> ftra_attrs;

	ftam_diag (ftra -> ftra_diags, ftra -> ftra_ndiag, 1,
		ftra -> ftra_action);
	if (ftra -> ftra_action != FACTION_SUCCESS)
	    fa -> fa_present = 0;
	fa -> fa_present &= ~fa -> fa_novalue;

	if (top
		&& (fa -> fa_present & FA_FILENAME)
		&& vf -> vf_oid
		&& (fa -> fa_present & FA_CONTENTS)
		&& oid_cmp (vf -> vf_oid, fa -> fa_contents) == 0) {
	    recurse++;
	    if (!didrecurse && !first)
#ifdef	BRIDGE
		fprintf (lsfp, "\r\n");
#else
		fprintf (lsfp, "\n");
#endif
	}
	if (!invis)
	    invis = recurse && !multi;/* recurse depends on top */

	if (dashl && !invis && !recurse) {
	    s = (fa -> fa_present & FA_ID_CREATE) ? fa -> fa_id_create : NULL;
	    ut = (fa -> fa_present & FA_DATE_MODIFY) ? &fa -> fa_date_modify
						     : NULLUTC;

	    if (fa -> fa_present & FA_CONTENTS) {
		register struct vfsmap *uf;

		for (uf = vfs; uf -> vf_entry; uf++)
		    if (oid_cmp (uf -> vf_oid, fa -> fa_contents) == 0)
			break;
		fprintf (lsfp, "%c ", uf -> vf_entry ? uf -> vf_stat : ' ');
	    }
	    else
		fprintf (lsfp, "  ");
	    fprintf (lsfp, "%-8.8s %-8.8s %8d ", s ? s : "",
		    (fa -> fa_present & FA_ACCOUNT) ? fa -> fa_account : "",
		    (fa -> fa_present & FA_FILESIZE) ? fa -> fa_filesize : 0);
	    if (ut) {
		mtime = gtime (ut2tm (ut));
		s = ctime (&mtime);
		if (mtime < longtimeago || mtime > now)
		    fprintf (lsfp, "%-7.7s %-4.4s ", s + 4, s + 20);
		else
		    fprintf (lsfp, "%-12.12s ", s + 4);
	    }
	    else
		fprintf (lsfp, "             ");
	}
	if (!invis) {
	    char *dp;

	    dp = top && (fa -> fa_present & FA_FILENAME) ? fa -> fa_files[0]
			: entry;
#ifdef	BRIDGE
	    fprintf (lsfp, "%s%s\r\n", dp, multi && recurse ? ":" : "");
#else
	    fprintf (lsfp, "%s%s\n", dp, multi && recurse ? ":" : "");
#endif
	}
    }

    if (ftg -> ftg_flags & FTG_DESELECT) {
	register struct FTAMdeselect   *ftde = &ftg -> ftg_deselect;

	ftam_diag (ftde -> ftde_diags, ftde -> ftde_ndiag, 1,
		ftde -> ftde_action);
	ftam_chrg (&ftde -> ftde_charges);
    }

    if (recurse) {
	struct FADUidentity faduids;
	register struct FADUidentity *faduid = &faduids;

	faduid -> fa_type = FA_FIRSTLAST;
	faduid -> fa_firstlast = FA_FIRST;

	(void) fdffnx (NOTOK, (struct PSAPdata *) 0, 0);
	(void) getvf (file, NULLCP, faduid, vf, fdffnx);

	(void) fdfls (file);

	(void) fdffnx (NOTOK, (struct PSAPdata *) 0, 0);
	didrecurse++;
    }

    if (top && !last && didrecurse)
	fprintf (lsfp, "\n");

    FTGFREE (ftg);
    return OK;

you_lose: ;
    FTGFREE (ftg);
    return NOTOK;
}

/*  */

static int  fdfls (file)
char   *file;
{
    register int    i,
		    j,
                    w;
    int     columns,
            width,
            lines;
    char   *bp,
    	    buffer[BUFSIZ];
    register struct filent *fi,
                          **xi,
                          **yi;

    switch (realstore) {
	case RFS_UNIX:
	    if (strcmp (file, ".")) {
#ifdef apollo
                if (*file == '/')
		    (void) sprintf (bp = buffer, "%s", file);
                else
#endif
		(void) sprintf (bp = buffer, "%s/", file);
		bp += strlen (bp);
		i = bp - buffer;
		for (xi = &filents; fi = *xi;)
		    if (strncmp (fi -> fi_name, buffer, i) == 0) {
			fi -> fi_entry = fi -> fi_name + i;
			if (!dashl && !silent && *fi -> fi_entry == '.') {
			    *xi = fi -> fi_next;
			    if (fi -> fi_name)
				free (fi -> fi_name);
			    if (fi -> fi_oid)
				oid_free (fi -> fi_oid);
			    free ((char *) fi);

			    nfilent--;
			}
			else
			    xi = &fi -> fi_next;
		    }
	    }
	    else
		bp = buffer;
	    break;

	default:
	    bp = buffer;
	    break;
    }
    
    switch (nfilent) {
	case 0: 
	    break;

	case 1: 
	    fi = filents;
	    if (dashl)
		(void) ls (fi -> fi_name, fi -> fi_entry, 0, 1, 1, 0, 0);
	    else
#ifdef	BRIDGE
		fprintf (lsfp, "%s\r\n", fi -> fi_entry);
#else
		fprintf (lsfp, "%s\n", fi -> fi_entry);
#endif
	    break;

	default: 
	    xi = (struct filent **)
	                        calloc ((unsigned) (nfilent + 1), sizeof *xi);
	    if (xi) {
		for (fi = filents, yi = xi; fi; fi = fi -> fi_next)
		    *yi++ = fi;
		qsort ((char *) xi, nfilent, sizeof *xi, filcmp);
	    }

	    if (dashl) {
		if (xi) {
		    for (filents = NULL, yi--; yi >= xi; yi--) {
			fi = *yi;
			fi -> fi_next = filents;
			filents = fi;
		    }

		    free ((char *) xi);
		}

		for (fi = filents; fi; fi = fi -> fi_next)
		    (void) ls (fi -> fi_name, fi -> fi_entry, 0, fi == filents,
				fi -> fi_next == NULL, 0, 1);
		break;
	    }

	    if (!xi) {
		for (fi = filents; fi; fi = fi -> fi_next)
#ifdef	BRIDGE
		    fprintf (lsfp, "%s\r\n", fi -> fi_entry);
#else
		    fprintf (lsfp, "%s\n", fi -> fi_entry);
#endif
		break;
	    }

	    width = 0;
	    for (yi = xi; fi = *yi; yi++)
		if ((w = strlen (fi -> fi_entry)) > width)
		    width = w;
	    if (lsfp != stdout) {
		columns = 1;
		lines = nfilent;
	    }
	    else {
		if ((columns = ncols (lsfp) / (width = (width + 8) & ~7)) == 0)
		    columns = 1;
		lines = (nfilent + columns - 1) / columns;
	    }
	    for (i = 0; i < lines; i++)
		for (j = 0; j < columns; j++) {
		    fi = xi[w = j * lines + i];
		    fprintf (lsfp, "%s", fi -> fi_entry);
		    if (w + lines >= nfilent) {
#ifdef	BRIDGE
			(void) fputc ('\r', lsfp);
			(void) fputc ('\n', lsfp);
#else
			(void) fputc ('\n', lsfp);
#endif
			break;
		    }
		    for (w = strlen (fi -> fi_entry);
			    w < width;
			    w = (w + 8) & ~7)
			(void) fputc ('\t', lsfp);
		}
	    free ((char *) xi);
    }

    return OK;
}

/*  */

/* ARGSUSED */

int	fdffnx (fd, px, status)
int	fd;
register struct PSAPdata *px;
int	status;
{
    register int    i;
    register PE	    pe,
		   *pep;
    register struct filent *fi;

    if (px == NULL) {
	register struct filent *gi;

	for (fi = filents; fi; fi = gi) {
	    gi = fi -> fi_next;
	    if (fi -> fi_name);
		free (fi -> fi_name);
	    if (fi -> fi_oid)
		oid_free (fi -> fi_oid);
	    free ((char *) fi);
	}
	filents = NULL, nfilent = toomany = 0;

	silent = status;
	return OK;
    }

    for (pep = px -> px_info, i = px -> px_ninfo - 1; i >= 0; pep++, i--) {
	int	result;
	struct type_DOCS_NBS__9__Datatype1 *d9;
	struct FTAMattributes fas;
	register struct FTAMattributes *fa = &fas;
	struct FTAMindication ftis;

	if ((pe = *pep) == NULLPE)
	    continue;

	d9 = NULL;
	if (decode_DOCS_NBS__9__Datatype1 (pe, 1, NULLIP, NULLVP, &d9)
		== NOTOK) {
	    if (silent)
		globerr = PY_pepy;
	    else
		advise (NULLCP, "%s", PY_pepy);

	    if (d9)
		free_DOCS_NBS__9__Datatype1 (d9);

	    continue;
	}

	if (debug)
	    WATCHP (DOCS_NBS__9__Datatype1, pe, 1);

	bzero ((char *) fa, sizeof *fa);
	result = fdf_d2attrs (ftamfd, d9, fa, &ftis);

	free_DOCS_NBS__9__Datatype1 (d9);

	if (result == NOTOK) {
	    register struct FTAMabort *fta = &ftis.fti_abort;

	    if (silent)
		globerr = "unable to interpret datatype";
	    else
		ftam_diag (fta -> fta_diags, fta -> fta_ndiag, fta -> fta_peer,
			   FACTION_PERM);
	    FAFREE (fa);

	    continue;
	}

	fi = (struct filent *) calloc (1, sizeof *fi);
	if (fi == NULL) {
	    if (toomany == 0) {
		if (silent)
		    globerr = "too many files, listing truncated";
		else
		    advise (NULLCP, "too many files, listing truncated");
		toomany++;
	    }

	    FAFREE (fa);
	    break;
	}

	if (!(fa -> fa_present & (FA_FILENAME | FA_CONTENTS))) {
	    if (silent)
		globerr = "no filename/contents found in FDF entry";
	    else
		advise (NULLCP, "no filename/contents found in FDF entry");

	    FAFREE (fa);
	    continue;
	}

	fi -> fi_name = fa -> fa_files[0];
	if (*fi -> fi_name == '/' && *(fi -> fi_name + 1) == '/')
	    fi -> fi_entry = fi -> fi_name + 1;
	else
	    fi -> fi_entry = fi -> fi_name;
	fa -> fa_files[0] = NULLCP;
	fi -> fi_oid = fa -> fa_contents;
	fa -> fa_contents = NULLOID;
	fi -> fi_next = filents;
	filents = fi, nfilent++;

	FAFREE (fa);
    }

    PXFREE (px);

    return status;
}


static int  filcmp (a, b)
struct filent **a,
	      **b;
{
    return strcmp ((*a) -> fi_entry, (*b) -> fi_entry);
}
