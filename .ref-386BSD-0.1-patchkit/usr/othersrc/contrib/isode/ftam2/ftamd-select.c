/* ftamd-select.c - FTAM responder -- selection regime */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftamd-select.c,v 7.4 91/02/22 09:23:54 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftamd-select.c,v 7.4 91/02/22 09:23:54 mrose Interim $
 *
 *
 * $Log:	ftamd-select.c,v $
 * Revision 7.4  91/02/22  09:23:54  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/12/23  18:40:09  mrose
 * update
 * 
 * Revision 7.2  90/11/21  11:30:40  mrose
 * sun
 * 
 * Revision 7.1  90/07/01  21:03:20  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:54:29  mrose
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


#include <grp.h>
#include <stdio.h>
#include <pwd.h>
#include "ftamsystem.h"


#define	NUID	400
#define	NGID	400


#define	FA_CHATTR \
    (FA_FILENAME | FA_ACCOUNT)


#define	FA_PERM_WRITE	(FA_PERM_INSERT | FA_PERM_REPLACE | FA_PERM_EXTEND \
				| FA_PERM_ERASE)
#define	FA_PERM_OWNER	FA_PERM_CHNGATTR
#define	FA_PERM_PARENT	FA_PERM_DELETE


#ifdef	SYS5
struct group  *getgrnam ();
struct passwd *getpwnam (), *getpwent (), *getpwuid ();
#endif

/*    DATA */

static char mvfile[MAXPATHLEN];
static PE   rdparam = NULLPE;

char   *getfile ();
#ifdef	BRIDGE
#define	E_OK	R_OK
#else
char   *getuser (), *getgroup ();
#endif


long	lseek ();

#ifdef	SYS5
#define	EACCESS	access

#define	rename(f1,f2)	\
    	(unlink (f2), link (f1, f2) != NOTOK ? unlink (f1) : NOTOK)
#endif

#ifdef	BRIDGE
#define	ftp_access(file,mode)	ftp_exist (file)
#endif

/*    SELECTION REGIME */

int	ftam_selection (ftg, ftm)
register struct FTAMgroup *ftg,
			  *ftm;
{
    int     action,
            state;

    bzero ((char *) ftm, sizeof *ftm);
    ftm -> ftg_flags = ftg -> ftg_flags;

    statok = 0;

    state = FSTATE_SUCCESS;
    if (ftg -> ftg_flags & FTG_SELECT) {
	register struct FTAMselect *ftse = &ftg -> ftg_select;
	register struct FTAMattributes *fa = &ftse -> ftse_attrs;
	struct FTAMdiagnostic *dp = ftm -> ftg_select.ftse_diags;

	errno = 0;
	if (!(fa -> fa_present & FA_FILENAME)
		|| fa -> fa_nfile != 1
		|| (myfile = getfile (fa -> fa_files[0])) == NULL
#ifdef	BRIDGE
		|| ftp_access (myfile, E_OK) == NOTOK) {
#else
		|| stat (myfile, &myst) == NOTOK) {
#endif
	    dp -> ftd_type = DIAG_PERM;
	    dp -> ftd_identifier = FS_SEL_FILENAME;
	    dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	    dp -> ftd_delay = DIAG_NODELAY;
	    if (errno) {
		(void) strcpy (dp -> ftd_data, sys_errname (errno));
		dp -> ftd_cc = strlen (dp -> ftd_data);
	    }
	    else
		dp -> ftd_cc = 0;
	    dp++;

	    state = FSTATE_FAILURE;
	    goto done_select;
	}
	statok = 1;
#ifndef	BRIDGE
	switch (myst.st_mode & S_IFMT) {
	    case S_IFREG: 
	    case S_IFDIR: 
		break;

	    default:
		if (myst.st_dev == null_dev && myst.st_ino == null_ino) {
		    myst.st_mode &= ~S_IFMT, myst.st_mode |= S_IFREG;
		    break;
		}
		dp -> ftd_type = DIAG_PERM;
		dp -> ftd_identifier = FS_SEL_AVAIL;
		dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
		dp -> ftd_delay = DIAG_NODELAY;
		dp -> ftd_cc = 0;
		dp++;

		state = FSTATE_FAILURE;
		goto done_select;
	}
#else
/* can't check a file you don't have */
#endif

	if (ftg -> ftg_flags & FTG_RDATTR)
	    ftse -> ftse_access |= FA_PERM_READATTR;
	if (ftg -> ftg_flags & FTG_CHATTR)
	    ftse -> ftse_access |= FA_PERM_CHNGATTR;
	if (ftg -> ftg_flags & FTG_DELETE)
	    ftse -> ftse_access |= FA_PERM_DELETE;
	if (chkaccess (NOTOK, ftse -> ftse_access, &ftse -> ftse_conctl, &dp)
		== NOTOK) {
	    state = FSTATE_FAILURE;
	    goto done_select;
	}
#ifndef	BRIDGE
	if (ftse -> ftse_account && strlen (ftse -> ftse_account) > 1) {
	    if ((mygid = findgid (ftse -> ftse_account)) == NOTOK) {
		dp -> ftd_type = DIAG_PERM;
		dp -> ftd_identifier = FS_SEL_ACCOUNT;
		dp -> ftd_observer = EREF_RFSU, dp -> ftd_source = EREF_IFSU;
		dp -> ftd_delay = DIAG_NODELAY;
		dp -> ftd_cc = 0;
		dp++;

		state = FSTATE_FAILURE;
		goto done_select;
	    }
	}
	else
#else
/* no account checking */
#endif
	    mygid = NOTOK;

done_select: ;
	myaccess = ftse -> ftse_access;
	ftm -> ftg_select.ftse_state = state;
	ftm -> ftg_select.ftse_ndiag = dp - ftm -> ftg_select.ftse_diags;
    }

    if (ftg -> ftg_flags & FTG_CREATE) {
	register struct FTAMcreate *ftce = &ftg -> ftg_create;
	register struct FTAMattributes *fa = &ftce -> ftce_attrs;
	struct FTAMdiagnostic *dp = ftm -> ftg_create.ftce_diags;
	register struct vfsmap *vf;

	if (!(fa -> fa_present & FA_FILENAME)
		|| fa -> fa_nfile != 1
		|| (myfile = getfile (fa -> fa_files[0])) == NULL) {
	    dp -> ftd_type = DIAG_PERM;
	    dp -> ftd_identifier = FS_SEL_FILENAME;
	    dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	    dp -> ftd_delay = DIAG_NODELAY;
	    dp -> ftd_cc = 0;
	    dp++;

	    state = FSTATE_FAILURE;
	    goto done_create;
	}
#ifdef	BRIDGE
	if (ftp_access (myfile, E_OK) != NOTOK)
#else
	if (stat (myfile, &myst) != NOTOK)
#endif
	    statok = 1;
	if (statok) {
#ifndef	BRIDGE  /* Assume file type allow corrective action */
	    switch (myst.st_mode & S_IFMT) {
		case S_IFREG: 
		    break;

		case S_IFDIR: 
		    switch (ftce -> ftce_override) {
			case FOVER_WRITE: 
			    dp -> ftd_type = DIAG_PERM;
			    dp -> ftd_identifier = FS_SEL_CRELOSE;
			    dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
			    dp -> ftd_delay = DIAG_NODELAY;
			    dp -> ftd_cc = 0;
			    dp++;

			    state = FSTATE_FAILURE;
			    goto done_create;

			default: 
			    break;
		    }
		    break;

		default: 
		    if (myst.st_dev == null_dev && myst.st_ino == null_ino) {
			myst.st_mode &= ~S_IFMT, myst.st_mode |= S_IFREG;
			break;
		    }
		    dp -> ftd_type = DIAG_PERM;
		    dp -> ftd_identifier = FS_SEL_AVAIL;
		    dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
		    dp -> ftd_delay = DIAG_NODELAY;
		    dp -> ftd_cc = 0;
		    dp++;

		    state = FSTATE_FAILURE;
		    goto done_create;
	    }
#endif
	    switch (ftce -> ftce_override) {
		case FOVER_FAIL: 
		    dp -> ftd_type = DIAG_PERM;
		    dp -> ftd_identifier = FS_SEL_EXISTS;
		    dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
		    dp -> ftd_delay = DIAG_NODELAY;
		    dp -> ftd_cc = 0;
		    dp++;

		    state = FSTATE_FAILURE;
		    goto done_create;

		case FOVER_SELECT:
			if (ftg -> ftg_flags & FTG_RDATTR)
			    ftce -> ftce_access |= FA_PERM_READATTR;
			if (ftg -> ftg_flags & FTG_CHATTR)
			    ftce -> ftce_access |= FA_PERM_CHNGATTR;
			if (ftg -> ftg_flags & FTG_DELETE)
			    ftce -> ftce_access |= FA_PERM_DELETE;
			if (chkaccess (NOTOK, ftce -> ftce_access, 
				    &ftce -> ftce_conctl, &dp) == NOTOK) {
			    state = FSTATE_FAILURE;
			    goto done_create;
			}
		    break;

		default: 
		    break;
	    }
	}

	for (vf = vfs; vf -> vf_entry; vf++)
	    if (vf -> vf_oid
		    && oid_cmp (vf -> vf_oid, fa -> fa_contents) == 0)
		break;
	if (!vf -> vf_entry) {
	    (void) sprintf (dp -> ftd_data,
			   "invalid contents-type %s",
			   sprintoid (fa -> fa_contents));
	    dp -> ftd_cc = strlen (dp -> ftd_data);
	    dp -> ftd_type = DIAG_PERM;
	    dp -> ftd_identifier = FS_SEL_ATRVALUE;
	    dp -> ftd_observer = EREF_RFSU, dp -> ftd_source = EREF_IFSU;
	    dp -> ftd_delay = DIAG_NODELAY;
	    dp++;

	    state = FSTATE_FAILURE;
	    goto done_create;
	}

	if (chkattrs (fa, fa -> fa_present & ~(FA_FILENAME | FA_CONTENTS),
		    1, &dp) == NOTOK) {
	    state = FSTATE_FAILURE;
	    goto done_create;
	}

#ifndef	BRIDGE   /* no account checking */
	if (ftce -> ftce_account && strlen (ftce -> ftce_account) > 1) {
	    if ((mygid = findgid (ftce -> ftce_account)) == NOTOK) {
		dp -> ftd_type = DIAG_PERM;
		dp -> ftd_identifier = FS_SEL_ACCOUNT;
		dp -> ftd_observer = EREF_RFSU, dp -> ftd_source = EREF_IFSU;
		dp -> ftd_delay = DIAG_NODELAY;
		dp -> ftd_cc = 0;
		dp++;

		state = FSTATE_FAILURE;
		goto done_create;
	    }
	}
	else
#endif
	    mygid = NOTOK;

done_create: ;
	ftm -> ftg_create.ftce_state = state;
	ftm -> ftg_create.ftce_ndiag = dp - ftm -> ftg_create.ftce_diags;
    }

    if (ftg -> ftg_flags & FTG_RDATTR)
	ftm -> ftg_readattr.ftra_action = FACTION_SUCCESS;

    if (ftg -> ftg_flags & FTG_CHATTR) {
	register struct FTAMchngattr   *ftca = &ftg -> ftg_chngattr;
	register struct FTAMattributes *fa = &ftca -> ftca_attrs;
	struct FTAMdiagnostic *dp = ftm -> ftg_chngattr.ftca_diags;

	if (chkattrs (fa, fa -> fa_present, 0, &dp) == NOTOK)
	    action = FACTION_PERM;
	else
	    action = FACTION_SUCCESS;

	ftm -> ftg_chngattr.ftca_action = action;
	ftm -> ftg_chngattr.ftca_ndiag = dp - ftm -> ftg_chngattr.ftca_diags;
    }
    
    if (ftg -> ftg_flags & FTG_OPEN) {
	register struct FTAMopen *ftop = &ftm -> ftg_open;

	ftop -> ftop_state = FSTATE_FAILURE;
	ftop -> ftop_action = FACTION_SUCCESS;
	if (ftop -> ftop_contents = ftg -> ftg_open.ftop_contents)
	    ftop -> ftop_parameter = ftg -> ftg_open.ftop_parameter;

	FCINIT (&ftop -> ftop_conctl);
    }

    if (ftg -> ftg_flags & FTG_CLOSE)
	ftm -> ftg_close.ftcl_action = FACTION_SUCCESS;

    if (ftg -> ftg_flags & FTG_DESELECT)
	ftm -> ftg_deselect.ftde_action = FACTION_SUCCESS;

    if (ftg -> ftg_flags & FTG_DELETE)
	ftm -> ftg_delete.ftxe_action = FACTION_SUCCESS;

    if (ftg -> ftg_flags & FTG_SELECT) {
	register struct FTAMselect *ftse = &ftm -> ftg_select;
	register struct FTAMattributes *fa = &ftse -> ftse_attrs;

	if (state != FSTATE_SUCCESS) {
	    ftse -> ftse_action = FACTION_PERM;
	    *fa = ftg -> ftg_select.ftse_attrs;	/* struct copy */
	    ftm -> ftg_flags &= ~(FTG_RDATTR | FTG_CHATTR | FTG_OPEN);
	    return;
	}
	ftse -> ftse_action = FACTION_SUCCESS;
	fa -> fa_present = FA_FILENAME;
	fa -> fa_nfile = 0;
	fa -> fa_files[fa -> fa_nfile++] = myfile;
    }

    if (ftg -> ftg_flags & FTG_CREATE) {
	register struct FTAMattributes *fa = &ftg -> ftg_create.ftce_attrs;
	register struct FTAMcreate *ftce = &ftm -> ftg_create;
	struct FTAMdiagnostic  *dp = ftce -> ftce_diags + ftce -> ftce_ndiag;

	if (state != FSTATE_SUCCESS) {
	    ftce -> ftce_action = FACTION_PERM;
	    ftce -> ftce_attrs = *fa;	/* struct copy */
	    ftm -> ftg_flags &= ~(FTG_RDATTR | FTG_CHATTR | FTG_OPEN);
	    return;
	}
	switch (ftg -> ftg_create.ftce_override) {
	    case FOVER_SELECT:
	    default: 
		if (statok)
		    break;
		goto do_create;

	    case FOVER_DELETE: 
#ifdef	BRIDGE
		if (statok && ftp_delete (myfile) == NOTOK) {
#else
		if (statok
			&& ((myst.st_mode & S_IFMT) == S_IFREG
				    ? unlink (myfile)
				    : rmdir (myfile)) == NOTOK) {
	    bad_override: ;
#endif
		    dp -> ftd_type = DIAG_PERM;
		    dp -> ftd_identifier = FS_SEL_CRELOSE;
		    dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
		    dp -> ftd_delay = DIAG_NODELAY;
#ifdef	BRIDGE
		    (void) strcpy (dp -> ftd_data, ftp_error);
#else
		    (void) strcpy (dp -> ftd_data, sys_errname (errno));
#endif
		    dp -> ftd_cc = strlen (dp -> ftd_data);
		    dp++;

	    bad_create: ;
		    ftce -> ftce_action = FACTION_PERM;
		    ftce -> ftce_attrs = *fa;	/* struct copy */
		    ftce -> ftce_ndiag = dp - ftce -> ftce_diags;

		    ftce -> ftce_state = FSTATE_FAILURE;
		    ftm -> ftg_flags &= ~(FTG_RDATTR | FTG_CHATTR | FTG_OPEN);
		    return;
		}
		/* else fall */

	    case FOVER_FAIL:
		statok = 0;
	do_create: ;
		if (!(fa -> fa_present & FA_CONTENTS)
			|| oid_cmp (vfs[VFS_FDF].vf_oid, fa -> fa_contents)) {
#ifdef	BRIDGE
		    if (ftp_create (myfile) == NOTOK) {
#else
		    if ((myfd = open (myfile, O_RDWR | O_CREAT | O_TRUNC,
					0666)) == NOTOK) {
#endif
		bad_open: ;
			dp -> ftd_type = DIAG_PERM;
			dp -> ftd_identifier = FS_SEL_CREATE;
			dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
			dp -> ftd_delay = DIAG_NODELAY;
			(void) strcpy (dp -> ftd_data, sys_errname (errno));
			dp -> ftd_cc = strlen (dp -> ftd_data);
			dp++;

			goto bad_create;
		    }
		}
		else
#ifdef	BRIDGE
		    if (ftp_mkdir (myfile) == NOTOK)
#else
		    if (mkdir (myfile, 0755) == NOTOK)
#endif
			goto bad_open;
		if (chngattrs (fa -> fa_present
			& ~(FA_FILENAME | FA_CONTENTS), fa, &dp) == NOTOK)
		    goto bad_create;
		break;

	    case FOVER_WRITE: 
		if (!statok)
		    goto do_create;
#ifndef	BRIDGE
#ifdef	SUNOS4
		if (myst.st_size > 0 && truncate (myfile, (off_t) 0) == NOTOK)
		    goto bad_override;
#else
		if (myst.st_size > 0 && truncate (myfile, 0) == NOTOK)
		    goto bad_override;
#endif
#endif
		break;
	}

	ftce -> ftce_action = FACTION_SUCCESS;
	(void) readattrs (FA_FILENAME | FA_ACTIONS | FA_CONTENTS,
			  &ftce -> ftce_attrs, fa -> fa_contents,
			  fa -> fa_parameter, myfile, &myst, &dp);
	if (fa -> fa_present & FA_ACTIONS)
	    ftce -> ftce_attrs.fa_permitted &= fa -> fa_permitted;
	ftce -> ftce_ndiag = dp - ftce -> ftce_diags;
    }

    if (ftg -> ftg_flags & FTG_RDATTR
	    && (ftm -> ftg_readattr.ftra_action == FACTION_SUCCESS)) {
	register struct FTAMreadattr   *ftra = &ftm -> ftg_readattr;
	struct FTAMdiagnostic  *dp = ftra -> ftra_diags + ftra -> ftra_ndiag;

	if (!statok) {
#ifdef	BRIDGE
	    if (0) { /* assume OK */
#else
	    if ((myfd != NOTOK ? fstat (myfd, &myst)
			       : stat (myfile, &myst)) == NOTOK) {
		dp -> ftd_type = DIAG_PERM;
		dp -> ftd_identifier = FS_MGT_READ;
		dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
		dp -> ftd_delay = DIAG_NODELAY;
		dp -> ftd_cc = 0;
		dp++;
#endif

	bad_readattr: ;
		ftra -> ftra_action = FACTION_PERM;
		ftra -> ftra_ndiag = dp - ftra -> ftra_diags;
		return;
	    }
	    else
		statok++;
	}
	if (readattrs (ftg -> ftg_readattr.ftra_attrnames,
		       &ftra -> ftra_attrs, ftg -> ftg_flags & FTG_OPEN
		           ? ftg -> ftg_open.ftop_contents : NULLOID, NULLPE,
		       myfile, &myst, &dp) == NOTOK)
	    goto bad_readattr;
	ftra -> ftra_ndiag = dp - ftra -> ftra_diags;
    }

    if (ftg -> ftg_flags & FTG_CHATTR
	    && (ftm -> ftg_chngattr.ftca_action == FACTION_SUCCESS)) {
	register struct FTAMchngattr   *ftca = &ftm -> ftg_chngattr;
	register struct FTAMattributes *fa = &ftg -> ftg_chngattr.ftca_attrs;
	struct FTAMdiagnostic  *dp = ftca -> ftca_diags + ftca -> ftca_ndiag;

	if (chngattrs (fa -> fa_present, fa, &dp) == NOTOK) {
	    ftca -> ftca_action = FACTION_PERM;
	    ftca -> ftca_ndiag = dp - ftca -> ftca_diags;
	    return;
	}

	ftca -> ftca_ndiag = dp - ftca -> ftca_diags;
    }

    if (ftg -> ftg_flags & FTG_OPEN) {
#ifndef	BRIDGE
	int	mode;
#endif
	register struct FTAMopen *ftop = &ftm -> ftg_open;
	struct FTAMdiagnostic  *dp = ftop -> ftop_diags + ftop -> ftop_ndiag;
	
	ftop -> ftop_state = FSTATE_SUCCESS;
	if (statok == 0) {
	    if (stat (myfile, &myst) == NOTOK) {
#ifndef	BRIDGE
unavailable: ;
#endif
		dp -> ftd_type = DIAG_PERM;
		dp -> ftd_identifier = FS_SEL_AVAIL;
		dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
		dp -> ftd_delay = DIAG_NODELAY;
		if (errno) {
		    (void) strcpy (dp -> ftd_data, sys_errname (errno));
		    dp -> ftd_cc = strlen (dp -> ftd_data);
		}
		else
		    dp -> ftd_cc = 0;
		dp++;

		ftop -> ftop_state = FSTATE_FAILURE;
		goto done_open;
	    }
	    else
		statok = 1;
	}

        if (ftop -> ftop_contents == NULL) {
	    register struct FTAMattributes *fa;

	    if (ftg -> ftg_flags & FTG_CREATE) {
		fa = &ftg -> ftg_create.ftce_attrs;

		ftop -> ftop_contents = fa -> fa_contents;
		ftop -> ftop_parameter = fa -> fa_parameter;
		advise (LLOG_DEBUG, NULLCP,
			"using contents-type from CREATE <%s, 0x%x>",
			oid2ode (ftop -> ftop_contents),
			ftop -> ftop_parameter);
		goto find_myvfs;
	    }

	    if ((ftg -> ftg_flags & FTG_RDATTR)
		    && ftm -> ftg_readattr.ftra_action == FACTION_SUCCESS
		    && ((fa = &ftm -> ftg_readattr.ftra_attrs) -> fa_present
			        & FA_CONTENTS)) {
		ftop -> ftop_contents = fa -> fa_contents;
		ftop -> ftop_parameter = fa -> fa_parameter;
		advise (LLOG_DEBUG, NULLCP,
			"using contents-type from READ-ATTRIBUTE <%s, 0x%x>",
			oid2ode (ftop -> ftop_contents),
			ftop -> ftop_parameter);

find_myvfs: ;
		if ((myvf = st2vfs (myfd, myfile, &myst, ftop -> ftop_contents,
				    ftamfd)) == NULL)
		    goto no_ascertain;
		if (oid_cmp (ftop -> ftop_contents, myvf -> vf_oid) == 0)
		    goto find_param;
		advise (LLOG_DEBUG, NULLCP,
			"wrong intuition; back to step one");
		ftop -> ftop_contents = NULLOID;
		ftop -> ftop_parameter = NULLPE;
	    }

	    if ((myvf = st2vfs (myfd, myfile, &myst, NULLOID, ftamfd))
		    == NULL) {
no_ascertain: ;
		(void) strcpy (dp -> ftd_data,
			       "unable to ascertain contents-type");
		dp -> ftd_cc = strlen (dp -> ftd_data);
		dp -> ftd_type = DIAG_PERM;
		dp -> ftd_identifier = FS_ACC_LCL;
		dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
		dp -> ftd_delay = DIAG_NODELAY;
		dp++;

		ftop -> ftop_state = FSTATE_FAILURE;
		goto done_open;
	    }
	    ftop -> ftop_contents = myvf -> vf_oid;
	    advise (LLOG_DEBUG, NULLCP,
		    "using contents-type from st2vfs: %s",
		    oid2ode (ftop -> ftop_contents));

find_param: ;
	    if (myvf -> vf_mandatory == 0 && ftop -> ftop_parameter)
		ftop -> ftop_parameter = NULLPE;
	    else if (ftop -> ftop_parameter == NULL) {
		char buffer[BUFSIZ];

		if (rdparam)
		    pe_free (rdparam), rdparam = NULLPE;
		if (enc_f (myvf -> vf_number, &_ZDOCS_mod, &rdparam, 1, 0,
			   NULLCP, myvf -> vf_parameter) == NOTOK) {
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "unable to build parameter: %s", PY_pepy);
		    if (rdparam)
			pe_free (rdparam), rdparam = NULLPE;
		    goto no_ascertain;
		}
		ftop -> ftop_parameter = rdparam;

		vpushstr (buffer);
		vunknown (ftop -> ftop_parameter);
		vpopstr ();
		advise (LLOG_DEBUG, NULLCP,
			"generating parameter from vfs: %s", buffer);
	    }
	}
	else
	    if ((myvf = st2vfs (myfd, myfile, &myst, ftop -> ftop_contents,
				ftamfd)) == NULL) {
		dp -> ftd_cc = 0;
bad_param: ;
		dp -> ftd_type = DIAG_PERM;
		dp -> ftd_identifier = FS_ACC_TYPINCON;
		dp -> ftd_observer = EREF_RFSU, dp -> ftd_source = EREF_IFSU;
		dp -> ftd_delay = DIAG_NODELAY;
		dp++;

		ftop -> ftop_state = FSTATE_FAILURE;
		goto done_open;
	    }

        if (oid_cmp (ftop -> ftop_contents, myvf -> vf_oid)) {
	    advise (LLOG_NOTICE, NULLCP, "simplifying document type");

	    ftop -> ftop_contents = myvf -> vf_oid;
	    ftop -> ftop_parameter = NULLPE;
	    if (myvf -> vf_mandatory) {
		char buffer[BUFSIZ];

		if (rdparam)
		    pe_free (rdparam), rdparam = NULLPE;
		if (enc_f (myvf -> vf_number, &_ZDOCS_mod, &rdparam, 1, 0,
			   NULLCP, myvf -> vf_parameter) == NOTOK) {
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "unable to build parameter: %s", PY_pepy);
		    if (rdparam)
			pe_free (rdparam), rdparam = NULLPE;
		    goto no_ascertain;
		}
		ftop -> ftop_parameter = rdparam;

		vpushstr (buffer);
		vunknown (ftop -> ftop_parameter);
		vpopstr ();
		advise (LLOG_DEBUG, NULLCP,
			"generating parameter from myvf: %s", buffer);
	    }
	}

	if (ftop -> ftop_parameter) {
	    if (myvf -> vf_number < 0) {
		(void) sprintf (dp -> ftd_data,
				"unexpected document type parameter");
		dp -> ftd_cc = strlen (dp -> ftd_data);
		goto bad_param;
	    }
	    myparam = NULL;
	    if (dec_f (myvf -> vf_number, &_ZDOCS_mod, ftop -> ftop_parameter,
		       1, NULLIP, NULLVP, &myparam) == NOTOK) {
		(void) sprintf (dp -> ftd_data,
				"unable to parse document type parameter: %s",
				PY_pepy);
		dp -> ftd_cc = strlen (dp -> ftd_data);
		goto bad_param;
	    }
	    if (myvf -> vf_check
		    && (*myvf -> vf_check) (myparam, dp -> ftd_data)
			    == NOTOK)
		goto bad_param;
	}
	else
	    if (myvf -> vf_mandatory > 0) {
		(void) strcpy (dp -> ftd_data,
			       "mandatory document type parameter missing");
		dp -> ftd_cc = strlen (dp -> ftd_data);
		goto bad_param;
	    }

	mymode = ftg -> ftg_open.ftop_mode;
#ifndef	BRIDGE
	if (mymode & FA_PERM_WRITE)
	    mode = (mymode & FA_PERM_READ) ? O_RDWR : O_WRONLY;
	else
	    mode = O_RDONLY;
#endif

	errno = 0;
#ifndef	BRIDGE
	switch (myst.st_mode & S_IFMT) {
	    case S_IFREG:
		if (myfd == NOTOK && (myfd = open (myfile, mode)) == NOTOK)
		    goto unavailable;
		break;
		    
	    case S_IFDIR:
		if (mode == O_RDONLY)
		    break;
		/* else fall */

	    default:
		goto unavailable;
	}
#endif

	myconctl = ftg -> ftg_open.ftop_conctl;	/* struct copy */
	ftm -> ftg_open.ftop_conctl = myconctl;	/*   .. */
	mylockstyle = ftg -> ftg_open.ftop_locking;

	if (chkaccess (myfd, mymode, &myconctl, &dp) == NOTOK) {
	    ftop -> ftop_state = FSTATE_FAILURE;
	    goto done_open;
	}

done_open: ;
	ftop -> ftop_ndiag = dp - ftop -> ftop_diags;

	if (ftop -> ftop_state != FSTATE_SUCCESS) {
	    ftop -> ftop_action = FACTION_PERM;
	    return;
	}
    }

    if (ftg -> ftg_flags & FTG_CLOSE
	    && (ftm -> ftg_close.ftcl_action == FACTION_SUCCESS)) {
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
	if (myvf && myparam) {
	    (void) fre_obj (myparam, _ZDOCS_mod.md_dtab[myvf -> vf_number],
			    &_ZDOCS_mod, 1);
	    myparam = NULL;
	}
    }

    if (ftg -> ftg_flags & FTG_DESELECT) {
	register struct FTAMdeselect   *ftde = &ftm -> ftg_deselect;

	if (ftde -> ftde_action == FACTION_SUCCESS) {
	/* anything to charge if (mygid != NOTOK)?  ha! */
	}
    }

    if (ftg -> ftg_flags & FTG_DELETE) {
	register struct FTAMdelete *ftxe = &ftm -> ftg_delete;
	struct FTAMdiagnostic  *dp = ftxe -> ftxe_diags + ftxe -> ftxe_ndiag;

	if (ftxe -> ftxe_action == FACTION_SUCCESS) {
#ifdef	BRIDGE
	    if (ftp_delete (myfile) == NOTOK) {
#else
	    if (!statok && stat (myfile, &myst) == NOTOK)
		myst.st_mode = S_IFREG;
	    if (((myst.st_mode & S_IFMT) == S_IFREG ? unlink (myfile)
						    : rmdir (myfile))
			== NOTOK) {
#endif
		dp -> ftd_type = DIAG_PERM;
		dp -> ftd_identifier = FS_SEL_DELETE;
		dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
		dp -> ftd_delay = DIAG_NODELAY;
#ifdef	BRIDGE
		(void) strcpy (dp -> ftd_data, ftp_error);
#else
		(void) strcpy (dp -> ftd_data, sys_errname (errno));
#endif
		dp -> ftd_cc = strlen (dp -> ftd_data);
		dp++;

		ftxe -> ftxe_action = FACTION_PERM;
		ftxe -> ftxe_ndiag = dp - ftxe -> ftxe_diags;
		return;
	    }
	    advise (LLOG_NOTICE, NULLCP, "delete %s", myfile);
	}
    }
}

/*  */

#ifdef	BRIDGE
/* ARGSUSED */
#endif

static int  chkaccess (fd, request, fc, diags)
int	fd,
	request;
#ifndef	BRIDGE
register
#endif
struct FTAMconcurrency *fc;
register struct FTAMdiagnostic **diags;
{
    int     result;
#ifndef	BRIDGE
    register char  *cp;
#endif
    register struct FTAMdiagnostic *dp = *diags;

    result = OK;

#ifndef	BRIDGE
    if (((request & FA_PERM_READ) && EACCESS (myfile, R_OK) == NOTOK)
	    || ((request & FA_PERM_WRITE) && EACCESS (myfile, W_OK) == NOTOK)
	    || ((request & FA_PERM_OWNER)
			&& (myuid != myst.st_uid && myuid != 0))) {
no_access: ;
	dp -> ftd_type = DIAG_PERM;
	dp -> ftd_identifier = FS_SEL_ACCAVAIL;
	dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	dp -> ftd_delay = DIAG_NODELAY;
	dp -> ftd_cc = 0;
	dp++;

	result = NOTOK;
	goto out;
    }
    if (request & FA_PERM_PARENT) {
	if (cp = rindex (myfile, '/')) {
	    *cp = NULL;
	    result = EACCESS (*myfile ? myfile : "/", W_OK);
	    *cp = '/';
	}
	else
	    result = EACCESS (".", W_OK);

	if (result == NOTOK)
	    goto no_access;
    }
#else
/* already selected file and know it exists, FTP cannot tell us more */
#endif

#ifndef	BRIDGE
out: ;

    if (attrs & FATTR_STORAGE) {
	if (fd == NOTOK) {
	    mylock = 0;

	    if (((request & FA_PERM_READATTR)
			&& (fc -> fc_readattrlock & FLOCK_RESTRICT))
		    || ((request & FA_PERM_CHNGATTR)
			&& (fc -> fc_chngattrlock & FLOCK_RESTRICT))
		    || ((request & FA_PERM_DELETE)
			&& (fc -> fc_deletelock & FLOCK_RESTRICT))) {
		dp -> ftd_type = DIAG_PERM;
		dp -> ftd_identifier = FS_SEL_CONSUPRT;
		dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
		dp -> ftd_delay = DIAG_NODELAY;
		dp -> ftd_cc = 0;
		dp++;

		result = NOTOK;
	    }
	}
	else {
#ifdef	SYS5
	    struct flock    fs;
#endif

	    if ((request & FA_PERM_WRITE)
		    && ((fc -> fc_insertlock & FLOCK_RESTRICT)
			    || (fc -> fc_replacelock & FLOCK_RESTRICT)
			    || (fc -> fc_eraselock & FLOCK_RESTRICT)
			    || (fc -> fc_extendlock & FLOCK_RESTRICT))) {
		mylock = 1;
#ifndef	SYS5
		if (flock (fd, LOCK_EX) == NOTOK)
		    goto bad_concur;
#else
		fs.l_type = F_WRLCK;
		fs.l_whence = L_SET;
		fs.l_start = fs.l_len = 0;
		if (fcntl (fd, F_SETLKW, &fs) == NOTOK)
		    goto bad_concur;
#endif
	    }
	    else
		if ((request & FA_PERM_READ)
			&& (fc -> fc_readlock & FLOCK_RESTRICT)) {
		    mylock = 1;
#ifndef	SYS5
		    if (flock (fd, LOCK_SH) == NOTOK) {
#else
		    fs.l_type = F_RDLCK;
		    fs.l_whence = L_SET;
		    fs.l_start = fs.l_len = 0;
		    if (fcntl (fd, F_SETLKW, &fs) == NOTOK) {
#endif
bad_concur: ;
			dp -> ftd_type = DIAG_PERM;
			dp -> ftd_identifier = FS_ACC_CONAVAIL;
			dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
			dp -> ftd_delay = DIAG_NODELAY;
			(void) strcpy (dp -> ftd_data, sys_errname (errno));
			dp -> ftd_cc = strlen (dp -> ftd_data);
			dp++;

			mylock = 0;
			result = NOTOK;
		    }
		}
	}
    }
#endif

    *diags = dp;
    return result;
}

/*  */

static int  chkattrs (fa, present, select, diags)
register struct FTAMattributes *fa;
long	present;
int	select;
register struct FTAMdiagnostic **diags;
{
    int     id,
	    result;
    char   *file;
    register struct FTAMdiagnostic *dp = *diags;

    result = OK;

    present &= ~FA_FUTURESIZE;	/* be liberal in what you accept... */
    if (present & ~(FA_CHATTR | FA_RDATTR)) {
	dp -> ftd_type = DIAG_PERM;
	dp -> ftd_identifier = select ? FS_SEL_INITIAL
	    : (present & FA_RDATTR) ? FS_MGT_CHANGE : FS_MGT_EXIST;
	dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	dp -> ftd_delay = DIAG_NODELAY;
	dp -> ftd_cc = 0;
	dp++;

	result = NOTOK;
    }

    id = select ? FS_SEL_ATRVALUE : FS_MGT_VALUE;

    if (present & FA_FILENAME) {
	if (fa -> fa_nfile != 1
		|| (file = getfile (fa -> fa_files[0])) == NULL) {
	    dp -> ftd_type = DIAG_PERM;
	    dp -> ftd_identifier = id;
	    dp -> ftd_observer = EREF_RFSU, dp -> ftd_source = EREF_IFSU;
	    dp -> ftd_delay = DIAG_NODELAY;
	    (void) strcpy (dp -> ftd_data, "00 bad filename");
	    dp -> ftd_cc = strlen (dp -> ftd_data);
	    dp++;

	    result = NOTOK;
	}
	else
	    (void) strcpy (mvfile, file);
    }

#ifndef	BRIDGE
    if ((present & FA_ACCOUNT) && strlen (fa -> fa_account) > 1) {
	if (findgid (fa -> fa_account) == NOTOK) {
	    dp -> ftd_type = DIAG_PERM;
	    dp -> ftd_identifier = id;
	    dp -> ftd_observer = EREF_RFSU, dp -> ftd_source = EREF_IFSU;
	    dp -> ftd_delay = DIAG_NODELAY;
	    (void) strcpy (dp -> ftd_data, "02 bad storage account");
	    dp -> ftd_cc = strlen (dp -> ftd_data);
	    dp++;
	
	    result = NOTOK;
    	}
    }

    if ((present & FA_FILESIZE) && fa -> fa_filesize < 0) {
	dp -> ftd_type = DIAG_PERM;
	dp -> ftd_identifier = id;
	dp -> ftd_observer = EREF_RFSU, dp -> ftd_source = EREF_IFSU;
	dp -> ftd_delay = DIAG_NODELAY;
	(void) strcpy (dp -> ftd_data, "13bad filesize");
	dp -> ftd_cc = strlen (dp -> ftd_data);
	dp++;

	result = NOTOK;
    }
#endif

    *diags = dp;
    return result;
}

/*  */

int	readattrs (attrnames, fa, proposed, parameter, file, st, diags)
int	attrnames;
register struct FTAMattributes *fa;
OID	proposed;
PE	parameter;
char   *file;
struct stat *st;
register struct FTAMdiagnostic **diags;
{
#ifndef	BRIDGE
    int     result;
    char   *cp;
    register struct tm *tm;
#endif
    register struct FTAMdiagnostic *dp = *diags;

    fa -> fa_present = attrnames;
    fa -> fa_novalue = attrnames & (FA_SECURITY | FA_PRIVATE);

    if (attrnames & FA_FILENAME) {
	fa -> fa_nfile = 0;
	fa -> fa_files[fa -> fa_nfile++] = file;
    }

    if (attrnames & FA_ACTIONS) {
	fa -> fa_permitted = 0;
#ifndef	BRIDGE
	if (EACCESS (file, R_OK) != NOTOK)
#endif
	    fa -> fa_permitted |= FA_PERM_READ;

#ifndef	BRIDGE
	if (EACCESS (file, W_OK) != NOTOK)
#endif
	    fa -> fa_permitted |= FA_PERM_WRITE;

	if (fa -> fa_permitted & (FA_PERM_READ | FA_PERM_WRITE))
	    fa -> fa_permitted |= FA_PERM_TRAVERSAL;

	fa -> fa_permitted |= FA_PERM_READATTR;

#ifndef	BRIDGE
	if (myuid == st -> st_uid || myuid == 0)
#endif
	    fa -> fa_permitted |= FA_PERM_OWNER;

#ifndef	BRIDGE
	if (cp = rindex (file, '/')) {
	    *cp = NULL;
	    result = EACCESS (*file ? file : "/", W_OK);
	    *cp = '/';
	}
	else
	    result = EACCESS (".", W_OK);
	if (result != NOTOK)
#endif
	    fa -> fa_permitted |= FA_PERM_PARENT;
    }

    if (attrnames & FA_CONTENTS) {
	register struct vfsmap *vf;

	if (vf = st2vfs (myfd, file, st, proposed, ftamfd)) {
	    fa -> fa_contents = vf -> vf_oid;
	    if (proposed
		    && oid_cmp (proposed, vf -> vf_oid) == 0
		    && parameter
		    && vf -> vf_number >= 0
		    && vf -> vf_check) {
		caddr_t	p = NULL;

		if (dec_f (vf -> vf_number, &_ZDOCS_mod, parameter, 1, NULLIP,
			   NULLVP, &p) == NOTOK) {
		    advise (LLOG_NOTICE, NULLCP,
			    "unable to parse document type parameter: %s",
			    PY_pepy);
		    goto bad_param;
		}
		if ((*vf -> vf_check) (p, dp -> ftd_data) == NOTOK) {
		    advise (LLOG_NOTICE, NULLCP,
			    "unacceptable document type parameter: %s",
			    dp -> ftd_data);
		    goto bad_param;
		}
		(void) fre_obj (p, _ZDOCS_mod.md_dtab[vf -> vf_number],
				&_ZDOCS_mod, 1);
		fa -> fa_parameter = parameter;
	    }
bad_param: ;

	    if (vf -> vf_parameter) {
		if (rdparam)
		    pe_free (rdparam), rdparam = NULLPE;

		if (enc_f (vf -> vf_number, &_ZDOCS_mod, &rdparam, 1, 0,
			   NULLCP, vf -> vf_parameter) == NOTOK) {
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "unable to build parameter: %s", PY_pepy);
		    if (rdparam)
			pe_free (rdparam), rdparam = NULLPE;
		}
		fa -> fa_parameter = rdparam;
	    }
	    else
		fa -> fa_parameter = NULLPE;
	}
	else
	    fa -> fa_present &= ~FA_CONTENTS;
    }

#ifdef	BRIDGE
/* these values cannot be obtained from FTP */
    if (attrnames & FA_ACCOUNT)
#else
    if ((attrnames & FA_ACCOUNT)
	    && (fa -> fa_account = getgroup (st -> st_gid)) == NULL)
#endif
	fa -> fa_present &= ~FA_ACCOUNT;

    if (attrnames & FA_DATE_CREATE)
#ifndef	BRIDGE
	if (tm = gmtime ((long *) &st -> st_mtime))
	    tm2ut (tm, &fa -> fa_date_create);
	else
#endif
	    fa -> fa_novalue |= FA_DATE_CREATE;

    if (attrnames & FA_DATE_MODIFY)
#ifndef	BRIDGE
	if (tm = gmtime ((long *) &st -> st_mtime))
	    tm2ut (tm, &fa -> fa_date_modify);
	else
#endif
	    fa -> fa_novalue |= FA_DATE_MODIFY;

    if (attrnames & FA_DATE_READ)
#ifndef	BRIDGE
	if (tm = gmtime ((long *) &st -> st_atime))
	    tm2ut (tm, &fa -> fa_date_read);
	else
#endif
	    fa -> fa_novalue |= FA_DATE_READ;

    if (attrnames & FA_DATE_ATTR)
#ifndef	BRIDGE
	if (tm = gmtime ((long *) &st -> st_ctime))
	    tm2ut (tm, &fa -> fa_date_attribute);
	else
#endif
	    fa -> fa_novalue |= FA_DATE_ATTR;

#ifdef	BRIDGE
    if (attrnames & FA_ID_CREATE)
#else
    if ((attrnames & FA_ID_CREATE)
	    && (fa -> fa_id_create = getuser (st -> st_uid)) == NULL)
#endif
	fa -> fa_novalue |= FA_ID_CREATE;

#ifdef	BRIDGE
    if (attrnames & FA_ID_MODIFY)
#else
    if ((attrnames & FA_ID_MODIFY)
	    && ((st -> st_mode & 0022)
		    || (fa -> fa_id_modify = getuser (st -> st_uid)) == NULL))
#endif
	fa -> fa_novalue |= FA_ID_MODIFY;

#ifdef	BRIDGE
    if (attrnames & FA_ID_READ)
#else
    if ((attrnames & FA_ID_READ)
	    && ((st -> st_mode & 0044)
		    || (fa -> fa_id_read = getuser (st -> st_uid)) == NULL))
#endif
	fa -> fa_novalue |= FA_ID_READ;

#ifdef	BRIDGE
    if (attrnames & FA_ID_ATTR)
#else
    if ((attrnames & FA_ID_ATTR)
	    && ( (st -> st_mode & 0022)
		    || (fa -> fa_id_attribute = getuser (st -> st_uid))
				== NULL))
#endif
	fa -> fa_novalue |= FA_ID_ATTR;

    if (attrnames & FA_AVAILABILITY)
	fa -> fa_availability = FA_AVAIL_IMMED;

    if (attrnames & FA_FILESIZE)
#ifdef	BRIDGE
	fa -> fa_novalue |= FA_FILESIZE;
#else
	fa -> fa_filesize = (int) st -> st_size;
#endif

    if (attrnames & FA_FUTURESIZE)
	fa -> fa_novalue |= FA_FUTURESIZE;

    *diags = dp;
    return OK;
}

/*  */

static int  chngattrs (present, fa, diags)
long	present;
register struct FTAMattributes *fa;
register struct FTAMdiagnostic **diags;
{
#ifndef	BRIDGE
    int     gid,
            result;
#endif
    register struct FTAMdiagnostic *dp = *diags;
#ifndef	BRIDGE
    struct  stat    n1;
#endif

#ifdef	BRIDGE
    statok = 1;
#else
    if ((myfd != NOTOK ? fstat (myfd, &myst) : stat (myfile, &myst))
	    == NOTOK) {
bad_system: ;
	(void) strcpy (dp -> ftd_data, sys_errname (errno));
	dp -> ftd_cc = strlen (dp -> ftd_data);
	goto no_change;
    }
    statok = 1;
    if (myuid != myst.st_uid && myuid != 0) {
	errno = EPERM;
	goto bad_system;
    }
#endif

#ifndef	BRIDGE
    if ((present & FA_ACCOUNT) && strlen (fa -> fa_account) > 1)
	if ((gid = findgid (fa -> fa_account)) != NOTOK) {
#ifndef	SYS5
	    (void) seteuid (0);
	    result = myfd != NOTOK ? fchown (myfd, -1, gid)
		    : chown (myfile, -1, gid);
	    (void) seteuid (myuid);
#else
	    result = chgrp (myfile, gid);
#endif

	    if (result == NOTOK) {
	    	(void) sprintf (dp -> ftd_data, "%s: %s", fa -> fa_account,
				sys_errname (errno));
	    	dp -> ftd_cc = strlen (dp -> ftd_data);

    	no_change: ;
	    	dp -> ftd_type = DIAG_PERM;
	    	dp -> ftd_identifier = FS_MGT_CHANGE;
	    	dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	    	dp -> ftd_delay = DIAG_NODELAY;
	    	dp++;

	    	*diags = dp;
	    	return NOTOK;
	    }

	    myst.st_gid = gid;
        }
#endif

    if (present & FA_FILENAME) {
#ifdef	BRIDGE
	if (ftp_rename (myfile, mvfile) == NOTOK) {
	    (void) sprintf (dp -> ftd_data, "%s: %s", fa -> fa_files[0],
			    ftp_error);
#else
	if (stat (mvfile, &n1) == 0) {  /*file exists*/
	    advise (LLOG_NOTICE, NULLCP, "file %s already exists",mvfile);
	    (void) sprintf (dp->ftd_data, "File \"%s\" already exists",
			    fa -> fa_files[0]);
	    dp -> ftd_cc = strlen (dp -> ftd_data);
	    dp -> ftd_type = DIAG_PERM;
	    dp -> ftd_identifier = FS_MGT_CHANGE;
	    dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	    dp -> ftd_delay = DIAG_NODELAY;
	    dp++;

	    *diags = dp;
	    return NOTOK;
	} else if (rename (myfile, mvfile) == NOTOK) {	/* on EXDEV could do gonzo
						   copy, but why bother? */
	    (void) sprintf (dp -> ftd_data, "%s: %s", fa -> fa_files[0],
		    sys_errname (errno));
#endif
	    dp -> ftd_cc = strlen (dp -> ftd_data);
#ifndef	BRIDGE
	    goto no_change;
#else
	    dp -> ftd_type = DIAG_PERM;
	    dp -> ftd_identifier = FS_MGT_CHANGE;
	    dp -> ftd_observer = dp -> ftd_source = EREF_RFSU;
	    dp -> ftd_delay = DIAG_NODELAY;
	    dp++;

	    *diags = dp;
	    return NOTOK;
#endif
	}
	advise (LLOG_NOTICE, NULLCP, "rename %s to %s", myfile, mvfile);

	myfile = mvfile;
    }

    *diags = dp;
    return OK;
}

/*  */

static char *getfile (file)
char   *file;
{
    register char  *bp;
#ifndef	BRIDGE
    register char  *cp,
                   *pp;
    register struct passwd *pw;
#endif
    static int  i = 0;
    static char buffer1[MAXPATHLEN],
                buffer2[MAXPATHLEN];

    bp = (i++ % 2) ? buffer1 : buffer2;

#ifndef	BRIDGE
    switch (*file) {
	case '/': 
	    if (strlen (file) >= MAXPATHLEN)
		goto trunc;

	    (void) strcpy (bp, file);
	    break;

	case '~': 
	    if (cp = index (pp = file + 1, '/'))
		*cp = NULL;

	    if (*pp == NULL)
		pp = myhome;
	    else {
		if ((pw = getpwnam (pp)) == NULL)
		    return NULL;
		else
		    pp = pw -> pw_dir;
	    }

	    if (strlen (pp) + 1 + (cp ? strlen (cp) : 0) >= MAXPATHLEN)
		goto trunc;

	    (void) sprintf (bp, "%s/%s", pp, cp ? cp + 1 : "");
	    if (cp)
		*cp = '/';
	    break;

	default: 
	    if (strlen (file) + myhomelen + 1 >= MAXPATHLEN)
		goto trunc;

	    (void) sprintf (bp, "%s/%s", myhome, file);
	    break;
    }

    compath (bp);

#ifndef	apollo		/* always return RELATIVE pathnames */
    if (strncmp (bp, myhome, myhomelen - 1) == 0)
	switch (bp[myhomelen - 1]) {
	    case NULL: 
		(void) strcpy (bp, ".");
		break;

	    case '/': 
		bp += myhomelen;
		break;

	    default: 
		break;
	}
#endif

    return bp;

trunc: ;
    errno = 0;
    return NULLCP;
#else
	(void) strcpy (bp, file);
	return bp;
#endif
}

/*  */

#ifndef	BRIDGE
/* originally used algorithms similar to those in /bin/ls; Don Preuss of
   Apollo suggested these algorithms as they work better with distributed
   /etc/passwd and /etc/group files */

static char *getuser (uid)
int	uid;
{
    static struct passwd *pw = NULL;

    if (pw == NULL || pw -> pw_uid != uid)
	pw = getpwuid (uid);
    return (pw ? pw -> pw_name : NULL);
}

/*  */

static char *getgroup (gid)
int	gid;
{
    register struct group *gr;
    static int	my_gid = -1;
    static char my_name[NMAX + 1];

    if (my_gid != gid) {
	if ((gr = getgrgid (gid)) == NULL)
	    return NULL;

	my_gid = gr -> gr_gid;
	(void) strcpy (my_name, gr -> gr_name);
    }

    return my_name;
}

/*  */

int	findgid (group)
char   *group;
{
    int	    i;
#ifdef	BSD42
    int	    gidset[NGROUPS];
#endif
    register struct group *gr;
    static int my_gid = -1;
    static char my_name[NMAX + 1] = "";

    if (*group == NULL)
	return NOTOK;

    if (strcmp (my_name, group) != 0) {
	if ((gr = getgrnam (group)) == NULL)
	    return NOTOK;

#ifdef	BSD42
	for (i = getgroups (NGROUPS, gidset) - 1; i >= 0; i--)
	    if (gr -> gr_gid == gidset[i])
		break;
	if (i < 0)
	    return NOTOK;
#endif

	(void) strcpy (my_name, gr -> gr_name);
	my_gid = gr -> gr_gid;
    }

    return my_gid;
}
#endif

/*  */

#ifndef	SYS5
#ifndef	BRIDGE
static int  EACCESS (file, mode)
char   *file;
int	mode;
{
    int	    result;

    (void) seteuid (0);
    (void) setruid (myuid);

    result = access (file, mode);

    (void) setruid (0);
    (void) seteuid (myuid);

    return result;
}
#endif
#else

/*  */

static int  chgrp (file, gid)
char   *file;
int	gid;
{
    int     i,
	    pid,
	    status;
    char    group[10];
    struct stat st;

    (void) sprintf (group, "%d", gid);

    switch (pid = fork ()) {
	case NOTOK: 
	    return NOTOK;

	case OK: 
	    execl ("/bin/chgrp", "chgrp", group, file, NULLCP);
	    execl ("/usr/bin/chgrp", "chgrp", group, file, NULLCP);
	    execl ("/etc/chgrp", "chgrp", group, file, NULLCP);
	    _exit (NOTOK);

	default: 
	    while ((i = wait (&status)) != NOTOK && pid != i)
		continue;
	    if (i != NOTOK && status) {
		if (stat (file, &st) == NOTOK || st.st_gid != gid) {
		    i = NOTOK;
		    errno = EACCES;
		}
		else
		    status = OK;
	    }
	    return (i == NOTOK ? NOTOK : status);
    }
}

/*  */

static int  mkdir (dir, mode)
char   *dir;
int	mode;
{
    int     i,
	    pid,
	    status;
    struct stat st;

    switch (pid = fork ()) {
	case NOTOK: 
	    return NOTOK;

	case OK: 
	    (void) umask (~mode);
	    execl ("/bin/mkdir", "mkdir", dir, NULLCP);
	    execl ("/usr/bin/mkdir", "mkdir", dir, NULLCP);
	    execl ("/etc/mkdir", "mkdir", dir, NULLCP);
	    _exit (NOTOK);

	default: 
	    while ((i = wait (&status)) != NOTOK && pid != i)
		continue;
	    if (i != NOTOK && status) {
		if (stat (dir, &st) == NOTOK
		        || (st.st_mode & S_IFMT) != S_IFDIR) {
		    i = NOTOK;
		    errno = EACCES;
		}
		else
		    status = OK;
	    }
	    return (i == NOTOK ? NOTOK : status);
    }
}

/*  */

static int  rmdir (dir)
char   *dir;
{
    int     i,
	    pid,
	    status;

    switch (pid = fork ()) {
	case NOTOK: 
	    return NOTOK;

	case OK: 
	    execl ("/bin/rmdir", "rmdir", dir, NULLCP);
	    execl ("/usr/bin/rmdir", "rmdir", dir, NULLCP);
	    execl ("/etc/rmdir", "rmdir", dir, NULLCP);
	    _exit (NOTOK);

	default: 
	    while ((i = wait (&status)) != NOTOK && pid != i)
		continue;
	    if (i != NOTOK && status) {
		if (access (dir, 0x00) != NOTOK) {
		    i = NOTOK;
		    errno = EACCES;
		}
		else
		    status = OK;
	    }
	    return (i == NOTOK ? NOTOK : status);
    }
}

/*  */

static int  truncate (file, length)
char   *file;
int	length;
{
    int	    fd;

    if (length != 0) {		/* XXX: too much work to get right */
	errno = EINVAL;
	return NOTOK;
    }

    if ((fd = open (file, O_WRONLY | O_TRUNC)) == NOTOK)
	return NOTOK;

    (void) close (fd);
    return OK;
}


/* ARGSUSED */

int	ftruncate (fd, length)	/* works only 'cause we're lucky */
int	fd,
	length;
{
    return truncate (myfile, length);
}
#endif

/*    DEBUG */

#if	defined(FTAMDEBUG) && defined(BSD42)
#include <syscall.h>


static int  unlink (file)
char   *file;
{
    if (debug) {
	int     i,
	        b;

again: 	;
	fprintf (stderr, "unlink(\"%s\")? y, w, l: ", file);

	i = b = getchar ();
	while (b != '\n' && b != EOF)
	    b = getchar ();

	switch (i) {
	    case 'y': 
		break;

	    case 'w': 
		return OK;

	    case 'l': 
		return NOTOK;

	    default: 
		goto again;
	}
    }

    return syscall (SYS_unlink, file);
}


static int  rmdir (dir)
char   *dir;
{
    if (debug) {
	int     i,
	        b;

again: 	;
	fprintf (stderr, "rmdir(\"%s\")? y, w, l: ", dir);

	i = b = getchar ();
	while (b != '\n' && b != EOF)
	    b = getchar ();

	switch (i) {
	    case 'y': 
		break;

	    case 'w': 
		return OK;

	    case 'l': 
		return NOTOK;

	    default: 
		goto again;
	}
    }

    return syscall (SYS_rmdir, dir);
}


/* VARARGS2 */

static int  open (file, flags, mode)
char   *file;
int     flags,
        mode;
{
    if (debug) {
	int     i,
	        b;

again: 	;
	fprintf (stderr, "open(\"%s\",0x%x,0%o)? y, l: ", file, flags,
			(flags & O_CREAT) ? mode : 0);

	i = b = getchar ();
	while (b != '\n' && b != EOF)
	    b = getchar ();

	switch (i) {
	    case 'y': 
		break;

	    case 'l': 
		return NOTOK;

	    default: 
		goto again;
	}
    }

    return syscall (SYS_open, file, flags, mode);
}


static int  mkdir (dir, mode)
char   *dir;
int     mode;
{
    if (debug) {
	int     i,
	        b;

again: 	;
	fprintf (stderr, "mkdir(\"%s\",0%o)? y, w, l: ", dir, mode);

	i = b = getchar ();
	while (b != '\n' && b != EOF)
	    b = getchar ();

	switch (i) {
	    case 'y': 
		break;

	    case 'w': 
		return OK;

	    case 'l': 
		return NOTOK;

	    default: 
		goto again;
	}
    }

    return syscall (SYS_mkdir, dir, mode);
}


static int  chown (file, uid, gid)
char   *file;
int     uid,
        gid;
{
    if (debug) {
	int     i,
	        b;

again: 	;
	fprintf (stderr, "chown(\"%s\",%d,%d)? y, w, l: ", file, uid, gid);

	i = b = getchar ();
	while (b != '\n' && b != EOF)
	    b = getchar ();

	switch (i) {
	    case 'y': 
		break;

	    case 'w': 
		return OK;

	    case 'l': 
		return NOTOK;

	    default: 
		goto again;
	}
    }

    return syscall (SYS_chown, file, uid, gid);
}


static int  fchown (fd, uid, gid)
int     fd;
int     uid,
        gid;
{
    if (debug) {
	int     i,
	        b;

again: 	;
	fprintf (stderr, "fchown(%d,%d,%d)? y, w, l: ", fd, uid, gid);

	i = b = getchar ();
	while (b != '\n' && b != EOF)
	    b = getchar ();

	switch (i) {
	    case 'y': 
		break;

	    case 'w': 
		return OK;

	    case 'l': 
		return NOTOK;

	    default: 
		goto again;
	}
    }

    return syscall (SYS_fchown, fd, uid, gid);
}


static int  truncate (file, length)
char   *file;
int     length;
{
    if (debug) {
	int     i,
	        b;

again: 	;
	fprintf (stderr, "truncate(\"%s\",%d)? y, w, l: ", file, length);

	i = b = getchar ();
	while (b != '\n' && b != EOF)
	    b = getchar ();

	switch (i) {
	    case 'y': 
		break;

	    case 'w': 
		return OK;

	    case 'l': 
		return NOTOK;

	    default: 
		goto again;
	}
    }

    return syscall (SYS_truncate, file, length);
}


static int  rename (old, new)
char   *old;
char   *new;
{
    if (debug) {
	int     i,
	        b;

again: 	;
	fprintf (stderr, "rename(\"%s\",\"%s\")? y, w, l: ", old, new);

	i = b = getchar ();
	while (b != '\n' && b != EOF)
	    b = getchar ();

	switch (i) {
	    case 'y': 
		break;

	    case 'w': 
		return OK;

	    case 'l': 
		return NOTOK;

	    default: 
		goto again;
	}
    }

    return syscall (SYS_rename, old, new);
}


static int  flock (fd, operation)
int	fd,
	operation;
{
    if (debug) {
	int     i,
	        b;

again: 	;
	fprintf (stderr, "flock(%d,0x%x)? y, w, l: ", fd, operation);

	i = b = getchar ();
	while (b != '\n' && b != EOF)
	    b = getchar ();

	switch (i) {
	    case 'y': 
		break;

	    case 'w': 
		return OK;

	    case 'l': 
		return NOTOK;

	    default: 
		goto again;
	}
    }

    return syscall (SYS_flock, fd, operation);
}
#endif
