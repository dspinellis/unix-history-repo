/* ftamsystem.c - FTAM responder routines */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftamsystem.c,v 7.7 91/02/22 09:24:08 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftamsystem.c,v 7.7 91/02/22 09:24:08 mrose Interim $
 *
 *
 * $Log:	ftamsystem.c,v $
 * Revision 7.7  91/02/22  09:24:08  mrose
 * Interim 6.8
 * 
 * Revision 7.6  91/01/14  13:32:45  mrose
 * kerberos
 * 
 * Revision 7.5  91/01/13  12:27:07  mrose
 * NBS
 * 
 * Revision 7.4  90/11/21  11:30:52  mrose
 * sun
 * 
 * Revision 7.3  90/11/05  13:29:57  mrose
 * nist
 * 
 * Revision 7.2  90/07/01  21:03:35  mrose
 * pepsy
 * 
 * Revision 7.1  90/01/16  22:37:20  mrose
 * very last time
 * 
 * Revision 7.0  89/11/23  21:54:40  mrose
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


#if	defined(DEBUG) && !defined(NULL_INITIATOR)
#define	NULL_INITIATOR
#endif

#include <ctype.h>
#include <stdio.h>
#include <grp.h>
#include <pwd.h>
#include "ftamsystem.h"
#ifdef	NULL_INITIATOR
#include "tailor.h"
#else
#include "logger.h"
#endif
#include <utmp.h>


#ifdef	SYS5
struct group  *getgrnam ();
struct passwd *getpwnam ();
#endif

#ifndef	ANON
#define	ANON	"ftp"
#endif

/*    UNIX DATA */

int   myuid;

int   myhomelen;
char  myhome[MAXPATHLEN];

dev_t	null_dev;
ino_t	null_ino;


static int   wtmp = NOTOK;

static long  clok;


struct utmp  uts;


long	lseek (), time ();

/*    VFS DATA */

struct vfsmap vfs[] = {
/* VFS_UBF */
    "FTAM-3", NULLOID, NULLCP, VF_WARN, 0, S_IFREG, binarypeek, 'b', VFS_XXX,
	FA_ACC_UA,
	-1, binarycheck,
	   _ZFTAM_3_ParametersDOCS,
	"unstructured binary file",

/* VFS_UTF */
    "FTAM-1", NULLOID, NULLCP, VF_WARN, 0, S_IFREG, textpeek, 't', VFS_UBF,
	FA_ACC_UA,
	-1, textcheck,
	   _ZFTAM_1_ParametersDOCS,
	"unstructured text file",

/* VFS_FDF */
    "NBS-9",  NULLOID, NULLCP, VF_NULL, 0, S_IFDIR, fdfpeek, 'd', VFS_XXX,
	FA_ACC_UA,
	1, NULLIFP,
	   _ZNBS_9_ParametersDOCS,
	"file directory file",

    NULL
};
#ifdef	BRIDGE
int	vfs_fdf = VFS_FDF;
#endif

/*    REGIME DATA */

int     fqos;
int     class;
int     units = FUNIT_READ | FUNIT_WRITE | FUNIT_LIMITED | FUNIT_ENHANCED
			| FUNIT_GROUPING;
int     attrs = FATTR_STORAGE;

int     fadusize = 0;

/*    ACTIVITY DATA */

int     myfd = NOTOK;		/* handle to file */
char   *myfile;
struct stat myst;
int     statok;

struct vfsmap   *myvf;		/* active contents type */
caddr_t myparam;		/*   .. */

int  myaccess;			/* current access request */

char *initiator;		/* current initiator identity */
#ifdef	NULL_INITIATOR
static int null_initiator = 0;	/*   none given, do EurOSInet style */
#endif

struct FADUidentity  mylocation;/* current location */

int  mymode;			/* current processing mode */
int  myoperation;		/*   .. */

#ifdef	notdef
AEI	mycalling;		/* current calling AET */
AEI	myresponding;		/* current responding AET */
#endif

char *account;			/* current account */
int  mygid;			/* "inner" account */

int  mylock;			/* current concurrency control */
struct FTAMconcurrency myconctl;/*   .. */

int  mylockstyle;		/* current locking style */


int  mycontext;			/* current access context */
int  mylevel;			/*   .. */

#ifdef	BRIDGE
static char *RemoteHost;
static char *password;

int	ftp_default = VFS_UBF;
#endif

/*    REGIME */

#ifdef	BRIDGE
#define	seterr(id,ob,so,des) \
{ \
    dp -> ftd_identifier = (id); \
    dp -> ftd_observer = (ob), dp -> ftd_source = (so); \
    (void) strncpy (dp -> ftd_data, des, FTD_SIZE);\
    dp -> ftd_cc = strlen(dp -> ftd_data);\
    goto bad2; \
}
#else
#define	seterr(id,ob,so,des) \
{ \
    dp -> ftd_identifier = (id); \
    dp -> ftd_observer = (ob), dp -> ftd_source = (so); \
    goto bad2; \
}
#endif


int	ftam_start (fts)
register struct FTAMstart *fts;
{
    register int    i;
#ifndef	BRIDGE
    int	    guest;
    struct passwd *pw;
#endif
    struct stat st;
    register struct isodocument *id;
    register struct vfsmap *vf;
    register struct FTAMcontent *fx;
    struct FTAMdiagnostic   diags[NFDIAG];
    register struct FTAMdiagnostic *dp = diags;
    struct FTAMindication   ftis;
    struct FTAMindication *fti = &ftis;

    (void) time (&clok);

    if (stat ("/dev/null", &st) != NOTOK)
	null_dev = st.st_dev, null_ino = st.st_ino;
    else
	null_dev = (dev_t) 0, null_ino = (ino_t) 0;

    for (vf = vfs; vf -> vf_entry; vf++)
	if (id = getisodocumentbyentry (vf -> vf_entry)) {
	    if ((vf -> vf_oid = oid_cpy (id -> id_type)) == NULLOID)
		adios (NULLCP, "%s: out of memory", vf -> vf_entry);
	}
	else
	    if (vf -> vf_flags & VF_WARN)
		advise (LLOG_NOTICE, NULLCP, "%s: unknown", vf -> vf_entry);

    ftamfd = fts -> fts_sd;
    if ((class = fts -> fts_class) & FCLASS_TM)
	class = FCLASS_TM;
    else
	if (class & FCLASS_TRANSFER)
	    class = FCLASS_TRANSFER;
	else
	    if (class & FCLASS_MANAGE)
		class = FCLASS_MANAGE;
	    else
		seterr (FS_ACS_CLASS, EREF_RFSU, EREF_IFSU, "");
    units &= fts -> fts_units;
    attrs &= fts -> fts_attrs;
    if ((fqos = fts -> fts_fqos) != FQOS_NORECOVERY)
	seterr (FS_ACS_ROLLBACK, EREF_RFPM, EREF_IFSU, "");
    if ((fadusize = fts -> fts_ssdusize) < 0)
	fadusize = 0;

    for (fx = fts -> fts_contents.fc_contents,
		i = fts -> fts_contents.fc_ncontent - 1;
	    i >= 0;
	    fx++, i--) {
	if (fx -> fc_result != PC_ACCEPT)
	    continue;

	for (vf = vfs; vf -> vf_entry; vf++)
	    if (vf -> vf_oid
		    && oid_cmp (vf -> vf_oid, fx -> fc_dtn) == 0) {
		vf -> vf_flags |= VF_OK;
		vf -> vf_id = fx -> fc_id;
		break;
	    }
	if (!vf -> vf_entry) {
	    advise (LLOG_NOTICE, NULLCP, "%s: unknown document-type",
		    oid2ode (fx -> fc_dtn));
	    fx -> fc_result = PC_REJECTED;
	}
    }

    if ((initiator = fts -> fts_initiator) == NULL) {
#ifdef	NULL_INITIATOR
	initiator = ANON;
	null_initiator = 1;
#else
	seterr (FS_ACS_IDENTITY, EREF_RFSU, EREF_IFSU, "");
#endif
    }
    fts -> fts_initiator = NULL;

#ifdef	BRIDGE
/* scan initiator for remote host */
    if ((RemoteHost = rindex(initiator, '@')) == NULL) {
	advise (LLOG_EXCEPTIONS, NULLCP, "missing remote host name in \"%s\"",
		initiator);
	seterr (FS_ACS_IDENTITY, EREF_RFSU, EREF_IFSU,
		"missing remote hostname");
    }
    *RemoteHost++ = '\0';
    if (strcmp (initiator, "ANON") == 0 || strcmp (initiator, ANON) == 0) {
	initiator = "ANONYMOUS"; /* FTP guest name */
    }
    password = (fts -> fts_password == NULL) ? "guest" : fts -> fts_password;
    account = fts -> fts_account;
    advise (LLOG_NOTICE, NULLCP,
	    "attemping connection with TCP host \"%s\" for user \"%s\"",
	    RemoteHost, initiator);
    if (ftp_login(RemoteHost, initiator, password, account) == NOTOK)
	seterr (FS_ACS_IDENTITY, EREF_RFSU, EREF_IFSU, ftp_error);
    (void) strcpy (myhome, "");
    myhomelen = strlen (myhome);
#else
    guest = 0;
#ifdef	NULL_INITIATOR
    if (!baduser (NULLCP, initiator) && baduser ("ftamguests", initiator)) {
	initiator = ANON;
	null_initiator = 1;
    }
#endif
    if (strcmp (initiator, "ANON") == 0 || strcmp (initiator, ANON) == 0) {
	if ((pw = getpwnam (ANON)) && pw -> pw_uid == 0)
	    pw = NULL;
	guest = 1;
    }
    else
	pw = baduser ("ftamusers", initiator) ? NULL : getpwnam (initiator);
    if (pw == NULL)
	seterr (FS_ACS_USER, EREF_RFSU, EREF_IFSU, "");
    if ((!guest && fts -> fts_password == NULL)
	    || *pw -> pw_passwd == NULL
	    || (!guest && !chkpassword (initiator, pw -> pw_passwd,
					fts -> fts_password)))
	seterr (FS_ACS_PASSWORD, EREF_RFSU, EREF_IFSU, "");

#ifdef DEBUG
    advise (LLOG_DEBUG, NULLCP,
	    "initiator=%s, account=%s", initiator, fts -> fts_account);
#endif
    if ((account = fts -> fts_account) && (strlen(fts -> fts_account) > 1)) {
	register struct group *gr = getgrnam (account);
	register char **gp;

	if (gr == NULL) {
bad_account: ;
	    seterr (FS_ACS_ACCT, EREF_RFPM, EREF_IFSU, "");
	}
	if (gr -> gr_gid != pw -> pw_gid) {
	    for (gp = gr -> gr_mem; *gp; gp++)
		if (strcmp (*gp, initiator) == 0)
		    break;
	    if (!*gp)
		goto bad_account;
	}

	fts -> fts_account = NULL;
    }

    if (chdir (pw -> pw_dir) == NOTOK) {
	dp -> ftd_type = DIAG_PERM;
	dp -> ftd_identifier = FS_ACS_MGMT;
	dp -> ftd_observer = EREF_RFPM, dp -> ftd_source = EREF_IFSU;
	dp -> ftd_delay = DIAG_NODELAY;
	(void) sprintf (dp -> ftd_data, "unable to change to %s: %s",
		pw -> pw_dir, sys_errname (errno));
	dp -> ftd_cc = strlen (dp -> ftd_data);
	dp++;
    }
#endif

    if ((wtmp = open ("/usr/adm/wtmp", O_WRONLY | O_APPEND)) != NOTOK) {
	char    line[32];

	(void) sprintf (line, "ftam%d", getpid ());
	(void) SCPYN (uts.ut_line, line);
#ifndef	BRIDGE
	(void) SCPYN (uts.ut_name, pw -> pw_name);
#else
	(void) SCPYN (uts.ut_name, initiator);
#endif
#if	!defined(SYS5) && !defined(bsd43_ut_host)
	(void) SCPYN (uts.ut_host,
		na2str (fts -> fts_callingaddr.pa_addr.sa_addr.ta_addrs));
#else
	uts.ut_type = USER_PROCESS;
#endif
	uts.ut_time = clok;
	(void) write (wtmp, (char *) &uts, sizeof uts);
#if	defined(SYS5) || defined(bsd43_ut_host)
	(void) close (wtmp);
#endif
    }

#ifndef	BRIDGE
    if (cflag || guest) {
	(void) setisobject (1);		/* for PDU pretty-printing
					   AND for A-ASSOCIATE.RESPONSE!!! */
	if (chroot (pw -> pw_dir) == NOTOK) {
	    if (!debug)
		dp = diags;
	    dp -> ftd_type = DIAG_PERM;
	    if (debug) {
		dp -> ftd_identifier = FS_ACS_MGMT;
		dp -> ftd_observer = EREF_RFPM, dp -> ftd_source = EREF_IFSU;
	    }
	    else {
		dp -> ftd_identifier = FS_ACS_USER;
		dp -> ftd_observer = EREF_RFSU, dp -> ftd_source = EREF_IFSU;
	    }
	    dp -> ftd_delay = DIAG_NODELAY;
	    (void) sprintf (dp -> ftd_data, "unable to change root to %s: %s",
		    pw -> pw_dir, sys_errname (errno));
	    dp -> ftd_cc = strlen (dp -> ftd_data);
	    if (debug)
		dp++;
	    else
		goto bad1;
	}
#ifdef	NULL_INITIATOR
	else
	    if (null_initiator) {
		if (chdir (pw -> pw_dir = "/pub") == NOTOK) {
		    dp -> ftd_type = DIAG_PERM;
		    dp -> ftd_identifier = FS_ACS_MGMT;
		    dp -> ftd_observer = EREF_RFPM, dp -> ftd_source = EREF_IFSU;
		    dp -> ftd_delay = DIAG_NODELAY;
		    (void) sprintf (dp -> ftd_data,
				    "unable to change to %s: %s",
				    pw -> pw_dir, sys_errname (errno));
		    dp -> ftd_cc = strlen (dp -> ftd_data);
		    dp++;
		}
	    }
#endif
	else {
	    dp -> ftd_type = DIAG_INFORM;
	    dp -> ftd_identifier = FS_GEN_NOREASON;
	    dp -> ftd_observer = EREF_RFSU, dp -> ftd_source = EREF_RFSU;
	    dp -> ftd_delay = DIAG_NODELAY;
	    if (guest)
		(void) strcpy (dp -> ftd_data,
			"ANONymous user permitted, access restrictions apply");
	    dp -> ftd_cc = strlen (dp -> ftd_data);
	    dp++;

	    pw -> pw_dir = "/";
	}
    }

    (void) sprintf (myhome, "%s/", pw -> pw_dir);
    myhomelen = strlen (myhome);

    (void) setgid (pw -> pw_gid);
#ifndef	SYS5
    (void) initgroups (pw -> pw_name, pw -> pw_gid);
    (void) seteuid (myuid = pw -> pw_uid);
#else
    (void) setuid (myuid = pw -> pw_uid);
#endif

    (void) umask (0022);
#endif

    if (FInitializeResponse (ftamfd, FSTATE_SUCCESS, FACTION_SUCCESS,
			     NULLOID, NULLAEI, NULLPA, fts -> fts_manage,
			     class, units, attrs, NULLPE,
			     fqos, &fts -> fts_contents, diags, dp - diags,
			     fti) == NOTOK)
	ftam_adios (&fti -> fti_abort, "F-INITIALIZE.RESPONSE");

    advise (LLOG_NOTICE, NULLCP, "accepting association");
    ftam_diag (diags, dp - diags);
    return;

bad2: ;
    dp -> ftd_type = DIAG_PERM;
    dp -> ftd_delay = DIAG_NODELAY;
#ifndef	BRIDGE
    dp -> ftd_cc = 0;

bad1: ;
#endif
    advise (LLOG_NOTICE, NULLCP, "rejecting association");
    ftam_diag (diags, 1);

    if (FInitializeResponse (ftamfd, FSTATE_FAILURE, FACTION_PERM, NULLOID,
	    NULLAEI, NULLPA, fts -> fts_manage, class, units, 0, NULLPE,
	    fqos, (struct FTAMcontentlist *) 0, diags, 1, fti) == NOTOK)
	ftam_adios (&fti -> fti_abort, "F-INITIALIZE.RESPONSE(reject)");

    closewtmp ();

    exit (1);
}

/*  */

int	ftam_indication (fti)
register struct FTAMindication *fti;
{
    switch (fti -> fti_type) {
	case FTI_FINISH: 
	    ftam_finishindication (&fti -> fti_finish);
	    break;

	case FTI_ABORT: 
	    ftam_abortindication (&fti -> fti_abort);
	    break;

	case FTI_BULKBEGIN:
	    ftam_bulkbeginindication (&fti -> fti_group);
	    break;

	case FTI_READWRITE:
	    ftam_readwriteindication (&fti -> fti_readwrite);
	    break;
	
	case FTI_DATA:
	    ftam_dataindication (&fti -> fti_data);
	    break;

	case FTI_DATAEND:
	    ftam_dataendindication (&fti -> fti_dataend);
	    break;

	case FTI_CANCEL:
	    ftam_cancelindication (&fti -> fti_cancel);
	    break;

	case FTI_TRANSEND:
	    ftam_transendindication (&fti -> fti_transend);
	    break;

	case FTI_BULKEND:
	    ftam_bulkendindication (&fti -> fti_group);
	    break;

	case FTI_MANAGEMENT:
	    ftam_managementindication (&fti -> fti_group);
	    break;

	default: 
	    adios (NULLCP, "unknown indication type=%d", fti -> fti_type);
    }
}

/*    TERMINATION */

/* ARGSUSED */

static	ftam_finishindication (ftf)
struct FTAMfinish *ftf;
{
#ifdef	DEBUG
    long    now;
    struct FTAMcharging fcs;
    register struct FTAMcharging   *fc = &fcs;
#else
#define	fc	((struct FTAMcharging *) 0)
#endif
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;
#ifdef	BRIDGE
    (void) ftp_quit ();
#endif

    advise (LLOG_NOTICE, NULLCP, "F-TERMINATE.INDICATION");

#ifdef	DEBUG
    fc -> fc_ncharge = 0;
    if (account) {
	(void) time (&now);

	fc -> fc_charges[fc -> fc_ncharge].fc_resource = "elapsed time";
	fc -> fc_charges[fc -> fc_ncharge].fc_unit = "seconds";
	fc -> fc_charges[fc -> fc_ncharge++].fc_value = (int) (now - clok);
    }
#endif

    if (FTerminateResponse (ftamfd, NULLPE, fc, fti) == NOTOK)
	ftam_adios (&fti -> fti_abort, "F-TERMINATE.RESPONSE");

    FTFFREE (ftf);

    closewtmp ();

    exit (0);
}


closewtmp ()
{
#if	!defined(SYS5) && !defined(bsd43_ut_host)
    long    now;

    (void) time (&now);

    if (wtmp != NOTOK) {
	(void) lseek (wtmp, 0L, L_XTND);
	(void) SCPYN (uts.ut_name, "");
	(void) SCPYN (uts.ut_host, "");
	uts.ut_time = now;
	(void) write (wtmp, (char *) &uts, sizeof uts);
	(void) close (wtmp);
    }
#endif
}

/*    ABORT */

static ftam_abortindication (fta)
register struct FTAMabort *fta;
{
    struct FTAMindication   ftis;

    advise (LLOG_NOTICE, NULLCP, "F-%s-ABORT.INDICATION %d",
	    fta -> fta_peer ? "U" : "P", fta -> fta_action);
    ftam_diag (fta -> fta_diags, fta -> fta_ndiag);
#ifdef	BRIDGE
    (void) ftp_abort ();
    (void) ftp_quit ();
#endif 

    if (fta -> fta_action != FACTION_PERM && !fta -> fta_peer)
	(void) FUAbortRequest (ftamfd, FACTION_PERM,
			       (struct FTAMdiagnostic *) 0, 0, &ftis);

    closewtmp ();

    exit (1);
}
