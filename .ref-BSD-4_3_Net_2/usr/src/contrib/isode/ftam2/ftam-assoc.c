/* ftam-assoc.c - interactive initiator FTAM -- association management */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftam-assoc.c,v 7.7 91/02/22 09:23:37 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftam-assoc.c,v 7.7 91/02/22 09:23:37 mrose Interim $
 *
 *
 * $Log:	ftam-assoc.c,v $
 * Revision 7.7  91/02/22  09:23:37  mrose
 * Interim 6.8
 * 
 * Revision 7.6  91/01/07  12:41:46  mrose
 * update
 * 
 * Revision 7.5  90/12/23  18:39:57  mrose
 * update
 * 
 * Revision 7.4  90/12/11  10:52:53  mrose
 * lock-and-load
 * 
 * Revision 7.3  90/07/09  14:37:10  mrose
 * sync
 * 
 * Revision 7.2  89/12/01  13:56:06  mrose
 * touch-up
 * 
 * Revision 7.1  89/11/30  23:51:50  mrose
 * pa2str
 * 
 * Revision 7.0  89/11/23  21:54:15  mrose
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

static struct PSAPaddr vfs_bound;

/*  */

int	f_open (vec)
char  **vec;
{
    register int    i;
    int     manage;
    char    passwd[BUFSIZ];
#ifndef	BRIDGE
    char    buffer[BUFSIZ],
            prompt[BUFSIZ];
#endif
    AEI	    aei;
    register struct vfsmap *vf;
    register struct PSAPaddr   *pa;
    struct FTAMcontentlist  fcs;
    register struct FTAMcontentlist *fc = &fcs;
    register struct FTAMcontent *fx;
    struct FTAMconnect  ftcs;
    register struct FTAMconnect *ftc = &ftcs;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;

    if (*++vec == NULL) {
#ifdef	BRIDGE
	return NOTOK;
#else
	if (getline ("host: ", buffer) == NOTOK
	        || str2vecX (buffer, vec, 0, NULLIP, NULL, 0) < 1)
	    return OK;
#endif
    }

    if (host)
	free (host);
    host = strdup (*vec);

    if (*++vec == NULL) {
#ifdef	BRIDGE
	return NOTOK;
#else
	if (user == NULL)
	    user = strdup (myuser ? myuser : "");
	(void) sprintf (prompt, "user (%s:%s): ", host, user);
	if (getline (prompt, buffer) == NOTOK)
	    return OK;
	if (str2vec (buffer, vec) < 1)
	    *vec = user, user = NULL;
#endif
    }

    if (user)
	free (user);
    user = strdup (*vec);

    if (*++vec) {
	if (account)
	    free (account);
#ifdef	BRIDGE
	account = (**vec) ? strdup (*vec) : "";
#else
	account = strdup (*vec);
#endif
    }

#ifdef	BRIDGE
    if (!strcmp (user, "anonymous") || !strcmp (user, "ANONYMOUS") ){
	free (user);
	user = strdup ("ANON");
    }
#endif

    if (strcmp (user, "anon") == 0) {
	free (user);
	user = strdup ("ANON");
    }

#ifdef	BRIDGE
    if (strcmp (user, "ANON") && !*++vec)
	return NOTOK;
    else
	(void) strcpy (passwd, *vec ? strdup(*vec) : "");
#else
    if (strcmp (user, "ANON")) {
	(void) sprintf (prompt, "password (%s:%s): ", host, user);
	(void) strcpy (passwd, getpassword (prompt));
    }
    else
	(void) strcpy (passwd, myuser ? myuser : "");
#endif

#ifdef	BRIDGE
    if ((aei = _str2aei (host, "filestore", "iso ftam", 0, NULLCP, NULLCP))
	    == NULLAEI) {
	advise (NULLCP, "unable to resolve service: %s", PY_pepy);
#else
    if (userdn && *userdn) {
#ifdef	DEBUG
	static	int	once_only = 1;

	if (once_only)
	    set_lookup_dase (1), once_only = 0;
#endif
	(void) sprintf (prompt, "DN-password (%s): ", userdn);
	(void) strcpy (buffer, getpassword (prompt));
    }
    else
	buffer[0] = NULL;
	    
    if ((aei = _str2aei (host, storename, "iso ftam", ontty,
			 userdn && *userdn ? userdn : NULLCP,
			 buffer[0] ? buffer : NULLCP))
	    == NULLAEI) {
	advise (NULLCP, "unable to resolve service: %s", PY_pepy);
#endif
	return NOTOK;
    }
    if ((pa = aei2addr (aei)) == NULLPA) {
	advise (NULLCP, "address translation failed");
	return NOTOK;
    }

    manage = 0;
    fqos = FQOS_NORECOVERY;
    class = FCLASS_TM | FCLASS_TRANSFER | FCLASS_MANAGE;
    units |=  FUNIT_READ | FUNIT_WRITE | FUNIT_LIMITED | FUNIT_ENHANCED
		| FUNIT_GROUPING;
    attrs = FATTR_STORAGE;
    fc -> fc_ncontent = 0;
    for (vf = vfs; vf -> vf_entry; vf++)
	if (vf -> vf_oid)
	    fc -> fc_contents[fc -> fc_ncontent++].fc_dtn = vf -> vf_oid;

#ifndef	BRIDGE
    if (verbose) {
	fprintf (stderr, "%s... ", host);
	(void) fflush (stderr);
    }
#endif
    if (FInitializeRequest (NULLOID, NULLAEI, aei, NULLPA, pa, manage, class,
			    units, attrs, NULLPE, fqos, fc,
			    *user ? user : NULLCP, account,
			    passwd[0] ? passwd : NULLCP, strlen (passwd),
			    &myqos, trace ? FTraceHook : NULLIFP, ftc, fti)
	    == NOTOK) {
#ifndef	BRIDGE
	if (verbose)
	    fprintf (stderr, "loses big\n");
#endif
	ftam_advise (&fti -> fti_abort, "F-INITIALIZE.REQUEST");

	return NOTOK;
    }

    switch (ftc -> ftc_state) {
	case FSTATE_SUCCESS: 
#ifndef	BRIDGE
	    if (verbose)
		fprintf (stderr, "connected\n");
#endif
#ifdef	DEBUG
	    if (debug)
		advise (NULLCP,
			"responding AE title: %s, responding PSAP address: %s",
			sprintaei (&ftc -> ftc_respondtitle),
			paddr2str (&ftc -> ftc_respondaddr, NULLNA));
#endif
	    vfs_bound = ftc -> ftc_respondaddr;		/* struct copy */

	    ftamfd = ftc -> ftc_sd;
	    context = ftc -> ftc_context, ftc -> ftc_context = NULLOID;
	    fqos = ftc -> ftc_fqos;
	    class = ftc -> ftc_class;
	    units = ftc -> ftc_units;
	    attrs = ftc -> ftc_attrs;

	    for (fx = ftc -> ftc_contents.fc_contents,
			i = ftc -> ftc_contents.fc_ncontent - 1;
		    i >= 0;
		    fx++, i--) {
		if (fx -> fc_result != PC_ACCEPT)
		    continue;

		for (vf = vfs; vf -> vf_entry; vf++)
		    if (oid_cmp (vf -> vf_oid, fx -> fc_dtn) == 0) {
			vf -> vf_flags |= VF_OK;
			vf -> vf_id = fx -> fc_id;
			break;
		    }
	    }
	    for (vf = vfs; vf -> vf_entry; vf++)
		if (vf -> vf_oid
			&& !(vf -> vf_flags & VF_OK)
			&& (vf -> vf_flags & VF_WARN))
		    advise (NULLCP,
			"warning: virtual filestore has no support for %ss",
			vf -> vf_text);

	    vf = &vfs[VFS_UBF];
	    if (vf -> vf_oid == NULLOID || !(vf -> vf_flags & VF_OK))
		advise (NULLCP,
			"%s files not negotiated, suggest you quit now!",
			vf -> vf_entry);

	    if ((vf = &vfs[tmode]) != &vfs[VFS_DEF]
		    && (vf -> vf_oid == NULLOID
			    || !(vf -> vf_flags & VF_OK))) {
		advise (NULLCP, "negotiation prevents transfer of %ss",
			vf -> vf_text);

		tmode = VFS_DEF;
	    }

	    for (vf = vfs; vf -> vf_entry; vf++)    /* prime the pump */
		if (vf -> vf_peek)
		    (void) (*vf -> vf_peek) (vf, NOTOK, NULLCP,
					     (struct stat *) 0, ftamfd);

	    if ((fadusize = ftc -> ftc_ssdusize) < 0)
		fadusize = 0;
	    break;

	default: 
#ifndef	BRIDGE
	    if (verbose)
		fprintf (stderr, "failed\n");
#endif
	    break;
    }
    ftam_diag (ftc -> ftc_diags, ftc -> ftc_ndiag,
		ftamfd != NOTOK || fti -> fti_abort.fta_peer,
		ftc -> ftc_action);

    FTCFREE (ftc);

    if (ftamfd != NOTOK) {
	vec[0] = "sd";
	vec[1] = NULLCP;

	(void) f_cd (vec);
    }

    return (ftamfd != NOTOK ? OK : NOTOK);
}

/*  */

/* ARGSUSED */

int	f_close (vec)
char  **vec;
{
    struct FTAMrelease  ftrs;
    register struct FTAMrelease *ftr = &ftrs;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;

#ifdef	BRIDGE
    if (ftamfd == NOTOK)
	return OK;
#endif

    if (FTerminateRequest (ftamfd, NULLPE, ftr, fti) == NOTOK) {
	ftam_advise (&fti -> fti_abort, "F-TERMINATE.REQUEST");

	(void) FUAbortRequest (ftamfd, FACTION_PERM,
		    (struct FTAMdiagnostic *) 0, 0, &ftis);
	ftamfd = NOTOK;

	return NOTOK;
    }

    ftam_chrg (&ftr -> ftr_charges);

    FTRFREE (ftr);

    ftamfd = NOTOK;
    if (rcwd) {
	free (rcwd);
	rcwd = NULL;
    }

    return OK;
}

/*  */

int	f_quit (vec)
char  **vec;
{
    if (ftamfd != NOTOK)
	(void) f_close (vec);

    return DONE;
}

/*  */

#define	AMASK	"\020\01STORAGE\02SECURITY\03PRIVATE"


/* ARGSUSED */

#ifndef	BRIDGE
int	f_status (vec)
char  **vec;
{
    int	    hit;
    register struct vfsmap *vf;

    printf ("associated with virtual filestore on \"%s\"\n  at %s\n",
	    host, pa2str (&vfs_bound));
    printf ("  as user \"%s\"", user);
    if (account)
	printf (" using account \"%s\"", account);
    printf ("\n");

    printf ("application-context: %s\nservice class: ", oid2ode (context));
    switch (class) {
	case FCLASS_TRANSFER: 
	    printf ("transfer");
	    break;

	case FCLASS_ACCESS: 
	    printf ("access");
	    break;

	case FCLASS_MANAGE: 
	    printf ("management");
	    break;

	case FCLASS_TM: 
	    printf ("transfer-and-management");
	    break;

	case FCLASS_UNCONS: 
	    printf ("unconstrained");
	    break;

	default: 
	    printf ("%d", class);
    }
    printf (", ftam-QoS: ");
    switch (fqos) {
	case FQOS_NORECOVERY: 
	    printf ("no-recovery");
	    break;

	default: 
	    printf ("class-%d-recovery", fqos);
	    break;
    }

    printf ("\nfunctional units: %s\n", sprintb (units, UMASK));

    printf ("attribute groups: %s\n", sprintb (attrs, AMASK));

    printf ("document types:");
    hit = 0;
    for (vf = vfs; vf -> vf_entry; vf++)
	if (vf -> vf_oid && (vf -> vf_flags & VF_OK)) {
	    printf ("\n  %-16.16s %s (%s)", sprintoid (vf -> vf_oid),
		    vf -> vf_text, vf -> vf_entry);
	    hit = 1;
	}
    if (!hit)
	printf (" none negotiated!");

    printf ("\nestimated integral FADU size: %d\n", fadusize);

    return OK;
}
#endif
