/* ryinitiator.c - generic interactive initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/lookup/RCS/ryinitiator.c,v 7.6 91/02/22 09:27:37 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/lookup/RCS/ryinitiator.c,v 7.6 91/02/22 09:27:37 mrose Interim $
 *
 *
 * $Log:	ryinitiator.c,v $
 * Revision 7.6  91/02/22  09:27:37  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/12/23  18:44:26  mrose
 * update
 * 
 * Revision 7.4  90/12/11  10:54:05  mrose
 * lock-and-load
 * 
 * Revision 7.3  90/10/29  18:40:21  mrose
 * updates
 * 
 * Revision 7.2  90/07/09  14:39:53  mrose
 * sync
 * 
 * Revision 7.1  90/07/01  21:04:28  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  22:56:41  mrose
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
#include <varargs.h>
#include "PasswordLookup-types.h"
#include "ryinitiator.h"

/*    DATA */

static char *myname = "ryinitiator";


extern char *isodeversion;

/*    INITIATOR */

ryinitiator (argc, argv, myservice, mycontext, mypci, ops, dispatches, quit)
int	argc;
char  **argv,
       *myservice,
       *mycontext,
       *mypci;
struct RyOperation ops[];
struct dispatch *dispatches;
IFP	quit;
{
    int	    iloop,
	    sd;
    char    buffer[BUFSIZ],
	   *vec[NVEC + 1];
    register struct dispatch   *ds;
    struct SSAPref sfs;
    register struct SSAPref *sf;
    register struct PSAPaddr *pa;
    struct AcSAPconnect accs;
    register struct AcSAPconnect   *acc = &accs;
    struct AcSAPindication  acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;
    AEI	    aei;
    OID	    ctx,
	    pci;
    struct PSAPctxlist pcs;
    register struct PSAPctxlist *pc = &pcs;
    struct RoSAPindication rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject *rop = &roi -> roi_preject;

    if (myname = rindex (argv[0], '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = argv[0];

    if (argc < 2)
	adios (NULLCP, "usage: %s host [operation [ arguments ... ]]", myname);

    if ((aei = _str2aei (argv[1], myservice, mycontext, argc < 3, NULLCP,
			 NULLCP)) == NULLAEI)
	adios (NULLCP, "unable to resolve service: %s", PY_pepy);
    if ((pa = aei2addr (aei)) == NULLPA)
	adios (NULLCP, "address translation failed");

    if ((ctx = ode2oid (mycontext)) == NULLOID)
	adios (NULLCP, "%s: unknown object descriptor", mycontext);
    if ((ctx = oid_cpy (ctx)) == NULLOID)
	adios (NULLCP, "out of memory");
    if ((pci = ode2oid (mypci)) == NULLOID)
	adios (NULLCP, "%s: unknown object descriptor", mypci);
    if ((pci = oid_cpy (pci)) == NULLOID)
	adios (NULLCP, "out of memory");
    pc -> pc_nctx = 1;
    pc -> pc_ctx[0].pc_id = 1;
    pc -> pc_ctx[0].pc_asn = pci;
    pc -> pc_ctx[0].pc_atn = NULLOID;

    if ((sf = addr2ref (PLocalHostName ())) == NULL) {
	sf = &sfs;
	(void) bzero ((char *) sf, sizeof *sf);
    }

    if (argc < 3) {
	printf ("%s", myname);
	if (sf -> sr_ulen > 2)
	    printf (" running on host %s", sf -> sr_udata + 2);
	if (sf -> sr_clen > 2)
	    printf (" at %s", sf -> sr_cdata + 2);
	printf (" [%s, ", oid2ode (ctx));
	printf ("%s]\n", oid2ode (pci));
	printf ("using %s\n", isodeversion);

	printf ("%s... ", argv[1]);
	(void) fflush (stdout);
    
	iloop = 1;
    }
    else {
	for (ds = dispatches; ds -> ds_name; ds++)
	    if (strcmp (ds -> ds_name, argv[2]) == 0)
		break;
	if (ds -> ds_name == NULL)
	    adios (NULLCP, "unknown operation \"%s\"", argv[2]);

	iloop = 0;
    }

    if (AcAssocRequest (ctx, NULLAEI, aei, NULLPA, pa, pc, NULLOID,
		0, ROS_MYREQUIRE, SERIAL_NONE, 0, sf, NULLPEP, 0, NULLQOS,
		acc, aci)
	    == NOTOK)
	acs_adios (aca, "A-ASSOCIATE.REQUEST");

    if (acc -> acc_result != ACS_ACCEPT) {
	if (iloop)
	    printf ("failed\n");

	adios (NULLCP, "association rejected: [%s]",
		AcErrString (acc -> acc_result));
    }
    if (iloop) {
	printf ("connected\n");
	(void) fflush (stdout);
    }

    sd = acc -> acc_sd;
    ACCFREE (acc);

    if (RoSetService (sd, RoPService, roi) == NOTOK)
	ros_adios (rop, "set RO/PS fails");

    if (iloop) {
	for (;;) {
	    if (getline (buffer) == NOTOK)
		break;

	    if (str2vec (buffer, vec) < 1)
		continue;

	    for (ds = dispatches; ds -> ds_name; ds++)
		if (strcmp (ds -> ds_name, vec[0]) == 0)
		    break;
	    if (ds -> ds_name == NULL) {
		advise (NULLCP, "unknown operation \"%s\"", vec[0]);
		continue;
	    }

	    invoke (sd, ops, ds, vec + 1);
	}
    }
    else
	invoke (sd, ops, ds, argv + 3);

    (*quit) (sd, (struct dispatch *) NULL, (char **) NULL, (caddr_t *) NULL);
}

/*  */

static	invoke (sd, ops, ds, args)
int	sd;
struct RyOperation ops[];
register struct dispatch *ds;
char  **args;
{
    int	    result;
    caddr_t in;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;

    in = NULL;
    if (ds -> ds_argument && (*ds -> ds_argument) (sd, ds, args, &in) == NOTOK)
	return;

    switch (result = RyStub (sd, ops, ds -> ds_operation, RyGenID (sd), NULLIP,
			     in, ds -> ds_result, ds -> ds_error, ROS_SYNC,
			     roi)) {
	case NOTOK:		/* failure */
	    if (ROS_FATAL (rop -> rop_reason))
		ros_adios (rop, "STUB");
	    ros_advise (rop, "STUB");
	    break;

	case OK:		/* got a result/error response */
	    break;

	case DONE:		/* got RO-END? */
	    adios (NULLCP, "got RO-END.INDICATION");
	    /* NOTREACHED */

	default:
	    adios (NULLCP, "unknown return from RyStub=%d", result);
	    /* NOTREACHED */
    }

    if (ds -> ds_mod && ds -> ds_ind >= 0 && in)
	(void) fre_obj (in, ds -> ds_mod -> md_dtab[ds -> ds_ind],
			ds -> ds_mod, 1);
}

/*  */

static int  getline (buffer)
char   *buffer;
{
    register int    i;
    register char  *cp,
                   *ep;
    static int  sticky = 0;

    if (sticky) {
	sticky = 0;
	return NOTOK;
    }

    printf ("%s> ", myname);
    (void) fflush (stdout);

    for (ep = (cp = buffer) + BUFSIZ - 1; (i = getchar ()) != '\n';) {
	if (i == EOF) {
	    printf ("\n");
	    clearerr (stdin);
	    if (cp != buffer) {
		sticky++;
		break;
	    }

	    return NOTOK;
	}

	if (cp < ep)
	    *cp++ = i;
    }
    *cp = NULL;

    return OK;
}

/*  */

void	ros_adios (rop, event)
register struct RoSAPpreject *rop;
char   *event;
{
    ros_advise (rop, event);

    _exit (1);
}


void	ros_advise (rop, event)
register struct RoSAPpreject *rop;
char   *event;
{
    char    buffer[BUFSIZ];

    if (rop -> rop_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s", RoErrString (rop -> rop_reason),
		rop -> rop_cc, rop -> rop_cc, rop -> rop_data);
    else
	(void) sprintf (buffer, "[%s]", RoErrString (rop -> rop_reason));

    advise (NULLCP, "%s: %s", event, buffer);
}

/*  */

void	acs_adios (aca, event)
register struct AcSAPabort *aca;
char   *event;
{
    acs_advise (aca, event);

    _exit (1);
}


void	acs_advise (aca, event)
register struct AcSAPabort *aca;
char   *event;
{
    char    buffer[BUFSIZ];

    if (aca -> aca_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s",
		AcErrString (aca -> aca_reason),
		aca -> aca_cc, aca -> aca_cc, aca -> aca_data);
    else
	(void) sprintf (buffer, "[%s]", AcErrString (aca -> aca_reason));

	advise (NULLCP, "%s: %s (source %d)", event, buffer,
		aca -> aca_source);
}

/*  */

#ifndef	lint
void	_advise ();


void	adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);

    _exit (1);
}
#else
/* VARARGS */

void	adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
void	advise (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);
}


static void  _advise (ap)
va_list	ap;
{
    char    buffer[BUFSIZ];

    asprintf (buffer, ap);

    (void) fflush (stdout);

    fprintf (stderr, "%s: ", myname);
    (void) fputs (buffer, stderr);
    (void) fputc ('\n', stderr);

    (void) fflush (stderr);
}
#else
/* VARARGS */

void	advise (what, fmt)
char   *what,
       *fmt;
{
    advise (what, fmt);
}
#endif


#ifndef	lint
void	ryr_advise (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);
}
#else
/* VARARGS */

void	ryr_advise (what, fmt)
char   *what,
       *fmt;
{
    ryr_advise (what, fmt);
}
#endif
