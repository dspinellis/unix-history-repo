/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * ryinitiator.c : initators interface to the ISODE Ry-Library
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/ryinitiator.c,v 7.3 91/02/22 09:28:28 mrose Interim $
 *
 * $Log:	ryinitiator.c,v $
 * Revision 7.3  91/02/22  09:28:28  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:55:06  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:08:08  ow
 * Initial revision
 * 
 */

#ifndef       lint
static char *rcsid = "$Header: /f/osi/others/rfa/RCS/ryinitiator.c,v 7.3 91/02/22 09:28:28 mrose Interim $";
#endif

/*
 *                              NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */

#include <stdio.h>
#include <varargs.h>
#include "RFA-ops.h"
#include "RFA-types.h"


void	errexit (), errmsg (), ros_adios (), ros_errmsg (),
	acs_errmsg (), acs_errexit ();


static char *myservice = "rfa";

static char *mycontext = "rfa";
static char *mypci = "rfa pci";


extern char *isodeversion;
extern char *myname;

static int ryconnect ();


makeconn (thehost, password, user)
char	*thehost;
char 	*password, *user;
{
	int	result;
	PE	data;
	struct type_RFA_Initiate *initial;

	if ((initial = (struct type_RFA_Initiate *)malloc(sizeof *initial)) == NULL)
		errexit ("memory", "out of");

	initial -> user = str2qb (user, strlen (user), 1);
	initial -> password = str2qb (password, strlen(password), 1);

	if (encode_RFA_Initiate (&data, 1, 0, NULLCP, initial) == NOTOK) {
		errmsg (NULLCP, "Error encoding data");
		return 0;
	}
	data -> pe_context = 3;	/* hack */

	result = ryconnect (thehost, data, myservice, mycontext, mypci);

	free_RFA_Initiate (initial);

	return result;
}


static	int	ry_sd = NOTOK;

static int ryconnect (thehost, data, theservice, thecontext, thepci)
char   *thehost,
       *theservice,
       *thecontext,
       *thepci;
PE	data;
{
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

    if ((aei = str2aei (thehost, theservice)) == NULLAEI)
		errexit (NULLCP, "%s-%s: unknown application-entity",thehost, theservice);
    if ((pa = aei2addr (aei)) == NULLPA)
		errexit (NULLCP, "address translation failed");

    if ((ctx = ode2oid (thecontext)) == NULLOID)
		errexit (NULLCP, "%s: unknown object descriptor", thecontext);
    if ((ctx = oid_cpy (ctx)) == NULLOID)
		errexit (NULLCP, "out of memory");
    if ((pci = ode2oid (thepci)) == NULLOID)
		errexit (NULLCP, "%s: unknown object descriptor", thepci);
    if ((pci = oid_cpy (pci)) == NULLOID)
		errexit (NULLCP, "out of memory");
    pc -> pc_nctx = 1;
    pc -> pc_ctx[0].pc_id = 1;
    pc -> pc_ctx[0].pc_asn = pci;
    pc -> pc_ctx[0].pc_atn = NULLOID;

    if ((sf = addr2ref (PLocalHostName ())) == NULL) {
		sf = &sfs;
		(void) bzero ((char *) sf, sizeof *sf);
    }

    if (AcAssocRequest (ctx, NULLAEI, aei, NULLPA, pa, pc, NULLOID,
		0, ROS_MYREQUIRE, SERIAL_NONE, 0, sf, &data, 1, NULLQOS,
		acc, aci)
	    == NOTOK)
		acs_errexit (aca, "A-ASSOCIATE.REQUEST");

    if (acc -> acc_result != ACS_ACCEPT) {
		int slen;
		char *str;

		if (acc -> acc_ninfo > 0 && (str = prim2str(acc->acc_info[0], &slen)))
	   		errexit (NULLCP, "association rejected: [%s] %*.*s",
				AcErrString (acc -> acc_result), slen, slen, str);
		else
	   		errexit (NULLCP, "association rejected: [%s]",
		   		AcErrString (acc -> acc_result));
    }

    ry_sd = acc -> acc_sd;
    ACCFREE (acc);

    if (RoSetService (ry_sd, RoPService, roi) == NOTOK)
	ros_adios (rop, "set RO/PS fails");
    return OK;
}

closeconn ()
{
    struct AcSAPrelease acrs;
    register struct AcSAPrelease   *acr = &acrs;
    struct AcSAPindication  acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;

    if (ry_sd == NOTOK)
	    return;

    if (AcRelRequest (ry_sd, ACF_NORMAL, NULLPEP, 0, NOTOK, acr, aci) == NOTOK)
		acs_errexit (aca, "A-RELEASE.REQUEST");

    if (!acr -> acr_affirmative) {
		(void) AcUAbortRequest (ry_sd, NULLPEP, 0, aci);
		errexit (NULLCP, "release rejected by peer: %d", acr -> acr_reason);
    }

    ACRFREE (acr);
}



invoke (op, arg, res, err)
int	op;
caddr_t	arg, *res;
int *err;
{
    int	    result;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;

    switch (result = RyOperation (ry_sd, table_RFA_Operations, op,
			     arg, res, err, roi)) {
		case NOTOK:		/* failure */
			if (ROS_FATAL (rop -> rop_reason))
				ros_adios (rop, "STUB");
			ros_errmsg (rop, "STUB");
			break;

		case OK:		/* got a result/error response */
			if (*err == RY_REJECT) { 
				errmsg (NULLCP, "REJECTED"); 
				return NOTOK; 
			}  
			break;

		case DONE:		/* got RO-END? */
			errexit (NULLCP, "got RO-END.INDICATION");
			/* NOTREACHED */

		default:
			errexit (NULLCP, "unknown return from RyStub=%d", result);
	    /* NOTREACHED */
    }

	return result;
}


void	ros_adios (rop, event)
register struct RoSAPpreject *rop;
char   *event;
{
    ros_errmsg (rop, event);

    cleanup ();

    _exit (1);
}


void	ros_errmsg (rop, event)
register struct RoSAPpreject *rop;
char   *event;
{
    char    buffer[BUFSIZ];

    if (rop -> rop_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s", RoErrString (rop -> rop_reason),
		rop -> rop_cc, rop -> rop_cc, rop -> rop_data);
    else
	(void) sprintf (buffer, "[%s]", RoErrString (rop -> rop_reason));

    errmsg (NULLCP, "%s: %s", event, buffer);
}


void	acs_errexit (aca, event)
register struct AcSAPabort *aca;
char   *event;
{
    acs_errmsg (aca, event);

    cleanup ();
    _exit (1);
}


void	acs_errmsg (aca, event)
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

	errmsg (NULLCP, "%s: %s (source %d)", event, buffer,
		aca -> aca_source);
}


#ifndef	lint
void	_errmsg ();


void	errexit (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _errmsg (ap);

    cleanup ();

    va_end (ap);

    _exit (1);
}
#else
/* VARARGS */

void	errexit (what, fmt)
char   *what,
       *fmt;
{
    errexit (what, fmt);
}
#endif


#ifndef	lint
void	errmsg (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _errmsg (ap);

    va_end (ap);
}


static void  _errmsg (ap)
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

void	errmsg (what, fmt)
char   *what,
       *fmt;
{
    errmsg (what, fmt);
}
#endif


#ifndef	lint
void	ryr_errmsg (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _errmsg (ap);

    va_end (ap);
}
#else
/* VARARGS */

void	ryr_errmsg (what, fmt)
char   *what,
       *fmt;
{
    ryr_errmsg (what, fmt);
}
#endif

