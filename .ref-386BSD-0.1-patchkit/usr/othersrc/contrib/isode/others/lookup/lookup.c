/* lookup.c - password lookup service -- initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/lookup/RCS/lookup.c,v 7.2 91/02/22 09:27:32 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/lookup/RCS/lookup.c,v 7.2 91/02/22 09:27:32 mrose Interim $
 *
 *
 * $Log:	lookup.c,v $
 * Revision 7.2  91/02/22  09:27:32  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:04:26  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  22:56:36  mrose
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
#include "PasswordLookup-types.h"	/* type definitions */
#include "ryinitiator.h"		/* for generic interctive initiators */
#include "PasswordLookup-ops.h"		/* operation definitions */

/*    DATA */

static char *myservice = "passwdstore";

static char *mycontext = "isode passwd lookup demo";
static char *mypci = "isode passwd lookup demo pci";


					/* ARGUMENTS */
int	do_lookupUser (), do_lookupUID (), do_help (), do_quit ();

					/* RESULTS */
int	lookup_result ();

					/* ERRORS */
int	lookup_error ();


static struct dispatch dispatches[] = {
    "lookupUser", operation_PasswordLookup_lookupUser,
    do_lookupUser, &_ZPasswordLookup_mod, _ZUserNamePasswordLookup,
    lookup_result, lookup_error,
    "find user by name",

    "lookupUID", operation_PasswordLookup_lookupUID,
    do_lookupUID, &_ZPasswordLookup_mod, _ZUserIDPasswordLookup,
    lookup_result, lookup_error,
    "find user by id",

    "help", 0,
    do_help, (modtyp *) 0, 0,
    NULLIFP, NULLIFP,
    "print this information",

    "quit", 0,
    do_quit, (modtyp *) 0, 0,
    NULLIFP, NULLIFP,
    "terminate the association and exit",

    NULL
};

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    (void) ryinitiator (argc, argv, myservice, mycontext, mypci,
			table_PasswordLookup_Operations, dispatches, do_quit);

    exit (0);			/* NOTREACHED */
}

/*    ARGUMENTS */

/* ARGSUSED */

static int  do_lookupUser (sd, ds, args, arg)
int	sd;
struct dispatch *ds;
char  **args;
register struct type_PasswordLookup_UserName **arg;
{
    char   *cp;

    if ((cp = *args++) == NULL) {
	advise (NULLCP, "usage: lookupUser username");
	return NOTOK;
    }

    if ((*arg = str2qb (cp, strlen (cp), 1)) == NULL)
	    adios (NULLCP, "out of memory");

    return OK;
}

/*  */

/* ARGSUSED */

static int  do_lookupUID (sd, ds, args, arg)
int	sd;
struct dispatch *ds;
char  **args;
register struct type_PasswordLookup_UserID **arg;
{
    char   *cp;

    if ((cp = *args++) == NULL) {
	advise (NULLCP, "usage: lookupUID userid");
	return NOTOK;
    }

    if ((*arg = (struct type_PasswordLookup_UserID *) calloc (1, sizeof **arg))
	    == NULL)
	adios (NULLCP, "out of memory");

    (*arg) -> parm = atoi (cp);

    return OK;
}

/*  */

/* ARGSUSED */

static int  do_help (sd, ds, args, dummy)
int	sd;
register struct dispatch *ds;
char  **args;
caddr_t *dummy;
{
    printf ("\nCommands are:\n");
    for (ds = dispatches; ds -> ds_name; ds++)
	printf ("%s\t%s\n", ds -> ds_name, ds -> ds_help);

    return NOTOK;
}

/*  */

/* ARGSUSED */

static int  do_quit (sd, ds, args, dummy)
int	sd;
struct dispatch *ds;
char  **args;
caddr_t *dummy;
{
    struct AcSAPrelease acrs;
    register struct AcSAPrelease   *acr = &acrs;
    struct AcSAPindication  acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;

    if (AcRelRequest (sd, ACF_NORMAL, NULLPEP, 0, NOTOK, acr, aci) == NOTOK)
	acs_adios (aca, "A-RELEASE.REQUEST");

    if (!acr -> acr_affirmative) {
	(void) AcUAbortRequest (sd, NULLPEP, 0, aci);
	adios (NULLCP, "release rejected by peer: %d", acr -> acr_reason);
    }

    ACRFREE (acr);

    exit (0);
}

/*    RESULTS */

/* ARGSUSED */

static int  lookup_result (sd, id, dummy, result, roi)
int	sd,
    	id,
    	dummy;
register struct type_PasswordLookup_Passwd *result;
struct RoSAPindication *roi;
{
    print_qb (result -> name);
    putchar (':');
    print_qb (result -> passwd);
    printf (":%d:%d:", result -> uid -> parm, result -> gid -> parm);
    print_qb (result -> gecos);
    putchar (':');
    print_qb (result -> dir);
    putchar (':');
    print_qb (result -> shell);
    putchar ('\n');

    return OK;
}


static	print_qb (q)
register struct qbuf *q;
{
    register struct qbuf *p;

    if (q == NULL)
	return;

    for (p = q -> qb_forw; p != q; p = p -> qb_forw)
	printf ("%*.*s", p -> qb_len, p -> qb_len, p -> qb_data);
}

/*    ERRORS */

/* ARGSUSED */

static int  lookup_error (sd, id, error, parameter, roi)
int	sd,
	id,
    	error;
caddr_t parameter;
struct RoSAPindication *roi;
{    
    register struct RyError *rye;

    if (error == RY_REJECT) {
	advise (NULLCP, "%s", RoErrString ((int) parameter));
	return OK;
    }

    if (rye = finderrbyerr (table_PasswordLookup_Errors, error))
	advise (NULLCP, "%s",  rye -> rye_name);
    else
	advise (NULLCP, "Error %d", error);

    return OK;
}
