/* delete.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/delete.c,v 7.1 91/02/22 09:40:24 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/delete.c,v 7.1 91/02/22 09:40:24 mrose Interim $
 *
 *
 * $Log:	delete.c,v $
 * Revision 7.1  91/02/22  09:40:24  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:19:59  mrose
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


#include "quipu/util.h"
#include "quipu/remove.h"
#include "pepsy.h"
#include "quipu/DAS_pre_defs.h"

extern DN       dn;

#define	OPT	(!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define	RPS	(!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	opt, rps;

call_delete (argc, argv)
int             argc;
char          **argv;
{
	DN              dnptr,
	                trail = NULLDN; 
	struct ds_removeentry_arg remove_arg;
	struct DSError  error;

	if ((argc = service_control (OPT, argc, argv, &remove_arg.rma_common)) == -1)
		return;

	if (argc > 1) 
		if (move (argv[1]) == OK)
			argc--;

	if (argc != 1) {
		ps_printf (OPT,"Unknown option %s\n",argv[1]);
		Usage (argv[0]);
		return;
	}
	remove_arg.rma_object = dn;

	if (rebind () != OK)
		return;

	/* Strong authentication */
	if (remove_arg.rma_common.ca_security != (struct security_parms *) 0)
	{
	struct signature *sign_operation();

	remove_arg.rma_common.ca_sig =
		sign_operation((caddr_t)&remove_arg, 
			_ZRemoveEntryArgumentDataDAS, &_ZDAS_mod);
	}

	while (ds_removeentry (&remove_arg, &error) != DS_OK) {
		if (dish_error (OPT, &error) == 0)
			return;
		remove_arg.rma_object = error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
	} 
		
	ps_print (RPS, "Removed ");
	dn_print (RPS, dn, EDBOUT);
	delete_cache (dn);
	for (dnptr = dn; dnptr->dn_parent != NULLDN; dnptr = dnptr->dn_parent)
		trail = dnptr;

	if (trail != NULLDN) 
		trail->dn_parent = NULLDN;
	else
		dn = NULLDN;

	dn_comp_free (dnptr);
	ps_print (RPS, "\n");
}
