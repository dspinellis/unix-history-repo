/* modifyrdn.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/modifyrdn.c,v 7.1 91/02/22 09:40:45 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/modifyrdn.c,v 7.1 91/02/22 09:40:45 mrose Interim $
 *
 *
 * $Log:	modifyrdn.c,v $
 * Revision 7.1  91/02/22  09:40:45  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:20:15  mrose
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
#include "quipu/modifyrdn.h"
#include "pepsy.h"
#include "quipu/DAS_pre_defs.h"

extern DN       dn;

#define	OPT	(!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define	RPS	(!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	opt, rps;

call_modifyrdn (argc, argv)
int             argc;
char          **argv;
{
	struct ds_modifyrdn_arg modrdn_arg;
	struct DSError  error;
	RDN             newname = NULLRDN;
	DN              dnptr;
	DN              trail;
	int             x;
	char 		deleterdn = TRUE;


	if ((argc = service_control (OPT,argc, argv, &modrdn_arg.mra_common)) == -1)
		return;

	for (x = 1; x < argc; x++) {
		if (test_arg (argv[x], "-name",2)) {
			if ((newname = str2rdn (argv[++x])) == NULLRDN) {
				ps_printf (OPT,"invalid RDN %s\n",argv[x]);
				return;
			}
		} else if (test_arg (argv[x],"-delete",2))
			deleterdn = TRUE;
		else if (test_arg (argv[x],"-nodelete",3))
			deleterdn = FALSE;
		else if (move (argv[x]) == OK)
			continue;
		else {
			ps_printf (OPT,"Unknown option %s\n",argv[x]);
			Usage (argv[0]);
			return;
		}
	}

	modrdn_arg.deleterdn = deleterdn;
	modrdn_arg.mra_object = dn;
	if (newname == NULLRDN) {
		ps_print (OPT, "Invalid RDN\n");
		Usage (argv[0]);
		return;
	}
	modrdn_arg.mra_newrdn = newname;

	if (rebind () != OK)
		return;

	/* Strong authentication */
	if (modrdn_arg.mra_common.ca_security != (struct security_parms *) 0)
	{
	struct signature *sign_operation();

	modrdn_arg.mra_common.ca_sig =
		sign_operation((caddr_t)&modrdn_arg, _ZModifyRDNArgumentDataDAS,&_ZDAS_mod);
	}

	while (ds_modifyrdn (&modrdn_arg, &error) != DS_OK) {
		if (dish_error (OPT, &error) == 0) {
			rdn_free(modrdn_arg.mra_newrdn);
			return;
		}
		modrdn_arg.mra_object = error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
	} 

	ps_print (RPS, "Modify done\n");
	delete_cache (dn);	/* re-cache when next read */
	for (dnptr = dn; dnptr->dn_parent != NULLDN; dnptr = dnptr->dn_parent)
		trail = dnptr;

	dn_comp_free (dnptr);
	trail->dn_parent = dn_comp_new (rdn_cpy (newname));
	rdn_free(modrdn_arg.mra_newrdn);
}
