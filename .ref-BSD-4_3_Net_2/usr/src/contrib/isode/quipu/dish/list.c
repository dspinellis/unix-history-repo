/* list.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/list.c,v 7.2 91/02/22 09:40:40 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/list.c,v 7.2 91/02/22 09:40:40 mrose Interim $
 *
 *
 * $Log:	list.c,v $
 * Revision 7.2  91/02/22  09:40:40  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:55:27  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:20:11  mrose
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
#include "quipu/list.h"
#include "quipu/sequence.h"
#include "pepsy.h"
#include "quipu/DAS_pre_defs.h"

extern DN       dn;

#define	OPT	(!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define	RPS	(!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	opt, rps;

extern char	move_flag;

extern int      sizelimit;
char  list_show;

call_list (argc, argv)
int             argc;
char          **argv;
{
	struct ds_list_arg list_arg;
	struct ds_list_result result;
	struct list_cache *ptr;
	struct DSError  error;
	int             x;
	char            nocacheflag = FALSE;
	extern 	int	copy_flag;

	list_show = TRUE;
	move_flag = FALSE;

	list_arg.lsa_common.ca_servicecontrol.svc_sizelimit = sizelimit;

	if ((argc = service_control (OPT, argc, argv, &list_arg.lsa_common)) == -1)
		return;

	for (x = 1; x < argc; x++) {

		if (test_arg (argv[x], "-nocache",4))
			nocacheflag = TRUE;
		else if (test_arg (argv[x], "-noshow",4))
			list_show = FALSE;
		else if (test_arg (argv[x], "-move",2))
			move_flag = TRUE;
		else if (test_arg (argv[x], "-nomove",3))
			move_flag = FALSE;
		else if (move (argv[x]) == OK)
			continue;
		else {
			ps_printf (OPT,"Unknown option %s\n",argv[x]);
			Usage (argv[0]);
			return;
		}

	}

	list_arg.lsa_object = dn;

	if ((!nocacheflag) && copy_flag)
		if ((ptr = find_list_cache (dn,list_arg.lsa_common.ca_servicecontrol.svc_sizelimit)) != NULLCACHE) {
			print_list_subordinates (ptr->list_subs, ptr->list_problem);
			consolidate_move();
			return;
		}

	if (rebind () != OK)
		return;

	/* Strong authentication */
	if (list_arg.lsa_common.ca_security != (struct security_parms *) 0)
	{
	struct signature *sign_operation();

	list_arg.lsa_common.ca_sig = 
		sign_operation((caddr_t)&list_arg, _ZListArgumentDataDAS,
			 &_ZDAS_mod);
	}

	while (ds_list (&list_arg, &error, &result) != DS_OK) {	/* deal with error */
		if (dish_error (OPT, &error) == 0)
			return;
		list_arg.lsa_object = error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
	}

	if (result.lsr_common.cr_aliasdereferenced) {
		ps_print (RPS, "(Alias dereferenced - ");
		dn_print (RPS, result.lsr_object, EDBOUT);
		dn_free (result.lsr_object);
		ps_print (RPS, ")\n");
	}
	print_list_subordinates (result.lsr_subordinates, result.lsr_limitproblem);

	cache_list (result.lsr_subordinates, result.lsr_limitproblem,dn,
	list_arg.lsa_common.ca_servicecontrol.svc_sizelimit);

	subords_free (result.lsr_subordinates);

	consolidate_move();
}

print_list_subordinates (ptr, prob)
struct subordinate *ptr;
int             prob;
{
DN adn;
DN newdn;
int seqno;
extern char * result_sequence;

	adn = dn_cpy (dn);
	newdn = dn_comp_new (rdn_comp_new(NULLAttrT,NULLAttrV));	
	if (adn != NULLDN)
		dn_append (adn,newdn);
	else {
		dn_free(adn);
		adn = newdn;
	}

	if (result_sequence)
		set_sequence (result_sequence);

	if (ptr == NULLSUBORD)
		if (list_show)
			ps_print (RPS,"No children\n");

	for (; ptr != NULLSUBORD; ptr = ptr->sub_next) {
		rdn_free (newdn->dn_rdn);
		dn_comp_fill (newdn,rdn_cpy(ptr->sub_rdn));
		seqno = add_sequence (adn);
		if (seqno != 0)
			ps_printf (RPS,"%-3d ",seqno);
		if (list_show)
			rdn_print (RPS, ptr->sub_rdn, READOUT);
		ps_print (RPS, "\n");
	}

	dn_free (adn);

	if (prob != LSR_NOLIMITPROBLEM)
		if (list_show)
			ps_print (RPS, "(Limit problem)\n");

}
