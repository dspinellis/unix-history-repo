/* dsargument.h - structures for argument and result */

/*
 * $Header: /f/osi/h/quipu/RCS/dsargument.h,v 7.3 91/02/22 09:25:49 mrose Interim $
 *
 *
 * $Log:	dsargument.h,v $
 * Revision 7.3  91/02/22  09:25:49  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/11/20  15:30:40  mrose
 * cjr
 * 
 * Revision 7.1  90/07/09  14:38:30  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:56:32  mrose
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


#ifndef QUIPUDSARG
#define QUIPUDSARG

#include "quipu/entry.h"	/* for getedb stuff */

/* CN_CTX values used by high level decoders */
#define CN_CTX_UNKNOWN		  0
#define CN_CTX_X500_DAP		  1
#define CN_CTX_X500_DSP		  2
#define CN_CTX_QUIPU_DSP	  3


struct DSArgument {
   int arg_type;
#define ds_recog_op(a) ((a >= OP_READ) && (a <= OP_MODIFYRDN))
#define qu_recog_op(a) ((a >= OP_READ) && (a <= OP_GETEDB))
   union {
       struct ds_read_arg arg_un_read;
       struct ds_compare_arg arg_un_compare;
       struct ds_abandon_arg arg_un_abandon;
       struct ds_list_arg arg_un_list;
       struct ds_search_arg arg_un_search;
       struct ds_addentry_arg arg_un_addentry;
       struct ds_removeentry_arg arg_un_removeentry;
       struct ds_modifyentry_arg arg_un_modifyentry;
       struct ds_modifyrdn_arg arg_un_modifyrdn;
       struct getedb_arg arg_un_getedb;
   } arg_un;
#define arg_rd arg_un.arg_un_read
#define arg_cm arg_un.arg_un_compare
#define arg_ab arg_un.arg_un_abandon
#define arg_ls arg_un.arg_un_list
#define arg_sr arg_un.arg_un_search
#define arg_ad arg_un.arg_un_addentry
#define arg_rm arg_un.arg_un_removeentry
#define arg_me arg_un.arg_un_modifyentry
#define arg_mr arg_un.arg_un_modifyrdn
#define arg_ge arg_un.arg_un_getedb
};

struct DSResult {
    int result_type;    /* same values as for DSArgument                */
    union {
       struct ds_read_result result_un_read;
       struct ds_compare_result result_un_compare;
       struct ds_list_result result_un_list;
       struct ds_search_result result_un_search;
       struct getedb_result result_un_getedb;
    } result_un;
#define res_rd result_un.result_un_read
#define res_cm result_un.result_un_compare
#define res_ls result_un.result_un_list
#define res_sr result_un.result_un_search
#define res_ge result_un.result_un_getedb
};

typedef struct cross_ref
    {
	DN			  xref_dn;
	struct access_point	* xref_ap;
	struct cross_ref	* xref_next;
    } CrossRefs;
#define NULLXREF ((struct cross_ref *) NULL)

typedef struct chain_arg
    {
	DN				  cha_originator;
	DN				  cha_target;
	struct op_progress		  cha_progress;
	struct trace_info		* cha_trace;
	char				  cha_aliasderef;
	int				  cha_aliasedrdns;
	char				  cha_entryonly;
	int				  cha_returnrefs;
	int				  cha_reftype;
	PE				  cha_domaininfo;
	char				* cha_timelimit;
	struct security_parms 		* cha_security;
    } * ChainingArg;

typedef struct chain_res
    {
	PE				  chr_domaininfo;
	struct cross_ref		* chr_crossrefs;
	struct security_parms 		* chr_security;
    } * ChainingRes;

typedef struct ds_op_arg
    {
	struct chain_arg	dca_charg;
	struct DSArgument	dca_dsarg;
	int			dca_choice;
	struct alg_id *		dca_alg;
	int			dca_len;
	char *			dca_bit;
    } OPArgument;

typedef struct ds_op_res
    {
	struct chain_res	dcr_chres;
	struct DSResult	dcr_dsres;
	int			dcr_choice;
	struct alg_id *		dcr_alg;
	int			dcr_len;
	char *			dcr_bit;
    } OPResult;

#endif
