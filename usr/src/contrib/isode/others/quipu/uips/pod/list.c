#include <sys/types.h>

#include "quipu/util.h"
#include "quipu/common.h"
#include "quipu/entry.h"
#include "quipu/name.h"

#include "sequence.h"
#include "filt.h"
#include "defs.h"

extern str_seq dnseq, backseq, showseq;
extern int entry_number, back_buf_num, dn_number;

#ifndef NO_STATS
extern LLog    *log_stat;
#endif

extern char goto_path[], base_path[], friendly_base_path[], mvalue [];

str_seq SortList();
void dn2buf();

dsEnqError list_start()
{
  struct ds_search_arg search_arg;
  struct ds_search_result result;
  struct DSError          error;
  dsEnqError return_error;

  return_error = Okay;

  if (get_default_service (&search_arg.sra_common) != 0) {
    return localdsaerror;
  }

  search_arg.sra_common.ca_servicecontrol.svc_options = SVC_OPT_PREFERCHAIN;

  search_arg.sra_baseobject = (*base_path != 'T'?
                               str2dn (base_path):
                               NULLDN);

  search_arg.sra_eis.eis_allattributes = FALSE;
  search_arg.sra_eis.eis_infotypes = EIS_ATTRIBUTETYPESONLY;
  search_arg.sra_eis.eis_select = 0;

  search_arg.sra_searchaliases = TRUE;
  search_arg.sra_subset = SRA_ONELEVEL;

  search_arg.sra_filter = filter_alloc();
  search_arg.sra_filter->flt_type = FILTER_NOT;
  search_arg.sra_filter->flt_next = NULLFILTER;
  search_arg.sra_filter->flt_un.flt_un_filter = filter_alloc();
  search_arg.sra_filter->flt_un.flt_un_filter->flt_type = FILTER_ITEM;
  search_arg.sra_filter->flt_un.flt_un_filter->flt_next = NULLFILTER;
  search_arg.sra_filter->flt_un.flt_un_filter->flt_un.flt_un_item.fi_type
    = FILTERITEM_EQUALITY;
  search_arg.sra_filter->flt_un.flt_un_filter->flt_un.flt_un_item.fi_un.
    fi_un_ava.ava_type = AttrT_new("2.5.4.0");

  search_arg.sra_filter->flt_un.flt_un_filter->flt_un.flt_un_item.fi_un.
    fi_un_ava.ava_value =
      str2AttrV("dsa", search_arg.sra_filter->flt_un.flt_un_filter->
                flt_un.flt_un_item.fi_un.fi_un_ava.ava_type->
                oa_syntax);

#ifndef NO_STATS
  LLOG (log_stat,LLOG_NOTICE,("search +%s,extent %d, val objectClass != dsa",
			      base_path,search_arg.sra_subset));
#endif

  if (search_arg.sra_filter->flt_un.flt_un_filter->flt_un.flt_un_item.
      fi_un.fi_un_ava.ava_value == NULLAttrV) {
    return_error = localdsaerror;
  } else if (ds_search (&search_arg, &error, &result) != DS_OK) {
    free_seq(dnseq);
    dnseq = NULLDS;
    dn_number = 0;
    log_ds_error(&error);
    ds_error_free(&error);
    switch (error.dse_type) {
    case DSE_LOCALERROR:
      return_error = duaerror;
      break;
    case DSE_REMOTEERROR:
      return_error = localdsaerror;
      break;
    case DSE_ATTRIBUTEERROR:
      return_error = attributerror;
      break;
    case DSE_REFERRAL:
    case DSE_DSAREFERRAL:
      return_error = remotedsaerror;
      break;
    case DSE_SECURITYERROR:
      return_error = security;
      break;
    case DSE_NAMEERROR:
      return_error = namerror;
      break;
    case DSE_SERVICEERROR:
      return_error = serviceerror;
      break;
    default:
      return_error = localdsaerror;
      break;
    }
  } else {
    dn_number = 0;
    if (result.CSR_entries != NULLENTRYINFO) {
      register EntryInfo *ptr;
      
      free_seq(dnseq);
      dnseq = NULLDS;
      dn_number = 0;

      for (ptr = result.CSR_entries; ptr != NULLENTRYINFO;
           ptr = ptr->ent_next) {
        dn_number++;
        dn2buf ((caddr_t)ptr->ent_dn, goto_path);
        add_seq (&dnseq, goto_path);
      }

      if (dn_number) dnseq = SortList(dnseq);
    } else if (result.CSR_limitproblem == LSR_NOLIMITPROBLEM) {
      free_seq(dnseq);
      dnseq = NULLDS;
      dn_number = 0;
      return_error = nothingfound;
    }

    if (result.CSR_limitproblem != LSR_NOLIMITPROBLEM) {
      switch (result.CSR_limitproblem) {
      case LSR_TIMELIMITEXCEEDED:
	if (dn_number > 0) return_error = timelimit_w_partial;
	else {
	  free_seq(dnseq);
	  dnseq = NULLDS;
	  return_error = timelimit;
	}
	break;
      case LSR_SIZELIMITEXCEEDED:
	return_error = listsizelimit;
	break;
      case LSR_ADMINSIZEEXCEEDED:
	if (dn_number > 0) return_error = adminlimit_w_partial;
	else {
	  free_seq(dnseq);
	  dnseq = NULLDS;
	  return_error = adminlimit;
	}
	break;
      }
    }
    if (result.CSR_entries) entryinfo_free(result.CSR_entries, 0);
  }
  entry_number = dn_number;
  filter_free(search_arg.sra_filter);
  dn_free(search_arg.sra_baseobject);
  ds_error_free(&error);
  return return_error;
}

