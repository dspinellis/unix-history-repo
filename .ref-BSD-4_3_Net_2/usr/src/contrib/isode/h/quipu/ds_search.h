/* ds_search.h - structures for searching */

/*
 * $Header: /f/osi/h/quipu/RCS/ds_search.h,v 7.6 91/03/09 11:54:05 mrose Exp $
 *
 *
 * $Log:	ds_search.h,v $
 * Revision 7.6  91/03/09  11:54:05  mrose
 * update
 * 
 * Revision 7.5  91/02/22  09:25:45  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/11/20  15:30:35  mrose
 * cjr
 * 
 * Revision 7.3  90/10/17  11:46:24  mrose
 * sync
 * 
 * Revision 7.2  90/07/09  14:38:27  mrose
 * sync
 * 
 * Revision 7.1  90/03/15  11:18:05  mrose
 * quipu-sync
 * 
 * Revision 7.0  89/11/23  21:56:31  mrose
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


#ifndef QUIPUSRCH
#define QUIPUSRCH

#include "quipu/config.h"
#include "quipu/commonarg.h"
#include "quipu/ds_error.h"
#include "quipu/dap.h"

typedef struct {
	    AttributeType fi_sub_type;
	    AV_Sequence fi_sub_initial;
	    AV_Sequence fi_sub_any;
	    AV_Sequence fi_sub_final;
				/* initial and final should be zero or  */
				/* one components only                  */
	    char       *fi_sub_match; /* for DSA use */
	} Filter_Substrings;

struct filter_item {
    int         fi_type;
#define FILTERITEM_EQUALITY 1
#define FILTERITEM_SUBSTRINGS 2
#define FILTERITEM_GREATEROREQUAL 3
#define FILTERITEM_LESSOREQUAL 4
#define FILTERITEM_PRESENT 5
#define FILTERITEM_APPROX 6
    union {
	AttributeType fi_un_type;
	AVA fi_un_ava;
	Filter_Substrings fi_un_substrings;
    } fi_un;
	/* field for DSA use - no need to fill is DUA */
    IFP	    fi_ifp;
};

#define NULLFITEM (struct filter_item *) NULL
#define UNSUB    fi_un.fi_un_substrings
#define UNAVA    fi_un.fi_un_ava
#define UNTYPE   fi_un.fi_un_type
#define filter_item_alloc() (struct filter_item *) smalloc (sizeof (struct filter_item));

typedef struct filter {
    int        flt_type;
#define FILTER_ITEM 1
#define FILTER_AND 2
#define FILTER_OR 3
#define FILTER_NOT 4
   struct filter *flt_next;
   union {
	struct filter_item flt_un_item;
					/* a basic item                 */
	struct filter *flt_un_filter;
					/* or a pointer to a chain of   */
					/* filters                      */
   } flt_un;
}filter, *Filter;

#define NULLFILTER (Filter)NULL
#define FUITEM   flt_un.flt_un_item
#define FUFILT   flt_un.flt_un_filter
#define filter_alloc() (Filter) smalloc (sizeof (filter));

struct ds_search_arg {
    CommonArgs sra_common;
    DN sra_baseobject;
    int sra_subset;
#define SRA_BASEOBJECT          0
#define SRA_ONELEVEL            1
#define SRA_WHOLESUBTREE        2
    Filter sra_filter;
    char sra_searchaliases;
    EntryInfoSelection sra_eis;
};

struct ds_search_unit {
    CommonResults srr_common;
    DN srr_object;
    EntryInfo *srr_entries;
    POQ		srr_poq;	
};

struct ds_search_result {
    char	srr_correlated;
    union {
	struct ds_search_unit	* srr_unit;
	struct ds_search_result * srr_parts;
    } srr_un;
#define CSR_common	srr_un.srr_unit->srr_common
#define CSR_object	srr_un.srr_unit->srr_object
#define CSR_entries	srr_un.srr_unit->srr_entries
#define CSR_limitproblem	srr_un.srr_unit->srr_poq.poq_limitproblem
#define srr_limitproblem	srr_poq.poq_limitproblem
#define CSR_cr	srr_un.srr_unit->srr_poq.poq_cref
#define srr_cr	srr_poq.poq_cref
    struct ds_search_result * srr_next;
};
#define NULLSRR	((struct ds_search_result *) 0)

/* following used by search for scheduling */

struct ds_search_task {
   DN  st_baseobject;
   DN  st_alias;
   DN  st_bind;
   int st_subset;
   int st_size;
#ifdef TURBO_INDEX
   int st_optimized;
#endif
   struct di_block	* st_di;
   struct ds_search_task * st_next;	
   ContinuationRef	st_cr;
   struct ds_search_task * st_save;	
   char st_entryonly;
};
#define NULL_ST ((struct ds_search_task *) NULL)
#define st_alloc() (struct ds_search_task *) smalloc (sizeof(struct ds_search_task));

/* search max 1000 entries before worrying about time limits */
#define SEARCH_DELTA_SIZE 1000	

/* character used to mark T.61 strings */
#define T61_MARK '$'

#ifdef TURBO_AVL

/* used by search to pass info to routines called by avl routines */
struct search_kid_arg {
        EntryInfo               **ska_einfo;
        struct ds_search_arg    *ska_arg;
        struct ds_search_task   **ska_local;
        struct ds_search_task   **ska_refer;
        int                     ska_extent;
	int			ska_tmp;
	int			ska_domore;
        DN                      ska_path;
        DN                      ska_dnend;
	DN			ska_bind;
        int                     ska_ismanager;
};

#endif

#endif
