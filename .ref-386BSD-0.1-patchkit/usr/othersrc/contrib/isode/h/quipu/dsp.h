/* dsp.h - a few DSP things */

/*
 * $Header: /f/osi/h/quipu/RCS/dsp.h,v 7.2 91/02/22 09:25:51 mrose Interim $
 *
 *
 * $Log:	dsp.h,v $
 * Revision 7.2  91/02/22  09:25:51  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/20  15:30:44  mrose
 * cjr
 * 
 * Revision 7.0  89/11/23  21:56:33  mrose
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


#ifndef DSP
#define DSP

#include "isoaddrs.h"
#include "quipu/name.h"

struct op_progress {            /* represents OperationProgress         */
    int         op_resolution_phase;
#define OP_PHASE_NOTDEFINED     -1
#define OP_PHASE_NOTSTARTED     1
#define OP_PHASE_PROCEEDING     2
#define OP_PHASE_COMPLETED      3
    int         op_nextrdntoberesolved;
};

struct access_point {           /* represents AccessPoint               */
    DN                  ap_name;
    struct PSAPaddr     * ap_address;
		    /*  from ISODE                                      */
		    /* In INCA, this may be left out                    */
    struct access_point *ap_next;
};
#define NULLACCESSPOINT ((struct access_point *) NULL)

				/* Continuation Ref definded in DSP     */
				/* represents ContinuationReference     */
typedef struct continuation_ref {
    DN          cr_name;
    struct op_progress cr_progress;
    int         cr_rdn_resolved;
#define CR_RDNRESOLVED_NOTDEFINED       -1
    int         cr_aliasedRDNs;
#define CR_NOALIASEDRDNS -1
    int         cr_reftype;
#define RT_UNDEFINED    -1
#define RT_SUPERIOR     1
#define RT_SUBORDINATE  2
#define RT_CROSS        3
#define RT_NONSPECIFICSUBORDINATE       4
    struct access_point		* cr_accesspoints;
    struct continuation_ref *cr_next;
				/*  for chaining Continuation Refs      */
				/* They usually occur in SETs           */
}continuation_ref, *ContinuationRef;

#define NULLCONTINUATIONREF ((ContinuationRef) 0)

struct trace_info {
    DN          ti_target;
    DN          ti_dsa;
    struct op_progress  ti_progress;
    struct trace_info   *ti_next;
};

#define NULLTRACEINFO ((struct trace_info *) 0)
struct trace_info	* ti_cpy();

	/* THIS SECTION GIVES VARIOUS COMMON STRUCTURES */

typedef struct svccontrol {     /* represents ServiceControls           */
    int         svc_options;
#define SVC_OPT_PREFERCHAIN             0X001
#define SVC_OPT_CHAININGPROHIBIT        0X002
#define SVC_OPT_LOCALSCOPE              0X004
#define SVC_OPT_DONTUSECOPY             0X008
#define SVC_OPT_DONTDEREFERENCEALIAS    0X010
    int        svc_prio;
#define SVC_PRIO_LOW    0
#define SVC_PRIO_MED    1
#define SVC_PRIO_HIGH   2
    int         svc_timelimit;  /* time limit in second                 */
#define SVC_NOTIMELIMIT -1
    int         svc_sizelimit;
#define SVC_NOSIZELIMIT -1
    int         svc_scopeofreferral;    /* Parameter for DSP only       */
#define SVC_REFSCOPE_NONE       -1
#define SVC_REFSCOPE_DMD        0
#define SVC_REFSCOPE_COUNTRY    1
    char * 	svc_tmp;	/* pepsy */
    int 	svc_len;	/* pepsy */
} svccontrol, ServiceControl;

#endif
