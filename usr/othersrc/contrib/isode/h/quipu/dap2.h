/* dap.h - */

/* 
 * $Header: /f/osi/h/quipu/RCS/dap2.h,v 7.2 91/02/22 09:25:42 mrose Interim $
 *
 *
 * $Log:	dap2.h,v $
 * Revision 7.2  91/02/22  09:25:42  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:46:22  mrose
 * sync
 * 
 * Revision 7.0  90/07/27  05:02:57  mrose
 * *** empty log message ***
 * 
 * Revision 7.0  89/11/23  21:56:29  mrose
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


#ifndef QUIPUDAP2
#define QUIPUDAP2

#ifndef	_MANIFEST_
#include "manifest.h"
#endif
#ifndef	_GENERAL_
#include "general.h"
#endif

#ifndef	_RoSAP_
#include "rosap.h"		/* definitions for ROSE-USERs */
#endif

#ifndef	_RoNot_
#include "ronot.h"		/* definitions for RoBIND-USERs */
#endif

#include "quipu/common.h"
#include "quipu/dsargument.h"

/*  */

/* An additional value for "asyn" parameters for interruptibility */
#define ROS_INTR	-1

/*  */

struct DAPconnect {
    int	    dc_sd;		/* association descriptor */

    int     ds_pctx_id;		/* Context identifier of directory access AS */

    struct AcSAPconnect dc_connect;/* info from A-CONNECT.CONFIRMATION */

    int	    dc_result;		/* result */
#define DC_RESULT	1
#define DC_ERROR	2
#define DC_REJECT	3

    union {
	struct ds_bind_arg	  dc_bind_res;	/* DC_RESULT */
	struct ds_bind_error	  dc_bind_err;	/* DC_ERROR */
    } dc_un;
};
#ifndef DCFREE
#define	DCFREE(dc) { \
    ACCFREE (&((dc) -> dc_connect)); \
    switch ((dc) -> dc_result) { \
	case DC_RESULT: \
	    bind_arg_free (&(dc) -> dc_un.dc_bind_res); \
	    break; \
	case DC_ERROR: \
	    break; \
	default: \
	    break; \
    } \
}
#endif

struct DAPrelease {		/* DAP-UNBIND.CONFIRMATION */
    int	    dr_affirmative;	/* T   = connection released
				   NIL = request refused */
    
    int	    dr_reason;		/* reason for result */
				/* Takes values from acr_reason */
};

struct DAPabort {		/* DAP-ABORT.INDICATION */
    int	    da_source;		/* abort source */
#define	DA_USER		0	/*   DAP-user */
#define	DA_PROVIDER	1	/*   DAP-provider */
#define	DA_LOCAL	2	/*   DAP interface internal error */

    int	    da_reason;		/* reason for failure */
#define DA_NO_REASON	0	/* Ain't no reason to some things */
#define DA_RONOT	1	/* Error from RONOT provider */
#define DA_ROS		2	/* Error from ROS provider called */
#define DA_ARG_ENC 	3	/* Error encoding argument */
#define DA_RES_ENC 	4	/* Error encoding result */
#define DA_ERR_ENC 	5	/* Error encoding error */
#define DA_ARG_DEC 	6	/* Error decoding argument */
#define DA_RES_DEC	7	/* Error decoding result */
#define DA_ERR_DEC 	8	/* Error decoding error */

				/* diagnostics from provider */
#define	DA_SIZE	512
    int	    da_cc;		/*   length */
    char    da_data[DA_SIZE];	/*   data */
};

struct DAPresult {
    int			  dr_id;
    struct DSResult	  dr_res;	/* Decoded result and op type */
};
#ifndef DRFREE
#define DRFREE(dr) \
    ds_res_free (&((dr)->dr_res))
#endif

struct DAPerror {
    int			  de_id;
    struct DSError	  de_err;	/* Decoded error and error type */
};
#ifndef DEFREE
#define DEFREE(de) \
    ds_error_free (&((de)->de_err))
#endif

struct DAPpreject {
    int		  dp_id;	/* Operation id or -1 */

    int		  dp_source;	/* same values as DAPabort.da_source */

    int		  dp_reason;	/* reason for failure */
#define DP_NO_REASON	0	/* Ain't no reason to some things */
#define DP_ROS		1	/* ROSE error */
#define DP_INVOKE	2	/* Failure during invocation */
#define DP_RESULT	3	/* Failure during result */
#define DP_ERROR	4	/* Failure during error */

				/* diagnostics from provider */
#define	DP_SIZE	512
    int	    dp_cc;		/*   length */
    char    dp_data[DP_SIZE];	/*   data */
};

struct DAPindication {
    int	    di_type;
#define DI_RESULT	2	/* DAP operation result received */
#define DI_ERROR	3	/* DAP operation error received */
#define DI_PREJECT	4	/* DAP operation rejected */
#define	DI_ABORT	6	/* DAP association lost */
    union {
	struct DAPresult	di_un_result;
	struct DAPerror		di_un_error;
	struct DAPpreject	di_un_preject;
	struct DAPabort		di_un_abort;
    } di_un;
#define di_result di_un.di_un_result
#define di_error di_un.di_un_error
#define di_preject di_un.di_un_preject
#define di_abort di_un.di_un_abort
};


#ifndef	lint
#ifndef	__STDC__
#define	copyDAPdata(base,len,d) \
{ \
    register int i = len; \
    if ((d -> d/* */_cc = min (i, sizeof d -> d/* */_data)) > 0) \
	bcopy (base, d -> d/* */_data, d -> d/* */_cc); \
}
#else
#define	copyDAPdata(base,len,d) \
{ \
    register int i = len; \
    if ((d -> d##_cc = min (i, sizeof d -> d##_data)) > 0) \
	bcopy (base, d -> d##_data, d -> d##_cc); \
}
#endif
#else
#define	copyDAPdata(base,len,d)	bcopy (base, (char *) d, len)
#endif

/*  */

int	DapAsynBindReqAux ();	/* DAP-BIND.REQUEST (ARGUMENT) */
int	DapAsynBindRequest ();	/* DAP-BIND.REQUEST (ARGUMENT) */
int	DapAsynBindRetry();	/* DAP-BIND-RETRY.REQUEST */

int	DapUnBindRequest ();	/* DAP-UNBIND.REQUEST */
int	DapUnBindRetry ();	/* DAP-BIND-RETRY.REQUEST (pseudo) */

char   *DapErrString ();	/* return DAP error code in string form */

#endif
