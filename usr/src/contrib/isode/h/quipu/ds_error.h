/* ds_error.h - directory service errors */

/*
 * $Header: /f/osi/h/quipu/RCS/ds_error.h,v 7.1 91/02/22 09:25:44 mrose Interim $
 *
 *
 * $Log:	ds_error.h,v $
 * Revision 7.1  91/02/22  09:25:44  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:56:30  mrose
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


#ifndef DSERRORH
#define DSERRORH

#include "quipu/dsp.h"

struct DSE_abandon_fail {
    int DSE_ab_problem;
#define DSE_AB_NOSUCHOPERATION  1
#define DSE_AB_TOOLATE          2
#define DSE_AB_CANNOTABANDON    3
    int DSE_ab_invokeid;
};

struct DSE_at_problem {
    int         DSE_at_what;
#define DSE_AT_NOSUCHATTRIBUTE          1
#define DSE_AT_INVALIDATTRIBUTESYNTAX   2
#define DSE_AT_UNDEFINEDATTRIBUTETYPE   3
#define DSE_AT_INAPPROPRIATEMATCHING    4
#define DSE_AT_CONSTRAINTVIOLATION      5
#define DSE_AT_TYPEORVALUEEXISTS        6
    AttributeType DSE_at_type;
    AttributeValue DSE_at_value;
    struct DSE_at_problem *dse_at_next;
};
#define DSE_AT_NOPROBLEM ((struct DSE_at_problem*)0)

struct DSE_attribute {
    DN DSE_at_name;
    struct DSE_at_problem DSE_at_plist;
} ;


struct DSE_name {
    int DSE_na_problem;
#define DSE_NA_NOSUCHOBJECT             1
#define DSE_NA_ALIASPROBLEM             2
#define DSE_NA_INVALIDATTRIBUTESYNTAX   3
#define DSE_NA_ALIASDEREFERENCE         4
    DN DSE_na_matched;
};


struct DSE_referral {
    ContinuationRef DSE_ref_candidates;
    DN                  DSE_ref_prefix;
			/* Context prefix only in DSP           */
};

struct DSE_security {
    int DSE_sc_problem;
#define DSE_SC_AUTHENTICATION           1
#define DSE_SC_INVALIDCREDENTIALS       2
#define DSE_SC_ACCESSRIGHTS             3
#define DSE_SC_INVALIDSIGNATURE         4
#define DSE_SC_PROTECTIONREQUIRED       5
#define DSE_SC_NOINFORMATION		6
};

struct DSE_service {
    int DSE_sv_problem;
#define DSE_SV_BUSY                     1
#define DSE_SV_UNAVAILABLE              2
#define DSE_SV_UNWILLINGTOPERFORM       3
#define DSE_SV_CHAININGREQUIRED         4
#define DSE_SV_UNABLETOPROCEED          5
#define DSE_SV_INVALIDREFERENCE         6       /* DSP ONLY */
#define DSE_SV_TIMELIMITEXCEEDED        7
#define DSE_SV_ADMINLIMITEXCEEDED	8
#define DSE_SV_LOOPDETECT               9
#define DSE_SV_UNAVAILABLECRITICALEXTENSION	10
#define DSE_SV_OUTOFSCOPE		11
#define DSE_SV_DITERROR			12
};

struct DSE_update {
    int DSE_up_problem;
#define DSE_UP_NAMINGVIOLATION          1
#define DSE_UP_OBJECTCLASSVIOLATION     2
#define DSE_UP_NOTONNONLEAF             3
#define DSE_UP_NOTONRDN                 4
#define DSE_UP_ALREADYEXISTS            5
#define DSE_UP_AFFECTSMULTIPLEDSAS      6
#define DSE_UP_NOOBJECTCLASSMODS        7
};

struct DSError {
    int dse_type;
#define DSE_INTR_ABANDON_FAILED	-5	/* Call interrupted - abandon failed */
#define DSE_INTR_ABANDONED	-4	/* Call interrupted - abandoned */
#define DSE_INTRERROR		-3	/* Call interrupted */
#define DSE_LOCALERROR		-2	/* Error in DUA */
#define DSE_REMOTEERROR		-1	/* Problem with DSA */
#define DSE_NOERROR             0
#define DSE_ATTRIBUTEERROR      1
#define DSE_NAMEERROR           2
#define DSE_SERVICEERROR        3
#define DSE_REFERRAL            4
#define DSE_ABANDONED           5
			/* Abandoned does not have any parameter and    */
			/* so there is no struct for this value         */
#define DSE_SECURITYERROR       6
#define DSE_ABANDON_FAILED      7
#define DSE_UPDATEERROR         8
#define DSE_DSAREFERRAL		9
#define ds_recog_err(a) ((a >= DSE_ATTRIBUTEERROR) && (a <= DSE_DSAREFERRAL))
    union {
       struct DSE_attribute dse_un_attribute;
       struct DSE_name dse_un_name;
       struct DSE_service dse_un_service;
       struct DSE_referral dse_un_referral;
       struct DSE_security dse_un_security;
       struct DSE_abandon_fail dse_un_abandon_fail;
       struct DSE_update dse_un_update;
    }   dse_un;
};



	/* THIS SECTION DEFINES THE PROCEDURE CALLS */

/* All of the DUA calls are SYNCHRONOUS, with no access to referrals    */
/* A DUA can access lower level hooks if if needs to be more clever     */
/* Each call has a single structure fore each direction, as defined     */

/*
All routines return integer values with the following
possible values
*/

#define DS_OK  0                /* Success                              */
#define DS_ERROR_LOCAL -1       /* Error within the DUA module          */

#define DS_ERROR_CONNECT -2     /* Failed to connect to a remote DSA    */
#define DS_ERROR_PROVIDER -3    /* Other OSI provider error             */
#define DS_X500_ERROR	-4	/* Synonym for remote error */
#define DS_ERROR_REMOTE -4      /* Remote error.  Further details will  */
				/* be in the error parameter            */

#define DS_CONTINUE -5		/* operation not finished... continuing */
#define DS_SUSPEND -6		/* operation has deliberately suspended */
#define DS_ERROR_ABANDONED -7	/* RoIntr caused succesful ds_abandon */
#define DS_ERROR_ABANDON_FAILED -8	/* RoIntr caused unsuccesful ds_abandon */

#define ERR_ABANDON_FAIL dse_un.dse_un_abandon_fail
#define ERR_ATTRIBUTE    dse_un.dse_un_attribute
#define ERR_NAME         dse_un.dse_un_name
#define ERR_REFERRAL     dse_un.dse_un_referral
#define ERR_SECURITY     dse_un.dse_un_security
#define ERR_SERVICE      dse_un.dse_un_service
#define ERR_UPDATE       dse_un.dse_un_update
#define ERR_ALIAS        dse_un.dse_un_alias

#endif
