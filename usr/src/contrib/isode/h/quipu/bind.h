/* bind.h - bind parameters */

/*
 * $Header: /f/osi/h/quipu/RCS/bind.h,v 7.2 91/02/22 09:25:27 mrose Interim $
 *
 *
 * $Log:	bind.h,v $
 * Revision 7.2  91/02/22  09:25:27  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/20  15:30:24  mrose
 * cjr
 * 
 * Revision 7.0  89/11/23  21:56:24  mrose
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


#ifndef QUIPUBIND
#define QUIPUBIND
#include "quipu/name.h"
#include "quipu/authen.h"

	/* THIS SECTION GIVES BIND PARAMETERS */

/* Directory Bind Argument */

struct ds_bind_arg {
    int dba_version;                    /* Treat INT as bitstring */
#define DBA_VERSION_V1988  0
    int dba_auth_type;
#define DBA_AUTH_NONE      0
#define DBA_AUTH_SIMPLE    1
#define DBA_AUTH_STRONG    2
#define DBA_AUTH_EXTERNAL  3
#define DBA_AUTH_PROTECTED 4	/* special case of simple */
    char *dba_time1;			/* Timestamps for protected    */
    char *dba_time2;			/* simple authentication.      */
    struct random_number dba_r1;
    struct random_number dba_r2;
    DN dba_dn;
    int dba_passwd_len;                 /* len = 0 means no password    */
#define DBA_MAX_PASSWD_LEN 512
    char dba_passwd[DBA_MAX_PASSWD_LEN];
    struct signature *dba_sig;		/* signature for strong authen	*/
    struct certificate_list *dba_cpath;	/* certification path		*/
    char * dba_vtmp;	/* pepsy */
    int    dba_vlen;    /* pepsy */
};

struct ds_bind_error {
    int dbe_version;
    int dbe_type;
#define DBE_TYPE_SERVICE 1
#define DBE_TYPE_SECURITY 2
    int dbe_value;                      /* takes on values as define in */
					/* DSE_service or DSE_security  */
					/* according to dbe_type        */
    char * dbe_vtmp;	/* pepsy */
    int    dbe_vlen;    /* pepsy */
};

#endif
