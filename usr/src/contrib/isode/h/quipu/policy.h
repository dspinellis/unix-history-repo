/* policy.h - representation of security policy */

/* 
 * $Header: /f/osi/h/quipu/RCS/policy.h,v 7.1 91/02/22 09:26:04 mrose Interim $
 *
 *
 * $Log:	policy.h,v $
 * Revision 7.1  91/02/22  09:26:04  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/08/24  12:11:24  mrose
 * *** empty log message ***
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
#ifndef QUIPUPOLICY
#define QUIPUPOLICY
#include "psap.h"

struct security_policy {
        OID oid;
        int p_type;
#define POLICY_PARM_ABSENT      0
#define POLICY_PARM_UNKNOWN     1
#define POLICY_PARM_NUMERIC     2
#define POLICY_PARM_ACCESS      3
        union {
                int numeric;
		unsigned access;
        } un;
};

#define NULLPOLICY ((struct security_policy *) 0)

#define POLICY_ACCESS_DETECT	1
#define POLICY_ACCESS_READ 	2
#define POLICY_ACCESS_ADD	4
#define POLICY_ACCESS_WRITE	8
#define POLICY_ACCESS_ALL	15

#endif
