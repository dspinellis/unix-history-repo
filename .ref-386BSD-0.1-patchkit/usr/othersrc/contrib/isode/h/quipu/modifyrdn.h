/* modifyrdn.h - */

/*
 * $Header: /f/osi/h/quipu/RCS/modifyrdn.h,v 7.1 91/02/22 09:25:59 mrose Interim $
 *
 *
 * $Log:	modifyrdn.h,v $
 * Revision 7.1  91/02/22  09:25:59  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:56:39  mrose
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


#ifndef QUIPUMODRDN
#define QUIPUMODRDN

#include "quipu/commonarg.h"
#include "quipu/ds_error.h"
#include "quipu/dap.h"

struct ds_modifyrdn_arg {
    CommonArgs mra_common;
    DN mra_object;
    RDN mra_newrdn;
    char deleterdn;             /* set to TRUE or FLASE                 */
};

#endif
