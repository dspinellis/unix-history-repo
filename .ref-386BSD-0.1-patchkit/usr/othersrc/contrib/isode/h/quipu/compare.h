/* compare.h - */

/*
 * $Header: /f/osi/h/quipu/RCS/compare.h,v 7.1 91/02/22 09:25:31 mrose Interim $
 *
 *
 * $Log:	compare.h,v $
 * Revision 7.1  91/02/22  09:25:31  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:56:26  mrose
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


#ifndef QUIPUCOMP
#define QUIPUCOMP

#include "quipu/commonarg.h"
#include "quipu/ds_error.h"
#include "quipu/dap.h"

struct ds_compare_arg {
    CommonArgs cma_common;
    DN cma_object;
    AVA cma_purported;
};

struct ds_compare_result {
    CommonResults cmr_common;
    DN cmr_object;
    char cmr_matched;           /* set to TRUE or FALSE                 */
    char cmr_iscopy;            /* values defined in entrystruct        */
    char cmr_pepsycopy;
    time_t cmr_age;
};

#endif
