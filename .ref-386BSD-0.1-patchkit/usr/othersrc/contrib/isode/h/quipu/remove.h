/* remove.h - */

/*
 * $Header: /f/osi/h/quipu/RCS/remove.h,v 7.1 91/02/22 09:26:06 mrose Interim $
 *
 *
 * $Log:	remove.h,v $
 * Revision 7.1  91/02/22  09:26:06  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:56:43  mrose
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


#ifndef QUIPUREMOVE
#define QUIPUREMOVE

#include "quipu/commonarg.h"
#include "quipu/ds_error.h"
#include "quipu/dap.h"

struct ds_removeentry_arg {
    CommonArgs rma_common;
    DN rma_object;
};

#endif
