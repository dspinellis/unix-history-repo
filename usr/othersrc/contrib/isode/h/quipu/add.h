/* add.h - */

/*
 * $Header: /f/osi/h/quipu/RCS/add.h,v 7.1 91/02/22 09:25:22 mrose Interim $
 *
 *
 * $Log:	add.h,v $
 * Revision 7.1  91/02/22  09:25:22  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:56:20  mrose
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


#ifndef QUIPUADD
#define QUIPUADD

#include "quipu/commonarg.h"
#include "quipu/ds_error.h"
#include "quipu/dap.h"

struct ds_addentry_arg {
    CommonArgs ada_common;
    DN ada_object;
    Attr_Sequence ada_entry;
};

#endif
