/* modify.h - */

/*
 * $Header: /f/osi/h/quipu/RCS/modify.h,v 7.2 91/02/22 09:25:58 mrose Interim $
 *
 *
 * $Log:	modify.h,v $
 * Revision 7.2  91/02/22  09:25:58  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/20  15:30:51  mrose
 * cjr
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


#ifndef QUIPUMOD
#define QUIPUMOD

#include "quipu/commonarg.h"
#include "quipu/ds_error.h"
#include "quipu/dap.h"

struct entrymod {
    int em_type;
#define EM_ADDATTRIBUTE         1
#define EM_REMOVEATTRIBUTE      2
#define EM_ADDVALUES            3
#define EM_REMOVEVALUES         4
    Attr_Sequence em_what;      /* holds a single attribute.  the values */
				/* ignored for remove attribute          */
    struct entrymod *em_next;
};

#define NULLMOD (struct entrymod *)NULL
#define em_alloc() (struct entrymod *) smalloc (sizeof (struct entrymod))

struct ds_modifyentry_arg {
    CommonArgs mea_common;
    DN mea_object;
    struct entrymod *mea_changes;
};

#endif
