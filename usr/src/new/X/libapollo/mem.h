/* $Header: mem.h,v 10.1 86/11/29 13:53:41 jg Rel $ */
/* $Header: mem.h,v 10.1 86/11/29 13:53:41 jg Rel $ */
/* mem.h	Data structure for maintaining memory allocation of vs100
 *		framebuffer memory.
 *
 * Author:	Paul J. Asente
 * 		Digital Equipment Corporation
 * 		Western Reseach Lab
 * Date:	June 1983
 */

/****************************************************************************
 *									    *
 *  Copyright (c) 1983, 1984 by						    *
 *  DIGITAL EQUIPMENT CORPORATION, Maynard, Massachusetts.		    *
 *  All rights reserved.						    *
 * 									    *
 *  This software is furnished on an as-is basis and may be used and copied *
 *  only with inclusion of the above copyright notice. This software or any *
 *  other copies thereof may be provided or otherwise made available to     *
 *  others only for non-commercial purposes.  No title to or ownership of   *
 *  the software is hereby transferred.					    *
 * 									    *
 *  The information in this software is  subject to change without notice   *
 *  and  should  not  be  construed as  a commitment by DIGITAL EQUIPMENT   *
 *  CORPORATION.							    *
 * 									    *
 *  DIGITAL assumes no responsibility for the use  or  reliability of its   *
 *  software on equipment which is not supplied by DIGITAL.		    *
 * 									    *
 *									    *
 ****************************************************************************/

#include "param.h"

typedef struct _VSArea {
	struct _VSArea *next;
	struct _VSArea *prev;
	caddr_t vsPtr;
	struct {
	    unsigned int _vsFreeFlag : 1;
	    int _vsSize : 31;
	} s;
	union {
	    struct {
		struct _VSArea *next;
		struct _VSArea *prev;
	    } _vsFree;
	    int _vsType;
	} u;
} VSArea;

#define vsFreeFlag s._vsFreeFlag
#define vsSize s._vsSize
#define vsFree u._vsFree
#define vsType u._vsType

#define VS_FREE	0
#define VS_INUSE 1

#define BITMAP_TYPE 1
#define HALFTONE_TYPE 2
#define FONT_TYPE 3
