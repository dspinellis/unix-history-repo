/* ufn.h - user-friendly naming routines */

/*
 * $Header: /f/osi/h/quipu/RCS/ufn.h,v 7.1 91/02/22 09:26:09 mrose Interim $
 *
 *
 * $Log:	ufn.h,v $
 * Revision 7.1  91/02/22  09:26:09  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/06/11  09:57:41  mrose
 * *** empty log message ***
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#ifndef _QUIPUUFN_
#define _QUIPUUFN_

#include "quipu/util.h"
#include "quipu/name.h"
#include "quipu/entry.h"

typedef struct dn_seq * DNS;
#define NULLDNS ((struct dn_seq *) NULL)

typedef struct _envlist {
	DNS	Dns;
	int	Upper;
	int	Lower;
	struct  _envlist * Next;
} * envlist;
#define NULLEL ((envlist) NULL)
envlist	read_envlist ();

extern char ufn_notify;

extern int ufn_flags;
#define	UFN_NULL	0x00
#define	UFN_APPROX	0x01
#define	UFN_WILDHEAD	0x02
#define	UFN_ALL	(UFN_APPROX | UFN_WILDHEAD)

int	ufn_init ();
int	ufn_match ();

#endif
