/* name.h - */

/*
 * $Header: /f/osi/h/quipu/RCS/name.h,v 7.1 91/02/22 09:26:00 mrose Interim $
 *
 *
 * $Log:	name.h,v $
 * Revision 7.1  91/02/22  09:26:00  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:56:40  mrose
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


#ifndef QUIPUNAME
#define QUIPUNAME

#include "quipu/attr.h"

typedef struct ava {            /* represents AttributeValueAssertion */
    AttributeType ava_type;
    AttributeValue ava_value;
}ava, AVA;

typedef struct rdncomp {        /* RDN is sequence of attribute value   */
				/* assertions                           */
				/* represents RelativeDistinguishedName */
    attrType       	rdn_at;
    attrVal      	rdn_av;
    struct rdncomp      *rdn_next;
} rdncomp, *RDN;

#define NULLRDN ((RDN) 0)
#define rdn_comp_alloc()          (RDN) smalloc(sizeof(rdncomp))
RDN  rdn_comp_new ();
RDN  rdn_comp_cpy ();
RDN  str2rdn();
RDN  rdn_cpy ();
RDN  rdn_merge ();

typedef struct dncomp {         /* DN is sequence of RDNs.              */
				/* represents RDNSequence which is      */
				/* equivalent to DistinguishedName      */
    RDN                 dn_rdn;
    struct dncomp       *dn_parent;
} dncomp, *DN;

#define NULLDN ((DN) 0)

#define dn_comp_alloc()        (DN) smalloc(sizeof(dncomp))
#define dn_comp_print(x,y,z)   if (y!=NULLDN) rdn_print(x,y->dn_rdn,z)
#define dn_comp_fill(x,y)     x -> dn_rdn = y
#define dn_comp_cmp(x,y)      ((rdn_cmp (x->dn_rdn ,y->dn_rdn) == OK) ? OK : NOTOK )

DN  dn_comp_new ();
DN  dn_comp_cpy ();
DN  dn_cpy ();
DN  str2dn ();

#endif
