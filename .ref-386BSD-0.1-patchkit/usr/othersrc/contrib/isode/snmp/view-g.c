/* view-g.c - VIEW group */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/view-g.c,v 7.2 91/02/22 09:44:59 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/view-g.c,v 7.2 91/02/22 09:44:59 mrose Interim $
 *
 *
 * $Log:	view-g.c,v $
 * Revision 7.2  91/02/22  09:44:59  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/12/18  10:14:28  mrose
 * update
 * 
 * Revision 7.0  90/12/17  22:07:58  mrose
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


#include <stdio.h>
#include "mib.h"
#include "view-g.h"
#include "tailor.h"

/*    VIEW GROUP */

#define	viewPrimName	  0
#define	viewPrimTDomain   1
#define	viewPrimTAddr	  2
#define	viewPrimUser	  3
#define	viewPrimCommunity 4
#define	viewPrimType	  5

#define	P_VALID		  1		/* viewPrimType */


static int	viewmask = 0x1;
static OID	localAgent = NULLOID;
static OID	rfc1157Domain = NULLOID;

struct view *get_prent ();


static int  o_viewPrim (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifvar;
    register int    i;
    register unsigned int *ip,
			  *jp;
    register struct view *vu;
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;

    ifvar = (int) ot -> ot_info;
    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    if (oid -> oid_nelem <= ot -> ot_name -> oid_nelem)
		return int_SNMP_error__status_noSuchName;
	    if ((vu = get_prent (oid -> oid_elements
				     + ot -> ot_name -> oid_nelem,
				 oid -> oid_nelem
				     - ot -> ot_name -> oid_nelem, 0)) == NULL)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		OID	new;

		if ((vu = VHead -> v_forw) == VHead)
		    return NOTOK;

		if ((new = oid_extend (oid, vu -> v_insize)) == NULLOID)
		    return NOTOK;
		ip = new -> oid_elements + new -> oid_nelem - vu -> v_insize;
		jp = vu -> v_instance;
		for (i = vu -> v_insize; i > 0; i--)
		    *ip++ = *jp++;

		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;
	    }
	    else {
		int	j;

		if ((vu = get_prent (oid -> oid_elements
				         + ot -> ot_name -> oid_nelem,
				     j = oid -> oid_nelem
				     	     - ot -> ot_name -> oid_nelem, 1))
		         == NULL)
		    return NOTOK;

		if ((i = j - vu -> v_insize) < 0) {
		    OID	    new;

		    if ((new = oid_extend (oid, -i)) == NULLOID)
			return NOTOK;
		    if (v -> name)
			free_SNMP_ObjectName (v -> name);
		    v -> name = new;

		    oid = new;
		}
		else
		    if (i > 0)
			oid -> oid_nelem -= i;

		ip = oid -> oid_elements + ot -> ot_name -> oid_nelem;
		jp = vu -> v_instance;
		for (i = vu -> v_insize; i > 0; i--)
		    *ip++ = *jp++;
	    }
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    switch (ifvar) {
	case viewPrimName:
	    return o_specific (oi, v, (caddr_t) vu -> v_name);

	case viewPrimTDomain:
		return o_specific (oi, v,
				   (caddr_t) (vu -> v_community ? rfc1157Domain
					      		        : localAgent));

	case viewPrimTAddr:
#ifdef	TCP
	    if (vu -> v_community) {
		struct sockaddr_in *sin = (struct sockaddr_in *) &vu -> v_sa;

		return o_string (oi, v, (char *) &sin -> sin_addr, 4);
	    }
	    else
#endif
		return o_string (oi, v, NULLCP, 0);

	case viewPrimUser:
	case viewPrimCommunity:
	    if (vu -> v_community)
		return o_qbstring (oi, v, vu -> v_community);
	    else
		return o_string (oi, v, NULLCP, 0);

	case viewPrimType:
	    return o_integer (oi, v, P_VALID);

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

static struct view *get_prent (ip, len, isnext)
register unsigned int *ip;
int	len;
int	isnext;
{
    register struct view *v;

    for (v = VHead -> v_forw; v != VHead; v = v -> v_forw)
	switch (elem_cmp (v -> v_instance, v -> v_insize, ip, len)) {
	    case 0:
	        if (!isnext)
		    return v;
		if ((v = v -> v_forw) == VHead)
		    return NULL;
		/* else fall... */

	    case 1:
		return (isnext ? v : NULL);
	}

    return NULL;
}

/*  */

#define	viewAclView	  0
#define	viewAclCommunity  1
#define	viewAclUser	  2
#define	viewAclPrivileges 3
#define	viewAclType	  4

#define	A_VALID		  1		/* viewAclType */


static struct community *CLex = NULL;

struct community *get_acent ();


static int  o_viewAcl (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifvar;
    register int    i;
    register unsigned int *ip,
			  *jp;
    register struct community *c;
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;

    ifvar = (int) ot -> ot_info;
    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    if (oid -> oid_nelem <= ot -> ot_name -> oid_nelem)
		return int_SNMP_error__status_noSuchName;
	    if ((c = get_acent (oid -> oid_elements
				    + ot -> ot_name -> oid_nelem,
				oid -> oid_nelem
				    - ot -> ot_name -> oid_nelem, 0)) == NULL)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		OID	new;

		if ((c = CLex) == NULL)
		    return NOTOK;

		if ((new = oid_extend (oid, c -> c_insize)) == NULLOID)
		    return NOTOK;
		ip = new -> oid_elements + new -> oid_nelem - c -> c_insize;
		jp = c -> c_instance;
		for (i = c -> c_insize; i > 0; i--)
		    *ip++ = *jp++;

		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;
	    }
	    else {
		int	j;

		if ((c = get_acent (oid -> oid_elements
				        + ot -> ot_name -> oid_nelem,
				    j = oid -> oid_nelem
				     	    - ot -> ot_name -> oid_nelem, 1))
		         == NULL)
		    return NOTOK;

		if ((i = j - c -> c_insize) < 0) {
		    OID	    new;

		    if ((new = oid_extend (oid, -i)) == NULLOID)
			return NOTOK;
		    if (v -> name)
			free_SNMP_ObjectName (v -> name);
		    v -> name = new;

		    oid = new;
		}
		else
		    if (i > 0)
			oid -> oid_nelem -= i;

		ip = oid -> oid_elements + ot -> ot_name -> oid_nelem;
		jp = c -> c_instance;
		for (i = c -> c_insize; i > 0; i--)
		    *ip++ = *jp++;
	    }
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    switch (ifvar) {
	case viewAclView:
	    return o_specific (oi, v, (caddr_t) c -> c_view -> v_name);

	case viewAclCommunity:
	case viewAclUser:
	    return o_string (oi, v, c -> c_name, strlen (c -> c_name));

	case viewAclPrivileges:
	    return o_integer (oi, v,
			        ((c -> c_permission & OT_RDONLY) ? 3 : 0)
			      + ((c -> c_permission & OT_WRONLY) ? 8 : 0)
			      + ((c -> c_permission & OT_YYY) ? 4 : 0));

	case viewAclType:
	    return o_integer (oi, v, A_VALID);

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

static struct community *get_acent (ip, len, isnext)
register unsigned int *ip;
int	len;
int	isnext;
{
    register struct community *c;

    for (c = CLex; c; c = c -> c_next)
	switch (elem_cmp (c -> c_instance, c -> c_insize, ip, len)) {
	    case 0:
	        return (isnext ? c -> c_next : c);

	    case 1:
		return (isnext ? c : NULL);
	}

    return NULL;
}

/*  */

#define	viewTrapView	  0
#define	viewTrapGenerics  1
#define	viewTrapSpecifics 2
#define	viewTrapType      3

#define	T_VALID		  1		/* viewTrapType */


static OID    trapview = NULLOID;

struct trap *get_trent ();


static int  o_viewTrap (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifvar;
    register int    i;
    register unsigned int *ip,
			  *jp;
    register struct trap *t;
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;

    ifvar = (int) ot -> ot_info;
    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    if (oid -> oid_nelem <= ot -> ot_name -> oid_nelem)
		return int_SNMP_error__status_noSuchName;
	    if ((t = get_trent (oid -> oid_elements
				    + ot -> ot_name -> oid_nelem,
				oid -> oid_nelem
				    - ot -> ot_name -> oid_nelem, 0)) == NULL)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		OID	new;

		if ((t = UHead -> t_forw) == UHead)
		    return NOTOK;

		if ((new = oid_extend (oid, t -> t_insize)) == NULLOID)
		    return NOTOK;
		ip = new -> oid_elements + new -> oid_nelem - t -> t_insize;
		jp = t -> t_instance;
		for (i = t -> t_insize; i > 0; i--)
		    *ip++ = *jp++;

		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;
	    }
	    else {
		int	j;

		if ((t = get_trent (oid -> oid_elements
				        + ot -> ot_name -> oid_nelem,
				    j = oid -> oid_nelem
				     	    - ot -> ot_name -> oid_nelem, 1))
		         == NULL)
		    return NOTOK;

		if ((i = j - t -> t_insize) < 0) {
		    OID	    new;

		    if ((new = oid_extend (oid, -i)) == NULLOID)
			return NOTOK;
		    if (v -> name)
			free_SNMP_ObjectName (v -> name);
		    v -> name = new;

		    oid = new;
		}
		else
		    if (i > 0)
			oid -> oid_nelem -= i;

		ip = oid -> oid_elements + ot -> ot_name -> oid_nelem;
		jp = t -> t_instance;
		for (i = t -> t_insize; i > 0; i--)
		    *ip++ = *jp++;
	    }
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    switch (ifvar) {
	case viewTrapView:
	    return o_specific (oi, v, (caddr_t) t -> t_view -> v_name);

	case viewTrapGenerics:
	    {
		char   c = t -> t_generics & 0xff;
		
		return o_string (oi, v, &c, sizeof c);
	    }

	case viewTrapSpecifics:
	    return o_string (oi, v, NULLCP, 0);

	case viewTrapType:
	    return o_integer (oi, v, T_VALID);

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

static struct trap *get_trent (ip, len, isnext)
register unsigned int *ip;
int	len;
int	isnext;
{
    register struct trap *t;

    for (t = UHead -> t_forw; t != UHead; t = t -> t_forw)
	switch (elem_cmp (t -> t_instance, t -> t_insize, ip, len)) {
	    case 0:
	        if (!isnext)
		    return t;
		if ((t = t -> t_forw) == UHead)
		    return NULL;
		/* else fall... */

	    case 1:
		return (isnext ? t : NULL);
	}

    return NULL;
}

/*  */

static int  view_compar (a, b)
struct view **a,
            **b;
{
    return elem_cmp ((*a) -> v_instance, (*a) -> v_insize,
		     (*b) -> v_instance, (*b) -> v_insize);
}

static int  comm_compar (a, b)
struct community **a,
                 **b;
{
    return elem_cmp ((*a) -> c_instance, (*a) -> c_insize,
		     (*b) -> c_instance, (*b) -> c_insize);
}

static int  trap_compar (a, b)
struct trap **a,
            **b;
{
    return elem_cmp ((*a) -> t_instance, (*a) -> t_insize,
		     (*b) -> t_instance, (*b) -> t_insize);
}

/*  */

static struct wired {
    char  *w_args1;
    char  *w_args2;
}	wired[] = {
    "defViewWholeRW", NULL,
    "defViewWholeRO", NULL,
    "defViewStandardRW", "mgmt",
    "defViewStandardRO", "mgmt",

    NULL
};

/*  */

init_view () {
    char	buffer[BUFSIZ],
	       *vec[4];
    register OT	    ot;
    register struct wired *w;

    CHead -> c_forw = CHead -> c_back = CHead;
    UHead -> t_forw = UHead -> t_back = UHead;
    VHead -> v_forw = VHead -> v_back = VHead;

    vec[0] = "view";
    for (w = wired; w -> w_args1; w++) {
	vec[1] = w -> w_args1;
	if (vec[2] = w -> w_args2)
	    vec[3] = NULL;

	if (f_view (vec) == NOTOK)
	    adios (NULLCP, "you lose");
    }

    (void) strcpy (buffer, "defViewTrapDest.0");
    if ((trapview = text2oid (buffer)) == NULLOID)
	adios (NULLCP, "unknown OID \"defViewTrapDest.0\" for traps");
    trapview -> oid_nelem--;

    if ((localAgent = text2oid ("localAgent")) == NULLOID)
	adios (NULLCP, "unknown OID \"localAgent\"");
    if ((rfc1157Domain = text2oid ("rfc1157Domain")) == NULLOID)
	adios (NULLCP, "unknown OID \"rfc1157Domain\"");

    if (ot = text2obj ("viewPrimName"))
	ot -> ot_getfnx = o_viewPrim,
	ot -> ot_info = (caddr_t) viewPrimName;
    if (ot = text2obj ("viewPrimTDomain"))
	ot -> ot_getfnx = o_viewPrim,
	ot -> ot_info = (caddr_t) viewPrimTDomain;
    if (ot = text2obj ("viewPrimTAddr"))
	ot -> ot_getfnx = o_viewPrim,
	ot -> ot_info = (caddr_t) viewPrimTAddr;
    if (ot = text2obj ("viewPrimUser"))
	ot -> ot_getfnx = o_viewPrim,
	ot -> ot_info = (caddr_t) viewPrimUser;
    if (ot = text2obj ("viewPrimCommunity"))
	ot -> ot_getfnx = o_viewPrim,
	ot -> ot_info = (caddr_t) viewPrimCommunity;
    if (ot = text2obj ("viewPrimType"))
	ot -> ot_getfnx = o_viewPrim,
	ot -> ot_info = (caddr_t) viewPrimType;

    if (ot = text2obj ("viewAclView"))
	ot -> ot_getfnx = o_viewAcl,
	ot -> ot_info = (caddr_t) viewAclView;
    if (ot = text2obj ("viewAclCommunity"))
	ot -> ot_getfnx = o_viewAcl,
	ot -> ot_info = (caddr_t) viewAclCommunity;
    if (ot = text2obj ("viewAclUser"))
	ot -> ot_getfnx = o_viewAcl,
	ot -> ot_info = (caddr_t) viewAclUser;
    if (ot = text2obj ("viewAclPrivileges"))
	ot -> ot_getfnx = o_viewAcl,
	ot -> ot_info = (caddr_t) viewAclPrivileges;
    if (ot = text2obj ("viewAclType"))
	ot -> ot_getfnx = o_viewAcl,
	ot -> ot_info = (caddr_t) viewAclType;

    if (ot = text2obj ("viewTrapView"))
	ot -> ot_getfnx = o_viewTrap,
	ot -> ot_info = (caddr_t) viewTrapView;
    if (ot = text2obj ("viewTrapGenerics"))
	ot -> ot_getfnx = o_viewTrap,
	ot -> ot_info = (caddr_t) viewTrapGenerics;
    if (ot = text2obj ("viewTrapSpecifics"))
	ot -> ot_getfnx = o_viewTrap,
	ot -> ot_info = (caddr_t) viewTrapSpecifics;
    if (ot = text2obj ("viewTrapType"))
	ot -> ot_getfnx = o_viewTrap,
	ot -> ot_info = (caddr_t) viewTrapType;
}

/*  */

fin_view ()
{
    register int    i;
    char   *vec[3];
    register struct community *c;
    register struct view *v;
    register struct trap *t;

    if (CHead -> c_forw == CHead) {
	vec[0] = "community";
	vec[1] = "public";
	vec[2] = NULL;

	(void) f_community (vec);
    }

    for (c = CHead -> c_forw; c != CHead; c = c -> c_forw) {
	for (v = VHead -> v_forw; v != VHead; v = v -> v_forw)
	    if (oid_cmp (v -> v_name, c -> c_vu) == 0) {
		c -> c_view = v;
		break;
	    }
	if (v == VHead)
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "no such view as %s for community \"%s\"",
		    sprintoid (c -> c_vu), c -> c_name);
    }

    i = 0;
    for (v = VHead -> v_forw; v != VHead; v = v -> v_forw)
	i++;
    if (i > 0) {
	register struct view **base,
			     **bp,
			     **ep;

	if ((base = (struct view **) malloc ((unsigned) (i * sizeof *base)))
	        == NULL)
	    adios (NULLCP, "out of memory");
	ep = base;
	for (v = VHead -> v_forw; v != VHead; v = v -> v_forw) {
	    register int    j;
	    register unsigned int *ip,
				  *jp;
	    OID	    oid = v -> v_name;

	    v -> v_insize = 1 + (j = oid -> oid_nelem);
	    if ((v -> v_instance =
		    (unsigned int *) calloc ((unsigned) v -> v_insize,
					     sizeof *v -> v_instance)) == NULL)
		adios (NULLCP, "out of memory");
	    v -> v_instance[0] = oid -> oid_nelem;
	    for (ip = v -> v_instance + 1, jp = oid -> oid_elements;
		     j > 0;
		     j--)
		*ip++ = *jp++;

	    remque (*ep++ = v);
	}
	VHead -> v_forw = VHead -> v_back = VHead;

	if (i > 1)
	    qsort ((char *) base, i, sizeof *base, view_compar);

	bp = base;
	while (bp < ep)
	    insque (*bp++, VHead -> v_back);

	free ((char *) base);
    }

    i = 0;
    for (c = CHead -> c_forw; c != CHead; c = c -> c_forw)
	i++;
    if (i > 0) {
	int	j;
	register struct community **base,
			          **bp,
			          **ep;

	if ((base = (struct community **)
			    malloc ((unsigned) (i * sizeof *base))) == NULL)
	    adios (NULLCP, "out of memory");
	ep = base;
	for (c = CHead -> c_forw; c != CHead; c = c -> c_forw) {
	    register char *cp,
			  *dp;
	    register unsigned int *ip;

	    switch (c -> c_addr.na_stack) {
		case NA_TCP:
		    j = 4;
		    break;

		case NA_X25:
		    j = c -> c_addr.na_dtelen;
		    break;

		case NA_NSAP:
		    j = c -> c_addr.na_addrlen;
		    break;

		default:
		    j = 0;
		    break;
	    }

	    c -> c_insize = 1 + strlen (c -> c_name) + 1 + j;
	    if ((c -> c_instance =
		    (unsigned int *) calloc ((unsigned) c -> c_insize,
					     sizeof *c -> c_instance)) == NULL)
		adios (NULLCP, "out of memory");
	    ip = c -> c_instance;

	    *ip++ = strlen (c -> c_name);
	    for (cp = c -> c_name; *cp; cp++)
		*ip++ = *cp & 0xff;

	    *ip++ = j;
	    switch (c -> c_addr.na_stack) {
		case NA_TCP:
		    (void) sscanf (c -> c_addr.na_domain, "%u.%u.%u.%u",
				   ip, ip + 1, ip + 2, ip + 3);
		    break;

		case NA_X25:
		    dp = (cp = c -> c_addr.na_dte) + c -> c_addr.na_dtelen;
		    goto stuff_it;

		case NA_NSAP:
		    dp = (cp = c -> c_addr.na_address) + c ->c_addr.na_addrlen;
stuff_it: ;
		    while (cp < dp)
			*ip++ = *cp++ & 0xff;
		    break;

		default:
		    break;
	    }

	    *ep++ = c;
	}

	if (i > 1)
	    qsort ((char *) base, i, sizeof *base, comm_compar);

	bp = base;
	c = CLex = *bp++;
	while (bp < ep) {
	    c -> c_next = *bp;
	    c = *bp++;
	}
	c -> c_next = NULL;

	free ((char *) base);
    }
    else
	CLex = NULL;

    i = 0;
    for (t = UHead -> t_forw; t != UHead; t = t -> t_forw)
	i++;
    if (i > 0) {
	register struct trap **base,
			     **bp,
			     **ep;

	if ((base = (struct trap **) malloc ((unsigned) (i * sizeof *base)))
	        == NULL)
	    adios (NULLCP, "out of memory");
	ep = base;
	for (t = UHead -> t_forw; t != UHead; t = t -> t_forw) {
	    register int    j;
	    register unsigned int *ip,
				  *jp;
	    OID	    oid = t -> t_view -> v_name;

	    t -> t_insize = 1 + (j = oid -> oid_nelem);
	    if ((t -> t_instance =
		    (unsigned int *) calloc ((unsigned) t -> t_insize,
					     sizeof *t -> t_instance)) == NULL)
		adios (NULLCP, "out of memory");
	    t -> t_instance[0] = oid -> oid_nelem;
	    for (ip = t -> t_instance + 1, jp = oid -> oid_elements;
		     j > 0;
		     j--)
		*ip++ = *jp++;

	    remque (*ep++ = t);
	}
	UHead -> t_forw = UHead -> t_back = UHead;

	if (i > 1)
	    qsort ((char *) base, i, sizeof *base, trap_compar);

	bp = base;
	while (bp < ep)
	    insque (*bp++, UHead -> t_back);

	free ((char *) base);
    }
}

/*  */

int	f_community (vec)
char  **vec;
{
    register struct community *c;
    register struct NSAPaddr *na;

    vec++;

    if ((c = (struct community *) calloc (1, sizeof *c)) == NULL
	    || (c -> c_name = strdup (*vec)) == NULL)
	adios (NULLCP, "out of memory");
    vec++;

    na = &c -> c_addr;
    if (*vec) {
	if (str2sa (*vec, na, (struct sockaddr *) NULL, 0) == NOTOK)
	    adios (NULLCP, "unknown address \"%s\" for community \"%s\"",
		   *vec, c -> c_name);

	vec++;
    }
    else {
	na -> na_stack = NA_TCP;
	na -> na_community = ts_comm_tcp_default;
	(void) strcpy (na -> na_domain, "0.0.0.0");
    }

    if (*vec) {
	if (lexequ (*vec, "readOnly") == 0)
	    c -> c_permission = OT_RDONLY;
	else
	    if (lexequ (*vec, "readWrite") == 0)
		c -> c_permission = OT_RDWRITE;
	    else
		if (lexequ (*vec, "writeOnly") == 0)
		    c -> c_permission = OT_WRONLY;
		else
		    if (lexequ (*vec, "none")) {
			advise (LLOG_EXCEPTIONS, NULLCP,
				"invalid access mode \"%s\"", *vec);
			goto you_lose;
		    }

	vec++;
    }
    else
	c -> c_permission = OT_RDONLY;

    if (*vec) {
	char    buffer[BUFSIZ];

	(void) strcpy (buffer, *vec);
	if ((c -> c_vu = text2oid (buffer)) == NULLOID) {
	    advise (LLOG_EXCEPTIONS, NULLCP, "unknown OID \"%s\"", *vec);
	    goto you_lose;
	}

	if (*++vec)
	    goto you_lose;
    }
    else
	if ((c -> c_vu = text2oid ("defViewWholeRO")) == NULL) {
	    advise (LLOG_EXCEPTIONS, NULLCP, "unknown OID \"defViewWholeRO\"");
		goto you_lose;
	}

    insque (c, CHead -> c_back);

    return OK;

you_lose: ;
    free (c -> c_name);
    free ((char *) c);

    return NOTOK;
}

/*  */

int	f_proxy (vec)
char  **vec;
{
    char    buffer[BUFSIZ];
    register struct community *c;
    register struct view *v,
			 *u;
    register struct NSAPaddr *na;

    if ((v = (struct view *) calloc (1, sizeof *v)) == NULL)
	adios (NULLCP, "out of memory");
    v -> v_subtree.s_forw = v -> v_subtree.s_back = &v -> v_subtree;
    if ((c = (struct community *) calloc (1, sizeof *c)) == NULL)
	adios (NULLCP, "out of memory");
    c -> c_permission = OT_YYY;
    vec++;

    (void) strcpy (buffer, *vec);
    if ((v -> v_name = text2oid (buffer)) == NULL) {
	advise (LLOG_EXCEPTIONS, NULLCP, "unknown OID \"%s\"", *vec);
	goto you_lose;
    }
    c -> c_vu = v -> v_name;
    if (trapview && inSubtree (trapview, v -> v_name)) {
	advise (LLOG_EXCEPTIONS, NULLCP, "view \"%s\" is for traps", *vec);
	goto you_lose;
    }
    for (u = VHead -> v_forw; u != VHead; u = u -> v_forw)
	if (oid_cmp (u -> v_name, v -> v_name) == 0) {
	    advise (LLOG_EXCEPTIONS, NULLCP, "duplicate view \"%s\"", *vec);
	    goto you_lose;
	}
    vec++;

    if (lexequ (*vec, "rfc1157")) {
	advise (LLOG_EXCEPTIONS, NULLCP, "unsupported proxy domain \"%s\"",
		*vec);
	goto you_lose;
    }
    vec++;

    na = &c -> c_addr;
    if (*vec) {
	if (str2sa (*vec, na, &v -> v_sa, 1) == NOTOK)
	    adios (NULLCP, "unknown address \"%s\" for proxy %s",
		   *vec, oid2ode (v -> v_name));

	vec++;
    }
    else
	goto you_lose;

    if (*vec) {
	if ((v -> v_community = str2qb (*vec, strlen (*vec), 1)) == NULL)
	    adios (NULLCP, "out of memory");
	if ((c -> c_name = strdup (*vec)) == NULL)
	    adios (NULLCP, "out of memory");

	if (*++vec)
	    goto you_lose;
    }
    else
	goto you_lose;

    insque (v, VHead -> v_back);
    insque (c, CHead -> c_back);

    return OK;

you_lose: ;
    if (c -> c_name)
	free (c -> c_name);
    free ((char *) c);
    if (v -> v_name)
	oid_free (v -> v_name);
    if (v -> v_community)
	qb_free (v -> v_community);
    free ((char *) v);

    return NOTOK;
}

/*  */

int	f_trap (vec)
char  **vec;
{
    register struct trap *t;
    register struct view *v;
    struct NSAPaddr nas;
    register struct NSAPaddr *na = &nas;
    static int trapno = 1;

    vec++;

    if ((t = (struct trap *) calloc (1, sizeof *t)) == NULL
	    || (t -> t_name = strdup (*vec)) == NULL)
	adios (NULLCP, "out of memory");
    v = t -> t_view = &t -> t_vu;
    v -> v_subtree.s_forw = v -> v_subtree.s_back = &v -> v_subtree;
    t -> t_generics = 0xfe;
    vec++;

    trapview -> oid_elements[trapview -> oid_nelem++] = trapno;
    v -> v_name = oid_cpy (trapview);
    trapview -> oid_nelem--;
    if (v -> v_name == NULLOID)
	adios (NULLCP, "out of memory");

    if ((v -> v_community = str2qb (t -> t_name, strlen (t -> t_name), 1))
	    == NULL)
	adios (NULLCP, "out of memory");

    bzero ((char *) na, sizeof *na);
    if (*vec) {
	if (str2sa (*vec, na, &v -> v_sa, 0) == NOTOK)
	    adios (NULLCP, "unknown address \"%s\" for trap sink \"%s\"",
		   *vec, t -> t_name);

	vec++;
    }
    else
	goto you_lose;

    if (*vec) {
	char    buffer[BUFSIZ];
	OID	name;
	register struct view *u;

	(void) strcpy (buffer, *vec);
	if ((name = text2oid (buffer)) == NULL) {
	    advise (LLOG_EXCEPTIONS, NULLCP, "unknown OID \"%s\"", *vec);
	    goto you_lose;
	}
	oid_free (v -> v_name);
	v -> v_name = name;

	for (u = VHead -> v_forw; u != VHead; u = u -> v_forw)
	    if (oid_cmp (u -> v_name, v -> v_name) == 0) {
		advise (LLOG_EXCEPTIONS, NULLCP,
			"duplicate view \"%s\" for trap sink \"%s\"",
			*vec, t -> t_name);
		goto you_lose;
	    }

	vec++;
    }
    else
	trapno++;

    if (*vec) {
	if (sscanf (*vec, "%lx", &t -> t_generics) != 1)
	    goto you_lose;

	if (*++vec)
	    goto you_lose;
    }

    insque (t, UHead -> t_back);
    insque (v, VHead -> v_back);

    return OK;

you_lose: ;
    oid_free (v -> v_name);
    qb_free (v -> v_community);
    free (t -> t_name);
    free ((char *) t);

    return NOTOK;
}

/*  */

int  f_view (vec)
char  **vec;
{
    char    buffer[BUFSIZ];
    register struct subtree *s,
			    *x,
			    *y;
    register struct view *v,
			 *u;

    if (viewmask == 0) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"too many views starting with \"%s\"", *vec);
	return NOTOK;
    }

    if ((v = (struct view *) calloc (1, sizeof *v)) == NULL)
	adios (NULLCP, "out of memory");
    s = &v -> v_subtree;
    v -> v_subtree.s_forw = v -> v_subtree.s_back = s;
    vec++;

    (void) strcpy (buffer, *vec);
    if ((v -> v_name = text2oid (buffer)) == NULL) {
	advise (LLOG_EXCEPTIONS, NULLCP, "unknown OID \"%s\"", *vec);
	goto you_lose;
    }
    if (trapview && inSubtree (trapview, v -> v_name)) {
	advise (LLOG_EXCEPTIONS, NULLCP, "view \"%s\" is for traps", *vec);
	goto you_lose;
    }
    for (u = VHead -> v_forw; u != VHead; u = u -> v_forw)
	if (oid_cmp (u -> v_name, v -> v_name) == 0) {
	    advise (LLOG_EXCEPTIONS, NULLCP, "duplicate view \"%s\"", *vec);
	    goto you_lose;
	}

    for (vec++; *vec; vec++) {
	register struct subtree *z;
	OID	name;

	(void) strcpy (buffer, *vec);
	if ((name = text2oid (buffer)) == NULLOID) {
	    advise (LLOG_EXCEPTIONS, NULLCP, "unknown OID \"%s\"", *vec);
	    goto you_lose;
	}

	for (x = s -> s_forw; x != s; x = y) {
	    register int    i,
			    j;
	    y = x -> s_forw;

	    if (bcmp ((char *) x -> s_subtree -> oid_elements,
		      (char *) name -> oid_elements,
		      ((i = x -> s_subtree -> oid_nelem)
		           <= (j = name -> oid_nelem) ? i : j)
		          * sizeof name -> oid_elements[0]) == 0) {
		advise (LLOG_EXCEPTIONS, NULLCP,
			"%s %s %s",
			*vec,
			i <= j ? "already under" : "superceding",
			oid2ode (x -> s_subtree));
		if (i <= j)
		    goto another;

		remque (x);
		oid_free (x -> s_subtree);
		free ((char *) x);
	    }
	}

	if ((z = (struct subtree *) calloc (1, sizeof *z)) == NULL)
	    adios (NULLCP, "out of memory");
	z -> s_subtree = name;

	insque (z, s -> s_back);
another: ;
    }

    v -> v_mask = viewmask;
    viewmask <<= 1;
    insque (v, VHead -> v_back);

    return OK;

you_lose: ;
    for (x = s -> s_forw; x != s; x = y) {
	y = x -> s_forw;

	remque (x);
	oid_free (x -> s_subtree);
	free ((char *) x);
    }
    if (v -> v_name)
	oid_free (v -> v_name);
    free ((char *) v);

    return NOTOK;
}

/*  */

extern	int	tcpservice;
extern	int	udport;
extern	int	traport;


static int  str2sa (s, na, sock, proxy)
char   *s;
struct NSAPaddr *na;
struct sockaddr *sock;
int	proxy;
{
#ifdef	TCP
    register struct hostent *hp;
#endif
    struct TSAPaddr *ta;

#ifdef	TCP
    if (hp = gethostbystring (s)) {
	struct sockaddr_in    sin;

	na -> na_stack = NA_TCP;
	na -> na_community = ts_comm_tcp_default;
	inaddr_copy (hp, &sin);
	(void) strncpy (na -> na_domain, inet_ntoa (sin.sin_addr),
			sizeof na -> na_domain - 1);
    }
    else
#endif
	if ((ta = str2taddr (s)) && ta -> ta_naddr > 0) {
	    *na = ta -> ta_addrs[0];	/* struct copy */
	}
	else
	    return NOTOK;

    if (sock == NULL)
	return OK;

    switch (na -> na_stack) {
#ifdef	TCP
	case NA_TCP:
	    if (!tcpservice)
		goto you_lose;
	    {
		struct sockaddr_in sin;

		sin.sin_port = na -> na_port ? na -> na_port
					     : proxy ? udport : traport;

		if ((hp = gethostbystring (na -> na_domain)) == NULL)
		    return NOTOK;

		sin.sin_family = hp -> h_addrtype;
		inaddr_copy (hp, &sin);

		*((struct sockaddr_in *) sock) = sin;	/* struct copy */
	    }
	    break;
#endif

	default:
you_lose: ;
	    advise (LLOG_EXCEPTIONS, NULLCP, "address type unsupported");
	    return NOTOK;
    }

    return OK;
}
