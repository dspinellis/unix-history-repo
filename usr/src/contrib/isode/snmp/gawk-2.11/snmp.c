/* snmp.c - SNMP changes for gawk */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/gawk-2.11/RCS/snmp.c,v 7.10 91/02/22 09:45:08 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/gawk-2.11/RCS/snmp.c,v 7.10 91/02/22 09:45:08 mrose Interim $
 *
 *
 * $Log:	snmp.c,v $
 * Revision 7.10  91/02/22  09:45:08  mrose
 * Interim 6.8
 * 
 * Revision 7.9  91/01/07  12:42:41  mrose
 * update
 * 
 * Revision 7.8  90/10/23  20:44:36  mrose
 * update
 * 
 * Revision 7.7  90/10/02  15:17:05  mrose
 * robust
 * 
 * Revision 7.6  90/09/07  11:11:50  mrose
 * update
 * 
 * Revision 7.3  90/08/17  15:12:28  mrose
 * for-in
 * 
 * Revision 7.2  90/06/06  22:59:38  mrose
 * update
 * 
 * Revision 7.1  90/03/22  16:44:22  mrose
 * touch-up
 * 
 * Revision 7.0  90/03/05  10:33:19  mrose
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


#ifdef	SNMP
#include "awk.h"
#ifdef	HUGE
#undef	HUGE
#endif
#include <isode/snmp/objects.h>
#include <isode/pepsy/SNMP-types.h>
#include <isode/dgram.h>
#include <isode/internet.h>
#include <isode/isoaddrs.h>
#include <isode/tailor.h>

/*    DATA */

int	debug = 0;

int	snmp_enabled = 1;
int	snmp_scalars_as_arrays = 1;
char   *snmp_file = NULLCP;

static	integer	snmp_id = 0;
static	int	snmp_portno = 0;
static	int	snmp_retries = 3;
static	int	snmp_timeout = 10;

static	char   *snmp_agent = NULL;
static	char   *snmp_community = NULL;

NODE   *AGENT_node,
       *COMMUNITY_node,
       *DIAGNOSTIC_node,
       *ERROR_node,
       *RETRIES_node,
       *TIMEOUT_node;
NODE   *Ndot_string;

static	int	snmp_fd = NOTOK;
static	struct sockaddr_in in_socket;
static	PS	ps = NULLPS;

static	struct type_SNMP_Message	msgs;
static	struct type_SNMP_PDUs		pdus;
static	struct type_SNMP_PDU		parms;
static	struct type_SNMP_VarBindList	vps;
static	struct type_SNMP_VarBind	vs;


struct snmp_search {
    struct search s_search;	/* must be first element in struct */

    OT	    s_parent;

    struct type_SNMP_VarBindList *s_prototype;

#define	NREQ	10
    struct snmp_req {
	struct type_SNMP_VarBindList *r_bindings;
	PE	r_pb;

	integer	r_id;
	PE	r_pe;

	struct type_SNMP_Message *r_msg;
    }	    s_reqs[NREQ];

    struct snmp_search *s_prev;
    struct snmp_search *s_next;
};

static struct snmp_search *head = NULL;
static struct snmp_search *tail = NULL;


char   *snmp_error (), *snmp_variable ();


#ifndef	SYS5
long	random ();
#endif

/*    INIT */

int	snmp_init ()
{
    char   *addr;
    register struct hostent *hp;
    struct sockaddr_in lo_socket;

    snmp_onceonly ();

    if (lookup (variables, "AGENT")
	    || !(hp = gethostbystring (getlocalhost ())))
	return 1;

    inaddr_copy (hp, &lo_socket);
    addr = inet_ntoa (lo_socket.sin_addr);
    AGENT_node = install (variables, "AGENT",
			  node (make_string (addr, strlen (addr)),
				Node_var, (NODE *) NULL));

    return 0;
}

/*  */

int	f_integer (), f_octets (), f_display (), f_objectID (), f_null (),
	f_ulong (), f_ipaddr (), f_clnpaddr ();


static struct pair {
    char   *pp_name;
    IFP	    pp_value;
}	pairs[] = {
    "INTEGER", f_integer,
    "Services", f_integer,
    "Privileges", f_integer,
    "OctetString", f_octets,
    "DisplayString", f_display,
    "ObjectID", f_objectID,
    "NULL", f_null,
    "IpAddress", f_ipaddr,
    "NetworkAddress", f_ipaddr,
    "Counter", f_ulong,
    "Gauge", f_ulong,
    "TimeTicks", f_ulong,
    "ClnpAddress", f_clnpaddr,

    NULL
};


static	snmp_onceonly () {
    int	    i;
    register struct pair *pp;
    register struct type_SNMP_Message *msg = &msgs;
    register struct type_SNMP_PDUs *pdu = &pdus;
    register struct type_SNMP_PDU *parm = &parms;
    register struct type_SNMP_VarBindList *vp = &vps;
    register struct type_SNMP_VarBind *v = &vs;
    OS	    os;
    register OT	    ot,
		    ot2;

    Ndot_string = make_string (".", 1);
    Ndot_string -> flags |= PERM;

    if (readobjects (snmp_file) == NOTOK)
	fatal ("readobjects: %s", PY_pepy);

				/* mark entries that are actually columns! */
    for (ot = text2obj ("ccitt"); ot; ot = ot -> ot_next) {
	if (ot -> ot_syntax
		|| (i = strlen (ot -> ot_text)) <= 5
	        || strcmp (ot -> ot_text + i - 5, "Entry"))
	    continue;
	for (ot2 = ot -> ot_children; ot2; ot2 = ot2 -> ot_sibling)
	    if (ot2 -> ot_children)
		break;
	if (ot2)
	    continue;
	for (ot2 = ot -> ot_children; ot2; ot2 = ot2 -> ot_sibling)
	    if (ot2 -> ot_syntax)
		ot2 -> ot_getfnx = (IFP) 1;
	    else
		if (debug > 0)
		    fprintf (stderr, "no syntax for columnar object \"%s\"\n",
			     ot -> ot_text);
    }

    for (pp = pairs; pp -> pp_name; pp++)
	if ((os = text2syn (pp -> pp_name)) == NULL)
	    fatal ("lost syntax for \"%s\"", pp -> pp_name);
        else
	    os -> os_decode = pp -> pp_value;

    bzero ((char *) msg, sizeof *msg);
    msg -> version = int_SNMP_version_version__1;
    msg -> data = pdu;

    bzero ((char *) pdu, sizeof *pdu);
    pdu -> offset = type_SNMP_PDUs_get__request;
    pdu -> un.get__request = parm;

    bzero ((char *) parm, sizeof *parm);
    parm -> variable__bindings = vp;

    bzero ((char *) vp, sizeof *vp);
    vp -> VarBind = v;

    bzero ((char *) v, sizeof *v);
    if ((v -> value = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_NULL))
	    == NULLPE)
	fatal ("pe_alloc: out of memory");

    ps_len_strategy = PS_LEN_LONG;

#ifndef	SYS5
    srandom (getpid ());
#else
    srand (getpid ());
#endif
}

/*    CHECK */

int	snmp_check (r, name)
NODE   *r;
char   *name;
{
    char    c;
    register char   *cp;
    OT	    ot;

    for (cp = name; is_identchar (*cp); cp++)
	continue;
    if (c = *cp)
	*cp = NULL;
    if ((ot = text2obj (name)) && ot -> ot_syntax) {
	r -> magic = (caddr_t) ot;
	if (ot -> ot_getfnx || snmp_scalars_as_arrays)
	    r -> type = Node_var_array;
    }
    *cp = c;
}

/*    GET */

int	snmp_get (ptr, instname)
NODE   *ptr;
char   *instname;
{
    int	    gotone,
	    retries,
	    status = -1;
    struct type_SNMP_Message *msg = &msgs;
    register struct type_SNMP_PDU *parm = msg -> data -> un.get__request;
    register struct type_SNMP_VarBind *v =
					parm -> variable__bindings -> VarBind;
    PE	    pe = NULLPE,
	    p = NULLPE;
    OID	    oid;
    OT	    ot = (OT) ptr -> magic;
    NODE   *value = NULL;

    if (snmp_ready (1) == NOTOK)
	goto out;

    parm -> request__id = snmp_id;
    if (v -> name)
	free_SNMP_ObjectName (v -> name), v -> name = NULL;

    if (instname == NULL) {
	if (ot -> ot_getfnx || snmp_scalars_as_arrays) {
	    register struct snmp_search *s;

	    for (s = tail; s; s = s -> s_prev) {
		register struct snmp_req *sr;

		if (ot -> ot_name -> oid_nelem
			   != (oid = s -> s_parent -> ot_name) -> oid_nelem + 1
		        || bcmp ((char *) ot -> ot_name -> oid_elements,
				 (char *) oid -> oid_elements,
				 oid -> oid_nelem
				         * sizeof ot -> ot_name -> oid_elements[0]))
		    continue;
		for (sr = s -> s_reqs; sr -> r_bindings; sr++) {
		    register struct type_SNMP_VarBindList *vp;

		    for (vp = sr -> r_bindings; vp; vp = vp -> next) {
			if (ot -> ot_name -> oid_nelem
			        >= (v = vp -> VarBind) -> name -> oid_nelem)
			    fatal ("snmp_get: internal error");
			if (bcmp ((char *) v -> name -> oid_elements,
				  (char *) ot -> ot_name -> oid_elements,
				  ot -> ot_name -> oid_nelem
				      * sizeof ot -> ot_name 
							-> oid_elements[0]))
			    continue;

			goto get_value;
		    }
		}
		status = int_SNMP_error__status_noSuchName;
		goto out;
	    }
	    if (ot -> ot_getfnx) {
		snmp_diag (NULLCP,
			   "can't use SNMP array variable as scalar unless within for-in construct");
		goto out;
	    }
	}

	if ((oid = v -> name = oid_extend (ot -> ot_name, 1)) == NULL) {
no_mem_for_inst: ;
	    snmp_diag (NULLCP, "oid_extend: out of memory");
	    goto out;
	}
	v -> name -> oid_elements[v -> name -> oid_nelem - 1] = 0;
    }
    else {
	register int	i;
	register unsigned int *ip,
			      *jp;
	OID	inst = str2oid (instname);

	if (inst == NULL) {
	    snmp_diag (NULLCP, "str2oid: bad instance identifier \"%s\"",
		       instname);
	    goto out;
	}
	if ((oid = v -> name = oid_extend (ot -> ot_name, inst -> oid_nelem))
	        == NULL)
	    goto no_mem_for_inst;
	ip = oid -> oid_elements + oid -> oid_nelem - inst -> oid_nelem;
	jp = inst -> oid_elements;
	for (i = inst -> oid_nelem; i > 0; i--)
	    *ip++ = *jp++;
    }

    if (encode_SNMP_Message (&pe, 1, 0, NULLCP, msg) == NOTOK) {
	snmp_diag (NULLCP, "encode_SNMP_Message: %s", PY_pepy);
	goto out;
    }

    msg = NULL, gotone = 0;
    for (retries = snmp_retries; retries > 0; ) {
	int	len;
	fd_set	rfds;

	if (debug > 1)
	    print_SNMP_Message (pe, 1, NULLIP, NULLVP, NULLCP);
	len = ps -> ps_byteno;
	if (pe2ps (ps, pe) == NOTOK) {
	    snmp_diag (NULLCP, "pe2ps: %s", ps_error (ps -> ps_errno));
	    goto error_x;
	}
	if (debug > 0 && (len = ps -> ps_byteno - len) > 484)
	    fprintf (stderr, "sent message of %d octets\n", len);

	FD_ZERO (&rfds);
	FD_SET (snmp_fd, &rfds);

	switch (xselect (snmp_fd + 1, &rfds, NULLFD, NULLFD, snmp_timeout)) {
	    case NOTOK:
	        snmp_diag ("failed", "xselect");
		goto error_x;

	    default:
		if (FD_ISSET (snmp_fd, &rfds))
		    break;
		/* else fall... */
	    case OK:
		if (debug > 0)
		    fprintf (stderr, "timeout...\n");
		retries--;
		continue;
	}

	if ((p = ps2pe (ps)) == NULLPE) {
	    snmp_diag (NULLCP, "ps2pe: %s", ps_error (ps -> ps_errno));
	    goto error_x;
	}
	if (decode_SNMP_Message (p, 1, NULLIP, NULLVP, &msg) == NOTOK) {
	    snmp_diag (NULLCP, "decode_SNMP_Message: %s", PY_pepy);
	    goto out;
	}
	if (debug > 1)
	    print_SNMP_Message (p, 1, NULLIP, NULLVP, NULLCP);

	if (msg -> data -> offset != type_SNMP_PDUs_get__response) {
	    snmp_diag (NULLCP, "unexpected message type %d",
		       msg -> data -> offset);
	    goto out;
	}

	if ((parm = msg -> data -> un.get__response) -> request__id == snmp_id)
	    break;

	if (debug > 0)
	    fprintf (stderr, "bad ID (got %ld, wanted %ld)\n",
		     (long) parm -> request__id, (long) snmp_id);

	if (msg)
	    free_SNMP_Message (msg), msg = NULL;
	if (p)
	    pe_free (p), p = NULLPE;

	gotone++;
    }
    if (retries <= 0) {
	snmp_diag (NULLCP,
		   "no %sresponse within %d retries of %s%d second%s each",
		   gotone ? "acceptable " : "", snmp_retries + gotone,
		   gotone ? "upto " : "",
		   snmp_timeout, snmp_timeout != 1 ? "s" : "");
	goto out;
    }

    if ((status = parm -> error__status) != int_SNMP_error__status_noError) {
	char   *cp = snmp_variable (parm, parm -> error__index);

	snmp_diag (NULLCP, cp ? "%s at position %d (%s)" : "%s at position %d",
		   snmp_error (status), parm -> error__index, cp);
	goto out;
    }

    if (parm -> variable__bindings == NULL
	    || (v = parm -> variable__bindings -> VarBind) == NULL) {
	snmp_diag (NULLCP, "missing variable in response");
	goto out;
    }
    if (debug > 0 && parm -> variable__bindings -> next)
	fprintf (stderr, "too many responses starting with: %s\n",
		 oid2ode (parm -> variable__bindings -> next -> VarBind -> name));

    if (oid_cmp (oid, v -> name)) {
	char    buffer[BUFSIZ];

	(void) strcpy (buffer, oid2ode (v -> name));
	snmp_diag (NULLCP, "wrong variable returned (got %s, wanted %s)",
		 buffer, oid2ode (oid));
	goto out;
    }
		 
get_value: ;
    if ((*ot -> ot_syntax -> os_decode) (&value, v -> value) == NOTOK) {
	snmp_diag (NULLCP, "decode error for variable \"%s\": %s",
		 oid2ode (v -> name), PY_pepy);
	goto out;
    }

    goto out;

error_x: ;
    if (ps)
	ps_free (ps), ps = NULLPS;
    if (snmp_fd != NOTOK)
	(void) close_udp_socket (snmp_fd), snmp_fd = NOTOK;

out: ;
    if (msg && msg != &msgs)
	free_SNMP_Message (msg);
    if (p)
	pe_free (p);
    if (pe)
	pe_free (pe);

    deref = ptr -> var_value;
    do_deref ();

    ptr -> var_value = value ? value : Nnull_string;

    assign_number (&ERROR_node -> var_value, (AWKNUM) status);
}

/*    SCAN */

struct search *snmp_assoc_scan (symbol, instance)
NODE   *symbol,
       *instance;
{
    register struct snmp_search *s;
    OID	    inst;
    register OT	    ot = (OT) symbol -> magic;
    register struct type_SNMP_VarBindList **vp,
					  **vp2;

    if (!ot -> ot_getfnx && (!snmp_scalars_as_arrays || instance))
	fatal ("can't use SNMP scalar variable as control for for-in");

    if (instance) {
	char   *instname;
	NODE   *r;

	if ((r = instance) -> type != Node_val)
	    r = r_tree_eval (instance);

	if ((inst = str2oid (instname = force_string (r) -> stptr)) == NULL) {
	    snmp_diag (NULLCP, "str2oid: bad instance identifier \"%s\"",
		       instname);
	    free_temp (r);
	    return NULL;
	}

	if (r != instance)
	    free_temp (r);
    }
    else
	inst = NULL;
    emalloc (s, struct snmp_search *, sizeof *s, "snmp_assoc_scan1");
    bzero ((char *) s, sizeof *s);

    ot -> ot_name -> oid_nelem--;
    s -> s_parent = name2obj (ot -> ot_name);
    ot -> ot_name -> oid_nelem++;

    if ((ot = s -> s_parent) == NULL)
	fatal ("unable to find parent for \"%s\"", snmp_name (symbol));

    s -> s_prototype = NULL;

    vp = &s -> s_reqs[0].r_bindings, vp2 = &s -> s_prototype;
    for (ot = ot -> ot_children; ot; ot = ot -> ot_sibling) {
	register int    i;
	register unsigned int *ip,
			      *jp;
	register struct type_SNMP_VarBindList *bind;
	register struct type_SNMP_VarBind *v,
					  *v2;

	if (!ot -> ot_syntax)
	    continue;
	emalloc (bind, struct type_SNMP_VarBindList *, sizeof *bind,
		 "snmp_assoc_scan2");
	*vp = bind, vp = &bind -> next;
	bind -> next = NULL;

	emalloc (v, struct type_SNMP_VarBind *, sizeof *v, "snmp_assoc_scan3");
	bind -> VarBind = v;
	if (inst) {
	    if ((v -> name = oid_extend (ot -> ot_name, inst -> oid_nelem))
		    == NULL)
		fatal ("oid_extend: out of memory");
	    ip = v -> name -> oid_elements + v -> name -> oid_nelem 
					   - inst -> oid_nelem;
	    jp = inst -> oid_elements;
	    for (i = inst -> oid_nelem; i > 0; i--)
		*ip++ = *jp++;
	}
	else
	    if ((v -> name = oid_cpy (ot -> ot_name)) == NULL)
		fatal ("oid_cpy: out of memory");
	v -> value = NULL;

	emalloc (bind, struct type_SNMP_VarBindList *, sizeof *bind,
		 "snmp_assoc_scan4");
	*vp2 = bind, vp2 = &bind -> next;
	bind -> next = NULL;

	emalloc (v2, struct type_SNMP_VarBind *, sizeof *v2,
		 "snmp_assoc_scan5");
	bind -> VarBind = v2;
	if ((v2 -> name = oid_cpy (v -> name)) == NULL)
	    fatal ("oid_cpy: out of memory");
	v2 -> value = NULL;
    }

    if (head == NULL)
	head = tail = s;
    else {
	tail -> s_next = s;
	s -> s_prev = tail;
	tail = s;
    }

    return snmp_assoc_next (&s -> s_search, 0);
}

/*  */

struct search *snmp_assoc_next (lookat, done)
struct search *lookat;
int	done;
{
    int	    i;
    char   *cp;
    register struct snmp_search *s = (struct snmp_search *) lookat;
    register struct search *l = &s -> s_search;
    struct OIDentifier	oids;
    OID	    oid;
    OT	    ot = s -> s_parent;
    register struct type_SNMP_VarBind *v;

    deref = l -> retval, l -> retval = NULL;
    do_deref ();

    if (done
	    || snmp_get_next (s) == NOTOK
	    || s -> s_reqs[0].r_bindings == NULL) {
	register struct snmp_req *sr;

	if (s -> s_prototype)
	    free_SNMP_VarBindList (s -> s_prototype);
	for (sr = s -> s_reqs; sr -> r_bindings; sr++) {
	    free_SNMP_VarBindList (sr -> r_bindings);
	    if (sr -> r_pb)
		pe_free (sr -> r_pb);
	    if (sr -> r_msg)
		free_SNMP_Message (sr -> r_msg);
	    if (sr -> r_pe)
		pe_free (sr -> r_pe);
	}

	if (tail != s)
	    fatal ("snmp_assoc_next: internal error1");
	if (tail = s -> s_prev)
	    tail -> s_next = NULL;
	else
	    head = NULL;

	free ((char *) s);
	return NULL;
    }

    if ((v = s -> s_reqs[0].r_bindings -> VarBind) == NULL
	     || (oid = v -> name) == NULL)
	fatal ("snmp_assoc_next: internal error2");
    if (ot -> ot_name -> oid_nelem >= oid -> oid_nelem
	    || bcmp ((char *) ot -> ot_name -> oid_elements,
		     (char *) oid -> oid_elements,
		     ot -> ot_name -> oid_nelem
		             * sizeof oid -> oid_elements[0]))
	fatal ("snmp_assoc_next: internal error3");

    oids.oid_nelem = oid -> oid_nelem - (i = ot -> ot_name -> oid_nelem + 1);
    oids.oid_elements = oid -> oid_elements + i;

    cp = sprintoid (&oids);
    l -> retval = make_string (cp, strlen (cp));

    return l;
}

/*  */

static int  snmp_get_next (s)
register struct snmp_search *s;
{
    register struct type_SNMP_VarBindList  *vp,
					   *vp2,
					  **vpp,
					  **vpp2;
    register struct snmp_req *sr,
			     *sp;

    if (snmp_ready (0) == NOTOK || snmp_get_next_aux (s) == NOTOK)
	return NOTOK;

    vpp = &s -> s_prototype, vp = NULL;
    for (sr = s -> s_reqs; sr -> r_bindings; sr++) {
	vpp2 = &sr -> r_msg -> data -> un.get__request -> variable__bindings;

	while ((vp = *vpp) && (vp2 = *vpp2)) {
	    OID	    v = vp -> VarBind -> name,
		    v2 = vp2 -> VarBind -> name;

	    if (v -> oid_nelem > v2 -> oid_nelem
		    || bcmp ((char *) v -> oid_elements,
			     (char *) v2 -> oid_elements,
			     v -> oid_nelem
			         * sizeof v -> oid_elements[0])) {
		*vpp = vp -> next;
		vp -> next = NULL;
		free_SNMP_VarBindList (vp);

		*vpp2 = vp2 -> next;
		vp2 -> next = NULL;
		free_SNMP_VarBindList (vp2);
	    }
	    else
		vpp = &vp -> next, vpp2 = &vp2 -> next;
	}

	if (vp == NULL && (vp2 = *vpp)) {
	    snmp_diag (NULLCP, "too many responses starting with: %s\n",
		       oid2ode (vp2 -> VarBind -> name));
	    return NOTOK;
	}
    }
    if (vp) {
	snmp_diag (NULLCP, "missing variable in response");
	return NOTOK;
    }

    for (sp = s -> s_reqs; sp < sr; sp++) {
	if (sp -> r_bindings)
	    free_SNMP_VarBindList (sp -> r_bindings);
	if (sp -> r_pb)
	    pe_free (sp -> r_pb);

	if (sp -> r_bindings
	        && sp -> r_msg -> data -> un.get__request
						    -> variable__bindings) {
	    sp -> r_bindings = sp -> r_msg -> data -> un.get__request 
							-> variable__bindings;
	    sp -> r_msg -> data -> un.get__request -> variable__bindings =NULL;

	    sp -> r_pb = sp -> r_pe, sp -> r_pe = NULL;

	    free_SNMP_Message (sp -> r_msg), sp -> r_msg = NULL;
	    continue;
	}

	if (sp -> r_msg)
	    free_SNMP_Message (sp -> r_msg);
	if (sp -> r_pe)
	    pe_free (sp -> r_pe);

	bzero ((char *) sp, sizeof *sp);
    }

    for (sr--; sr >= s -> s_reqs; sr--)
	if (sr -> r_bindings)
	    for (sp = s -> s_reqs; sp < sr; sp++)
		if (!sp -> r_bindings) {
		    *sp = *sr;		/* struct copy */
		    bzero ((char *) sr, sizeof *sr);
		    break;
		}

    return OK;
}

/*  */

static int  snmp_get_next_aux (s)
register struct snmp_search *s;
{
    int	    gotone,
	    result,
	    retries,
	    status = -1;
    struct type_SNMP_Message *msg;
    register struct type_SNMP_PDU *parm;
    struct type_SNMP_VarBindList  *vp;
    register struct snmp_req *sr;
    PE	    p = NULLPE;

    vp = msgs.data -> un.get__request -> variable__bindings;
    msgs.data -> offset = type_SNMP_PDUs_get__next__request;
    for (sr = s -> s_reqs; sr -> r_bindings; sr++)
	if ((result = req_ready (sr, 1)) == NOTOK)
	    break;
    msgs.data -> un.get__request -> variable__bindings = vp;
    msgs.data -> offset = type_SNMP_PDUs_get__request;

    if (result == NOTOK) {
	snmp_diag (NULLCP, "encode_SNMP_Message: %s", PY_pepy);
	return NOTOK;
    }

    msg = NULL, gotone = 0;
    for (retries = snmp_retries; retries > 0; ) {
	int	len;
	fd_set	rfds;

	for (sr = s -> s_reqs; sr -> r_bindings; sr++) {
	    if (sr -> r_msg)
		continue;

	    if (debug > 1)
		print_SNMP_Message (sr -> r_pe, 1, NULLIP, NULLVP, NULLCP);
	    len = ps -> ps_byteno;
	    if (pe2ps (ps, sr -> r_pe) == NOTOK) {
		snmp_diag (NULLCP, "pe2ps: %s", ps_error (ps -> ps_errno));
		goto error_x;
	    }
	    if (debug > 0 && (len = ps -> ps_byteno - len) > 484)
		fprintf (stderr, "sent message of %d octets\n", len);
	}

	FD_ZERO (&rfds);
	FD_SET (snmp_fd, &rfds);

	switch (xselect (snmp_fd + 1, &rfds, NULLFD, NULLFD, snmp_timeout)) {
	    case NOTOK:
	        snmp_diag ("failed", "xselect");
		goto error_x;

	    default:
		if (FD_ISSET (snmp_fd, &rfds))
		    break;
		/* else fall... */
	    case OK:
		retries--;
		continue;
	}

again: ;
	if ((p = ps2pe (ps)) == NULLPE) {
	    snmp_diag (NULLCP, "ps2pe: %s", ps_error (ps -> ps_errno));
	    goto error_x;
	}
	if (decode_SNMP_Message (p, 1, NULLIP, NULLVP, &msg) == NOTOK) {
	    snmp_diag (NULLCP, "decode_SNMP_Message: %s", PY_pepy);
	    goto out;
	}
	if (debug > 1)
	    print_SNMP_Message (p, 1, NULLIP, NULLVP, NULLCP);

	if (msg -> data -> offset != type_SNMP_PDUs_get__response) {
	    snmp_diag (NULLCP, "unexpected message type %d",
		       msg -> data -> offset);
	    goto out;
	}
	gotone++;

	parm = msg -> data -> un.get__response;
	for (sr = s -> s_reqs; sr -> r_bindings; sr++)
	    if (parm -> request__id == sr -> r_id)
		break;
	if (!sr -> r_bindings || sr -> r_msg) {
	    if (debug > 0)
		fprintf (stderr, "%s response ID (got %ld)\n",
			 sr -> r_bindings ? "duplicate" : "unexpected",
			 (long) parm -> request__id);
	    if (msg)
		free_SNMP_Message (msg), msg = NULL;
	    if (p)
		pe_free (p), p = NULLPE;
	    goto check;
	}

	switch (status = parm -> error__status) {
	    case int_SNMP_error__status_noError:
	        break;

	    case int_SNMP_error__status_tooBig:
		{
		    register int    i;
		    register struct type_SNMP_VarBindList **vpp;
		    register struct snmp_req *sp;
		    struct snmp_req *sz = s -> s_reqs + NREQ;

		    i = 0;
		    for (vp = sr -> r_bindings; vp; vp = vp -> next)
			i++;
		    if ((i >>= 1) < 1) {
			snmp_diag (NULLCP,
				   "%s for request with single variable",
				   snmp_error (status));
			goto out;
		    }
		    for (sp = sr + 1; sp -> r_bindings; sp++)
			if (sp >= sz) {
			    snmp_diag (NULLCP,
				       "too many operations needed (%d max)",
				       NREQ - 1);
			    goto out;
			}
		    for (sz = sr + 1; sp > sz; sp--)
			*sp = *(sp - 1);	/* struct copy */
		    bzero ((char *) sp, sizeof *sp);

		    for (vpp = &sr -> r_bindings;
			     i-- > 0;
			     vpp = &((*vpp) -> next))
			continue;
		    sp -> r_bindings = *vpp, *vpp = NULL;
		    
		    vp = msgs.data -> un.get__request -> variable__bindings;
		    msgs.data -> offset = type_SNMP_PDUs_get__next__request;
		    if ((result = req_ready (sr, 0)) != NOTOK)
			result = req_ready (sp, 0);
		    msgs.data -> un.get__request -> variable__bindings = vp;
		    msgs.data -> offset = type_SNMP_PDUs_get__request;
		    if (result == NOTOK) {
			snmp_diag (NULLCP, "encode_SNMP_Message: %s", PY_pepy);
			goto out;
		    }

		    retries = snmp_retries;
		    goto check;
		}
		break;

	    default:
	    {
		char   *cp = snmp_variable (parm, parm -> error__index);

		snmp_diag (NULLCP, cp ? "%s at position %d (%s)"
				      : "%s at position %d",
			   snmp_error (status), parm -> error__index, cp);
	    }
		goto out;
	}

	pe_free (sr -> r_pe);
	sr -> r_pe = p, p = NULLPE;

	sr -> r_msg = msg, msg = NULL;

	for (sr = s -> s_reqs; sr -> r_bindings; sr++)
	    if (!sr -> r_msg)
		break;
	if (!sr -> r_bindings) {
	    assign_number (&ERROR_node -> var_value, (AWKNUM) status);

	    return OK;
	}

check: ;
	FD_ZERO (&rfds);
	FD_SET (snmp_fd, &rfds);

	if (select_dgram_socket (snmp_fd + 1, &rfds, NULLFD, NULLFD, 1) > OK)
	    goto again;
    }
    snmp_diag (NULLCP,
	       "no %sresponse within %d retries of %s%d second%s each",
	       gotone ? "acceptable " : "", snmp_retries + gotone,
	       gotone ? "up to " : "", snmp_timeout,
	       snmp_timeout != 1 ? "s" : "");
    return NOTOK;

error_x: ;
    if (ps)
	ps_free (ps), ps = NULLPS;
    if (snmp_fd != NOTOK)
	(void) close_udp_socket (snmp_fd), snmp_fd = NOTOK;

out: ;
    if (p)
	pe_free (p);

    return NOTOK;
}

/*  */

static int  req_ready (sr, do_val)
register struct snmp_req *sr;
int	do_val;
{
    register struct type_SNMP_Message *msg = &msgs;
    register struct type_SNMP_PDU *parm = msg -> data -> un.get__request;

    if (do_val) {
	register struct type_SNMP_VarBindList *vp;

	for (vp = sr -> r_bindings; vp; vp = vp -> next) {
	    register struct type_SNMP_VarBind *v = vp -> VarBind;

	    if (v -> value)
		pe_free (v -> value);
	    if ((v -> value = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM,
					PE_PRIM_NULL)) == NULLPE)
		fatal ("pe_alloc: out of memory");
	}
    }

    if (snmp_id >= 0x7fffffff)
	snmp_id = 0;
    snmp_id++;

    parm -> request__id = sr -> r_id = snmp_id;
    parm -> variable__bindings = sr -> r_bindings;

    if (sr -> r_msg)
	free_SNMP_Message (sr -> r_msg), sr -> r_msg = NULL;
    if (sr -> r_pe)
	pe_free (sr -> r_pe), sr -> r_pe = NULL;

    return encode_SNMP_Message (&sr -> r_pe, 1, 0, NULLCP, &msgs);
}

/*    DECODE */

static NODE *make_octet_node (base, len)
char   *base;
int	len;
{
    register char *bp,
		  *cp,
		  *ep;
    char   *s = "";
    register NODE *r;

    r = newnode (Node_val);
    emalloc (r -> stptr, char *, len * 3 + 1, "make_octet_node");
    bp = r -> stptr;
    for (ep = (cp = base) + len; cp < ep; cp++, s = ":") {
	(void) sprintf (bp, "%s%02x", s, *cp & 0xff);
	bp += strlen (bp);
    }
    *bp = NULL;		/* in case len == 0 */
    r -> stlen = bp - r -> stptr;
    r -> stref = 1;
    r -> flags |= STR | MALLOC;

    return r;
}


static int  f_integer (x, pe)
NODE  **x;
PE	pe;
{
    integer	i = prim2num (pe);

    if (i == NOTOK && pe -> pe_errno != PE_ERR_NONE) {
	(void) strcpy (PY_pepy, pe_error (pe -> pe_errno));
	return NOTOK;
    }

    *x = make_number ((AWKNUM) i);

    return OK;
}


static int  f_octets (x, pe)
NODE  **x;
PE	pe;
{
    struct qbuf *qb = prim2qb (pe);

    if (qb == NULL || qb_pullup (qb) == NOTOK) {
	(void) strcpy (PY_pepy, qb ? "qb_pullup: out of memory"
				   : pe_error (pe -> pe_errno));
	return NOTOK;
    }

    *x = make_octet_node (qb -> qb_forw -> qb_data, qb -> qb_forw -> qb_len);

    qb_free (qb);

    return OK;
}


static int  f_display (x, pe)
NODE  **x;
PE	pe;
{
    struct qbuf *qb = prim2qb (pe);

    if (qb == NULL || qb_pullup (qb) == NOTOK) {
	(void) strcpy (PY_pepy, qb ? "qb_pullup: out of memory"
				   : pe_error (pe -> pe_errno));
	return NOTOK;
    }

    *x = make_string (qb -> qb_forw -> qb_data, qb -> qb_forw -> qb_len);

    qb_free (qb);

    return OK;
}


static int  f_objectID (x, pe)
NODE  **x;
PE	pe;
{
    char   *cp;
    OID	    oid = prim2oid (pe);

    if (oid == NULLOID) {
	(void) strcpy (PY_pepy, pe_error (pe -> pe_errno));
	return NOTOK;
    }
    cp = sprintoid (oid);

    *x = make_string (cp, strlen (cp));

    return OK;
}


/* ARGSUSED */

static int  f_null (x, pe)
NODE  **x;
PE	pe;
{
    *x = make_str_node ("NULL", 4, 0);

    return OK;
}


static int  f_ipaddr (x, pe)
NODE  **x;
PE	pe;
{
    char    ipaddr[16];
    struct type_SNMP_IpAddress *ip;
    struct qbuf *qb;

    if (decode_SNMP_IpAddress (pe, 1, NULLIP, NULLVP, &ip) == NOTOK)
	return NOTOK;
    if (qb_pullup (ip) == NOTOK) {
	(void) strcpy (PY_pepy, "qb_pullup: out of memory");
	free_SNMP_IpAddress (ip);
	return NOTOK;
    }
    if ((qb = ip -> qb_forw) -> qb_len != 4) {
	(void) sprintf (PY_pepy,
			"IpAddress is wrong length (got %d, wanted 4)",
			qb -> qb_len);
	free_SNMP_IpAddress (ip);
	return NOTOK;
    }
    (void) sprintf (ipaddr, "%d.%d.%d.%d",
		    qb -> qb_data[0] & 0xff, qb -> qb_data[1] & 0xff,
		    qb -> qb_data[2] & 0xff, qb -> qb_data[3] & 0xff);

    *x = make_str_node (ipaddr, strlen (ipaddr), 0);

    free_SNMP_IpAddress (ip);

    return OK;
}


extern	u_long prim2ulong ();

static int  f_ulong (x, pe)
NODE  **x;
PE	pe;
{
    u_long    i = prim2ulong (pe);

    if (i == 0 && pe -> pe_errno != PE_ERR_NONE) {
	(void) strcpy (PY_pepy, pe_error (pe -> pe_errno));
	return NOTOK;
    }

    *x = make_number ((AWKNUM) i);

    return OK;
}


static int  f_clnpaddr (x, pe)
NODE  **x;
PE	pe;
{
    int	    len;
    struct type_SNMP_ClnpAddress *clnp;
    struct qbuf *qb;

    if (decode_SNMP_ClnpAddress (pe, 1, NULLIP, NULLVP, &clnp) == NOTOK)
	return NOTOK;
    if (qb_pullup (clnp) == NOTOK) {
	(void) strcpy (PY_pepy, "qb_pullup: out of memory");
	free_SNMP_ClnpAddress (clnp);
	return NOTOK;
    }
    qb = clnp -> qb_forw;
    if ((len = qb -> qb_data[0] & 0xff) >= qb -> qb_len)
	len = qb -> qb_len - 1;

    *x = make_octet_node (qb -> qb_data + 1, len);

    free_SNMP_ClnpAddress (clnp);

    return OK;
}

/*    MISC */

static	snmp_ready (do_id)
int	do_id;
{
    int	    changed = 0;
    char   *pp;
    struct sockaddr_in lo_socket;
    register struct sockaddr_in *lsock = &lo_socket;
    register struct sockaddr_in *isock = &in_socket;
    register struct hostent *hp;
    register struct servent *sp;
    register NODE   *tmp;

    deref = DIAGNOSTIC_node -> var_value;
    do_deref ();

    DIAGNOSTIC_node -> var_value = Nnull_string;

    if ((snmp_retries = (int) RETRIES_node -> var_value -> numbr) <= 0)
	snmp_retries = 1;
    if ((snmp_timeout = (int) TIMEOUT_node -> var_value -> numbr) <= 0)
	snmp_timeout = 1;

    if (do_id) {
	if (snmp_id >= 0x7fffffff)
	    snmp_id = 0;
	snmp_id++;
    }

    if (snmp_fd == NOTOK || ps == NULLPS)
	changed++;

    tmp = force_string (AGENT_node -> var_value);
    if (snmp_agent == NULL || strcmp (snmp_agent, tmp -> stptr)) {
	if (snmp_agent)
	    free (snmp_agent), snmp_agent = NULL;

	emalloc (snmp_agent, char *, strlen (tmp -> stptr) + 1, "snmp_ready1");
	(void) strcpy (snmp_agent, tmp -> stptr);

	changed++;
    }

    tmp = force_string (COMMUNITY_node -> var_value);
    if (snmp_community == NULL || strcmp (snmp_community, tmp -> stptr)) {
	register struct type_SNMP_Message *msg = &msgs;

	if (snmp_community)
	    free (snmp_community), snmp_community = NULL;

	emalloc (snmp_community, char *, strlen (tmp -> stptr) + 1,
		 "snmp_ready2");
	(void) strcpy (snmp_community, tmp -> stptr);
	if ((msg -> community = str2qb (snmp_community,
					strlen (snmp_community), 1)) == NULL) {
	    snmp_diag (NULLCP, "str2qb: out of memory");
	    free (snmp_community), snmp_community = NULL;
	    return NOTOK;
	}
    }

    if (changed) {
	if (ps)
	    ps_free (ps), ps = NULLPS;
	if (snmp_fd != NOTOK)
	    (void) close_udp_socket (snmp_fd), snmp_fd = NOTOK;
    }
    else
	return OK;

    bzero ((char *) lsock, sizeof *lsock);
    if ((hp = gethostbystring (pp = strcmp (snmp_agent, "localhost")
					? getlocalhost () : "localhost"))
	    == NULL) {
	snmp_diag (NULLCP, "%s: unknown host", pp);
	return NOTOK;
    }
    lsock -> sin_family = hp -> h_addrtype;
    inaddr_copy (hp, lsock);
    if ((snmp_fd = start_udp_client (lsock, 0, 0, 0)) == NOTOK) {
	snmp_diag ("failed", "start_udp_server");
	return NOTOK;
    }

    if ((hp = gethostbystring (pp = snmp_agent)) == NULL) {
	struct TSAPaddr *ta;
	struct NSAPaddr *na;

	if ((ta = str2taddr (snmp_agent))
	        && ta -> ta_naddr > 0
	        && (na = ta -> ta_addrs) -> na_stack == NA_TCP) {
	    snmp_portno = na -> na_port;

	    if (hp = gethostbystring (pp = na -> na_domain))
		goto got_host;
	}

	snmp_diag (NULLCP, "%s: unknown host", pp);
	return NOTOK;
    }
got_host: ;
    if (snmp_portno == 0)
	snmp_portno = (sp = getservbyname ("snmp", "udp"))
			    ? sp -> s_port
			    : htons ((u_short) 161);

    bzero ((char *) isock, sizeof *isock);
    isock -> sin_family = hp -> h_addrtype;
    inaddr_copy (hp, isock);	
    isock -> sin_port = snmp_portno;

    if (*snmp_community == '/' && snmp_map (isock) == NOTOK)
	return NOTOK;

    if (join_udp_server (snmp_fd, isock) == NOTOK) {
	snmp_diag ("failed", "join_udp_server");
	return NOTOK;
    }

    if ((ps = ps_alloc (dg_open)) == NULLPS
	    || dg_setup (ps, snmp_fd, MAXDGRAM, read_udp_socket,
			 write_udp_socket, check_udp_socket) == NOTOK) {
	if (ps == NULLPS)
	    snmp_diag (NULLCP, "ps_alloc: out of memory");
	else
	    snmp_diag (NULLCP, "dg_setup: %s", ps_error (ps -> ps_errno));

	return NOTOK;
    }

#ifndef	SYS5
    snmp_id = ((int) random ()) & 0x7fffffff;
#else
    snmp_id = ((int) rand ()) & 0x7fffffff;
#endif

    return OK;
}

/*  */

/* Reads an IP address to community mapping file.  Use UNIX modes for
   protection of information therein...

   Syntax:

	<netaddr>	[<netmask>]	<community>

	Each token is seperated by LWSP, though double-quotes may be used to
	prevent separation.

 */

static	snmp_map (isock)
struct sockaddr_in *isock;
{
    int	    result = NOTOK;
    u_long	hostaddr,
		netmask,
		netaddr;
    register char *cp;
    char    buffer[BUFSIZ + 1],
	   *vec[NVEC + 1];
    FILE   *fp;

    if ((fp = fopen (snmp_community, "r")) == NULL) {
	snmp_diag (snmp_community, "unable to read");
	return NOTOK;
    }

    hostaddr = isock -> sin_addr.s_addr;

    while (fgets (buffer, sizeof buffer, fp)) {
	if (*buffer == '#')
	    continue;
	if (cp = index (buffer, '\n'))
	    *cp = NULL;
	bzero ((char *) vec, sizeof vec);
	switch (str2vec (buffer, vec)) {
	    case 3:
		netmask = inet_addr (vec[1]);
	        cp = vec[2];
		break;

	    case 2:
		netmask = 0xffffffff;
	        cp = vec[1];
	        break;

	    default:
		continue;
	}
	if ((netaddr = inet_network (vec[0])) == NOTOK)
	    continue;
	netaddr = ntohl (netaddr);
	if (!(netaddr & 0xff000000))
	    netaddr <<=   (netaddr & 0x00ff0000) ? 8
		        : (netaddr & 0x0000ff00) ? 16
		        :			   24;
	netaddr = htonl (netaddr);
	if ((hostaddr & netmask) != netaddr)
	    continue;

	if ((msgs.community = str2qb (cp, strlen (cp), 1)) == NULL) {
	    snmp_diag (NULLCP, "str2qb: out of memory");
	    goto done;
	}

	result = OK;
	break;
    }
    if (result == NOTOK)
	snmp_diag (NULLCP, "no match for IP-address in %s", snmp_community);

done: ;
    (void) fclose (fp);

    return result;
}

/*  */

#ifndef	lint
static	snmp_diag (va_alist)
va_dcl
{
    char   *what,
	    buffer[BUFSIZ];
    va_list ap;

    va_start (ap);

    what = va_arg (ap, char *);

    _asprintf (buffer, what, ap);

    va_end (ap);

    if (debug > 0)
	fprintf (stderr, "%s\n", buffer);

    deref = DIAGNOSTIC_node -> var_value;
    do_deref ();

    DIAGNOSTIC_node -> var_value = make_string (buffer, strlen (buffer));
}
#else
/* VARARGS */

static	snmp_diag (what, fmt)
char   *what,
       *fmt;
{
    snmp_diag (what, fmt);
}
#endif

/*  */

char   *snmp_name (ptr)
NODE   *ptr;
{
    return ((OT) (ptr -> magic)) -> ot_text;
}

/*  */

static char *errors[] = {
    "noError", "tooBig", "noSuchName", "badValue", "readOnly", "genErr"
};


static char *snmp_error (i)
int	i;
{
    static char buffer[BUFSIZ];

    if (0 < i && i < sizeof errors / sizeof errors[0])
	return errors[i];
    (void) sprintf (buffer, "error %d", i);

    return buffer;
}


static char *snmp_variable (parm, idx)
register struct type_SNMP_PDU *parm;
int	idx;
{
    register struct type_SNMP_VarBindList *vp;

    if (idx <= 0 || (vp = parm -> variable__bindings) == NULL)
	return NULL;
    for (idx--; idx > 0; idx--)
	if ((vp = vp -> next) == NULL)
	    return NULL;

    return oid2ode (vp -> VarBind -> name);
}
#endif	/* SNMP */
