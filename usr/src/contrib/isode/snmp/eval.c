/* eval.c - MIB realization of the EVAL group */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/eval.c,v 7.7 91/02/22 09:43:13 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/eval.c,v 7.7 91/02/22 09:43:13 mrose Interim $
 *
 *
 * $Log:	eval.c,v $
 * Revision 7.7  91/02/22  09:43:13  mrose
 * Interim 6.8
 * 
 * Revision 7.6  91/01/13  11:05:42  mrose
 * update
 * 
 * Revision 7.5  91/01/12  21:38:32  mrose
 * typo
 * 
 * Revision 7.4  91/01/12  21:25:41  mrose
 * again
 * 
 * Revision 7.3  91/01/12  21:22:49  mrose
 * update
 * 
 * Revision 7.2  90/12/18  10:13:19  mrose
 * update
 * 
 * Revision 7.1  90/11/20  15:31:59  mrose
 * update
 * 
 * Revision 7.0  90/11/18  09:30:47  mrose
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


#include <ctype.h>
#include <stdio.h>
#include "mib.h"

/*    FUNCTION MIB */

#define	NSTACK	10

static	integer	*tos;
static	integer	*roof;
static	integer	 fstack[NSTACK + 1];


#define	NEXPR	10

struct expr {
    PStream e_ps;

    int	    e_eval;	/* exprEval */

    char   *e_expr;	/* exprExpr */
    int	    e_size;

    int	    e_status;	/* exprStatus */
#define	E_noError	0
#define	E_divide	1
#define	E_overflow	2
#define	E_underflow	3
#define	E_noSuchName	4
#define	E_notAnInteger	5
#define	E_other		6

			/* exprHints */
    char    e_hints[255 + 1];

    struct {
	char   *expr;

	int	size;
    }	    e_save;
};

static integer	exprNumber = 0;

static struct expr exprs[NEXPR];
static struct expr *curexpr = NULL;
static struct expr *roofexpr;

/*  */

#define	functAdd	 0
#define	functSub	 1
#define	functMul	 2
#define	functDiv	 3
#define	functMod	 4
#define	functNeg	 5
#define	functXch	 6
#define	functClr	 7
#define	functDup	 8
#define	functPop	 9
#define	functAbs	10
#define	functSgn	11
#define	functMin	12
#define	functAnd	13
#define	functOr		14
#define	functNot	15


static int  o_funct (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifvar;
    integer arg1,
	    arg2;
    register OT	    ot = oi -> oi_type;

    ifvar = (int) ot -> ot_info;
    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    if (curexpr == NULL)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if (curexpr == NULL)
		return NOTOK;
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

#define	get_arg(a) \
    if (tos <= fstack + 1) { \
	curexpr -> e_status = E_underflow; \
	(void) strcpy (curexpr -> e_hints, "stack underflow"); \
	return NOTOK; \
    } \
    (a) = *--tos;

    switch (ifvar) {
	case functAdd:
	case functSub:
	case functMul:
	case functDiv:
	case functMod:
	case functXch:
	case functDup:
	case functMin:
	case functAnd:
	case functOr:
		get_arg (arg2);
		get_arg (arg1);
		break;

	case functNeg:
	case functPop:
	case functAbs:
	case functSgn:
	case functNot:
		get_arg (arg1);
		break;

	case functClr:
		arg1 = 0;
		break;

	default:
	    return int_SNMP_error__status_genErr;
    }
#undef	get_arg

    switch (ifvar) {
	case functAdd:
	    arg1 += arg2;
	    break;

	case functSub:
	    arg1 -= arg2;
	    break;

	case functMul:
	    arg1 *= arg2;
	    break;

	case functDiv:
	    if (arg2 == 0) {
		curexpr -> e_status = E_divide;
		(void) strcpy (curexpr -> e_hints, "division by zero");
		return NOTOK;
	    }
	    arg1 /= arg2;
	    break;

	case functMod:
	    if (arg2 == 0) {
		curexpr -> e_status = E_divide;
		(void) strcpy (curexpr -> e_hints, "modulus by zero");
		return NOTOK;
	    }
	    arg1 %= arg2;
	    break;

	case functNeg:
	    arg1 = -arg1;
	    break;

	case functXch:
	    *tos++ = arg2;
	    break;

	case functClr:
	    tos = fstack;
	    return NOTOK;

	case functDup:
	    *tos++ = arg1;
	    break;

	case functPop:
	    arg1 = tos <= fstack ? 0 : *--tos;
	    return NOTOK;

	case functAbs:
	    if (arg1 < 0)
		arg1 = -arg1;
	    break;

	case functSgn:
	    arg1 = arg1 > 0 ? 1 : arg1 < 0 ? -1 : 0;
	    break;

	case functMin:
	    if (arg2 < arg1)
		arg1 = arg2;
	    break;

	case functAnd:
	    arg1 &= arg2;
	    break;

	case functOr:
	    arg1 |= arg2;
	    break;

	case functNot:
	    arg1 = !arg1;
	    break;

	default:
	    return int_SNMP_error__status_noSuchName;
    }

    return o_integer (oi, v, arg1);
}

/*  */

/* assumes that exprEval occurs first in variable-bindings of get... */

#define	exprIndex	0
#define	exprEval	1
#define	exprExpr	2
#define	exprStatus	3
#define	exprHints	4


static int  o_expressions (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifnum,
	    ifvar;
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;
    register struct expr *e;

    ifvar = (int) ot -> ot_info;
    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    if (oid -> oid_nelem != ot -> ot_name -> oid_nelem + 1)
		return int_SNMP_error__status_noSuchName;
	    if ((ifnum = oid -> oid_elements[oid -> oid_nelem - 1]) == 0
		    || ifnum > exprNumber)
		return int_SNMP_error__status_noSuchName;
	    if ((e = exprs + ifnum - 1) -> e_expr == NULL)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		OID	new;

		for (e = exprs; e < roofexpr; e++)
		    if (e -> e_expr)
			break;
		if (e >= roofexpr)
		    return NOTOK;
		ifnum = (e - exprs) + 1;

		if ((new = oid_extend (oid, 1)) == NULLOID)
		    return NOTOK;
		new -> oid_elements[new -> oid_nelem - 1] = ifnum;

		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;
	    }
	    else {
		int	i = ot -> ot_name -> oid_nelem;

		if ((ifnum = oid -> oid_elements[i]) >= NEXPR)
		    return NOTOK;
		for (e = exprs + ifnum; e < roofexpr; e++)
		    if (e -> e_expr)
			break;
		if (e >= roofexpr)
		    return NOTOK;
		ifnum = (e - exprs) + 1;

		oid -> oid_elements[i] = ifnum;
		oid -> oid_nelem = i + 1;
	    }
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    switch (ifvar) {
	case exprIndex:
	    return o_integer (oi, v, ifnum);

	case exprEval:
	    e -> e_eval = 0, e -> e_status = E_noError, e -> e_hints[0] = NULL;
	    (void) eval_expr (curexpr = e);
	    curexpr = NULL;
	    return o_integer (oi, v, e -> e_eval);

	case exprExpr:
	    return o_string (oi, v, e -> e_expr, e -> e_size);

	case exprStatus:
	    return o_integer (oi, v, e -> e_status);

	case exprHints:
	    return o_string (oi, v, e -> e_hints, strlen (e -> e_hints));

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

static int  eval_expr (e)
register struct expr *e;
{
    PElementClass class;
    PElementForm  form;
    PElementID	  id;
    PElementLen	  len;
    PS	    ps = &e -> e_ps;

    roof = (tos = fstack) + (sizeof fstack / sizeof fstack[0]);
    tos++;

    bzero ((char *) ps, sizeof *ps);
    if (str_open (ps) == NOTOK) {
	e -> e_status = E_other;
	(void) strcpy (e -> e_hints, "str_open failed");
	return NOTOK;
    }
    if (str_setup (ps, e -> e_expr, e -> e_size, 1) == NOTOK) {
	e -> e_status = E_other;
	(void) sprintf (e -> e_hints, "str_setup failed: %s",
			ps_error (ps -> ps_errno));
	return NOTOK;
    }

    if (read_tl (e, &class, &form, &id, &len) == NOTOK)
	return NOTOK;
    if (PE_ID (class, id) != PE_ID (PE_CLASS_UNIV, PE_CONS_SEQ)) {
	e -> e_status = E_other;
	(void) sprintf (e -> e_hints, "unexpected TAG: %d/%d", class, id);
	return NOTOK;
    }

    while (ps -> ps_cnt > 0) {
	integer	i;
	OID	oid;

	if (read_tl (e, &class, &form, &id, &len) == NOTOK)
	    return NOTOK;
	switch (PE_ID (class, id)) {
	    case PE_ID (PE_CLASS_UNIV, PE_PRIM_INT):
		if (read_long (e, ps -> ps_ptr, len, form, &i) == NOTOK)
		    return NOTOK;
		break;

	    case PE_ID (PE_CLASS_UNIV, PE_PRIM_OID):
		if (read_oid (e, ps -> ps_ptr, len, form, &oid) == NOTOK)
		    return NOTOK;
		if (get_var_value (e, oid, &i) == NOTOK)
		    return NOTOK;
		break;

	    default:
		e -> e_status = E_other;
		(void) sprintf (e -> e_hints, "unexpected TAG: %d/%d",
				class, id);
		return NOTOK;
	}
	ps -> ps_ptr += len, ps -> ps_cnt -= len;

	if (tos < roof)
	    *tos++ = i;
	else {
	    e -> e_status = E_overflow;
	    (void) strcpy (e -> e_hints, "stack overflow");
	    return NOTOK;
	}
    }

    if (tos <= fstack) {
	e -> e_status = E_underflow;
	(void) strcpy (e -> e_hints, "stack underflow");
	return NOTOK;
    }
    e -> e_eval = *--tos;

    if (tos > fstack + 1 && e -> e_hints[0] == NULL)
	(void) sprintf (e -> e_hints,
		"%d items left on stack after evaluating expression",
		tos - fstack - 1);

    return OK;
}

/*  */

static int  read_tl (e, class, form, id, len)
struct expr *e;
PElementClass *class;
PElementForm  *form;
PElementID    *id;
PElementLen   *len;
{
    PS	    ps = &e -> e_ps;

    if (ps_read_id (ps, 0, class, form, id) == NOTOK) {
	e -> e_status = E_other;
	(void) sprintf (e -> e_hints, "error reading TAG info: %s",
			ps_error (ps -> ps_errno));
	return NOTOK;
    }
    if (ps_read_len (ps, len) == NOTOK) {
	e -> e_status = E_other;
	(void) sprintf (e -> e_hints, "error reading LEN info: %s",
			ps_error (ps -> ps_errno));
	return NOTOK;
    }

    return OK;
}

/*  */

static int  read_long (e, base, len, form, result)
struct expr *e;
char   *base;
int	len;
PElementForm  form;
integer *result;
{
    register integer    i;
    register PElementData dp,
			  ep;

    if (form != PE_FORM_PRIM) {
	e -> e_status = E_other;
	(void) sprintf (e -> e_hints, "integer: %s", pe_error (PE_ERR_PRIM));
	return NOTOK;
    }
    if (len > sizeof (i)) {
	e -> e_status = E_other;
	(void) sprintf (e -> e_hints, "integer: %s", pe_error (PE_ERR_OVER));
	return NOTOK;
    }

    i = (*(dp = (PElementData) base) & 0x80) ? (-1) : 0;
    for (ep = dp + len; dp < ep;)
	i = (i << 8) | (*dp++ & 0xff);

    *result = i;

    return OK;
}

/*  */

static int  read_oid (e, base, len, form, ox)
struct expr *e;
char   *base;
int	len;
PElementForm  form;
OID   *ox;
{
    register unsigned int i,
			 *ip;
    register PElementData dp,
			  ep;
    static OIDentifier oid;
    register OID	o = &oid;

    if (form != PE_FORM_PRIM
	    || (dp = (PElementData) base) == NULLPED
	    || len == 0) {
	e -> e_status = E_other;
	(void) sprintf (e -> e_hints, "oid: %s", pe_error (PE_ERR_PRIM));
	return NOTOK;
    }
    ep = dp + len;

    if (o -> oid_elements)
	free ((char *) o -> oid_elements);

    for (i = 1; dp < ep; i++) {	/* another whacko OSI encoding... */
	if (*dp == 0x80) {
	e -> e_status = E_other;
	    (void) sprintf (e -> e_hints, "oid: %s", pe_error (PE_ERR_OID));
	    return NOTOK;
	}

	while (*dp++ & 0x80)
	    if (dp > ep) {
		e -> e_status = E_other;
		(void) sprintf (e -> e_hints, "oid: %s",
				pe_error (PE_ERR_OID));
		return NOTOK;
	    }
    }

    if ((ip = (unsigned int *) malloc ((i + 1) * sizeof *ip)) == NULL) {
	e -> e_status = E_other;
	(void) sprintf (e -> e_hints, "oid: %s", pe_error (PE_ERR_NMEM));
	return NOTOK;
    }
    o -> oid_elements = ip, o -> oid_nelem = i;
    
    for (dp = (PElementData) base; dp < ep; ) {
	i = 0;
	do {
	    i <<= 7; 
	    i |= *dp & 0x7f;
	} while (*dp++ & 0x80);

	if (ip != o -> oid_elements)
	    *ip++ = i;
	else
	    if (i < 40)
		*ip++ = 0, *ip++ = i;
	    else
		if (i < 80)
		    *ip++ = 1, *ip++ = i - 40;
		else
		    *ip++ = 2, *ip++ = i - 80;
    }

    *ox = o;

    return OK;
}

/*  */

static int  get_var_value (e, oid, i)
struct expr *e;
OID	oid;
integer *i;
{
    int	    status;
    integer *result;
    OI	    oi;
    OS	    os;
    OT	    ot;
    struct type_SNMP_VarBind *v;

    if ((oi = name2inst (oid)) == NULL) {
	e -> e_status = E_noSuchName;
	(void) sprintf (e -> e_hints, "variable %s unknown", oid2ode (oid));
	return NOTOK;
    }
    if ((ot = oi -> oi_type) == NULL) {
	e -> e_status = E_noSuchName;
	(void) sprintf (e -> e_hints, "no object type for variable %s",
			oid2ode (oid));
	return NOTOK;
    }
    if ((os = ot -> ot_syntax) == NULL) {
	e -> e_status = E_noSuchName;
	(void) sprintf (e -> e_hints, "no object syntax for variable %s",
			oid2ode (oid));
	return NOTOK;
    }

    if (!os -> os_data2) {
	e -> e_status = E_notAnInteger;
	(void) sprintf (e -> e_hints, "variable %s is not integer-valued",
			oid2ode (oid));
	return NOTOK;
    }

    if (ot -> ot_getfnx == NULL) {
	e -> e_status = E_noSuchName;
	(void) sprintf (e -> e_hints, "no get method for variable %s",
			oid2ode (oid));
	return NOTOK;
    }

    if ((v = (struct type_SNMP_VarBind *) calloc (1, sizeof *v)) == NULL
	    || (v -> name = oid_cpy (oid)) == NULL
	    || (v -> value = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM,
				       PE_PRIM_NULL)) == NULL) {
	if (v)
	    free_SNMP_VarBind (v);
	e -> e_status = E_other;
	(void) sprintf (e -> e_hints, "out of memory");
	return NOTOK;
    }
    
    if ((status = (*ot -> ot_getfnx) (oi, v, type_SNMP_PDUs_get__request))
	    != int_SNMP_error__status_noError) {
	if (e -> e_hints[0] == NULL)
	    (void) sprintf (e -> e_hints, "%s: %d", oid2ode (oid), status);
losing: ;
	free_SNMP_VarBind (v);
	return NOTOK;
    }

    if ((*os -> os_decode) (&result, v -> value) == NOTOK) {
	e -> e_status = E_other;
	(void) sprintf (e -> e_hints, "%s: decoding error!", oid2ode (oid));
	goto losing;
    }
    *i = *result;
    free ((char *) result);
    free_SNMP_VarBind (v);

    return OK;
}

/*  */

static int  s_expressions (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifnum
#ifndef	lint
		 ,
	    ifvar
#endif
		 ;
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;
    register OS	    os = ot -> ot_syntax;
    register struct expr *e;
    struct qbuf *qb;

#ifndef	lint
    ifvar = (int) ot -> ot_info;
#endif
    switch (offset) {
	case type_SNMP_PDUs_set__request:
	case type_SNMP_PDUs_commit:
	case type_SNMP_PDUs_rollback:
	    if (oid -> oid_nelem != ot -> ot_name -> oid_nelem + 1)
		return int_SNMP_error__status_noSuchName;
	    if ((ifnum = oid -> oid_elements[oid -> oid_nelem - 1]) == 0
		    || ifnum > exprNumber + 1)
		return int_SNMP_error__status_noSuchName;
	    e = exprs + ifnum - 1;
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    if (os == NULLOS) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"no syntax defined for object \"%s\"", ot -> ot_text);

	return int_SNMP_error__status_genErr;
    }

    switch (offset) {
	case type_SNMP_PDUs_set__request:
	    if (e -> e_save.expr)
		free (e -> e_save.expr), e -> e_save.expr = NULL;
	    if ((*os -> os_decode) ((caddr_t *) &qb, v -> value) == NOTOK)
		return int_SNMP_error__status_badValue;
	    e -> e_save.expr = qb2str (qb);
	    e -> e_save.size = qb -> qb_len;
	    (*os -> os_free) (qb);
	    if (e -> e_save.expr == NULL)
		return int_SNMP_error__status_genErr;
	    if (e -> e_save.size == 0)
		free (e -> e_save.expr), e -> e_save.expr = NULL;
	    break;

	case type_SNMP_PDUs_commit:
	    if (e -> e_expr)
		free (e -> e_expr);
	    e -> e_expr = e -> e_save.expr, e -> e_save.expr = NULL;
	    e -> e_size = e -> e_save.size;
	    for (e = exprs + NEXPR - 1; e >= exprs; e--)
		if (e -> e_expr)
		    break;
	    exprNumber = (e - exprs) + 1;
	    break;

	case type_SNMP_PDUs_rollback:
	    if (e -> e_save.expr)
		free (e -> e_save.expr), e -> e_save.expr = NULL;
	    break;
    }

    return int_SNMP_error__status_noError;
}

/*  */

init_eval () {
    register OT	    ot;

    roof = (tos = fstack) + (sizeof fstack / sizeof fstack[0]);
    roofexpr = exprs + NEXPR;

    bzero ((char *) exprs, sizeof exprs);

    {
	OS	os;

	if (os = text2syn ("INTEGER"))
	    os -> os_data2 = 1;

	if (os = text2syn ("Counter"))
	    os -> os_data2 = 1;

	if (os = text2syn ("Gauge"))
	    os -> os_data2 = 1;

	if (os = text2syn ("TimeTicks"))
	    os -> os_data2 = 1;
    }

    if (ot = text2obj ("functAdd"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functAdd;
    if (ot = text2obj ("functSub"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functSub;
    if (ot = text2obj ("functMul"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functMul;
    if (ot = text2obj ("functDiv"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functDiv;
    if (ot = text2obj ("functMod"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functMod;
    if (ot = text2obj ("functNeg"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functNeg;
    if (ot = text2obj ("functXch"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functXch;
    if (ot = text2obj ("functClr"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functClr;
    if (ot = text2obj ("functDup"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functDup;
    if (ot = text2obj ("functPop"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functPop;
    if (ot = text2obj ("functAbs"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functAbs;
    if (ot = text2obj ("functSgn"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functSgn;
    if (ot = text2obj ("functMin"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functMin;
    if (ot = text2obj ("functAnd"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functAnd;
    if (ot = text2obj ("functOr"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functOr;
    if (ot = text2obj ("functNot"))
	ot -> ot_getfnx = o_funct,
	ot -> ot_info = (caddr_t) functNot;

    if (ot = text2obj ("exprNumber"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &exprNumber;
    if (ot = text2obj ("exprIndex"))
	ot -> ot_getfnx = o_expressions,
	ot -> ot_info = (caddr_t) exprIndex;
    if (ot = text2obj ("exprEval"))
	ot -> ot_getfnx = o_expressions,
	ot -> ot_info = (caddr_t) exprEval;
    if (ot = text2obj ("exprExpr"))
	ot -> ot_getfnx = o_expressions,
	ot -> ot_setfnx = s_expressions,
	ot -> ot_info = (caddr_t) exprExpr;	
    if (ot = text2obj ("exprStatus"))
	ot -> ot_getfnx = o_expressions,
	ot -> ot_info = (caddr_t) exprStatus;
    if (ot = text2obj ("exprHints"))
	ot -> ot_getfnx = o_expressions,
	ot -> ot_info = (caddr_t) exprHints;
}

/*  */

int	f_expression (vec)
char  **vec;
{
    int	    i;
    register char *cp;
    register struct expr *e;
    PE	    pe;
    PS	    ps;

    vec++;

    if (sscanf (*vec, "%d", &i) != 1) {
invalid: ;
	advise (LLOG_EXCEPTIONS, NULLCP, "invalid expression index: %s", *vec);
	return NOTOK;
    }
    if (i <= 0 || i > NEXPR)
	goto invalid;
    vec++;

    e = exprs + i - 1;
    if (e -> e_expr)
	free (e -> e_expr), e -> e_expr = NULL;

    if ((pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_CONS, PE_CONS_SEQ)) == NULL)
	adios (NULLCP, "pe_alloc: out of memory");
    while (cp = *vec++) {
	OID	oid;
	PE	p;

	if (isdigit (*cp)) {
	    if (sscanf (cp, "%ld", &i) != 1) {
		advise (LLOG_EXCEPTIONS, NULLCP, "bad integer-value: %s", cp);
		return NOTOK;
	    }
	    p = num2prim ((integer) i, PE_CLASS_UNIV, PE_PRIM_INT);
	}
	else {
	    if ((oid = text2oid (cp)) == NULL) {
		advise (LLOG_EXCEPTIONS, NULLCP, "unknown variable: %s", cp);
		return NOTOK;
	    }
	    p = obj2prim (oid, PE_CLASS_UNIV, PE_PRIM_OID);
	    oid_free (oid);
	}

	if (seq_add (pe, p, NOTOK) == NOTOK)
	    adios (NULLCP, "seq_add: out of memory");
    }

    if ((ps = ps_alloc (str_open)) == NULLPS)
	adios (NULLCP, "ps_alloc: failed");
    if (str_setup (ps, NULLCP, BUFSIZ, 0) == NOTOK)
	adios (NULLCP, "std_setup: %s", ps_error (ps -> ps_errno));
    if (pe2ps (ps, pe) == NOTOK)
	adios (NULLCP, "pe2ps: %s", ps_error (ps -> ps_errno));

    e -> e_size = ps -> ps_ptr - (e -> e_expr = ps -> ps_base);

    ps -> ps_base = NULL, ps -> ps_cnt = 0;
    ps -> ps_ptr = NULL, ps -> ps_bufsiz = 0;

    ps_free (ps);

    if (debug)
	vunknown (pe);
    pe_free (pe);

    if (i > exprNumber)
	exprNumber = i;

    return OK;
}
