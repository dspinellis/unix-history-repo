/* prnt.c */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepsy/RCS/prnt.c,v 7.9 91/02/22 09:49:54 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/pepsy/RCS/prnt.c,v 7.9 91/02/22 09:49:54 mrose Interim $
 *
 *
 * $Log:	prnt.c,v $
 * Revision 7.9  91/02/22  09:49:54  mrose
 * Interim 6.8
 * 
 * Revision 7.8  90/12/23  17:25:20  mrose
 * patches
 * 
 * Revision 7.7  90/11/11  09:58:56  mrose
 * touch-up
 * 
 * Revision 7.6  90/11/04  19:21:00  mrose
 * update
 * 
 * Revision 7.5  90/10/23  20:43:16  mrose
 * update
 * 
 * Revision 7.4  90/08/08  14:14:38  mrose
 * update
 * 
 * Revision 7.3  90/07/27  08:48:34  mrose
 * update
 * 
 * Revision 7.2  90/07/09  14:53:08  mrose
 * sync
 * 
 * Revision 7.1  90/07/01  20:02:00  mrose
 * update
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


/* LINTLIBRARY */

#include	<stdio.h>
#include	<ctype.h>
#include	<varargs.h>
#include	"pepsy-driver.h"
#include	"psap.h"
#include	"pepsy.h"
#include	"tailor.h"

#define PRINT_TYPES	0
#define	CHOICE_PUSH


extern PE p_setpresent();
extern IFP vfnx;
extern FILE *vfp;

extern ptpe *next_ptpe();
extern int pepsylose ();

int     xlevel = 0;
int     tabed = 0;
int     xpushed = 0;

#define NEXT_PTPE(p)	(p = next_ptpe(p))
#define CHKTAG(mod, p, pe)	p_ismatch(p, mod, pe->pe_class, pe->pe_id)

/* SUPPRESS 36 *//* for Saber C */

/*
 * to guarentee the rules that vname and a vprint-type routine are called
 * alternatively. Basically can't have two vname's in a row
 */
static vnamelock = 0;
   /* if vnamelock > 0 don't call vname */
#define VNAME(x)  if (vnamelock++ <= 0) vname(x); else
#define VTAG(class, tag)	if (vnamelock++ <= 0)  \
					vtag((int )class, (int )tag); else

/* as vprint takes a variable number of arguements we have to put all of
 * the arguements inside () and remove them when we expand to vprint
 */
#define VPRINT(x)	vnamelock = 0, vprint x
#define VSTRING(x)	vnamelock = 0, vstring(x)
#define VUNKNOWN(x)	vnamelock = 0, vunknown(x)
#define VPUSH		vnamelock = 0, vpush
#define VPOP		vnamelock = 0, vpop


/*
 * Print out ASN data given in pe using the information given in the tables
 */
prnt_f(typ, mod, pe, explicit, len, buf)
/* ARGSUSED */
int     typ;			/* which type it is */
modtyp *mod;			/* ASN Module it is from */
PE      pe;
int     explicit;	/* nonzero means we are call top level 
			 * print final \n
			 */
int    *len;
char  **buf;
{
    ptpe   *p;

    if (typ < 0 || typ >= mod->md_nentries) {
	return (ppepsylose (mod, NULLPTPE, pe, "prnt_f:Illegal type %d\n",typ));
    }

    p = mod->md_ptab[typ];

    if (p->pe_type != PE_START) {
	return (ppepsylose (mod, p, pe, "prnt_f: missing PE_START\n"));
    }

#if EXTRA_BRACKETS
    if (explicit) {
	if (p->pe_typename)
	    VNAME(p->pe_typename);
    }
    VPUSH();
#endif


    if (p_pr_obj(explicit, pe, p, mod) == NOTOK) {
#if EXTRA_BRACKETS
	VPOP();
#endif
	return (NOTOK);
    }
#if EXTRA_BRACKETS
    VPOP();
#endif
    return (OK);
}

/*
 * Parse an object. The top level of an object does not have any
 * offset field which makes it different to pr_type routine which
 * must assume that it has an offset.
 */
static int
p_pr_obj(expl, pe, p, mod)
int     expl;			/* do we look at the tag */
PE      pe;
ptpe   *p;
modtyp *mod;			/* Module it is from */
{
    int     cnt = 0;

#if PRINT_TYPES
    if (p->pe_typename)
	VNAME(p->pe_typename);
#endif

    p++;
    while (p->pe_type != PE_END) {

	if (ISDTYPE(p) && expl && CHKTAG(mod, p, pe) == 0) {
	    if (DEFAULT(p)) {
		return (ppepsylose (mod, p, pe,
		    "p_pr_obj:Default not implemented\n"));
	    }
	    else if (OPTIONAL(p))
		return (NO_DATA_USED);
	    else {
		return (ppepsylose (mod, p, pe,
		    "p_pr_obj:missing mandatory parameter", p, mod));
	    }
	}
	if (p->pe_typename)
	    VNAME(p->pe_typename);
	switch (p->pe_type) {
	case PE_END:
	case PE_START:
	    goto bad;

	case UCODE:
	    if (mod->md_pucode == NULLIFP
	    || (*mod->md_pucode) (pe, p) == NOTOK)
		goto bad;
	    break;


	default:
	    if (p_pr_type(expl, pe, p, mod) == NOTOK)
		goto bad;
	    break;
	}
	if (ISDTYPE(p) && cnt > 0)
	    return (ppepsylose (mod, p, pe, "p_pr_obj:compound type found\n"));
	if (ISDTYPE(p) && pe != NULLPE) {
	    return (OK);
	}
	if (NEXT_PTPE(p) == NULLPTPE)
	    goto bad;
    }

    return (OK);

bad:
    return (NOTOK);
}


/*
 * Parse a single type. If a basic type parse it, if a compound type
 * call the appropriate parsing routine
 */
static int
p_pr_type(expl, pe, p, mod)
int     expl;			/* do we look at the tag */
PE      pe;
ptpe   *p;
modtyp *mod;			/* Module it is from */
{
    int     cnt = 0;
    integer     i;
    OID     oid;


    while (p->pe_type != PE_END) {

	if (ISDTYPE(p) && expl && CHKTAG(mod, p, pe) == 0) {
	    if (DEFAULT(p)) {
#ifdef	PRINT_DEFAULTS
		setpval(p, p + 1, mod);
#endif
		return (OK);
	    } else if (OPTIONAL(p))
		return (OK);
	    else {
		dmp_ptpe("p_pr_type:missing mandatory parameter", p, mod);
		goto bad;
	    }
	}
	switch (p->pe_type) {
	case PE_END:
	case PE_START:
	    goto bad;

        case BOPTIONAL:
            if (CHKTAG(mod, p + 1, pe) == 0) {
                return (OK);
            }
            p++;
            continue;

	case UCODE:
	    if (mod->md_pucode == NULLIFP
	    || (*mod->md_pucode) (pe, p) == NOTOK)
		goto bad;
	    break;

	case ETAG:
	    switch (p->pe_ucode) {
	    default:
		if (p_pr_etype(pe->pe_cons, p, mod) == NOTOK)
		    goto bad;
	    }
	    break;

	case SSEQ_START:
	case SEQ_START:
	    if (p_pr_seq(pe, p, mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case SSEQOF_START:
	case SEQOF_START:
	    if (p_pr_seqof(pe, p, mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case SSET_START:
	case SET_START:
	    if (p_pr_set(pe, p, mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case SSETOF_START:
	case SETOF_START:
	    if (p_pr_setof(pe, p, mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case IMP_OBJ:
	    p++;
	    if (p->pe_type == EXTOBJ || p->pe_type == SEXTOBJ) {
		if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP,
		    (char **) 0) == NOTOK)
		    goto bad;
	    } else {
		if (p_pr_obj(0, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK) {
		    goto bad;
		}
	    }
	    break;

	case SOBJECT:
	case OBJECT:
	    if (p_pr_obj(expl, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case SCHOICE_START:
	case CHOICE_START:
	    if (p_pr_choice(pe, p, mod) == NOTOK)
		goto bad;
	    break;

	case SEXTOBJ:
	case EXTOBJ:
	    if (p[1].pe_type != EXTMOD) {
		return (ppepsylose (mod, p, pe, "p_pr_type: missing EXTMOD"));
	    }
	    if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP,
		   (char **) 0) == NOTOK)
		   goto bad;
	    break;

	case INTEGER:
	    if (pe != NULLPE) {
		if ((i = prim2num(pe)) == NOTOK && pe->pe_errno != PE_ERR_NONE)
		    goto bad;
		VPRINT(("%d", i));
	    }
	    break;

#ifdef	PEPSY_REALS
	case REALTYPE:
	    if (pe != NULLPE) {
		double r;

		if ((r = prim2real(pe)) == NOTOK
		    && pe->pe_errno != PE_ERR_NONE)
		    goto bad;
		VPRINT(("%g", r));
	    }
	    break;
#endif

	case BOOLEAN:
	    if (pe != NULLPE) {
		if ((i = prim2flag(pe)) == NOTOK)
		    goto bad;
		VPRINT((i ? "TRUE" : "FALSE"));
	    }
	    break;

	case T_NULL:
	    VPRINT(("NULL"));
	    break;

	case SANY:
	    if (pe != NULLPE) {
		if (pe->pe_errno != PE_ERR_NONE) {
		    goto bad;
		} else
		    VUNKNOWN(pe);
	    }
	    break;


	case ANY:
	    if (pe != NULLPE) {
		if (pe->pe_errno != PE_ERR_NONE) {
		    goto bad;
		} else
		    VUNKNOWN(pe);
	    }
	    break;

	case SOCTETSTRING:
	    if (pe != NULLPE) {
		VSTRING(pe);
	    }
	    break;

	case OCTETSTRING:
	case T_STRING:
	case OCTET_PTR:
	    if (pe != NULLPE) {
		VSTRING(pe);
	    }
	    break;

	case SBITSTRING:
	    if (pe != NULLPE) {
		PE	bstr;

		if ((bstr = prim2bit(pe)) == NULLPE)
		    goto bad;
		if (p -> pe_ucode >= 0 && mod->md_ptrtab &&
		    mod -> md_ptrtab[p -> pe_ucode] &&
		    bstr -> pe_nbits < LOTSOFBITS)
		    VPRINT (("%s", bit2str(bstr,
					   mod -> md_ptrtab[p -> pe_ucode])));
		else if (bstr->pe_nbits < LOTSOFBITS)
		    VPRINT(("%s", bit2str(bstr, "\020")));
		else
		    VUNKNOWN(pe);
	    }
	    break;

	case BITSTR_PTR:
	case BITSTRING:
	    if (pe != NULLPE) {
		PE	bstr;

		if ((bstr = prim2bit(pe)) == NULLPE)
		    goto bad;
		if (p -> pe_ucode >= 0 && mod->md_ptrtab &&
		    mod -> md_ptrtab[p -> pe_ucode] &&
		    bstr -> pe_nbits < LOTSOFBITS)
		    VPRINT (("%s", bit2str(bstr,
					   mod -> md_ptrtab[p -> pe_ucode])));
		else if (bstr->pe_nbits < LOTSOFBITS)
		    VPRINT(("%s", bit2str(bstr, "\020")));
		else
		    VUNKNOWN(pe);
	    }
	    break;

	case SOBJID:
	    if ((oid = (OID) prim2oid(pe)) == NULLOID) {
		goto bad;
	    } else {
		VPRINT(("%s", oid2ode(oid)));
	    }
	    break;

	case OBJID:
	    if ((oid = (OID) prim2oid(pe)) == NULLOID) {
		goto bad;
	    } else {
		VPRINT(("%s", oid2ode(oid)));
	    }
	    break;

	case FN_CALL:
	    if ((FN_PTR(mod, p))(pe) == NOTOK)
		return ppepsylose (mod, &p[1], NULLPE,
				  "p_pr_type:FN_CALL:call failed");
	    break;

	default:
	    return (ppepsylose (mod, p, pe, "p_pr_type: %d not implemented\n",
		p->pe_type));
	}
	if (ISDTYPE(p) && cnt > 0)
	    return (ppepsylose (mod, p, pe, "p_pr_type:compound type found\n"));
	if (ISDTYPE(p) && pe != NULLPE)
	    return (OK);
	if (NEXT_PTPE(p) == NULLPTPE)
	    goto bad;
    }

    return (OK);

bad:
    return (NOTOK);
}

/*
 * Parse a sequence, calling appropriate routines to parse each sub
 * type
 */
static int
p_pr_seq(head, p, mod)
PE      head;
ptpe   *p;
modtyp *mod;			/* Module it is from */
{
    PE      pe;

    if (p->pe_type != SEQ_START && p->pe_type != SSEQ_START)
	return (ppepsylose (mod, p, head, "p_pr_seq: missing SEQ_START\n"));
#if PRINT_TYPES
    if (p->pe_typename)
	VNAME(p->pe_typename);
#endif
    VPUSH();

    p++;
    if (p->pe_type == DFLT_B)
	p++;


    pe = first_member(head);
    while (p->pe_type != PE_END) {
	if (ISDTYPE(p) && OPTIONAL(p)) {
	    if (pe == NULLPE || CHKTAG(mod, p, pe) == 0)
		goto next;
	} else if (ISDTYPE(p) && (pe == NULLPE || CHKTAG(mod, p, pe) == 0)) {
	    if (DEFAULT(p)) {
#ifdef	PRINT_DEFAULTS
		setpval(p, p + 1, mod);
#endif
		goto next;
	    } else {
		dmp_ptpe("p_pr_seq:missing mandatory parameter", p, mod);
		goto bad;
	    }
	}
	if (p->pe_typename)
	    VNAME(p->pe_typename);
	switch (p->pe_type) {
	case OPTL:
	    break;

	case UCODE:
	    if (mod->md_pucode == NULLIFP
	    || (*mod->md_pucode) (pe, p) == NOTOK)
		goto bad;
	    break;

	case BOPTIONAL:
	    if (CHKTAG(mod, p + 1, pe) == 0)
		goto next;
	    p++;
	    continue;

	case ETAG:
	    if (p_pr_type(1, pe, p, mod) == NOTOK)
		goto bad;
	    break;

	case SEQ_START:
	    if (p_pr_seq(pe, p, mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case SEQOF_START:
	    if (p_pr_seqof(pe, p, mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case SET_START:
	    if (p_pr_set(pe, p, mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case SETOF_START:
	    if (p_pr_setof(pe, p, mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case IMP_OBJ:
	    p++;
	    if (p->pe_type == EXTOBJ) {
		if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP,
		    (char **) 0) == NOTOK)
		    goto bad;
	    } else {
		if (p_pr_obj(0, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK) {
		    goto bad;
		}
	    }
	    break;

	case SOBJECT:
	    if (p_pr_obj(1, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case OBJECT:
#if PRINT_TYPES
    if (p->pe_typename)
	VNAME(p->pe_typename);
#endif
	    if (p_pr_obj(1, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case SCHOICE_START:
	case CHOICE_START:
	    if (p_pr_choice(pe, p, mod) == NOTOK)
		goto bad;
	    break;

	case SEXTOBJ:
	    if (p[1].pe_type != EXTMOD) {
		return (ppepsylose (mod, p, pe, "p_pr_seq: missing EXTMOD"));
	    }
	    if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0,NULLIP,
		(char **)0) == NOTOK)
		goto bad;
	    break;

	case EXTOBJ:
	    if (p[1].pe_type != EXTMOD) {
		return (ppepsylose (mod, p, pe, "p_pr_seq: missing EXTMOD"));
	    }
	    if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP,
		    (char **)0) == NOTOK)
		goto bad;
	    break;

	default:
	    if (p_pr_type(1, pe, p, mod) == NOTOK)
		goto bad;
	    break;
	}

	if (ISDTYPE(p) && pe != NULLPE)
	    pe = next_member(head, pe);
next:
	if (NEXT_PTPE(p) == NULLPTPE)
	    goto bad;
    }

    VPOP();
    return (OK);

bad:
    VPOP();
    return (NOTOK);
}


/*
 * Parse a set, calling appropriate routines to parse each sub type
 */
static int
p_pr_set(head, p, mod)
PE      head;
ptpe   *p;
modtyp *mod;			/* Module it is from */
{
    PE      pe;

    if (p->pe_type != SET_START && p->pe_type != SSET_START)
	return (ppepsylose (mod, p, head, "p_pr_seq: missing SET_START\n"));
#if PRINT_TYPES
    if (p->pe_typename)
	VNAME(p->pe_typename);
#endif
    VPUSH();

    p++;
    if (p->pe_type == DFLT_B)
	p++;

    while (p->pe_type != PE_END) {

	if (ISDTYPE(p) && OPTIONAL(p)) {
	    if ((pe = (PE) p_setpresent(head, p, mod)) == NULLPE)
		goto next;
	} else if (ISDTYPE(p)
	    && (pe = (PE) p_setpresent(head, p, mod)) == NULLPE) {
	    if (DEFAULT(p)) {
#ifdef PRINT_DEFAULTS
		setpval(p, p + 1, mod);
#endif
		goto next;
	    } else {
		dmp_ptpe("p_pr_set:missing mandatory parameter", p, mod);
		goto bad;
	    }
	}
	if (p->pe_typename)
	    VNAME(p->pe_typename);
	switch (p->pe_type) {
	case OPTL:
	    break;

	case UCODE:
	    if (mod->md_pucode == NULLIFP
	    || (*mod->md_pucode) (pe, p) == NOTOK)
		goto bad;
	    break;

	case BOPTIONAL:
	    if ((pe = (PE) p_setpresent(head, p + 1, mod)) == NULLPE)
		goto next;
	    p++;
	    continue;

	case ETAG:
	    if (p_pr_type(1, pe, p, mod) == NOTOK)
		goto bad;
	    break;

	case SEQ_START:
	    if (p_pr_seq(pe, p, mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case SEQOF_START:
	    if (p_pr_seqof(pe, p, mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case SET_START:
	    if (p_pr_set(pe, p, mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case SETOF_START:
	    if (p_pr_setof(pe, p, mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case IMP_OBJ:
	    p++;
	    if (p->pe_type == EXTOBJ) {
		if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP,
		       (char **) 0) == NOTOK)
		       goto bad;
	    } else {
		if (p_pr_obj(0, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK) {
		    goto bad;
		}
	    }
	    break;

	case SOBJECT:
	    if (p_pr_obj(1, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case OBJECT:
	    if (p_pr_obj(1, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK) {
		goto bad;
	    }
	    break;

	case SCHOICE_START:
	case CHOICE_START:
	    if (p_pr_choice(pe, p, mod) == NOTOK)
		goto bad;
	    break;

	case SEXTOBJ:
	    if (p[1].pe_type != EXTMOD) {
		return (ppepsylose (mod, p, pe, "p_pr_set: missing EXTMOD"));
	    }
	    if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP, (char **)0) == NOTOK)
		    return (NOTOK);
	    break;

	case EXTOBJ:
	    if (p[1].pe_type != EXTMOD) {
		return (ppepsylose (mod, p, pe, "p_pr_set: missing EXTMOD"));
	    }
	    if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP,
		(char **)0) == NOTOK)
		goto bad;
	    break;

	default:
	    if (p_pr_type(1, pe, p, mod) == NOTOK)
		goto bad;
	    break;
	}

next:
	if (NEXT_PTPE(p) == NULLPTPE)
	    goto bad;
    }

    VPOP();
    return (OK);

bad:
    VPOP();
    return (NOTOK);
}


/*
 * Parse a sequence of calling appropriate routines to parse each sub
 * type
 */

static int
p_pr_seqof(head, p, mod)
PE      head;
ptpe   *p;
modtyp *mod;			/* Module it is from */
{
    PE      pe;
    ptpe   *start;		/* first entry in list */
    int     dflt = 0;

    if (p->pe_type != SEQOF_START && p->pe_type != SSEQOF_START) {
	return (ppepsylose (mod, p, head, "p_pr_seqof: missing SEQOF_START\n"));
    }
#if PRINT_TYPES
    if (p->pe_typename)
	VNAME(p->pe_typename);
#endif
    VPUSH();

    p++;

    if (p->pe_type == DFLT_B)
	p++;

    start = p;


    pe = first_member(head);
    while (pe != NULLPE) {
	while (p->pe_type != PE_END) {

	    if (ISDTYPE(p) && CHKTAG(mod, p, pe) == 0) {
		if (DEFAULT(p))
		    return (ppepsylose (mod, p, pe,
			"p_pr_seqof:Default not implemented\n"));
		else if (OPTIONAL(p))
		    goto next;
		else {
		    return (ppepsylose (mod, p, pe,
			"p_pr_seqof:missing mandatory parameter"));
		}
	    }
	    if (p->pe_typename)
		VNAME(p->pe_typename);
	    switch (p->pe_type) {
	    case UCODE:
		if (mod->md_pucode == NULLIFP
		|| (*mod->md_pucode) (pe, p) == NOTOK)
		    goto bad;
		break;

	    case BOPTIONAL:
		if (CHKTAG(mod, p + 1, pe) == 0)
		    goto next;
		p++;
		continue;

	    case ETAG:
		if (p_pr_type(1, pe, p, mod) == NOTOK)
		    goto bad;
		break;

		/*
		 * case SCTRL:  parm = (char *) ((char *) parm);
		 * break;
		 */

	    case SEQ_START:
		if (p_pr_seq(pe, p, mod) == NOTOK) {
		    goto bad;
		}
		break;

	    case SEQOF_START:
		if (p_pr_seqof(pe, p, mod) == NOTOK) {
		    goto bad;
		}
		break;

	    case SET_START:
		if (p_pr_set(pe, p, mod) == NOTOK) {
		    goto bad;
		}
		break;

	    case SETOF_START:
		if (p_pr_setof(pe, p, mod) == NOTOK) {
		    goto bad;
		}
		break;

	    case SOBJECT:
		if (p_pr_obj(1, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK) {
		    goto bad;
		}
		break;

	    case OBJECT:
		if (p_pr_obj(1, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK) {
		    goto bad;
		}
		break;

	    case SCHOICE_START:
	    case CHOICE_START:
		if (p_pr_choice(pe, p, mod) == NOTOK)
		    goto bad;
		break;

	    case SEXTOBJ:
		if (p[1].pe_type != EXTMOD) {
		    return (ppepsylose (mod, p, pe,"p_pr_seqof:missing EXTMOD"));
		}
		if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP,
		     (char **)0) == NOTOK)
		     goto bad;
		break;

	    case EXTOBJ:
		if (p[1].pe_type != EXTMOD) {
		    return (ppepsylose (mod, p, pe,"p_pr_seqof:missing EXTMOD"));
		}
		if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP, 
		    (char **) 0) == NOTOK)
		    goto bad;
		break;

	    default:
		if (p_pr_type(1, pe, p, mod) == NOTOK)
		    goto bad;
		break;
	    }

	    if (ISDTYPE(p) && dflt == 0)
		pe = next_member(head, pe);
    next:
	    if (NEXT_PTPE(p) == NULLPTPE)
		goto bad;
	}
	/* parm = (char *) (parm); */
	p = start;
    }

    VPOP();
    return (OK);

bad:
    VPOP();
    return (NOTOK);
}

/*
 * Parse a setof, calling appropriate routines to parse each sub type
 */
static int
p_pr_setof(head, p, mod)
PE      head;
ptpe   *p;
modtyp *mod;			/* Module it is from */
{
    PE      pe;
    ptpe   *start;

    if (p->pe_type != SETOF_START && p->pe_type != SSETOF_START)
	return (ppepsylose (mod, p, head, "p_pr_setof: missing SETOF_START\n"));
#if PRINT_TYPES
    if (p->pe_typename)
	VNAME(p->pe_typename);
#endif
    VPUSH();

    p++;
    if (p->pe_type == DFLT_B)
	p++;

    start = p;

    for (pe = first_member(head); pe; pe = next_member(head, pe)) {
	while (p->pe_type != PE_END) {
	    if (pe == NULLPE || CHKTAG(mod, p, pe) == 0) {
		if (DEFAULT(p)) {
#ifdef PRINT_DEFAULTS
		    setpval(p, p + 1, mod);
#endif
		    goto next;
		} else {
		    dmp_ptpe("p_pr_setof:missing mandatory parameter", p, mod);
		    goto bad;
		}
	    }

	    if (p->pe_typename)
		VNAME(p->pe_typename);
	    switch (p->pe_type) {
	    case UCODE:
		if (mod->md_pucode == NULLIFP
		|| (*mod->md_pucode) (pe, p) == NOTOK)
		    goto bad;
		break;

	    case BOPTIONAL:
		if ((pe = (PE) p_setpresent(head, p + 1, mod)) == NULLPE)
		    goto next;
		p++;
		continue;

	    case ETAG:
		if (p_pr_type(1, pe->pe_cons, p, mod) == NOTOK)
		    goto bad;
		break;

		/*
		 * case SCTRL: parm = (char *) (parm); break;
		 */

	    case SEQ_START:
		if (p_pr_seq(pe, p, mod) == NOTOK) {
		    goto bad;
		}
		break;

	    case SEQOF_START:
		if (p_pr_seqof(pe, p, mod) == NOTOK) {
		    goto bad;
		}
		break;

	    case SET_START:
		if (p_pr_set(pe, p, mod) == NOTOK) {
		    goto bad;
		}
		break;

	    case SETOF_START:
		if (p_pr_setof(pe, p, mod) == NOTOK) {
		    goto bad;
		}
		break;

	    case SOBJECT:
		if (p_pr_obj(1, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK) {
		    goto bad;
		}
		break;

	    case OBJECT:
		if (p_pr_obj(1, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK) {
		    goto bad;
		}
		break;

	    case SCHOICE_START:
	    case CHOICE_START:
		if (p_pr_choice(pe, p, mod) == NOTOK)
		    goto bad;
		break;

	    case SEXTOBJ:
		if (p[1].pe_type != EXTMOD) {
		    return (ppepsylose (mod, p, pe,"p_pr_setof:missing EXTMOD"));
		}
		if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP,
		    (char **) 0) == NOTOK)
		    goto bad;
		break;

	    case EXTOBJ:
		if (p[1].pe_type != EXTMOD) {
		    return (ppepsylose (mod, p, pe,"p_pr_setof:missing EXTMOD"));
		}
		if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP,
		    (char **) 0) == NOTOK)
		    goto bad;
		break;

	    default:
		if (p_pr_type(1, pe, p, mod) == NOTOK)
		    goto bad;
		break;
	    }

    next:
	    if (NEXT_PTPE(p) == NULLPTPE)
		goto bad;
	}
	/* parm = (char *)(parm); */
	p = start;
    }

    VPOP();
    return (OK);

bad:
    VPOP();
    return (NOTOK);

}

/*
 * parse a choice field. This means find which choice is taken
 */
static int
p_pr_choice(head, p, mod)
PE      head;
ptpe   *p;
modtyp *mod;			/* Module it is from */
{
    if (p->pe_type != CHOICE_START && p->pe_type != SCHOICE_START) {
	return (ppepsylose (mod, p, head, "p_pr_choice:missing CHOICE_START"));
    }
#if PRINT_TYPES
    if (p->pe_typename)
	VNAME(p->pe_typename);
#endif
#ifdef CHOICE_PUSH
    VPUSH();
#endif

    p++;
    if (p->pe_type == DFLT_B)
	p++;

    if (p->pe_type == SCTRL) {
	p++;
    }

    for (; p->pe_type != PE_END; NEXT_PTPE(p)) {
	if (ISDTYPE(p) && p_ismatch(p, mod, head->pe_class, head->pe_id)) {
	    if (p->pe_typename)
		VNAME(p->pe_typename);
	    if (p_pr_type(0, head, p, mod) == NOTOK)
		return (NOTOK);
#ifdef CHOICE_PUSH
	    VPOP();
#endif
	    NEXT_PTPE(p);
	    if (p->pe_type == UCODE) {
		if (mod->md_pucode == NULLIFP
		|| (*mod->md_pucode) (head, p) == NOTOK)
		    return (NOTOK);
	    }
	    return (OK);
	}
    }

    dmp_ptpe("p_pr_choice: no choice taken", p, mod);
#ifdef CHOICE_PUSH
	VPOP();
#endif
    return (NOTOK);
}

/*
 * Calculate the next ptpe entry in the sequence. Count a sequence as
 * one element
 */
ptpe   *
next_ptpe(p)
ptpe   *p;
{
    int     level;

    level = 0;
    if (p->pe_type == PE_END) {
	(void) ppepsylose (NULLMODTYP, p, NULLPE,
	    "next_ptpe: unexpected PE_END");
	return (NULLPTPE);
    }
    do {
again:
	switch (p->pe_type) {
	case SSEQ_START:
	case SSEQOF_START:
	case SSET_START:
	case SSETOF_START:
	case SCHOICE_START:
	case SEQ_START:
	case SEQOF_START:
	case SET_START:
	case SETOF_START:
	case CHOICE_START:
	    level++;
	    break;

	case UCODE:
	case SCTRL:
	case CH_ACT:
	case INTEGER:
	case REALTYPE:
	case BOOLEAN:
	case SANY:
	case ANY:
	case T_NULL:
	case OBJECT:
	case SOBJECT:
	case BITSTRING:
	case SBITSTRING:
	case BITSTR_LEN:
	case OCTETSTRING:
	case T_STRING:
	case OCTET_LEN:
	case SOCTETSTRING:
	case OBJID:
	case SOBJID:
	case OPTL:
	case EXTMOD:
	case DFLT_B:
	case FN_CALL:
	case FFN_CALL:
	    break;

	case OCTET_PTR:
	case BITSTR_PTR:
	case IMP_OBJ:
	case ETAG:
	case EXTOBJ:
	case SEXTOBJ:
	case DFLT_F:
	case BOPTIONAL:
	    p++;
	    goto again;

	case PE_END:
	    level--;
	    break;

	default:
	    (void) ppepsylose (NULLMODTYP, p, NULLPE,
		"next_ptpe: unknown type %d\n", p->pe_type);
	    return (NULLPTPE);
	}
	p++;
    } while (level > 0 || p->pe_type == DFLT_B);

    return (p);
}

/*
 * Parse a single type for explicit tag If a basic type parse it, if
 * a compound type call the appropriate parsing routine
 */
static int
p_pr_etype(pe, p, mod)
PE      pe;
ptpe   *p;
modtyp *mod;			/* Module it is from */
{
    integer     i;
    OID     oid;

    if (p->pe_type != ETAG)
	return (ppepsylose (mod, p, pe, "p_pr_etype: missing ETAG\n"));
    

    if (PRINT_TAG(p))
	VTAG (CLASS (p), TAG (p));
    p++;

    switch (p->pe_type) {
    case PE_END:
    case PE_START:
	goto bad;

    case UCODE:
	if (mod->md_pucode == NULLIFP
	|| (*mod->md_pucode) (pe, p) == NOTOK)
	    goto bad;
	break;

    case BOPTIONAL:
	return (ppepsylose (mod, p, pe, "p_pr_etype:illegal BOPTIONAL\n"));

    case ETAG:
	switch (p->pe_ucode) {

	default:
	    if (p_pr_etype(pe->pe_cons, p, mod) == NOTOK)
		goto bad;
	}
	break;

    case SSEQ_START:
    case SEQ_START:
	if (p_pr_seq(pe, p, mod) == NOTOK)
	    goto bad;
	break;

    case SSEQOF_START:
    case SEQOF_START:
	if (p_pr_seqof(pe, p, mod) == NOTOK)
	    goto bad;
	break;

    case SSET_START:
    case SET_START:
	if (p_pr_set(pe, p, mod) == NOTOK)
	    goto bad;
	break;

    case SSETOF_START:
    case SETOF_START:
	if (p_pr_setof(pe, p, mod) == NOTOK)
	    goto bad;
	break;

    case IMP_OBJ:
	p++;
	if (p->pe_type == EXTOBJ) {
	    if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP,
		   (char **) 0) == NOTOK)
		   goto bad;
	} else {
#if PRINT_TYPES
    if (p->pe_typename)
	VNAME(p->pe_typename);
#endif
	    if (p_pr_obj(0, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK)
		goto bad;
	}
	break;

    case SOBJECT:
#if PRINT_TYPES
    if (p->pe_typename)
	VNAME(p->pe_typename);
#endif
	if (p_pr_obj(1, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK)
	    goto bad;
	break;

    case OBJECT:
#if PRINT_TYPES
    if (p->pe_typename)
	VNAME(p->pe_typename);
#endif
	if (p_pr_obj(1, pe, mod->md_ptab[p->pe_tag], mod) == NOTOK)
	    goto bad;
	break;

    case SCHOICE_START:
    case CHOICE_START:
	if (p_pr_choice(pe, p, mod) == NOTOK)
	    goto bad;
	break;

    case SEXTOBJ:
	if (p[1].pe_type != EXTMOD) {
	    return (ppepsylose (mod, p, pe, "p_pr_etype: missing EXTMOD"));
	}
	if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP,
	       (char **) 0) == NOTOK)
	       goto bad;
	break;

    case EXTOBJ:
	if (p[1].pe_type != EXTMOD) {
	    return (ppepsylose (mod, p, pe, "p_pr_etype: missing EXTMOD"));
	}
	if (prnt_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0, NULLIP,
	       (char **) 0) == NOTOK)
	       goto bad;
	break;

    case INTEGER:
	if (pe != NULLPE) {
	    if ((i = prim2num(pe)) == NOTOK && pe->pe_errno != PE_ERR_NONE)
		goto bad;
	    VPRINT(("%d", i));
	}
	break;

#ifdef	PEPSY_REALS
    case REALTYPE:
	if (pe != NULLPE) {
	    double r;

	    if ((r = prim2real(pe)) == NOTOK
		&& pe->pe_errno != PE_ERR_NONE)
		goto bad;
	    VPRINT(("%g", r));
	}
	break;
#endif


    case BOOLEAN:
	if (pe != NULLPE) {
	    if ((i= prim2flag(pe)) == NOTOK)
		goto bad;
	    VPRINT((i ? "TRUE" : "FALSE"));
	}
	break;

    case T_NULL:
	VPRINT(("NULL"));
	break;

    case ANY:
	if (pe != NULLPE) {
	    if (pe->pe_errno != PE_ERR_NONE) {
		goto bad;
	    } else
		VUNKNOWN(pe);
	}
	break;

    case SANY:
	if (pe != NULLPE) {
	    if (pe->pe_errno != PE_ERR_NONE) {
		goto bad;
	    } else
		VUNKNOWN(pe);
	}
	break;

    case SOCTETSTRING:
    case OCTETSTRING:
    case T_STRING:
    case OCTET_PTR:
	if (pe != NULLPE) {
	    VSTRING(pe);
	}
	break;

    case BITSTRING:
    case BITSTR_PTR:
    case SBITSTRING:
	if (pe != NULLPE) {
	    PE	bstr;

	    if ((bstr = prim2bit(pe)) == NULLPE)
		goto bad;
	    if (p -> pe_ucode >= 0 && mod->md_ptrtab &&
		mod -> md_ptrtab[p -> pe_ucode] &&
		bstr -> pe_nbits < LOTSOFBITS)
		VPRINT (("%s", bit2str(bstr,
				       mod -> md_ptrtab[p -> pe_ucode])));
	    else if (bstr->pe_nbits < LOTSOFBITS)
		VPRINT(("%s", bit2str(bstr, "\020")));
	    else
		VUNKNOWN(pe);
	}
	break;

    case SOBJID:
	if (((OID) oid = prim2oid(pe)) == NULLOID) {
	    goto bad;
	} else {
	    VPRINT(("%s", oid2ode(oid)));
	}
	break;

    case OBJID:
	if (((OID) oid = prim2oid(pe)) == NULLOID) {
	    goto bad;
	} else {
	    VPRINT(("%s", oid2ode(oid)));
	}
	break;

    default:
	return (ppepsylose (mod, p, pe, "p_pr_etype: %d not implemented\n",
	    p->pe_type));
    }

    return (OK);

bad:
    return (NOTOK);
}

/*
 * Is there a match at for this tag and class pair. Return 1 if yes 0
 * if no We will search through contained objects and through choices
 */
p_ismatch(p, mod, cl, tag)
ptpe   *p;
modtyp *mod;			/* Module it is from */
unsigned int cl, tag;
{
    if (!ISDTYPE(p))
	return (0);
    switch (p->pe_type) {
    case SOBJECT:
    case OBJECT:
	/* Needs to be changed for optional and default */
	return (p_ismatch(p = mod->md_ptab[p->pe_tag] + 1, mod, cl, tag));

    case SEXTOBJ:
    case EXTOBJ:
	if (p[1].pe_type != EXTMOD) {
	    (void) ppepsylose (mod, p, NULLPE, "p_ismatch: missing EXTMOD");
	    return (0); /* fixup ismatch return -1 */
	}
	return (p_ismatch(EXT2MOD(mod, (p + 1))->md_ptab[p->pe_tag] + 1,
			  EXT2MOD(mod, (p + 1)), cl, tag));

    case SCHOICE_START:
    case CHOICE_START:
	for (p++; p->pe_type != PE_END; p = NEXT_PTPE(p)) {
	    if (!ISDTYPE(p))
		continue;
	    if (p_ismatch(p, mod, cl, tag))
		return (1);
	}
	return (0);


    case SANY:
	return (1);

    case ANY:
	if (STAG(p) == -1)
	    return (1);
	/* else fall through - not sure if this is needed */

    default:
	return (tag == TAG(p) && cl == CLASS(p));
    }
    /* NOTREACHED */
    /* return (0); */
}

/*
 * determine if the given field is present in the data This is simple
 * if the field is a simple type with an obvious tag but in the case
 * of an object or a CHOICE type the tag is not obvious. If the
 * object is a CHOICE there are more than one possible tag that could
 * match and in this case we must try to match each one of them.
 */
PE
p_setpresent(head, p, mod)
PE      head;
ptpe   *p;
modtyp *mod;
{
    PE      pe;
    modtyp	*nmod;

    while (!ISDTYPE(p) && p->pe_type != PE_END) {
	p++;
    }
    if (!ISDTYPE(p) || p->pe_type == PE_END)
	return (NULLPE);

    switch (p->pe_type) {
    case EXTOBJ:
    case SEXTOBJ:
	/* Needs to be changed for optional and default */
	nmod = EXT2MOD(mod, (p + 1));
	return (p_setpresent(head, p = nmod->md_ptab[p->pe_tag] + 1, nmod));

    case OBJECT:
    case SOBJECT:
	{
	    /* Needs to be changed for optional and default */
	    return (p_setpresent(head, p = mod->md_ptab[p->pe_tag] + 1, mod));
	}

    case SCHOICE_START:
    case CHOICE_START:
	for (p++; p->pe_type != PE_END; p = NEXT_PTPE(p)) {
	    if (!ISDTYPE(p))
		continue;
	    if ((pe = (PE) p_setpresent(head, p, mod)))
		return (pe);
	}
	return (NULLPE);

    default:
	return (set_find(head, CLASS(p), TAG(p)));
    }
}

#ifdef PRINT_DEFAULTS
/*
 * set the default value to that value in the structure
 */
setpval(typ, dflt, mod)
ptpe   *typ, *dflt;
modtyp *mod;
{
    int     len, i;
    integer intval;
    char   *ptr, *optr;
    PE      pe_ptr;

    switch (typ->pe_type) {

    case INTEGER:
	intval = IVAL(mod, dflt);
	(*vfnx) (vfp, "%d (DEFAULT INTEGER)\n", intval);
	break;


#ifdef	PEPSY_REALS
    case REALTYPE:
	(*vfnx) (vfp, "%f (DEFAULT Real)\n", RVAL(mod, dflt));
	break;
#endif

    case BOOLEAN:
	intval = IVAL(mod, dflt);
	/*
	 * (*vfnx) (vfp, "%s  %d (DEFAULT BOOLEAN)\n",
	 * (typ->pe_typename) ? typ->pe_typename : "", charval);
	 */
	(*vfnx) (vfp, "%d (DEFAULT BOOLEAN)\n", intval);
	break;

    case T_NULL:
	/* Only one value */
	break;

    case SBITSTRING:
    case BITSTRING:
	(PE) pe_ptr = strb2bitstr(PVAL(mod, dflt), IVAL(mod, dflt), 0, 0);
	(*vfnx) (vfp, " '");
	optr = ptr = bitstr2strb((PE) pe_ptr, &len);
	for (i = 0; i < len; i += 8)
	    (*vfnx) (vfp, "%.2x", *ptr++);
	(*vfnx) (vfp, "'H (DEFAULT BITSTRING)\n");
	pe_free (pe_ptr);
	free (optr);
	break;

    case SOCTETSTRING:
    case OCTETSTRING:
        ptr = PVAL(mod, dflt);	/* array of octets */
        intval = IVAL(mod, dflt);		/* length of array */
	if (printable(ptr, intval)) {
	    (*vfnx) (vfp, "\"");
	    for (; *ptr && intval-- > 0; ptr++)
                (void) fputc(*ptr, vfp);
	    (*vfnx) (vfp, "\"\n");
	} else {
	    (*vfnx) (vfp, "'");
	    if (ptr) {
		for (; intval-- > 0; ptr++)
		    (*vfnx) (vfp, "%.2x", *ptr & 0xff);
	    }
	    (*vfnx) (vfp, "'H \n");
	}
	break;

    case OBJECT:
	setpval(mod->md_ptab[typ->pe_tag] + 1, dflt, mod);
	break;

    case SOBJECT:
	setpval(mod->md_ptab[typ->pe_tag] + 1, dflt, mod);
	break;

    case IMP_OBJ:
	typ++;

    case ANY:
    case SANY:
    case SEXTOBJ:
    case EXTOBJ:
    case OBJID:
    case SOBJID:
    case SEQ_START:
    case SET_START:
    case -1:			/* Just use the pepy method of null
				 * pointers */
	/*
	 * This is the posy/pepy hack way of doing things at the
	 * moment
	 */
	(char *) ptr = NULL;
	break;

    default:
	return (ppepsylose (mod, p, pe, "setpval: type %d not implemented\n",
	    typ->pe_type));
	break;
    }

}
#endif
/*
 * return non zero if we can print out the string
 */
printable(strptr, len)
char   *strptr;
int	len;
{
    if (strptr == NULL || *strptr == '\0') {
	return (0);
    }

     while (len-- > 0) {
	if (!isprint(*strptr++))
	    return (0);
    }

    return (1);
}

/*
 * (Dump) Print out a printable entry in a human recognisable form
 */
dmp_ptpe(s, p, mod)
char   *s;
modtyp *mod;			/* Module it is from */
ptpe   *p;
{
    int     i, j;
    ptpe  **par, **prev;
    char   *name;

    (void) fprintf(vfp, "%s:(%s)", s, mod->md_name);
    /*
     * Calculate what table it is in - we assume they are in order of
     * increasing address
     */

    par = NULL;
    if (mod->md_ptab != NULL && mod->md_ptab[0] < p) {
	par = mod->md_ptab;
	name = "Printing:";
    }
    if (par == NULL) {
	(void) fprintf(vfp, "can't find entry 0x%x\n", p);
	return;
    }
    prev = par;
    for (i = mod->md_nentries; i > 0; i--) {
	if (*par > p)
	    break;
	par++;
    }
    if (par == prev) {
	(void) ppepsylose (mod, p, NULLPE,
	    "dmp_ptpe:par == prev == 0x%x internal error\n", (int) par);
	return;
    }
    par--;
    j = p - *par;

    (void) fprintf(vfp, "%s type %d + %d ", name, par - prev, j);
    p_pr_entry(p);
}
