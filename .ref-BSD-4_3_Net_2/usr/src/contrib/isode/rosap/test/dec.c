/* dec.c */

#ifndef	lint
static char *rcsid = "$Header$";
#endif

/* 
 * $Header$
 *
 *
 * $Log$
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


/*
 * These routines are the driving routines for parsing encoding and printing
 * data
 */
#include	<stdio.h>
#include	"../h/psap.h"
#include	"pepsy.h"
#include	"pepdefs.h"

#define PEPYPARM	char **
#ifndef	PEPYPARM
#define PEPYPARM	char **
#endif
extern	PEPYPARM	NullParm;



extern	PE	pr_seq(), pr_seqof(), pr_set(), pr_setof(), pr_type();
extern	PE	en_seq(), en_seqof(), en_set(), en_setof(), en_type();
extern	PE	pr_choice(), en_choice(), en_etype(), pr_etype(), pr_obj();
extern	PE	setpresent();

extern	tpe	*next_tpe();
extern	int	_pverbose;

#define NEXT_TPE(p)	p = next_tpe(p)
#define CHKTAG(mod, p, pe)	ismatch(p, mod, pe->pe_class, pe->pe_id) 

/* Have we got an optional object which we have allocated sapce for */
#define ALLOC_MEM(p, parm)	(p->pe_type == SOBJECT \
	&& p[-1].pe_type == MALLOC)

/*SUPPRESS 36*/ /* for Saber C */

/*
 * decode the specified type of the specified module into the given pe
 */
dec_f(typ, mod, pe, explicit, len, buf, parm)
/*ARGSUSED*/
int	typ;	/* which type it is */
modtyp	*mod;	/* Module it is from */
PE      pe;
int     explicit;
int    *len;
char  **buf;
PEPYPARM parm;
{
    tpe	*p;

    if (typ < 0 || typ >= mod->md_nentries)
	ferrd(1, "dec_f:Illegal typ %d\n", typ);
    
    p = mod->md_dtab[typ];
    if (p->pe_type != PE_START)
	ferr(1, "dec_f: missing PE_START\n");
    p++;
    if ((pe = pr_obj(explicit, pe, parm, p, mod)) == NULLPE)
		return (NOTOK);

    return (OK);
}

/*
 * Parse an object. The top level of an object does not have any offset field
 * which makes it different to pr_type routine which must assume that it
 * has an offset.
 */
PE
pr_obj(expl, pe, parm, p, mod)
int	expl;	/* do we look at the tag */
PE      pe;
PEPYPARM parm;
tpe	*p;
modtyp	*mod;	/* Module it is from */
{
	int	cnt = 0;


    if (_pverbose > 6) {
	printf(dfp, "1st Decoding the type %d \n",p->pe_type);
    }
    while (p->pe_type != PE_END) {

	if (ISDTYPE(p) && expl && CHKTAG(mod, p, pe) == 0) {
		if (DEFAULT(p))
		    ferr(1, "pr_obj:Default not implemented\n");
		else if (OPTIONAL(p)) {
			if (ALLOC_MEM(p, parm))
			    fix_mem(parm, p);
			goto next;
		} else {
			dmp_tpe("pr_obj:missing mandatory parameter", p, mod);
			return (NULLPE);
		}
	}

	if (_pverbose > 6) {
	    printf("2nd Decoding the type %d \n",p->pe_type);
	}

	switch (p->pe_type) {
	case PE_END:
	case PE_START:
		return (NULLPE);

	case UCODE:
		if ((*mod->md_ducode)(pe, parm, p, mod) != OK)
			return (NULLPE);
		break;

	case ETAG:
	    switch (p->pe_ucode) {
	    default:
		p++;
		if (pr_obj(1, pe->pe_cons, parm, p, mod) == NULLPE)
		    return (NULLPE);
	    }
	    break;

	case MALLOC:
		if ((*(parm) = (char *)  calloc(1, p->pe_tag)) == NULL)
			return NULLPE;
		break;

	case SEQ_START:
		if ((pe = pr_seq(pe, parm, p, mod)) == NULLPE)
			return (NULLPE);
		break;

	case SEQOF_START:
		if ((pe = pr_seqof(pe, parm, p, mod)) == NULLPE)
			return (NULLPE);
		break;

	case SET_START:
		if ((pe = pr_set(pe, parm, p, mod)) == NULLPE)
			return (NULLPE);
		break;

	case SETOF_START:
		if ((pe = pr_setof(pe, parm, p, mod)) == NULLPE)
			return (NULLPE);
		break;

	case IMP_OBJ:
		p++;
		if (p->pe_type == EXTOBJ || p->pe_type == SEXTOBJ) {
		    dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 0, NULL,
		       (char **) 0, parm);
		} else {
		    if (p->pe_type == SOBJECT) {
			if ((pe = pr_obj(0, pe, parm,
			  mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
			    return (NULLPE);
		    } else
			if ((pe = pr_obj(0, pe, *parm + p->pe_ucode,
			  mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
			    return (NULLPE);
		}
		break;

	case SOBJECT:
	case OBJECT:
		if ((pe = pr_obj(expl, pe, parm, mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
			return (NULLPE);
		break;

	case CHOICE_START:
		if ((pe = pr_choice(pe, parm, p, mod)) == NULLPE)
			return (NULLPE);
		break;

	case SEXTOBJ:
	case EXTOBJ:
	    if (p[1].pe_type != EXTMOD) {
		dmp_tpe("pr_type: missing EXTMOD", p, mod);
		ferr(1, "pr_type:internal error\n");
	    }
	    dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 1, NULL,
		(char **) 0, parm);
	    break;

	default:
		if ((pe = pr_type(expl, pe, parm, p, mod)) == NULLPE)
			return (NULLPE);
		break;
	}
	if (ISDTYPE(p) && cnt > 0)
		ferr(1, "pr_obj:compound type found\n"); 
	if (ISDTYPE(p) && pe != NULLPE)
		return (pe);
next:
	NEXT_TPE(p);
    }

    return (pe);
}
/*
 * Parse a single type.
 * If a basic type parse it, if a compound type call the appropriate
 * parsing routine
 */
PE
pr_type(expl, pe, parm, p, mod)
int	expl;	/* do we look at the tag */
PE      pe;
PEPYPARM parm;
tpe	*p;
modtyp	*mod;	/* Module it is from */
{
	int	cnt = 0;
	OID	oid;


    if (_pverbose > 6) {
	printf("pr_type:type %d \n",p->pe_type);
    }
    while (p->pe_type != PE_END) {
/*
print_pe (pe, 1);
*/

	if (ISDTYPE(p) && expl && CHKTAG(mod, p, pe) == 0) {
		if (DEFAULT(p)) {
		    setdval(p, p + 1, parm, mod);
		    goto next;
		} else if (OPTIONAL(p)) {
			if (ALLOC_MEM(p, parm))
			    fix_mem(parm, p);
			goto next;
		} else {
			dmp_tpe("pr_type:missing mandatory parameter", p, mod);
			return (NULLPE);
		}
	}

	if (_pverbose > 6) {
	    printf("2nd prtype: type %d \n",p->pe_type);
	}

	switch (p->pe_type) {
	case PE_END:
	case PE_START:
		return (NULLPE);

	case UCODE:
		if ((*mod->md_ducode)(pe, parm, p, mod) != OK)
			return (NULLPE);
		break;

	case ETAG:
	    switch (p->pe_ucode) {
	    default:
		p++;
		if (pr_etype(pe->pe_cons, parm, p, mod) == NULLPE)
		    return (NULLPE);
	    }
	    break;

	case MALLOC:
		if ((*(parm) = (char *)  calloc(1, p->pe_tag)) == NULL)
			return NULLPE;
		break;

	case SEQ_START:
		if ((pe = pr_seq(pe, parm, p, mod)) == NULLPE)
			return (NULLPE);
		break;

	case SEQOF_START:
		if ((pe = pr_seqof(pe, parm, p, mod)) == NULLPE)
			return (NULLPE);
		break;

	case SET_START:
		if ((pe = pr_set(pe, parm, p, mod)) == NULLPE)
			return (NULLPE);
		break;

	case SETOF_START:
		if ((pe = pr_setof(pe, parm, p, mod)) == NULLPE)
			return (NULLPE);
		break;

	case IMP_OBJ:
		p++;
		if (p->pe_type == EXTOBJ || p->pe_type == SEXTOBJ) {
		    dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 0, NULL,
		       (char **) 0, parm);
		} else {
		    if (p->pe_type == SOBJECT) {
			if ((pe = pr_obj(0, pe, parm,
			  mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
			    return (NULLPE);
		    } else
			if ((pe = pr_obj(0, pe, *parm + p->pe_ucode,
			  mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
			    return (NULLPE);
		}
		break;

	case SOBJECT:
	case OBJECT:
		if ((pe = pr_obj(expl, pe, parm, mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
			return (NULLPE);
		break;

	case CHOICE_START:
		if ((pe = pr_choice(pe, parm, p, mod)) == NULLPE)
			return (NULLPE);
		break;

	case SEXTOBJ:
	case EXTOBJ:
	    if (p[1].pe_type != EXTMOD) {
		dmp_tpe("pr_type: missing EXTMOD", p, mod);
		ferr(1, "pr_type:internal error\n");
	    }
	    dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 1, NULL,
		(char **) 0, parm);
	    break;

	case INTEGER:
		if (pe != NULLPE) {
		    if (_pverbose > 6) {
			printf("integer value is %d\n",prim2num(pe));
		    }
		    if (((*(int *)(*parm + p->pe_ucode)) = prim2num(pe))
		      == NOTOK && pe->pe_errno != PE_ERR_NONE) {
			printf("pr_type:bad integer %s",
			pe_error(pe->pe_errno));
			return (NULLPE);
		    }
		}
		break;

	case BOOLEAN:
		if (pe != NULLPE) {
			    if (_pverbose > 6) {
				printf("boolean value is %d\n",prim2flag(pe));
			    }
			if (((* (char *)(*parm + p->pe_ucode)) = prim2flag(pe))==NOTOK && pe->pe_errno != PE_ERR_NONE) {
					printf("pr_type:bad integer %s",
					pe_error(pe->pe_errno));
				return (NULLPE);
			}
		}
		break;

	case T_NULL:
		break;

	case SCONS_ANY:
	case SANY:
		if (pe != NULLPE) {
			((* (PE *) parm) = pe)->pe_refcnt++;
			if (pe->pe_errno != PE_ERR_NONE) {
				printf("pr_type:bad integer %s",
					pe_error(pe->pe_errno));
				return (NULLPE);
			}
		}
		break;


	case CONS_ANY:
	case ANY:
		if (pe != NULLPE) {
			(* (PE *) (*parm + p->pe_ucode) = pe) ->pe_refcnt++;
			if (pe->pe_errno != PE_ERR_NONE) {
				printf("pr_type:bad integer %s",
					pe_error(pe->pe_errno));
				return (NULLPE);
			}
		}
		break;

	case SOCTETSTRING:
		if (pe != NULLPE) {
		    if ((*((struct qbuf **) parm) = prim2qb(pe)) ==
		    (struct qbuf *) NULL && pe->pe_errno != PE_ERR_NONE) {
				printf("pr_type:bad octet string %s",
					pe_error(pe->pe_errno));
				return (NULLPE);
			}
		}
		break;

	case OCTETSTRING:
		if (pe != NULLPE) {
		    if ((*((struct qbuf **) (*parm + p->pe_ucode))
			= prim2qb(pe)) == (struct qbuf *) NULL
			&& pe->pe_errno != PE_ERR_NONE) {
				printf("pr_type:bad octet string %s",
					pe_error(pe->pe_errno));
				return (NULLPE);
			}
		}
		break;

	case SBITSTRING:
		if (pe != NULLPE) {
		    /*
		    if ((*((PE *)(*parm + p->pe_ucode))=prim2bit(pe))== NULLPE) 
    I think this is a bug in ISODE's routines prim2bit
		    does the wrong thing
		    */
		    if ((*((PE *)parm)=prim2bit(pe_cpy(pe))) == NULLPE) {
			printf("pr_type:out of memory");
			return (NULLPE);
		    }
		}
		break;

	case BITSTRING:
		if (pe != NULLPE) {
		    /*
		    if ((*((PE *)(*parm + p->pe_ucode))=prim2bit(pe))== NULLPE) 
    I think this is a bug in ISODE's routines prim2bit
		    does the wrong thing
		    */
		    if ((*((PE *)(*parm + p->pe_ucode))=prim2bit(pe_cpy(pe))) == NULLPE) {
			printf("pr_type:out of memory");
			return (NULLPE);
		    }
		}
		break;

	case SOBJID:
	/* This is messy because ISODE's library routine uses a static. Don't
	 * know why they do
	 */
	    if ((oid = prim2oid(pe)) == NULLOID
	    || (*(OID *)parm = oid_cpy(oid)) == NULLOID) {
		    printf("pr_type:Object Identifier: out of memory");
		    if (oid && oid->oid_elements) {
			free(oid->oid_elements);
			oid->oid_elements = NULL;
		    }
		    return (NULLPE);
	    }
	    if (oid && oid->oid_elements) {
		free(oid->oid_elements);
		oid->oid_elements = NULL;
	    }
	    break;

	case OBJID:
	    if ((oid = prim2oid(pe)) == NULLOID
	    || (*(OID *)(*parm + p->pe_ucode) = oid_cpy(oid)) == NULLOID) {
		    printf("pr_type:Object Identifier: out of memory");
		    if (oid && oid->oid_elements) {
			free(oid->oid_elements);
			oid->oid_elements = NULL;
		    }
		    return (NULLPE);
	    }
	    if (oid && oid->oid_elements) {
		free(oid->oid_elements);
		oid->oid_elements = NULL;
	    }
	    break;

	default:
		dmp_tpe("pr_type: type not implemented", p, mod);
		ferrd(1, "pr_type: %d not implemented\n", p->pe_type);
		break;
	}
	if (ISDTYPE(p) && cnt > 0)
		ferr(1, "pr_type:compound type found\n"); 
	if (ISDTYPE(p) && pe != NULLPE)
		return (pe);
next:
	NEXT_TPE(p);
    }

    return (pe);
}

/*
 * Parse a sequence, calling appropriate routines to parse each sub type
 */
PE
pr_seq(head, parm, p, mod)
PE      head;
PEPYPARM parm;
tpe	*p;
modtyp	*mod;	/* Module it is from */
{
    PE      pe;
    int	*popt = NULL;	/* Pointer to optional field */
    int	optcnt = 0;	/* Number of optionals bits so far */

    if (p->pe_type != SEQ_START)
	    ferr(1, "pr_seq: missing SEQ_START\n");
    p++;

    if (p->pe_type == DFLT_B)
	p++;

    pe = first_member(head);
    while (p->pe_type != PE_END) {
	if (_pverbose > 6) {
	    printf("pr_seq:type %d\n",p->pe_type);
	}


	if (ISDTYPE(p) && OPTIONAL(p)) {
	    switch (p->pe_type) {
	    case INTEGER:
	    case BOOLEAN:
	    case T_NULL:
		if (pe == NULLPE || CHKTAG(mod, p, pe) == 0) {
		    optcnt++;
		    goto next;
		}
		SETBIT(*popt, optcnt++);
		break;

#if 0
	    case ANY:
	    case CONS_ANY:
	    case SANY:
	    case SCONS_ANY:
		dmp_tpe("pr_seq:optional/default ANY", p, mod);
		ferr(1, "unable to distinguish tags");
#endif

	    default:
		if (pe == NULLPE || CHKTAG(mod, p, pe) == 0) {
		    if (ALLOC_MEM(p, parm))
			fix_mem(parm, p);
		    goto next;
		}
		break;
	    }
	} else if (ISDTYPE(p) && (pe == NULLPE || CHKTAG(mod, p, pe) == 0)) {
	    if (DEFAULT(p)) {
		setdval(p, p + 1, parm, mod);
		goto next;
	    } else {
		dmp_tpe("pr_seq:missing mandatory parameter", p, mod);
		return (NULLPE);
	    }
	}

	switch (p->pe_type) {
	case OPTL:
	    popt = (int *) (*parm + p->pe_ucode);
	    break;

	case UCODE:
	    if ((*mod->md_ducode)(pe, parm, p, mod) != OK)
		    return (NULLPE);
	    break;

	case ETAG:
	    if ((pe = pr_type(1, pe, parm, p, mod)) == NULLPE)
		return NULLPE;
	    break;

	case MALLOC:
	    if ((*parm = (char *)  calloc(1, p->pe_tag)) == NULL)
		return NULLPE;
	    break;

	case SEQ_START:
	    if ((pe = pr_seq(pe, (char **)(*parm + p->pe_ucode), p, mod))
	      == NULLPE)
		return (NULLPE);
	    break;

	case SEQOF_START:
		if ((pe = pr_seqof(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
		    return (NULLPE);
		break;

	case SET_START:
		if ((pe = pr_set(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
			return (NULLPE);
		break;

	case SETOF_START:
		if ((pe = pr_setof(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
			return (NULLPE);
		break;

	case IMP_OBJ:
		p++;
		if (p->pe_type == EXTOBJ || p->pe_type == SEXTOBJ) {
		    dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 0, NULL,
		       (char **) 0, *parm + p->pe_ucode);
		} else {
		    if (p->pe_type == SOBJECT) {
			if ((pe = pr_obj(0, pe, parm,
			  mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
			    return (NULLPE);
		    } else
			if ((pe = pr_obj(0, pe, *parm + p->pe_ucode,
			  mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
			    return (NULLPE);
		}
		break;

	case SOBJECT:
		if ((pe = pr_obj(1, pe, parm,
		    mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
			return (NULLPE);
		break;

	case OBJECT:
		if ((pe = pr_obj(1, pe, *parm + p->pe_ucode,
		    mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
			return (NULLPE);
		break;

	case CHOICE_START:
		if ((pe = pr_choice(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
			return (NULLPE);
		break;

	case SEXTOBJ:
	    if (p[1].pe_type != EXTMOD) {
		dmp_tpe("pr_seq: missing EXTMOD", p, mod);
		ferr(1, "pr_seq:internal error\n");
	    }
	    dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 1, NULL,
		(char **) 0, parm);
	    break;

	case EXTOBJ:
	    if (p[1].pe_type != EXTMOD) {
		dmp_tpe("pr_seq: missing EXTMOD", p, mod);
		ferr(1, "pr_seq:internal error\n");
	    }
	    dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 1, NULL,
		(char **) 0, *parm + p->pe_ucode);
	    break;

	default:
		if ((pe = pr_type(1, pe, parm, p, mod)) == NULLPE)
			return (NULLPE);
		break;
		/*
		ferrd(1, "pr_seq: unknown type %d\n", p->pe_type);
		*/
	}

	if (ISDTYPE(p) && pe != NULLPE)
		pe = next_member(head, pe);
    next:
	    NEXT_TPE(p);
    }

    return (head);

}


/*
 * Parse a set, calling appropriate routines to parse each sub type
 */
PE
pr_set(head, parm, p, mod)
PE      head;
PEPYPARM parm;
tpe	*p;
modtyp	*mod;	/* Module it is from */
{
	PE      pe;
	int	*popt = NULL;	/* Pointer to optional field */
	int	optcnt = 0;	/* Number of optionals bits so far */

	if (p->pe_type != SET_START)
		ferr(1, "pr_seq: missing SET_START\n");
	p++;

    if (p->pe_type == DFLT_B)
	p++;

	while (p->pe_type != PE_END) {

	    if (_pverbose > 6) {
		printf("dec_set with the type %d\n",p->pe_type);
	    }

	    if (ISDTYPE(p) && OPTIONAL(p)) {
		switch (p->pe_type) {
		case INTEGER:
		case BOOLEAN:
		case T_NULL:
		    if ((pe = setpresent(head, p, mod)) == NULLPE) {
			optcnt++;
			goto next;
		    }
		    SETBIT(*popt, optcnt++);
		    break;

#if 0
		case ANY:
		case CONS_ANY:
		case SANY:
		case SCONS_ANY:
		    dmp_tpe("pr_set:optional/default ANY\n", p, mod);
		    ferr(1, "unable to distinguish tags");
#endif

		default:
		    if ((pe = setpresent(head, p, mod)) == NULLPE) {
			if (ALLOC_MEM(p, parm))
			    fix_mem(parm, p);
			goto next;
		    }
		    break;
		}
	    } else if (ISDTYPE(p) && (pe = setpresent(head, p, mod)) == NULLPE) {
		if (DEFAULT(p)) {
		    setdval(p, p + 1, parm, mod);
		    goto next;
		} else {
		    dmp_tpe("pr_set:missing mandatory parameter", p, mod);
		    return (NULLPE);
		}
	    }

	    switch (p->pe_type) {
	    case OPTL:
		popt = (int *) (*parm + p->pe_ucode);
		break;

	    case UCODE:
		    if ((*mod->md_ducode)(pe, parm, p, mod) != OK)
			    return (NULLPE);
		    break;

	    case ETAG:
		    if ((pe = pr_type(1, pe, parm, p, mod)) == NULLPE)
			    return NULLPE;
		    break;

	    case MALLOC:
		    if ((*parm = (char *)  calloc(1, p->pe_tag)) == NULL)
			    return NULLPE;
		    break;

	    case SEQ_START:
		    if ((pe = pr_seq(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
			    return (NULLPE);
		    break;
    
	    case SEQOF_START:
		    if ((pe = pr_seqof(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
			    return (NULLPE);
		    break;
    
	    case SET_START:
		    if ((pe = pr_set(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
			    return (NULLPE);
		    break;
    
	    case SETOF_START:
		    if ((pe = pr_setof(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
			    return (NULLPE);
		    break;

	    case IMP_OBJ:
		    p++;
		    if (p->pe_type == EXTOBJ || p->pe_type == SEXTOBJ) {
			dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 0, NULL,
			   (char **) 0, *parm + p->pe_ucode);
		    } else {
			if (p->pe_type == SOBJECT) {
			    if ((pe = pr_obj(0, pe, parm,
			      mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
				return (NULLPE);
			} else
			    if ((pe = pr_obj(0, pe, *parm + p->pe_ucode,
			      mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
				return (NULLPE);
		    }
		    break;
    
	    case SOBJECT:
		    if ((pe = pr_obj(1, pe, parm, mod->md_dtab[p->pe_tag] + 1,
			mod)) == NULLPE)
			    return (NULLPE);
		    break;
    
	    case OBJECT:
		    if ((pe = pr_obj(1, pe, *parm + p->pe_ucode,
			mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
			    return (NULLPE);
		    break;
    
	    case CHOICE_START:
		    if ((pe = pr_choice(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
			    return (NULLPE);
		    break;

	    case SEXTOBJ:
		if (p[1].pe_type != EXTMOD) {
		    dmp_tpe("pr_set: missing EXTMOD", p, mod);
		    ferr(1, "pr_set:internal error\n");
		}
		dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 1, NULL,
		    (char **) 0, parm);
		break;

	    case EXTOBJ:
		if (p[1].pe_type != EXTMOD) {
		    dmp_tpe("pr_set: missing EXTMOD", p, mod);
		    ferr(1, "pr_set:internal error\n");
		}
		dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 1, NULL,
		    (char **) 0, *parm + p->pe_ucode);
		break;

	    default:
		    if ((pe = pr_type(1, pe, parm, p, mod)) == NULLPE)
			    return (NULLPE);
		    break;
		    /*
		    ferrd(1, "pr_set: unknown type %d\n", p->pe_type);
		    */
	    }

	next:
		NEXT_TPE(p);
	}

	return (head);

}


/*
 * Parse a sequence of calling appropriate routines to parse each sub type
 */
PE
pr_seqof(head, parm, p, mod)
PE      head;
PEPYPARM parm;
tpe	*p;
modtyp	*mod;	/* Module it is from */
{
    PE      pe;
    tpe  *start;		/* first entry in list */
    int	dflt = 0;

    if (p->pe_type != SEQOF_START) {
	    dmp_tpe("pr_seqof:missing SEQOF_START", p, mod);
	    ferr(1, "pr_seqof: missing SEQOF_START\n");
    }
    p++;

    if (p->pe_type == DFLT_B)
	p++;

    start = p;

    pe = first_member(head);
    while (pe != NULLPE) {
	while (p->pe_type != PE_END) {

	    if (_pverbose > 6) {
		printf("dec_seqof with the type %d\n",p->pe_type);
	    }

	    if (ISDTYPE(p) && CHKTAG(mod, p, pe) == 0) {
		if (DEFAULT(p)) {
		    setdval(p, p + 1, parm, mod);
		    goto next;
		} else if (OPTIONAL(p)) {
		    if (ALLOC_MEM(p, parm))
			fix_mem(parm, p);
		    goto next;
		} else {
		    dmp_tpe( "pr_seqof:missing mandatory parameter", p,
		      mod);
		    return (NULLPE);
		}
	    }

	    switch (p->pe_type) {
	    case UCODE:
		if ((*mod->md_ducode)(pe, parm, p, mod) != OK)
		    return (NULLPE);
		break;

	    case ETAG:
		if ((pe = pr_type(1, pe, parm, p, mod)) == NULLPE)
		    return NULLPE;
		break;

	    case MALLOC:
		if ((*parm = (char *)  calloc(1, p->pe_tag)) == NULL)
		    return NULLPE;
		break;

	    case SCTRL:
		parm = (char **) ((char *)*parm + p->pe_ucode);
		break;

	    case SEQ_START:
		if ((pe = pr_seq(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
		    return (NULLPE);
		break;
    
	    case SEQOF_START:
		if ((pe = pr_seqof(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
		    return (NULLPE);
		break;
    
	    case SET_START:
		if ((pe = pr_set(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
		    return (NULLPE);
		break;
    
	    case SETOF_START:
		if ((pe = pr_setof(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
		    return (NULLPE);
		break;
    
	    case SOBJECT:
		if ((pe = pr_obj(1, pe, parm,
		  mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
		    return (NULLPE);
		break;
    
	    case OBJECT:
		if ((pe = pr_obj(1, pe, *parm + p->pe_ucode,
		  mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
		    return (NULLPE);
		break;
    
	    case CHOICE_START:
		if ((pe = pr_choice(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
		    return (NULLPE);
		break;

	case SEXTOBJ:
	    if (p[1].pe_type != EXTMOD) {
		dmp_tpe("pr_seqof: missing EXTMOD", p, mod);
		ferr(1, "pr_seqof:internal error\n");
	    }
	    dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 1, NULL,
		(char **) 0, parm);
	    break;

	case EXTOBJ:
	    if (p[1].pe_type != EXTMOD) {
		dmp_tpe("pr_seqof: missing EXTMOD", p, mod);
		ferr(1, "pr_seqof:internal error\n");
	    }
	    dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 1, NULL,
		(char **) 0, *parm + p->pe_ucode);
	    break;

	    default:
		if ((pe = pr_type(1, pe, parm, p, mod)) == NULLPE)
		    return (NULLPE);
		break;
	    }

	    if (ISDTYPE(p) && dflt == 0)
		pe = next_member(head, pe);
	next:
	    NEXT_TPE(p);
	}
	parm = (char **)(*parm + p->pe_ucode);
	p = start;
    }

    return (head);

}

/*
 * Parse a setof, calling appropriate routines to parse each sub type
 */
PE
pr_setof(head, parm, p, mod)
PE      head;
PEPYPARM parm;
tpe	*p;
modtyp	*mod;	/* Module it is from */
{
	PE      pe;
	int	dflt;
	tpe	*start;

	if (p->pe_type != SETOF_START)
		ferr(1, "pr_setof: missing SETOF_START\n");
    p++;

    if (p->pe_type == DFLT_B)
	p++;

	start = p;
	pe = first_member(head);

	for (pe = first_member(head); pe; pe = next_member(head, pe)) {
		while (p->pe_type != PE_END) {
		    if (_pverbose > 6) {
			printf("dec_seqof with the type %d\n", p->pe_type);
		    }

		    if (pe == NULLPE || CHKTAG(mod, p, pe) == 0) {
			if (DEFAULT(p)) {
			    setdval(p, p + 1, parm, mod);
			    goto next;
			} else {
			    dmp_tpe( "pr_setof:missing mandatory parameter",
			     p, mod);
				return (NULLPE);
			}
		    }
			else
			    dflt = 0;

		    switch (p->pe_type) {
		    case UCODE:
			    if ((*mod->md_ducode)(pe, parm, p, mod) != OK)
				    return (NULLPE);
			    break;

		    case ETAG:
			    if ((pe = pr_type(1, pe->pe_cons, parm, p, mod)) == NULLPE)
				    return NULLPE;
			    break;

		    case MALLOC:
			    if ((*parm = (char *)  calloc(1, p->pe_tag)) == NULL)
				    return NULLPE;
			    break;

		    case SCTRL:
			    parm = (char **) (*parm + p->pe_ucode);
			    break;

		    case SEQ_START:
			    if ((pe = pr_seq(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
				    return (NULLPE);
			    break;
	    
		    case SEQOF_START:
			    if ((pe = pr_seqof(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
				    return (NULLPE);
			    break;
	    
		    case SET_START:
			    if ((pe = pr_set(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
				    return (NULLPE);
			    break;
	    
		    case SETOF_START:
			    if ((pe = pr_setof(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
				    return (NULLPE);
			    break;
	    
		    case SOBJECT:
			    if ((pe = pr_obj(1, pe, parm,
			    mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
				    return (NULLPE);
			    break;
	    
		    case OBJECT:
			    if ((pe = pr_obj(1, pe, *parm + p->pe_ucode,
			    mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
				    return (NULLPE);
			    break;
	    
		    case CHOICE_START:
			    if ((pe = pr_choice(pe, *parm + p->pe_ucode,
			    p, mod)) == NULLPE)
				    return (NULLPE);
			    break;

		    case SEXTOBJ:
			if (p[1].pe_type != EXTMOD) {
			    dmp_tpe("pr_setof: missing EXTMOD", p, mod);
			    ferr(1, "pr_setof:internal error\n");
			}
			dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 1,
			    NULL, (char **) 0, parm);
			break;

		    case EXTOBJ:
			if (p[1].pe_type != EXTMOD) {
			    dmp_tpe("pr_setof: missing EXTMOD", p, mod);
			    ferr(1, "pr_setof:internal error\n");
			}
			dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 1,
			    NULL, (char **) 0, *parm + p->pe_ucode);
			break;

		    default:
			    if ((pe = pr_type(1, pe, parm, p, mod)) == NULLPE)
				    return (NULLPE);
			    break;
			    /*
			    ferrd(1, "pr_setof: unknown type %d\n", p->pe_type);
			    */
		    }

#if 0
		    /* fixup - we only delete in this function */	
		    if (ISDTYPE(p) && dflt == 0)
			set_del(head, pe->pe_class, pe->pe_id);
#endif
		    next:
			NEXT_TPE(p);
		}
		parm = (char **)(*parm + p->pe_ucode);
		p = start;
	}

	return (head);

}

/*
 * parse a choice field. This means find which choice is taken
 */
PE
pr_choice(head, parm, p, mod)
PE      head;
PEPYPARM parm;
tpe	*p;
modtyp	*mod;	/* Module it is from */
{
    int	*poffset;
    int	cnt;
    PE  pe;

	if (p->pe_type != CHOICE_START) {
	    dmp_tpe("pr_choice:missing CHOICE_START", p, mod);
	    ferrd(1, "pr_choice:illegal table entry %d\n", p->pe_type);
	}
	
	p++;

    if (p->pe_type == DFLT_B)
	p++;

	if (p->pe_type == MALLOC) {
	    if ((*parm = (char *) calloc(1, p->pe_tag)) == NULL)
		return NULLPE;
	    p++;
	}
	if (p->pe_type == SCTRL) {
	    if ((poffset = (int *) (*parm + p->pe_ucode)) == NULL)
		return NULLPE;
	    p++;
	}

	for (cnt = 1; p->pe_type != PE_END; NEXT_TPE(p), cnt++) {
	    if (ISDTYPE(p) && ismatch(p, mod, head->pe_class, head->pe_id)) {
		pe = pr_etype(head, parm, p, mod);
		*poffset = cnt;
		return (pe);
	    }
	}
	dmp_tpe("pr_choice: no choice taken", p, mod);
	return (NULLPE);
}

/*
 * Calculate the next tpe entry in the sequence. Count a sequence as one element
 */
tpe	*
next_tpe(p)
tpe	*p;
{
	int	level;



	level = 0;
	if (p->pe_type == PE_END)
		ferr(1, "next_tpe:PE_END");
	do {
	    again:
		switch (p->pe_type) {
		case SEQ_START:
		case SEQOF_START:
		case SET_START:
		case SETOF_START:
		case CHOICE_START:
			level++;
			break;

		case UCODE:
		case MALLOC:
		case SCTRL:
		case CH_ACT:
		case INTEGER:
		case BOOLEAN:
		case SANY:
		case SCONS_ANY:
		case ANY:
		case CONS_ANY:
		case T_NULL:
		case OBJECT:
		case SOBJECT:
		case BITSTRING:
		case SBITSTRING:
		case OCTETSTRING:
		case SOCTETSTRING:
		case OBJID:
		case SOBJID:
		case OPTL:
		case EXTMOD:
		case DFLT_B:
			break;

		case IMP_OBJ:
		case ETAG:
		case EXTOBJ:
		case SEXTOBJ:
		case DFLT_F:
			p++;
			goto again;

		case PE_END:
			level--;
			break;

		default:
			ferrd(1, "next_tpe: unknown type %d\n", p->pe_type);
		}
		p++;
	} while (level > 0 || p->pe_type == DFLT_B);

	return (p);
}

/*
 * check that pe is non null and that the tag and class match and return
 * zero if the don't
 */
chktag(mod, p, pe)
modtyp  *mod;   /* Module it is from */
tpe	*p;
PE	pe;
{
	int	cl, tag;

	if (pe == NULLPE)
		return (0);
#if 0
	if (p->pe_type == OBJECT && skip_next_tag) /* first time we find skip */
		return (1);
	if (skip_next_tag) {	/* 2nd time reset skip */
	    skip_next_tag = 0;
	    return (1);
	}
#endif
	if (!findcltag(p, mod, &cl, &tag))
		return (0);
	if (pe->pe_id != tag || pe->pe_class != cl)
		return (0);
	
	return (1);
}

#if 0
/*
	test if we need to check the tag associated with the tpe entry p
*/
legal_tag(p)
tpe *p;
{
	int	ret;

	switch (p->pe_type) {
		
		case MALLOC:
		case SCTRL:
		case CH_ACT:
		case ANY:
		case OBJECT:
		case CHOICE_START:
		case CONS_ANY:
	/*	case ETAG:  don't think we should have this here */
			ret = 0;
			break;

		default:
#if 0
			if (skip_next_tag) {
				skip_next_tag = 0;
				ret = 0;
			}
			else
#endif
				ret = 1;
			break;
	}
	return ret;
}
#endif

/*
 * Parse a single type for explicit tag
 * If a basic type parse it, if a compound type call the appropriate
 * parsing routine
 */
PE
pr_etype(pe, parm, p, mod)
PE      pe;
PEPYPARM parm;
tpe	*p;
modtyp	*mod;	/* Module it is from */
{
    switch (p->pe_type) {
    case PE_END:
    case PE_START:
	    return (NULLPE);

    case UCODE:
	    if ((*mod->md_ducode)(pe, parm, p, mod) != OK)
		    return (NULLPE);
	    break;

    case ETAG:
	switch (p->pe_ucode) {

	default:
	    p++;
	    if (pr_etype(pe->pe_cons, parm, p, mod) == NULLPE)
		return (NULLPE);
	}
	break;

    case MALLOC:
	    if ((*(parm) = (char *)  calloc(1, p->pe_tag)) == NULL)
		    return NULLPE;
	    break;

    case SEQ_START:
	    if ((pe = pr_seq(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
		    return (NULLPE);
	    break;

    case SEQOF_START:
	    if ((pe = pr_seqof(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
		    return (NULLPE);
	    break;

    case SET_START:
	    if ((pe = pr_set(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
		    return (NULLPE);
	    break;

    case SETOF_START:
	    if ((pe = pr_setof(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
		    return (NULLPE);
	    break;

    case IMP_OBJ:
	    p++;
	    if (p->pe_type == EXTOBJ || p->pe_type == SEXTOBJ) {
		dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 0, NULL,
		   (char **) 0, *parm + p->pe_ucode);
	    } else {
		if (p->pe_type == SOBJECT) {
		    if ((pe = pr_obj(0, pe, parm,
		      mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
			return (NULLPE);
		} else
		    if ((pe = pr_obj(0, pe, *parm + p->pe_ucode,
		      mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
			return (NULLPE);
	    }
	    break;

    case SOBJECT:
	    if ((pe = pr_obj(1, pe, parm,
	      mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
		    return (NULLPE);
	    break;

    case OBJECT:
	    if ((pe = pr_obj(1, pe, *parm + p->pe_ucode,
	      mod->md_dtab[p->pe_tag] + 1, mod)) == NULLPE)
		    return (NULLPE);
	    break;

    case CHOICE_START:
	    if ((pe = pr_choice(pe, *parm + p->pe_ucode, p, mod)) == NULLPE)
		    return (NULLPE);
	    break;

    case SEXTOBJ:
	if (p[1].pe_type != EXTMOD) {
	    dmp_tpe("pr_etype: missing EXTMOD", p, mod);
	    ferr(1, "pr_etype:internal error\n");
	}
	dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 1, NULL,
	    (char **) 0, parm);
	break;

    case EXTOBJ:
	if (p[1].pe_type != EXTMOD) {
	    dmp_tpe("pr_etype: missing EXTMOD", p, mod);
	    ferr(1, "pr_etype:internal error\n");
	}
	dec_f(p->pe_tag, (modtyp *)p[1].pe_ucode, pe, 1, NULL,
	    (char **) 0, *parm + p->pe_ucode);
	break;

    case INTEGER:
	    if (pe != NULLPE) {
			if (_pverbose > 6) {
			    printf("integer value is %d\n",prim2num(pe));
			}
		    if (((*(int *)(*parm + p->pe_ucode)) = prim2num(pe))== NOTOK && pe->pe_errno != PE_ERR_NONE) {
			    printf("pr_etype:bad integer %s",
				    pe_error(pe->pe_errno));
			    return (NULLPE);
		    }
	    }
	    break;

    case BOOLEAN:
	    if (pe != NULLPE) {
			if (_pverbose > 6) {
			    printf("boolean value is %d\n",prim2flag(pe));
			}
		    if (((* (char *)(*parm + p->pe_ucode)) = prim2flag(pe))==NOTOK && pe->pe_errno != PE_ERR_NONE) {
			    printf("pr_etype:bad integer %s",
				    pe_error(pe->pe_errno));
			    return (NULLPE);
		    }
	    }
	    break;

    case T_NULL:
	    break;

    case ANY:
    case CONS_ANY:
	    if (pe != NULLPE) {
		    (* (PE *) (*parm + p->pe_ucode) = pe) ->pe_refcnt++;
		    if (pe->pe_errno != PE_ERR_NONE) {
			    printf("pr_etype:bad integer %s",
				    pe_error(pe->pe_errno));
			    return (NULLPE);
		    }
	    }
	    break;

    case SANY:
    case SCONS_ANY:
	    /* This could require changes when I find out what the CONS_ANY type
	     * is really for
	     */
	    if (pe != NULLPE) {
		    ((PE) (*parm ) = pe) ->pe_refcnt++;
		    if (pe->pe_errno != PE_ERR_NONE) {
			    printf("pr_etype:bad integer %s",
				    pe_error(pe->pe_errno));
			    return (NULLPE);
		    }
	    }
	    break;

    case SOCTETSTRING:
	    if (pe != NULLPE) {
		if ((*((struct qbuf **) parm)
		    = prim2qb(pe)) == (struct qbuf *) NULL
		    && pe->pe_errno != PE_ERR_NONE) {
			    printf("pr_etype:bad octet string %s",
				    pe_error(pe->pe_errno));
			    return (NULLPE);
		    }
	    }
	    break;

    case OCTETSTRING:
	    if (pe != NULLPE) {
		if ((*((struct qbuf **) (*parm + p->pe_ucode))
		    = prim2qb(pe)) == (struct qbuf *) NULL
		    && pe->pe_errno != PE_ERR_NONE) {
			    printf("pr_etype:bad octet string %s",
				    pe_error(pe->pe_errno));
			    return (NULLPE);
		    }
	    }
	    break;

    case SBITSTRING:
	    if (pe != NULLPE) {
		if (((*(PE *) parm) = pe_cpy(pe)) == NULLPE) {
			    printf("pr_etype:out of memory");
			    return (NULLPE);
		    }
	    }
	    break;

    case BITSTRING:
	    if (pe != NULLPE) {
		if ((*((PE *) (*parm + p->pe_ucode))= pe_cpy(pe)) == NULLPE) {
			    printf("pr_etype:out of memory");
			    return (NULLPE);
		    }
	    }
	    break;

    case SOBJID:
	if ((*(OID *)parm = oid_cpy(prim2oid(pe))) == NULLOID) {
		printf("pr_etype:Object Identifier: out of memory");
		return (NULLPE);
	}
	break;

    case OBJID:
	if ((*(OID *)(*parm + p->pe_ucode) = oid_cpy(prim2oid(pe))) == NULLOID) {
		printf("en_etype:Object Identifier: out of memory");
		return (NULLPE);
	}
	break;

    default:
	    ferrd(1, "pr_etype: %d not implemented\n", p->pe_type);
	    break;
    }

    return (pe);
}

/*
 * determine what the class and tag must be of the given object
 */
findcltag(p, mod, pcl, ptag)
tpe	*p;
modtyp	*mod;	/* Module it is from */
int	*pcl, *ptag;
{
    if (!ISDTYPE(p))
	return (0);
    if (p->pe_type != OBJECT) {
	*pcl = CLASS(p);
	*ptag = TAG(p);
	return (1);
    }
    p = mod->md_dtab[p->pe_tag] + 1;
    while (p->pe_type != PE_END) {
	if (ISDTYPE(p))
            return (findcltag(p, mod, pcl, ptag));
    }
    dmp_tpe("findcltag:warning:object with no data in it", p, mod);
    return (0);
}

/*
 * Is there a match at for this tag and class pair. Return 1 if yes 0 if no
 * We will search through contained objects and through choices
 */
ismatch(p, mod, cl, tag)
tpe     *p;
modtyp  *mod;   /* Module it is from */
unsigned int     cl, tag;
{
    while (!ISDTYPE(p))
	p++;

    switch (p->pe_type) {
    case SOBJECT:
    case OBJECT:
	/* Needs to be changed for optional and default */
	return (ismatch(p = mod->md_dtab[p->pe_tag] + 1, mod, cl, tag));

    case SEXTOBJ:
    case EXTOBJ:
	if (p[1].pe_type != EXTMOD) {
	    dmp_tpe("ismatch: missing EXTMOD", p, mod);
	    ferr(1, "ismatch:internal error\n");
	}
	return(ismatch(((modtyp *)p[1].pe_ucode)->md_dtab[p->pe_tag] + 1,
	    (modtyp *)p[1].pe_ucode, cl, tag));

    case CHOICE_START:
	for (p++; p->pe_type != PE_END; p = NEXT_TPE(p)) {
	    if (!ISDTYPE(p))
		continue;
	    if (ismatch(p, mod, cl, tag))
		return (1);
	}
	return (0);

    case SANY:
    case SCONS_ANY:
	return (1);

    case ANY:
    case CONS_ANY:
	if (TAG(p) == -1)
	    return (1);
	/* else fall through - not sure if this is needed */

    default:
        return (tag == TAG(p) && cl == CLASS(p));
    }
}

/*
 * determine if the given field is present in the data
 * This is simple if the field is a simple type with an obvious tag but
 * in the case of an object or a CHOICE type the tag is not obvious. If the
 * object is a CHOICE there are more than one possible tag that could match
 * and in this case we must try to match each one of them.
 */
PE
setpresent(head, p, mod)
PE	head;
tpe	*p;
modtyp	*mod;
{
    PE	pe;

    if (!ISDTYPE(p))
	return (NULLPE);
    switch (p->pe_type) {
    case OBJECT:
    case SOBJECT:
	/* Needs to be changed for optional and default */
	return (setpresent(head, p = mod->md_dtab[p->pe_tag] + 1, mod));

    case CHOICE_START:
	for (p++; p->pe_type != PE_END; p = NEXT_TPE(p)) {
	    if (!ISDTYPE(p))
		continue;
	    if ((pe = setpresent(head, p, mod)))
		return (pe);
	}
	return (NULLPE);

    default:
        return (set_find(head, CLASS(p), TAG(p)));
    }
}

/*
 * set the default value to that value in the structure
 */
setdval(typ, dflt, parm, mod)
tpe	*typ, *dflt;
char 	**parm;
modtyp	*mod;
{

again:
    switch (typ->pe_type) {
    case MALLOC:
	if ((*(parm) = (char *)  calloc(1, typ->pe_tag)) == NULL)
	    ferrd(1, "setdval:calloc failed on %d\n", typ->pe_tag);
	typ++;
	goto again;

    case INTEGER:
	    *(int *)(*parm + typ->pe_ucode) = IVAL(dflt);
	    break;

    case BOOLEAN:
	     *(char *)(*parm + typ->pe_ucode) = IVAL(dflt);
	    break;

    case T_NULL:
	    /* Only one value */
	    break;	

    case SBITSTRING:
	*(PE *)parm = strb2bitstr(PVAL(dflt), IVAL(dflt), 0, 0);
	break;

    case BITSTRING:
	*(PE *)(*parm + typ->pe_ucode) =
	    strb2bitstr(PVAL(dflt), IVAL(dflt), 0, 0);
	break;

    case SOCTETSTRING:
	*(struct qbuf **)parm = str2qb(PVAL(dflt), IVAL(dflt), 1);
	break;

    case OCTETSTRING:
	*(struct qbuf **)(*parm + typ->pe_ucode) =
	    str2qb(PVAL(dflt), IVAL(dflt), 1);
	break;

    case OBJECT:
	    setdval(mod->md_dtab[typ->pe_tag] + 1, dflt,
	      (char **) (*parm + typ->pe_ucode), mod);
	      break;

    case SOBJECT:
	    setdval(mod->md_dtab[typ->pe_tag] + 1, dflt, parm, mod);
	    break;

    case IMP_OBJ:
	typ++;

    case SCONS_ANY:
    case ANY:
    case CONS_ANY:
    case SANY:
    case SEXTOBJ:
    case EXTOBJ:
    case OBJID:
    case SOBJID:
    case SEQ_START:
    case SET_START:
    case -1:	/* Just use the pepy method of null pointers */
	/* This is the posy/pepy hack way of doing things at the moment */
	    *(char **)(*parm + typ->pe_ucode) = NULL;
	    break;

    default:
	/* dmp_tpe("setdval: type not implemented", typ, mod); - need mod*/
	ferrd(1, "setdval: %d not implemented\n", typ->pe_type);
	break;
    }

}
/*
 * fix up the allocation of memory. We have allocated memory for an optional
 * object that is not present. ISODE routines get upset if this is present
 * because it then believes the object is present and tries to process it ...
 */
fix_mem(parm, p)
char	**parm;
tpe	*p;
{
    if (p->pe_type != SOBJECT || p[-1].pe_type != MALLOC
	|| p[1].pe_type != PE_END)
	ferr(1, "fix_mem:inconsistency\n");
    if (*parm)
	free(*parm);
    *parm = NULL;
}
