/* dec.c */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepsy/RCS/dec.c,v 7.12 91/02/22 09:48:41 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/pepsy/RCS/dec.c,v 7.12 91/02/22 09:48:41 mrose Interim $
 *
 *
 * $Log:	dec.c,v $
 * Revision 7.12  91/02/22  09:48:41  mrose
 * Interim 6.8
 * 
 * Revision 7.11  90/12/23  17:24:17  mrose
 * patches
 * 
 * Revision 7.10  90/12/11  10:33:08  mrose
 * sync
 * 
 * Revision 7.9  90/11/20  15:27:05  mrose
 * update
 * 
 * Revision 7.8  90/11/11  10:53:33  mrose
 * update
 * 
 * Revision 7.7  90/11/04  19:18:12  mrose
 * update
 * 
 * Revision 7.6  90/10/23  20:42:52  mrose
 * update
 * 
 * Revision 7.5  90/10/17  14:39:26  mrose
 * update
 * 
 * Revision 7.4  90/08/18  00:44:05  mrose
 * touch-up
 * 
 * Revision 7.3  90/08/08  14:14:13  mrose
 * update
 * 
 * Revision 7.2  90/07/27  08:48:54  mrose
 * update
 * 
 * Revision 7.1  90/07/09  14:52:18  mrose
 * sync
 * 
 * Revision 7.0  90/07/01  19:54:13  mrose
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


/* LINTLIBRARY */

#include	<stdio.h>
#include	"pepsy-driver.h"
#include	"psap.h"
#include	"pepsy.h"
#include	"tailor.h"

extern int pepsylose ();

extern tpe *next_tpe(), *fdflt_b();

#define NEXT_TPE(p)	(p = next_tpe(p))
#define CHKTAG(mod, p, pe)	ismatch(p, mod, pe->pe_class, pe->pe_id)


static char oomsg[] = "Out of memory";
#define oom(a,b)	pepsylose ((a), (b), NULLPE, oomsg)

static char inpmsg[] = "Illegal Null Pointer";
#define inpm(a,b)	pepsylose ((a), (b), NULLPE, inpmsg)

static PE setpresent();

#define F_CI 0x100	/* called internally */

/* Have we got an optional object which we have allocated space for */
#define ALLOC_MEM(p, parm)	(p->pe_type == SOBJECT \
	&& p[-1].pe_type == MEMALLOC)

/*
 * decode the specified type of the specified module into the given
 * pe
 */
dec_f(typ, mod, pe, explicit, len, buf, parm)
/* ARGSUSED */
int     typ;			/* which type it is */
modtyp	*mod;			/* Module it is from */
PE      pe;
int	explicit;
int	*len;
char	**buf;
char	**parm;
{
    tpe    *p;
    int	    iflag;	/* are we called internally ? */

    if (typ < 0 || typ >= mod->md_nentries) {
	return (pepsylose (mod, NULLTPE, NULLPE, "dec_f: Illegal typ %d", typ));
    }

    p = mod->md_dtab[typ];
    if (p->pe_type != PE_START) {
	return (pepsylose (mod, NULLTPE, NULLPE, "dec_f: missing PE_START"));
    }
    p++;
    iflag = explicit & F_CI;
    explicit &= ~F_CI;
    if (!iflag)
	*parm = NULLCP;	/* initialise this for the MALLOCs that follow */

    if (pr_obj(explicit, pe, parm, p, mod) == NOTOK)
	goto bad;

    return (OK);
bad:
    return (NOTOK);
}

/*
 * Parse an object. The top level of an object does not have any
 * offset field which makes it different to pr_type routine which
 * must assume that it has an offset.
 */
static int
pr_obj(expl, pe, parm, p, mod)
int     expl;			/* do we look at the tag */
PE      pe;
char	**parm;
tpe	*p;
modtyp	*mod;			/* Module it is from */
{
    int     cnt = 0;


    DLOG (psap_log, LLOG_DEBUG, ("1st Decoding the type %d", p->pe_type));

    while (p->pe_type != PE_END) {

	if (ISDTYPE(p) && expl && CHKTAG(mod, p, pe) == 0) {
	    if (DEFAULT(p)) 
		return pepsylose (mod, p, pe,
				  "pr_obj:Default not implemented");
	    else if (OPTIONAL(p)) {
		if (ALLOC_MEM(p, parm))
		    fix_mem(parm, p);
		goto next;
	    } else
		return pepsylose (mod, p, pe, "pr_obj:missing mandatory parameter");
	}
	DLOG (psap_log, LLOG_DEBUG, ("2nd Decoding the type %d", p->pe_type));

	switch (p->pe_type) {
	case PE_END:
	case PE_START:
	    return pepsylose (mod, p, pe, "pr_obj:illegal END/START");

	/*
	 * This allows Functions to be called at the very end of the 
	 * decoding -- With the decoded data - I hope - very messy
	 */
	case UCODE:
	    if (mod->md_ducode == NULLIFP
	    || (*mod->md_ducode) (parm, pe, p, 0) == NOTOK)
		goto bad;
	    break;


	default:
	    if (pr_type(expl, pe, parm, p, mod) == NOTOK)
		goto bad;
	    break;
	}
	if (ISDTYPE(p) && cnt > 0)
	    return pepsylose (mod, p, NULLPE, "pr_obj:compound type found");
next:
	if (NEXT_TPE(p) == NULLTPE)
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
pr_type(expl, pe, parm, p, mod)
int     expl;			/* do we look at the tag */
PE      pe;
char	**parm;
tpe	*p;
modtyp	*mod;			/* Module it is from */
{
    int	cnt = 0;
    int len;
    OID     oid;
    char    *nparm;
    /* there is no such pointer as &(*parm + p->pe_ucode) (ucode non zero) so
     * we fudge it by making a temporary one. As no memory needs to be
     * allocated through it this should work
     */


    DLOG (psap_log, LLOG_DEBUG, ("pr_type:type %d", p->pe_type));

    while (p->pe_type != PE_END) {

	if (ISDTYPE(p) && expl && CHKTAG(mod, p, pe) == 0) {
	    if (DEFAULT(p)) {
		if (setdval(p, FDFLT_B(p), parm, mod) == NOTOK)
		    goto bad;
		return (NO_DATA_USED);
	    } else if (OPTIONAL(p)) {
		if (ALLOC_MEM(p, parm))
		    fix_mem(parm, p);
		return (NO_DATA_USED);
	    } else
		return pepsylose (mod, p, pe,
				  "pr_type:missing mandatory parameter");
	}
	DLOG (psap_log, LLOG_DEBUG, ("pr_type: type %d", p->pe_type));

	switch (p->pe_type) {
	case PE_END:
	case PE_START:
	    return pepsylose (mod, p, pe, "pr_type:illegal END/START");

	case BOPTIONAL:
	    if (CHKTAG(mod, p + 1, pe) == 0) {
		if (ALLOC_MEM(p, parm))
                    fix_mem(parm, p);
		if (IF_USELECT(p)) {
		    if (p -> pe_ucode >= 0 &&
			(mod -> md_ducode == NULLIFP ||
			 (*mod->md_ducode)(parm, pe, p, 0) == NOTOK))
			goto bad;
		}
		else CLR_OPT_PRESENT(p, parm);
		return (NO_DATA_USED);
	    }
	    if (IF_USELECT(p)) {
		if (p -> pe_ucode >= 0 &&
		    (mod -> md_ducode == NULLIFP ||
		     (*mod->md_ducode)(parm, pe, p, 1) == NOTOK))
		    goto bad;
	    }
	    else
		SET_OPT_PRESENT(p, parm);
	    p++;
	    continue;

	case FREE_ONLY:	/* the next entry(s) only for freeing routines 
			 * so skip this and next entry
			 */
	    break;
	
	case FFN_CALL:	/* call function to free - skip this here */
	    break;

	case UCODE:
	    if (mod->md_ducode == NULLIFP
	    || (*mod->md_ducode) (parm, pe, p, 0) == NOTOK)
		goto bad;
	    break;

	case ETAG:
	    switch (p->pe_ucode) {
	    default:
		p++;
		if (pr_etype(pe->pe_cons, parm, p, mod) == NOTOK)
		    return (NOTOK);
	    }
	    break;

	case MEMALLOC:
	    if (*parm)
		break;	/* already allocated */
	    if ((*(parm) = (char *) calloc(1, (unsigned ) p->pe_tag)) == NULL)
		return oom(mod, p);
	    break;

	/* The difference between here and the en_type makes me think
	 * that this is never called for SEQ_START ???? 
	 */
	case SSEQ_START:
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (pr_seq(pe, &nparm, p, mod) == NOTOK)
		    goto bad;
	    } else if (pr_seq(pe, parm, p, mod) == NOTOK)
		    goto bad;
	    break;

	case SEQ_START:
	    if (pr_seq(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case SSEQOF_START:
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (pr_seqof(pe, &nparm, p, mod) == NOTOK)
		    goto bad;
	    } else if (pr_seqof(pe, parm, p, mod) == NOTOK)
		goto bad;
	    break;

	case SEQOF_START:
	    if (pr_seqof(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case SSET_START:
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (pr_set(pe, &nparm, p, mod) == NOTOK)
		    goto bad;
	    } else if (pr_set(pe, parm, p, mod) == NOTOK)
		goto bad;
	    break;

	case SET_START:
	    if (pr_set(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case SSETOF_START:
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (pr_setof(pe, &nparm, p, mod) == NOTOK)
		    goto bad;
	    } else if (pr_setof(pe, parm, p, mod) == NOTOK)
		goto bad;
	    break;

	case SETOF_START:
	    if (pr_setof(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case IMP_OBJ:
	    p++;
	    if (p->pe_type == EXTOBJ) {
		if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0|F_CI, (int *)0,
		      (char **) 0, (char **) (*parm + p->pe_ucode)) == NOTOK)
		      goto bad;
	    } else if (p->pe_type == SEXTOBJ) {
		if (p->pe_ucode > 0) {
		    if (*parm == NULLCP)
			return inpm(mod, p);
		    nparm = *parm + p->pe_ucode;
		    if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0|F_CI, (int *)0,
			  (char **) 0, (char **) &nparm) == NOTOK)
			  goto bad;
		} else if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0|F_CI, (int *)0,
			  (char **) 0, (char **) parm) == NOTOK)
		      goto bad;
	    } else {
		if (p->pe_type == SOBJECT) {
		    if (p->pe_ucode > 0) {
			if (*parm == NULLCP)
			    return inpm(mod, p);
			nparm = *parm + p->pe_ucode;
			if (pr_obj(0, pe, &nparm, mod->md_dtab[p->pe_tag] + 1, mod)
				== NOTOK)
			    goto bad;
		    } else if (pr_obj(0, pe, parm, mod->md_dtab[p->pe_tag] + 1, mod)
				== NOTOK)
			goto bad;
		} else if (pr_obj(0, pe, (char **) (*parm + p->pe_ucode),
			mod->md_dtab[p->pe_tag] + 1, mod) == NOTOK)
		    goto bad;
	    }
	    break;

	case SOBJECT:
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (pr_obj(expl, pe, &nparm, mod->md_dtab[p->pe_tag] + 1, mod)==NOTOK)
		    goto bad;
	    } else if (pr_obj(expl, pe, parm, mod->md_dtab[p->pe_tag] + 1, mod)==NOTOK)
		goto bad;
	    break;

	case OBJECT:

	    if (pr_obj(expl, pe, (char **) (*parm + p->pe_ucode),
	       mod->md_dtab[p->pe_tag] + 1, mod)==NOTOK)
		goto bad;
	    break;

	case SCHOICE_START:
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (pr_choice(pe, &nparm, p, mod) == NOTOK)
		    goto bad;
	    } else if (pr_choice(pe, parm, p, mod) == NOTOK)
		goto bad;
	    break;

	case CHOICE_START:
	    if (pr_choice(pe, parm, p, mod) == NOTOK)
		goto bad;
	    break;

	case SEXTOBJ:
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		if (p[1].pe_type != EXTMOD)
		    return pepsylose (mod, p, pe, "pr_type: missing EXTMOD");
		nparm = *parm + p->pe_ucode;
		if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *)0,
		      (char **) 0, &nparm) == NOTOK)
		      goto bad;
		} else if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *)0,
		      (char **) 0, parm) == NOTOK)
		  goto bad;
	    break;

	case EXTOBJ:
	    if (p[1].pe_type != EXTMOD)
		return pepsylose (mod, p, pe, "pr_type: missing EXTMOD");
	    if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *)0,
		  (char **) 0, (char **) (*parm + p->pe_ucode)) == NOTOK)
		  goto bad;
	    break;

	case INTEGER:
	    if (pe != NULLPE) {
		DLOG (psap_log, LLOG_DEBUG, ("pr_type:integer %d",
					     prim2num(pe)));
		if (((*(integer *) (*parm + p->pe_ucode)) = prim2num(pe))
		    == NOTOK && pe->pe_errno != PE_ERR_NONE)
		    return pepsylose (mod, p, pe, "pr_type:bad integer %s",
				      pe_error(pe->pe_errno));
	    }
	    break;

#ifdef	PEPSY_REALS
	case REALTYPE:
	    if (pe != NULLPE) {
		DLOG (psap_log, LLOG_DEBUG, ("pr_type:Real %g",
					      prim2real(pe)));

		if (((*(double *) (*parm + p->pe_ucode)) = prim2real(pe))
		    == NOTOK && pe->pe_errno != PE_ERR_NONE)
		    return pepsylose (mod, p, pe, "pr_type:bad real %s",
				      pe_error(pe->pe_errno));
	    }
	    break;

#endif

	case BOOLEAN:
	    if (pe != NULLPE) {
		int	i;

		DLOG (psap_log, LLOG_DEBUG, ("boolean %d",
					     prim2flag(pe)));
		if ((i = prim2flag (pe)) == NOTOK)
		      return pepsylose (mod, p, pe, "pr_type:bad integer %s",
					pe_error(pe->pe_errno));
		*(char *) (*parm + p->pe_ucode) = i & 0xff;
	    }
	    break;

	case T_NULL:
	    break;

	case SANY:
	    if (pe != NULLPE) {
		((*(PE *) (parm + p->pe_ucode)) = pe)->pe_refcnt++;
		if (pe->pe_errno != PE_ERR_NONE)
		    return pepsylose (mod, p, pe, "pr_type:bad ANY %s",
				      pe_error(pe->pe_errno));
	    }
	    break;

	case ANY:
	    if (pe != NULLPE) {
		(*(PE *) (*parm + p->pe_ucode) = pe)->pe_refcnt++;
		if (pe->pe_errno != PE_ERR_NONE)
		    return pepsylose(mod, p, pe, "pr_type:bad ANY %s",
				     pe_error(pe->pe_errno));
	    }
	    break;

	case SOCTETSTRING:
	    if (pe != NULLPE) {
		if ((*((struct qbuf **) (parm + p->pe_ucode)) = prim2qb(pe)) ==
		    (struct qbuf *) NULL && pe->pe_errno != PE_ERR_NONE)
		    return pepsylose(mod, p, pe, "pr_type:bad octet string %s",
				     pe_error(pe->pe_errno));
	    }
	    break;

	case OCTETSTRING:
	    if (pe != NULLPE) {
		if ((*((struct qbuf **) (*parm + p->pe_ucode))
		     = prim2qb(pe)) == (struct qbuf *) NULL
		    && pe->pe_errno != PE_ERR_NONE)
		    return pepsylose (mod, p, pe,
				      "pr_type:bad octet string %s",
				      pe_error(pe->pe_errno));
	    }
	    break;

	case T_STRING:
	    if ((*((char **) (*parm + p->pe_ucode)) = prim2str(pe, &len))
		== NULLCP && pe->pe_errno != PE_ERR_NONE)
		  return pepsylose (mod, p, pe, "pr_type:bad octet string %s",
				  pe_error(pe->pe_errno));
	    /* undocumented feature of prim2str that it adds a NULL char
	     * to the end of the string
	     */
	    break;

	case BITSTR_PTR:
	    if (p[1].pe_type != BITSTR_LEN)
		return pepsylose (mod, &p[1], NULLPE,
				  "pr_type: missing BITSTR_PTR");

	    pe = prim2bit(pe);
	    if ((*((char **) (*parm + p->pe_ucode)) =
	        bitstr2strb(pe, (int *)(*parm + (p + 1)->pe_ucode)))
		== NULLCP && pe->pe_errno != PE_ERR_NONE)
		  return pepsylose (mod, p, pe, "pr_type:bad bit string %s",
				  pe_error(pe->pe_errno));
	    break;


	case OCTET_PTR:
	    if (p[1].pe_type != OCTET_LEN)
		return pepsylose (mod, &p[1], NULLPE,
				  "pr_type: missing OCTET_PTR");
	    if ((*((char **) (*parm + p->pe_ucode)) =
	        prim2str(pe, (int *)(*parm + (p + 1)->pe_ucode)))
		== NULLCP && pe->pe_errno != PE_ERR_NONE)
		  return pepsylose (mod, p, pe, "pr_type:bad octet string %s",
				  pe_error(pe->pe_errno));
	    break;


	case SBITSTRING:
	    if (pe != NULLPE) {
		if ((*((PE *) (parm + p->pe_ucode)) = prim2bit(pe_cpy(pe))) == NULLPE)
		    return pepsylose (mod, p, pe, "pr_type:out of memory");
	    }
	    break;

	case BITSTRING:
	    if (pe != NULLPE) {
		if ((*((PE *) (*parm + p->pe_ucode)) =
		     prim2bit(pe_cpy(pe))) == NULLPE)
		    return pepsylose(mod, p, pe, "pr_type:out of memory");
	    }
	    break;

	case SOBJID:
	    /*
	     * This is messy because ISODE's library routine uses a
	     * static. Don't know why they do
	     */
	    if ((oid = prim2oid(pe + p->pe_ucode)) == NULLOID
		|| (*(OID *) parm = oid_cpy(oid)) == NULLOID) {
		if (oid && oid->oid_elements) {
		    free((char *) oid->oid_elements);
		    oid->oid_elements = NULL;
		}
		return pepsylose (mod, p, pe,
				  "pr_type:Object Identifier: out of memory");
	    }
	    if (oid && oid->oid_elements) {
		free((char *) oid->oid_elements);
		oid->oid_elements = NULL;
	    }
	    break;

	case OBJID:
	    if ((oid = prim2oid(pe)) == NULLOID
		|| (*(OID *) (*parm + p->pe_ucode) = oid_cpy(oid)) == NULLOID) {
		if (oid && oid->oid_elements) {
		    free((char *) oid->oid_elements);
		    oid->oid_elements = NULL;
		}
		return pepsylose (mod, p, pe,
				  "pr_type:Object Identifier: out of memory");
	    }
	    if (oid && oid->oid_elements) {
		free((char *) oid->oid_elements);
		oid->oid_elements = NULL;
	    }
	    break;

	case FN_CALL:
	    if ((FN_PTR(mod, p))(parm, pe) == NOTOK)
		return pepsylose (mod, p, NULLPE,
				  "pr_type:FN_CALL:call failed");
	    break;

	default:
	    return pepsylose (mod, p, pe, "pr_type: type not implemented");
	}
	if (ISDTYPE(p) && cnt > 0)
	    return pepsylose (mod, p, pe, "pr_type:compound type found");

	if (ISDTYPE(p) && pe != NULLPE)
	    return (OK);
	if (NEXT_TPE(p) == NULLTPE)
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
pr_seq(head, parm, p, mod)
PE      head;
char	**parm;
tpe	*p;
modtyp	*mod;			/* Module it is from */
{
    PE      pe;
    int    *popt = NULL;	/* Pointer to optional field */
    int     optcnt = 0;		/* Number of optionals bits so far */
    char    *nparm;

    if (p->pe_type != SEQ_START && p->pe_type != SSEQ_START)
	return pepsylose (mod, p, head, "pr_seq: missing SEQ_START");
    p++;

    if (p->pe_type == DFLT_B)
	p++;

    pe = first_member(head);
    while (p->pe_type != PE_END) {
	DLOG (psap_log, LLOG_DEBUG, ("pr_seq:type %d", p->pe_type));

	if (ISDTYPE(p) && OPTIONAL(p)) {
	    switch (p->pe_type) {
	    case INTEGER:
	    case REALTYPE:
	    case BOOLEAN:
	    case T_NULL:
		if (pe == NULLPE || CHKTAG(mod, p, pe) == 0) {
		    optcnt++;
		    goto next;
		}
		SETBIT(*popt, optcnt++);
		break;


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
		if(setdval(p, FDFLT_B(p), parm, mod) == NOTOK)
		    goto bad;
		goto next;
	    } else 
		return pepsylose (mod, p, pe,
				  "pr_seq:missing mandatory parameter");
	}
	switch (p->pe_type) {
	case OPTL:
	    popt = (int *) (*parm + p->pe_ucode);
	    break;

	case FREE_ONLY:	/* the next entry(s) only for freeing routines 
			 * so skip this and next entry
			 */
	    break;
	
	case FFN_CALL:	/* call function to free - skip this here */
	    break;

	case UCODE:
	    if (mod->md_ducode == NULLIFP
	    || (*mod->md_ducode) (parm, pe, p, 0) == NOTOK)
		goto bad;
	    break;

	case BOPTIONAL:
	    if (pe == NULLPE || CHKTAG(mod, p + 1, pe) == 0) {
		if (ALLOC_MEM(p, parm))
                    fix_mem(parm, p);
		if (IF_USELECT(p)) {
		    if (p -> pe_ucode >= 0 &&
			(mod -> md_ducode == NULLIFP ||
			 (*mod->md_ducode)(parm, pe, p, 0) == NOTOK))
			goto bad;
		}
		else CLR_OPT_PRESENT(p, parm);
		goto next;
	    }
	    if (IF_USELECT(p)) {
		if (p -> pe_ucode >= 0 &&
		    (mod -> md_ducode == NULLIFP ||
		     (*mod->md_ducode)(parm, pe, p, 1) == NOTOK))
		    goto bad;
	    }
	    else SET_OPT_PRESENT(p, parm);
	    p++;
	    continue;

	case ETAG:
	    if (pr_type(1, pe, parm, p, mod) == NOTOK)
		goto bad;
	    break;

	case MEMALLOC:
	    if (*parm)
		break;	/* already allocated */
	    if ((*(parm) = (char *) calloc(1, (unsigned ) p->pe_tag))==NULLCP)
		return oom(mod, p);
	    break;

	case SEQ_START:
	    if (pr_seq(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case SEQOF_START:
	    if (pr_seqof(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case SET_START:
	    if (pr_set(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case SETOF_START:
	    if (pr_setof(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case IMP_OBJ:
	    p++;
	    if (p->pe_type == EXTOBJ) {
		if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0|F_CI, (int *)0,
		      (char **) 0, (char **) (*parm + p->pe_ucode)) == NOTOK)
		      goto bad;
	    } else if (p->pe_type == SEXTOBJ) {
		if (p->pe_ucode > 0) {
		    if (*parm == NULLCP)
			return inpm(mod, p);
		    nparm = *parm + p->pe_ucode;
		    if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0|F_CI, (int *)0,
			  (char **) 0, (char **) &nparm) == NOTOK)
			  goto bad;
		} else if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0|F_CI, (int *)0,
			  (char **) 0, (char **) parm) == NOTOK)
		      goto bad;
	    } else {
		if (p->pe_type == SOBJECT) {
		    if (p->pe_ucode > 0) {
			if (*parm == NULLCP)
			    return inpm(mod, p);
			nparm = *parm + p->pe_ucode;
			if (pr_obj(0, pe, &nparm, mod->md_dtab[p->pe_tag] + 1, mod)
				== NOTOK)
			    goto bad;
		    } else 
			if (pr_obj(0, pe, parm, mod->md_dtab[p->pe_tag] + 1, mod)
				== NOTOK)
			    goto bad;
		} else if (pr_obj(0, pe, (char **) (*parm + p->pe_ucode),
			mod->md_dtab[p->pe_tag] + 1, mod) == NOTOK)
		    goto bad;
	    }
	    break;

	case SOBJECT:
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (pr_obj(1, pe, &nparm, mod->md_dtab[p->pe_tag] + 1, mod) == NOTOK)
		    goto bad;
	    } else if (pr_obj(1, pe, parm, mod->md_dtab[p->pe_tag] + 1, mod) == NOTOK)
		goto bad;
	    break;

	case OBJECT:
	    if (pr_obj(1, pe, (char **) (*parm + p->pe_ucode),
		    mod->md_dtab[p->pe_tag] + 1, mod) == NOTOK)
		goto bad;
	    break;

	case SCHOICE_START:
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (pr_choice(pe, &nparm, p, mod) == NOTOK)
		    goto bad;
	    } else if (pr_choice(pe, parm, p, mod) == NOTOK)
		    goto bad;
	    break;

	case CHOICE_START:
	    if (pr_choice(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case SEXTOBJ:
	    if (p[1].pe_type != EXTMOD)
		return pepsylose (mod, p, pe, "pr_seq: missing EXTMOD");
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *) 0,
		      (char **) 0, &nparm) == NOTOK)
		      goto bad;
	    } else if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *) 0,
		      (char **) 0, parm) == NOTOK)
		      goto bad;
	    break;

	case EXTOBJ:
	    if (p[1].pe_type != EXTMOD)
		return pepsylose (mod, p, pe, "pr_seq: missing EXTMOD");

	    if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *) 0,
		  (char **) 0, (char **) (*parm + p->pe_ucode)) == NOTOK)
		  goto bad;
	    break;

	default:
	    /* only called if we have a match */
	    if (pr_type(1, pe, parm, p, mod) == NOTOK)
		goto bad;
	    break;
	}

	if (ISDTYPE(p) && pe != NULLPE)
	    pe = next_member(head, pe);
next:
	if (NEXT_TPE(p) == NULLTPE)
	    goto bad;
    }

    return (OK);

bad:
    return (NOTOK);
}


/*
 * Parse a set, calling appropriate routines to parse each sub type
 */
static int
pr_set(head, parm, p, mod)
PE      head;
char	**parm;
tpe	*p;
modtyp	*mod;			/* Module it is from */
{
    PE      pe;
    int    *popt = NULL;	/* Pointer to optional field */
    int     optcnt = 0;		/* Number of optionals bits so far */
    char    *nparm;

    if (p->pe_type != SET_START && p->pe_type != SSET_START)
	return pepsylose (mod, p, NULLPE, "pr_seq: missing SET_START");
    p++;

    if (p->pe_type == DFLT_B)
	p++;

    while (p->pe_type != PE_END) {
	DLOG (psap_log, LLOG_DEBUG, ("pr_set type %d", p->pe_type));

	if (ISDTYPE(p) && OPTIONAL(p)) {
	    switch (p->pe_type) {
	    case INTEGER:
	    case REALTYPE:
	    case BOOLEAN:
	    case T_NULL:
		if ((pe = setpresent(head, p, mod)) == NULLPE) {
		    optcnt++;
		    goto next;
		}
		SETBIT(*popt, optcnt++);
		break;


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
		if (setdval(p, FDFLT_B(p), parm, mod) == NOTOK)
		    goto bad;
		goto next;
	    } else
		return pepsylose (mod, p, pe,
				  "pr_set:missing mandatory parameter");
	}
	switch (p->pe_type) {
	case OPTL:
	    popt = (int *) (*parm + p->pe_ucode);
	    break;

	case FREE_ONLY:	/* the next entry(s) only for freeing routines 
			 * so skip this and next entry
			 */
	    break;
	
	case FFN_CALL:	/* call function to free - skip this here */
	    break;

	case UCODE:
	    if (mod->md_ducode == NULLIFP
	    || (*mod->md_ducode) (parm, pe, p, 0) == NOTOK)
		goto bad;
	    break;

	case BOPTIONAL:
	    if ((pe = setpresent(head, p + 1, mod)) == NULLPE) {
		if (ALLOC_MEM(p, parm))
                    fix_mem(parm, p);
		if (IF_USELECT(p)) {
		    if (p -> pe_ucode >= 0 &&
			(mod -> md_ducode == NULLIFP ||
			(*mod->md_ducode)(parm, pe, p, 0) == NOTOK))
			goto bad;
		}
		else CLR_OPT_PRESENT(p, parm);
		goto next;
	    }
	    if (IF_USELECT(p)) {
		if (p -> pe_ucode >= 0 &&
		    (mod -> md_ducode == NULLIFP ||
		     (*mod->md_ducode)(parm, pe, p, 1) == NOTOK))
		    goto bad;
	    }
	    else SET_OPT_PRESENT(p, parm);
	    p++;
	    continue;

	case ETAG:
	    if (pr_type(1, pe, parm, p, mod) == NOTOK)
		goto bad;
	    break;

	case MEMALLOC:
	    if (*parm)
		break;	/* already allocated */
	    if ((*(parm) = (char *) calloc(1, (unsigned ) p->pe_tag))==NULLCP)
		return oom(mod, p);
	    break;

	case SEQ_START:
	    if (pr_seq(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case SEQOF_START:
	    if (pr_seqof(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case SET_START:
	    if (pr_set(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case SETOF_START:
	    if (pr_setof(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case IMP_OBJ:
	    p++;
	    if (p->pe_type == EXTOBJ) {
		if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0|F_CI, (int *) 0,
		      (char **) 0, (char **) (*parm + p->pe_ucode)) == NOTOK)
		      goto bad;
	    } else if (p->pe_type == SEXTOBJ) {
		if (p->pe_ucode > 0) {
		    if (*parm == NULLCP)
			return inpm(mod, p);
		    nparm = *parm + p->pe_ucode;
		    if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0|F_CI, (int *) 0,
			  (char **) 0, (char **) &nparm) == NOTOK)
			  goto bad;
		} else if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0|F_CI, (int *) 0,
			  (char **) 0, (char **) parm) == NOTOK)
			  goto bad;
	    } else {
		if (p->pe_type == SOBJECT) {
		    if (p->pe_ucode > 0) {
			if (*parm == NULLCP)
			    return inpm(mod, p);
			nparm = *parm + p->pe_ucode;
			if (pr_obj(0, pe, &nparm, mod->md_dtab[p->pe_tag] + 1, mod)
				== NOTOK)
			    goto bad;
		    } else if (pr_obj(0, pe, parm, mod->md_dtab[p->pe_tag] + 1, mod)
				== NOTOK)
			    goto bad;
		} else if (pr_obj(0, pe, (char **) (*parm + p->pe_ucode),
			mod->md_dtab[p->pe_tag] + 1, mod) == NOTOK)
		    goto bad;
	    }
	    break;

	case SOBJECT:
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (pr_obj(1, pe, &nparm, mod->md_dtab[p->pe_tag] + 1, mod)== NOTOK)
		    goto bad;
	    } else if (pr_obj(1, pe, parm, mod->md_dtab[p->pe_tag] + 1, mod)== NOTOK)
		    goto bad;
	    break;

	case OBJECT:
	    if (pr_obj(1, pe, (char **) (*parm + p->pe_ucode),
		    mod->md_dtab[p->pe_tag] + 1, mod) == NOTOK)
		goto bad;
	    break;

	case SCHOICE_START:
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (pr_choice(pe, &nparm, p, mod) == NOTOK)
		    goto bad;
	    } else if (pr_choice(pe, parm, p, mod) == NOTOK)
		goto bad;
	    break;

	case CHOICE_START:
	    if (pr_choice(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
		goto bad;
	    break;

	case SEXTOBJ:
	    if (p[1].pe_type != EXTMOD)
		return pepsylose (mod, p, pe, "pr_set: missing EXTMOD");
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *) 0,
		      (char **) 0, &nparm) == NOTOK)
		      goto bad;
	    } else if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *) 0,
		      (char **) 0, parm) == NOTOK)
		      goto bad;
	    break;

	case EXTOBJ:
	    if (p[1].pe_type != EXTMOD) 
		return pepsylose (mod, p, pe, "pr_set: missing EXTMOD");

	    if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *) 0,
		  (char **) 0, (char **) (*parm + p->pe_ucode)) == NOTOK)
		  goto bad;
	    break;

	default:
	    if (pr_type(1, pe, parm, p, mod) == NOTOK)
		goto bad;
	    break;
	}

next:
	if (NEXT_TPE(p) == NULLTPE)
	    goto bad;
    }

    return (OK);

bad:
    return (NOTOK);

}


/*
 * Parse a sequence of calling appropriate routines to parse each sub
 * type
 */
static int
pr_seqof(head, parm, p, mod)
PE      head;
char	**parm;
tpe    *p;
modtyp *mod;			/* Module it is from */
{
    PE      pe;
    tpe    *start;		/* first entry in list */
    int     dflt = 0;
    char    *nparm;

    if (p->pe_type != SEQOF_START && p->pe_type != SSEQOF_START)
	return pepsylose (mod, p, head, "pr_seqof:missing SEQOF_START");

    p++;

    if (p->pe_type == DFLT_B)
	p++;

    start = p;

    pe = first_member(head);
    while (pe != NULLPE) {
	while (p->pe_type != PE_END) {

	    DLOG (psap_log, LLOG_DEBUG, ("pr_seqof type %d", p->pe_type));

	    if (ISDTYPE(p) && CHKTAG(mod, p, pe) == 0) {
		if (DEFAULT(p)) {
		    if (setdval(p, FDFLT_B(p), parm, mod) == NOTOK)
			goto bad;
		    goto next;
		} else if (OPTIONAL(p)) {
		    if (ALLOC_MEM(p, parm))
			fix_mem(parm, p);
		    goto next;
		} else
		    return pepsylose (mod, p, pe,
				      "pr_seqof:missing mandatory parameter");
	    }
	    switch (p->pe_type) {
	    case FREE_ONLY:	/* the next entry(s) only for freeing routines 
			     * so skip this and next entry
			     */
		break;
	    
	    case FFN_CALL:	/* call function to free - skip this here */
		break;

	    case UCODE:
		if (mod->md_ducode == NULLIFP
		|| (*mod->md_ducode) (parm, pe, p, 0) == NOTOK)
		    goto bad;
		break;

	    case BOPTIONAL:
		if (pe == NULLPE || CHKTAG(mod, p + 1, pe) == 0) {
		    if (ALLOC_MEM(p, parm))
			fix_mem(parm, p);
		    if (IF_USELECT(p)) {
			if (p -> pe_ucode >= 0 &&
			    (mod -> md_ducode == NULLIFP ||
			     (*mod->md_ducode)(parm, pe, p, 0) == NOTOK))
			    goto bad;
		    }
		    else CLR_OPT_PRESENT(p, parm);
		    goto next;
		}
		if (IF_USELECT(p)) {
		    if (p -> pe_ucode >= 0 &&
			(mod -> md_ducode == NULLIFP ||
			 (*mod->md_ducode)(parm, pe, p, 1) == NOTOK))
			goto bad;
		}
		else SET_OPT_PRESENT(p, parm);
		p++;
		continue;

	    case ETAG:
		if (pr_type(1, pe, parm, p, mod) == NOTOK)
		    goto bad;
		break;

	    case MEMALLOC:
		if (*parm)
		    break;	/* already allocated */
		if ((*(parm) = (char *) calloc(1, (unsigned ) p->pe_tag))
			== NULLCP)
		    return oom(mod, p);
		break;

	    case SCTRL:
		parm = (char **) ((char *) *parm + p->pe_ucode);
		break;

	    case SEQ_START:
		if (pr_seq(pe, (char **) (*parm + p->pe_ucode), p, mod) ==NOTOK)
		goto bad;
		break;

	    case SEQOF_START:
		if (pr_seqof(pe, (char **) (*parm + p->pe_ucode), p,mod)==NOTOK)
		    goto bad;
		break;

	    case SET_START:
		if (pr_set(pe, (char **) (*parm + p->pe_ucode), p, mod) ==NOTOK)
		    goto bad;
		break;

	    case SETOF_START:
		if (pr_setof(pe, (char **)(*parm + p->pe_ucode), p, mod)==NOTOK)
		    goto bad;
		break;

	    case SOBJECT:
		if (p->pe_ucode > 0) {
		    if (*parm == NULLCP)
			return inpm(mod, p);
		    nparm = *parm + p->pe_ucode;
		    if (pr_obj(1,pe, &nparm, mod->md_dtab[p->pe_tag]+1, mod)==NOTOK)
			goto bad;
		} else if (pr_obj(1,pe, parm, mod->md_dtab[p->pe_tag]+1, mod)==NOTOK)
			goto bad;
		break;

	    case OBJECT:
		if (pr_obj(1, pe, (char **) (*parm + p->pe_ucode),
			mod->md_dtab[p->pe_tag] + 1, mod) == NOTOK)
		    goto bad;
		break;

	    case SCHOICE_START:
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (pr_choice(pe, &nparm, p, mod) == NOTOK)
		    goto bad;
		break;

	    case CHOICE_START:
		if (pr_choice(pe, (char **)(*parm + p->pe_ucode), p,mod)==NOTOK)
		    goto bad;
		break;

	    case SEXTOBJ:
		if (p[1].pe_type != EXTMOD)
		    return pepsylose (mod, p, pe, "pr_seqof: missing EXTMOD");
		if (p->pe_ucode > 0) {
		    if (*parm == NULLCP)
			return inpm(mod, p);
		    nparm = *parm + p->pe_ucode;
		    if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *) 0,
			  (char **) 0, &nparm) == NOTOK)
			  goto bad;
		} else if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *) 0,
			  (char **) 0, parm) == NOTOK)
			  goto bad;
		break;

	    case EXTOBJ:
		if (p[1].pe_type != EXTMOD) 
		    return pepsylose (mod, p, pe, "pr_seqof: missing EXTMOD");

		if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *) 0,
		      (char **) 0, (char **) (*parm + p->pe_ucode)) == NOTOK)
		      goto bad;
		break;

	    default:
		if (pr_type(1, pe, parm, p, mod) == NOTOK)
		    goto bad;
		break;
	    }

	    if (ISDTYPE(p) && dflt == 0)
		pe = next_member(head, pe);
    next:
	    if (NEXT_TPE(p) == NULLTPE)
		goto bad;
	}
	parm = (char **) (*parm + p->pe_ucode);
	p = start;
    }

    return (OK);

bad:
    return (NOTOK);
}

/*
 * Parse a setof, calling appropriate routines to parse each sub type
 */
static int
pr_setof(head, parm, p, mod)
PE      head;
char	**parm;
tpe    *p;
modtyp *mod;			/* Module it is from */
{
    PE      pe;
    tpe    *start;
    char	*nparm;

    if (p->pe_type != SETOF_START && p->pe_type != SSETOF_START)
	return pepsylose(mod, p, head, "pr_setof: missing SETOF_START");
    p++;

    if (p->pe_type == DFLT_B)
	p++;

    start = p;
    pe = first_member(head);

    for (pe = first_member(head); pe; pe = next_member(head, pe)) {
	while (p->pe_type != PE_END) {
	    DLOG (psap_log, LLOG_DEBUG, ("pr_setof type %d", p->pe_type));

	    if (pe == NULLPE || CHKTAG(mod, p, pe) == 0) {
		if (DEFAULT(p)) {
		    if (setdval(p, FDFLT_B(p), parm, mod) == NOTOK)
			goto bad;
		    goto next;
		} else
		    return pepsylose (mod, p, pe,
				      "pr_setof:missing mandatory parameter");
	    }

	    switch (p->pe_type) {
	    case FREE_ONLY:	/* the next entry(s) only for freeing routines 
			     * so skip this and next entry
			     */
		break;
	    
	    case FFN_CALL:	/* call function to free - skip this here */
		break;

	    case UCODE:
		if (mod->md_ducode == NULLIFP
		|| (*mod->md_ducode) (parm, pe, p, 0) == NOTOK)
		    goto bad;
		break;

	    case BOPTIONAL:
		if ((pe = setpresent(head, p + 1, mod)) == NULLPE) {
		    if (ALLOC_MEM(p, parm))
			fix_mem(parm, p);
		    if (IF_USELECT(p)) {
			if (p -> pe_ucode >= 0 &&
			    (mod -> md_ducode == NULLIFP ||
			     (*mod->md_ducode)(parm, pe, p, 0) == NOTOK))
			    goto bad;
		    }
		    else CLR_OPT_PRESENT(p, parm);
		    goto next;
		}
		if (IF_USELECT(p)) {
		    if (p -> pe_ucode >= 0 &&
			(mod -> md_ducode == NULLIFP ||
			 (*mod->md_ducode)(parm, pe, p, 1) == NOTOK))
			goto bad;
		}
		else SET_OPT_PRESENT(p, parm);
		p++;
		continue;

	    case ETAG:
		if (pr_type(1, pe->pe_cons, parm, p, mod) == NOTOK)
		    goto bad;
		break;

	    case MEMALLOC:
		if (*parm)
		    break;	/* already allocated */
		if ((*(parm) = (char *) calloc(1, (unsigned )p->pe_tag))
			== NULLCP)
		    return oom(mod, p);
		break;

	    case SCTRL:
		parm = (char **) (*parm + p->pe_ucode);
		break;

	    case SEQ_START:
		if (pr_seq(pe, (char **) (*parm + p->pe_ucode), p, mod) ==NOTOK)
		    goto bad;
		break;

	    case SEQOF_START:
		if (pr_seqof(pe, (char **)(*parm + p->pe_ucode), p, mod)==NOTOK)
		    goto bad;
		break;

	    case SET_START:
		if (pr_set(pe, (char **) (*parm + p->pe_ucode), p, mod) ==NOTOK)
		    goto bad;
		break;

	    case SETOF_START:
		if (pr_setof(pe, (char **)(*parm + p->pe_ucode), p, mod)==NOTOK)
		    goto bad;
		break;

	    case SOBJECT:
		if (p->pe_ucode > 0) {
		    if (*parm == NULLCP)
			return inpm(mod, p);
		    nparm = *parm + p->pe_ucode;
		    if (pr_obj(1, pe, &nparm, mod->md_dtab[p->pe_tag]+1, mod)==NOTOK)
			goto bad;
		} else if (pr_obj(1, pe, parm, mod->md_dtab[p->pe_tag]+1, mod)==NOTOK)
			goto bad;
		break;

	    case OBJECT:
		if (pr_obj(1, pe, (char **) (*parm + p->pe_ucode),
			mod->md_dtab[p->pe_tag] + 1, mod) == NOTOK)
		    goto bad;
		break;

	    case SCHOICE_START:
		if (p->pe_ucode > 0) {
		    if (*parm == NULLCP)
			return inpm(mod, p);
		    nparm = *parm + p->pe_ucode;
		    if (pr_choice(pe, &nparm, p, mod) == NOTOK)
			goto bad;
		} else if (pr_choice(pe, parm, p, mod) == NOTOK)
		    goto bad;
		break;

	    case CHOICE_START:
		if (pr_choice(pe, (char **)(*parm + p->pe_ucode), p,mod)==NOTOK)
		    goto bad;
		break;

	    case SEXTOBJ:
		if (p[1].pe_type != EXTMOD)
		    return pepsylose (mod, p, pe, "pr_setof: missing EXTMOD");
		if (p->pe_ucode > 0) {
		    if (*parm == NULLCP)
			return inpm(mod, p);
		    nparm = *parm + p->pe_ucode;
		    if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI,
			  (int *) 0, (char **) 0, &nparm) == NOTOK)
			  goto bad;
		} else if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI,
			  (int *) 0, (char **) 0, parm) == NOTOK)
			  goto bad;
		break;

	    case EXTOBJ:
		if (p[1].pe_type != EXTMOD)
		    return pepsylose (mod, p, pe, "pr_setof: missing EXTMOD");
		if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI,
		      (int *) 0, (char **) 0, (char **) (*parm + p->pe_ucode)) == NOTOK)
		      goto bad;
		break;

	    default:
		if (pr_type(1, pe, parm, p, mod) == NOTOK)
		    goto bad;
		break;
	    }

    next:
	    if (NEXT_TPE(p) == NULLTPE)
		goto bad;
	}
	parm = (char **) (*parm + p->pe_ucode);
	p = start;
    }

    return (OK);

bad:
    return (NOTOK);
}

/*
 * parse a choice field. This means find which choice is taken
 */
static int
pr_choice(head, parm, p, mod)
PE      head;
char	**parm;
tpe    *p;
modtyp *mod;			/* Module it is from */
{
    int    *poffset;
    int     cnt;
    tpe    *savep = NULLTPE;

    if (p->pe_type != CHOICE_START && p->pe_type != SCHOICE_START)
	return pepsylose (mod, p, head, "pr_choice:missing CHOICE_START");

    p++;

    if (p->pe_type == DFLT_B)
	p++;

    if (p->pe_type == MEMALLOC) {
	    if (*parm == NULLCP) {
		/* not already allocated */
		if ((*(parm) = (char *) calloc(1,(unsigned )p->pe_tag))==NULLCP)
		    return oom(mod, p);
	    }
	p++;
    }
    if (p->pe_type == SCTRL) {
	if (IF_USELECT(p)) {
	    savep = p;
	} else if ((poffset = (int *) (*parm + p->pe_ucode)) == NULL)
	    goto bad;
	p++;
    }
    for (cnt = 1; p->pe_type != PE_END; NEXT_TPE(p)) {
	if (ISDTYPE(p)) {
	    if (ismatch(p, mod, head->pe_class, head->pe_id)) {
		if (pr_etype(head, parm, p, mod) == NOTOK)
		    goto bad;
		if (savep) {
		    if (savep -> pe_ucode >= 0  &&
			(mod -> md_ducode == NULLIFP ||
			 (*mod -> md_ducode)(parm, head, savep, cnt) == NOTOK))
			goto bad;
		}
		else
		    *poffset = cnt;
		NEXT_TPE(p);
		if (p->pe_type == UCODE) {
		    if (mod->md_ducode == NULLIFP
		    || (*mod->md_ducode) (parm, head, p, 0) == NOTOK)
			return (NOTOK);
		}
		return (OK);
	    }
	    cnt++;
	}
    }
    (void) pepsylose(mod, p, head, "pr_choice: no choice taken");
bad:
    return (NOTOK);
}

/*
 * Parse a single type for explicit tag If a basic type parse it, if
 * a compound type call the appropriate parsing routine
 */
static int
pr_etype(pe, parm, p, mod)
PE      pe;
char	**parm;
tpe    *p;
modtyp *mod;			/* Module it is from */
{
    int cnt;
    char    *nparm;

    switch (p->pe_type) {
    case PE_END:
    case PE_START:
	return pepsylose (mod, p, pe, "pr_etype:illegal END/START");

    case FREE_ONLY:	/* the next entry(s) only for freeing routines 
		     * so skip this and next entry
		     */
	break;
    
    case FFN_CALL:	/* call function to free - skip this here */
	break;

    case UCODE:
	if (mod->md_ducode == NULLIFP
	    || (*mod->md_ducode) (parm, pe, p, 0) == NOTOK)
	    goto bad;
	break;

    case BOPTIONAL:
	return pepsylose (mod, p, pe, "pr_etype:illegal BOPTIONAL");
	
    case ETAG:
	switch (p->pe_ucode) {

	default:
	    p++;
	    if (pr_etype(pe->pe_cons, parm, p, mod) == NOTOK)
		goto bad;
	}
	break;

    case MEMALLOC:
	if (*parm)
	    break;	/* already allocated */
	if ((*(parm) = (char *) calloc(1, (unsigned ) p->pe_tag)) == NULLCP)
	    return oom(mod, p);
	break;

    case SEQ_START:
	if (pr_seq(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
	    goto bad;
	break;

    case SSEQOF_START:
	if (p->pe_ucode > 0) {
	    if (*parm == NULLCP)
		return inpm(mod, p);
	    nparm = *parm + p->pe_ucode;
	    if (pr_seqof(pe, &nparm, p, mod) == NOTOK)
		goto bad;
	} else if (pr_seqof(pe, parm, p, mod) == NOTOK)
		goto bad;
	break;

    case SEQOF_START:
	if (pr_seqof(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
	    goto bad;
	break;

    case SSEQ_START:
	if (p->pe_ucode > 0) {
	    if (*parm == NULLCP)
		return inpm(mod, p);
	    nparm = *parm + p->pe_ucode;
	    if (pr_seq(pe, &nparm, p, mod) == NOTOK)
		goto bad;
	} else if (pr_seq(pe, parm, p, mod) == NOTOK)
		goto bad;
	break;

    case SET_START:
	if (pr_set(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
	    goto bad;
	break;

    case SSET_START:
	if (p->pe_ucode > 0) {
	    if (*parm == NULLCP)
		return inpm(mod, p);
	    nparm = *parm + p->pe_ucode;
	    if (pr_set(pe, &nparm, p, mod) == NOTOK)
		goto bad;
	} else if (pr_set(pe, parm, p, mod) == NOTOK)
	    goto bad;
	break;

    case SETOF_START:
	if (pr_setof(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
	    goto bad;
	break;

    case SSETOF_START:
	if (p->pe_ucode > 0) {
	    if (*parm == NULLCP)
		return inpm(mod, p);
	    nparm = *parm + p->pe_ucode;
	    if (pr_setof(pe, &nparm, p, mod) == NOTOK)
		goto bad;
	} else if (pr_setof(pe, parm, p, mod) == NOTOK)
	    goto bad;
	break;

    case IMP_OBJ:
	p++;
	if (p->pe_type == EXTOBJ) {
	    if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0|F_CI, (int *) 0,
		  (char **) 0, (char **) (*parm + p->pe_ucode)) == NOTOK)
		  goto bad;
	} else if (p->pe_type == SEXTOBJ) {
	    if (p->pe_ucode > 0) {
		if (*parm == NULLCP)
		    return inpm(mod, p);
		nparm = *parm + p->pe_ucode;
		if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0|F_CI, (int *) 0,
		      (char **) 0, (char **) &nparm) == NOTOK)
		      goto bad;
	    } else if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 0|F_CI, (int *) 0,
		      (char **) 0, (char **) parm) == NOTOK)
		      goto bad;
	} else {
	    if (p->pe_type == SOBJECT) {
		if (p->pe_ucode > 0) {
		    if (*parm == NULLCP)
			return inpm(mod, p);
		    nparm = *parm + p->pe_ucode;
		    if (pr_obj(0, pe, (char **)&nparm, mod->md_dtab[p->pe_tag]+1, mod)==NOTOK)
			goto bad;
		} else if (pr_obj(0, pe, (char **)parm, mod->md_dtab[p->pe_tag]+1, mod)==NOTOK)
			goto bad;
	    } else if (pr_obj(0, pe, (char **) (*parm + p->pe_ucode),
		    mod->md_dtab[p->pe_tag] + 1, mod) == NOTOK)
		goto bad;
	}
	break;

    case SOBJECT:
	if (p->pe_ucode > 0) {
	    if (*parm == NULLCP)
		return inpm(mod, p);
	    nparm = *parm + p->pe_ucode;
	    if (pr_obj(1, pe, &nparm, mod->md_dtab[p->pe_tag] + 1, mod) == NOTOK)
		goto bad;
	} else if (pr_obj(1, pe, parm, mod->md_dtab[p->pe_tag] + 1, mod) == NOTOK)
	    goto bad;
	break;

    case OBJECT:
	if (pr_obj(1, pe, (char **) (*parm + p->pe_ucode),
		mod->md_dtab[p->pe_tag] + 1, mod) == NOTOK)
	    goto bad;
	break;

    case SCHOICE_START:
	if (p->pe_ucode > 0) {
	    if (*parm == NULLCP)
		return inpm(mod, p);
	    nparm = *parm + p->pe_ucode;
	    if (pr_choice(pe, &nparm, p, mod) == NOTOK)
		goto bad;
	} else if (pr_choice(pe, parm, p, mod) == NOTOK)
	    goto bad;
	break;

    case CHOICE_START:
	if (pr_choice(pe, (char **) (*parm + p->pe_ucode), p, mod) == NOTOK)
	    goto bad;
	break;

    case SEXTOBJ:
	if (p[1].pe_type != EXTMOD)
	    return pepsylose (mod, p, pe, "pr_etype: missing EXTMOD");
	if (p->pe_ucode > 0) {
	    if (*parm == NULLCP)
		return inpm(mod, p);
	    nparm = *parm + p->pe_ucode;
	    if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *) 0,
		  (char **) 0, &nparm) == NOTOK)
		  goto bad;
	} else if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *) 0,
		  (char **) 0, parm) == NOTOK)
	      goto bad;
	break;

    case EXTOBJ:
	if (p[1].pe_type != EXTMOD)
	    return pepsylose (mod, p, pe, "pr_etype: missing EXTMOD");

	if (dec_f(p->pe_tag, EXT2MOD(mod, (p + 1)), pe, 1|F_CI, (int *) 0,
	      (char **) 0, (char **) (*parm + p->pe_ucode)) == NOTOK)
	      goto bad;
	break;

    case INTEGER:
	if (pe != NULLPE) {
	    DLOG (psap_log, LLOG_DEBUG, ("pr_etype:integer %d", prim2num(pe)));

	    if (((*(integer *)(*parm + p->pe_ucode)) = prim2num(pe)) == NOTOK &&
		pe->pe_errno != PE_ERR_NONE) 
		return pepsylose (mod, p, pe, "pr_etype:bad integer %s",
				  pe_error(pe->pe_errno));
	}
	break;

#ifdef	PEPSY_REALS
    case REALTYPE:
	if (pe != NULLPE) {
	    DLOG (psap_log, LLOG_DEBUG, ("pr_etype:Real %g", prim2real(pe)));

	    if (((*(double *) (*parm + p->pe_ucode)) = prim2real(pe))
		== NOTOK && pe->pe_errno != PE_ERR_NONE) 
		  return pepsylose (mod, p, pe, "pr_etype:bad real %s",
				    pe_error(pe->pe_errno));
	}
	break;

#endif

    case BOOLEAN:
	if (pe != NULLPE) {
	    int	    i;

	    DLOG(psap_log, LLOG_DEBUG, ("pr_etype:boolean %d", prim2flag(pe)));

	    if ((i = prim2flag (pe)) == NOTOK)
		return pepsylose (mod, p, pe, "pr_etype:bad integer %s",
				  pe_error(pe->pe_errno));
	    (*(char *) (*parm + p->pe_ucode)) = i & 0xff;
	}
	break;

    case T_NULL:
	break;

    case ANY:
	if (pe != NULLPE) {
	    (*(PE *) (*parm + p->pe_ucode) = pe)->pe_refcnt++;
	    if (pe->pe_errno != PE_ERR_NONE)
		return pepsylose (mod, p, pe, "pr_etype:bad ANY %s",
				  pe_error(pe->pe_errno));
	}
	break;

    case SANY:
	if (pe != NULLPE) {
	    (*(PE *) (parm + p->pe_ucode) = pe) -> pe_refcnt++;
	    if (pe->pe_errno != PE_ERR_NONE)
		return pepsylose (mod, p, pe, "pr_etype:bad ANY %s",
				  pe_error(pe->pe_errno));
	}
	break;

    case SOCTETSTRING:
	if (pe != NULLPE) {
	    if ((*((struct qbuf **) (parm + p->pe_ucode))
		 = prim2qb(pe)) == (struct qbuf *) NULL
		&& pe->pe_errno != PE_ERR_NONE)
		return pepsylose (mod, p, pe, "pr_etype:bad octet string %s",
				  pe_error(pe->pe_errno));
	}
	break;

    case OCTETSTRING:
	if (pe != NULLPE) {
	    if ((*((struct qbuf **) (*parm + p->pe_ucode))
		 = prim2qb(pe)) == (struct qbuf *) NULL
		&& pe->pe_errno != PE_ERR_NONE)
		return pepsylose (mod, p, pe, "pr_etype:bad octet string %s",
				  pe_error(pe->pe_errno));
	}
	break;

    case T_STRING:
	if ((*((char **) (*parm + p->pe_ucode)) = prim2str(pe, &cnt))
	    == NULLCP && pe->pe_errno != PE_ERR_NONE)
	      return pepsylose (mod, p, pe, "pr_type:bad octet string %s",
			      pe_error(pe->pe_errno));
	/* undocumented feature of prim2str that it adds a NULL char
	 * to the end of the string
	 */
	break;

    case OCTET_PTR:
	if (p[1].pe_type != OCTET_LEN)
	    return pepsylose (mod, &p[1], NULLPE,"pr_etype: missing OCTET_PTR");
	if ((*((char **) (*parm + p->pe_ucode)) =
	    prim2str(pe, (int *)(*parm + (p + 1)->pe_ucode)))
	    == NULLCP && pe->pe_errno != PE_ERR_NONE)
	      return pepsylose (mod, p, pe, "pr_etype:bad octet string %s",
			      pe_error(pe->pe_errno));
	break;

	case BITSTR_PTR:
	    if (p[1].pe_type != BITSTR_LEN)
		return pepsylose (mod, &p[1], NULLPE,
				  "pr_etype: missing BITSTR_PTR");
	    pe = prim2bit(pe);
	    if ((*((char **) (*parm + p->pe_ucode)) =
	        bitstr2strb(pe, (int *)(*parm + (p + 1)->pe_ucode)))
		== NULLCP && pe->pe_errno != PE_ERR_NONE)
		  return pepsylose (mod, p, pe, "pr_etype:bad bit string %s",
				  pe_error(pe->pe_errno));
	    break;

    case SBITSTRING:
	if (pe != NULLPE) {
	    if (((*(PE *) (parm + p->pe_ucode)) = prim2bit(pe_cpy(pe))) == NULLPE)
		return pepsylose (mod, p, pe, "pr_etype:out of memory");
	}
	break;

    case BITSTRING:
	if (pe != NULLPE) {
	    if ((*((PE *) (*parm + p->pe_ucode)) = prim2bit(pe_cpy(pe))) == NULLPE)
		return pepsylose (mod, p, pe, "pr_etype:out of memory");
	}
	break;

    case SOBJID:
	if ((*(OID *) (parm + p->pe_ucode) = oid_cpy(prim2oid(pe))) == NULLOID)
	    return pepsylose (mod, p, pe, "pr_etype:OID: out of memory");
	break;

    case OBJID:
	if ((*(OID *) (*parm + p->pe_ucode) = oid_cpy(prim2oid(pe))) == NULLOID)
	    return pepsylose (mod, p, pe, "en_etype:OID: out of memory");
	break;

    case FN_CALL:
	if ((FN_PTR(mod, p))(parm, pe) == NOTOK)
	    return pepsylose (mod, p, NULLPE, "pr_etype:FN_CALL:call failed");
	break;

    default:
	return pepsylose(mod, p, pe,
			 "pr_etype: %d not implemented", p->pe_type);
    }

    return (OK);
bad:
    return (NOTOK);
}


/*
 * determine if the given field is present in the data This is simple
 * if the field is a simple type with an obvious tag but in the case
 * of an object or a CHOICE type the tag is not obvious. If the
 * object is a CHOICE there are more than one possible tag that could
 * match and in this case we must try to match each one of them.
 */
static PE
setpresent(head, p, mod)
PE      head;
tpe    *p;
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
	return (setpresent(head, p = nmod->md_dtab[p->pe_tag] + 1, nmod));

    case OBJECT:
    case SOBJECT:
	/* Needs to be changed for optional and default */
	return (setpresent(head, p = mod->md_dtab[p->pe_tag] + 1, mod));

    case SCHOICE_START:
    case CHOICE_START:
	for (p++; p && p->pe_type != PE_END; p = NEXT_TPE(p)) {
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
static int
setdval(typ, dflt, parm, mod)
tpe    *typ, *dflt;
char  **parm;
modtyp *mod;
{
    char	*p;
    integer	i;
    int		no;	/* number of octets */
    char	*nparm;

again:
    switch (typ->pe_type) {
    case MEMALLOC:
        if (*parm)
            break;	/* already allocated */

	if ((*(parm) = (char *) calloc(1, (unsigned ) typ->pe_tag)) == NULL) {
	    (void) pepsylose (mod, typ, NULLPE,
			      "setdval:calloc failed on %d", typ->pe_tag);
	    return NOTOK;
	}
        /* fall through and do the same stuff as for ETAG */
	
    case ETAG:
	typ++;
	goto again;

    case INTEGER:
	*(integer *) (*parm + typ->pe_ucode) = IVAL(mod, dflt);
	break;

#ifdef	PEPSY_REALS
    case REALTYPE:
	*(double *) (*parm + typ->pe_ucode) = RVAL(mod, dflt);
	break;
#endif

    case BOOLEAN:
	*(char *) (*parm + typ->pe_ucode) = IVAL(mod, dflt);
	break;

    case T_NULL:
	/* Only one value */
	break;

    case SBITSTRING:
	*(PE *) (parm + typ->pe_ucode) = strb2bitstr(PVAL(mod, dflt), (int )IVAL(mod, dflt), 0, 0);
	break;

    case BITSTRING:
	*(PE *) (*parm + typ->pe_ucode) =
	   strb2bitstr(PVAL(mod, dflt), (int )IVAL(mod, dflt), 0, 0);
	break;

    case SOCTETSTRING:
	*(struct qbuf **) (parm + typ->pe_ucode) = str2qb(PVAL(mod, dflt),
	    (int )IVAL(mod, dflt), 1);
	break;

    case OCTETSTRING:
	*(struct qbuf **) (*parm + typ->pe_ucode) =
	   str2qb(PVAL(mod, dflt), (int )IVAL(mod, dflt), 1);
	break;

    case T_STRING:
	*(char **) (*parm + typ->pe_ucode) = strdup(PVAL(mod, dflt));
	break;

    case OCTET_PTR:
	if (typ[1].pe_type != OCTET_LEN)
	    return pepsylose (mod, typ, NULLPE, "setdval:missing OCTET_LEN");
	i = IVAL(mod, dflt);
	p = smalloc((int )i + 1);
	bcopy(PVAL(mod, dflt), p, (int )i);
	p[i] = '\0';
	*(char **) (*parm + typ->pe_ucode) = p;
	*(int *) (*parm + (typ + 1)->pe_ucode) = i;
	break;

    case BITSTR_PTR:
	if (typ[1].pe_type != BITSTR_LEN)
	    return pepsylose (mod, typ, NULLPE, "setdval:missing BITSTR_LEN");
	i = IVAL(mod, dflt);
	no = (i + 7)/8;	/* round up */
	p = smalloc(no + 1);
	bcopy(PVAL(mod, dflt), p, no);
	p[no] = '\0';
	*(char **) (*parm + typ->pe_ucode) = p;
	*(int *) (*parm + (typ + 1)->pe_ucode) = i;
	break;

    case OBJECT:
	if (setdval(mod->md_dtab[typ->pe_tag] + 1, dflt,
		    (char **) (*parm + typ->pe_ucode), mod) == NOTOK)
	    return NOTOK;
	break;

    case SOBJECT:
	if (typ->pe_ucode > 0) {
	    nparm = *parm + typ->pe_ucode;
	    if (setdval(mod->md_dtab[typ->pe_tag] + 1, dflt, &nparm, mod) == NOTOK)
		return NOTOK;
	} else if (setdval(mod->md_dtab[typ->pe_tag] + 1, dflt, parm, mod) == NOTOK)
		return NOTOK;
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
	*(char **) (*parm + typ->pe_ucode) = NULL;
	break;

    case FN_CALL:
	if ((FN_PTR(mod, typ))(parm, NULLPE) == NOTOK)
	    return pepsylose (mod, typ, NULLPE,
			      "setdval:FN_CALL:call failed");
	break;

    default:
	/*
	 * dmp_tpe("setdval: type not implemented", typ, mod); - need
	 * mod
	 */
	(void) pepsylose(mod, typ, NULLPE,
			 "setdval: %d not implemented", typ->pe_type);
	return NOTOK;
    }
    return OK;
}
/*
 * fix up the allocation of memory. We have allocated memory for an
 * optional object that is not present. ISODE routines get upset if
 * this is present because it then believes the object is present and
 * tries to process it ...
 */
static int
fix_mem(parm, p)
char  **parm;
tpe    *p;
{
    if (p->pe_type != SOBJECT || p[-1].pe_type != MEMALLOC
	|| p[1].pe_type != PE_END)
	SLOG (psap_log, LLOG_EXCEPTIONS, NULLCP, ("fix_mem:inconsistency"));
    if (*parm)
	free(*parm);
    *parm = NULL;
}
