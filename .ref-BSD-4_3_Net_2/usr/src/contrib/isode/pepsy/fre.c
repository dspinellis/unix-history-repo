/* fre.c */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepsy/RCS/fre.c,v 7.10 91/03/09 11:55:08 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/pepsy/RCS/fre.c,v 7.10 91/03/09 11:55:08 mrose Exp $
 *
 *
 * $Log:	fre.c,v $
 * Revision 7.10  91/03/09  11:55:08  mrose
 * update
 * 
 * Revision 7.9  91/02/22  09:48:58  mrose
 * Interim 6.8
 * 
 * Revision 7.8  91/01/07  12:41:20  mrose
 * update
 * 
 * Revision 7.7  90/12/23  17:24:30  mrose
 * patches
 * 
 * Revision 7.6  90/11/11  10:53:52  mrose
 * update
 * 
 * Revision 7.5  90/11/04  19:17:06  mrose
 * update
 * 
 * Revision 7.4  90/10/23  20:43:01  mrose
 * update
 * 
 * Revision 7.3  90/08/18  00:44:26  mrose
 * touch-up
 * 
 * Revision 7.2  90/07/27  08:48:48  mrose
 * update
 * 
 * Revision 7.1  90/07/09  14:52:38  mrose
 * sync
 * 
 * Revision 7.0  90/07/01  19:54:20  mrose
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


/* LINTLIBRARY */

/*
 * These routines are the driving routines for freeing of the data
 */
#include	<stdio.h>
#include	"pepsy-driver.h" /* for PEPSY_VERSION defn */
#include	"psap.h"
#include	"pepsy.h"


extern tpe *next_tpe();
extern int pepsylose ();
#define NEXT_TPE(p)	p = next_tpe(p)
#define CHKTAG(mod, p, pe)	ismatch(p, mod, pe->pe_class, pe->pe_id)

/*
 * free an objects data. Basic algorithm is to walk through it twice
 * first time freeing all the "children" of the data structure - then
 * the second time free the structure itself
 */
fre_obj(parm, p, mod, dofree)
modtyp *mod;
tpe    *p;
char   *parm;
int	dofree;
{
    char   *malptr = NULL;	/* Have we seen a malloc */
    int	    ndofree = dofree;	/* Does the function below deallocate space */

    if (parm == 0)
	return (OK);

    if (p->pe_type != PE_START) {
	(void) pepsylose (mod, p, NULLPE, "fre_obj: missing PE_START\n");
	return (NOTOK);
    }

    for (p++; p->pe_type != PE_END; NEXT_TPE(p)) {

again:
	/*
	 * we have to have all these cases here because it is different to the
	 * situation when the entry is not the main entry of the typereference.
	 */
	switch (p->pe_type) {
	case MEMALLOC:
	    if (dofree) {
		malptr = parm;
		ndofree = 0;	/* we are deallocating space on this level */
	    }
	    break;

	default:
	    if (fre_type(parm, p, mod, ndofree) != OK)
		return (NOTOK);
	    break;
	}
    }

    if (malptr && dofree) { /* If we saw a malloc free item */
	free(malptr);
	malptr = NULLCP;
    }

    return (OK);
}

/*
 * Handle freeing of single type field. All the more general routines
 * fall back to this so we can put the code to free something just
 * here once and it will handle all the cases else where
 */
fre_type(parm, p, mod, dofree)
char   *parm;
tpe    *p;
modtyp *mod;			/* Module it is from */
int	dofree;
{

    if (parm == 0)
	return OK;

again:
    switch (p->pe_type) {
    case MEMALLOC:
	break;

    case PE_END:
    case PE_START:
    case UCODE:
	break;

    case BOPTIONAL:
    case FREE_ONLY:  /* this next entry is for us */
    case DFLT_F:
	p++;
	goto again;

    case ETAG:
	switch (p->pe_ucode) {

	default:
	    p++;
	    if (fre_type(parm, p, mod, dofree) != OK)
		return (NOTOK);
	}
	break;

    case SEQ_START:
    case SET_START:
	if (fre_seq(*(char **) (parm + p->pe_ucode), p, mod, 1) != OK)
	    return (NOTOK);
	break;

    case SEQOF_START:
    case SETOF_START:
	if (fre_seqof(*(char **) (parm + p->pe_ucode), p, mod, 1) != OK)
	    return (NOTOK);
	break;

    case SSEQ_START:
    case SSET_START:
	if (fre_seq((char *) parm + p->pe_ucode, p, mod, dofree) != OK)
	    return (NOTOK);
	break;

    case SSEQOF_START:
    case SSETOF_START:
	if (fre_seqof((char *) parm + p->pe_ucode, p, mod, dofree) != OK)
	    return (NOTOK);
	break;

    case IMP_OBJ:
	p++;
	if (p->pe_type == EXTOBJ) {
	    if (fre_obj(*(char **) (parm + p->pe_ucode),
		    (EXT2MOD(mod, (p + 1)))->md_dtab[p->pe_tag],
		    EXT2MOD(mod, (p + 1)), 1) != OK)
		return (NOTOK);
	} else if (p->pe_type == SEXTOBJ) {
	    if (fre_obj((parm + p->pe_ucode),
		    (EXT2MOD(mod, (p + 1)))->md_dtab[p->pe_tag],
		    EXT2MOD(mod, (p + 1)), dofree) != OK)
		return (NOTOK);
	} else if (p->pe_type == SOBJECT) {
	    if (fre_obj((char *) parm + p->pe_ucode, mod->md_dtab[p->pe_tag], mod, dofree) != OK)
		return (NOTOK);
	} else
	    if (fre_obj(*(char **) (parm + p->pe_ucode),
		    mod->md_dtab[p->pe_tag], mod, 1) != OK)
		return (NOTOK);
	break;

    case SOBJECT:
	if (fre_obj((char *) parm + p->pe_ucode, mod->md_dtab[p->pe_tag], mod, dofree) != OK)
	    return (NOTOK);
	break;

    case OBJECT:
	if (fre_obj(*(char **) (parm + p->pe_ucode), mod->md_dtab[p->pe_tag],
		mod, 1) != OK)
	    return (NOTOK);
	break;

    case SCHOICE_START:
	if (fre_choice((char *) parm + p->pe_ucode, p, mod, dofree) != OK)
	    return (NOTOK);
	break;

    case CHOICE_START:
	if (fre_choice(*(char **) (parm + p->pe_ucode), p, mod, 1) != OK)
	    return (NOTOK);
	break;

    case SEXTOBJ:
	if (p[1].pe_type != EXTMOD) {
	    (void) pepsylose (mod, p, NULLPE, "fre_type:missing EXTMOD");
	    return (NOTOK);
	}
	if (fre_obj(parm + p->pe_ucode, (EXT2MOD(mod, (p + 1)))->md_dtab[p->pe_tag],
		EXT2MOD(mod, (p + 1)), dofree) != OK)
	    return (NOTOK);
	break;

    case EXTOBJ:
	if (p[1].pe_type != EXTMOD) {
	    (void) pepsylose (mod, p, NULLPE, "fre_type:missing EXTMOD");
	    return (NOTOK);
	}
	if (fre_obj(*(char **) (parm + p->pe_ucode),
		(EXT2MOD(mod, (p + 1)))->md_dtab[p->pe_tag],
		EXT2MOD(mod, (p + 1)), 1) != OK)
	    return (NOTOK);
	break;

    case INTEGER:
    case BOOLEAN:
    case T_NULL:
    case REALTYPE:
	break;

    case SANY:
	/*
	 * These tests of the pointer don't appear necessary from the
	 * definition of encoding and decoding but ISODE generates
	 * freeing code that does these checks and ISODE's ps layer
	 * definitely requires it
	 */
	if (parm != NULL) {
	    pe_free((PE) parm);
	    parm = NULL;
	}
	break;

    case ANY:
	if (*(char **) (parm + p->pe_ucode) != NULL) {
	    pe_free(*(PE *) (parm + p->pe_ucode));
	    *((PE *) (parm + p->pe_ucode)) = NULLPE;
	}
	break;

    case SOCTETSTRING:
	if (parm != NULL) {
	    qb_free((struct qbuf *) parm);
	    parm = NULL;
	}
	break;

    case T_STRING:
    case OCTET_PTR:
    case BITSTR_PTR:
	if (*(char **) (parm + p->pe_ucode) != NULL) {
	    free(*(char **) (parm + p->pe_ucode));
	    *(char **) (parm + p->pe_ucode) = NULLCP;
	}
	break;

    case OCTETSTRING:
	if (*(char **) (parm + p->pe_ucode) != NULL) {
	    qb_free(*(struct qbuf **) (parm + p->pe_ucode));
	    *(struct qbuf **) (parm + p->pe_ucode) = (struct qbuf *)0;
	}
	break;

    case SBITSTRING:
	if (parm != NULL) {
	    pe_free((PE) parm);
	    parm = NULL;
	}
	break;

    case BITSTRING:
	if (*(char **) (parm + p->pe_ucode) != NULL) {
	    pe_free(*(PE *) (parm + p->pe_ucode));
	    *(PE *) (parm + p->pe_ucode) = NULLPE;
	}
	break;

    case SOBJID:
	if (parm != NULL) {
	    oid_free((OID) parm);
	    parm = NULL;
	}
	break;

    case OBJID:
	if (*(char **) (parm + p->pe_ucode) != NULL) {
	    oid_free(*(OID *) (parm + p->pe_ucode));
	    *(OID *) (parm + p->pe_ucode) = NULLOID;
	}
	break;

    case FN_CALL:
	 break;	/* can't do anything with this */

    case FFN_CALL:
	if ((FN_PTR(mod, p))(parm) == NOTOK)
	    return pepsylose (mod, p, NULLPE,
			      "fre_type:FN_CALL:call failed");
	break;

    default:
	(void) pepsylose (mod, p, NULLPE, "fre_type: %d not implemented\n",
	    p->pe_type);
	return (NOTOK);
    }

    return (OK);
}

/*
 * free elements of a sequential type. e.g. sequence or set
 */
static fre_seq(parm, p, mod, dofree)
char   *parm;
tpe    *p;
modtyp *mod;			/* Module it is from */
int	dofree;
{
    int    *popt = NULL;	/* Pointer to optional field */
    int     optcnt = 0;		/* Number of optionals bits so far */
    char   *malptr = NULL;	/* Have we seen a malloc */
    int	    ndofree = dofree;	/* Does the function below deallocate space */


    if (parm == 0)
	return OK;

    if (p->pe_type != SEQ_START && p->pe_type != SET_START
     && p->pe_type != SSEQ_START && p->pe_type != SSET_START) {
	(void) pepsylose (mod, p, NULLPE, "fre_seq: bad starting item %d\n",
	    p->pe_type);
	return (NOTOK);
    }
    p++;

    if (p->pe_type == DFLT_B)
	p++;

    while (p->pe_type != PE_END) {

	switch (p->pe_type) {
	case MEMALLOC:
	    if (dofree) {
		malptr = parm;
		ndofree = 0;	/* we are deallocating space on this level */
	    }
	    break;

	case OPTL:
	    popt = (int *) (parm + p->pe_ucode);
	    break;

	case ETAG:
	    p++;
	    continue;

	case UCODE:
	    break;

	case SET_START:
	case SEQ_START:
	    if (fre_seq(*(char **) (parm + p->pe_ucode), p, mod, 1) != OK)
		return (NOTOK);
	    break;

	case SETOF_START:
	case SEQOF_START:
	    if (fre_seqof(*(char **) (parm + p->pe_ucode), p, mod, 1) != OK)
		return (NOTOK);
	    break;

	case SSEQ_START:
	case SSET_START:
	    if (fre_seq((char *) parm + p->pe_ucode, p, mod, ndofree) != OK)
		return (NOTOK);
	    break;

	case SSEQOF_START:
	case SSETOF_START:
	    if (fre_seqof((char *) parm + p->pe_ucode, p, mod, ndofree) != OK)
		return (NOTOK);
	    break;

	case IMP_OBJ:
	    p++;
	    continue;

	case SOBJECT:
	    if (fre_obj((char *) parm + p->pe_ucode,
		mod->md_dtab[p->pe_tag], mod, ndofree) != OK)
		return (NOTOK);
	    break;

	case OBJECT:
	    if (fre_obj(*(char **) (parm + p->pe_ucode),
		    mod->md_dtab[p->pe_tag], mod, 1) != OK)
		return (NOTOK);
	    break;

	case SCHOICE_START:
	    if (fre_choice((char *) parm + p->pe_ucode, p, mod, ndofree) != OK)
		return (NOTOK);
	    break;

	case CHOICE_START:
	    if (fre_choice(*(char **) (parm + p->pe_ucode), p, mod, 1) != OK)
		return (NOTOK);
	    break;

	case SEXTOBJ:
	    if (p[1].pe_type != EXTMOD) {
		(void) pepsylose (mod, p, NULLPE, "fre_seq:missing EXTMOD");
		return (NOTOK);
	    }
	    if (fre_obj(parm + p->pe_ucode, (EXT2MOD(mod, (p + 1)))->md_dtab[p->pe_tag],
		    EXT2MOD(mod, (p + 1)), ndofree) != OK)
		return (NOTOK);
	    break;

	case EXTOBJ:
	    if (p[1].pe_type != EXTMOD) {
		(void) pepsylose (mod, p, NULLPE, "fre_seq:missing EXTMOD");
		return (NOTOK);
	    }
	    if (fre_obj(*(char **) (parm + p->pe_ucode),
		    (EXT2MOD(mod, (p + 1)))->md_dtab[p->pe_tag],
		    EXT2MOD(mod, (p + 1)), 1) != OK)
		return (NOTOK);
	    break;

	default:
	    if (fre_type(parm, p, mod, ndofree) != OK)
		return (NOTOK);
	    break;
	}

next:
	NEXT_TPE(p);
    }
    if (malptr && dofree) {	/* If we saw a malloc free item */
	free(malptr);
	malptr = NULLCP;
    }
    
    return (OK);

}

/*
 * free all the fields in a SET OF/SEQUENCE OF type structure. We
 * must follow the linked list until the end
 */
static fre_seqof(parm, p, mod, dofree)
char   *parm;
tpe    *p;
modtyp *mod;			/* Module it is from */
int	dofree;
{
    tpe    *start;		/* first entry in list */
    char   *oparm;

    if (parm == 0)
	return OK;

    if (p->pe_type != SEQOF_START && p->pe_type != SETOF_START
     && p->pe_type != SSEQOF_START && p->pe_type != SSETOF_START) {
	(void) pepsylose (mod, p, NULLPE, "fre_seqof: illegal field");
	return (NOTOK);
    }
    for (start = p; (char *) parm != NULL; p = start) {
	p++;

	if (p->pe_type == DFLT_B)
	    p++;

	while (p->pe_type != PE_END) {

	    switch (p->pe_type) {
	    case MEMALLOC:
		break;

	    case ETAG:
		p++;
		continue;

	    case UCODE:
		break;

	    case SEQ_START:
	    case SET_START:
		if (fre_seq(*(char **) (parm + p->pe_ucode), p, mod, 1) != OK)
		    return (NOTOK);
		break;

	    case SEQOF_START:
	    case SETOF_START:
		if (fre_seqof(*(char **) (parm + p->pe_ucode), p, mod, 1) != OK)
		    return (NOTOK);
		break;

	    case SSEQ_START:
	    case SSET_START:
		if (fre_seq((char *) parm + p->pe_ucode, p, mod, dofree) != OK)
		    return (NOTOK);
		break;

	    case SSEQOF_START:
	    case SSETOF_START:
		if (fre_seqof((char *) parm + p->pe_ucode, p, mod, dofree) != OK)
		    return (NOTOK);
		break;

	    case IMP_OBJ:
		p++;
		continue;

	    case SOBJECT:
		if (fre_obj(parm + p->pe_ucode, mod->md_dtab[p->pe_tag], mod, 0) != OK)
		    return (NOTOK);
		break;

	    case OBJECT:
		if (fre_obj(*(char **) (parm + p->pe_ucode),
			mod->md_dtab[p->pe_tag], mod, 1) != OK)
		    return (NOTOK);
		break;

	    case SCHOICE_START:
		if (fre_choice((char *) parm + p->pe_ucode, p, mod, 0) != OK)
		    return (NOTOK);
		break;

	    case CHOICE_START:
		if (fre_choice(*(char **)(parm + p->pe_ucode), p, mod, 1) != OK)
		    return (NOTOK);
		break;

	    case SEXTOBJ:
		if (p[1].pe_type != EXTMOD) {
		    (void) pepsylose (mod, p, NULLPE,
			"fre_seqof: missing EXTMOD");
		    return (NOTOK);
		}
		if (fre_obj(parm + p->pe_ucode, (EXT2MOD(mod, (p + 1)))->md_dtab[p->pe_tag],
			EXT2MOD(mod, (p + 1)), 0) != OK)
		    return (NOTOK);
		break;

	    case EXTOBJ:
		if (p[1].pe_type != EXTMOD) {
		    (void) pepsylose (mod, p, NULLPE,
			"fre_seqof: missing EXTMOD");
		    return (NOTOK);
		}
		if (fre_obj(*(char **) (parm + p->pe_ucode),
			(EXT2MOD(mod, (p + 1)))->md_dtab[p->pe_tag],
			EXT2MOD(mod, (p + 1)), 1) != OK)
		    return (NOTOK);
		break;

	    default:
		if (fre_type(parm, p, mod, 1) != OK)
		    return (NOTOK);
		break;
	    }

	    NEXT_TPE(p);
	}
	oparm = parm;
	parm = *(char **) (parm + p->pe_ucode);	/* Any more ? */
	if (dofree) {
	    free(oparm);
	    oparm = NULLCP;
	}
    }

    return (OK);

}

/*
 * free the item of the choice. Use the SCTRL field to determine
 * which item is present and then call the appropriate routine to
 * free it
 */
static fre_choice(parm, p, mod, dofree)
char   *parm;
tpe    *p;
modtyp *mod;			/* Module it is from */
int	dofree;
{
    int     cnt;
    char   *malptr = NULL;	/* Have we seen a malloc */
    int	    ndofree = dofree;	/* Does the function below deallocate space */

    if (parm == 0)
	return OK;

    if (p->pe_type != CHOICE_START && p->pe_type != SCHOICE_START) {
	(void) pepsylose (mod, p, NULLPE,
	    "fre_choice:CHOICE_START missing found %d\n", p->pe_type);
    }
    p++;

    if (p->pe_type == DFLT_B)
	p++;

    if (p->pe_type == MEMALLOC) {
	if (dofree) {
	    malptr = parm;
	    ndofree = 0;	/* we are deallocating space on this level */
	}
	p++;
    }
    if (p->pe_type != SCTRL) {
	(void) pepsylose (mod, p, NULLPE,
	    "fre_choice: missing SCTRL information\n");
	return (NOTOK);
    }
    cnt = *(int *) (parm + p->pe_ucode);
    if (cnt != 0)
	cnt--;
    if (cnt < 0) {
	(void) pepsylose (mod, p, NULLPE,"fre_choice:offset negative %d", cnt);
	return (NOTOK);
    }
    for (p++; p->pe_type != PE_END; NEXT_TPE(p)) {
	if (ISDTYPE(p)) {
	    if (cnt == 0) {
		if (fre_type(parm, p, mod, ndofree) != OK)
		    return (NOTOK);
		if (malptr && dofree) {	/* If we saw a malloc free item */
		    free(malptr);
		    malptr = NULLCP;
		}
		return (OK);
	    }
	    cnt--;
	}
    }

    (void) pepsylose (mod, p, NULLPE, "fre_choice: no choice taken");
    return (NOTOK);
}
/*
 * look out for FN_CALL - if this entry is really a FN_CALL return non zero
 * else return 0
 * Basically we have to stop FN_CALL being tested by hasdata which will call
 * the decoding function which is illegal and gives rubbish.
 */
callsfn(p, mod)
tpe	*p;
modtyp	*mod;
{

    while (p->pe_type != PE_END) {
	switch (p->pe_type) {
	case ETAG:
	    p++;
	    continue;

	case FN_CALL:
	    return (1);

	default:
	case INTEGER:
	case REALTYPE:
	case BOOLEAN:
	case T_NULL:
	case IMP_OBJ:
	case OBJECT:
	    return (0);
	}
    }

    (void) pepsylose (mod, p, NULLPE,"callsfn:Corrupted tables:PE_END found\n");
    ferr(1, "callsfn:Mangled tables\n");
    /*NOTREACHED*/

}
