/* pe2qb_f.c - presentation element to qbuf, the qbuf must be one piece. */

/*
 *                                NOTICE
 *
 * Acquisition, use, and distribution of this module and related
 * materials are subject to the restrictions of a license agreement.
 * Consult the Preface in the User's Manual for the full terms of
 * this agreement.
 *
 */


/* LINTLIBRARY */

#include "psap.h"

/*    DATA */

#define moveit(c, l)        if(Qcp + l > Ecp) { \
				printf("pe2qb_f: Qcp %o Ecp %o len %d\n", \
					Qcp, Ecp, l); \
				return(NOTOK); \
			    } \
			    if(l == 1) { \
				*Qcp++ = *c; \
				Len++; \
			    }  \
			    else { \
				    bcopy(c, Qcp, l); \
				    Qcp += l; \
				    Len  += l; \
			    }

static PElement pe_eoc = { PE_CLASS_UNIV, PE_FORM_PRIM, PE_UNIV_EOC, 0 };

char *Qcp, *Ecp;

int Len;

/*  */

int	pe2qb_f (pe)
register PE	pe;
{
    register PE	    p;
    register int elm_len;
    byte    elmbuffer[1 + sizeof(PElementLen)];
    register byte  *bp,  *ep;
    PElementForm    form;
    register PElementID id;
    register PElementLen len;

    if ((form = pe -> pe_form) == PE_FORM_ICONS) {
	elm_len = pe->pe_len;
	moveit(pe->pe_prim, elm_len);
	return(Len);
    }

    /* Put the id into the qbuf */

    *Qcp = ((pe -> pe_class << PE_CLASS_SHIFT) & PE_CLASS_MASK)
		| ((form << PE_FORM_SHIFT) & PE_FORM_MASK);

    if ((id = pe -> pe_id) < PE_ID_XTND) {
	*Qcp++ |= id;
	Len++;
    }
    else {
        byte    idbuffer[1 + sizeof (PElementID)];
	register PElementID jd;

	ep = (bp = idbuffer);
    	*bp = *Qcp | PE_ID_XTND;
	for (jd = id; jd != 0; jd >>= PE_ID_SHIFT)
	    ep++;

	for (bp = ep; id != 0; id >>= PE_ID_SHIFT)
	    *bp-- = id & PE_ID_MASK;
	for (bp = idbuffer + 1; bp < ep; bp++)
	    *bp |= PE_ID_MORE;

	bp = ++ep;
        elm_len = bp - idbuffer;
        moveit(idbuffer, elm_len);
    }


    /* Put the length into the qbuf */

    if ((len = pe -> pe_len) == PE_LEN_INDF) {
	*Qcp++ = PE_LEN_XTND;
	Len++;
    }
    else
	if (len <= PE_LEN_SMAX) {
	    *Qcp++ = len & 0xff;
	    Len++;
	}
	else {
	    ep = elmbuffer + sizeof elmbuffer - 1;
	    for (bp = ep; len != 0 && elmbuffer < bp; len >>= 8)
		*bp-- = len & 0xff;
	    *bp = PE_LEN_XTND | ((ep - bp) & 0xff);
	    elm_len = ep - bp + 1;
    	    moveit(bp, elm_len);
	}

    /* Now put the actual value into the qbuf */
    
    switch (pe -> pe_form) {
	case PE_FORM_PRIM: 
	    elm_len = pe->pe_len;
	    moveit(pe->pe_prim, elm_len);
	    break;

	case PE_FORM_CONS: 
	    if (pe -> pe_len) {
		for (p = pe -> pe_cons; p; p = p -> pe_next)
		    if (pe2qb_f (p) == NOTOK)
			return NOTOK;

		if (pe -> pe_len == PE_LEN_INDF
			&& pe2qb_f (&pe_eoc) == NOTOK)
		    return NOTOK;
	    }
	    break;

	default:
	    abort();
    }

    return(Len);
}
