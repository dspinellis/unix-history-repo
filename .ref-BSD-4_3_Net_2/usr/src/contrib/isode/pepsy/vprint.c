/* vprint.c - pepy printer support */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepsy/RCS/vprint.c,v 7.4 91/02/22 09:50:15 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/pepsy/RCS/vprint.c,v 7.4 91/02/22 09:50:15 mrose Interim $
 *
 *
 * $Log:	vprint.c,v $
 * Revision 7.4  91/02/22  09:50:15  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/12/23  18:42:12  mrose
 * update
 * 
 * Revision 7.2  90/11/11  09:59:04  mrose
 * touch-up
 * 
 * Revision 7.1  90/07/27  08:49:12  mrose
 * update
 * 
 * Revision 7.0  90/07/01  19:54:34  mrose
 * *** empty log message ***
 * 
 * Revision 7.0  89/11/23  22:12:08  mrose
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


/* LINTLIBRARY */

#include <ctype.h>
#include <stdio.h>
#include <varargs.h>
#include "UNIV-types.h"
#include "psap.h"
#include "pepsy.h"
#include "logger.h"


int	fprintf ();

/*    DATA */

#define	VPRINT(s)	vprint1 (), vwrite ((s)), vprint2 ()

static int vlevel = 0;

static int didname = 0;
static int didvpop = 0;
static int didvpush = 0;
static int docomma = 0;

static char  *py_classlist[] = {
    "UNIVERSAL", "APPLICATION", "", "PRIVATE"
};

static char *vbp = NULL;
static char *vsp;

IFP   vfnx = fprintf;
FILE *vfp = stdout;
static PS    vps = NULLPS;

char   *oct2str (), *newbuf ();

/*    VPUSH/VPOP */

vpush () {
    if (didvpush)
	vwrite ("\n"), didvpush = 0;
    else
	if (!didname && docomma)
	    vwrite (",\n");

    if (didname)
	vwrite (" ");
    else
	if (vfp && vlevel > 0)
	    (*vfnx) (vfp, "%*s", vlevel * 3, "");
    vwrite ("{");
    vlevel++;

    didname = didvpop = docomma = 0, didvpush = 1;
}


vpop () {
    if (didname || docomma)
	vwrite ("\n");

    vlevel--;
    if (!didvpush && vfp && vlevel > 0)
	(*vfnx) (vfp, "%*s", vlevel * 3, "");
    vwrite ("}");
    if (vlevel == 0)
	vwrite ("\n");

    didname = didvpush = 0, didvpop = docomma = vlevel ? 1 : 0;
}

/*    VNAME/VTAG */

vname (name)
char   *name;
{
    if (didvpush)
	vwrite ("\n"), didvpush = 0;
    else
	if (docomma)
	    vwrite (",\n");

    if (vfp && vlevel > 0)
	(*vfnx) (vfp, "%*s", vlevel * 3, "");
    vwrite (name);

    didname = 1;
}


vtag (class, id)
int	class,
	id;
{
    register char *bp;
    static char buffer[BUFSIZ];

    if (didname)
	return;

    bp = buffer;
    *bp++ = '[';
    switch (class) {
	case PE_CLASS_UNIV: 
	case PE_CLASS_APPL: 
	case PE_CLASS_PRIV: 
	    (void) sprintf (bp, "%s ", py_classlist[class]);
	    bp += strlen (bp);
	    break;

	case PE_CLASS_CONT: 
	default:
	    break;
    }
    (void) sprintf (bp, "%d]", id);

    vname (buffer);
}

/*    VPRINT */

#ifndef	lint
vprint (va_alist)
va_dcl
{
    char    buffer[BUFSIZ];
    va_list ap;
    
    vprint1 ();

    va_start (ap);

    _asprintf (buffer, NULLCP, ap);
    
    va_end (ap);

    vwrite (buffer);

    vprint2 ();
}
#else
/* VARARGS */

vprint (fmt)
char   *fmt;
{
    vprint (fmt);
}
#endif


static	vprint1 ()
{
    if (didvpush) {
	vwrite ("\n"), didvpush = 0;
	goto indent;
    }
    else
	if (didname)
	    vwrite (" ");
	else {
	    if (docomma)
		vwrite (",\n");
indent: ;
	    if (vfp && vlevel > 0)
		(*vfnx) (vfp, "%*s", vlevel * 3, "");
	}
}


static	vprint2 ()
{
    if (vlevel == 0)
	vwrite ("\n");

    didname = didvpop = 0, docomma = vlevel ? 1 : 0;
}

/*  */

static	vwrite (s)
char   *s;
{
    if (vfp)
	(*vfnx) (vfp, "%s", s);
    else {
	register char   c,
		       *cp;

	if (vps)
		for (cp = s; *cp; cp++) {
		    if (*cp == '\n' )
			    (void) ps_write (vps, (PElementData) " ", 1);
		    else
			    (void) ps_write (vps, (PElementData) cp, 1);
		}
	else
		for (cp = s; *cp; )
		    *vbp++ = (c = *cp++) != '\n' ? c : ' ';
    }
}

/*    VSTRING */

vstring (pe)
register PE	pe;
{
    register PE	    p;

    switch (pe -> pe_form) {
	case PE_FORM_PRIM: 
	case PE_FORM_ICONS:
	    VPRINT (oct2str ((char *) pe -> pe_prim, (int) pe -> pe_len));
	    break;

	case PE_FORM_CONS: 
	    vpush ();
	    for (p = pe -> pe_cons; p; p = p -> pe_next)
		vstring (p);
	    vpop ();
	    break;
    }
}

/*  */

static char   *oct2str (s, len)
register char  *s;
register int	len;
{
    int     ia5ok;
    register int    k;
    register char  *bp,
                   *cp,
                   *dp,
		   *zp;

    ia5ok = 1, k = 0;
    for (dp = (cp = s) + len; cp < dp; cp++) {
	switch (*cp) {
	    case ' ': 
		continue;

	    case '"': 
		break;

	    case '\b':
	    case '\f':
	    case '\n':
	    case '\r':
	    case '\t':
	    case '\\':
		ia5ok = -1, k++;
		continue;

	    case '-': 
		if (cp > s && *(cp + 1) == '-')
		    break;
		continue;

	    default: 
		if (iscntrl (*cp) || isspace (*cp) || (*cp & 0x80))
		    break;
		continue;
	}
	ia5ok = 0;
	break;
    }

    switch (ia5ok) {
	case 1: 
	    zp = newbuf (len + 2);
	    (void) sprintf (zp, "\"%*.*s\"", len, len, s);
	    break;

	case -1: 
	    bp = zp = newbuf (len + k + 2);
	    *bp++ = '"';
	    for (cp = s; cp < dp; cp++)
		if (*cp >= ' ' && *cp != '\\')
		    *bp++ = *cp;
		else {
		    *bp++ = '\\';
		    switch (*cp) {
			case '\b':
			    *bp++ = 'b';
			    break;
			case '\f':
			    *bp++ = 'f';
			    break;
			case '\n':
			    *bp++ = 'n';
			    break;
			case '\r':
			    *bp++ = 'r';
			    break;
			case '\t':
			    *bp++ = 't';
			    break;

			case '\\':
			    *bp++ =  '\\';
			    break;
		    }
		}
	    (void) sprintf (bp, "\"");
	    break;

	case 0: 
	default: 
	    bp = zp = newbuf (len * 2 + 3);
	    *bp++ = '\'';
	    for (cp = s; cp < dp; cp++) {
		(void) sprintf (bp, "%02x", *cp & 0xff);
		bp += strlen (bp);
	    }
	    (void) sprintf (bp, "'H");
	    break;
    }

    return zp;
}

/*  */

char   *bit2str (pe, s)
PE	pe;
char   *s;
{
    int     ia5ok;
    register int    hit,
		    i,
                    j,
                    k;
    register char  *bp,
                   *cp,
		   *zp;

    j = pe -> pe_nbits;
    if ((cp = s) && *++cp) {
	ia5ok = 1, hit = 0;
	for (i = 0; i < j;)
	    if (bit_test (pe, i++) == 1) {
		do {
		    if (!(k = *cp++ & 0xff))
			break;
		    if (k == i) {
			hit += hit ? 2 : 1;
			for (; *cp > ' '; cp++)
			    hit++;
		    }
		    else
			for (; *cp > ' '; cp++)
			    continue;
		} while (k != 0 && k < i);
		if (k == 0 || k > i) {
		    ia5ok = 0;
		    break;
		}
	    }
    }
    else
	ia5ok = 0;

    if (ia5ok) {
	bp = zp = newbuf (hit + 3);
	*bp++ = '{';

	cp = s, cp++;
	for (i = hit = 0; i < j;)
	    if (bit_test (pe, i++) == 1) {
		do {
		    if (!(k = *cp++ & 0xff))
			break;
		    if (k == i) {
			if (hit)
			    *bp++ = ',';
			*bp++ = ' ';
			for (; *cp > ' '; cp++)
			    *bp++ = *cp;
		    }
		    else
			for (; *cp > ' '; cp++)
			    continue;
		} while (k != 0 && k < i);
		if (k == 0 || k > i)
		    break;
		hit++;
	    }

	(void) sprintf (bp, "%s}", hit ? " " : "");
    }
    else {
	bp = zp = newbuf (j + 3);
	*bp++ = '\'';
	for (i = 0; i < j; i++)
	    *bp++ = bit_test (pe, i) ? '1' : '0';
	(void) sprintf (bp, "'B");
    }

    return zp;
}

/*  */

#ifdef vunknown
#undef vunknown
#endif

vunknown (pe)
register PE	pe;
{
    int     i;
#ifdef	notyet	    /* could comment this in, but then all programs need -lm */
    double  j;
#endif
    OID	    oid;
    register PE	    p;

    switch (pe -> pe_form) {
	case PE_FORM_PRIM: 
	    switch (PE_ID (pe -> pe_class, pe -> pe_id)) {
		case PE_ID (PE_CLASS_UNIV, PE_PRIM_BOOL): 
		    if ((i = prim2flag (pe)) == NOTOK)
			goto bad_pe;
		    VPRINT (i ? "TRUE" : "FALSE");
		    break;

		case PE_ID (PE_CLASS_UNIV, PE_PRIM_INT): 
		case PE_ID (PE_CLASS_UNIV, PE_PRIM_ENUM): 
		    if ((i = prim2num (pe)) == NOTOK
			    && pe -> pe_errno != PE_ERR_NONE)
			goto bad_pe;
		    vprint ("%d", i);
		    break;

#ifdef	notyet
		case PE_ID (PE_CLASS_UNIV, PE_PRIM_REAL): 
		    if ((j = prim2real (pe)) == NOTOK
			    && pe -> pe_errno != PE_ERR_NONE)
			goto bad_pe;
		    vprint ("%g", j);
		    break;
#endif

		case PE_ID (PE_CLASS_UNIV, PE_PRIM_BITS): 
		    if ((p = prim2bit (pe)) == NULLPE)
			goto bad_pe;
		    if (p -> pe_nbits < LOTSOFBITS) {
			VPRINT (bit2str (p, "\020"));
			break;
		    }
		    /* else fall... */

		default: 
	    bad_pe: ;
		    vtag ((int) pe -> pe_class, (int) pe -> pe_id);
		/* fall */

		case PE_ID (PE_CLASS_UNIV, PE_PRIM_OCTS): 
		case PE_ID (PE_CLASS_UNIV, PE_DEFN_IA5S): 
		case PE_ID (PE_CLASS_UNIV, PE_DEFN_NUMS): 
		case PE_ID (PE_CLASS_UNIV, PE_DEFN_PRTS): 
		case PE_ID (PE_CLASS_UNIV, PE_DEFN_T61S): 
		case PE_ID (PE_CLASS_UNIV, PE_DEFN_VTXS): 
		case PE_ID (PE_CLASS_UNIV, PE_DEFN_GENT): 
		case PE_ID (PE_CLASS_UNIV, PE_DEFN_UTCT): 
		case PE_ID (PE_CLASS_UNIV, PE_DEFN_GFXS): 
		case PE_ID (PE_CLASS_UNIV, PE_DEFN_VISS): 
		case PE_ID (PE_CLASS_UNIV, PE_DEFN_GENS): 
		case PE_ID (PE_CLASS_UNIV, PE_DEFN_CHRS): 
		case PE_ID (PE_CLASS_UNIV, PE_PRIM_ODE): 
		    vstring (pe);
		    break;

		case PE_ID (PE_CLASS_UNIV, PE_PRIM_NULL):
		    VPRINT ("NULL");
		    break;

		case PE_ID (PE_CLASS_UNIV, PE_PRIM_OID): 
		    if ((oid = prim2oid (pe)) == NULLOID)
			goto bad_pe;
		    VPRINT (oid2ode (oid));
		    break;
	    }
	    break;

	case PE_FORM_CONS: 
	    switch (PE_ID (pe -> pe_class, pe -> pe_id)) {
		case PE_ID (PE_CLASS_UNIV, PE_CONS_SEQ):
		case PE_ID (PE_CLASS_UNIV, PE_CONS_SET):
		    break;

		case PE_ID (PE_CLASS_UNIV, PE_CONS_EXTN):
		    (void) print_UNIV_EXTERNAL (pe, 1, NULLIP, NULLVP,
						NULLCP);
		    return;

		default:
		    vtag ((int) pe -> pe_class, (int) pe -> pe_id);
		    break;
	    }
	    vpush ();
	    for (p = pe -> pe_cons; p; p = p -> pe_next)
		vunknown (p);
	    vpop ();
	    break;

	case PE_FORM_ICONS:
	    vtag ((int) pe -> pe_class, (int) pe -> pe_id);
	    vstring (pe);
	    break;
    }
}

/*    VPUSHFP/VPOPFP */

vpushfp (fp, pe, s, rw)
FILE   *fp;
PE	pe;
char   *s;
int	rw;
{
    vpushpp ((caddr_t) fp, fprintf, pe, s, rw);
}

vsetfp (fp, s)
FILE	* fp;
char	* s;
{
    vfp = fp;
    vfnx = fprintf;

    if(s != NULLCP)
	(*vfnx) (vfp, "%s\n", s);

    vlevel = didname = didvpush = didvpop = docomma = 0;
}

vpopfp ()
{
    (*vfnx) (vfp, "-------\n");
    (void) fflush (vfp);

    vpopp ();
}

/*    VPUSHSTR/VPOPSTR */

vpushstr (cp)
char   *cp;
{
    vfp = NULL;
    vbp = vsp = cp;

    vlevel = didname = didvpush = didvpop = docomma = 0;
}


vpopstr ()
{
    while (--vbp >= vsp)
	if (*vbp != ' ')
	    break;
    *++vbp = NULL;

    vfp = stdout;
}

/*    VPUSHPP */

vpushpp (pv, pfnx, pe, text, rw)
caddr_t pv;
IFP	pfnx;
register PE	pe;
char   *text;
int	rw;
{
    vfp = (FILE *) pv, vfnx = pfnx;

    (*vfnx) (vfp, "%s %s", rw ? "read" : "wrote", text ? text : "pdu");
    if (pe -> pe_context != PE_DFLT_CTX)
	(*vfnx) (vfp, ", context %d", pe -> pe_context);
    (*vfnx) (vfp, "\n");

    vlevel = didname = didvpush = didvpop = docomma = 0;
}

vpopp ()
{
    vfp = stdout, vfnx = fprintf;
}


vpushquipu (ps)
PS	ps;
{
    vps = ps;
    vfp = NULL;

    vlevel = didname = didvpush = didvpop = docomma = 0;
}


vpopquipu ()
{
	vpopp();
	vps = NULLPS;
}

/*    PVPDU - for pepsy */

#undef	pvpdu

pvpdu (lp, ind, mod, pe, text, rw)
register LLog *lp;
int	ind;		/* index into tables */
modtyp	*mod;		/* pointer to tables */
register PE pe;
char   *text;
int	rw;
{
    register char   *bp;
    char   buffer[BUFSIZ];

    vfp = (FILE *) lp, vfnx = ll_printf;

    bp = buffer;
    (void) sprintf (bp, "%s %s", rw ? "read" : "wrote",
		    text ? text : "pdu");
    bp += strlen (bp);
    if (pe -> pe_context != PE_DFLT_CTX) {
	(void) sprintf (bp, ", context %d", pe -> pe_context);
	bp += strlen (bp);
    }
    LLOG (lp, LLOG_ALL, ("%s", buffer));

    vlevel = didname = didvpush = didvpop = docomma = 0;

    if (mod == NULL)
        (void) vunknown (pe);
    else
        (void) prnt_f (ind, mod, pe, 1, NULLIP, NULLVP);

    (void) ll_printf (lp, "-------\n");

    (void) ll_sync (lp);

    vfp = stdout, vfnx = fprintf;
}

/*    MISC */

static char *bp = NULL;

static char *newbuf (i)
int	i;
{
    static unsigned int len = 0;

    if (i++ < len)
	return bp;

    if (bp)
	free (bp);
    if ((bp = malloc ((unsigned int) i)))
	len = i;
    else
	len = 0;

    return bp;
}
/*  VPDU - support for backwards compatibility */

_vpdu (lp, fnx, pe, text, rw)
register LLog *lp;
IFP	fnx;
register PE pe;
char   *text;
int	rw;
{
    register char   *bp;
    char   buffer[BUFSIZ];

    vfp = (FILE *) lp, vfnx = ll_printf;

    bp = buffer;
    (void) sprintf (bp, "%s %s", rw ? "read" : "wrote",
		    text ? text : "pdu");
    bp += strlen (bp);
    if (pe -> pe_context != PE_DFLT_CTX) {
	(void) sprintf (bp, ", context %d", pe -> pe_context);
	bp += strlen (bp);
    }
    LLOG (lp, LLOG_ALL, ("%s", buffer));

    vlevel = didname = didvpush = didvpop = docomma = 0;

    (void) (*fnx) (pe, 1, NULLIP, NULLVP, NULLCP);

    (void) ll_printf (lp, "-------\n");

    (void) ll_sync (lp);

    vfp = stdout, vfnx = fprintf;
}

#ifdef PEP_TEST
free_bp()
{
    if (bp)
	free(bp);
}
#endif
