/* pl2pe.c - presentation list to presentation element */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/pl2pe.c,v 7.2 91/02/22 09:36:18 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/pl2pe.c,v 7.2 91/02/22 09:36:18 mrose Interim $
 *
 *
 * $Log:	pl2pe.c,v $
 * Revision 7.2  91/02/22  09:36:18  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/21  11:31:04  mrose
 * sun
 * 
 * Revision 7.0  89/11/23  22:13:07  mrose
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


/* Presentation lists are a human-readable, unambiguous way of describing
   a presentation element.

    SYNTAX:		list	::	"(" class code arguments ")"

			class	::	"UNIV" / "APPL" / "CONT" / "PRIV"

			code	::	name / number

			name	::	letter (letter / digit / dash)*

			number	::	"0x" [0-f] [0-f]* /
					"0" [0-7] [0-7]* /
					[1-9] [0-9]* /
					"\"" (IA5 subset)* "\""

			arguments::	primitive / constructor

			primitive::	number number*

			constructor::	list*

   NOTE WELL:	A single "number" must be representable in no more than
		(sizeof (int)) bytes.
 */


/* LINTLIBRARY */

#include <ctype.h>
#include <stdio.h>
#include "psap.h"

/*    DATA */

typedef struct PList {
    u_char pl_code;
#define	PL_CODE_LPAR	0
#define	PL_CODE_NAME	1
#define	PL_CODE_NUM	2
#define	PL_CODE_RPAR	3

    union {
	char    un_pl_name[BUFSIZ];
	int     un_pl_num;
    }                       pl_un;
#define	pl_name	pl_un.un_pl_name
#define	pl_num	pl_un.un_pl_num
}			PList, *PL;


PE  pl2pe_aux ();

/*  */

PE	pl2pe (ps)
register PS	ps;
{
    struct PList    pls;
    register PL	    pl = &pls;

    if (pl_read_lex (ps, pl) == NOTOK) {
	if (ps -> ps_errno == PS_ERR_EOF)
	    ps -> ps_errno = PS_ERR_NONE;
	return NULLPE;
    }
    if (pl -> pl_code != PL_CODE_LPAR)
	return ps_seterr (ps, PS_ERR_XXX, NULLPE);

    return pl2pe_aux (ps, pl);
}
    
/*  */

static PE  pl2pe_aux (ps, pl)
register PS	ps;
register PL	pl;
{
    PElementClass   class;
    PElementID	    id;
    register PE	    pe;

    if (pl_read_class (ps, pl, &class) == NOTOK)
	return NULLPE;
    if (pl_read_id (ps, pl, class, &id) == NOTOK)
	return NULLPE;

    if ((pe = pe_alloc (class, PE_FORM_PRIM, id)) == NULLPE)
	return ps_seterr (ps, PS_ERR_NMEM, NULLPE);

    if (pl_read_lex (ps, pl) == NOTOK)
	return NULLPE;
    switch (pl -> pl_code) {
	case PL_CODE_LPAR: 
	    if (pl_read_cons (ps, pl, &pe -> pe_cons) == NOTOK)
		goto you_lose;	/* else fall */
	case PL_CODE_RPAR: 
	    pe -> pe_form = PE_FORM_CONS;
	    break;

	case PL_CODE_NUM: 
	    if (pl_read_prim (ps, pl, pe) == NOTOK)
		goto you_lose;
	    break;

	default: 
	    ps -> ps_errno = PS_ERR_XXX;
	    goto you_lose;
    }
    return pe;

you_lose: ;
    pe_free (pe);
    return NULLPE;
}

/*  */

static int pl_read_class (ps, pl, class)
register PS	ps;
register PL	pl;
register PElementClass *class;
{
    register int    i;

    if (pl_read_lex (ps, pl) == NOTOK)
	return NOTOK;
    if (pl -> pl_code != PL_CODE_NAME)
	return ps_seterr (ps, PS_ERR_XXX, NOTOK);

    if ((i = pl_read_name (pl -> pl_name, pe_classlist, pe_maxclass)) == NOTOK)
	return ps_seterr (ps, PS_ERR_XXX, NOTOK);

    *class = i;
    return OK;
}

/*  */

static int pl_read_id (ps, pl, class, id)
register PS	ps;
register PL	pl;
register PElementClass class;
register PElementID *id;
{
    register int    i;
    register char **list;

    if (pl_read_lex (ps, pl) == NOTOK)
	return NOTOK;
    switch (pl -> pl_code) {
	case PL_CODE_NAME: 
	    switch (class) {
		case PE_CLASS_UNIV: 
		    list = pe_univlist, i = pe_maxuniv;
		    break;
		case PE_CLASS_APPL: 
		    list = pe_applist, i = pe_maxappl;
		    break;
		default: 
		    list = NULL, i = 0;
		    break;
		case PE_CLASS_PRIV:
		    list = pe_privlist, i = pe_maxpriv;
		    break;
	    }
	    if ((i = pl_read_name (pl -> pl_name, list, i)) == NOTOK)
		return ps_seterr (ps, PS_ERR_XXX, NOTOK);
	    break;

	case PL_CODE_NUM: 
	    i = pl -> pl_num;
	    break;

	default: 
	    return ps_seterr (ps, PS_ERR_XXX, NOTOK);
    }

    *id = i;
    return OK;
}

/*  */

static int  pl_read_name (name, list, n)
register char *name,
	     **list;
register int   n;
{
    register int    i;
    register char  *bp;

    for (i = n; i > 0; i--)
	if ((bp = *list++) && strcmp (bp, name) == 0)
	    return (n - i);

    return NOTOK;
}

/*  */

static int  pl_read_cons (ps, pl, pe)
register PS	ps;
register PL	pl;
register PE    *pe;
{
    register PE	    p,
		    q;

    if ((p = pl2pe_aux (ps, pl)) == NULLPE)
	return NOTOK;
    *pe = p;

    for (q = p;; q = q -> pe_next = p) {
	if (pl_read_lex (ps, pl) == NOTOK)
	    return NOTOK;
	switch (pl -> pl_code) {
	    case PL_CODE_LPAR: 
		if ((p = pl2pe_aux (ps, pl)) == NULLPE)
		    return NOTOK;
		break;

	    default: 
		return ps_seterr (ps, PS_ERR_XXX, NOTOK);

	    case PL_CODE_RPAR: 
		return OK;
	}
    }
}

/*  */

static int  pl_read_prim (ps, pl, pe)
register PS	ps;
register PL	pl;
register PE	pe;
{
    register int    i,
                    len,
                    n;
    register PElementData dp,
			  ep;

    if ((len = pl -> pl_num) == 0)
	goto out;
    if ((dp = PEDalloc (len)) == NULLPED)
	return ps_seterr (ps, PS_ERR_NMEM, NOTOK);

    pe -> pe_prim = dp, pe -> pe_len = len;

    for (ep = dp + len; dp < ep;) {
	i = min (ep - dp, sizeof (int));
	if (pl_read_lex (ps, pl) == NOTOK)
	    return NOTOK;
	if (pl -> pl_code != PL_CODE_NUM)
	    return ps_seterr (ps, PS_ERR_XXX, NOTOK);
	n = pl -> pl_num;
	while (i-- > 0)
	    *dp++ = (n >> (i * 8)) & 0xff;
    }

out: ;
    if (pl_read_lex (ps, pl) == NOTOK)
	return NOTOK;
    if (pl -> pl_code != PL_CODE_RPAR)
	return ps_seterr (ps, PS_ERR_XXX, NOTOK);

    return OK;
}

/*  */

#ifdef	XXX
static int  pl_read_lex (ps, pl)
register PS	ps;
register PL	pl;
{
    int     i = pl_read_lex_aux (ps, pl);

    fprintf (stderr, "pl_read_lex returns ");
    if (i == NOTOK) {
	fprintf (stderr, "NOTOK [%s]\n", ps_error (ps -> ps_errno));
	return NOTOK;
    }
    switch (pl -> pl_code) {
	case PL_CODE_LPAR: 
	    fprintf (stderr, "LPAR");
	    break;
	case PL_CODE_RPAR: 
	    fprintf (stderr, "RPAR");
	    break;
	case PL_CODE_NAME: 
	    fprintf (stderr, "NAME \"%s\"", pl -> pl_name);
	    break;
	case PL_CODE_NUM: 
	    fprintf (stderr, "NUM 0x%x", pl -> pl_num);
	    break;
	default: 
	    fprintf (stderr, "code %d", pl -> pl_code);
	    break;
    }
    fprintf (stderr, "\n");
    if (pl -> pl_code == PL_CODE_RPAR)
	sleep(1);

    return i;
}

#define	pl_read_lex	pl_read_lex_aux
#endif

/*  */

static int  pl_read_lex (ps, pl)
register PS	ps;
register PL	pl;
{
    register int    base,
                    n;
    register char  *bp;
    byte    c;

    do {
	if (pl_read (ps, &c) == NOTOK)
	    return NOTOK;
    } while (isspace ((u_char) c));

    switch (c) {
	case '(': 
	    pl -> pl_code = PL_CODE_LPAR;
	    return OK;
	case ')': 
	    pl -> pl_code = PL_CODE_RPAR;
	    return OK;

	case ';':
	    do {
		if (pl_read (ps, &c) == NOTOK)
		    return NOTOK;
	    } while (c != '\n');
	    return pl_read_lex (ps, pl);

	default: 
	    if (isalpha ((u_char) c)) {
		pl -> pl_code = PL_CODE_NAME;
		bp = pl -> pl_name;
		while (isalnum ((u_char) c) || c == '-') {
		    *bp++ = c;
		    if (pl_read (ps, &c) == NOTOK)
			return NOTOK;
		}
		*bp = NULL;
		ps -> ps_scratch = c;
		return OK;
	    }

	    if (c == '"') {
		pl -> pl_code = PL_CODE_NUM;
		for (n = 0;;) {
		    if (pl_read (ps, &c) == NOTOK)
			return NOTOK;
		    if (c == '"') {
			pl -> pl_num = n;
			return OK;
		    }
		    n = (n << 8) | (c & 0xff);
		}
	    }

	    if (!isdigit ((u_char) c))
		return ps_seterr (ps, PS_ERR_XXX, NOTOK);

	    pl -> pl_code = PL_CODE_NUM;
	    if (c == '0') {
		if (pl_read (ps, &c) == NOTOK)
		    return NOTOK;
		if (c == 'x' || c == 'X') {
		    base = 16;
		    if (pl_read (ps, &c) == NOTOK)
			return NOTOK;
		}
		else {
		    base = 8;
		    if (c < '0' || c > '7') {
			pl -> pl_num = 0;
			ps -> ps_scratch = c;
			return OK;
		    }
		}
	    }
	    else
		base = 10;

	    for (n = 0;;) {
		switch (base) {
		    case 10: 
			if (c < '0' || c > '9')
			    return ps_seterr (ps, PS_ERR_XXX, NOTOK);
			break;

		    case 8: 
			if (c < '0' || c > '7')
			    return ps_seterr (ps, PS_ERR_XXX, NOTOK);
			break;

		    case 16: 
			if (c >= '0' && c <= '9')
			    break;
			if (c >= 'A' && c <= 'F')
			    c += 'a' - 'A';
			else
			    if (c < 'a' || c > 'f')
				return ps_seterr (ps, PS_ERR_XXX, NOTOK);
			c += '9' + 1 - 'a';
			break;
		}
		n = (n * base) + c - '0';
		if (pl_read (ps, &c) == NOTOK)
		    return NOTOK;
		if (!isxdigit ((u_char) c)) {
		    pl -> pl_num = n;
		    ps -> ps_scratch = c;
		    return OK;
		}
	    }
    }
}

/*  */

static int pl_read (ps, c)
register PS	ps;
register byte  *c;
{
    if (ps -> ps_scratch) {
	*c = ps -> ps_scratch;
	ps -> ps_scratch = 0;
	return OK;
    }

    return ps_read (ps, c, 1);
}
