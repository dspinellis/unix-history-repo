/* etabs.c */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepsy/RCS/etabs.c,v 7.8 91/02/22 09:48:54 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/pepsy/RCS/etabs.c,v 7.8 91/02/22 09:48:54 mrose Interim $
 *
 *
 * $Log:	etabs.c,v $
 * Revision 7.8  91/02/22  09:48:54  mrose
 * Interim 6.8
 * 
 * Revision 7.7  91/01/08  12:49:27  mrose
 * update
 * 
 * Revision 7.6  90/12/11  10:33:33  mrose
 * sync
 * 
 * Revision 7.5  90/11/11  10:53:43  mrose
 * update
 * 
 * Revision 7.4  90/11/04  19:18:31  mrose
 * update
 * 
 * Revision 7.3  90/08/18  00:44:20  mrose
 * touch-up
 * 
 * Revision 7.2  90/07/27  08:49:15  mrose
 * update
 * 
 * Revision 7.1  90/07/09  14:52:33  mrose
 * sync
 * 
 * Revision 7.0  90/07/01  19:54:18  mrose
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
#include <ctype.h>
#include "pepsydefs.h"
#include "sym.h"
#include "pass2.h"
#include "mine.h"

s_table *head;

extern s_table *lookup_list(), *proc_def();

extern char *c_tag(), *c_class();
extern char *ec_tag(), *ec_class();
extern char *strip_last();
extern char *get_val(), *get_comp(), *get_string();
extern s_table *get_offset();
extern char *my_strcat(), *strp2name();
extern char *my_new_str();
extern char *mymodule;
extern char *modsym();
extern char *concat();
extern char *genlabel();
extern char *notidtoid();
extern char *code2name();
extern char *yp2name();
extern YV calc_yv();
extern SY syfind();
static s_table *en_ptr;
extern s_table *ptr;
extern char	*rm_indirect();
extern char	*getfield();
extern char	*setfield();

static int cons_type = 0;
/* int     explicit; */

s_table *save_ptr;

extern YT gen_etag();

#define WORDSIZE	20
#define MAXNAME		256	/* maximum size of a identifier */

#ifdef	DEBUG
char   *str_yp_code[] = {
    "Undefined", "Boolean", "Integer", "Named number list", "Bitstring",
    "Named Bitstring list", "Octet String", "Null", "Sequence",
    "Sequence of", "Sequence list", "Set", "Set of", "Set list",
    "Choice", "Any", "Object Identifier",
    "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "Identifier",

};

#endif

/*
 * table encode a type. generate tables for the encoding of a type
 */
tenc_typ(fp, yp, id, type)
FILE	*fp;
YP      yp;
char   *id;
char   *type;
{

    char   *t, *f;
    char   *p1;
    char   *s1, *s2, *s3;
    char   *s;
    s_table *ptr1, *ptr2;
    YP      y;
    YAL		yal;


    if (yp->yp_code < 0 || yp->yp_code > YP_REAL)
	ferrd(1, "tenc_typ: unimplemented type %d\n", yp->yp_code);

    if (yp == NULL) {
	ferr(0, "tenc_typ:NULL argument\n");
	return;
    }

    /* Preserve the type of the containing object */
    if (type)
	t = type;
    else if (yp->yp_param_type) {
	char *t1;
	/* we have a [[ P type ]] specification */
	if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
	    fprintf(stderr,
	    "\ntenc_typ:SETLIST can't extract direct type from %s\n",
		yp->yp_param_type);
	    exit(1);
	}
	t = strdup(t1);
    } else
	t = my_strcat("struct ", modsym(mymodule, id, "type"));

    if (yal = yp->yp_bef_alist) {
	yal->yal_type = t;
	if (yal->yal_enc)
	    gen_act(fp, yp->yp_bef_alist->yal_enc);
    }
    if (yal = yp->yp_aft_alist) {
	yal->yal_type = t;
    }
    if (yal = yp->yp_control_act)
	yal -> yal_type = t;

    if (yal = yp->yp_optional_act)
	yal -> yal_type = t;

    if (yp->yp_flags & YP_DEFAULT)
	gdflt(fp, yp, G_ENC);

    if ((yp->yp_flags & YP_PARMVAL) && yp->yp_parm) {
	if ((f = getfield(yp->yp_parm)) == NULLCP) {
	    fprintf(stderr, "\ntenc_typ: can't extract field from %s\n",
		yp->yp_parm);
	    exit(1);
	}
	f = strdup(f);
    } else
	f = yp->yp_varexp;

    if ((yp->yp_flags & (YP_OPTIONAL|YP_OPTCONTROL|YP_DEFAULT))
	== (YP_OPTIONAL|YP_OPTCONTROL)) {
	char    *f1;
	char	*bitno;

	if (yp -> yp_optional_act && yp -> yp_optional_act -> yal_enc) {
	    fprintf (fp, "\t{ BOPTIONAL, %d, 0, FL_USELECT},\n",
		     yp -> yp_optional_act -> yal_enc -> a_num);
	}
	else {
	    if ((f1 = getfldbit(yp->yp_optcontrol, &bitno)) == NULLCP) {
		fprintf(stderr,
			"\ntenc_typ:BOPTIONAL: can't extract field from %s\n",
			yp->yp_optcontrol);
		exit(1);
	    }
	    (void) fprintf(fp, "\t{ BOPTIONAL, AOFFSET(%s, %s), %s, 0},\n",
			   t, f1, bitno);
	}
    }

    /* handle explicit tags - one day may have to change this if anyone
     * ever defines a type with more than one explicit tag
     */
    if (yp->yp_flags & YP_TAG && !(yp->yp_flags & YP_IMPLICIT)) {
	(void) fprintf(fp, "\t{ ETAG, 0, ");
	(void) fprintf(fp, "%s, %s },\n", ec_tag(yp), ec_class(yp));
    }

    if (yp->yp_yfn && yp->yp_yfn->yfn_enc) {
	gen_fn(fp, yp, yp->yp_yfn->yfn_enc);

	if (yp->yp_aft_alist && yp->yp_aft_alist->yal_enc)
	    gen_act(fp, yp->yp_aft_alist->yal_enc);

	return;
    }

    switch (yp->yp_code) {

    case YP_UNDF:
	ferr(1, "tenc_typ:Undefined type\n");

    case YP_BOOL:
	p1 = "BOOLEAN";
	if (yp->yp_intexp)
	    f = setfield(yp->yp_intexp);
	if (noindirect(f))
	    ferr(1, "tenc_typ:BOOL: must specify a field [[ b .. ]]\n");
	if (yp->yp_varexp || (yp->yp_intexp && !noindirect(f)))
	    break;
	ferr(1, "tenc_typ:BOOL: couldn't determine type\n");

    case YP_INTLIST:

    case YP_INT:

    case YP_ENUMLIST:
	if (yp->yp_intexp)
	    f = setfield(yp->yp_intexp);
	if (noindirect(f))
	    ferr(1, "tenc_typ:INT: must specify a field [[ i .. ]]\n");
	if (yp->yp_varexp || (yp->yp_intexp && !noindirect(f))) {
	    p1 = "INTEGER";
	    break;
	}
	ferr(1, "tenc_typ:INT: couldn't determine type\n");
	break;

    case YP_REAL:
	if (yp->yp_strexp)
	    f = setfield(yp->yp_strexp);
	if (noindirect(f))
	    ferr(1, "tenc_typ:REAL: must specify a field [[ r .. ]]\n");
	if (yp->yp_varexp || (yp->yp_strexp && !noindirect(f))) {
	    p1 = "REALTYPE";
	    break;
	}
	ferr(1, "tenc_typ:REAL: couldn't determine type\n");
	break;

    case YP_BIT:
    case YP_BITLIST:
	if (yp->yp_strexp && yp->yp_intexp) {
	    if (yp->yp_strexp)
		f = setfield(yp->yp_strexp);
	    if (noindirect(f))
		ferr(1, "tenc_typ:BIT: must specify a field [[ x .. ]]\n");
	    p1 = "BITSTR_PTR";
	    prnte(fp, t, f, yp, p1);
	    if (yp->yp_intexp)
		f = setfield(yp->yp_intexp);
	    if (noindirect(f))
		ferr(1, "tenc_typ:BIT: must specify a field [[ x .. ]]\n");
	    p1 = "BITSTR_LEN";
	    break;
	}
	if (yp->yp_strexp == NULLCP && yp->yp_intexp)
	    f = setfield(yp->yp_intexp);
	if (yp->yp_varexp || (yp->yp_intexp && !noindirect(f))) {
	    p1 = "BITSTRING";
	    break;
	}
	t = NULL;
	p1 = NULL;
	(void) fprintf(fp, "\t{ SBITSTRING, 0, %s, %s },\n",
		c_tag(yp), c_class(yp));
	break;

    case YP_OCT:
	if (yp->yp_strexp) {
	    switch (yp->yp_prfexp) {
	    case 'q': /* [[ q parm->qbufptr ]] */
		if (yp->yp_strexp)
		    f = setfield(yp->yp_strexp);
		if (noindirect(f))
		    p1 = "SOCTETSTRING";
		else
		    p1 = "OCTETSTRING";
		break;

	    case 's': /* [[ s ptr ]] */
		if (yp->yp_strexp)
		    f = setfield(yp->yp_strexp);
		if (noindirect(f))
		    ferr(1, "tenc_typ:OCT: must specify a field [[ s .. ]]\n");
		p1 = "T_STRING";
		break;
		
	    case 'o': /* [[ o ptr $ length ]] */
		if (yp->yp_strexp)
		    f = setfield(yp->yp_strexp);
		if (noindirect(f))
		    ferr(1, "tenc_typ:OCT: must specify a field [[ s .. ]]\n");
		p1 = "OCTET_PTR";
		prnte(fp, t, f, yp, p1);
		if (yp->yp_intexp)
		    f = setfield(yp->yp_intexp);
		if (noindirect(f))
		    ferr(1, "tenc_typ:OCT: must specify a field [[ s .. ]]\n");
		p1 = "OCTET_LEN";
		break;

	    default:
	       fprintf(stderr,"\ntenc_typ: Unknown Octet string specifier %c\n",
		   yp->yp_prfexp);
		exit(1);
	    }
	    break;
	}

	if (f && !noindirect(f)) {
	    p1 = "OCTETSTRING";
	    break;
	}
	t = NULL;
	p1 = NULL;
	(void) fprintf(fp, "\t{ SOCTETSTRING, 0, %s, %s },\n",
		c_tag(yp), c_class(yp));
	break;

    case YP_OID:
	if (yp->yp_strexp)
	    f = setfield(yp->yp_strexp);
	if (yp->yp_varexp || (yp->yp_strexp && !noindirect(f))) {
	    p1 = "OBJID";
	    break;
	}
	t = NULL;
	p1 = NULL;
	(void) fprintf(fp, "\t{ SOBJID, 0, %s, %s },\n",
		c_tag(yp), c_class(yp));
	break;


    case YP_SEQ:
    case YP_SET:
    case YP_ANY:
	if (yp->yp_strexp)
	    f = setfield(yp->yp_strexp);
	if (yp->yp_varexp || (yp->yp_strexp && !noindirect(f))) {
	    p1 = "ANY";
	    break;
	}
	t = NULL;
	p1 = NULL;
	(void) fprintf(fp, "\t{ SANY, 0, %s, %s },\n",
		c_tag(yp), c_class(yp));
	break;

    case YP_NULL:
	p1 = "T_NULL";
	t = NULL;
	break;

    case YP_IDEFINED:
	p1 = NULL;
	if ((yp->yp_flags & YP_PARMVAL) && yp->yp_prfexp)
		ferr(1,
    "\n[[ ? reference ]] [[ p reference ]] is illegal\n\t only one allowed\n");

	if (yp->yp_prfexp) { /* [[ ? parm->field ]] - complex to process */
	    gen_identry(fp, t, f, yp, gen_ventry);
	    break;
	}

	{
	    /* Predefined Universal Type */
	    struct univ_typ *p, *univtyp();

	    if ((p = univtyp(yp->yp_identifier))) {
		if (p->univ_flags & UNF_EXTMOD) {
		    yp->yp_module = p->univ_mod;
		    goto do_obj;
		}
		if (f == NULL || noindirect(f)) {/* No offset type */
		    if (yp->yp_flags & YP_TAG
			&& yp->yp_flags & YP_IMPLICIT)
				prstfield(fp, p->univ_tab, t, f,
				int2tstr(yp->yp_tag->yt_value->yv_number),
				c_flags(yp, yp->yp_tag->yt_class));
			/*
			(void) fprintf(fp, "\t{ S%s, 0, %d, %s },\n",
				p->univ_tab,
				yp->yp_tag->yt_value->yv_number,
				c_flags(yp, yp->yp_tag->yt_class));
			 */
		    else
			prstfield(fp, p->univ_tab, t, f, int2tstr(p->univ_id),
				c_flags(yp, p->univ_class));
			/*
			(void) fprintf(fp, "\t{ S%s, 0, %d, %s },\n",
				p->univ_tab, p->univ_id,
				c_flags(yp, p->univ_class));
			 */
		    break;
		}
		if (yp->yp_flags & YP_TAG && yp->yp_flags & YP_IMPLICIT)
			prtfield(fp, p->univ_tab, t, f,
			    int2tstr(yp->yp_tag->yt_value->yv_number),
			    c_flags(yp, yp->yp_tag->yt_class));
		    /*
		    (void) fprintf(fp, "\t{ %s, OFFSET(%s, %s), %d, %s },\n",
			    p->univ_tab, t, f,
			    yp->yp_tag->yt_value->yv_number,
			    c_flags(yp, yp->yp_tag->yt_class));
		     */
		else
		    prtfield(fp, p->univ_tab, t, f, int2tstr(p->univ_id),
			    c_flags(yp, p->univ_class));
		    /*
		    (void) fprintf(fp, "\t{ %s, OFFSET(%s, %s), %d, %s },\n",
			    p->univ_tab, t, f, p->univ_id,
			    c_flags(yp, p->univ_class));
		     */
		break;
	    }
	}
do_obj:
	if (yp->yp_flags & YP_TAG && yp->yp_flags & YP_IMPLICIT)
	    (void) fprintf(fp, "\t{ IMP_OBJ, 0, %s, %s },\n", c_tag(yp), c_class(yp));
	if (yp->yp_module == NULL
	    || strcmp(yp->yp_module, mymodule) == 0) {
	    if (f == NULL || noindirect(f)) {	/* No offset type */
		  prstfield(fp, "OBJECT", t, f,
		      concat("_Z", proc_name(yp->yp_identifier, 0)),
		      c_class(yp));
		/*
		(void) fprintf(fp, "\t{ SOBJECT, 0, _Z%s, %s },\n",
		      proc_name(yp->yp_identifier, 0), c_class(yp));
		 */
	    } else
		  prtfield(fp, "OBJECT", t, f,
		      concat("_Z", proc_name(yp->yp_identifier, 0)),
		      c_class(yp));
		/*
		(void) fprintf(fp,
			"\t{ OBJECT, OFFSET(%s, %s), _Z%s, %s },\n",
		t, f, proc_name(yp->yp_identifier, 0), c_class(yp));
		*/
	} else {
	    if (f == NULL || noindirect(f)) {	/* No offset type */
		  prstfield(fp, "EXTOBJ", t, f,
		      concat("_Z", strp2name(yp->yp_identifier, yp->yp_module)),
			c_class(yp));
		/*
		(void) fprintf(fp, "\t{ SEXTOBJ, 0, _Z%s, %s },\n",
			strp2name(yp->yp_identifier, yp->yp_module),
			c_class(yp));
		 */
	    } else
		  prtfield(fp, "EXTOBJ", t, f,
		      concat("_Z", strp2name(yp->yp_identifier, yp->yp_module)),
			c_class(yp));
		/*
		(void) fprintf(fp,
			"\t{ EXTOBJ, OFFSET(%s, %s), _Z%s, %s },\n",
		  t, f, strp2name(yp->yp_identifier, yp->yp_module),
			c_class(yp));
		 */

	    (void) fprintf(fp, "\t{ EXTMOD, %d, 0, 0 },\n",
		    gen_modref(yp->yp_module));
	}
	break;

    case YP_SEQLIST:
	p1 = NULL;
	/* support for -h flag */
	cons_type++;
	save_ptr = en_ptr;
	if (yp->yp_varexp == NULL && type != NULL)
	    ferr(1, "tenc_typ:YP_SEQLIST:NULL varexp pointer\n");
	prcte(fp, type, t, f, yp, "SEQ_START");
	if (y = yp->yp_type) {
	    char *t1;

	    /* compute the type of data */
	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntenc_typ:SEQLIST: can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tenc_typ:YP_SEQLIST:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;

	    if (optfield(y)) {
		(void) fprintf(fp,
			"\t{ OPTL, OFFSET(%s, optionals), 0, 0 },\n",
			yp->yp_structname);
	    }
	    tenc_loop(fp, y, id, yp->yp_structname);
	}
	(void) fprintf(fp, "\t{ PE_END, 0, 0, 0 },\n");
	en_ptr = save_ptr;
	cons_type--;
	break;

    case YP_SETLIST:
	p1 = NULL;
	/* support for -h flag */
	cons_type++;
	if (yp->yp_varexp == NULL && type != NULL)
	    ferr(1, "tenc_typ:YP_SETLIST:NULL varexp pointer\n");
	prcte(fp, type, t, f, yp, "SET_START");
	if (y = yp->yp_type) {
	    char *t1;

	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntenc_typ:SETLIST can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tenc_typ:YP_SETLIST:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;
	    if (optfield(y)) {
		(void) fprintf(fp,
			"\t{ OPTL, OFFSET(%s, optionals), 0, 0 },\n",
			yp->yp_structname);
	    }
	    tenc_loop(fp, y, id, yp->yp_structname);
	}
	(void) fprintf(fp, "\t{ PE_END, 0, 0, 0 },\n");
	en_ptr = save_ptr;
	cons_type--;
	break;

    case YP_SEQTYPE:
	p1 = NULL;
	cons_type++;
	save_ptr = en_ptr;
	prcte(fp, type, t, f, yp, "SEQOF_START");
	if (y = yp->yp_type) {
	    char *t1;

	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntenc_typ:SETLIST can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tenc_typ:YP_SEQTYPE:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;
	    tenc_loop(fp, y, id, yp->yp_structname);
	}
	if (yp->yp_flags & YP_CONTROLLED) {
	    char *f1;

	    if ((f1 = getfield(yp->yp_control)) == NULLCP) {
		fprintf(stderr, "\ntenc_typ:SEQ OF: can't extract field from %s\n",
		    yp->yp_control);
		exit(1);
	    }
	    (void) fprintf(fp, "\t{ PE_END, OFFSET(%s, %s), 0, 0 },\n",
		    yp->yp_structname, f1);
	} else if (yp->yp_structname != NULL)
	    (void) fprintf(fp, "\t{ PE_END, OFFSET(%s, next), 0, 0 },\n",
		    yp->yp_structname);
	else
	    (void) fprintf(fp, "\t{ PE_END, 0, 0, 0 },\n");
	en_ptr = save_ptr;
	cons_type--;
	break;

    case YP_SETTYPE:
	p1 = NULL;
	cons_type++;
	save_ptr = en_ptr;
	prcte(fp, type, t, f, yp, "SETOF_START");

	if (y = yp->yp_type) {
	    char *t1;

	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntenc_typ:SETTYPE can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tenc_typ:YP_SETTYPE:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;
	    tenc_loop(fp, y, id, yp->yp_structname);
	}
	if (yp->yp_flags & YP_CONTROLLED) {
	    char *f1;

	    if ((f1 = getfield(yp->yp_control)) == NULLCP) {
		fprintf(stderr, "\ntenc_typ:SET OF: can't extract field from %s\n",
		    yp->yp_control);
		exit(1);
	    }
	    (void) fprintf(fp, "\t{ PE_END, OFFSET(%s, %s), 0, 0 },\n",
		    yp->yp_structname, f1);
	} else if (yp->yp_structname != NULL)
	    (void) fprintf(fp, "\t{ PE_END, OFFSET(%s, next), 0, 0 },\n",
		    yp->yp_structname);
	else
	    (void) fprintf(fp, "\t{ PE_END, 0, 0, 0 },\n");
	en_ptr = save_ptr;
	cons_type--;
	break;

    case YP_CHOICE:
	p1 = NULL;
	/* support for -h flag */
	if (hflag && (y = yp->yp_type) && !y->yp_next) {
	    tenc_typ(fp, y, id, yp->yp_structname);
	    break;
	}
	cons_type++;
	save_ptr = en_ptr;
	if (type == NULL || type && noindirect(f))
	    prstfield(fp, "CHOICE_START", t, f, 0, c_class(yp));
	else
	    prtfield(fp, "CHOICE_START", t, type ? f : NULLCP, 0, c_class(yp));
	if (y = yp->yp_type) {
	    char *t1;

	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntenc_typ:CHOICE can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tenc_typ:YP_CHOICE:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;

	    if (yp -> yp_control_act && yp->yp_control_act->yal_enc) {
		(void) fprintf (fp, "\t{ SCTRL, %d, 0, FL_USELECT },\n",
				yp -> yp_control_act -> yal_enc -> a_num);
	    }
	    else if (yp->yp_flags & YP_CONTROLLED) {
		char *f1;

		if ((f1 = getfield(yp->yp_control)) == NULLCP) {
		    fprintf(stderr, "\ntenc_typ:CHOICE: can't extract field from %s\n",
			yp->yp_control);
		    exit(1);
		}
		(void) fprintf(fp, "\t{ SCTRL, OFFSET(%s, %s), 0, 0 },\n",
			yp->yp_structname, f1);
	    } else if (yp->yp_structname != NULL)
	    (void) fprintf(fp, "\t{ SCTRL, OFFSET(%s, offset), 0, 0 },\n",
		    yp->yp_structname);
	    else
		ferr(1, "\nCHOICE missing SCTRL\n");
	    tenc_loop(fp, y, id, yp->yp_structname);
	}
	(void) fprintf(fp, "\t{ PE_END, 0, 0, 0 },\n");
	en_ptr = save_ptr;
	cons_type--;
	break;

    default:
	ferrd(1, "tenc_typ: yp_code = %d  not implemented\n", yp->yp_code);
    }

    if (p1 != NULL)
	prnte(fp, t, f, yp, p1);

    if (yp->yp_aft_alist && yp->yp_aft_alist->yal_enc)
	gen_act(fp, yp->yp_aft_alist->yal_enc);

}

static int fflags[] = {
    0, 1, 2, 2, 3, 3, 4, 5, 16, 16, 16, 17, 17, 17,
0, -1, 6, 0, 10, 9};

/*
 * calculate the tag string of the given type and return it
 */
char   *
c_tag(yp)
YP      yp;
{
    static char buf[WORDSIZE];
    int     i;

    if (yp->yp_flags & YP_TAG && yp->yp_flags & YP_IMPLICIT) {
	i = yp->yp_tag->yt_value->yv_number;
    } else {
	if (yp->yp_code < 0 || yp->yp_code > YP_REAL
	    || yp->yp_code == YP_CHOICE)
	    i = 0;
	else
	    i = fflags[yp->yp_code];
	/* Choice now legal argument - to allow prte_* routines to work */
	if (i == 0 && yp->yp_code != YP_CHOICE)
	    ferrd (1, "c_tag:Unknown Tag %d", yp->yp_code);
    }

    (void) sprintf(buf, "%d", i);

    return (buf);
}

/*
 * calculate the tag string of the explicit tag and return it
 */
char   *
ec_tag(yp)
YP      yp;
{
    static char buf[WORDSIZE];
    int     i;

    if (!(yp->yp_flags & YP_TAG) || yp->yp_flags & YP_IMPLICIT)
	ferr(1, "ec_tag:internal error:called with out explicit tag\n");

    i = yp->yp_tag->yt_value->yv_number;

    (void) sprintf(buf, "%d", i);

    return (buf);
}

/*
 * produce a string that represents the class/flags field for a given
 * yp entry taking the class to be that given in cl
 */
char   *
c_flags(yp, cl)
YP      yp;
int     cl;
{
    char   *p1;
    static char buf[STRSIZE];

    switch (yp->yp_code) {
    case YP_IDEFINED:
    case YP_CHOICE:
	if (yp->yp_flags & YP_TAG)
	    break;
	if (yp->yp_flags & YP_OPTIONAL
	    && ((yp->yp_flags & (YP_OPTIONAL|YP_OPTCONTROL|YP_DEFAULT))
		!= (YP_OPTIONAL|YP_OPTCONTROL))) {
	    p1 = "FL_OPTIONAL";
	} else if (yp->yp_flags & YP_DEFAULT) {
	    p1 = "FL_DEFAULT";
	} else
	    p1 = "0";
	return (p1);

    default:
	break;
    }
    p1 = class2str(cl);
    if (yp->yp_flags & YP_OPTIONAL
	    && ((yp->yp_flags & (YP_OPTIONAL|YP_OPTCONTROL|YP_DEFAULT))
		!= (YP_OPTIONAL|YP_OPTCONTROL))) {
	strncpy(buf, p1, STRSIZE);
	p1 = strncat(buf, "|FL_OPTIONAL", STRSIZE);
    } else if (yp->yp_flags & YP_DEFAULT) {
	strncpy(buf, p1, STRSIZE);
	p1 = strncat(buf, "|FL_DEFAULT", STRSIZE);
    }
    return (p1);
}
/*
 * turn the class number into its corresponding string
 */
char	*
class2str(cl)
int	cl;
{
    register char *p1;

    switch (cl) {
    case PE_CLASS_UNIV:
	p1 = "FL_UNIVERSAL";
	break;

    case PE_CLASS_APPL:
	p1 = "FL_APPLICATION";
	break;

    case PE_CLASS_PRIV:
	p1 = "FL_PRIVATE";
	break;

    case PE_CLASS_CONT:
	p1 = "FL_CONTEXT";
	break;

    default:
	ferrd(1, "class2str: illegal class found %d\n", cl);

    }
    return (p1);
}

/*
 * calculate a string specifying the class for the given type and
 * return it
 */
char   *
c_class(yp)
YP      yp;
{
    int     i;

    if (yp->yp_flags & YP_TAG && yp->yp_flags & YP_IMPLICIT) {
	i = yp->yp_tag->yt_class;
    } else {
	i = PE_CLASS_UNIV;
    }
    return (c_flags(yp, i));

}
/*
 * calculate a string specifying the class for the explicit tag and
 * return it
 */
char   *
ec_class(yp)
YP      yp;
{
    int     i;
    char   *p1;
    static char buf[STRSIZE];

    if (!(yp->yp_flags & YP_TAG) || yp->yp_flags & YP_IMPLICIT)
	ferr(1, "ec_class:internal error:called with out explicit tag\n");
    switch (yp->yp_code) {
    case YP_IDEFINED:
    case YP_CHOICE:
	if (yp->yp_flags & YP_TAG)
	    break;
	if (yp->yp_flags & YP_OPTIONAL
	    && ((yp->yp_flags & (YP_OPTIONAL|YP_OPTCONTROL|YP_DEFAULT))
		!= (YP_OPTIONAL|YP_OPTCONTROL))) {
	    p1 = "FL_OPTIONAL";
	} else if (yp->yp_flags & YP_DEFAULT) {
	    p1 = "FL_DEFAULT";
	} else
	    p1 = "0";
	return (p1);

    default:
	break;
    }

    i = yp->yp_tag->yt_class;

    switch (i) {
    case PE_CLASS_UNIV:
	p1 = "FL_UNIVERSAL";
	break;

    case PE_CLASS_APPL:
	p1 = "FL_APPLICATION";
	break;

    case PE_CLASS_PRIV:
	p1 = "FL_PRIVATE";
	break;

    case PE_CLASS_CONT:
	p1 = "FL_CONTEXT";
	break;

    default:
	ferrd(1, "c_class: illegal class found %d\n", i);

    }
    if ((yp->yp_flags & YP_OPTIONAL) 
	    && ((yp->yp_flags & (YP_OPTIONAL|YP_OPTCONTROL|YP_DEFAULT))
		!= (YP_OPTIONAL|YP_OPTCONTROL))) {
	strncpy(buf, p1, STRSIZE);
	p1 = strncat(buf, "|FL_OPTIONAL", STRSIZE);
    } else if (yp->yp_flags & YP_DEFAULT) {
	strncpy(buf, p1, STRSIZE);
	p1 = strncat(buf, "|FL_DEFAULT", STRSIZE);
    }
    return (p1);
}

/*
 * generate tables for encoding a contructed type
 */
tenc_loop(fp, yp, id, type)
FILE	*fp;
YP      yp;
char   *id;
char   *type;
{
    for (; yp != NULL; yp = yp->yp_next) {
	tenc_typ(fp, yp, id, type);
    }
}

/*
 * Print the string and exit if argument greater than zero
 */
ferr(i, s)
int     i;
char   *s;
{
    (void) fprintf(stderr, "%s", s);
    if (i > 0)
	exit(i);
}

/*
 * Print the integer and exit if argument greater than zero
 */
ferrd(i, s, d)
int     i;
char   *s;
int     d;
{
    (void) fprintf(stderr, s, d);
    if (i > 0)
	exit(i);
}

/*
 * Print the string and exit if argument greater than zero
 */
ferrs(i, s, d)
int     i;
char   *s;
char   *d;
{
    (void) fprintf(stderr, s, d);
    if (i > 0)
	exit(i);
}

/*
 * return a copy of the string s minus its last character
 */
char   *
strip_last(s)
char   *s;
{
    char   *t, *r;

    if (s) {
	t = new_string(s);
	for (r = t; *r != '\0'; r++);
	;
	*--r = '\0';
	return t;
    } else
	return NULL;
}

/*
 * add the declaration specified by the strings type and id to the
 * start of the declaration list
 */
add_list(type, id)
char   *type, *id;
{

    s_table *prev;

    if ((prev = (s_table *) malloc(sizeof(s_table))) == NULL)
	ferr(1, "add_list: Out of memory\n");
    prev->type = type;
    prev->name = id;
    prev->parent = NULL;
    prev->defined = 0;
    prev->next = head;
    head = prev;
}

/*
 * print the declaration list
 */
print_list()
{
    s_table *prev;

    for (prev = head; prev != NULL; prev = prev->next) {
	(void) printf("type is %s\n", prev->type);
	(void) printf("name is %s\n", prev->name);
	(void) printf("\n");
    }
}

/*
 * parse the declaration in the string s returning the type in v1 and
 * the name in v2
 */
parse_decl(s, v1, v2)
char  **s, **v1, **v2;
{
    char   *t;

    for (t = *s; *t != '\0' && !(isalnum(*t) || *t == '_'); t++);

    *s = t;
    if (*t != '\0') {
	for (; *t != '*'; t++);
	*t = '\0';
	*v1 = my_strcat(*s, "*");
	Printf(3, ("the type is %s\n", *v1));
	if (*++t == '*')
	    t++;
	for (*s = t; isalnum(*t) || *t == '_'; t++);
	if (*t != '\0') {
	    *t = '\0';
	    t++;
	}
	*v2 = new_string(*s);	/* don't really need new_string */
	Printf(2, ("the name is %s\n", *v2));
	*s = t;
    }
}

/*
 * return the next identifier in the string s
 */
char   *
get_val(s)
char  **s;
{
    char   *t, *r;

    for (t = *s; *t != '\0' && !(isalnum(*t) || *t == '_' || *t == '.'); t++);

    if (*t != '\0') {
	for (*s = t; isalnum(*t) || *t == '_' || *t == '.'; t++);
	*t = '\0';
	r = *s;
	Printf(3, ("arg is |%s|\n", r));
	*s = ++t;
	return r;
    } else
	return NULL;
}

/*
 * return the next component (sequence of characters up to the next
 * ';' or '\0') of the string s
 */
char   *
get_comp(s)
char  **s;
{
    char   *t, *r;

    for (t = *s; *t != '\0' && !(isalnum(*t) || *t == '_' || *t == ';'); t++);

    if (*t != '\0') {
	for (*s = t; *t != '\0' && *t != ';'; t++);
	*t = '\0';
	r = *s;
	Printf(3, ("component is |%s|\n", r));
	*s = ++t;
	return r;
    } else
	return NULL;
}

/*
 * return a copy of that part of the string s which may contain
 * definitions for the variables generated by posy
 */
char   *
get_string(s, direction)
char   *s;
int     direction;
{
    char   *t, *t1;

    if (direction & YP_ENCODER)
	return new_string(s);
    if (direction & YP_DECODER) {
	t = new_string(s);
	for (t1 = t; !(isalnum(*t1) || *t1 == '_'); t1++);
	if (*t1 == 'i' && *++t1 == 'f' && *++t1 == ' ') {	/* MEMALLOC code */
	    for (; *t1 != '}'; t1++)	/* skip MEMALLOC code */
		;
	    t1++;
	    Printf(4, ("returning the string %s\n", t1));
	    return t1;
	} else
	    return t;
    }
}

/*
 * Determine wether this list contains any items that will generate
 * an optional field. If so return non zero
 */
optfield(yp)
YP      yp;
{
    for (; yp; yp = yp->yp_next) {
	if (yp->yp_flags & YP_OPTIONAL) {

	    if ((yp->yp_flags & (YP_OPTIONAL|YP_OPTCONTROL|YP_DEFAULT))
		== (YP_OPTIONAL|YP_OPTCONTROL))
		return (0);

	    switch (yp->yp_code) {
	    case YP_BOOL:
	    case YP_INT:
	    case YP_INTLIST:
	    case YP_ENUMLIST:
	    case YP_NULL:
		return (1);
	    }
	}
    }
    return (0);
}

gen_dflts(fp, yp, type)
YP      yp;
char   *type;
{
    YP      y;

    if (yp == NULL)
	return;
    switch (yp->yp_code) {
    case YP_IDEFINED:
	break;

    case YP_CHOICE:
    case YP_SEQTYPE:
    case YP_SETTYPE:
    case YP_SEQLIST:
    case YP_SETLIST:
	for (y = yp->yp_type; y != NULL; y = y->yp_next) {
	    gen_dflts(fp, y, type);
	}
	break;

    default:
	break;
    }
    /* Output definitions for default entries */
    if (yp->yp_flags & YP_DEFAULT)
	defdflt(fp, yp, type);
    
    if (yp->yp_yfn)
       declfns(fp, yp->yp_yfn);
}
/*
 * Compute the concatenation into a temporary buffer of two strings
 * after having run notid on them first
 */
char   *
strp2name(s1, s2)
char   *s1, *s2;
{
    char   *p;
    static char buf[STRSIZE * 2 + 5];

    if (strlen(s1) > STRSIZE || strlen(s2) > STRSIZE)
	ferr(1, "strp2name:string to big\n");
    strcpy(buf, p = notidtoid(s1));
    free(p);
    strcat(buf, p = notidtoid(s2));
    free(p);

    return (buf);
}

/*
 * Output the definitions for default entries and initialise the yp's
 * to have pointers which reference these definitions for use by
 * gdflt routine.
 */
defdflt(fp, yp, name)
YP      yp;
char   *name;
{
    YV      yv;
    YV      yv1;
    SY      sy;
    YP      yp1;
    int     size, i;
    char   *str;
    char   *label;
    struct univ_typ	*p;
    int		code;

    if ((yp->yp_flags & YP_DEFAULT) == 0)
	ferrd(1, "defdflt:called with out a default code = %d\n", yp->yp_code);
    yv = yp->yp_default;

    yp1 = yp;

    /* Find the bottom definition */
    code = yp1->yp_code;
    while (code == YP_IDEFINED) {
	if ((sy = syfind(yp1->yp_identifier)) == NULL) {
	    if ((p = univtyp(yp1->yp_identifier)) == NULL
	       || p->univ_type <= YP_UNDF)
		ferrs(1,
		    "defdflt:IDEFINED:cannot find definition of symbol %s\n",
		    yp1->yp_identifier);
	    code = p->univ_type;
	    break;
		
	}
	yp1 = sy->sy_type;
	code = yp1->yp_code;
    }

    switch (code) {
    case YP_BOOL:
    case YP_INT:
    case YP_INTLIST:
    case YP_ENUMLIST:
	switch (yv->yv_code) {
	case YV_NUMBER:
	case YV_BOOL:
	    /* None needed */
	    break;

	case YV_IDEFINED:
	    if ((yv1 = calc_yv(yp1, yv->yv_identifier)) == NULL) {
		ferrs(1, "defdflt:BOOL/INT:cannot find definition of %s\n",
		      yv->yv_identifier);
	    }
	    /* None Needed */
	    break;

	default:
	    ferrd(1, "defdflt:INT/BOOL:unimplemented value code = %d\n",
		  yv->yv_code);
	}
	break;

    case YP_REAL:
	switch (yv->yv_code) {
	case YV_REAL:
	     yv1 = yv;
	     goto dumpdef3;

	case YV_IDEFINED:
	    if ((yv1 = calc_yv(yp1, yv->yv_identifier)) == NULL) {
		ferrs(1, "defdflt:REAL:cannot find definition of %s\n",
		      yv->yv_identifier);
	    }
	    goto dumpdef3;
	    break;

	default:
	    ferrd(1, "defdflt:REAL:unimplemented value code = %d\n",
		  yv->yv_code);
	}
	break;

    case YP_BIT:
    case YP_BITLIST:
	switch (yv->yv_code) {
	    /*
	     * This is an illegal value for a bit string ! - BUT ACSE
	     * uses it !
	     */
	    /* gdflt also patched to support it */
	case YV_IDEFINED:
	    ferrs(0,
		  "warning:bitstring default specified illegally with identifier %s\n",
		  yv->yv_identifier);
	    if ((yv1 = calc_yv(yp1, yv->yv_identifier)) == NULL) {
		ferrs(1, "defdflt:BIT:cannot find definition of %s\n",
		      yv->yv_identifier);
	    }
	    /* doesn't work fix posy-yacc.y */
	    size = numtobstr(yv1, &str);
	    goto dumpdef1;

	case YV_NUMBER:
	    size = numtobstr(yv, &str);
	    goto dumpdef1;

	case YV_VALIST:
	    if ((size = valisttobs(yp1, yv, &str)) < 0) {
		ferrs(1, "defdflt:bad default value for bit string %s\n",
		      yp->yp_flags & YP_ID ? yp->yp_identifier : "");
	    }
	    goto dumpdef1;

	case YV_HSTRING:
	    str = yv->yv_xstring;
	    size = yv->yv_xlength*4;
	    goto dumpdef1;

	case YV_BSTRING:
	    str = yv->yv_xstring;
	    size = yv->yv_xlength;
	    goto dumpdef1;

	default:
	    /* Could be a syntax error */
	    ferrd(1, "defdflt:BIT:illegal value code = %d\n", yv->yv_code);
	}
	break;

    case YP_IDEFINED:
	ferrs(1, "defdflt:IDEFINED:internal error on symbol %s\n",
	      yp1->yp_identifier);
	break;

    case YP_OCT:
	switch (yv->yv_code) {
	case YV_NUMBER:
	    size = numtobstr(yv, &str);
	    goto dumpdef2;

	case YV_STRING:
	    str = yv->yv_string;
	    size = strlen(str);
	    goto dumpdef2;

	case YV_HSTRING:
	    str = yv->yv_xstring;
	    size = (yv->yv_xlength + 1)/2;
	    goto dumpdef2;

	case YV_BSTRING:
	    str = yv->yv_xstring;
	    size = (yv->yv_xlength + 7)/8;	/* round up */
	    goto dumpdef2;

	default:
	    /* Could be a syntax error */
	    ferrd(1, "defdflt:OCT:illegal value code = %d\n", yv->yv_code);
	}
	break;

    case YP_NULL:
    case YP_SEQ:
    case YP_SEQTYPE:
    case YP_SEQLIST:
    case YP_SET:
    case YP_SETTYPE:
    case YP_SETLIST:
    case YP_CHOICE:
    case YP_ANY:
    case YP_OID:
	/* None yet */
	break;

    default:
	ferrd(1, "defdflt:unknown type %d\n", code);
    }

    return;

dumpdef1:	/* Bitstrings */
    label = genlabel(name, yp);
    yp->yp_action0 = label;
    yp->yp_act0_lineno = size;
    i = (size + NBPC - 1) / NBPC;
    (void) fprintf(fp, "\nstatic char %s[] = ", label);
    if (printable(str, i))
	prstr(fp, str, i);
    else
	prhstr(fp, str, i);
    (void) fprintf(fp, ";\n");
    return;

dumpdef2:	/* Octet strings (and aliases) */
    label = genlabel(name, yp);
    yp->yp_action0 = label;
    yp->yp_act0_lineno = size;
    (void) fprintf(fp, "\nstatic char %s[] = ", label);
    if (printable(str, size))
	prstr(fp, str, size);
    else
	prhstr(fp, str, size);
    (void) fprintf(fp, ";\n");
    return;

dumpdef3:	/* Reals */
    label = genlabel(name, yp);
    yp->yp_action0 = label;
    (void) fprintf(fp, "\nstatic double %s = %f;\n", label, yv1->yv_real);
    return;

}
/*
 * generate the default entry for encoding/decoding fields. This
 * should contain the default value which the encoder will know means
 * default encoding
 */
gdflt(fp, yp, which)
FILE	*fp;
YP      yp;
int     which;			/* Which type of entries to generate
				 * G_ENC encode G_DEC decode */
{
    YV      yv;
    YV      yv1;
    SY      sy;
    YP      yp1;
    int     size;
    char   *str;

    char   *ndflt;
    struct univ_typ     *p;
    int         code;


    if (which == G_ENC)
	ndflt = "DFLT_F";
    else
	ndflt = "DFLT_B";

    if ((yp->yp_flags & YP_DEFAULT) == 0)
	ferrd(1, "gdflt:called with out a default code = %d\n", yp->yp_code);
    yv = yp->yp_default;

    yp1 = yp;

    /* Find the bottom definition */
    code = yp1->yp_code;
    while (code == YP_IDEFINED) {
	if ((sy = syfind(yp1->yp_identifier)) == NULL) {
             if ((p = univtyp(yp1->yp_identifier)) == NULL
               || p->univ_type <= YP_UNDF)
		    ferrs(1,
			"gdflt:IDEFINED:cannot find definition of symbol %s\n",
			yp1->yp_identifier);
	     code = p->univ_type;
	     break;

	}
	yp1 = sy->sy_type;
	code = yp1->yp_code;
    }

    switch (code) {
    case YP_BOOL:
    case YP_INT:
    case YP_INTLIST:
    case YP_ENUMLIST:
	switch (yv->yv_code) {
	case YV_NUMBER:
	case YV_BOOL:
	    (void) fprintf(fp, "\t{ %s,	%d,	0,	0 },\n", ndflt,
		    yp->yp_default->yv_number);
	    break;

	case YV_IDEFINED:
	    if ((yv1 = calc_yv(yp1, yv->yv_identifier)) == NULL) {
		ferrs(1, "gdflt:BOOL/INT:cannot find definition of %s\n",
		      yv->yv_identifier);
	    }
	    (void) fprintf(fp, "\t{ %s,	%d,	0,	0 },\n", ndflt,
		    yv1->yv_number);
	    break;

	default:
	    ferrd(1, "gdflt:INT/BOOL:unimplemented value code = %d\n",
		  yv->yv_code);
	}
	break;

    case YP_BIT:
    case YP_BITLIST:
	switch (yv->yv_code) {
#ifdef ILLEGAL_DEFAULTS
	case YV_IDEFINED:	/* supporting illegal default
				 * specification */
#endif
	case YV_NUMBER:
	case YV_HSTRING:
	case YV_BSTRING:
	case YV_VALIST:
	    (void) fprintf(fp, "\t{ %s,	%d,	%d,	0 },\n", ndflt,
		    yp->yp_act0_lineno, addptr(yp->yp_action0));
	    break;

	default:
	    /* Could be a syntax error */
	    ferrd(1, "gdflt:BIT:illegal value code = %d\n", yv->yv_code);
	}
	break;

    case YP_IDEFINED:
	ferrs(1, "gdflt:IDEFINED:internal error on symbol %s\n",
	      yp1->yp_identifier);
	break;

    case YP_REAL:
	switch (yv->yv_code) {
#ifdef ILLEGAL_DEFAULTS
	case YV_IDEFINED:	/* Illegal according to ASN.1 but we can do it
				 * so why not support it
				 */
#endif
	case YV_REAL:
	    (void) fprintf(fp, "\t{ %s,	0,	%d,	0 },\n", ndflt,
		    addptr(concat("&", yp->yp_action0)));
	    break;

	default:
	    /* Could be a syntax error */
	    ferrd(1, "gdflt:REAL:illegal value code = %d\n", yv->yv_code);
	}
	break;

    case YP_OCT:
	switch (yv->yv_code) {
	case YV_NUMBER:
	case YV_STRING:
	case YV_HSTRING:
	case YV_BSTRING:
	    (void) fprintf(fp, "\t{ %s,	%d,	%d,	0 },\n", ndflt,
		    yp->yp_act0_lineno, addptr(yp->yp_action0));
	    break;

	default:
	    /* Could be a syntax error */
	    ferrd(1, "gdflt:OCT:illegal value code = %d\n", yv->yv_code);
	}
	break;

    case YP_NULL:
    case YP_SEQ:
    case YP_SEQTYPE:
    case YP_SEQLIST:
    case YP_SET:
    case YP_SETTYPE:
    case YP_SETLIST:
    case YP_CHOICE:
    case YP_ANY:
    case YP_OID:
	(void) fprintf(fp, "\t{ %s,	0,	0,	0 },\n", ndflt);
	break;

    default:
	ferrd(1, "gdflt:unknown type %d\n", yp->yp_code);
    }

}
/*
 * Calculate the value associated with the given identifier id by
 * looking at the value definitions associated with type definition
 * yp. Returns the value definition if found or NULL if not.
 */
YV
calc_yv(yp, id)
YP      yp;
char   *id;
{
    YV      yv;

    for (yv = yp->yp_value; yv != NULL; yv = yv->yv_next) {
	if (yv->yv_flags & YV_NAMED && strcmp(yv->yv_named, id) == 0)
	    return (yv);
    }

    return (NULL);
}

/*
 * ******* This does not work. posy needs to be fixed for case of
 * '01'b ***** Turn a Literal number value in yv into a bistring
 * initialisation. Return the length of the bit string or less than
 * zero on error. Set the (char *) pointer, whose address is in
 * ppstr, to point to a string containing the a reference to a
 * character array which contains the bits.
 */

numtobstr(yv, ppstr)
YV      yv;
char  **ppstr;
{

    int     ibits, lastb, i;
    char   *buf;

    buf = malloc(NBPI / NBPC + 1);
    bzero(buf, NBPI / NBPC + 1);
    lastb = -1;
    ibits = yv->yv_number;
    for (i = 0; i < NBPI; i++) {
	if ((1 << i) & ibits) {
	    buf[i / NBPC] |= 1 << (NBPC - 1 - (i % NBPC));
	    lastb = i;
	}
    }

    *ppstr = buf;
    return (lastb + 1);
}
#define ROUNDUP		10
/*
 * Take a list of Values (YV_VALIST) which should contain a list of
 * bits and convert them into a bitstring initialisation. As in
 * numtobstr return the size of the bit string or a negative number
 * if there is an error. Put a reference to a character array which
 * contains the definition of the bits in the character pointer whose
 * address is in ppstr. yp is the definition of the type which
 * contains the names of all the defined bits.
 */
valisttobs(yp, yv, ppstr)
YP      yp;
YV      yv;
char  **ppstr;
{

    YV      yv1, yv2;
    int     lastb, val, nsize, size;
    char   *buf;

    lastb = -1;
    size = ROUNDUP;
    if ((buf = malloc(size)) == NULL) {
	ferrd(1, "valisttobs:malloc:failed on %d\n", size);
    }
    bzero(buf, size);
    for (yv1 = yv->yv_idlist; yv1 != NULL; yv1 = yv1->yv_next) {
	if ((yv2 = calc_yv(yp, yv1->yv_identifier)) == NULL) {
	    return (-1);
	}
	val = yv2->yv_number;
	/* Bug here probably */
	if (size < val / NBPC) {
	    nsize = val / NBPC + ROUNDUP;
	    if ((buf = realloc(buf, nsize)) == NULL) {
		ferrd(1, "valisttobs:realloc:failed on %d\n", nsize);
	    }
	    bzero(buf + size, nsize - size);
	    size = nsize;
	}
	buf[val / NBPC] |= 1 << (NBPC - 1 - (val % NBPC));
	if (val > lastb)
	    lastb = val;
    }
    *ppstr = buf;
    return (lastb + 1);
}
/*
 * Print the string out in a format acceptable as a quoted string in
 * a C program including the quotes. Using \ escapes for unprintable
 * characters
 */
prstr(fp, str, len)
FILE   *fp;
char   *str;
int     len;
{
    (void) fputc('"', fp);
    while (len-- > 0) {
	if (isprint(*str & 0xff)) {
	    (void) fputc(*str & 0xff, fp);
	    str++;
	    continue;
	}
	(void) fprintf(fp, "\\%0o", *str & 0xff);
    }
    (void) fputc('"', fp);
#define MAXPLINE	16
}
/*
 * output a initialisation for a character array as unsigned hex
 * numbers
 */
prhstr(fp, str, len)
FILE   *fp;
char   *str;
int     len;
{
    int     npline;		/* number on this line */

    (void) fprintf(fp, "{\n");
    npline = 0;
    while (len > 0) {
	if (npline >= MAXPLINE) {
	    (void) fputc('\n', fp);
	    npline = 0;
	}
	npline++;
	(void) fprintf(fp, " 0x%02x,", *str++ & 0xff);
	len--;
    }
    (void) fprintf(fp, "}");
}
/*
 * determine if the string is printable i.e. only sensible to be read
 * as a character string. 1 (true) if it is 0, if it isn't
 */
printable(str, i)
char   *str;
int     i;
{
    while (i-- > 0) {
	if (!isprint(*str & 0xff))
	    return (0);		/* look for the first non printable
				 * character */
	str++;
    }
    return (1);
}
/*
 * generate a unique identifier  using the name given and the name if
 * present in yp. Return a pointer to it in a space malloc'ed out
 */
char   *
genlabel(name, yp)
char   *name;
YP      yp;
{
    char    buf[MAXNAME];
    static int cnt;
    char   *p1, *p2;

    p1 = notidtoid(name);
    if (yp->yp_flags & YP_ID) {
	p2 = notidtoid(yp->yp_id);
	(void) sprintf(buf, "L%s_%s_%d", p1, p2, cnt++);
	free(p2);
    } else
	(void) sprintf(buf, "L%s_X_%d", p1, cnt++);
    free(p1);

    return (my_new_str(buf));
}
/*
 * generate a ptr table reference for the given module table entry
 */
gen_modref(mod)
char	*mod;
{
    char	buf[BUFSIZ];
    char	*p1;
    int		ind;

    p1 = notidtoid(mod);
    (void) sprintf(buf, "&%s%s%s", PREFIX, p1, MODTYP_SUFFIX);
    ind = addptr(buf);
    free(p1);

    return (ind);
}

char	*
setfield(p)
char	*p;
{
    char	*f;

    if ((f = getfield(p)) == NULLCP) {
	fprintf(stderr, "\nsetfield: can't extract field from %s\n", p);
	exit(1);
    }
    return (strdup(f));
}

/*
 * print a normal table entry
 */
prnte(fp, t, f, yp, p1)
FILE	*fp;
char	*t;	/* parent type */
char	*f; 	/* field name */
YP	*yp;	/* object */
char	*p1;	/* table entry name */
{
    if (p1 == NULL)
	ferr(1, "prnte: called with a NULL p1\n");

    if (t && noindirect(f))
	prstfield(fp, p1, t, f, c_tag(yp), c_class(yp));
    else
	prtfield(fp, p1, t, f, c_tag(yp), c_class(yp));
}

/*
 * generate the entry allowing for defined types and then call the given
 * function to generate the base entry
 * Algorithm:
 * If we can find the base type 
 *   i) generate any ETAG that subsequent definitions might have
 *   ii) call the function to generate the base entry
 *   iii) and  the function checks that it matches the type of the value pass
 * else - can't find the base type - probably because it is external
 *   i)  generate a warning and exit
 */
gen_identry(fp, t, f, yp, fn)
FILE	*fp;
char	*t, *f;
YP	yp;
int	(*fn)();
{
   YP	yp1;
   int	code;
   SY	sy;
   YT	pd_yt = NULLYT;	/* Passed down tag if any */
   YT	yt;
   struct univ_typ	*p = NULL;
   char	*flags;		/* the flags if any which each subtype must have */
   int	save_flags;
    
    yp1 = yp;
    code = yp1->yp_code;

    if (yp->yp_flags & YP_OPTIONAL)
	flags = "|FL_OPTIONAL";
    else if (yp->yp_flags & YP_DEFAULT)
	flags = "|FL_DEFAULT";
    else
	flags = "";

    /* any explicit tag for original yp type is handled before this
     * routine is called so don't call gen_etag for it here
     * but we do need to initialise pd_yt for the case of an IMPLICIT TAG.
     */
    if ((yp->yp_flags & (YP_TAG|YP_IMPLICIT))== (YP_TAG|YP_IMPLICIT))
	pd_yt = yp->yp_tag;

    while (code == YP_IDEFINED) {
        if ((sy = syfind(yp1->yp_identifier)) == NULL) {
            if ((p = univtyp(yp1->yp_identifier)) == NULL
               || p->univ_type <= YP_UNDF)
                ferrs(1,
                    "\ngen_identry:symbol %s is not defined in this file:\npepsy cannot support value passing for this type, sorry\n",
                    yp1->yp_identifier);
            code = p->univ_type;
            break;

        }
        yp1 = sy->sy_type;
	    /* check for Explicit tags & generate ETAG entries */
	pd_yt = gen_etag(fp, pd_yt, yp1, flags);
        code = yp1->yp_code;
    }

    if (p) {
	/* how do we check type is consistent with value passed ? */
	yp1 = new_type(code, -1);

	yp1->yp_flags |= yp->yp_flags & (YP_OPTIONAL|YP_DEFAULT);

	if (pd_yt == NULLYT) {
	    yp1->yp_tag = new_tag(p->univ_class);
	    yp1->yp_tag->yt_value = new_value(YV_NUMBER);
	    yp1->yp_tag->yt_value->yv_number = p->univ_id;
	} else
	    yp1->yp_tag = pd_yt;
	yp1->yp_flags |= YP_TAG|YP_IMPLICIT;

	(*fn)(fp, yp, yp1, t, f);

     /* lets free the yp1 allocated above */
	if (pd_yt == NULLYT) {
	    free(yp1->yp_tag->yt_value);
	    free(yp1->yp_tag);
	}
	free(yp1);

	return;
    }
	/* check type is consistent with value passed some where??*/

    save_flags = yp1->yp_flags;
    yp1->yp_flags |= yp->yp_flags & (YP_OPTIONAL|YP_DEFAULT);

    if (pd_yt) {
	yt = yp1->yp_tag;
	yp1->yp_tag = pd_yt;
	yp1->yp_flags |= YP_TAG|YP_IMPLICIT;
    }

    (*fn)(fp, yp, yp1, t, f);

    if (pd_yt) {	/* restore the tag for later */
	yp1->yp_tag = yt;
    }
    yp1->yp_flags = save_flags;

}

/*
 * generate the ETAG entry if needed for following the given defined type
 * down to what it is. Given that its use above has an IMPLICIT tag pd_yt
 * if it is non Null
 */
YT
gen_etag(fp, pd_yt, yp, flags)
FILE	*fp;
YT	pd_yt;
YP	yp;
char	*flags;
{
    YT	yt;

    yt = yp->yp_tag;
    if (yt && yt->yt_value) {
	if ((yp->yp_flags & (YP_TAG|YP_IMPLICIT)) == YP_TAG) {
				    /* EXPLICIT TAG so generate an ETAG */
	    if (pd_yt)
		yt = pd_yt;  /* if we have a value passed down use that */
	    (void) fprintf(fp, "\t{ ETAG, 0, %d, %s%s },\n",
		yt->yt_value->yv_number, class2str(yt->yt_class), flags);
	    pd_yt = NULLYT; /* non't pass on any value */

	} else if ((yp->yp_flags & (YP_TAG|YP_IMPLICIT))== YP_TAG|YP_IMPLICIT) {
	    /* otherwise it is implicit and so pass its tag down
	     * unless we already have a tag being passed in which case
	     * the passed down tag overrides this current tag
	     */
	    if (pd_yt == NULLYT)
		pd_yt = yt;
	}
    }

    return (pd_yt);
}

/*
 * generate the table entry for a value passing defined type which
 * is equivalent to the given primative type
 */
gen_ventry(fp, oyp, yp, t, f)
FILE	*fp;
YP	oyp, yp;
char	*t, *f;
{
    char	*p1;
    register char	s = oyp->yp_prfexp;	/* type of value passing */


    if (noindirect(f) && s != 'q' && s != 'a')
	ferrs(1,
        "gen_ventry: must specify a field for primative value- not %s\n", f);

    switch (s) {
    case 'q': /* [[ q parm->qbufptr ]] */
	if (yp->yp_code != YP_OCT)
	    warning("qbuf pointer passed for a %s by type %s",
	    code2name(yp->yp_code), yp2name(oyp));
		 
	f = setfield(oyp->yp_strexp);
	if (noindirect(f))
	    p1 = "SOCTETSTRING";
	else
	    p1 = "OCTETSTRING";
	break;

    case 's': /* [[ s ptr ]] */
	if (yp->yp_code != YP_OCT)
	    warning("string pointer passed for a %s by type %s",
	    code2name(yp->yp_code), yp2name(oyp));

	f = setfield(oyp->yp_strexp);
	p1 = "T_STRING";
	break;

    case 'o': /* [[ o ptr $ length ]] */
	if (yp->yp_code != YP_OCT)
	    warning("octet/length pair passed for a %s by type %s",
	    code2name(yp->yp_code), yp2name(oyp));
	f = setfield(oyp->yp_strexp);
	p1 = "OCTET_PTR";
	prnte(fp, t, f, yp, p1);
	if (oyp->yp_intexp)
	    f = setfield(oyp->yp_intexp);
	if (noindirect(f))
	    ferr(1, "gen_ventry:OCT: must specify a field [[ s .. ]]\n");
	p1 = "OCTET_LEN";
	break;

    case 'x': /* [[ x ptr $ length ]] */
	if (yp->yp_code != YP_BIT && yp->yp_code != YP_BITLIST)
	    warning("bit string/length pair passed for a %s by type %s",
	    code2name(yp->yp_code), yp2name(oyp));

	f = setfield(oyp->yp_strexp);
	p1 = "BITSTR_PTR";
	prnte(fp, t, f, yp, p1);
	f = setfield(oyp->yp_intexp);
	if (noindirect(f))
	    ferr(1, "tenc_typ:BIT: must specify a field [[ x .. ]]\n");
	p1 = "BITSTR_LEN";
	break;

    case 'r': /* [[ r REAL ]] */
	if (yp->yp_code != YP_REAL)
	    warning("Real passed for a %s by type %s",
	    code2name(yp->yp_code), yp2name(oyp));

	f = setfield(oyp->yp_strexp);
	p1 = "REALTYPE";
	break;

    case 'i': /* [[ i INTEGER ]] */
	if (yp->yp_code != YP_INT && yp->yp_code != YP_INTLIST
	 && yp->yp_code != YP_ENUMLIST)
	    warning("integer passed for a %s by type %s",
	    code2name(yp->yp_code), yp2name(oyp));

	if (oyp->yp_intexp)
	    f = setfield(oyp->yp_intexp);
	p1 = "INTEGER";
	break;

    case 't': /* [[ t Bitvector ]] */
	if (yp->yp_code != YP_BIT && yp->yp_code != YP_BITLIST)
	    warning("Bitvector (PE) passed for a %s by type %s",
	    code2name(yp->yp_code), yp2name(oyp));

	f = setfield(oyp->yp_intexp);
	if (oyp->yp_varexp && !noindirect(f))
	    p1 = "BITSTRING";
	else
	    p1 = "SBITSTRING";
	break;

    case 'b': /* [[ b BOOLEAN ]] */
	if (yp->yp_code != YP_BOOL)
	    warning("Boolean passed for a %s by type %s",
	    code2name(yp->yp_code), yp2name(oyp));

	if (oyp->yp_intexp)
	    f = setfield(oyp->yp_intexp);
	p1 = "BOOLEAN";
	break;

    case 'O': /* [[ O Object Identifier ]] */
	if (yp->yp_code != YP_OID)
	    warning("Object Identifier pointer passed for a %s by type %s",
	    code2name(yp->yp_code), yp2name(oyp));
	f = setfield(oyp->yp_strexp);
	p1 = "OBJID";
	break;

    case 'a': /* [[ a ANY ]] */
	if (yp->yp_code != YP_ANY)
	    warning("PE pointer passed for a %s by type %s",
	    code2name(yp->yp_code), yp2name(oyp));
	f = setfield(oyp->yp_strexp);
	if (noindirect(f))
	    p1 = "SANY";
	else
	    p1 = "ANY";
	break;
     default:
       ferrd("gen_vident:unknown Value passed %d\n", (int )s);
   }

    prnte(fp, t, f, yp, p1);
}

/*
 * generate a table entry for a function call that handles this type
 */
gen_fn(fp, yp, fn)
FILE	*fp;
YP	yp;
char	*fn;	/* name of routine to generate */
{

    gen_identry(fp, fn, NULLCP, yp, gen_fnentry);
}

/*
 * generate a table entry for a function call that handles this type
 */
gen_fnentry(fp, oyp, yp, fn, dummy)
FILE	*fp;
YP	oyp;
YP	yp;
char	*fn;	/* name of routine to generate */
char	*dummy;
{

    (void) fprintf(fp, "\t{ FN_CALL, %d, %s, %s },\n",
	addptr(fn), c_tag(yp), c_class(yp));
}
/*
 * declare the functions that are used
 * One day generate ANSII C definitions as well
 */
declfns(fp, fn)
FILE	*fp;
YFN	fn;
{
    if (fn->yfn_enc) {
	(void) fprintf(fp, "extern int	%s();\n", fn->yfn_enc);
    }
    if (fn->yfn_dec) {
	(void) fprintf(fp, "extern int	%s();\n", fn->yfn_dec);
    }
    if (fn->yfn_prt) {
	(void) fprintf(fp, "extern int	%s();\n", fn->yfn_prt);
    }
    if (fn->yfn_fre) {
	(void) fprintf(fp, "extern int	%s();\n", fn->yfn_fre);
    }
}
/*
 * generate the table entry to handle an action - UCODE
 */
gen_act(fp, act)
FILE	*fp;
Action	act;
{
    (void) fprintf(fp, "\t{ UCODE, %d, 0, 0 }, /* line %d */\n", act->a_num, 
			act->a_line);
}

/*
 * print out the field entry for a type where all the parameters are given
 */
prtfield(fp, typ, t, f, cl, fl)
FILE	*fp;
char	*typ, *t, *f, *cl, *fl;
{
    if (cl == NULLCP)
	cl = "0";

    if (f && t) {
	if (*f == '&')
	    f++;
	(void) fprintf(fp, "\t{ %s, OFFSET(%s, %s), %s, %s },\n", typ, t, f,
							cl, fl);
    } else
	(void) fprintf(fp, "\t{ %s, 0, %s, %s },\n", typ, cl, fl);
}

/*
 * print out the field entry for a Simple type where all the parameters
 * are given
 */
prstfield(fp, typ, t, f, cl, fl)
FILE	*fp;
char	*typ, *t, *f, *cl, *fl;
{
    if (cl == NULLCP)
	cl = "0";

    if (f && t && *f == '&') {
	    f++;
	(void) fprintf(fp, "\t{ S%s, OFFSET(%s, %s), %s, %s },\n", typ, t, f,
							cl, fl);
    } else
	(void) fprintf(fp, "\t{ S%s, 0, %s, %s },\n", typ, cl, fl);
}

/*
 * convert an integer into a temporary string. Useful for calling
 * the printing routines with
 */
char	*
int2tstr(i)
int	i;
{
    static char	buf[STRSIZE];

    sprintf(buf, "%d", i);

    return (buf);
}

static char	*codetab[] = {
    "Undefined", "BOOLEAN", "INTEGER", "INTEGER (named numbers)", "BIT STRING",
    "BIT STRING (named bits)", "OCTET STRING", "NULL", "SEQUENCE",
    "SEQUENCE OF", "SEQUENCE",  "SET", "SET OF", "SET", "CHOICE",
    "ANY", "OBJECT IDENTIFIER", "Defined type", "ENUMERATED",
    "REAL", "Imported type",

    NULL
};
/*
 * produce a user readable name for a yp_code value
 */
char	*
code2name(code)
int	code;
{
    static char	buf[STRSIZE];

    if (code < 0 || code > YP_IMPTYPE) {
	sprintf(buf, "Unknown code (%d)", code);
	return (buf);
    }

    return (codetab[code]);
}
/*
 * print out a description of the yp type for the user that is good enough
 * for them to identifier the entry if possible
 */
char	*
yp2name(yp)
YP	yp;
{
    static char	buf[STRSIZE*4];
    char	*p;

    p = buf;
    if (yp->yp_code == YP_IDEFINED) {
	if (yp->yp_module) {
	     sprintf(p, "%s.", yp->yp_module);
	     p += strlen(p);
	}
	if (yp->yp_identifier)
	     sprintf(p, "%s", yp->yp_identifier);
	else
	     strcpy(p, "(no identifier)");
	p += strlen(p);
	if (yp->yp_modid) {
	     sprintf(p, " (%s)", sprintoid(yp->yp_modid));
	     p += strlen(p);
	}
    } else {
	sprintf(p, "%s", code2name(yp->yp_code));
	p += strlen(p);
    }

    if (yp->yp_flags & YP_ID) {
	sprintf(p, " %s", yp->yp_id);
	p += strlen(p);
    }

    if (yp->yp_lineno > 0) {
	sprintf(p, " on line %d", yp->yp_lineno);
	p += strlen(p);
    }

    return (buf);

}
/*
 * generate a table entry for the given compound type. It determines wether to
 * generate a simple type (prstfield) or not.
 */
prcte(fp, type, t, f, yp, p1)
FILE	*fp;
char	*type;	/* zero if we are foundation type of the table */
char	*t;	/* parent type */
char	*f; 	/* field name */
YP	*yp;	/* object */
char	*p1;	/* table entry name */
{
    if (type == NULL || type && noindirect(f))
	prstfield(fp, p1, t, f, c_tag(yp), c_class(yp));
    else
	prtfield(fp, p1, t, type ? f : NULLCP, c_tag(yp), c_class(yp));
}
