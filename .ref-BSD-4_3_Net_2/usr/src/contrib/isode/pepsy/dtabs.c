/* dtabs.c */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepsy/RCS/dtabs.c,v 7.10 91/02/22 09:48:47 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/pepsy/RCS/dtabs.c,v 7.10 91/02/22 09:48:47 mrose Interim $
 *
 *
 * $Log:	dtabs.c,v $
 * Revision 7.10  91/02/22  09:48:47  mrose
 * Interim 6.8
 * 
 * Revision 7.9  91/01/08  12:49:21  mrose
 * update
 * 
 * Revision 7.8  90/12/23  17:24:26  mrose
 * patches
 * 
 * Revision 7.7  90/12/11  10:33:19  mrose
 * sync
 * 
 * Revision 7.5  90/11/11  09:58:51  mrose
 * touch-up
 * 
 * Revision 7.4  90/11/04  19:18:21  mrose
 * update
 * 
 * Revision 7.3  90/08/18  00:44:15  mrose
 * touch-up
 * 
 * Revision 7.2  90/07/27  08:49:09  mrose
 * update
 * 
 * Revision 7.1  90/07/09  14:52:25  mrose
 * sync
 * 
 * Revision 7.0  90/07/01  19:54:15  mrose
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
#include "pepsydefs.h"
#include "pass2.h"
#include "mine.h"
#include <ctype.h>


extern FILE *fp;
extern char *c_tag(), *c_class();
extern char *ec_tag(), *ec_class();
extern char *strip_last();
extern char *str_yp_code[];
extern char *get_val(), *get_comp(), *strp2name();
extern s_table *lookup_list(), *get_offset();
extern YP tdec_loop();

extern char *concat();
extern char *my_strcat();
extern char	*rm_indirect();
extern char	*getfield();
extern char	*setfield();

extern int	gen_sentry();

/* extern int explicit; */

#define WORDSIZE	20

/*
 * table encode a type. generate tables for the encoding of a type
 */
tdec_typ(fp, yp, id, type)
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
    YAL	yal;

    if (yp->yp_code < 0 || yp->yp_code > YP_REAL)
	ferrd(1, "tdec_typ: unimplemented type %d\n", yp->yp_code);

    if (yp == NULL) {
	ferr(0, "tdec_typ:NULL arguement\n");
	return;
    }

    if (type)
	t = type;
    else if (yp->yp_param_type) {
	char *t1;
	/* we have a [[ P type ]] specification */
	if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
	    fprintf(stderr,
	    "\ntdec_typ:SETLIST can't extract direct type from %s\n",
		yp->yp_param_type);
	    exit(1);
	}
	t = strdup(t1);
    } else
	t = my_strcat("struct ", modsym(mymodule, id, "type"));

    if (yal = yp->yp_bef_alist) {
	yal->yal_type = t;
	if (yal->yal_dec)
	    gen_act(fp, yp->yp_bef_alist->yal_dec);
    }
    if (yal = yp->yp_aft_alist) {
	yal->yal_type = t;
    }
    if (yal = yp->yp_control_act)
	yal -> yal_type = t;

    if (yal = yp->yp_optional_act)
	yal -> yal_type = t;

    if (type == NULLCP) {
	switch (yp->yp_code) {
	    /*
	     * These generate MEMALLOC entries inside *_START ..
	     * PE_END fields for historical reasons. One day we might
	     * fix this to be all done the same way.
	     */
	case YP_SEQLIST:
	case YP_SEQTYPE:
	case YP_SETLIST:
	case YP_SETTYPE:
	case YP_CHOICE:
	    break;

	default:
	    if (yp->yp_varexp == NULL)
		break;		/* S* type entry - doesn't need a
				 * malloc */

	    (void) fprintf(fp, "\t{ MEMALLOC, 0, sizeof (%s), 0 },\n", t);
	    break;
	}
    }


    if ((yp->yp_flags & YP_PARMVAL) && yp->yp_parm) {
	if ((f = getfield(yp->yp_parm)) == NULLCP) {
	    fprintf(stderr, "\ntdec_typ: can't extract field from %s\n",
		yp->yp_parm);
	    exit(1);
	}
	f = strdup(f);
    } else
	f = yp->yp_varexp;

    if ((yp->yp_flags & (YP_OPTIONAL|YP_OPTCONTROL|YP_DEFAULT))
	== (YP_OPTIONAL|YP_OPTCONTROL)) {
	char *f1;
	char *bitno;

	if (yp -> yp_optional_act && yp -> yp_optional_act -> yal_dec) {
	    (void) fprintf (fp, "\t{ BOPTIONAL, %d, 0, FL_USELECT},\n",
			    control_act(yp -> yp_optional_act -> yal_dec));
	}
	else {
	    if ((f1 = getfldbit(yp->yp_optcontrol, &bitno)) == NULLCP) {
		fprintf(stderr,
			"\ntdec_typ:BOPTIONAL: can't extract field from %s\n",
			yp->yp_optcontrol);
		exit(1);
	    }
	    (void) fprintf(fp, "\t{ BOPTIONAL, AOFFSET(%s, %s), %s, 0},\n", t,
			   f1, bitno);
	}
    }

    if (yp->yp_flags & YP_TAG && !(yp->yp_flags & YP_IMPLICIT)) {
	(void) fprintf(fp, "\t{ ETAG, 0, ");
	(void) fprintf(fp, "%s, %s },\n", ec_tag(yp), ec_class(yp));
    }

    if (yp->yp_yfn && yp->yp_yfn->yfn_dec) {

	gen_fn(fp, yp, yp->yp_yfn->yfn_dec);

	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);

	if (gen_freefn(fp, yp)) {
	    if (yp->yp_aft_alist && yp->yp_aft_alist->yal_dec)
		gen_act(fp, yp->yp_aft_alist->yal_dec);
	    return;
	}
    }

    switch (yp->yp_code) {

    case YP_UNDF:
	ferr(1, "tdec_typ:Undefined type\n");

    case YP_BOOL:
	if (yp->yp_intexp)
	    f = setfield(yp->yp_intexp);
	if (noindirect(f))
	    ferr(1, "tdec_typ:BOOL: must specify a field for boolean\n");
	p1 = "BOOLEAN";
	if (yp->yp_varexp || (yp->yp_intexp && !noindirect(f)))
	    break;
	ferr(1, "tdec_typ:BOOL: couldn't determine type\n");

	/* This needs to be fixed up in the action generating area */
    case YP_INTLIST:

    case YP_INT:

    case YP_ENUMLIST:
	if (yp->yp_intexp)
	    f = setfield(yp->yp_intexp);
	if (noindirect(f))
	    ferr(1, "tdec_typ:INT: must specify a field for an integer\n");
	if (yp->yp_varexp || (yp->yp_intexp && !noindirect(f))) {
	    p1 = "INTEGER";
	    break;
	}
	ferr(1, "tdec_typ:INT: couldn't determine type\n");
	break;

    case YP_REAL:
	if (yp->yp_strexp)
	    f = setfield(yp->yp_strexp);
	if (noindirect(f))
	    ferr(1, "tdec_typ:REAL: must specify a field for a REAL\n");
	if (yp->yp_varexp || (yp->yp_strexp && !noindirect(f))) {
	    p1 = "REALTYPE";
	    break;
	}
	ferr(1, "tdec_typ:REAL: couldn't determine type\n");
	break;


    case YP_BIT:
    case YP_BITLIST:
	if (yp->yp_strexp && yp->yp_intexp) {
	    if (yp->yp_strexp)
		f = setfield(yp->yp_strexp);
	    if (noindirect(f))
		ferr(1, "tdec_typ:BIT: must specify a [[ x ... ]] value\n");
	    p1 = "BITSTR_PTR";
	    prnte(fp, t, f, yp, p1);
	    if (yp->yp_intexp)
		f = setfield(yp->yp_intexp);
	    if (noindirect(f))
		ferr(1, "tdec_typ:BIT: must specify a [[ x ... ]] value\n");
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
		    ferr(1, "tdec_typ:OCTET: must specify a field [[ s .. ]]\n");
		p1 = "T_STRING";
		break;
		
	    case 'o': /* [[ o ptr $ length ]] */
		if (yp->yp_strexp)
		    f = setfield(yp->yp_strexp);
		if (noindirect(f))
		    ferr(1, "tdec_typ:OCTET: must specify a field [[ o .. ]]\n");
		p1 = "OCTET_PTR";
		prnte(fp, t, f, yp, p1);
		if (yp->yp_intexp)
		    f = setfield(yp->yp_intexp);
		if (noindirect(f))
		    ferr(1, "tdec_typ:OCTET: must specify a field [[ o .. $ .. ]]\n");
		p1 = "OCTET_LEN";
		break;

	    default:
	       fprintf(stderr,"\ntdec_typ: Unknown Octet string specifier %c\n",
		   yp->yp_prfexp);
		exit(1);
	    }
	    break;
	}
	if (f) {
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

	    if (yp->yp_flags & YP_DEFAULT)
		gdflt(fp, yp, G_DEC);

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
		    else
                        prstfield(fp, p->univ_tab, t, f, int2tstr(p->univ_id),
                                c_flags(yp, p->univ_class));
		    goto out;
		}
		if (yp->yp_flags & YP_TAG && yp->yp_flags & YP_IMPLICIT)
                        prtfield(fp, p->univ_tab, t, f,
                            int2tstr(yp->yp_tag->yt_value->yv_number),
                            c_flags(yp, yp->yp_tag->yt_class));
		else
                    prtfield(fp, p->univ_tab, t, f, int2tstr(p->univ_id),
                            c_flags(yp, p->univ_class));
		goto out;
	    }
	}
do_obj:
	if (yp->yp_flags & YP_TAG && yp->yp_flags & YP_IMPLICIT)
	    (void) fprintf(fp, "\t{ IMP_OBJ, 0, %s, %s },\n", c_tag(yp), c_class(yp));
	if (yp->yp_module == NULL
	    || strcmp(yp->yp_module, mymodule) == 0) {
	    if (f == NULL || noindirect(f))
                  prstfield(fp, "OBJECT", t, f,
                      concat("_Z", proc_name(yp->yp_identifier, 0)),
                      c_class(yp));
	    else
                  prtfield(fp, "OBJECT", t, f,
                      concat("_Z", proc_name(yp->yp_identifier, 0)),
                      c_class(yp));

	} else {
	    if (f == NULL || noindirect(f))
                  prstfield(fp, "EXTOBJ", t, f,
                      concat("_Z", strp2name(yp->yp_identifier, yp->yp_module)),
                        c_class(yp));
	    else
                  prtfield(fp, "EXTOBJ", t, f,
                      concat("_Z", strp2name(yp->yp_identifier, yp->yp_module)),
                        c_class(yp));
	    (void) fprintf(fp, "\t{ EXTMOD, %d, 0, 0 },\n",
		    gen_modref(yp->yp_module));
	}
    out:
	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);
	break;

    case YP_SEQLIST:
	p1 = NULL;
	/* support for -h flag */
	if (yp->yp_varexp == NULL && type != NULL)
	    ferr(1, "tdec_typ:YP_SEQLIST:NULL varexp pointer\n");
	prcte(fp, type, t, f, yp, "SEQ_START");

	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);
	if (y = yp->yp_type) {
	    char *t1;

	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntdec_typ:SEQLIST can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tdec_typ:YP_SEQLIST:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;

	    if (!type || !noindirect(f))
		genmalloc(fp, yp);

	    if (optfield(y)) {
		(void) fprintf(fp,
			"\t{ OPTL, OFFSET(%s, optionals), 0, 0 },\n",
			yp->yp_structname);
	    }
	    tdec_loop(fp, y, id, yp->yp_structname);
	}
	(void) fprintf(fp, "\t{ PE_END, 0, 0, 0 },\n");
	break;

    case YP_SETLIST:
	p1 = NULL;
	/* support for -h flag */
	p1 = NULL;
	if (yp->yp_varexp == NULL && type != NULL)
	    ferr(1, "tdec_typ:YP_SETLIST:NULL varexp pointer\n");
	prcte(fp, type, t, f, yp, "SET_START");

	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);
	if (y = yp->yp_type) {
	    char *t1;

	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntdec_typ:SETLIST can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tdec_typ:YP_SETLIST:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;

	    if (!type || !noindirect(f))
		genmalloc(fp, yp);

	    if (optfield(y)) {
		(void) fprintf(fp,
			"\t{ OPTL, OFFSET(%s, optionals), 0, 0 },\n",
			yp->yp_structname);
	    }
	    tdec_loop(fp, y, id, yp->yp_structname);
	}
	(void) fprintf(fp, "\t{ PE_END, 0, 0, 0 },\n");
	break;

    case YP_SEQTYPE:		/* What is the difference ?? */
	p1 = NULL;
	prcte(fp, type, t, f, yp, "SEQOF_START");

	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);

	if (y = yp->yp_type) {
	    char *t1;

	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntdec_typ:SEQTYPE can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tdec_typ:YP_SEQTYPE:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;

	    if (!type || !noindirect(f))
		genmalloc(fp, yp);

	    tdec_loop(fp, y, id, yp->yp_structname);
	}
	if (yp->yp_flags & YP_CONTROLLED) {
	    char *f1;

	    if ((f1 = getfield(yp->yp_control)) == NULLCP) {
		fprintf(stderr, "\ntdec_typ:SEQ OF: can't extract field from %s\n",
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
	break;

    case YP_SETTYPE:
	p1 = NULL;
	prcte(fp, type, t, f, yp, "SETOF_START");

	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);

	if (y = yp->yp_type) {
	    char *t1;


	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntdec_typ:SETTYPE can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tdec_typ:YP_SETTYPE:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;

	    if (!type || !noindirect(f))
		genmalloc(fp, yp);

	    tdec_loop(fp, y, id, yp->yp_structname);
	}
	if (yp->yp_flags & YP_CONTROLLED) {
	    char *f1;

	    if ((f1 = getfield(yp->yp_control)) == NULLCP) {
		fprintf(stderr, "\ntdec_typ:SET OF: can't extract field from %s\n",
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
	break;

    case YP_CHOICE:
	p1 = NULL;
	/* support for -h flag */
	if (hflag && (y = yp->yp_type) && !y->yp_next) {
	    tdec_typ(fp, y, id, yp->yp_structname);
	    break;
	}
	if (type == NULL || type && noindirect(f))
	    prstfield(fp, "CHOICE_START", t, f, 0, c_class(yp));
	else
	    prtfield(fp, "CHOICE_START", t, type ? f : NULLCP, 0, c_class(yp));

	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);
	if (y = yp->yp_type) {
	    char *t1;

	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntdec_typ:CHOICE can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tdec_typ:YP_CHOICE:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;

	    if (!type || !noindirect(f))
		genmalloc(fp, yp);


	    if (yp -> yp_control_act && yp->yp_control_act->yal_dec) {
		(void) fprintf (fp, "\t{ SCTRL, %d, 0, FL_USELECT },\n",
				control_act(yp->yp_control_act -> yal_dec));
	    }
	    else if (yp->yp_flags & YP_CONTROLLED) {
		char *f1;

		if ((f1 = getfield(yp->yp_control)) == NULLCP) {
		    fprintf(stderr, "\ntdec_typ:CHOICE: can't extract field from %s\n",
			    yp->yp_control);
		    exit(1);
		}
		(void) fprintf(fp, "\t{ SCTRL, OFFSET(%s, %s), 0, 0 },\n",
			       yp->yp_structname, f1);
	    } else if (yp->yp_structname != NULL)
	        (void) fprintf(fp,"\t{ SCTRL, OFFSET(%s, offset), 0, 0 },\n",
			       yp->yp_structname);
	    else
		ferr(1, "\nCHOICE missing SCTRL\n");

	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
			    "\ntdec_typ:CHOICE can't extract direct type from %s\n",
			    yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tdec_typ:YP_CHOICE:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;
	    tdec_loop(fp, y, id, yp->yp_structname);
	}
	(void) fprintf(fp, "\t{ PE_END, 0, 0, 0 },\n");
	break;

    default:
	ferrd(1, "tdec_typ: yp_code = %d  not implemented\n", yp->yp_code);
    }

    if (p1 != NULL) {
	prnte(fp, t, f, yp, p1);

	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);
    }

    if (yp->yp_aft_alist && yp->yp_aft_alist->yal_dec)
	gen_act(fp, yp->yp_aft_alist->yal_dec);

}

static int fflags[] = {
    0, 1, 2, 2, 3, 3, 4, 5, 16, 16, 16, 17, 17, 17,
0, -1, 7,};

/*
 * generate tables for encoding a contructed type
 */
YP 
tdec_loop(fp, yp, id, type)
FILE	*fp;
YP      yp;
char   *id;
char   *type;
{
    for (; yp != NULL; yp = yp->yp_next) {
	tdec_typ(fp, yp, id, type);
    }
}
/*
 * Generate a malloc of for the given object
 */
genmalloc(fp, yp)
FILE	*fp;
YP      yp;
{
    if (hasdatstr(yp))
	(void) fprintf(fp, "\t{ MEMALLOC, 0, sizeof (%s), 0 },\n",
	    yp->yp_structname);
}
/*
 * Has Data Structure,
 * determine if this type needs a data structure allocated to it - calls
 * itself recursively to handle the cases of pulled up types
 * returns non zero if it does need a type allocated for it
 */
hasdatstr(yp)
YP      yp;
{
    YP	    y;
    YP	yp1;
    struct univ_typ	*p;

   switch (yp -> yp_code) {
 	case YP_BIT: 
	case YP_BITLIST: 
	case YP_SEQ: 
	case YP_SET: 
	case YP_ANY: 
	case YP_OCT: 
	case YP_OID: 
	    break;

	case YP_IDEFINED: 

	yp1 = lkup(yp);

	if (yp1->yp_code == YP_IDEFINED) {
	    if ((p = univtyp(yp1->yp_identifier)) == NULL
	      || p->univ_type <= YP_UNDF) {
		    return (1);	/* we can't tell unless we know what this is */
		    /* ferr(0, "\tcomptag:treated as implicit\n"); */
		}
		if (p->univ_flags & UNF_HASDATA)
		    return (1);
		return (0);
	} else
	    return (hasdatstr(yp1));

	case YP_SEQLIST: 
	case YP_SETLIST: 
	case YP_CHOICE: 
	    if (hflag && (y = yp -> yp_type) && !y -> yp_next) {
		return (hasdatstr(y));
	    }
	    /* else fall */

	default: 
	    return (1);
    }
    return (0);
}

int control_act (act)
Action act;
{
    register char *p;

    for (p = act -> a_data; *p; p++)
	if (!isspace (*p) && *p == '0')
	    return -1;
    return act -> a_num;
}

/*
 * generate an entry for the freeing routines
 * if there is a freeing function just call that
 * if not specify the type and offset so the free routines know how to free it
 * return non zero if we don't want the normal decoding entry to be
 * generated after us for freeing purposes.
 */
gen_freefn(fp, yp)
FILE	*fp;
YP	yp;
{
    char	*p1;
    char	*fn;

    if (yp->yp_yfn && (fn = yp->yp_yfn->yfn_fre)) {
	(void) fprintf(fp, "\t{ FFN_CALL, %d, 0, 0, },\n", addptr(fn));
	return (1);	/* don't free as per normal */
    }

    (void) fprintf(fp, "\t{ FREE_ONLY, 0, 0, 0, },\n");
    return (0);
    
}

