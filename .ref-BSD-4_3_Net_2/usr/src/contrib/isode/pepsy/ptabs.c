/* ptabs.c */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepsy/RCS/ptabs.c,v 7.8 91/02/22 09:49:58 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/pepsy/RCS/ptabs.c,v 7.8 91/02/22 09:49:58 mrose Interim $
 *
 *
 * $Log:	ptabs.c,v $
 * Revision 7.8  91/02/22  09:49:58  mrose
 * Interim 6.8
 * 
 * Revision 7.7  91/01/08  12:50:56  mrose
 * update
 * 
 * Revision 7.6  90/12/23  17:25:26  mrose
 * patches
 * 
 * Revision 7.5  90/12/11  10:41:09  mrose
 * sync
 * 
 * Revision 7.4  90/11/11  10:54:20  mrose
 * update
 * 
 * Revision 7.3  90/11/04  19:21:09  mrose
 * update
 * 
 * Revision 7.2  90/07/27  08:49:29  mrose
 * update
 * 
 * Revision 7.1  90/07/09  14:53:16  mrose
 * sync
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

extern FILE *fp;
extern char *c_tag(), *c_class();
extern char *ec_tag(), *ec_class(), *pec_class();
extern char *strip_last();
extern char *str_yp_code[];
extern char *get_val(), *get_comp(), *strp2name();
extern s_table *lookup_list(), *get_offset();
extern YP tprnt_loop();
extern int	gen_pentry();

extern char *concat();
extern char *my_strcat();
extern char	*rm_indirect();
extern char	*getfield();
extern char	*setfield();

/*
extern int explicit;
*/

static int cons_type = 0;

s_table *ptr;
s_table *save_ptr;

#define WORDSIZE	20

/*
 * Marshall's three extra conditions for changing the printing output
 */
static int	mrose1;	/* if NamedType */
static int	mrose2;	/* !mrose1 && -h && DefinedType */
static int	mrose3; /* (mrose1 || !mrose2) && TAG && (OPTIONAL|DEFAULT) */

/*
 * table printe a type. generate tables for the printing of a type
 */
tprnt_typ(fp, yp, id, type)
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
    int		flag;
    int		ptr_tblidx = -1;

    if (yp->yp_code < 0 || yp->yp_code > YP_REAL)
	ferrd(1, "tdec_typ: unimplemented type %d\n", yp->yp_code);

    if (yp == NULL) {
	ferr(0, "tprnt_typ:NULL arguement\n");
	return;
    }

    if (yp->yp_bef_alist && yp->yp_bef_alist->yal_prn)
	gen_act(fp, yp->yp_bef_alist->yal_prn);

    if (yp->yp_flags & YP_ID)
	mrose1 = 1;
    else
	mrose1 = 0;

    if (!mrose1 && hflag && yp->yp_code == YP_IDEFINED)
	mrose2 = 1;
    else
	mrose2 = 0;

    if ((mrose1 || !mrose2) && yp->yp_flags & YP_TAG
       && yp->yp_flags & (YP_OPTIONAL|YP_DEFAULT))
        mrose3 = 1;
    else
	mrose3 = 0;

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

    if ((yp->yp_flags & YP_PARMVAL) && yp->yp_parm) {
	if ((f = getfield(yp->yp_parm)) == NULLCP) {
	    fprintf(stderr, "\ntprnt_typ:can't extract field from %s\n",
		yp->yp_parm);
	    exit(1);
	}
	f = strdup(f);
    } else
	f = yp->yp_varexp;

    if ((yp->yp_flags & (YP_OPTIONAL|YP_OPTCONTROL|YP_DEFAULT))
	== (YP_OPTIONAL|YP_OPTCONTROL)) {
	yp->yp_flags &= ~YP_OPTCONTROL;
	flag = 1;
    } else
	flag = 0;

    if (yp->yp_flags & YP_TAG && !(yp->yp_flags & YP_IMPLICIT)) {
	prte_enoff(fp, "ETAG", yp, ptr_tblidx);
    }

    if (yp->yp_yfn && yp->yp_yfn->yfn_prt) {
	gen_fn(fp, yp, yp->yp_yfn->yfn_prt);
	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);

	if (yp->yp_aft_alist && yp->yp_aft_alist->yal_prn)
	    gen_act(fp, yp->yp_aft_alist->yal_prn);

	return;
    }

    switch (yp->yp_code) {

    case YP_UNDF:
	ferr(1, "tprnt_typ:Undefined type\n");

    case YP_BOOL:
	if (yp->yp_intexp)
	    f = setfield(yp->yp_intexp);
	if (noindirect(f))
	    ferr(1, "tdec_typ:BOOL: must specify a field [[ b .. ]]\n");
	p1 = "BOOLEAN";
	if (yp->yp_varexp || (yp->yp_intexp && !noindirect(f)))
	    break;
	ferr(1, "tdec_typ:BOOL: can't find a type for boolean\n");

	/* This needs to be fixed up in the action generating area */
    case YP_INTLIST:

    case YP_INT:

    case YP_ENUMLIST:
	if (yp->yp_intexp)
	    f = setfield(yp->yp_intexp);
	if (noindirect(f))
	    ferr(1, "tdec_typ:INT: must specify a field [[ i .. ]]\n");
	p1 = "INTEGER";
	if (yp->yp_varexp || (yp->yp_intexp && !noindirect(f)))
	    break;
	ferr(1, "tdec_typ:INT: couldn't determine type\n");

    case YP_REAL:
	if (yp->yp_strexp)
	    f = setfield(yp->yp_strexp);
	if (noindirect(f))
	    ferr(1, "tdec_typ:REAL: must specify a field [[ r .. ]]\n");
	p1 = "REALTYPE";
	if (yp->yp_varexp || (yp->yp_strexp && !noindirect(f)))
	    break;
	ferr(1, "tdec_typ:INT: couldn't determine type\n");


    case YP_BITLIST:
	ptr_tblidx = addptr (modsym(mymodule,
				    yp->yp_varexp ? yp -> yp_varexp : id,
				    "bits"));
	/* fall */
    case YP_BIT:
	if (yp->yp_strexp && yp->yp_intexp) {
	    if (yp->yp_strexp)
		f = setfield(yp->yp_strexp);
	    if (noindirect(f))
		ferr(1, "tdec_typ:BIT: must specify a field [[ x .. ]]\n");
	    p1 = "BITSTR_PTR";
	    prte_off(fp, p1, yp, t, f, ptr_tblidx);
	    if (yp->yp_intexp)
		f = setfield(yp->yp_intexp);
	    if (noindirect(f))
		ferr(1, "tdec_typ:BIT: must specify a field [[ x .. ]]\n");
	    p1 = "BITSTR_LEN";
	    break;
	}
	if (yp->yp_strexp == NULLCP && yp->yp_intexp)
	    f = setfield(yp->yp_intexp);
	if (yp->yp_varexp || (yp->yp_strexp && !noindirect(f))) {
	    p1 = "BITSTRING";
	    break;
	}
	t = NULL;
	p1 = NULL;
	(void) fprintf(fp, "\t{ SBITSTRING, %d, %s, %s, NULL },\n",
		       ptr_tblidx, c_tag(yp), c_class(yp));
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
		    ferr(1, "tdec_typ:OCT: must specify a field [[ s .. ]]\n");
		p1 = "T_STRING";
		break;
		
	    case 'o': /* [[ o ptr $ length ]] */
		if (yp->yp_strexp)
		    f = setfield(yp->yp_strexp);
		if (noindirect(f))
		    ferr(1, "tdec_typ:OCT: must specify a field [[ o .. ]]\n");
		p1 = "OCTET_PTR";
		prte_off(fp, p1, yp, t, f, ptr_tblidx);
		if (yp->yp_intexp)
		    f = setfield(yp->yp_intexp);
		if (noindirect(f))
		    ferr(1, "tdec_typ:OCT: must specify a field [[ o .. ]]\n");
		p1 = "OCTET_LEN";
		break;

	    default:
	       fprintf(stderr,"\ntprnt_typ:Unknown Octet string specifier %c\n",
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
	prte_noff(fp, "SOCTETSTRING", yp, ptr_tblidx);
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
	prte_noff(fp, "SOBJID", yp, ptr_tblidx);
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
	prte_noff(fp, "SANY", yp, ptr_tblidx);
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
	    gen_identry(fp, t, f, yp, gen_pentry);

	    if (yp->yp_flags & YP_DEFAULT)
		gdflt(fp, yp, G_DEC);

	    break;
	}

	pr_deftyp(fp, yp, t, f);
	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);
	break;

    case YP_SEQLIST:
	p1 = NULL;
	/* support for -h flag */
	cons_type++;
	save_ptr = ptr;
	if (yp->yp_varexp == NULL && type != NULL)
	    ferr(1, "tprnt_typ:YP_SEQLIST:NULL varexp pointer\n");
	if (type == NULL)
	    prte_noff(fp, "SSEQ_START", yp, ptr_tblidx);
	else if (noindirect(f))
	    prte_noff(fp, "SSEQ_START", yp, ptr_tblidx);
	else
	    prte_off(fp, "SEQ_START", yp, t, f, ptr_tblidx);

	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);
	if (y = yp->yp_type) {
	    char *t1;

	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntprnt_typ:SEQLIST can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tprnt_typ:YP_SEQLIST:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;
#ifdef DO_OFFSETS
	    if (optfield(y)) {
		(void) fprintf(fp,
		"\t{ OPTL, OFFSET(%s, optionals), 0, 0, NULL },\n",
				    yp->yp_structname);
	    }
#endif
	    tprnt_loop(fp, y, id, yp->yp_structname);
	}
	(void) fprintf(fp, "\t{ PE_END, 0, 0, 0, NULL },\n");
	ptr = save_ptr;
	cons_type--;
	break;

    case YP_SETLIST:
	p1 = NULL;
	/* support for -h flag */
	cons_type++;
	if (yp->yp_varexp == NULL && type != NULL)
	    ferr(1, "tprnt_typ:YP_SETLIST:NULL varexp pointer\n");
	if (type == NULL)
	    prte_noff(fp, "SSET_START", yp, ptr_tblidx);
	else if (noindirect(f))
	    prte_noff(fp, "SSET_START", yp, ptr_tblidx);
	else
	    prte_off(fp, "SET_START", yp, t, f, ptr_tblidx);

	if (yp->yp_flags & YP_DEFAULT)
	    ddflt(fp, yp);
	if (y = yp->yp_type) {
	    char *t1;

	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntprnt_typ:SETLIST can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tprnt_typ:YP_SETLIST:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;
#ifdef DO_OFFSETS
	    if (optfield(y)) {
		(void) fprintf(fp,
		    "\t{ OPTL, OFFSET(%s, optionals), 0, 0, NULL },\n",
					yp->yp_structname);
	    }
#endif
	    tprnt_loop(fp, y, id, yp->yp_structname);
	}
	(void) fprintf(fp, "\t{ PE_END, 0, 0, 0, NULL },\n");
	ptr = save_ptr;
	cons_type--;
	break;

    case YP_SEQTYPE:		/* What is the difference ?? */
	p1 = NULL;
	cons_type++;
	save_ptr = ptr;
	if (type == NULL)
	    prte_noff(fp, "SSEQOF_START", yp, ptr_tblidx);
	else if (noindirect(f))
	    prte_noff(fp, "SSEQOF_START", yp, ptr_tblidx);
	else
	    prte_off(fp, "SEQOF_START", yp, t, f, ptr_tblidx);
	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);

	if (y = yp->yp_type) {
	    char *t1;

	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntprnt_typ:SEQTYPE can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tprnt_typ:YP_SEQTYPE:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;
	    tprnt_loop(fp, y, id, yp->yp_structname);
	}
#ifdef DO_OFFSETS
	if (yp->yp_flags & YP_CONTROLLED) {
	    char *f1;

	    if ((f1 = getfield(yp->yp_control)) == NULLCP) {
		fprintf(stderr, "\ntprnt_typ:SEQ OF: can't extract field from %s\n",
		    yp->yp_control);
		exit(1);
	    }
	    (void) fprintf(fp, "\t{ PE_END, OFFSET(%s, %s), 0, 0 },\n",
		    yp->yp_structname, f1);
	} else if (yp->yp_structname != NULL)
	    (void) fprintf(fp,
		"\t{ PE_END, OFFSET(%s, next), 0, 0, NULL },\n",
		    yp->yp_structname);
	else
#endif
	    (void) fprintf(fp, "\t{ PE_END, 0, 0, 0, NULL },\n");
	ptr = save_ptr;
	cons_type--;
	break;

    case YP_SETTYPE:
	p1 = NULL;
	cons_type++;
	save_ptr = ptr;
	if (type == NULL)
	    prte_noff(fp, "SSETOF_START", yp, ptr_tblidx);
	else if (noindirect(f))
	    prte_noff(fp, "SSETOF_START", yp, ptr_tblidx);
	else
	    prte_off(fp, "SETOF_START", yp, t, f, ptr_tblidx);

	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);

	if (y = yp->yp_type) {
	    char *t1;

	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
		    "\ntprnt_typ:SETTYPE can't extract direct type from %s\n",
			yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tprnt_typ:YP_SETTYPE:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;
	    tprnt_loop(fp, y, id, yp->yp_structname);
	}
	if (yp->yp_flags & YP_CONTROLLED) {
	    char *f1;

	    if ((f1 = getfield(yp->yp_control)) == NULLCP) {
		fprintf(stderr, "\ntprnt_typ:SET OF: can't extract field from %s\n",
		    yp->yp_control);
		exit(1);
	    }
	    (void) fprintf(fp, "\t{ PE_END, OFFSET(%s, %s), 0, 0 },\n",
		    yp->yp_structname, f1);
	} else if (yp->yp_structname != NULL)
	    (void) fprintf(fp,
		"\t{ PE_END, OFFSET(%s, next), 0, 0, NULL },\n",
		    yp->yp_structname);
	else
	    (void) fprintf(fp, "\t{ PE_END, 0, 0, 0, NULL },\n");
	ptr = save_ptr;
	cons_type--;
	break;

    case YP_CHOICE:
	p1 = NULL;
	/* support for -h flag */
	if (hflag && (y = yp->yp_type) && !y->yp_next) {
	    tprnt_typ(fp, y, id, yp->yp_structname);
	    break;
	}
	cons_type++;
	save_ptr = ptr;
	/* Generates an unused tags field - so beware */
	if (type == NULL)
	    prte_noff(fp, "SCHOICE_START", yp, ptr_tblidx);
	else if (noindirect(f))
	    prte_noff(fp, "SCHOICE_START", yp, ptr_tblidx);
	else
	    prte_off(fp, "CHOICE_START", yp, t, f, ptr_tblidx);

	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);
	if (y = yp->yp_type) {
	    char *t1;
	    char *f1;

#ifdef DO_OFFSETS
	    if (yp->yp_flags & YP_CONTROLLED) {

		if ((f1 = getfield(yp->yp_control)) == NULLCP) {
		    fprintf(stderr,
			    "\ntprnt_typ:CHOICE: can't extract field from %s\n",
			    yp->yp_control);
		    exit(1);
		}

	    } else
		f1 = "offset";

	    if ((yp->yp_flags & YP_ID) && yp->yp_id)
		(void) fprintf(fp,
			       "\t{ SCTRL, OFFSET(%s, %s), 0, 0, \"%s\" },\n",
			       yp->yp_structname, f1, yp->yp_id);
	    else
		(void) fprintf(fp,
			       "\t{ SCTRL, OFFSET(%s, %s), 0, 0, NULL },\n",
			       yp->yp_structname, f1);
#else
	    if ((yp->yp_flags & YP_ID) && yp->yp_id)
		(void) fprintf(fp,"\t{ SCTRL, 0, 0, 0, \"%s\" },\n",
			       yp->yp_id);
	    else
		(void) fprintf(fp, "\t{ SCTRL, 0, 0, 0, NULL },\n");

#endif
	    if (yp->yp_param_type) {
		/* we have a [[ P type ]] specification */
		if ((t1 = rm_indirect(yp->yp_param_type)) == NULLCP) {
		    fprintf(stderr,
			    "\ntprnt_typ:CHOICE can't extract direct type from %s\n",
			    yp->yp_param_type);
		    exit(1);
		}
		yp->yp_structname = strdup(t1);
	    } else if (type) {
		if (yp->yp_declexp == NULL)
		    ferr(1, "tprnt_typ:YP_CHOICE:no declexp\n");
		yp->yp_structname = my_strcat("struct ", yp->yp_declexp);
	    } else
		yp->yp_structname = t;
	    tprnt_loop(fp, y, id, yp->yp_structname);
	}
	(void) fprintf(fp, "\t{ PE_END, 0, 0, 0, NULL },\n");
	ptr = save_ptr;
	cons_type--;
	break;

    default:
	ferrd(1, "tprnt_typ: yp_code = %d  not implemented\n", yp->yp_code);
    }

    if (p1 != NULL) {
	if (t != NULL)
	    prte_off(fp, p1, yp, t, f, ptr_tblidx);
	else
	    prte_noff(fp, p1, yp, ptr_tblidx);

	if (yp->yp_flags & YP_DEFAULT)
	    gdflt(fp, yp, G_DEC);
    }

    if (flag)
	yp->yp_flags |= YP_OPTCONTROL;

    if (yp->yp_aft_alist && yp->yp_aft_alist->yal_prn)
	gen_act(fp, yp->yp_aft_alist->yal_prn);

}

static int fflags[] = {
    0, 1, 2, 2, 3, 3, 4, 5, 16, 16, 16, 17, 17, 17,
0, -1, 7,};

/*
 * generate tables for printing a contructed type
 */
YP 
tprnt_loop(fp, yp, id, type)
FILE	*fp;
YP      yp;
char   *id;
char   *type;
{
    for (; yp != NULL; yp = yp->yp_next) {
	tprnt_typ(fp, yp, id, type);
    }
}


ddflt(fp, yp)
FILE	*fp;
YP      yp;
{
    switch (yp->yp_code) {
    case YP_BOOL:
    case YP_INT:
    case YP_INTLIST:
	(void) fprintf(fp, "\t{DFLT_B,      %d,     0,      0 },\n",
		yp->yp_default->yv_number);
	break;
    case YP_BIT:
    case YP_BITLIST:
    case YP_OCT:
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
    case YP_IDEFINED:
    case YP_ENUMLIST:
    case YP_REAL:
	(void) fprintf(fp, "\t{DFLT_B,      0,      0,      0 },\n");
	break;

    default:
	ferr(1, "ddflt:unknown type %d\n", yp->yp_code);
    }

}

/*
 * print a Non offset table entry
 */
prte_noff(fp, type, yp, idx)
FILE	*fp;
char	*type;
YP	yp;
int	idx;
{
    char	*tag;
    char	*flags;
    char	*typename;
    char	buf1[BUFSIZ];

    tag = c_tag(yp);
    flags = c_class(yp);
    if (mrose3) {		/* need to append FL_PRTAG flag */
	strncpy(buf1, flags, BUFSIZ);
	strncat(buf1, "|FL_PRTAG", BUFSIZ);
	flags = buf1;
    }
    if (mrose1)
	typename = yp->yp_id;
    else if (mrose2)
	typename = yp->yp_identifier;
    else
	typename = (char *)0;
    if (typename)
	(void) fprintf(fp, "\t{ %s, %d, %s, %s, \"%s\" },\n",
		       type, idx, tag, flags, typename);
    else
	(void) fprintf(fp, "\t{ %s, %d, %s, %s, NULL},\n",
		       type,idx, tag, flags);
}

/*
 * print a Non offset table entry for an ETAG - special case
 */
prte_enoff(fp, type, yp, idx)
FILE	*fp;
char	*type;
YP	yp;
int	idx;
{
    char	*tag;
    char	*flags;
    char	*typename;
    char	buf1[BUFSIZ];

    tag = ec_tag(yp);
    flags = ec_class(yp);
    if (mrose3) {		/* need to append FL_PRTAG flag */
	strncpy(buf1, flags, BUFSIZ);
	strncat(buf1, "|FL_PRTAG", BUFSIZ);
	flags = buf1;
	mrose3 = 0;	/* don't want the next tag */
    }
    if (mrose1) {
	typename = yp->yp_id;
	mrose1 = 0;
    } else if (mrose2) {
	typename = yp->yp_identifier;
	mrose2 = 0;
    } else
	typename = NULL;
    if (typename)
	(void) fprintf(fp, "\t{ %s, %d, %s, %s, \"%s\" },\n",
				    type, idx, tag, flags, typename);
    else
	(void) fprintf(fp, "\t{ %s, %d, %s, %s, NULL },\n",
				    type, idx, tag, flags);
}

/*
 * print an offset table entry
 */
prte_off(fp, type, yp, t, f, idx)
FILE	*fp;
char	*type;
YP	yp;
char	*t, *f;
int	idx;
{
    char	*tag;
    char	*flags;
    char	*typename;
    char	buf1[BUFSIZ];

    tag = c_tag(yp);
    flags = c_class(yp);
    if (mrose3) {		/* need to append FL_PRTAG flag */
	strncpy(buf1, flags, BUFSIZ);
	strncat(buf1, "|FL_PRTAG", BUFSIZ);
	flags = buf1;
    }
    if (mrose1)
	typename = yp->yp_id;
    else if (mrose2)
	typename = yp->yp_identifier;
    else
	typename = (char *)0;
#ifdef DO_OFFSETS
    if (typename)
	(void) fprintf(fp, "\t{ %s, OFFSET(%s, %s), %s, %s, \"%s\" },\n",
				    type, 	t, f, 	tag, flags, typename);
    else
	(void) fprintf(fp, "\t{ %s, OFFSET(%s, %s), %s, %s, NULL},\n",
				    type, 	t, f,	 tag, flags);
#else
    if (typename)
	(void) fprintf(fp, "\t{ %s, %d, %s, %s, \"%s\" },\n",
		       type, idx, tag, flags, typename);
    else
	(void) fprintf(fp, "\t{ %s, %d, %s, %s, NULL},\n",
		       type, idx, tag, flags);
#endif
}

/*
 * handle the very complex task of defined types.
 * Basically generating object calls
 */
pr_deftyp(fp, yp, t, f)
FILE	*fp;
YP	yp;
char	*t;
char	*f;
{
    /* Predefined Universal Type */
    struct univ_typ *p, *univtyp();

    if ((p = univtyp(yp->yp_identifier))) {
	if (p->univ_flags & UNF_EXTMOD) {
	    yp->yp_module = p->univ_mod;
	    goto do_obj;
	}
	prte_univt(fp, p, yp, t, f);
	return;
    }

do_obj:
    if (yp->yp_flags & YP_TAG && yp->yp_flags & YP_IMPLICIT)
	prte_noff(fp, "IMP_OBJ", yp, -1);
	prte_obj(fp, yp, t, f);
}

/*
 * print an offset table entry for an OBJECT type entry
 */
prte_obj(fp, yp, t, f)
FILE	*fp;
YP	yp;
char	*t, *f;
{
    char	*type;
    char	*obj;
    char	*flags;
    char	*typename;
    char	*off;
    char	buf1[BUFSIZ];
    char	buf2[BUFSIZ];
    int		extflag;

    if (yp->yp_module == NULL || strcmp(yp->yp_module, mymodule) == 0) {
	if (f && !noindirect(f))
	    type = "OBJECT";
	else
	    type = "SOBJECT";
	obj = proc_name(yp->yp_identifier, 0);
	extflag = 0;
    } else {
	if (f && !noindirect(f))
	    type = "EXTOBJ";
	else
	    type = "SEXTOBJ";
	obj = strp2name(yp->yp_identifier, yp->yp_module);
	extflag = 1;
    }
    flags = c_class(yp);
    if (mrose3) {		/* need to append FL_PRTAG flag */
	strncpy(buf1, flags, BUFSIZ);
	strncat(buf1, "|FL_PRTAG", BUFSIZ);
	flags = buf1;
    }
    if (mrose1)
	typename = yp->yp_id;
    else if (mrose2)
	typename = yp->yp_identifier;
    else
	typename = (char *)0;
#ifdef DO_OFFSETS
    if (f && !noindirect(f)) {
	sprintf(buf2, "OFFSET(%s, %s)", t, f);
	off = buf2;
    } else if (f && *f == '&') {
	sprintf(buf2, "OFFSET(%s, %s)", t, f + 1);
	off = buf2;
    } else
#endif
        off = "0";
    if (typename)
	(void) fprintf(fp, "\t{ %s, %s, _Z%s, %s, \"%s\" },\n",
				    type, off, obj, flags, typename);
    else
	(void) fprintf(fp, "\t{ %s, %s, _Z%s, %s, NULL},\n",
				    type, off, obj, flags);
    if (extflag)
	(void) fprintf(fp, "\t{ EXTMOD, %d, 0, 0, NULL },\n",
		gen_modref(yp->yp_module));
}

/*
 * print an table entry for Universal type with the given entry
 */
prte_univt(fp, p, yp, t, f)
FILE	*fp;
struct univ_typ *p;
YP	yp;
char	*t, *f;
{
    char	*type;
    int		tag;
    int		class;
    char	*flags;
    char	*typename;
    char	*off;
    char	buf1[BUFSIZ];
    char	buf2[BUFSIZ];
    char	buf3[BUFSIZ];

    if (f == NULL || noindirect(f))  {
	sprintf(buf3, "S%s", p->univ_tab);
	type = buf3;
    } else
	type = p->univ_tab;

    if (yp->yp_flags & YP_TAG && yp->yp_flags & YP_IMPLICIT) {
	tag = yp->yp_tag->yt_value->yv_number;
	class = yp->yp_tag->yt_class;
    } else {
	tag = p->univ_id;
	class = p->univ_class;
    }
    
    strncpy(buf1, c_flags(yp, class), BUFSIZ);
    flags = buf1;
    if (mrose3) {		/* need to append FL_PRTAG flag */
	strncat(buf1, "|FL_PRTAG", BUFSIZ);
    }
    if (mrose1)
	typename = yp->yp_id;
    else if (mrose2)
	typename = yp->yp_identifier;
    else
	typename = (char *)0;
#ifdef DO_OFFSETS
    if (f && !noindirect(f)) {
	sprintf(buf2, "OFFSET(%s, %s)", t, f);
	off = buf2;
    } else if (f && *f == '&') {
	sprintf(buf2, "OFFSET(%s, %s)", t, f + 1);
	off = buf2;
    } else
#endif
        off = "0";
    if (typename)
	(void) fprintf(fp, "\t{ %s, %s, %d, %s, \"%s\" },\n",
				    type, off, tag, flags, typename);
    else
	(void) fprintf(fp, "\t{ %s, %s, %d, %s, NULL},\n",
				    type, off, tag, flags);
}

/*
 * generate the table entry for a value passing defined type which
 * is equivalent to the given primative type
 */
gen_pentry(fp, oyp, yp, t, f)
FILE	*fp;
YP	oyp, yp;
char	*t, *f;
{
    char	*p1;
    register char	s = oyp->yp_prfexp;	/* type of value passing */
    int		idx;


    if (noindirect(f) && s != 'q' && s != 'a')
	ferrs(1,
        "gen_ventry: must specify a field for primative value- not %s\n", f);

#if 0
/* can't get id from in here - yet */
    if (yp->yp_code == YP_BITLIST) {
	idx = addptr (modsym(mymodule, yp->yp_varexp ? yp -> yp_varexp : id,
                                    "bits"));
    } else
#endif
        idx = -1;

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
	prte_off(fp, p1, yp, t, f, idx);
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
	prte_off(fp, p1, yp, t, f, idx);
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

    prte_off(fp, p1, yp, t, f, idx);
}
