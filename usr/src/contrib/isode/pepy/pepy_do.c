/* pepy_do.c - PE parser (yacc-based) building routines */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepy/RCS/pepy_do.c,v 7.3 91/02/22 09:35:04 mrose Interim $";
#endif

/*
 * $Header: /f/osi/pepy/RCS/pepy_do.c,v 7.3 91/02/22 09:35:04 mrose Interim $
 *
 *
 * $Log:	pepy_do.c,v $
 * Revision 7.3  91/02/22  09:35:04  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/29  18:38:20  mrose
 * updates
 * 
 * Revision 7.1  90/10/23  20:42:42  mrose
 * update
 * 
 * Revision 7.0  89/11/23  22:11:49  mrose
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

#include "pepy.h"
#include <ctype.h>
#include <stdio.h>

extern struct tuple tuples[];
extern int	rflag;

char   *gensym (), *modsym ();

YP	lookup_type (), lookup_binding ();
YT	lookup_tag ();
char	*add_point ();

/*  */

do_type (yp, level, id, arg)
register YP	yp;
register int	level;
register char  *id,
	       *arg;
{
    register int    i;
    register char  *narg;
    char   *narg2,
	   *narg3;
    register struct tuple *t;
    register YP     y;
    register YV     yv;
    char	   *class, *value, *form;
    char	    tbuf1[32], tbuf2[32];
    int		    pushdown = 0;

    if (yp -> yp_flags & YP_COMPONENTS) {
	warning ("I shouldn't be here!");
	return;
    }

    if (level == 1) {
	printf ("(pe, explicit, len, buffer, parm)\n");
	printf ("%sPE     *pe;\nint\texplicit;\n",
		yp -> yp_code != YP_ANY
		    && yp -> yp_code != YP_NULL
		    && (yp -> yp_code != YP_CHOICE
			|| (yp -> yp_flags & YP_CONTROLLED))
		? "register " : "");
	printf ("int\tlen;\nchar   *buffer;\n%s parm;\n{\n",
		yp -> yp_param_type ? yp -> yp_param_type : "PEPYPARM");

	if (yp -> yp_action0) {
	    if (!Pflag && *sysin)
		printf ("# line %d \"%s\"\n", yp -> yp_act0_lineno, sysin);
	    printf ("%*s%s\n", level * 4, "", yp -> yp_action0);
	}
    }

    pushdown = (yp -> yp_flags & (YP_TAG | YP_IMPLICIT)) == YP_TAG;
    
    for (t = tuples; t -> t_type != YP_UNDF; t++)
	if (t -> t_type == yp -> yp_code) {
	    class = t -> t_class;
	    value = t -> t_id;
	    if((form = t -> t_form) == NULL)
		form = "PE_FORM_PRIM";
	    break;
	}
    if ((yp -> yp_flags & (YP_TAG | YP_IMPLICIT)) == (YP_TAG | YP_IMPLICIT)) {
	(void) sprintf (tbuf2, "PE_CLASS_%s",
		    pe_classlist[yp -> yp_tag -> yt_class]);
	class = tbuf2;
	(void) sprintf (tbuf1, "%d",val2int (yp -> yp_tag -> yt_value));
	value = tbuf1;
    }

    switch (yp -> yp_code) {
	case YP_BOOL:
	    printf ("%*sregister int %s = %s;\n\n", level * 4, "",
		    narg = gensym (), yp -> yp_intexp ? yp -> yp_intexp
					    : level == 1 ? "len" : "0");
	    break;
	case YP_INT:
	case YP_INTLIST:
    	case YP_ENUMLIST:
	    printf ("%*sregister integer %s = %s;\n\n", level * 4, "",
		    narg = gensym (), yp -> yp_intexp ? yp -> yp_intexp
					    : level == 1 ? "len" : "0");
	    break;

	case YP_REAL:
	    printf ("%*sregister double %s = 0.0;\n\n", level * 4, "",
		    narg = gensym ());
	    if (yp -> yp_strexp)
		    printf ("%*s%s = %s;\n", level * 4, "", narg,
			    yp -> yp_strexp);
	    break;

	case YP_BIT:
	case YP_BITLIST:
	    printf ("%*sPE\t%s_z = NULLPE;\n", level * 4, "",
		    narg = gensym ());
	    printf ("%*sregister PE *%s = &%s_z;\n\n", level * 4, "",
		    narg, narg);
	    narg = add_point (narg);
	    printf ("%*schar *%s;\n%*sint %s;\n", level * 4, "",
		    narg2 = gensym (), level * 4, "", narg3 = gensym ());

	    if (yp -> yp_strexp)
		printf ("%*s%s = %s;\n%*s%s = %s;\n", level * 4, "",
		    narg2, yp -> yp_strexp, level * 4, "", narg3,
		    yp -> yp_intexp);
	    else
	    if (level == 1)
		printf ("%*s%s = buffer;\n%*s%s = len;\n", level * 4, "",
		    narg2, level * 4, "", narg3);
	    else
		printf ("%*s%s = NULLCP;\n%*s%s = 0;\n", level * 4, "",
		    narg2, level * 4, "", narg3);
	    printf ("%*s%s = %s ? strb2bitstr (%s, %s, %s, %s) : NULLPE;\n",
		level * 4, "", narg, narg2, narg2, narg3, class, value);
	    break;

	case YP_OCT:
	    narg = gensym ();
 	    if (yp -> yp_prfexp != 'q') {
 		printf ("%*sregister char *%s;\n%*sint %s_len;\n\n",
 			level * 4, "", narg, level * 4, "", narg);
 		if (yp -> yp_strexp) {
 		    printf ("%*s%s = %s;\n", level * 4, "",
 			    narg, yp -> yp_strexp);
 		    if (yp -> yp_intexp)
 			printf ("%*s%s_len = %s;\n", level * 4, "",
 				narg, yp -> yp_intexp);
 		    else
 			printf ("%*s%s_len = strlen (%s);\n", level * 4, "",
 				narg, narg);
 		}
  		else
 		    if (level == 1) {
 			printf ("%*s%s = buffer;\n", level * 4, "", narg);
 			printf ("%*sif ((%s_len = len) == 0)\n", level * 4, "",
 				narg);
 			printf ("%*s%s_len = strlen (%s);\n",
 				(level + 1) * 4, "", narg, narg);
 		    }
 		    else
 			printf ("%*s%s = NULLCP;\n%*s%s_len = 0;\n",
 				level * 4, "", narg, level * 4, "", narg);
  	    }
 	    else {
 		printf ("%*sregister struct qbuf *%s;\n\n",
 			level * 4, "", narg);
 		printf ("%*s%s = %s;\n", level * 4, "", narg, yp -> yp_strexp);
 	    }
	    break;

	case YP_SEQ:
	case YP_SET:
	case YP_ANY:
	    if (yp -> yp_strexp) {
		printf ("%*sPE\t%s = %s;\n\n", level * 4, "",
			narg = gensym (), yp -> yp_strexp);
		break;
	    }
	    /* else fall */
	case YP_NULL:
	case YP_IDEFINED:
	    narg = NULL;
	    break;

	case YP_CHOICE:
	    if (yp -> yp_type && yp -> yp_control)
		printf ("%*sint\t%s;\n\n", level * 4, "", narg2 = gensym ());
	    narg = NULL;
	    break;

	case YP_OID:
	    printf ("%*sregister OID %s;\n\n", level * 4, "",
		    narg = gensym ());
	    if (yp -> yp_strexp)
		printf ("%*s%s = %s;\n", level * 4, "", narg, yp -> yp_strexp);
	    else if (level == 1)
		printf ("%*s%s = buffer ? str2oid (buffer) : NULLOID;\n",
		    level * 4, "", narg);
	    else
		printf ("%*s%s = NULLOID;\n", level * 4, "", narg);
	    break;

	case YP_SEQTYPE:
	case YP_SETTYPE:
	    printf ("%*sPE\t%s = NULLPE;\n", level * 4, "",
		    narg2 = gensym ());
	    /* and fall ... */
	case YP_SEQLIST:
	case YP_SETLIST:
	    printf ("%*sPE\t%s_z = NULLPE;\n", level * 4, "",
		    narg = gensym ());
	    printf ("%*sregister PE *%s = &%s_z;\n\n", level * 4, "",
		    narg, narg);
	    narg = add_point(narg);
	    break;

	default:
	    myyerror ("unknown type: %d", yp -> yp_code);
    }

    switch (yp -> yp_code) {
	case YP_SEQ:
	case YP_SET:
	    if (yp -> yp_strexp)
		break;
	    /* else fall */
	case YP_SEQTYPE:
	case YP_SETTYPE:
	case YP_SEQLIST:
	case YP_SETLIST:
	case YP_NULL:
	    printf ("%*sif ((%s = pe_alloc (%s, %s, %s)) == NULLPE) {\n",
		    level * 4, "", arg, class, form, value);
	    printf ("%*sadvise (NULLCP, \"%s: %%s\", PEPY_ERR_NOMEM);\n",
		    (level + 1) * 4, "", id);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
	    break;
    }

    if (!dflag && yp -> yp_action05)
	do_action (yp -> yp_action05, level, narg ? narg : arg,
		yp -> yp_act05_lineno);
    if (!dflag && yp -> yp_action1)
	do_action (yp -> yp_action1, level, narg ? narg : arg,
		yp -> yp_act1_lineno);

    switch (yp -> yp_code) {
	case YP_BOOL:
	    printf ("%*sif ((%s = flag2prim (%s, %s, ",
		    level * 4, "", arg, narg, class);
	    printf ("%s)) == NULLPE) {\n", value);
	    printf ("%*sadvise (NULLCP, \"%s: %%s\", PEPY_ERR_NOMEM);\n",
		    (level + 1) * 4, "", id);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
	    break;

	case YP_INT:
	case YP_INTLIST:
	case YP_ENUMLIST:
	    printf ("%*sif ((%s = %snum2prim (%s, %s, ",
		    level * 4, "", arg,
		    yp->yp_code == YP_ENUMLIST ? "e" : "",
		    narg, class);
	    printf ("%s)) == NULLPE) {\n", value);
	    printf ("%*sadvise (NULLCP, \"%s: %%s\", PEPY_ERR_NOMEM);\n",
		    (level + 1) * 4, "", id);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
	    if (yp -> yp_code == YP_INT)
		break;
	    uniqint (yp -> yp_value);
	    printf ("%*sswitch (%s) {\n", level * 4, "", narg);
	    for (yv = yp -> yp_value; yv; yv = yv -> yv_next) {
		printf ("%*scase %d:", (level + 1) * 4, "", val2int (yv));
		if (yv -> yv_flags & YV_NAMED)
		    printf ("\t/* %s */", yv -> yv_named);
		printf ("\n");
		if (!dflag && yv -> yv_action)
		    do_action (yv -> yv_action, level + 2, narg,
				yv -> yv_act_lineno);
		printf ("%*sbreak;\n", (level + 2) * 4, "");
	    }
	    if (!rflag && yp -> yp_code == YP_ENUMLIST) {
		printf ("%*sdefault:\n", (level + 1) * 4, "");
		printf ("%*sadvise (NULLCP, \"%s %%s%%d\", PEPY_ERR_UNK_COMP, %s);\n",
			(level + 2) * 4, "", id, narg);
		printf ("%*sreturn NOTOK;\n", (level + 2) * 4, "");
	    }
	    printf ("%*s}\n", level * 4, "");
	    break;

	case YP_REAL:
	    printf ("%*sif ((%s = real2prim (%s, %s, ",
		    level * 4, "", arg, narg, class);
	    printf ("%s)) == NULLPE) {\n", value);
	    printf ("%*sadvise (NULLCP, \"%s: %%s\", PEPY_ERR_NOMEM);\n",
		    (level + 1) * 4, "", id);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
	    break;

	case YP_BIT:
	case YP_BITLIST:
	    printf ("%*sif (%s == NULLPE) {\n", level * 4, "", narg);
	    printf ("%*sadvise (NULLCP, \"%s %%s\", PEPY_ERR_INIT_FAILED);\n",
		    (level + 1) * 4, "", id);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
	    if (!yp -> yp_strexp && level != 1)
		printf ("%*s%s -> pe_class = %s;\n%*s%s -> pe_id = %s;\n",
		    level * 4, "", narg, class, level * 4, "", narg, value);
	    if (yp -> yp_code == YP_BITLIST) {
		register int	j;

		for (yv = yp -> yp_value, i = -1; yv; yv = yv -> yv_next)
		    if ((j = val2int (yv)) > i)
			i = j;
		if (i >= 0)
		    printf ("%*sif (bit_test (%s, %d) == NOTOK)\n%*s(void) bit_off (%s, %d);\n",
			    level * 4, "", narg, i,
			    (level + 1) * 4, "", narg, i);
	    }
	    printf ("%*sif ((%s = bit2prim (%s)) == NULLPE) {\n",
		    level * 4, "", arg, narg);
	    printf ("%*sadvise (NULLCP, \"%s: %%s\", PEPY_ERR_NOMEM);\n",
		    (level + 1) * 4, "", id);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
	    if (yp -> yp_code == YP_BIT)
		break;
	    printf ("#define\tBITS\t\"\\020");
	    for (yv = yp -> yp_value; yv; yv = yv -> yv_next)
		if (yv -> yv_flags & YV_NAMED)
		    printf ("\\0%o%s", val2int (yv) + 1, yv -> yv_named);
		else
		    printf ("\\0%oBIT%d", val2int (yv) + 1, val2int (yv));
	    printf ("\"\n");
	    uniqint (yp -> yp_value);
	    if (!dflag)
		for (yv = yp -> yp_value; yv; yv = yv -> yv_next) {
		    if (!yv -> yv_action)
			continue;
		    printf ("%*sif (bit_test (%s, %d) > OK) {",
			    level * 4, "", narg, val2int (yv));
		    if (yv -> yv_flags & YV_NAMED)
			printf ("\t/* %s */", yv -> yv_named);
		    printf ("\n");
		    do_action (yv -> yv_action, level + 1, narg,
			       yv -> yv_act_lineno);
		    printf ("%*s}\n", level * 4, "");
		}
	    break;

	case YP_OCT:
	    printf ("%*sif (%s == %s) {\n",
		    level * 4, "", narg, 
		    yp -> yp_prfexp != 'q' ? "NULLCP" : "((struct qbuf *) 0)");
	    printf ("%*sadvise (NULLCP, \"%s %%s\", PEPY_ERR_INIT_FAILED);\n",
		    (level + 1) * 4, "", id);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
	    printf ("%*sif ((%s = ", level * 4, "", arg);
	    if (yp -> yp_prfexp != 'q')
		printf ("str2prim (%s, %s_len,", narg, narg);
	    else
		printf ("qb2prim (%s,", narg);
	    printf (" %s, %s)) == NULLPE) {\n", class, value);
	    printf ("%*sadvise (NULLCP, \"%s: %%s\", PEPY_ERR_NOMEM);\n",
		    (level + 1) * 4, "", id);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
	    break;

	case YP_NULL:
	    break;

	case YP_ANY:
	case YP_SEQ:
	case YP_SET:
	    if (!yp -> yp_strexp)
		break;
	    printf ("%*sif (%s == NULLPE) {\n", level * 4, "", narg);
	    printf ("%*sadvise (NULLCP, \"%s %%s\", PEPY_ERR_INIT_FAILED);\n",
		    (level + 1) * 4, "", id);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
#ifdef	notdef
	    printf ("%*sif ((%s = pe_cpy (%s)) == NULLPE) {\n",
		    level * 4, "", arg, narg);
	    printf ("%*sadvise (NULLCP, \"%s: %%s\", PEPY_ERR_NOMEM);\n",
		    (level + 1) * 4, "", id);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
#else
	    printf ("%*s(%s = %s) -> pe_refcnt++;\n",
		    level * 4, "", arg, narg);
#endif
	    break;

	case YP_OID:
	    printf ("%*sif (%s == NULLOID) {\n", level * 4, "", narg);
	    printf ("%*sadvise (NULLCP, \"%s %%s\", PEPY_ERR_INIT_FAILED);\n",
		    (level + 1) * 4, "", id);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
	    printf ("%*sif ((%s = obj2prim (%s, %s, %s)) == NULLPE) {\n",
		    level * 4, "", arg, narg, class, value);
	    printf ("%*sadvise (NULLCP, \"%s: %%s\", PEPY_ERR_NOMEM);\n",
		    (level + 1) * 4, "", id);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
	    break;

	case YP_SEQTYPE:
	    if (yp -> yp_type && yp -> yp_control) {
		printf ("%*sfor (%s) {\n",
			level * 4, "", yp -> yp_control);
		if (!dflag && yp -> yp_action3) {
		    do_action (yp -> yp_action3, ++level, narg ? narg : arg,
				yp -> yp_act3_lineno);
		    printf ("%*s{\n", level * 4, "");
		}
		do_type (yp -> yp_type, level + 1, "element", narg);
		if (!dflag && yp -> yp_action3)
		    printf ("%*s}\n", level-- * 4, "");
#ifndef notdef
		printf ("%*sseq_addon (%s, %s, %s);\n", (level + 1) * 4, "",
			arg, narg2, narg);
		printf ("%*s%s = %s;\n%*s}\n", (level + 1) * 4, "",
			narg2, narg, level * 4, "");
#else
		printf ("%*sif (seq_add (%s, %s, -1) == NOTOK) {\n",
			(level + 1) * 4, "", arg, narg);
		printf ("%*sadvise (NULLCP, \"%s %%s: %%s\", PEPY_ERR_BAD_SEQ,\n",
			(level + 2) * 4, "", id);
		printf ("%*spe_error (%s -> pe_errno));\n", (level + 4) * 4,
			"", arg);
		printf ("%*sreturn NOTOK;\n%*s}\n", (level + 2) * 4, "",
			(level + 1) * 4, "");
		printf ("%*s}\n", level * 4, "");
#endif
	    }
	    break;

	case YP_SEQLIST:
	    for (y = yp -> yp_type, i = 0; y; y = y -> yp_next, i++) {
		if (y -> yp_flags & YP_COMPONENTS)
		    do_components_seq (y, level, y -> yp_next == NULLYP,
				       id, arg, narg);
		else {
		    do_type_element (y, level, y -> yp_next == NULLYP,
				     id, narg);
		    printf ("%*sif (%s != NULLPE)\n", level * 4, "", narg);
		    printf ("%*sif (seq_add (%s, %s, -1) == NOTOK) {\n",
			    (level + 1) * 4, "", arg, narg);
		    printf ("%*sadvise (NULLCP, \"%s %%s%%s\", PEPY_ERR_BAD_SEQ,\n",
			    (level + 2) * 4, "", id);
		    printf ("%*spe_error (%s -> pe_errno));\n", (level + 4) * 4,
			    "", arg);
		    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 2) * 4, "",
			    (level + 1) * 4, "");
		}
	    }
	    for (y = yp -> yp_type; y; y = y -> yp_next) {
		register YP     z;

		if (!(y -> yp_flags & (YP_OPTIONAL | YP_DEFAULT))
			|| lookup_tag (y) == NULLYT)
		    continue;
		for (z = y -> yp_next; z; z = z -> yp_next)
		    if (!(z -> yp_flags & (YP_OPTIONAL | YP_DEFAULT))
			    || lookup_tag (z) == NULLYT)
			break;
		uniqtag (y, z);
		if (z == NULLYP)
		    break;
		y = z;
	    }
	    break;

	case YP_SETTYPE:
	    if (yp -> yp_type && yp -> yp_control) {
		printf ("%*sfor (%s) {\n",
			level * 4, "", yp -> yp_control);
		if (!dflag && yp -> yp_action3) {
		    do_action (yp -> yp_action3, ++level, narg ? narg : arg,
				yp -> yp_act3_lineno);
		    printf ("%*s{\n", level * 4, "");
		}
		do_type (yp -> yp_type, level + 1, "member", narg);
		if (!dflag && yp -> yp_action3)
		    printf ("%*s}\n", level-- * 4, "");
#ifndef notdef
		printf ("%*sset_addon (%s, %s, %s);\n", (level + 1) * 4, "",
			arg, narg2, narg);
		printf ("%*s%s = %s;\n%*s}\n", (level + 1) * 4, "",
			narg2, narg, level * 4, "");
#else
		printf ("%*sif (seq_add (%s, %s, -1) == NOTOK) {\n",
			(level + 1) * 4, "", arg, narg);
		printf ("%*sadvise (NULLCP, \"%s %%s%%s\", PEPY_ERR_BAD_SET,\n",
			(level + 2) * 4, "", id);
		printf ("%*spe_error (%s -> pe_errno));\n", (level + 4) * 4,
			"", arg);
		printf ("%*sreturn NOTOK;\n%*s}\n", (level + 2) * 4, "",
			(level + 1) * 4, "");
		printf ("%*s}\n", level * 4, "");
#endif
	    }
	    break;

	case YP_SETLIST:
	    if (yp -> yp_type) {
		for (y = yp -> yp_type; y; y = y -> yp_next) {
		    if (y -> yp_flags & YP_COMPONENTS)
			do_components_set (y, level, id, arg, narg);
		    else {
			do_type_member (y, level, narg);
			printf ("%*sif (%s != NULLPE)\n", level * 4, "", narg);
			printf ("%*sif (set_add (%s, %s) == NOTOK) {\n",
				(level + 1) * 4, "", arg, narg);
			printf ("%*sadvise (NULLCP, \"%s %%s%%s\", PEPY_ERR_BAD_SET,\n",
				(level + 2) * 4, "", id);
			printf ("%*spe_error (%s -> pe_errno));\n",
				(level + 4) * 4, "", arg);
			printf ("%*sreturn NOTOK;\n%*s}\n", (level + 2) * 4, "",
				(level + 1) * 4, "");
		    }
		}
				/* now pull up fully to check uniqueness */
		choice_pullup (y = copy_type (yp), CH_FULLY);
		uniqtag (y -> yp_type, NULLYP);
	    }
	    break;

	case YP_CHOICE:
	    if (yp -> yp_type && yp -> yp_control) {
		printf ("%*sswitch (%s = (%s)) {\n",
			level * 4, "", narg2, yp -> yp_control);
		for (y = yp -> yp_type, i = 0; y; y = y -> yp_next)
		    do_type_choice (y, ++i, level + 1, arg);
		choice_pullup (yp, CH_FULLY);
		uniqtag (yp -> yp_type, NULLYP);
		printf ("\n%*sdefault:\n", (level + 1) * 4, "");
		printf ("%*sadvise (NULLCP, \"%s %%s%%d\", PEPY_ERR_INVALID_CHOICE, \n",
			(level + 2) * 4, "", id);
		printf ("%*s%s);\n", (level + 4) * 4, "", narg2);
		printf ("%*sreturn NOTOK;\n", (level + 2) * 4, "");
		printf ("%*s}\n", level * 4, "");
		if ((yp -> yp_flags & YP_TAG)
			&& !(yp -> yp_flags & YP_PULLEDUP))
		    tag_pushdown (yp, level, arg, "choice");
	    }
	    break;

	case YP_IDEFINED:
	    printf ("%*sif (%s (", level * 4, "", modsym (yp -> yp_module,
		    yp -> yp_identifier, YP_ENCODER));
	    i = strlen (arg) - 3;
	    printf ("%*.*s, 0, ", i, i, arg + 2);
	    if (yp -> yp_intexp)
		printf ("%s, ", yp -> yp_intexp);
	    else if (level == 1)
		printf ("len, ");
	    else
		printf ("NULL, ");
	    if (yp -> yp_strexp)
		printf ("%s", yp -> yp_strexp);
	    else if (level == 1)
		printf ("buffer");
	    else
		printf ("NULLCP");
	    if (yp -> yp_flags & YP_PARMVAL)
		printf (", %s", yp -> yp_parm);
	    else
		printf (", NullParm");
	    printf (") == NOTOK)\n%*sreturn NOTOK;\n", (level + 1) * 4, "");
	    if ((yp -> yp_flags & (YP_TAG | YP_IMPLICIT))
		      == (YP_TAG | YP_IMPLICIT)) {
		if (is_nonimplicit_type (yp))
		    pushdown = 1;
		else {
		    printf ("%*s%s -> pe_class = %s;\n", level * 4, "",
			    arg, class);
		    printf ("%*s%s -> pe_id = %s;\n", level * 4, "",
			    arg, value);
		}
	    }
	    break;

	default:
	    myyerror ("unknown type: %d", yp -> yp_code);
    }
    if (pushdown && !(yp -> yp_flags & YP_PULLEDUP)) {
	switch (yp -> yp_code) { /* sets & seqs are implicit implicit's? */
	    case YP_CHOICE:
	        break;

	    default:
		tag_pushdown (yp, level, arg, id);
		break;
	    }
    }

    printf ("\n#ifdef DEBUG\n%*s(void) testdebug (%s, \"",
	    level * 4, "", arg);
    if (level == 1)
	printf ("%s.", mymodule);
    printf ("%s\");\n#endif\n\n", id);

    if (!dflag && yp -> yp_action2)
	do_action (yp -> yp_action2, level, arg, yp -> yp_act2_lineno);

    switch (yp -> yp_code) {
	case YP_BITLIST:
	    printf ("#undef\tBITS\n");
	    break;

	default:
	    break;
    }
}


static char *add_point (arg)
char *arg;
{
    char    buffer[BUFSIZ];

    (void) sprintf (buffer, "(*%s)", arg);
    return new_string (buffer);
}

/*  */

static  do_type_member (yp, level, narg)
register YP     yp;
register int    level;
char   *narg;
{
    int     pushdown = (yp -> yp_flags & (YP_TAG | YP_IMPLICIT)) == YP_TAG;
    char   *id = yp -> yp_flags & YP_ID ? yp -> yp_id : "member";

    if (!(yp -> yp_flags & YP_TAG)) {
	switch (yp -> yp_code) {
	    case YP_CHOICE:
	        break;
	    case YP_IDEFINED:
	        if (lookup_tag (yp) == NULLYT)
		    break;
		/* else drop ... */
	    default:
		tag_type (yp);
		break;
	}
    }
    printf ("%*s%s = NULLPE;\n\n", level * 4, "", narg);
    if (yp -> yp_flags & (YP_OPTIONAL | YP_DEFAULT)) {
	if (yp -> yp_flags & YP_OPTCONTROL)
	    printf ("%*sif (%s) {\n", level * 4, "", yp -> yp_optcontrol);
	else
	    return;
    }
    else
	printf ("%*s{\n", level * 4, "");

    level++;
    yp -> yp_flags |= YP_PULLEDUP;

    do_type (yp, level, id, narg);

    if (pushdown)
	tag_pushdown (yp, level, narg, id);

    level--;
    printf ("%*s}\n", level * 4, "");
}


/*  */

static  do_type_choice (yp, caseindex, level, narg)
register YP     yp;
register int    caseindex,
		level;
register char  *narg;
{
    int     pushdown = (yp -> yp_flags & YP_TAG)
			    && !(yp -> yp_flags & YP_IMPLICIT);
    char   *id = yp -> yp_flags & YP_ID ? yp -> yp_id : "member";

    printf ("%*scase %d:", level * 4, "", caseindex);
    if (yp -> yp_flags & YP_ID)
	printf ("\t/* %s */", yp -> yp_id);
    printf ("\n");
    level++;

    printf ("%*s{\n", level * 4, "");
    level++;

    yp -> yp_flags |= YP_PULLEDUP;

    do_type (yp, level, id, narg);

    if (pushdown) {
	tag_pushdown (yp, level, narg, id);
    }

    level--;
    printf ("%*s}\n%*sbreak;\n", level * 4, "", level * 4, "");
}

/*  */

do_action (action, level, arg, lineno)
register char  *action,
	       *arg;
register int level;
int	lineno;
{
    register char   c,
		    d;

    printf ("%*s{\n", level * 4, "");

    if (!Pflag && *sysin)
	printf ("# line %d \"%s\"\n", lineno, sysin);

    for (d = NULL; c = *action++; d = c)
	switch (d) {
	    case '$':
		if (c == '$') {
		    printf ("%s", arg);
		    c = NULL;
		    break;
		}
		putchar ('$');	/* fall */

	    default:
		if (c != '$')
		    putchar (c);
		break;
	}

    switch (d) {
	case '\n':
	    break;

	case '$':
	    putchar ('$');	/* fall */
	default:
	    putchar ('\n');
	    break;
    }

    printf ("%*s}\n", level * 4, "");
}

/*  */

/* ARGSUSED */

static  do_type_element (yp, level, last, id, narg)
register YP     yp;
register int    level;
int     last;
char   *id;
register char  *narg;
{
    printf ("%*s%s = NULLPE;\n\n", level * 4, "", narg);
    if (yp -> yp_flags & (YP_OPTIONAL | YP_DEFAULT)) {
	if (yp -> yp_flags & YP_OPTCONTROL)
	    printf ("%*sif (%s) {", level * 4, "", yp -> yp_optcontrol);
	else
	    return;
    }
    else {
	printf ("%*s{", level * 4, "");
    }
    level++;
    if (yp -> yp_flags & YP_ID)
	printf ("\t/* %s */", yp -> yp_id);
    printf ("\n");
    do_type (yp, level, yp -> yp_flags & YP_ID ? yp -> yp_id : "element",
	    narg);

    level--;
    printf ("%*s}\n\n", level * 4, "");
}

static do_components_seq (yp, level, last, id, arg, narg)
YP	yp;
register int level;
register char	*id,
    		*arg,
		*narg;
{
    YP	newyp, y;

    if (yp -> yp_module) {
	pyyerror (yp, "Can't do COMPONENTS OF with external types for %s",
		  yp -> yp_identifier);
	return;
    }

    if (!(newyp = lookup_type (yp->yp_module, yp -> yp_identifier))) {
	pyyerror (yp, "Can't find refernced COMPONENTS OF");
	return;
    }

    if (newyp -> yp_code != YP_SEQLIST) {
	yyerror_aux ("COMPONENTS OF type is not a SEQUENCE");
	print_type (yp, 0);
	return;
    }
    for (y = newyp -> yp_type; y; y = y -> yp_next) {
	if (y -> yp_flags & YP_COMPONENTS)
	    do_components_seq (y, level, last && y -> yp_next == NULLYP,
			       id, arg, narg);
	else {
	    do_type_element (y, level, last && y -> yp_next == NULLYP,
			     id, narg);
	    printf ("%*sif (%s != NULLPE)\n", level * 4, "", narg);
	    printf ("%*sif (seq_add (%s, %s, -1) == NOTOK) {\n",
		    (level + 1) * 4, "", arg, narg);
	    printf ("%*sadvise (NULLCP, \"%s %%s%%s\", PEPY_ERR_BAD_SEQ,\n",
		    (level + 2) * 4, "", id);
	    printf ("%*spe_error (%s -> pe_errno));\n", (level + 4) * 4,
		    "", arg);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 2) * 4, "",
		    (level + 1) * 4, "");
	}
    }
    for (y = newyp -> yp_type; y; y = y -> yp_next) {
	register YP     z;

	if (!(y -> yp_flags & (YP_OPTIONAL | YP_DEFAULT))
	    || lookup_tag (y) == NULLYT)
	    continue;
	for (z = y -> yp_next; z; z = z -> yp_next)
	    if (!(z -> yp_flags & (YP_OPTIONAL | YP_DEFAULT))
		|| lookup_tag (z) == NULLYT)
		break;
	uniqtag (y, z);
	if (z == NULLYP)
	    break;
	y = z;
    }
    return;
}



static  do_components_set (yp, level, arg, id, narg)
register YP     yp;
register int    level;
char   *narg, *arg, *id;
{

    YP	newyp, y;

    if (yp -> yp_module) {
	pyyerror (yp, "Can't do COMPONENTS OF with external types for %s",
		  yp -> yp_identifier);
	return;
    }

    if (!(newyp = lookup_type (yp->yp_module, yp -> yp_identifier))) {
	pyyerror (yp, "Can't find refernced COMPONENTS OF");
	return;
    }

    if (newyp -> yp_code != YP_SETLIST) {
	yyerror_aux ("COMPONENTS OF type is not a SET");
	print_type (yp, 0);
	return;
    }
    
    for (y = newyp -> yp_type; y; y = y -> yp_next) {
	if (y -> yp_flags & YP_COMPONENTS)
	    do_components_set (y, level, arg, id, narg);
	else {
	    do_type_member (y, level, narg);
	    printf ("%*sif (%s != NULLPE)\n", level * 4, "", narg);
	    printf ("%*sif (set_add (%s, %s) == NOTOK) {\n",
		    (level + 1) * 4, "", arg, narg);
	    printf ("%*sadvise (NULLCP, \"%s %%s%%s\", PEPY_ERR_BAD_SET,\n",
		    (level + 2) * 4, "", id);
	    printf ("%*spe_error (%s -> pe_errno));\n",
		    (level + 4) * 4, "", arg);
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 2) * 4, "",
		    (level + 1) * 4, "");
	}
    }
    choice_pullup (newyp, CH_FULLY);
}
