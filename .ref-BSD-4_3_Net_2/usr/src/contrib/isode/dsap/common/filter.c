/* filter.c - Directory Operation Filters */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/filter.c,v 7.3 91/02/22 09:19:21 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/filter.c,v 7.3 91/02/22 09:19:21 mrose Interim $
 *
 *
 * $Log:	filter.c,v $
 * Revision 7.3  91/02/22  09:19:21  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:41:59  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:34:32  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:42:17  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/ds_search.h"

extern LLog * log_dsap;

filter_free (filt)
Filter filt;
{
register Filter ptr;
register Filter next;

	for (ptr = filt; ptr != NULLFILTER; ptr=next) {
		if (ptr->flt_type  == FILTER_ITEM) {
			switch (ptr->FUITEM.fi_type) {
			case FILTERITEM_EQUALITY:
			case FILTERITEM_GREATEROREQUAL:
			case FILTERITEM_LESSOREQUAL:
			case FILTERITEM_APPROX:
				AttrT_free (ptr->FUITEM.UNAVA.ava_type);
				AttrV_free (ptr->FUITEM.UNAVA.ava_value);
				break;
			case FILTERITEM_PRESENT:
				AttrT_free (ptr->FUITEM.UNTYPE);
				break;
			case FILTERITEM_SUBSTRINGS:
				AttrT_free (ptr->FUITEM.UNSUB.fi_sub_type);
				avs_free (ptr->FUITEM.UNSUB.fi_sub_initial);
				avs_free (ptr->FUITEM.UNSUB.fi_sub_any);
				avs_free (ptr->FUITEM.UNSUB.fi_sub_final);
				break;
			}
		}  else
			filter_free (ptr->flt_un.flt_un_filter);

		next = ptr->flt_next;
		free ((char *) ptr);
	}
}

filter_append (a,b)
Filter a,b;
{
register Filter ptr,trail;

	if ( a == NULLFILTER)
		DLOG (log_dsap,LLOG_DEBUG,("appending to null filter !"));

	for (ptr=a; ptr!= NULLFILTER; ptr=ptr->flt_next)
		trail = ptr;

	trail->flt_next = b;
}


Filter strfilter (at,s,type)
AttributeType at;
char * s;
char type;
{
    Filter filt;

    at = AttrT_cpy (at);
    filt = filter_alloc ();
    filt -> flt_next = NULLFILTER;
    filt -> flt_type = FILTER_ITEM;

    if (type == FILTERITEM_SUBSTRINGS || type == -FILTERITEM_SUBSTRINGS) {
	char   *dp;

	if (*s == '*' && !s[1]) {
	    filt -> FUITEM.fi_type = FILTERITEM_PRESENT;
	    filt -> FUITEM.UNTYPE = at;
	    goto all_done;
	}

	filt -> FUITEM.fi_type = FILTERITEM_SUBSTRINGS;
	filt -> FUITEM.UNSUB.fi_sub_type = at;
	filt -> FUITEM.UNSUB.fi_sub_initial = NULLAV;
	filt -> FUITEM.UNSUB.fi_sub_any = NULLAV;
	filt -> FUITEM.UNSUB.fi_sub_final = NULLAV;
	if (dp = index (s, '*')) {
	    char    buffer[BUFSIZ];

	    (void) strcpy (buffer, s);
	    dp = buffer + (dp - s);
	    s = buffer;

	    *dp++ = NULL;
	    if (*s)
		filt -> FUITEM.UNSUB.fi_sub_initial =
		    str2avs (s, filt -> FUITEM.UNSUB.fi_sub_type);
	    s = dp;

	    if (dp = rindex (s, '*')) {
		AV_Sequence any_end = NULL;

		*dp++ = NULL;
		if (*dp)
		    filt -> FUITEM.UNSUB.fi_sub_final =
			str2avs (dp, filt -> FUITEM.UNSUB.fi_sub_type);

		do {
		    if (dp = index (s, '*'))
			*dp++ = NULL;
		    if (*s) {
			AV_Sequence any =
			    str2avs (s,
				     filt -> FUITEM.UNSUB.fi_sub_type);

			if (any_end) {
			    any_end -> avseq_next = any;
			    any_end = any_end -> avseq_next;
			}
			else
			    filt -> FUITEM.UNSUB.fi_sub_any = any_end = any;
		    }
		} while (s = dp);
	    }
	    else
		if (*s)
		    filt -> FUITEM.UNSUB.fi_sub_final =
			str2avs (s, filt -> FUITEM.UNSUB.fi_sub_type);
	}
	else
	    if (type == FILTERITEM_SUBSTRINGS)
		filt -> FUITEM.UNSUB.fi_sub_any =
		    str2avs (s, filt -> FUITEM.UNSUB.fi_sub_type);
	    else
		filt -> FUITEM.UNSUB.fi_sub_initial =
		    str2avs (s, filt -> FUITEM.UNSUB.fi_sub_type);
    }
    else {
	filt -> FUITEM.fi_type = type;
	filt -> FUITEM.UNAVA.ava_type = at;
	filt -> FUITEM.UNAVA.ava_value =
	    str2AttrV (s, filt -> FUITEM.UNAVA.ava_type -> oa_syntax);
    }
all_done: ;

    return filt;
}

Filter ocfilter (s)
char * s;
{
Filter filt;

	filt = filter_alloc ();
	filt->flt_next = NULLFILTER;
	filt->flt_type = FILTER_ITEM;
	filt->FUITEM.fi_type = FILTERITEM_EQUALITY;
	if ((filt->FUITEM.UNAVA.ava_type = AttrT_new ("ObjectClass")) == NULLAttrT) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("ObjectClass attribute unknown"))
		return NULLFILTER;
	}
	filt->FUITEM.UNAVA.ava_value = str2AttrV(s,
		filt->FUITEM.UNAVA.ava_type->oa_syntax);
	if (filt->FUITEM.UNAVA.ava_value == NULLAttrV) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("'%s' unknown",s));
		return NULLFILTER;
	}

	return filt;
}

Filter joinfilter (f, type)
Filter f;
char type;
{
Filter filt;

	filt = filter_alloc ();
	filt->flt_next = NULLFILTER;
	filt->flt_type = type;
	filt->FUFILT = f;

	return filt;
}

/* ARGSUSED */

int	fi_print (ps, fi, format)
PS	ps;
Filter	fi;
int	format;
{
    print_filter (ps, fi, 0);
}


print_filter (nps, fi, level)
PS nps;
register Filter	fi;
int	level;
{
    char   *cp;
    register Filter    fi2;
    register struct filter_item *fi3;

    switch (fi -> flt_type) {
	case FILTER_ITEM:
	    fi3 = &fi -> FUITEM;
	    if (level)
		ps_print (nps, "(");
	    switch (fi3 -> fi_type) {
	        case FILTERITEM_APPROX:
		    cp = "~=";
		    goto item;

	        case FILTERITEM_EQUALITY:
		    cp = "=";
		    goto item;

	        case FILTERITEM_GREATEROREQUAL:
		    cp = ">=";
		    goto item;

	        case FILTERITEM_LESSOREQUAL:
		    cp = ">=";
item: ;
		    AttrT_print (nps, fi3 -> UNAVA.ava_type, EDBOUT);
		    ps_print (nps, cp);
		    AttrV_print (nps, fi3 -> UNAVA.ava_value, EDBOUT);
		    break;

	        case FILTERITEM_SUBSTRINGS:
		    AttrT_print (nps, fi3 -> UNSUB.fi_sub_type, EDBOUT);
		    ps_print (nps, "=");
		    avs_print_aux (nps, fi3 -> UNSUB.fi_sub_initial,EDBOUT,"*");
		    ps_print (nps, "*");
		    avs_print_aux (nps, fi3 -> UNSUB.fi_sub_any, EDBOUT, "*");
		    ps_print (nps, "*");
		    avs_print_aux (nps, fi3 -> UNSUB.fi_sub_final, EDBOUT, "*");
		    break;

	        case FILTERITEM_PRESENT:
		    AttrT_print (nps, fi3 -> UNTYPE, EDBOUT);
		    ps_print (nps, "=*");
		    break;

		default:
		    ps_printf (nps,
			       "[internal error--malformed filter item type 0x%x]",
			       fi3 -> fi_type);
		    break;
	    }
	    if (level)
		ps_print (nps, ")");
	    break;
	
	case FILTER_AND:
	    cp = "&";
	    goto op;
	case FILTER_OR:
	    cp = "|";
	    goto op;

	case FILTER_NOT:
	    cp = "!";
op: ;
	    ps_printf (nps, "(%s", cp);
	    level++;
	    for (fi2 = fi -> FUFILT; fi2; fi2 = fi2 -> flt_next) {
		ps_print (nps, " ");
		print_filter (nps, fi2, level);
	    }
	    ps_print (nps, ")");
	    break;

	default:
	    ps_printf (nps, "[internal error--malformed filter type 0x%x]",
		       fi -> flt_type);
    }
}
