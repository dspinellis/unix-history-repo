/* util.c - General utility routines */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/util.c,v 7.3 91/02/22 09:20:45 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/util.c,v 7.3 91/02/22 09:20:45 mrose Interim $
 *
 *
 * $Log:	util.c,v $
 * Revision 7.3  91/02/22  09:20:45  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:43:11  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:35:23  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:44:40  mrose
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
#include "quipu/commonarg.h"
#include "quipu/malloc.h"
#include "tsap.h"
#include <varargs.h>

extern LLog * log_dsap;
extern char * dsa_mode;

char * SkipSpace (ptr)
register char * ptr;
{
	if (ptr == NULLCP)
		return (NULLCP);

	while ( isascii(*ptr) && isspace (*ptr))
		ptr++;
	return (ptr);
}

void StripSpace (b)
register char *b;
/* copy b to a less spaces and comments */
{
register char *a;

	if (*b == COMMENT ) {
		*b = 0;
		return;
	}

	while (isascii(*b) && (! isspace(*b)) && (*b))
		b++;

	if (!*b)
		return;         /* nothing needs doing */

	a = b;

	for (;;) {
		if (isascii(*b) && isspace (*b))
			b++;
		else if ( (!*b) || (*b == COMMENT))
			break;
		else
			*a++ = *b++;
		}

	*a = 0;
}

void StripSpace2 (b)
register char *b;
/* copy b to a less spaces and comments */
{
register char *a;

	while (isascii(*b) && (! isspace(*b)) && (*b))
		b++;

	if (!*b)
		return;         /* nothing needs doing */

	a = b;

	for (;;) {
		if (isascii(*b) && isspace (*b))
			b++;
		else if (!*b)
			break;
		else
			*a++ = *b++;
		}

	*a = 0;
}

char * TidyString2 (a)
register char * a;
{
register char * b;
char * c;
register int i = 0;

	/* removing multiple and trailing spaces */
	c = a, b = a;
	while (*a) {
		if (isascii(*a) && isspace (*a)) {
			*b = ' ';       /* make sure not a tab etc */
			a++;
			while (isascii(*a) && isspace (*a)) {
				a++;
				i = 1;
			}

			if (*a)
				b++;
			else
				break;
		}
		if (i)
			*b = *a;

		a++, b++;
	}

	*b = 0;

	if (*--b == '\n')
		*b-- = 0;

	if (*b == ' ')
		*b = 0;

	return (c);
}

char * TidyString (a)
register char * a;
{
	if (!*a)
		return (a);

	/* remove white space from front of string */
	while (isascii(*a) && isspace (*a))
		a++;

	return (TidyString2(a));
}

test_prim_pe (pe,class,id)
PE pe;
PElementClass class;
PElementID id;
{
	if (pe == NULLPE)
		return FALSE;

	if (pe->pe_form != PE_FORM_PRIM) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("Primative Attribute Value expected"));
		return FALSE;
	}

	if ( PE_ID (pe -> pe_class, pe -> pe_id) != PE_ID (class, id) ) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("Bad class/ID in Attribute Value"));
		return FALSE;
	}

	return (TRUE);
	
}

#ifndef lint
ps_printf (va_alist)
va_dcl
{
	PS ps;
	extern int std_flush ();
	va_list ap;

	va_start (ap);

	ps = va_arg (ap, PS);

#ifdef VSPRINTF

	if (ps->ps_flushP == std_flush) {
		char * fmt;

		fmt = va_arg (ap, char *);

		if (vfprintf ((FILE *)ps->ps_addr, fmt, ap) == EOF)
			ps->ps_errno = PS_ERR_IO;

	} else 
#endif

	{
		char buffer [8192]; 	/* How big should this go !!! */
		
		_asprintf (buffer,NULLCP,ap);

		ps_print (ps,buffer);
	}


	va_end (ap) ;
}
#else

/* VARARGS2 */
ps_printf (ps,fmt)
PS ps;
char * fmt;
{
(void) ps_printf (ps,fmt) ;
}

#endif


fatal (code,fmt)
int  code;
char *fmt;
{
	if (dsa_mode)
		log_dsap -> ll_syslog = LLOG_FATAL;
	LLOG (log_dsap,LLOG_FATAL,("Quipu failure (%d): %s",code,fmt));
	stop_listeners ();
	(void) fflush (stdout);
	(void) fprintf (stderr,"Quipu failure (%d): %s\n",code,fmt);
	exit (code);
}

static PS ps = NULLPS;

pslog (lp,event,str,func,ptr)
LLog * lp;
int event;
char * str;
int (*func) ();         /* assumes func (PS ,dataptr,(int) format); */
caddr_t ptr;
{
	/* log info to pstream */

    if (!(lp -> ll_events & event))
	return;

    if (ps == NULL
	    && (((ps = ps_alloc (str_open)) == NULLPS)
		    || str_setup (ps, NULLCP, BUFSIZ, 0) == NOTOK)) {
	if (ps)
	    ps_free (ps), ps = NULLPS;

	return;
    }

    (*func) (ps,ptr,EDBOUT);
    ps_print (ps, " ");
    *--ps -> ps_ptr = NULL, ps -> ps_cnt++;

    LLOG (lp,event,("%s: %s",str,ps->ps_base));

    ps->ps_cnt = ps->ps_bufsiz;
    ps->ps_ptr = ps->ps_base;
}

stop_listeners ()
{
struct TSAPdisconnect	  td_s;
struct TSAPdisconnect	* td = &(td_s);

	/* close all listeners */
	(void) TNetClose (NULLTA,td);
}

quipu_pe_cmp (a,b)
register PE a,b;
{
char *p,*q;
register int j,i;

	/* based on ISODE pe_cmp */

	if ( a == NULLPE) {
		if ( b == NULLPE )
			return (0);
		else 
			return (1);
	}
	if ( b == NULLPE)
		return (-1);

	if (a->pe_class != b->pe_class)
		if (a->pe_class >  b->pe_class)
			return (1) ;
		else
			return (-1);

	if (a->pe_form != b->pe_form)
		if (a->pe_form >  b->pe_form)
			return (1) ;
		else
			return (-1);

	if (a->pe_id != b->pe_id)
		if (a->pe_id >  b->pe_id)
			return (1) ;
		else
			return (-1);

	switch ( a->pe_form ) {
	    case PE_FORM_ICONS:
	    	if (a->pe_ilen != a->pe_ilen)
			return (a->pe_ilen > b->pe_ilen ? 1 : -1);
	    case PE_FORM_PRIM:
		if ( (i=a->pe_len) != b->pe_len)
			if (  i > b->pe_len )
				return (1);
			else
				return (-1);

		p = (char *) a->pe_prim;
		q = (char *) b->pe_prim;

		for (j=0; j<i; j++) {
			if (*p != *q)
				if ( *p > *q )
					return (1);
				else
					return (-1);
			p++;
			q++;
		}
		return (0) ;

	    case PE_FORM_CONS:
		for (a=a->pe_cons,b=b->pe_cons; a; a=a->pe_next,b=b->pe_next)
			if ((i = quipu_pe_cmp (a,b)) != 0)
				return (i);
		return (b ? 1 : 0);
	    default:
		return (1);

	}
}


IFP acl_fn = NULLIFP;

struct acl_info * acl_dflt ()
{
	if (acl_fn == NULLIFP)
		return ((struct acl_info *) NULL);
	else
		return ((struct acl_info *)(*acl_fn)());
}

