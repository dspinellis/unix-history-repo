/*
 * $XConsortium: XICWrap.c,v 11.7 91/05/30 13:10:36 rws Exp $
 */

/*
 * Copyright 1990, 1991 by OMRON Corporation, NTT Software Corporation,
 *                      and Nippon Telegraph and Telephone Corporation
 * Copyright 1991 by the Massachusetts Institute of Technology
 * Copyright 1991 by the Open Software Foundation
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of OMRON, NTT Software, NTT, Open
 * Software Foundation and M.I.T. not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission. OMRON, NTT Software, NTT, Open Software
 * Foundation and M.I.T. make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *
 * OMRON, NTT SOFTWARE, NTT, OPEN SOFTWARE FOUNDATION AND M.I.T. 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
 * SHALL OMRON, NTT SOFTWARE, NTT, OPEN SOFTWARE FOUNDATIONN OR M.I.T. BE
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 *	Authors: Li Yuhong		OMRON Corporation
 *		 Tatsuya Kato		NTT Software Corporation
 *		 Hiroshi Kuribayashi	OMRON Coproration
 *		 Muneiyoshi Suzuki	Nippon Telegraph and Telephone Co.
 * 
 *		 M. Collins		OSF  
 */				

#define NEED_EVENTS
#include "Xlibint.h"
#include "Xlcint.h"
#if NeedVarargsPrototypes
# include <stdarg.h>
# define Va_start(a,b) va_start(a,b)
#else
# include <varargs.h>
# define Va_start(a,b) va_start(a)
#endif

static int
_XIMNestedListToNestedList(nlist, list)
    XIMArg *nlist;   /* This is the new list */
    XIMArg *list;    /* The original list */
{
    register XIMArg *ptr = list;

    while (ptr->name) {
	if (!strcmp(ptr->name, XNVaNestedList)) {
	    nlist += _XIMNestedListToNestedList(nlist, (XIMArg *)ptr->value);
	} else {
	    nlist->name = ptr->name;
	    nlist->value = ptr->value;
	    ptr++;
	    nlist++;
	}
    }
    return ptr - list;
}

static void
_XIMCountNestedList(args, total_count)
    XIMArg *args;
    int *total_count;
{
    for (; args->name; args++) {
	if (!strcmp(args->name, XNVaNestedList))
	    _XIMCountNestedList((XIMArg *)args->value, total_count);
	else
	    ++(*total_count);
    }
}

#if NeedVarargsPrototypes
static void
_XIMCountVaList(va_list var, int *total_count)
#else
static void
_XIMCountVaList(var, total_count)
    va_list var;
    int *total_count;
#endif
{
    char *attr;

    *total_count = 0;

    for (attr = va_arg(var, char*); attr; attr = va_arg(var, char*)) {
	if (!strcmp(attr, XNVaNestedList)) {
	    _XIMCountNestedList(va_arg(var, XIMArg*), total_count);
	} else {
	    va_arg(var, XIMArg*);
	    ++(*total_count);
	}
    }
}

#if NeedVarargsPrototypes
static void
_XIMVaToNestedList(va_list var, int max_count, XIMArg **args_return)
#else
static void
_XIMVaToNestedList(var, max_count, args_return)
    va_list var;
    int max_count;
    XIMArg **args_return;
#endif
{
    XIMArg *args;
    char   *attr;

    if (max_count <= 0) {
	*args_return = (XIMArg *)NULL;
	return;
    }

    args = (XIMArg *)Xmalloc((unsigned)(max_count + 1) * sizeof(XIMArg));
    *args_return = args;
    if (!args) return;

    for (attr = va_arg(var, char*); attr; attr = va_arg(var, char*)) {
	if (!strcmp(attr, XNVaNestedList)) {
	    args += _XIMNestedListToNestedList(args, va_arg(var, XIMArg*));
	} else {
	    args->name = attr;
	    args->value = va_arg(var, XPointer);
	    args++;
	}
    }
    args->name = (char*)NULL;
}

/*ARGSUSED*/
#if NeedVarargsPrototypes
XVaNestedList
XVaCreateNestedList(int dummy, ...)
#else
XVaNestedList
XVaCreateNestedList(dummy, va_alist)
    int dummy;
    va_dcl
#endif
{
    va_list		var;
    XIMArg		*args = NULL;
    int			total_count;

    Va_start(var, dummy);
    _XIMCountVaList(var, &total_count);
    va_end(var);

    Va_start(var, dummy);
    _XIMVaToNestedList(var, total_count, &args);
    va_end(var);

    return (XVaNestedList)args;
}

#if NeedVarargsPrototypes
char *
XGetIMValues(XIM im, ...)
#else				/* NeedVarargsPrototypes */
char *
XGetIMValues(im, va_alist)
    XIM im;
    va_dcl
#endif				/* NeedVarargsPrototypes */
{
    va_list var;
    int     total_count;
    XIMArg *args;
    char   *ret;

    /*
     * so count the stuff dangling here
     */
    Va_start(var, im);
    _XIMCountVaList(var, &total_count);
    va_end(var);

    /*
     * now package it up so we can send it along
     */
    Va_start(var, im);
    _XIMVaToNestedList(var, total_count, &args);
    va_end(var);

    ret = (*im->methods->get_values) (im, args);
    if (args) Xfree((char *)args);
    return ret;
}

/*
 * Create an input context within the input method, 
 * and return a pointer to the input context.
 */

#if NeedVarargsPrototypes
XIC
XCreateIC(XIM im, ...)
#else
XIC
XCreateIC(im, va_alist)
    XIM im;		/* specified the attached input method */
    va_dcl		/* specified variable length argment list */
#endif
{
    va_list var;
    int     total_count;
    XIMArg *args;
    XIC     ic;
  
    /*
     * so count the stuff dangling here
     */
    Va_start(var, im);
    _XIMCountVaList(var, &total_count);
    va_end(var);

    /*
     * now package it up so we can send it along
     */
    Va_start(var, im);
    _XIMVaToNestedList(var, total_count, &args);
    va_end(var);

    ic = (XIC) (*im->methods->create_ic) (im, args);
    if (args) Xfree((char *)args);
    if (ic) {
	ic->core.next = im->core.ic_chain;
	im->core.ic_chain = ic;
    }
    return ic;
}

/*
 * Free the input context.
 */
void
XDestroyIC(ic)
    XIC ic;
{
    XIM im = ic->core.im;
    XIC *prev;

    (*ic->methods->destroy) (ic);
    if (im) {
	for (prev = &im->core.ic_chain; *prev; prev = &(*prev)->core.next) {
	    if (*prev == ic) {
		*prev = ic->core.next;
		break;
	    }
	}
    }
    Xfree ((char *) ic);
}

#if NeedVarargsPrototypes
char *
XGetICValues(XIC ic, ...)
#else
char *
XGetICValues(ic, va_alist)
    XIC ic;
    va_dcl
#endif
{ 
    va_list var;
    int     total_count;
    XIMArg *args;
    char   *ret;

    if (!ic->core.im)
	return (char *) NULL;

    /*
     * so count the stuff dangling here
     */
    Va_start(var, ic);
    _XIMCountVaList(var, &total_count);
    va_end(var);

    /*
     * now package it up so we can send it along
     */
    Va_start(var, ic);
    _XIMVaToNestedList(var, total_count, &args);
    va_end(var);

    ret = (*ic->methods->get_values) (ic, args);
    if (args) Xfree((char *)args);
    return ret;
}

#if NeedVarargsPrototypes
char *
XSetICValues(XIC ic, ...)
#else
char *
XSetICValues(ic, va_alist)
    XIC ic;
    va_dcl
#endif
{
    va_list var;
    int     total_count;
    XIMArg *args;
    char   *ret;

    if (!ic->core.im)
	return (char *) NULL;

    /*
     * so count the stuff dangling here
     */
    Va_start(var, ic);
    _XIMCountVaList(var, &total_count);
    va_end(var);

    /*
     * now package it up so we can send it along
     */
    Va_start(var, ic);
    _XIMVaToNestedList(var, total_count, &args);
    va_end(var);

    ret = (*ic->methods->set_values) (ic, args);
    if (args) Xfree((char *)args);
    return ret;
}

/*
 * Require the input manager to focus the focus window attached to the ic
 * argument.
 */
void
XSetICFocus(ic)
    XIC ic;
{
  if (ic->core.im)
      (*ic->methods->set_focus) (ic);
}

/*
 * Require the input manager to unfocus the focus window attached to the ic
 * argument.
 */
void
XUnsetICFocus(ic)
    XIC ic;
{
  if (ic->core.im)
      (*ic->methods->unset_focus) (ic);
}

/*
 * Return the XIM associated with the input context.
 */
XIM
XIMOfIC(ic)
    XIC ic;
{
    return ic->core.im;
}

char *XmbResetIC(ic)
    XIC ic;
{
    if (ic->core.im)
	return (*ic->methods->mb_reset)(ic);
    return (char *)NULL;
}

wchar_t *XwcResetIC(ic)
    XIC ic;
{
    if (ic->core.im)
	return (*ic->methods->wc_reset)(ic);
    return (wchar_t *)NULL;
}

int
XmbLookupString(ic, ev, buffer, nbytes, keysym, status)
    XIC ic;
    register XKeyEvent *ev;
    char *buffer;
    int nbytes;
    KeySym *keysym;
    Status *status;
{
    if (ic->core.im)
	return (*ic->methods->mb_lookup_string) (ic, ev, buffer, nbytes,
						 keysym, status);
    return XLookupNone;
}

int
XwcLookupString(ic, ev, buffer, nchars, keysym, status)
    XIC ic;
    register XKeyEvent *ev;
    wchar_t *buffer;
    int nchars;
    KeySym *keysym;
    Status *status;
{
    if (ic->core.im)
	return (*ic->methods->wc_lookup_string) (ic, ev, buffer, nchars,
						 keysym, status);
    return XLookupNone;
}
