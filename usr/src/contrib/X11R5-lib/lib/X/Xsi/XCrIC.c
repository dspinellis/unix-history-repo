/*
 * $XConsortium: XCrIC.c,v 1.34 92/07/29 13:54:58 rws Exp $
 */

/*
 * Copyright 1990, 1991 by OMRON Corporation
 * Copyright 1991 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of OMRON and MIT not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  OMRON and MIT make no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * OMRON AND MIT DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL OMRON OR MIT BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE. 
 *
 *	Author:	Seiji Kuwari	OMRON Corporation
 *				kuwa@omron.co.jp
 *				kuwa%omron.co.jp@uunet.uu.net
 */				

#include <stdio.h>
#include "Xlibint.h"
#include "Xi18nint.h"
#include "XIMlibint.h"
#if NeedVarargsPrototypes
# include <stdarg.h>
# define Va_start(a,b) va_start(a,b)
#else
# include <varargs.h>
# define Va_start(a,b) va_start(a)
#endif

#if __STDC__ && !defined(NORCONST)
#define RConst const
#else
#define RConst /**/
#endif

/*
 * Free the input context.
 */
void
_XipDestroyIC(supic)
    XIC supic;
{
    XipIC ic = (XipIC)supic;
    XipIM im = (XipIM)ic->core.im;
    /* check xim and ic*/
    if (im->fd >= 0) {
	ximDestroyICReq	req;
	ximEventReply	reply;

	req.reqType = XIM_DestroyIC;
	req.length = sz_ximDestroyICReq;
	req.xic = ic->icid;
	if ((_XipWriteToIM(im, (char *)&req, sz_ximDestroyICReq) >= 0) &&
	    (_XipFlushToIM(im) >= 0)) {
	    for (;;) {
		if ((_XipReadFromIM(im, (char *)&reply, sz_ximEventReply) < 0)
		    || (reply.state == 0xffff)) {
		    return;
		}
		if (reply.detail == XIM_CALLBACK) {
		    /*
		     * Call the callback routines.
		     */
		    if (_XipCallCallbacks(ic) < 0) {
			return;
		    }
		} else {
		    break;
		}
	    }
	}
    }

    _XUnregisterFilter(im->core.display, ic->core.focus_window,
		       ic->prototype_filter, (XPointer)ic);

    if (ic->mb) _XlcFreeLocale(ic->mb);
    if (ic->wc) _XlcFreeLocale(ic->wc);
#ifdef	XML
    if (ic->xlc_num > 0) {
	int i;

	for (i = 0; i < ic->xlc_num; i++) {
	    _XlcFreeLocale(ic->mb_temp[i]);
	    _XlcFreeLocale(ic->wc_temp[i]);
	}
    }
    if (ic->mb_temp) Xfree(ic->mb_temp);
    if (ic->wc_temp) Xfree(ic->wc_temp);
    if (ic->values.using_language) Xfree(ic->values.using_language);
    if (ic->values.current_language) Xfree(ic->values.current_language);
#endif	/* XML */
    if (ic->ct_buf) Xfree(ic->ct_buf);
    if (ic->wc_buf) Xfree(ic->wc_buf);
    (void) _XipFreeAllICQueue(ic);
}

static RConst XICMethodsRec ic_methods = {
    _XipDestroyIC,
    _XipSetICFocus,
    _XipUnsetICFocus,
    _XipSetICValues,
    _XipGetICValues,
    _XipmbResetIC,
    _XipwcResetIC,
    _XipmbLookupString,
    _XipwcLookupString
};

static Status
_StringToPixel(display, colormap, name, pixel)
    Display *display;
    Colormap colormap;
    char *name;
    unsigned long *pixel;
{
    XColor c, e_c;
    Status status;

    if (name[0] == '#') {
	status = XParseColor(display, colormap, name, &c);
	if (status != 0) {
	    status = XAllocColor(display, colormap, &c);
	}
    } else {
	status = XAllocNamedColor(display, colormap, name, &c, &e_c);
    }
    if (status == 0) {
	return(-1);
    } else {
	*pixel = c.pixel;
	return(0);
    }
}

unsigned long
_XipReadRdb(display, ic, mask, rdb, res_name, res_class)
    Display *display;
    XipIC ic;
    unsigned long mask;
    XrmDatabase rdb;
    char *res_name;
    char *res_class;
{
    XipIM im = ipIMofIC(ic);
    unsigned long ret_mask = 0;
    char name_prefix[256], class_prefix[256];
    char res_name_buf[256], res_class_buf[256];
    /*
    char def_fontset[256];
    */
#ifdef	XML
    char def_language[256];
#endif	/* XML */
    char def_fg_name[256], def_bg_name[256];
    unsigned long def_fg, def_bg;
    char *str_type[20];
    XrmValue value;
    XIMArg args[8], pre_args[4], st_args[4];
    int count = 0, pre_count = 0, st_count = 0;
    Colormap colormap, def_colormap;
    XWindowAttributes win_info;

    if (rdb == NULL) {
	return(0);
    }
    if (ic->core.client_window) {
	XGetWindowAttributes(display, ic->core.client_window, &win_info);
    } else {
	XGetWindowAttributes(display,
			     RootWindow(display, DefaultScreen(display)),
			     &win_info);
    }
    def_colormap = win_info.colormap;
    if (res_name == NULL || *res_name == '\0'
	|| res_class == NULL || *res_class == '\0') {
	strcpy(name_prefix, "*.");
	strcpy(class_prefix, "*.");
    } else {
	strcpy(name_prefix, res_name);
	strcpy(class_prefix, res_class);
	strcat(name_prefix, ".");
	strcat(class_prefix, ".");
    }
    if (!(mask & ((1 << ICForeground) | (1 << (ICForeground + StatusOffset))))){
	strcpy(res_name_buf, name_prefix);
	strcpy(res_class_buf, class_prefix);
	strcat(res_name_buf, "foreground");
	strcat(res_class_buf, "Foreground");
	if (XrmGetResource(rdb, res_name_buf, res_class_buf,
			   str_type, &value) == True) {
	    strncpy(def_fg_name, value.addr, (int)value.size);
	    if (ic->core.preedit_attr.colormap) {
		colormap = ic->core.preedit_attr.colormap;
	    } else {
		colormap = def_colormap;
	    }
	    if (_StringToPixel(display, colormap, def_fg_name, &def_fg) == 0) {
		if (!(mask & (1 << ICForeground))) {
		    pre_args[pre_count].name = (char *)XNForeground;
		    pre_args[pre_count].value = (XPointer)def_fg;
		    pre_count++;
		}
		if (!(mask & (1 << (ICForeground + StatusOffset)))) {
		    st_args[st_count].name = (char *)XNForeground;
		    st_args[st_count].value = (XPointer)def_fg;
		    st_count++;
		}
	    } else {
		fprintf(stderr, "XIM: Could not Alloc color \"%s\".",
			def_fg_name);
	    }
	}
    }
    if (!(mask & ((1 << ICBackground) | (1 << (ICBackground + StatusOffset))))){
	strcpy(res_name_buf, name_prefix);
	strcpy(res_class_buf, class_prefix);
	strcat(res_name_buf, "background");
	strcat(res_class_buf, "Background");
	if (XrmGetResource(rdb, res_name_buf, res_class_buf,
			   str_type, &value) == True) {	
	    strncpy(def_bg_name, value.addr, (int)value.size);	
	    if (ic->core.preedit_attr.colormap) {
		colormap = ic->core.preedit_attr.colormap;
	    } else {
		colormap = def_colormap;
	    }
	    if (_StringToPixel(display, colormap, def_bg_name, &def_bg) == 0) {
		if (!(mask & (1 << ICBackground))) {
		    pre_args[pre_count].name = (char *)XNBackground;
		    pre_args[pre_count].value = (XPointer)def_bg;
		    pre_count++;
		}
		if (!(mask & (1 << (ICBackground + StatusOffset)))) {
		    st_args[st_count].name = (char *)XNBackground;
		    st_args[st_count].value = (XPointer)def_bg;
		    st_count++;
		}
	    } else {
		fprintf(stderr, "XIM: Could not Alloc color \"%s\".",
			def_bg_name);
	    }	
	}	
    }
    /*
    if (!(mask & ((1 << ICFontSet) | (1 << (ICFontSet + StatusOffset))))) {
	strcpy(res_name_buf, name_prefix);	
	strcpy(res_class_buf, class_prefix);
	strcat(res_name_buf, "fontSet");
	strcat(res_class_buf, "FontSet");
	if (XrmGetResource(rdb, res_name_buf, res_class_buf,
			   str_type, &value) == True) {
	    strncpy(def_fontset, value.addr, value.size);
	    if (!(mask & (1 << ICFontSet))) {
		pre_args[pre_count].name = (char *)XNFontSet;
		pre_args[pre_count].value = (XPointer)def_fontset;
		pre_count++;
	    }
	    if (!(mask & (1 << (ICFontSet + StatusOffset)))) {
		st_args[st_count].name = (char *)XNForeground;
		st_args[st_count].value = (XPointer)def_fg;
		st_count++;
	    }
	}
    }
    */
#ifdef	XML
    if (!(mask & (1 << ICUsingLanguage))) {
	strcpy(res_name_buf, name_prefix);	
	strcpy(res_class_buf, class_prefix);
	strcat(res_name_buf, "usingLanguage");
	strcat(res_class_buf, "usingLanguage");
	if (XrmGetResource(rdb, res_name_buf, res_class_buf,
			   str_type, &value) == True) {
	    strncpy(def_language, value.addr, (int)value.size);
	    args[count].name = (char *)XNUsingLanguage;
	    args[count].value = (XPointer)def_language;
	    count++;
	}
    }
#endif	/* XML */
    if(pre_count) {
	pre_args[pre_count].name = (char *)NULL;
	(void)_XipICSetAttrValues(im, pre_args, &ic->core.preedit_attr,
				  &ret_mask, 0);
    }
    if(st_count) {
	st_args[st_count].name = (char *)NULL;
	(void)_XipICSetAttrValues(im, st_args, &ic->core.status_attr,
				  &ret_mask, StatusOffset);
    }
    if (count) {
	args[count].name = (char *)NULL;
	(void)_XipICSetValues(ic, args, &ret_mask);
    }
    return(ret_mask);
}

Bool
_XipCreateDefIC(im)
    XipIM im;
{
    im->default_ic = (XipIC)Xcalloc(1, sizeof(XipICRec));
    if (im->default_ic == NULL) {
	return False;
    }
    im->default_ic->core.im = (XIM)im;

    im->default_mask = _XipReadRdb(im->core.display, im->default_ic,
				(unsigned long)0,
				im->core.rdb, im->core.res_name,
				im->core.res_class);
    return True;
}


/*
 * Create an input context within the input method, 
 * and return a pointer the input context ti the caller.
 */
XIC
_XipCreateIC(supim, args)
    XIM supim;
    XIMArg *args;
{
    XipIM		im = (XipIM)supim;
    XipIC		ic;
    ximCreateICReq	req;
    ximCreateICReply	reply;
    ximEventReply	reply1;
    unsigned long	mask;
    extern Bool		_XipBackEndFilter();
#ifdef	XML
    char		*p, **nls_list, **l;
    unsigned int	i, n = 0;
    XLocale		xlc;
#endif	/* XML */

    /*
     * If im is not specified or the file descripter is not available,
     * return NULL.
     */
    if (im->fd < 0) {
	return(NULL);
    }

    if ((ic = (XipIC)Xcalloc(1, sizeof(XipICRec))) == NULL) {
	return(NULL);
    }

    mask = im->default_mask;
    if (mask)
	bcopy((char *)&im->default_ic->values, (char *)&ic->values,
	      sizeof(struct _ICValues));

    ic->methods = (XICMethods) &ic_methods;
    ic->prototype_filter = _XipBackEndFilter;
    ic->core.im = supim;
    (void)_XipICSetValues(ic, args, &mask);

    req.reqType = XIM_CreateIC;
    req.length = sz_ximCreateICReq + strlen(im->client_data);

    if ((_XipWriteToIM(im, (char *)&req, sz_ximCreateICReq) < 0) ||
	(_XipWriteToIM(im, im->client_data, strlen(im->client_data)) < 0) ||
	(_XipFlushToIM(im) < 0)) {
	return(NULL);
    }

    if (im->core.rdb && ic->values.res_name && ic->values.res_class) {
	mask |= _XipReadRdb(im->core.display, ic, mask, im->core.rdb,
			 ic->values.res_name, ic->values.res_class);
    }
	
#ifdef	XML
    if (im->xlc != NULL) {
#endif	/* XML */
	ic->mb = _XlcDupLocale(im->xlc);
	ic->wc = _XlcDupLocale(im->xlc);
#ifdef	XML
	ic->xlc_num = 0;
	ic->mb_temp = NULL;
	ic->wc_temp = NULL;
	n = strlen(im->xlc->xlc_db->lc_name) + 1;
	if ((p = Xmalloc(n)) == NULL) return(NULL);
	strcpy(p, im->xlc->xlc_db->lc_name);
	p[n - 1] = 0;
	ic->values.using_language = p;
	mask |= (1 << ICUsingLanguage);
    } else {
	ic->mb = NULL;
	ic->wc = NULL;
	ic->xlc_num = 0;
	if ((ic->mb_temp = (XLocale*)Xmalloc(sizeof(XLocale) * 32)) == NULL) {
	    return(NULL);
	}
	if ((ic->wc_temp = (XLocale*)Xmalloc(sizeof(XLocale) * 32)) == NULL) {
	    return(NULL);
	}
	_XlcListLocale(&nls_list);
	for (l = nls_list; *l; l++) {
	    xlc = _XlcMakeLocale(*l);
	    if (!xlc)
		continue;
	    ic->mb_temp[ic->xlc_num] = xlc;
	    ic->wc_temp[ic->xlc_num] = _XlcDupLocale(xlc);
	    n += strlen(ic->mb_temp[ic->xlc_num]->xlc_db->lc_name) + 1;
	    ic->xlc_num++;
	}
	Xfree((char *)nls_list);
	if ((p = Xmalloc(n)) == NULL) return(NULL);
	p[0] = '\0';
	for (i = 0; i < ic->xlc_num; i++) {
	    strcat(p, ic->mb_temp[i]->xlc_db->lc_name);
	    strcat(p, ";");
	}
	p[n - 1] = '\0';
	ic->values.using_language = p;
	mask |= (1 << ICUsingLanguage);
    }
#endif /* XML */


    /*
     * Attempt to send IC data to the input manager. If sending failed,
     * free IC structure and return NULL.
     */
    _XipSendICValues(ic, mask);
    ic->max_of_ct = ic->max_of_wc = 0;
    ic->ct_buf = NULL;
    ic->wc_buf = NULL;
    for (;;) {
	if ((_XipReadFromIM(im, (char *)&reply1, sz_ximEventReply) < 0) ||
	    (reply1.state == 0xffff)) {
	    goto _err_ret;
	}
	if (reply1.detail == XIM_CALLBACK) {
	    /*
	     * Call the callback routines.
	     */
	    if (_XipCallCallbacks(ic) < 0) {
		goto _err_ret;
	    }
	} else if (reply1.detail == XIM_IC) {
	    if (_XipReadFromIM(im, (char *)&reply, sz_ximCreateICReply) < 0) {
		goto _err_ret;
	    }
	    if (reply.state != 0) {
		goto _err_ret;
	    }
	    ic->icid = reply.xic;
	    break;
	} else {
	    break;
	}
    }

    /*
     * Attempt to get current IC data from the input manager.
     */
    _XipReceiveICValues(ic, (unsigned long)ICAllMask);

#ifdef	XML
    if (ic->xlc_num > 0) {
	for (i = 0; i < ic->xlc_num; i++) {
	    if (!strcmp(ic->values.current_language,
			ic->mb_temp[i]->xlc_db->lc_name)) {
		ic->mb = ic->mb_temp[i];
		ic->wc = ic->wc_temp[i];
		break;
	    }
	}
	if (ic->mb == NULL) {
	    ic->mb = ic->mb_temp[0];
	    ic->wc = ic->wc_temp[0];
	}
    }
#endif	/* XML */
    return((XIC)ic);

_err_ret:
    if (ic->mb) _XlcFreeLocale(ic->mb);
    if (ic->wc) _XlcFreeLocale(ic->wc);
#ifdef  XML
    if (ic->xlc_num > 0) {
	for (i = 0; i < ic->xlc_num; i++) {
	    _XlcFreeLocale(ic->mb_temp[i]);
	    _XlcFreeLocale(ic->wc_temp[i]);
	}
    }
    if (ic->mb_temp) Xfree(ic->mb_temp);
    if (ic->wc_temp) Xfree(ic->wc_temp);
    if (ic->values.using_language) Xfree(ic->values.using_language);
#endif  /* XML */
    Xfree((char *)ic);
    return(NULL);
}

/*
 * Reset the input context. 
 */
wchar_t *
_XipwcResetIC(ic)
    XIC ic;		/* specified the input context to reset*/
{
    XipIM im = ipIMofIC((XipIC)ic);
    ximResetICReq	req;
    ximEventReply	reply;

    /*
     * If im is not specified or the file descripter is not available,
     * return NULL.
     */
    if (im->fd < 0) {
	return((wchar_t *)NULL);
    }
    req.reqType = XIM_ResetIC;
    req.length = sz_ximResetICReq;
    req.xic = ((XipIC)ic)->icid;
    if ((_XipWriteToIM(im, (char *)&req, sz_ximResetICReq) >= 0) &&
	(_XipFlushToIM(im) >= 0)) {
	for (;;) {
	    if ((_XipReadFromIM(im, (char *)&reply, sz_ximEventReply) < 0) ||
		(reply.state == 0xffff)) {
		return((wchar_t *)NULL);
	    }
	    if (reply.detail == XIM_CALLBACK) {
		/*
		 * Call the callback routines.
		 */
		if (_XipCallCallbacks(ic) < 0) {
		    return((wchar_t *)NULL);
		}
	    } else {
		break;
	    }
	}
    }
    return((wchar_t *)NULL);
}

char *
_XipmbResetIC(ic)
    XIC ic;		/* specified the input context to reset*/
{
    XipIM im = ipIMofIC((XipIC)ic);
    ximResetICReq	req;
    ximEventReply	reply;

    /*
     * If im is not specified or the file descripter is not available,
     * return NULL.
     */
    if (im->fd < 0) {
	return(NULL);
    }
    req.reqType = XIM_ResetIC;
    req.length = sz_ximResetICReq;
    req.xic = ((XipIC)ic)->icid;
    if ((_XipWriteToIM(im, (char *)&req, sz_ximResetICReq) >= 0) &&
	(_XipFlushToIM(im) >= 0)) {
	for (;;) {
	    if ((_XipReadFromIM(im, (char *)&reply, sz_ximEventReply) < 0) ||
		(reply.state == 0xffff)) {
		return(NULL);
	    }
	    if (reply.detail == XIM_CALLBACK) {
		/*
		 * Call the callback routines.
		 */
		if (_XipCallCallbacks(ic) < 0) {
		    return(NULL);
		}
	    } else {
		break;
	    }
	}
    }
    return(NULL);
}

#ifdef	XML
void
_XipChangeLocale(ic, lc_name)
    XipIC ic;
    char *lc_name;
{
    XLocale xlc;
    int i;

    for (i = 0; i < ic->xlc_num; i++) {
	if ((!strcmp(lc_name, ic->mb_temp[i]->lc_lang)) ||
	    (!strcmp(lc_name, ic->mb_temp[i]->xlc_db->lc_name))) {
	    ic->mb = ic->mb_temp[i];
	    ic->wc = ic->wc_temp[i];
	    return;
	}
    }
    xlc = _XlcMakeLocale(lc_name);
    if (xlc) {
	ic->mb = ic->mb_temp[ic->xlc_num] = xlc;
	ic->wc = ic->wc_temp[ic->xlc_num] = _XlcDupLocale(xlc);
	ic->xlc_num++;
    }
}
#endif	/* XML */
