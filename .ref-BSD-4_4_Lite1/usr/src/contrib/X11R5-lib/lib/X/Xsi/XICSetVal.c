/*
 * $XConsortium: XICSetVal.c,v 1.29 92/07/28 17:54:11 rws Exp $
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

#include "Xlibint.h"
#include "Xi18nint.h"
#include "XIMlibint.h"
#include <X11/Xos.h>
#if NeedVarargsPrototypes
# include <stdarg.h>
# define Va_start(a,b) va_start(a,b)
#else
# include <varargs.h>
# define Va_start(a,b) va_start(a)
#endif

static void
CopyFromArg(src, dst, size)
    XPointer src;
    XPointer *dst;
    register unsigned int size;
{
    if (size > sizeof(XPointer))
      bcopy((char *)src, (char *)dst, (int)size);
    else {
	union {
	    long	longval;
	    short	shortval;
	    char	charval;
	    char*	charptr;
	    XPointer	ptr;
	} u;
	char *p = (char*)&u;
	if (size == sizeof(long))		u.longval = (long)src;
	else if (size == sizeof(short))		u.shortval = (short)src;
	else if (size == sizeof(char))		u.charval = (char)src;
	else if (size == sizeof(XPointer))	u.ptr = (XPointer)src;
	else if (size == sizeof(char *))	u.charptr = (char *)src;
	else					p = (char*)&src;

	bcopy(p, (char *) dst, (int) size);
    }
}

/*
 * Up date a attributes of input context depended on the nested list.
 */
char *
_XipICSetAttrValues(im, args, attr, mask, offset)
    register XipIM im;
    register XIMArg *args;
    ICAttributes *attr;
    unsigned long *mask;
    int offset;
{
    register XIMArg		*arg;
    register int		i;
    register XrmName		Name;
    register XIMrmResourceList	xrmres;
    register unsigned int	num_resources = im->core.ic_num_attr_resources;
    XrmQuark			spot_name =
				    XrmPermStringToQuark(XNSpotLocation);

    for (arg = args; arg && arg->name && *(arg->name) ; arg++) {
	Name = XrmStringToName(arg->name);
	for (xrmres = im->core.ic_attr_resources, i = 0;
	     i < num_resources; i++, xrmres++) {
	    if (Name == xrmres->xrm_name) {
		if (!(xrmres->mode & IMResourceWrite)) {
		    return(arg->name);
		}
		/*
		 * Size of XPoint is not bigger than XPointer, so
		 * could not call CopyFromArg();
		 */
		if (Name == spot_name) {
		   XPoint *p = (XPoint*)arg->value;
		   attr->spot_location.x = p->x;
		   attr->spot_location.y = p->y;
		} else {
		    (void) CopyFromArg(arg->value,
				       (char *)attr - xrmres->xrm_offset - 1,
				       (unsigned int)xrmres->xrm_size);
		}
		if (xrmres->mask >= 0) {
		    *mask |= (1L << (xrmres->mask + offset));
		}
		break;
	    }
	}
    }
    return(NULL);
}

/*
 * Up date input context depended on the argument list.
 */
char *
_XipICSetValues(ic, args, mask)
    register XipIC ic;
    register XIMArg *args;
    unsigned long *mask;
{
    XipIM			im = ipIMofIC(ic);
    register XIMArg		*arg;
    register int		i;
    register XrmName		Name;
    register XIMrmResourceList	xrmres;
    register unsigned int	num_resources = im->core.ic_num_resources;
    char 			*err;
    XrmQuark			preedit_name =
				    XrmPermStringToQuark(XNPreeditAttributes);
    XrmQuark			status_name =
				    XrmPermStringToQuark(XNStatusAttributes);
#ifdef XML
    XrmQuark			use_lang =
				    XrmPermStringToQuark(XNUsingLanguage);
    XrmQuark			cur_lang =
				    XrmPermStringToQuark(XNCurrentLanguage);
    int len;
    char *p;
#endif /* XML */

    for (arg = args; arg && arg->name && *(arg->name); arg++) {
	Name = XrmStringToName(arg->name);
	for (xrmres = im->core.ic_resources, i = 0;
	     i < num_resources; i++, xrmres++) {
	    if (Name == xrmres->xrm_name) {
		if (Name == preedit_name) {
		    if ((err = _XipICSetAttrValues(im, (XIMArg *)arg->value,
						   &ic->core.preedit_attr,
						   mask, 0)) != NULL) {
			return(err);
		    }
		} else if (Name == status_name) {
		    if ((err = _XipICSetAttrValues(im, (XIMArg *)arg->value,
						   &ic->core.status_attr,
						   mask, StatusOffset)) != NULL) {
			return(err);
		    }
		} else {
		    if (!(xrmres->mode & IMResourceWrite)) {
			return(arg->name);
		    }
#ifdef XML
		    if (Name == use_lang || Name == cur_lang) {
		       len = strlen((char *)arg->value);
		       p = Xmalloc(len + 1);
		       strcpy(p, (char *)arg->value);
		       arg->value = (XPointer)p;
		    }
#endif /* XML */
		    (void) CopyFromArg(arg->value,
				       (char *)ic - xrmres->xrm_offset - 1,
				       (unsigned int)xrmres->xrm_size);
		    if (xrmres->mask >= 0) {
			*mask |= (1L << (xrmres->mask));
		    }
	        }
		break;
	    }
	}
	if (i >= num_resources) return(arg->name);
    }
    return(NULL);
}

static int
_XipSendICAttributes(im, attr, mask, offset)
    XipIM im;
    register ICAttributes *attr;
    unsigned long mask;
    int offset;
{
    ximICAttributesReq req;
    register int i;
    register char *p = NULL;
    register char **font_name_list;

    req.area_x = attr->area.x;
    req.area_y = attr->area.y;
    req.area_width = attr->area.width;
    req.area_height = attr->area.height;
    req.spot_x = attr->spot_location.x;
    req.spot_y = attr->spot_location.y;
    req.colormap = attr->colormap;
    req.std_colormap = attr->std_colormap;
    req.foreground = attr->foreground;
    req.background = attr->background;
    req.pixmap = attr->background_pixmap;
    req.line_space = attr->line_space;
    req.cursor = attr->cursor;
    if (mask & (1 << (ICFontSet + offset))) {
	font_name_list = attr->fontset->core.font_name_list;
	req.nfonts = attr->fontset->core.num_of_fonts;
	req.nbytes = 0;
	for (i = 0; i < (int)req.nfonts; i++) {
	    req.nbytes += (strlen(font_name_list[i]) + 1);
	}
	if ((p = Xmalloc((unsigned)(req.nbytes + 1))) == NULL) return(-1);
	p[0] = '\0';
	for (i = 0; i < (int)req.nfonts; i++) {
	    strcat(p, font_name_list[i]);
	    strcat(p, ",");
	}
	p[req.nbytes - 1] = '\0';
    } else {
	req.nfonts = 0;
	req.nbytes = 0;
    }

    if (_XipWriteToIM(im, (char *)&req, sz_ximICAttributesReq) < 0) {
	return(-1);
    }
    if (req.nbytes > 0) {
	if (_XipWriteToIM(im, p, req.nbytes) < 0) {
	    return(-1);
	}
	Xfree(p);
    }
    return(0);
}

int
_XipSendICValues(ic, mask)
    register XipIC ic;
    unsigned long mask;
{
    XipIM im = ipIMofIC(ic);
    ximICValuesReq req;
    register char *p, *p2;
#ifdef	XML
    register int i;
#endif	/* XML */
    register unsigned int n = 0;

    req.mask = mask;
    req.c_window = ic->core.client_window;
    req.input_style = ic->core.input_style;
    req.focus_window = ic->core.focus_window;
    req.filter_events = ic->core.filter_events;
    req.max_keycode = 0;	/* noused */
#ifdef	XML
    if (mask & (1 << ICUsingLanguage)) {
	if (ic->xlc_num == 0) {
#endif	/* XML */
	    n = strlen(ic->mb->xlc_db->lc_name) + strlen(ic->mb->lc_lang) + 2;
	    if ((p = Xmalloc(n)) == NULL) return(-1);
	    strcpy(p, ic->mb->xlc_db->lc_name);
	    strcat(p, ",");
	    strcat(p, ic->mb->lc_lang);
	    p[n - 1] = '\0';
	    req.nbytes = strlen(p);
#ifdef	XML
	} else {
	    for (i = 0; i < ic->xlc_num; i++) {
		n += (strlen(ic->mb_temp[i]->xlc_db->lc_name)
		      + strlen(ic->mb_temp[i]->lc_lang) + 2);
	    }
	    if (n > 0) {
		if ((p = Xmalloc(n)) == NULL) return(-1);
		p[0] = '\0';
		for (i = 0; i < ic->xlc_num; i++) {
		    strcat(p, ic->mb_temp[i]->xlc_db->lc_name);
		    strcat(p, ",");
		    strcat(p, ic->mb_temp[i]->lc_lang);
		    strcat(p, ";");
		}
		p[n - 1] = '\0';
		req.nbytes = strlen(p);
	    } else {
		p = NULL;
		req.nbytes = 0;
	    }
	}
    } else {
	p = NULL;
	req.nbytes = 0;
    }
#endif	/* XML */

#ifdef	XML
    if (mask & (1 << ICCurrentLanguage)) {
	p2 = ic->values.current_language;
	req.nbytes2 = strlen(p2);
    } else {
	p2 = NULL;
	req.nbytes2 = 0;
    }
#else	/* XML */
    req.nbytes2 = strlen(ic->mb->xlc_db->lc_name);
    p2 = ic->mb->xlc_db->lc_name;
#endif	/* XML */

    if (_XipWriteToIM(im, (char *)&req, sz_ximICValuesReq) < 0) {
	return(-1);
    }
    if (req.nbytes > 0) {
	if (_XipWriteToIM(im, (char *)p, req.nbytes) < 0) {
	    return(-1);
	}
    }
    if (req.nbytes2 > 0) {
	if (_XipWriteToIM(im, (char *)p2, req.nbytes2) < 0) {
	    return(-1);
	}
    }
    if ((_XipSendICAttributes(im, &ic->core.preedit_attr, mask, 0) < 0) ||
	(_XipSendICAttributes(im, &ic->core.status_attr, mask,
			    StatusOffset) < 0) ||
	(_XipFlushToIM(im) < 0)) {
	return(-1);
    }
    return(0);
}

char *
_XipSetICValues(supic, args)
    XIC supic;
    XIMArg *args;
{
    XipIC		ic = (XipIC)supic;
    XipIM		im = ipIMofIC(ic);
    unsigned long	mask = 0L;
    Window		old_focus_window;
    ximChangeICReq	req;
    ximEventReply	reply;
    char		*err = NULL;

    if (im->fd < 0) return(NULL);
    old_focus_window = ic->core.focus_window;

    err = _XipICSetValues(ic, args, &mask);
    if (err)
	return(err);
    if (im->core.rdb &&
	(mask & (1L << ICResourceClass | 1L << ICResourceName))) {
	mask |= _XipReadRdb(im->core.display, ic, mask, im->core.rdb,
			    ic->values.res_name, ic->values.res_class);
    }
    
    if (mask & (1L << ICFocusWindow)) {
	_XUnregisterFilter(im->core.display, old_focus_window,
			   ic->prototype_filter, (XPointer)ic);
	_XRegisterFilterByMask(im->core.display, ic->core.focus_window,
			       KeyPressMask,
			       ic->prototype_filter, (XPointer)ic);
    }
#ifdef	XML
    if (mask & (1L << ICCurrentLanguage)) {
	_XipChangeLocale(ic, ic->values.current_language);
    }
#endif	/* XML */

    /*
     * Attempt to send IC data to the input manager. If sending failed,
     * generate -1.
     */
    req.reqType = XIM_ChangeIC;
    req.length = sz_ximChangeICReq;
    req.xic = ic->icid;

    if ((_XipWriteToIM(im, (char *)&req, sz_ximChangeICReq) >= 0) &&
	(_XipSendICValues(ic, mask) >= 0)) {
	for (;;) {
	    if ((_XipReadFromIM(im, (char *)&reply, sz_ximEventReply) < 0)
		|| (reply.state == 0xffff)) {
		return("unknownError"); /* XXX */
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
