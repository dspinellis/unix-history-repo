/*
 * $XConsortium: XICGetVal.c,v 1.16 91/12/02 17:29:37 rws Exp $
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
#if NeedVarargsPrototypes
# include <stdarg.h>
# define Va_start(a,b) va_start(a,b)
#else
# include <varargs.h>
# define Va_start(a,b) va_start(a)
#endif

/*
 * Get a attributes of input context depended on the nested argument list.
 */
static char *
_ICGetAttrValues(im, args, attr, mask, offset, mask_only)
    register XipIM im;
    register XIMArg *args;
    ICAttributes *attr;
    unsigned long *mask;
    int offset;
    char mask_only;
{
    register 			XIMArg *arg;
    register int		i;
    register XrmName		Name;
    register XIMrmResourceList	xrmres;
    XPointer 			*location;
    register unsigned int	num_resources = im->core.ic_num_attr_resources;
    XrmQuark			spot_name =
				    XrmPermStringToQuark(XNSpotLocation);
    XrmQuark			area_needed_name =
				    XrmPermStringToQuark(XNAreaNeeded);
    XrmQuark			area_name =
				    XrmPermStringToQuark(XNArea);

    for (arg = args; arg && arg->name && *(arg->name) ; arg++) {
	Name = XrmStringToName(arg->name);
	for (xrmres = im->core.ic_attr_resources, i = 0;
	     i < num_resources; i++, xrmres++) {
	    if (Name == xrmres->xrm_name) {
		if (!(xrmres->mode & IMResourceRead)) {
		    return(arg->name);
		}
		if (!mask_only) {
		    if (Name == area_needed_name || Name == area_name ||
						    Name == spot_name) {
			location = (XPointer*)arg->value;
			*location = (XPointer)Xmalloc(xrmres->xrm_size);
		    } else {
			location = (XPointer *)&arg->value;
		    }
		    (void)_XCopyToArg((char *)attr - xrmres->xrm_offset - 1,
				       location,
				       (unsigned int)xrmres->xrm_size);
		}
		if (xrmres->mask >= 0) {
		    *mask |= (1L << (xrmres->mask + offset));
		}
		break;
	    }
	}
	if (i >= num_resources) return(arg->name);
    }
    return(NULL);
}

/*
 * Get input context specified by the nested argument list.
 */
char *
_XipICGetValues(ic, args, mask, mask_only)
    register XipIC ic;
    register XIMArg *args;
    unsigned long *mask;
    char mask_only;
{
    XipIM			im = ipIMofIC(ic);
    register XIMArg		*arg;
    register int		i;
    register XrmName		Name;
    register XIMrmResourceList	xrmres;
    register unsigned int	num_resources = im->core.ic_num_resources;
    char			*err;
    XrmQuark			preedit_name =
				    XrmPermStringToQuark(XNPreeditAttributes);
    XrmQuark			status_name =
				    XrmPermStringToQuark(XNStatusAttributes);

    for (arg = args; arg && arg->name && *(arg->name); arg++) {
	Name = XrmStringToName(arg->name);
	for (xrmres = im->core.ic_resources, i = 0;
	     i < num_resources; i++, xrmres++) {
	    if (Name == xrmres->xrm_name) {
		if (Name == preedit_name) {
		    if ((err = _ICGetAttrValues(im, (XIMArg *)arg->value,
						&ic->core.preedit_attr,
						mask, 0, mask_only)) != NULL) {
			return(err);
		    }
		} else if (Name == status_name) {
		    if ((err = _ICGetAttrValues(im, (XIMArg *)arg->value,
						&ic->core.status_attr,
						mask, StatusOffset,
						mask_only)) != NULL) {
			return(err);
		    }
		} else {
		    if (!(xrmres->mode & IMResourceRead)) {
			return(arg->name);
		    }
		    if (!mask_only) {
			(void) _XCopyToArg((char *)ic - xrmres->xrm_offset - 1,
					   &arg->value,
					   (unsigned int)xrmres->xrm_size);
		    }
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
_ReceiveICAttrValues(im, attr, mask, offset)
    XipIM im;
    register ICAttributes *attr;
    unsigned long mask;
    int offset;
{
    ximICAttributesReq	reply;

    if (_XipReadFromIM(im, (char *)&reply, sz_ximICAttributesReq) < 0)
	return(-1);
    
    if (mask & (1L << (ICArea + offset))) {
	attr->area.x = reply.area_x;
	attr->area.y = reply.area_y;
	attr->area.width = reply.area_width;
	attr->area.height = reply.area_height;
    }
    if (mask & (1L << (ICAreaNeeded + offset))) {
	attr->area_needed.width = reply.areaneeded_width;
	attr->area_needed.height = reply.areaneeded_height;
    }
    if (mask & (1L << ICSpotLocation + offset)) {
	attr->spot_location.x = reply.spot_x;
	attr->spot_location.y = reply.spot_y;
    }
    if (mask & (1L << (ICColormap + offset))) {
	attr->colormap = reply.colormap;
    }
    if (mask & (1L << (ICStdColormap + offset))) {
	attr->std_colormap = reply.std_colormap;
    }
    if (mask & (1L << (ICForeground + offset))) {
	attr->foreground = reply.foreground;
    }
    if (mask & (1L << (ICBackground + offset))) {
	attr->background = reply.background;
    }
    if (mask & (1L << (ICBackgroundPixmap + offset))) {
	attr->background_pixmap = reply.pixmap;
    }
    if (mask & (1L << (ICLineSpace + offset))) {
	attr->line_space = reply.line_space;
    }
    if (mask & (1L << (ICCursor + offset))) {
	attr->cursor = reply.cursor;
    }
    return(0);
}

Status
_XipReceiveICValues(ic, mask)
    register XipIC ic;
    unsigned long mask;
{
    XipIM im = ipIMofIC(ic);
    register char *p = NULL, *p2 = NULL;
    ximGetICReq req;
    ximGetICReply reply;
    ximICValuesReq ic_req;

    req.reqType = XIM_GetIC;
    req.length = sz_ximGetICReq;
    req.xic = ic->icid;
    req.mask = mask;
    if ((_XipWriteToIM(im, (char *)&req, sz_ximGetICReq) < 0) ||
	(_XipFlushToIM(im) < 0)) {
	return(-1);
    }

    if (_XipReadFromIM(im, (char *)&reply, sz_ximGetICReply) < 0) {
	return(-1);
    }
    if (reply.state != 0) {
	return(-1);
    }
    if (_XipReadFromIM(im, (char *)&ic_req, sz_ximICValuesReq) < 0) {
	return(-1);
    }
    if (ic_req.nbytes > 0) {
	if ((p = Xmalloc((unsigned)(ic_req.nbytes + 1))) == NULL) return(-1);
	if (_XipReadFromIM(im, p, ic_req.nbytes) < 0) {
	    return(-1);
	}
	p[ic_req.nbytes] = '\0';
    }
    if (ic_req.nbytes2 > 0) {
	if ((p2 = Xmalloc((unsigned)(ic_req.nbytes2 + 1))) == NULL) return(-1);
	if (_XipReadFromIM(im, p2, ic_req.nbytes2) < 0) {
	    return(-1);
	}
	p2[ic_req.nbytes2] = '\0';
    }

    if (mask & (1L << ICInputStyle)) {
	ic->core.input_style = ic_req.input_style;
    }
    if (mask & (1L << ICClientWindow)) {
	ic->core.client_window = ic_req.c_window;
    }
    if (mask & (1L << ICFocusWindow)) {
	ic->core.focus_window = ic_req.focus_window;
    }
    if (mask & (1L << ICFilterEvents)) {
	ic->core.filter_events = ic_req.filter_events;
    }
#ifdef	XML
    if (mask & (1L << ICUsingLanguage)) {
	if (ic->values.using_language) Xfree(ic->values.using_language);
	if (p == NULL) {
	    ic->values.using_language = NULL;
	} else {
	    ic->values.using_language = p;
	}
    }
    if (mask & (1L << ICCurrentLanguage)) {
	if (ic->values.current_language) Xfree(ic->values.current_language);
	if (p2 == NULL) {
	    ic->values.current_language = NULL;
	} else {
	    ic->values.current_language = p2;
	}
    }
#else	/* XML */
    if (ic_req.nbytes > 0) Xfree(p);
    if (ic_req.nbytes2 > 0) Xfree(p2);
#endif	/* XML */
    if ((_ReceiveICAttrValues(im, &ic->core.preedit_attr, mask, 0) < 0) ||
	(_ReceiveICAttrValues(im, &ic->core.status_attr, mask, StatusOffset) < 0)) {
	return(-1);
    }
    return(0);
}

char*
_XipGetICValues(supic, args)
    XIC supic;
    XIMArg *args;
{
    XipIC		ic = (XipIC)supic;
    XipIM		im = ipIMofIC(ic);
    unsigned long	mask = 0L;
    char		*err;

    if (im->fd < 0) return(NULL);

    err = _XipICGetValues(ic, args, &mask, 1);
    if (err)
	return err;
    /*
     * Attempt to receive current IC data from the input manager.
     */
    if (!_XipReceiveICValues(ic, mask))
	(void)_XipICGetValues(ic, args, &mask, 0);
    return NULL;
}
