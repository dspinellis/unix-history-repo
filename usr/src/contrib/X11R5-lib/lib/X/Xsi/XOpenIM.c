/*
 * $XConsortium: XOpenIM.c,v 1.19 92/07/28 17:52:13 rws Exp $
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
#include <pwd.h>
#ifdef X_NOT_STDC_ENV
extern char *getenv();
#endif
#ifdef X_NOT_POSIX
extern int getuid();
#endif

#if __STDC__ && !defined(NORCONST)
#define RConst const
#else
#define RConst /**/
#endif

#define	offset(field)	XOffsetOf(XipIMRec, field)

static int compiled_resources;

static XIMResource im_resources[] = {
    { XNQueryInputStyle, sizeof(XIMStyles *), offset(values.input_styles),
      (unsigned short)IMResourceRead, (int)IMQueryInputStyle },
#ifdef	XML
    { XNQueryLanguage, sizeof(char *), offset(values.supported_language),
      (unsigned short)IMResourceRead, (int)IMQueryLanguage }
#endif	/* XML */
};

#undef offset

#define	offset(field)	XOffsetOf(XipICRec, field)

static XIMResource ic_resources[] = {
    { XNInputStyle, sizeof(XIMStyle), offset(core.input_style),
      (unsigned short)IMResourceReadWrite, ICInputStyle },
    { XNClientWindow, sizeof(Window), offset(core.client_window),
      (unsigned short)IMResourceReadWrite, ICClientWindow },
    { XNFocusWindow, sizeof(Window), offset(core.focus_window),
      (unsigned short)IMResourceReadWrite, ICFocusWindow },
    { XNResourceName, sizeof(char *), offset(values.res_name),
      (unsigned short)IMResourceReadWrite, ICResourceName },
    { XNResourceClass, sizeof(char *), offset(values.res_class),
      (unsigned short)IMResourceReadWrite, ICResourceClass },
    { XNFilterEvents, sizeof(long), offset(core.filter_events),
      (unsigned short)IMResourceRead, ICFilterEvents },
    { XNPreeditAttributes, sizeof(ICAttributes *), offset(core.preedit_attr),
      (unsigned short)IMResourceReadWrite, -1 },
    { XNStatusAttributes, sizeof(ICAttributes *), offset(core.status_attr),
      (unsigned short)IMResourceReadWrite, -1 }
#ifdef	XML
    ,
    { XNUsingLanguage, sizeof(char *), offset(values.using_language),
      (unsigned short)IMResourceReadWrite, ICUsingLanguage },
    { XNCurrentLanguage, sizeof(char *), offset(values.current_language),
      (unsigned short)IMResourceReadWrite, ICCurrentLanguage },
    { XNChangeLocaleCB, sizeof(char **), offset(values.ch_locale_cb),
      (unsigned short)IMResourceReadWrite, ICChangeLocaleCB },
#endif	/* XML */
};

#undef	offset

#define	attroffset(field)	XOffsetOf(ICAttributes, field)

static XIMResource attr_resources[] = {
    { XNArea, sizeof(XRectangle), attroffset(area),
      (unsigned short)IMResourceReadWrite, ICArea },
    { XNAreaNeeded, sizeof(XRectangle), attroffset(area_needed),
      (unsigned short)IMResourceReadWrite, ICAreaNeeded },
    { XNSpotLocation, sizeof(XPoint), attroffset(spot_location),
      (unsigned short)IMResourceReadWrite, ICSpotLocation },
    { XNColormap, sizeof(Colormap), attroffset(colormap),
      (unsigned short)IMResourceReadWrite, ICColormap },
    { XNStdColormap, sizeof(Atom), attroffset(std_colormap),
      (unsigned short)IMResourceReadWrite, ICStdColormap },
    { XNForeground, sizeof(long), attroffset(foreground),
      (unsigned short)IMResourceReadWrite, ICForeground },
    { XNBackground, sizeof(long), attroffset(background),
      (unsigned short)IMResourceReadWrite, ICBackground },
    { XNBackgroundPixmap, sizeof(Pixmap), attroffset(background_pixmap),
      (unsigned short)IMResourceReadWrite, ICBackgroundPixmap },
    { XNFontSet, sizeof(XFontSet *), attroffset(fontset),
      (unsigned short)IMResourceReadWrite, ICFontSet },
    { XNLineSpace, sizeof(int), attroffset(line_space),
      (unsigned short)IMResourceReadWrite, ICLineSpace },
    { XNCursor, sizeof(Cursor), attroffset(cursor),
      (unsigned short)IMResourceReadWrite, ICCursor },
    { XNPreeditStartCallback, sizeof(XIMCallback), attroffset(callbacks.start),
      (unsigned short)IMResourceReadWrite, -1 },
    { XNPreeditDoneCallback, sizeof(XIMCallback), attroffset(callbacks.done),
      (unsigned short)IMResourceReadWrite, -1 },
    { XNPreeditDrawCallback, sizeof(XIMCallback), attroffset(callbacks.draw),
      (unsigned short)IMResourceReadWrite, -1 },
    { XNPreeditCaretCallback, sizeof(XIMCallback), attroffset(callbacks.caret),
      (unsigned short)IMResourceReadWrite, -1 },
    { XNStatusStartCallback, sizeof(XIMCallback), attroffset(callbacks.start),
      (unsigned short)IMResourceReadWrite, -1 },
    { XNStatusDoneCallback, sizeof(XIMCallback), attroffset(callbacks.done),
      (unsigned short)IMResourceReadWrite, -1 },
    { XNStatusDrawCallback, sizeof(XIMCallback), attroffset(callbacks.draw),
      (unsigned short)IMResourceReadWrite, -1 }
};

#undef	attroffset


/*
 * Close the connection to the input manager, and free the private data
 */
static Status
_XipCloseIM(supim)
    XIM supim;
{
    XipIM im = (XipIM)supim;
    if (im->fd >= 0)
	_XipDisconnectIM(im->fd);
    _XlcFreeLocale(im->xlc);
    Xfree(im->client_data);
    if (im->default_ic) Xfree((char *)im->default_ic);
    return(Success);
}

static RConst XIMMethodsRec im_methods = {
    _XipCloseIM,
    _XipGetIMValues,
    _XipCreateIC
};

/*
 * Connects to an input method matching current locale specification, creates
 * a XIM object and return a pointer the newly created XIM back to the caller.
 */
XIM 
_XipOpenIM(lcd, display, rdb, res_name, res_class)
    XLCd lcd;
    Display *display;
    XrmDatabase rdb;
    char *res_name;
    char *res_class;
{
    Atom		atom_im;
    XipIM		xim;
    char		strbuf[128];
    char		*str, *prop_im, *p;
    XLocale		xlc = ((XsiLCd)lcd)->xlc;
#ifdef uniosu
    extern struct passwd *getpwuid();
#endif

    str = lcd->core.modifiers;
    while (str && strncmp(str+1, "im=", 3))
	str = index(str+1, '@');
    if (!str)
	strbuf[0] = '\0';
    else {
	str += 4;
	p = index(str, '@');
	if (p) {
	    strncpy(strbuf, str, p - str);
	    strbuf[p - str] = '\0';
	} else {
	    strcpy(strbuf, str);
	}
    }
    if (strbuf[0] == '\0')
	prop_im = XIM_INPUTMETHOD;
    else if (!strcmp(strbuf, "None") || !strcmp(strbuf, "NONE"))
	return(NULL);
#ifndef	NO_LOCAL_IM
    else if (!strcmp(strbuf, "Local"))
	return(_XipLocalOpenIM(lcd, display, rdb, res_name, res_class));
#endif
    else
	prop_im = strbuf;

    /*
     * Check whether an input manager is running or not. If the input manager
     * isn't running, return NULL.
     */
    if ((atom_im = XInternAtom(display, prop_im, True)) == None) {
	return(NULL);
    }
    if (XGetSelectionOwner (display, atom_im) == None) {
	return(NULL);
    }

    /*
     * Gets display name from the display structure. If the display name is
     * NULL, gets from the DISPLAY variable.
     */
    if (DisplayString(display)) {
      strcpy(strbuf, DisplayString(display));
    } else if (str = getenv("DISPLAY")) {
	/* If the display string is NULL, read the DISPLAY variable. */
	strcpy(strbuf, str);
    } else {
	/* No display name - error. */
	return(NULL);
    }

    /*
     * Attempt to allocate a XIM structure. Return NULL if allocation
     * failed.
     */
    if ((xim = (XipIM)Xmalloc(sizeof(XipIMRec))) == NULL) {
	return(NULL);
    }
    xim->methods = (XIMMethods)&im_methods;
    xim->sp = 0;
    xim->rp = 0;
    xim->rc = 0;
    xim->core.lcd = lcd;
    xim->core.ic_chain = NULL;
    xim->core.display = display;
    xim->core.rdb = rdb;
    xim->core.res_name = res_name;
    xim->core.res_class = res_class;
    xim->values.input_styles.supported_styles = NULL;
    xim->values.input_styles.count_styles = 0;
#ifdef	XML
    xim->values.supported_language = NULL;
#endif	/* XML */

    /*
     * Get user name.
     */
    if ((str = getenv("USER")) == NULL)
	str = getpwuid(getuid())->pw_name;
    xim->client_data = Xmalloc(strlen(str) + 1);
    if (!xim->client_data) {
	Xfree((char *)xim);
	return NULL;
    }
    (void)strcpy(xim->client_data, str);

    if (!compiled_resources) {
	_XIMCompileResourceList(im_resources, XIMNumber(im_resources));
	_XIMCompileResourceList(ic_resources, XIMNumber(ic_resources));
	_XIMCompileResourceList(attr_resources, XIMNumber(attr_resources));
	compiled_resources = 1;
    }
    xim->resources = (XIMrmResourceList)im_resources;
    xim->num_resources = XIMNumber(im_resources);
    xim->core.ic_resources = (XIMrmResourceList)ic_resources;
    xim->core.ic_num_resources = XIMNumber(ic_resources);
    xim->core.ic_attr_resources = (XIMrmResourceList)attr_resources;
    xim->core.ic_num_attr_resources = XIMNumber(attr_resources);

    /*
     * Call the Connect routine to get the network socket. If a value less
     * than 0 is returned, the connection failed. 
     */
    if ((_XipConnectIM(xim, atom_im, strbuf)) != True) {
	Xfree(xim->client_data);
	Xfree((char *)xim);
	return(NULL);
    }

    xim->xlc = _XlcDupLocale(xlc);

    /*
     * Create a default IC.
     */
    if (!_XipCreateDefIC(xim)) {
	XCloseIM((XIM)xim);
	return(NULL);
    }

    return((XIM)xim);
}

#ifdef	XML
XIM 
XmlOpenIM(display, rdb, res_name, res_class)
Display *display;
XrmDatabase rdb;
char *res_name;
char *res_class;
{
    XIM		supim;
    XipIM	im;

    if ((supim = XOpenIM(display, rdb, res_name, res_class)) == NULL) {
	return(NULL);
    }
    im = (XipIM)supim;
    if (im->xlc) {
	_XlcFreeLocale(im->xlc);
	im->xlc = NULL;
    }
    return(supim);
}
#endif	/* XML */
