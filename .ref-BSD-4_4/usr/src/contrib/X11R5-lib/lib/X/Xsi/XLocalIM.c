/*
 * $XConsortium: XLocalIM.c,v 1.11 92/10/22 14:26:20 rws Exp $
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

#define NEED_EVENTS
#include "Xlibint.h"
#include "Xi18nint.h"
#include "XIMlibint.h"
#include <X11/Xos.h>
#include <X11/keysymdef.h>
#if NeedVarargsPrototypes
# include <stdarg.h>
# define Va_start(a,b) va_start(a,b)
#else
# include <varargs.h>
# define Va_start(a,b) va_start(a)
#endif
#ifndef X_NOT_POSIX
#ifdef _POSIX_SOURCE
#include <limits.h>
#else
#define _POSIX_SOURCE
#include <limits.h>
#undef _POSIX_SOURCE
#endif
#endif
#ifndef PATH_MAX
#include <sys/param.h>
#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 1024
#endif
#endif
#endif

#ifdef X_NOT_STDC_ENV
extern char *getenv();
#endif

#if __STDC__ && !defined(NORCONST)
#define RConst const
#else
#define RConst /**/
#endif

#define	offset(field)	XOffsetOf(XipLocalIMRec, field)

static int compiled_resources;

static XIMResource im_resources[] = {
    { XNQueryInputStyle, sizeof(XIMStyles), offset(values.input_styles),
      (unsigned short)IMResourceRead, (int)IMQueryInputStyle },
#ifdef	XML
    { XNQueryLanguage, sizeof(char *), offset(values.supported_language),
      (unsigned short)IMResourceRead, (int)IMQueryLanguage }
#endif	/* XML */
};

#undef offset

#define	offset(field)	XOffsetOf(XipLocalICRec, field)

static XIMResource ic_resources[] = {
    { XNInputStyle, sizeof(XIMStyle), offset(core.input_style),
      (unsigned short)IMResourceReadWrite, ICInputStyle },
    { XNClientWindow, sizeof(Window), offset(core.client_window),
      (unsigned short)IMResourceReadWrite, -1 },
    { XNFocusWindow, sizeof(Window), offset(core.focus_window),
      (unsigned short)IMResourceReadWrite, ICFocusWindow },
    { XNResourceName, sizeof(char *), offset(values.res_name),
      (unsigned short)IMResourceReadWrite, -1 },
    { XNResourceClass, sizeof(char *), offset(values.res_class),
      (unsigned short)IMResourceReadWrite, -1 },
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
    { XNSpotLocation, (sizeof(XPoint) + 4), attroffset(spot_location),
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


#define ENTRY_CNT 256

#define BITONP(h, i) (h[i / BITSIZ] &  (1 << (i % BITSIZ)))
#define BITOFP(h, i) (!BITONP(h, i))
#define BIT_UP(h, i) (h[i / BITSIZ] |= (1 << (i % BITSIZ)))
#define BITDWN(h, i) (h[i / BITSIZ] &= ~(1 << (i % BITSIZ)))

#define div_up(a, b) ((a + b - 1) / b)

#ifndef XNLSPATHDEFAULT
#define XNLSPATHDEFAULT "/usr/lib/X11/nls"
#endif

#ifndef LOCAL_CVT_TBL_DIR
#define LOCAL_CVT_TBL_DIR	"/local_im_tbl/"
#endif

typedef struct {
    char *name;
    unsigned int mask;
} StateTbl;

static StateTbl state_tbl[] = {
    { "Lock",	LockMask },
    { "Control",	ControlMask },
    { "Mod1",	Mod1Mask },
    { "Mod2",	Mod2Mask },
    { "Mod3",	Mod3Mask },
    { "Mod4",	Mod4Mask },
    { "Mod5",	Mod5Mask },
    { NULL,	0 }
};

typedef struct {
    char *name;
    Bool (*func)();
    int (*init_func)();
} FuncTbl;

static int convert_on();
static int convert_off();
static int convert_on_init();
static int no_filter();

static FuncTbl func_tbl[] = {
    { "ConvertOn",	convert_on,	convert_on_init },
    { "ConvertOff",	convert_off,	NULL },
    { "NoFilter",	NULL,	no_filter }
};

static unsigned int
is_state(name)
register char *name;
{
    register StateTbl *p;

    for (p = state_tbl; p->name; p++) {
	if (!strcmp(p->name, name)) return(p->mask);
    }
    return(0);
}

static int
parse_bslash(buf, work)
register char *buf;
register unsigned char *work;
{
    register int i;
    switch(*buf) {
	case 'x': /* 16 */
	    for (buf++, *work = '\0', i = 1; i >= 0; i--, buf++) {
		if (*buf >= '0' && *buf <= '9') {
		    *work |= ((1 << (i * 4)) * (*buf - '0'));
		} else if (*buf >= 'A' && *buf <= 'F') {
		    *work |= ((1 << (i * 4)) * (*buf - 'A' + 10));
		} else if (*buf >= 'a' && *buf <= 'f') {
		    *work |= ((1 << (i * 4)) * (*buf - 'a' + 10));
		} else {
		    return(0);
		}
	    }
	    return(3);
	case 'o': /* 8 */
	    for (buf++, *work = '\0', i = 2; i >= 0; i--, buf++) {
		if (*buf >= '0' && *buf <= '7') {
		    *work |= ((1 << (i * 3)) * (*buf - '0'));
		} else {
		    return(0);
		}
	    }
	    return(4);
	case 'n': /*  */
	    *work = '\n'; return(1);
	case 't': /* */
	    *work = '\t'; return(1);
	case 'b': /* */
	    *work = '\b'; return(1);
	case 'r': /* */
	    *work = '\r'; return(1);
	case 'f': /* */
	    *work = '\f'; return(1);
	case 'e': /* */
	case 'E': /* */
	    *work = '\033'; return(1);
	case '\\': /* */
	    *work = '\\'; return(1);
	default:
	    for (*work = '\0', i = 2; i >= 0; i--, buf++) {
		if (*buf >= '0' && *buf <= '7') {
		    *work |= ((1 << (i * 3)) * (*buf - '0'));
		} else {
		    return(*buf);
		}
	    }
	    return(3);
    }
}

static XipLocalKeySymTbl *
get_string(buf, tbl)
register char *buf;
XipLocalKeySymTbl *tbl;
{
    XipLocalKeySymTbl work_tbl[8], *p, *t;
    unsigned char work[32], *str;
    register int i = 0, total = 0, ret;

    for (p = work_tbl, total = 0; *buf; p++, total++, buf++) {
	if ((buf = index(buf, '{')) == NULL) {
	    break;
	}
	buf++;
	for ( i = 0; *buf != '}';) {
	    if (*buf == (char)0x5c) {
		buf++;
		if (!(ret = parse_bslash(buf, &work[i]))) {
		    return(NULL);
		}
		buf += ret;
		i++;
	    } else {
		work[i++] = *buf;
		buf++;
	    }
	    if (!*buf) return(NULL);
	}
	if (i == 0) return(NULL);
	work[i] = '\0';
	str = (unsigned char *) Xmalloc(i + 1);
	strcpy((char *)str, (char *)work);
	p->str = str;
	p->keysym = NoSymbol;
	p->state = 0;
	total++;
    }
    if (total < 1) return(NULL);
    p->str = NULL;
    p->keysym = XK_VoidSymbol;
    p->state = 0;
    if (tbl) {
	for (i = 0, t = tbl, p = work_tbl;
	     t->keysym != XK_VoidSymbol && i < total; i++, t++, p++) {
	    t->str = p->str;
	}
    } else {
	tbl = (XipLocalKeySymTbl *) Xmalloc((sizeof(XipLocalKeySymTbl)
					    * (total + 1)));
	if (!tbl) return (NULL);
	bcopy((char *)work_tbl, (char *)tbl,
	      sizeof(XipLocalKeySymTbl) * (total + 1));
    }
    return(tbl);
}

static XipLocalKeySymTbl *
get_keysym(buf, len)
char *buf;
int *len;
{
    int cnt, total = 0;
    int i;
    XipLocalKeySymTbl work_tbl[8], *keysym_tbl, *p;
    char *k[8];

    *len = 0;
    for (cnt = 0; ; cnt++) {
	if ((buf = index(buf, '<')) == NULL) {
	    break;
	}
	k[cnt] = ++buf;
	if (buf = index(buf, '>')) {
	    *buf = '\0';
	    buf++;
	}
    }
    if (cnt < 1)
	return (NULL);

    for (i = 0, p = work_tbl, total = 0; i < cnt; i++, p++) {
	if (p->state = is_state(k[i])) {
	    i++;
	}
	if ((p->keysym = XStringToKeysym(k[i])) == 0) {
	    fprintf(stderr,
		    "Can't convert to KeySym \"%s\".", k[i]);
	    return (NULL);
	}
	p->str = NULL;
	total++;
    }
    p->keysym = XK_VoidSymbol;
    p->str = NULL;
    total++;
    keysym_tbl = (XipLocalKeySymTbl *) Xmalloc((sizeof(XipLocalKeySymTbl)
						* total));
    if (!keysym_tbl)
	return (NULL);
    bcopy((char *)work_tbl, (char *)keysym_tbl,
	  sizeof(XipLocalKeySymTbl) * total);
    *len = (total - 1);
    return (keysym_tbl);
}


static int
is_comment(c)
char c;
{
    if (c == '#')
	return (1);
    return (0);
}

static int
is_command(c)
char c;
{
    if (c != '<' && c != '{')
	return (1);
    return (0);
}

static int
is_keysym(c)
char c;
{
    if (c == '<')
	return (1);
    return (0);
}

static int
is_state_command(xcvt, f, t)
XipLocalCvt *xcvt;
char *f, *t;
{
    if (!*f || !*t) return(0);
    if (!strcmp(f, "InitialState"))
	if (!strcmp(t, "OnState"))
	    xcvt->off = False;
	else if (!strcmp(t, "OffState"))
	    xcvt->off = True;
	else return(0);
    return(1);
}

static FuncTbl *
get_command(name)
char *name;
{
    register FuncTbl *p;

    for (p = func_tbl; p->name; p++) {
	if (!strcmp(p->name, name)) {
	    return(p);
	}
    }
    fprintf(stderr, "Sorry, \"%s\" is not a supported command.\n", name);
    fprintf(stderr, "XIM supported commands are \n");
    for (p = func_tbl; p->name; p++) {
	fprintf(stderr, " %s \n", p->name);
    }
    return(NULL);
}

static Bool
convert_on(xcvt)
XipLocalCvt *xcvt;
{
    xcvt->off = False;
    return(1);
}

static Bool
convert_off(xcvt)
XipLocalCvt *xcvt;
{
    xcvt->off = True;
    return(True);
}

static int
convert_on_init(xcvt, tbl, len)
XipLocalCvt *xcvt;
XipLocalCvtTbl *tbl;
int len;
{
    register XipLocalKeySymTbl *to, *from, *p;

    p = (XipLocalKeySymTbl *) Xmalloc(sizeof(XipLocalKeySymTbl) * (len + 1));
    if (!p) return(-1);
    for (to = p, from = tbl->fromkey; len > 0; to++, from++, len--) {
	to->keysym = from->keysym;
	to->state = from->state;
    }
    to->keysym = XK_VoidSymbol;
    to->state = 0;
    xcvt->off_tbl.to.func = tbl->to.func;
    xcvt->off_tbl.com = True;
    xcvt->off_tbl.fromkey = p;
    return(0);
}

static int
no_filter(xcvt, tbl, len)
XipLocalCvt *xcvt;
XipLocalCvtTbl *tbl;
int len;
{
    XipLocalNestedKeySym *nested_keysym;

    nested_keysym = (XipLocalNestedKeySym *)
      Xmalloc(sizeof(XipLocalNestedKeySym));
    if (!nested_keysym) return(-1);
    nested_keysym->keysym = tbl->fromkey->keysym;
    nested_keysym->next = xcvt->no_filter;
    xcvt->no_filter = nested_keysym;
    return(1);
}

static Bool
is_nofilter(xcvt, keysym)
XipLocalCvt *xcvt;
KeySym keysym;
{
    XipLocalNestedKeySym *p;

    for (p = xcvt->no_filter; p; p = p->next) {
	if (p->keysym == keysym) return(True);
    }
    return(False);
}

static FILE *
open_convert_file(lang, filename)
char *lang;
char *filename;
{
    FILE	*fp;
    char	nlspath[PATH_MAX];
    char	*path;
    char	*dir;
    char	*env;

    if ((env = getenv("XNLSPATH")) == NULL) {
	env = XNLSPATHDEFAULT;
    }
    path = nlspath;
    strcpy(path, env);
    while (1) {
	if (path == NULL) {
	    fprintf(stderr, "%s \"%s\".\n%s%s%s.\n",
		    "XIM: Couldn't find any convert table for lang", lang,
		    "Please, create xnls_dir", LOCAL_CVT_TBL_DIR, lang);
	    return(NULL);
	}
	dir = path;
	if ((dir = index(dir, ':')) != NULL) {
	    *dir++ = '\0';
	}
	strcpy(filename, path);
	strcat(filename, LOCAL_CVT_TBL_DIR);
	strcat(filename, lang);
	if (fp = fopen(filename, "r")) {
	    return(fp);
	}
	path = dir;
    }
}

static XipLocalCvt *
_XipLocalCvtSetUp(xlc)
XLocale	xlc;
{
    char	filename[PATH_MAX];
    char	tmp_buf[32], *p;
    FILE	*fp;
    char buf[256], tobuf[256], frombuf[256], tostr[256];
    int	 cnt = 0;
    int len;
    int k;
    XipLocalCvt *cvt;
    KeySym bs;
    FuncTbl *func_tbl;
    int line = 0;
    int ret;

    strcpy(tmp_buf, xlc->xlc_db->lc_name);
    for (p = tmp_buf; *p && *p != '@'; p++);
    if (*p)  *p = '\0';

    if(CHANGE_MAX < div_up(ENTRY_CNT, BITSIZ)){
	fprintf(stderr, "XIM: %s%s%d%s",
		"Sorry, please set CHANGE_MAX(in file ",
		"Xi18nint.h) larger than ",
		div_up(ENTRY_CNT, BITSIZ) - 1,
		",\r\nand recompile.\r\n");
	return(NULL);
    }

    
    if(!(fp = open_convert_file(tmp_buf, filename))) return(NULL);
    if (!(cvt = (XipLocalCvt *) Xmalloc(sizeof(XipLocalCvt)))) {
	fprintf(stderr, "XIM: Malloc failed\n");
	goto _err_ret2;
    }
    if (!(cvt->tbl = (XipLocalCvtTbl *) Xmalloc(sizeof(XipLocalCvtTbl) * ENTRY_CNT))) {
	fprintf(stderr, "XIM: Malloc failed\n");
	goto _err_ret1;
    }

    cnt = 0;
    cvt->nmax = 0;
    cvt->no_filter = NULL;
    cvt->off = False;
    while(fgets(buf, BUFSIZ, fp)) {
	line++;
	if(is_comment(*buf) || 
	   (k = sscanf(buf, "%s %s %s", frombuf, tobuf, tostr)) <= 0)
	    continue;
	if (k < 2) {
	    goto _err_ret;
	}
	if(!(cvt->tbl[cnt].fromkey = get_keysym(frombuf, &len))){
	    if (!is_state_command(cvt, frombuf, tobuf))
		goto _err_ret;
	    continue;
	}
	if (len > cvt->nmax) cvt->nmax = len;
	if (is_command(*tobuf)) {
	    if(!(func_tbl = get_command(tobuf))) {
		goto _err_ret;
	    }
	    cvt->tbl[cnt].to.func = func_tbl->func;
	    cvt->tbl[cnt].com = True;
	    if (func_tbl->init_func) {
		if ((ret = (*func_tbl->init_func)(cvt, &cvt->tbl[cnt], len))
		     == -1)
		    goto _err_ret;
		else if (ret == 0)
		    continue;
	    }
	} else {
	    if (is_keysym(*tobuf)) {
		if(!(cvt->tbl[cnt].to.tokey = get_keysym(tobuf, &len))){
		    goto _err_ret;
		}
		if (k > 2) {
		    if(!(get_string(tostr, cvt->tbl[cnt].to.tokey))){
			goto _err_ret;
		    }
		}
	    } else {
		if(!(cvt->tbl[cnt].to.tokey = get_string(tobuf, NULL))){
		    goto _err_ret;
		}
	    }
	    cvt->tbl[cnt].com = False;
	}
	cnt++;

    }
    cvt->cnt = cnt;
    cvt->buf = (XipLocalKeySymTbl *) Xmalloc(sizeof(XipLocalKeySymTbl)
					     * (cvt->nmax + 1));
    cvt->buf_cnt = 0;
    cvt->bs = ((bs = XStringToKeysym("BackSpace"))? bs: 0x8);
    fclose(fp);
    return(cvt);
_err_ret:
    fprintf(stderr, "XIM: Error occurred at line %d in file \"%s\".\n",
	    line, filename);
    Xfree(cvt->tbl);
_err_ret1:
    Xfree(cvt);
_err_ret2:
    fclose(fp);
    return(NULL);
}

static XipLocalCvt *
_XipLocalDupCvt(cvt)
XipLocalCvt *cvt;
{
    XipLocalCvt *new;

    if (!(new = (XipLocalCvt *) Xmalloc(sizeof(XipLocalCvt)))) {
	return(NULL);
    }
    new->tbl = cvt->tbl;
    new->off_tbl = cvt->off_tbl;
    new->no_filter = cvt->no_filter;
    new->bs = cvt->bs;
    new->cnt = cvt->cnt;
    new->nmax = cvt->nmax;
    new->buf = (XipLocalKeySymTbl *) Xmalloc(sizeof(XipLocalKeySymTbl)
					     * (new->nmax + 1));
    new->buf_cnt = 0;
    new->off = cvt->off;
    return(new);
}

static void
_XipLocalFreeCvt(cvt)
XipLocalCvt *cvt;
{
    register int i, j;
    XipLocalCvtTbl *tbl;

    tbl = cvt->tbl;
    for (i = 0; i < cvt->cnt; i++, tbl++) {
	Xfree((*tbl).fromkey);
	if ((*tbl).com != True) {
	    for (j = 0; (*tbl).to.tokey[j].str; j++)
		Xfree((*tbl).to.tokey[j].str);
	    Xfree((*tbl).to.tokey);
	}
    }
    Xfree(cvt->tbl);
    Xfree(cvt);
}

/*
 * Close the connection to the input manager, and free the private data
 */
static Status
_XipLocalCloseIM(supim)
    XIM supim;
{
    XipLocalIM im = (XipLocalIM)supim;
    _XlcFreeLocale(im->xlc);
    if (im->xcvt) _XipLocalFreeCvt(im->xcvt);
    return(Success);
}

static RConst XIMMethodsRec im_methods = {
    _XipLocalCloseIM,
    _XipLocalGetIMValues,
    _XipLocalCreateIC
};

XIM 
_XipLocalOpenIM(lcd, display, rdb, res_name, res_class)
    XLCd lcd;
    Display *display;
    XrmDatabase rdb;
    char *res_name;
    char *res_class;
{
    XipLocalIM		xim;
    XLocale		xlc = ((XsiLCd)lcd)->xlc;
    XipLocalCvt		*xcvt;
    XIMStyle		*supported_styles;

    if ((xcvt = _XipLocalCvtSetUp(xlc)) == NULL) return(NULL);
    /*
     * Attempt to allocate a XIM structure. Return NULL if allocation
     * failed.
     */
    if ((xim = (XipLocalIM)Xmalloc(sizeof(XipLocalIMRec))) == NULL) {
	return(NULL);
    }
    xim->methods = (XIMMethods)&im_methods;
    xim->core.lcd = lcd;
    xim->core.ic_chain = NULL;
    xim->core.display = display;
    xim->core.rdb = rdb;
    xim->core.res_name = res_name;
    xim->core.res_class = res_class;

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

    if ((supported_styles = (XIMStyle *)Xmalloc((unsigned)(sizeof(XIMStyle))))
	== NULL) {
	return(NULL);
    }
    supported_styles[0] = (XIMPreeditNothing | XIMStatusNothing);
    xim->values.input_styles.supported_styles = supported_styles;
    xim->values.input_styles.count_styles = 1;
    xim->xlc = _XlcDupLocale(xlc);
    xim->xcvt = xcvt;

    return((XIM)xim);
}

int
_XipLocalCallCallbacks(ic)
    XipIC ic;
{
    return(0);
}

static int
key_check(cvt)
XipLocalCvt *cvt;
{
    int		dist, base;
    XipLocalKeySymTbl	*code_p;
    int		i;

    for(base = 0; cvt->buf[base].keysym != XK_VoidSymbol; base++) {
	for(dist = 0; dist < cvt->cnt; dist++) {
	    if(BITONP(cvt->check_flg, dist) &&
	       cvt->tbl[dist].fromkey->keysym != XK_VoidSymbol){
		code_p = cvt->tbl[dist].fromkey + base;
		if((code_p->keysym == cvt->buf[base].keysym) &&
		   ((!(code_p->state | (cvt->buf[base].state & ~ShiftMask))) ||
		    (code_p->state == (cvt->buf[base].state & ~ShiftMask)))) {
		    if((code_p + 1)->keysym == (KeySym)XK_VoidSymbol){
			/* matched */
			for(i = 0, base++;
			    (cvt->buf[i].keysym = cvt->buf[base].keysym) != XK_VoidSymbol;
			    i++, base++);
			return(dist);
		    }
			/* Not yet matched */
		} else {
		    BITDWN(cvt->check_flg, dist); /* off bit vecter */
		}
	    }
	}
    }

    /* Check bit vercters */
    for(i = 0; i < cvt->cnt / BITSIZ; i++){
	if(cvt->check_flg[i]) return(-1);
    }
    if((cvt->cnt % BITSIZ) &&
       (cvt->check_flg[i]& ~(~0 << (cvt->cnt % BITSIZ))))
	return(-1); /* posiblity */
    return(-2);	/* Not matched */
}

static int
off_key_check(cvt)
XipLocalCvt *cvt;
{
    int		base;
    XipLocalKeySymTbl	*code_p = cvt->off_tbl.fromkey;
    int		i;

    for(base = 0; cvt->buf[base].keysym != XK_VoidSymbol; base++) {
	if((code_p->keysym == cvt->buf[base].keysym) &&
	   ((!(code_p->state | (cvt->buf[base].state & ~ShiftMask))) ||
	   (code_p->state == (cvt->buf[base].state & ~ShiftMask)))) {
	    if((code_p + 1)->keysym == (KeySym)XK_VoidSymbol){
		/* matched */
		for(i = 0, base++;
		    (cvt->buf[i].keysym = cvt->buf[base].keysym) != XK_VoidSymbol;
		    i++, base++);
		return(0);
	    }
	} else {
	    return(-2);
	}
    }
    return(-1); /* posiblity */
}

static void
_XipLocalClearCvt(cvt)
XipLocalCvt *cvt;
{
    register int i;

    for(i = 0; i < div_up(cvt->cnt, BITSIZ); cvt->check_flg[i++] = ~0);
}

Bool
_XipLocalBackEndFilter(display, window, ev, client_data)
    Display *display;
    Window window;
    XEvent *ev;
    XPointer client_data;
{
    register XipLocalIC	ic = (XipLocalIC)client_data;
    XipLocalCvt		*xcvt = ic->xcvt;
    KeySym keysym;
    int keycode;
    int	i, c;		/* work */
    char str[256];
    XEvent dummy_ev;

    if (xcvt->cnt == 0)
	return(False);

    if (ev->xkey.keycode == 0) {
	if (_XipTypeOfNextICQueue((XipIC)ic) == XIM_KEYSYM){
	    if ((keycode = XKeysymToKeycode(display,
			    _XipKeySymOfNextICQueue((XipIC)ic))) != 0
		&& _XipStringOfNextICQueue((XipIC)ic) == NULL){
		ev->xkey.state = _XipStateOfNextICQueue((XipIC)ic);
		ev->xkey.keycode = keycode;
		_XipFreeNextICQueue((XipIC)ic);
	    }
	}
	return(False);
    }

    if (ic->out) ic->out = NULL;
    
    (void)XLookupString(ev, str, 256, &keysym, NULL);

    if (keysym == NoSymbol/* || nbytes < 1 */) {
	return(True);
    }
    if (is_nofilter(xcvt, keysym)) return(False);
    if (keysym == xcvt->bs) {
	if (xcvt->buf_cnt == 0) {
	    return(False);
	}
	xcvt->buf[xcvt->buf_cnt - 1].keysym = xcvt->buf[xcvt->buf_cnt].keysym;
	xcvt->buf[xcvt->buf_cnt - 1].state = xcvt->buf[xcvt->buf_cnt].state;
	_XipLocalClearCvt(xcvt);
	if (--xcvt->buf_cnt > 0) {
	    key_check(xcvt);
	}
	return(True);
    }
    if (xcvt->buf_cnt == 0) {
	_XipLocalClearCvt(xcvt);
    }
    xcvt->buf[xcvt->buf_cnt].keysym = keysym;
    xcvt->buf[xcvt->buf_cnt].state = ev->xkey.state;
    xcvt->buf[++xcvt->buf_cnt].keysym = XK_VoidSymbol;
    bcopy((char *)ev, (char *)&dummy_ev, sizeof(XEvent));
    dummy_ev.type = KeyPress;
    dummy_ev.xkey.state = 0;
    dummy_ev.xkey.keycode = 0;

    if (xcvt->off == True) {
	if ((c = off_key_check(xcvt)) == 0) {
	    xcvt->buf_cnt = 0;
	    (*xcvt->off_tbl.to.func)(xcvt);
	    _XipLocalClearCvt(xcvt);
	    return(True);
	}
    } else {
	c = key_check(xcvt);
    }
    switch (c) {
    case -1: /* Event was filtered */
	return (True);
    case -2: /* No match */
	for (i = 0; i < xcvt->buf_cnt; i++) {
	     if (_XipPutICQueue(ic, XIM_KEYSYM, 0, xcvt->buf[i].keysym,
				xcvt->buf[i].state, 0, NULL) < 0)
	       return(False);
	}
	ev->xkey.state = 0;
	ev->xkey.keycode = 0;
	if ((keycode = XKeysymToKeycode(display,
				_XipKeySymOfNextICQueue((XipIC)ic))) != 0
	    && _XipStringOfNextICQueue((XipIC)ic) == NULL) {
	    ev->xkey.state = _XipStateOfNextICQueue((XipIC)ic);
	    ev->xkey.keycode = keycode;
	    _XipFreeNextICQueue((XipIC)ic);
	}
	for (i = 1; i < xcvt->buf_cnt; i++) {
	    XPutBackEvent(display, &dummy_ev);
	}
	xcvt->buf_cnt = 0;
	_XipLocalClearCvt(xcvt);
	return (False);
    default: /* matched */
        xcvt->buf_cnt = 0;
	if (xcvt->tbl[c].com == True) {
	    if ((*xcvt->tbl[c].to.func)(xcvt) == True) {
		_XipLocalClearCvt(xcvt);
		return(True);
	    }
	}
	for (i = 0; xcvt->tbl[c].to.tokey[i].keysym != XK_VoidSymbol; i++) {
	     if (_XipPutICQueue(ic, XIM_KEYSYM, 0,
				xcvt->tbl[c].to.tokey[i].keysym,
				xcvt->tbl[c].to.tokey[i].state, 1,
				xcvt->tbl[c].to.tokey[i].str) < 0)
	       return(False);
	}
	ev->xkey.state = 0;
	ev->xkey.keycode = 0;
	if ((keycode = XKeysymToKeycode(display,
				_XipKeySymOfNextICQueue((XipIC)ic))) != 0
	    && _XipStringOfNextICQueue((XipIC)ic) == NULL) {
	    ev->xkey.state = _XipStateOfNextICQueue((XipIC)ic);
	    ev->xkey.keycode = keycode;
	    _XipFreeNextICQueue((XipIC)ic);
	}
	for (i = 1; xcvt->tbl[c].to.tokey[i].keysym != XK_VoidSymbol; i++) {
	    XPutBackEvent(display, &dummy_ev);
	}
	xcvt->buf_cnt = 0;
	_XipLocalClearCvt(xcvt);
	return(False);
    }
}

/*
 * Free the input context.
 */
void
_XipLocalDestroyIC(supic)
    XIC supic;
{
    XipLocalIC ic = (XipLocalIC)supic;
    XipLocalIM im = (XipLocalIM)ic->core.im;

    _XUnregisterFilter(im->core.display, ic->core.focus_window,
		       ic->prototype_filter, (XPointer)ic);

#ifdef	XML
    if (ic->xlc_num > 0) {
	int i;

	for (i = 0; i < ic->xlc_num; i++) {
	    _XlcFreeLocale(ic->mb_temp[i]);
	    _XlcFreeLocale(ic->wc_temp[i]);
	    _XipLocalFreeCvt(ic->xcvt);
	}
    }
    if (ic->values.using_language) Xfree(ic->values.using_language);
#endif	/* XML */
}

static RConst XICMethodsRec ic_methods = {
    _XipLocalDestroyIC,
    _XipLocalSetICFocus,
    _XipLocalUnsetICFocus,
    _XipLocalSetICValues,
    _XipLocalGetICValues,
    _XipLocalmbResetIC,
    _XipLocalwcResetIC,
    _XipmbLookupString,
    _XipwcLookupString
};

/*
 * Create an input context within the input method, 
 * and return a pointer the input context ti the caller.
 */
XIC
_XipLocalCreateIC(supim, args)
    XIM supim;
    XIMArg *args;
{
    XipLocalIM		im = (XipLocalIM)supim;
    XipLocalIC		ic;
    unsigned long	mask = 0L;
#ifdef	XML
    char		**nls_list, **l;
    XLocale		xlc;
#endif	/* XML */

    if ((ic = (XipLocalIC)Xcalloc(1, sizeof(XipLocalICRec))) == NULL) {
	return(NULL);
    }

    ic->methods = (XICMethods) &ic_methods;
    ic->prototype_filter = _XipLocalBackEndFilter;
    ic->core.im = supim;
    (void)_XipICSetValues(ic, args, &mask);

#ifdef	XML
    if (im->xlc != NULL) {
#endif	/* XML */
	ic->mb = _XlcDupLocale(im->xlc);
	ic->wc = _XlcDupLocale(im->xlc);
	ic->xcvt = _XipLocalDupCvt(im->xcvt);
#ifdef	XML
	ic->xlc_num = 0;
	ic->mb_temp = NULL;
	ic->wc_temp = NULL;
	ic->xcvt_temp = NULL;
    } else {
	ic->mb = NULL;
	ic->wc = NULL;
	ic->xcvt_temp = NULL;
	ic->xlc_num = 0;
	if ((ic->mb_temp = (XLocale*)Xmalloc(sizeof(XLocale) * 32)) == NULL) {
	    return(NULL);
	}
	if ((ic->wc_temp = (XLocale*)Xmalloc(sizeof(XLocale) * 32)) == NULL) {
	    return(NULL);
	}
	if ((ic->xcvt_temp = (XipLocalCvt**)
	     Xmalloc(sizeof(XipLocalCvt*) * 32)) == NULL) {
	    return(NULL);
	}
	_XlcListLocale(&nls_list);
	for (l = nls_list; *l; l++) {
	    xlc = _XlcMakeLocale(*l);
	    if (!xlc)
		continue;
	    if ((ic->xcvt_temp[ic->xlc_num] = _XipLocalCvtSetUp(xlc)) == NULL){
		_XlcFreeLocale(xlc);
	        continue;
	    }
	    ic->mb_temp[ic->xlc_num] = xlc;
	    ic->wc_temp[ic->xlc_num] = _XlcDupLocale(xlc);
	    ic->xlc_num++;
	}
	Xfree((char *)nls_list);
	ic->mb = ic->mb_temp[0];
	ic->wc = ic->wc_temp[0];
	ic->xcvt = ic->xcvt_temp[0];
    }
#endif /* XML */
    return((XIC)ic);
}

/*
 * Reset the input context. 
 */
wchar_t *
_XipLocalwcResetIC(supic)
    XIC supic;		/* specified the input context to reset*/
{
    register XipLocalIC	ic = (XipLocalIC)supic;

    ic->xcvt->buf_cnt = 0;
    _XipLocalClearCvt(ic->xcvt);
    return((wchar_t *)NULL);
}

char *
_XipLocalmbResetIC(supic)
    XIC supic;		/* specified the input context to reset*/
{
    register XipLocalIC	ic = (XipLocalIC)supic;

    ic->xcvt->buf_cnt = 0;
    _XipLocalClearCvt(ic->xcvt);
    return(NULL);
}

/*
 * Query Input Method.
 */
static char *
_IMGetValues(im, args)
    register XipLocalIM im;
    register XIMArg *args;
{
    register XIMArg		*arg;
    register int		i;
    register XrmName		Name;
    register XIMrmResourceList	xrmres;
    unsigned int		num_resources = im->num_resources;
    XrmQuark			query_input =
				     XrmPermStringToQuark(XNQueryInputStyle);

    for (arg = args; arg->name && *(arg->name); arg++) {
	Name = XrmStringToName(arg->name);
	for (xrmres = im->resources, i = 0;
	     i < num_resources; i++, xrmres++) {
	    if (Name == xrmres->xrm_name) {
		if (Name == query_input) {
		    char *p;
		    XIMStyles *styles;
		    int size = sizeof(XIMStyle)
			       * im->values.input_styles.count_styles;
		    int all_size = sizeof(XIMStyles) + size;
		    if ((p = Xmalloc((unsigned)all_size)) == NULL) {
			continue;
		    }
		    styles = (XIMStyles *)p;
		    styles->count_styles = im->values.input_styles.count_styles;
		    styles->supported_styles =
					(XIMStyle *)(p + sizeof(XIMStyles));
		    bcopy((char *)im->values.input_styles.supported_styles,
			  (char *)styles->supported_styles, size);
		    bcopy((char *)&styles, (char *)arg->value,
			  sizeof(XIMStyles *));
		} else {
		    (void) _XCopyToArg((char *)im - xrmres->xrm_offset - 1,
				       &arg->value,
				       (unsigned int)xrmres->xrm_size);
		}
		break;
	    }
	}
	if (i >= num_resources) return(arg->name);
    }
    return NULL;
}

char *
_XipLocalGetIMValues(supim, args)
    XIM supim;
    XIMArg *args;
{
    XipLocalIM im = (XipLocalIM)supim;

    return(_IMGetValues(im, args));
}

/*
 * Require the input manager to focus the focus window attached to the ic
 * argument.
 */
void
_XipLocalSetICFocus(supic)
    XIC supic;
{
    XipLocalIC		ic = (XipLocalIC)supic;
    XipLocalIM		im = ipLocalIMofIC(ic);

    _XRegisterFilterByMask(im->core.display, ic->core.focus_window,
			   KeyPressMask, ic->prototype_filter, (XPointer)ic);
}

/*
 * Require the input manager to unfocus the focus window attached to the ic
 * argument.
 */
void
_XipLocalUnsetICFocus(supic)
    XIC supic;
{
    XipLocalIC		ic = (XipLocalIC)supic;
    XipLocalIM		im = ipLocalIMofIC(ic);
    _XUnregisterFilter(im->core.display, ic->core.focus_window,
		       ic->prototype_filter, (XPointer)ic);
}

char*
_XipLocalGetICValues(supic, args)
    XIC supic;
    XIMArg *args;
{
    XipLocalIC		ic = (XipLocalIC)supic;
    unsigned long	mask = 0L;

    return(_XipICGetValues((XipIC)ic, args, &mask, 0));
}

#ifdef	XML
void
_XipLocalChangeLocale(ic, lc_name)
    XipLocalIC ic;
    char *lc_name;
{
    XLocale xlc;
    int i;

    for (i = 0; i < ic->xlc_num; i++) {
	if ((!strcmp(lc_name, ic->mb_temp[i]->lc_lang)) ||
	    (!strcmp(lc_name, ic->mb_temp[i]->xlc_db->lc_name))) {
	    ic->mb = ic->mb_temp[i];
	    ic->wc = ic->wc_temp[i];
	    ic->xcvt = ic->xcvt_temp[i];
	    return;
	}
    }
    xlc = _XlcMakeLocale(lc_name);
    if (xlc) {
	if ((ic->xcvt = ic->xcvt_temp[ic->xlc_num] = _XipLocalCvtSetUp(xlc))
	    == NULL) {
	    _XlcFreeLocale(xlc);
	    return;
	}
	ic->mb = ic->mb_temp[ic->xlc_num] = xlc;
	ic->wc = ic->wc_temp[ic->xlc_num] = _XlcDupLocale(xlc);
	ic->xlc_num++;
    }
}
#endif	/* XML */

char *
_XipLocalSetICValues(supic, args)
    XIC supic;
    XIMArg *args;
{
    XipLocalIC		ic = (XipLocalIC)supic;
    XipLocalIM		im = ipLocalIMofIC(ic);
    unsigned long	mask = 0L;
    Window		old_focus_window;
    char		*err = NULL;

    old_focus_window = ic->core.focus_window;

    err = _XipICSetValues((XipIC)ic, args, &mask);
    if (err)
	return(err);
    
    if (mask & (1L << ICFocusWindow)) {
	_XUnregisterFilter(im->core.display, old_focus_window,
			   ic->prototype_filter, (XPointer)ic);
	_XRegisterFilterByMask(im->core.display, ic->core.focus_window,
			       KeyPressMask,
			       ic->prototype_filter, (XPointer)ic);
    }
#ifdef	XML
    if (mask & (1L << ICCurrentLanguage)) {
	_XipLocalChangeLocale(ic, ic->values.current_language);
    }
#endif	/* XML */

    return(NULL);
}
