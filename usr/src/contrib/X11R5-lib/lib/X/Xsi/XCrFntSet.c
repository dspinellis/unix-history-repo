/*
 * $XConsortium: XCrFntSet.c,v 1.48 92/09/10 17:06:53 rws Exp $
 */

/*
 * Copyright 1990, 1991 by OMRON Corporation, NTT Software Corporation,
 *                      and Nippon Telegraph and Telephone Corporation
 * Copyright 1991 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of OMRON, NTT Software, NTT, and M.I.T.
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission. OMRON, NTT Software,
 * NTT, and M.I.T. make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *
 * OMRON, NTT SOFTWARE, NTT, AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD
 * TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL OMRON, NTT SOFTWARE, NTT, OR M.I.T. BE
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
 */

/*
 * _XsiCreateFontSet()
 * XmlCreateFontSet()
 * _XsiQueryFontSetFromId()
 * _XsiQueryFontSetWithName()
 *
 */
#include <X11/Xatom.h>
#include "Xlibint.h"
#include "Xi18nint.h"
#include <X11/Xos.h>

#ifdef DEBUG
#include <stdio.h>
#endif
#include <ctype.h>

#define PRELOAD                 1      /* preload fonts if locale exists */ 

#ifdef DEBUG
#define dbg_printf(f, p1, p2)   fprintf(stderr, f, p1, p2)
#else
#define dbg_printf(f, p1, p2)
#endif

#define XFNEXT_PREFIX	'+'     /* XLFD External prefix */
#define XFNDELIM	'-'     /* XLFD Delimiter */
#define DELIM_COUNT	13      /* Number of delimiters */
#define CSETPOS		13      /* CHARSET-REGISTRY position */

#if __STDC__ && !defined(NORCONST)
#define RConst const
#else
#define RConst /**/
#endif

/*ARGSUSED*/
static void
_XsiFreeFontSet(dpy, font_set)
    Display        *dpy;
    XFontSet        font_set;
{
    if (((XsiFontSet)font_set)->xlc)
	_XlcFreeLocale(((XsiFontSet)font_set)->xlc);
    if (font_set->core.num_of_fonts > 0)
	Xfree((char *) ((XsiFontSet)font_set)->ctid);
}

static RConst XFontSetMethodsRec fs_methods = {
    _XsiFreeFontSet,
    _XsimbTextEscapement,
    _XsimbTextExtents,
    _XsimbTextPerCharExtents,
    _XsimbDrawString,
    _XsimbDrawImageString,
    _XsiwcTextEscapement,
    _XsiwcTextExtents,
    _XsiwcTextPerCharExtents,
    _XsiwcDrawString,
    _XsiwcDrawImageString
};

static RConst XFontSetMethodsRec fs8_methods = {
    _XsiFreeFontSet,
    _Xsimb8TextEscapement,
    _Xsimb8TextExtents,
    _XsimbTextPerCharExtents,
    _Xsimb8DrawString,
    _Xsimb8DrawImageString,
    _XsiwcTextEscapement,
    _XsiwcTextExtents,
    _XsiwcTextPerCharExtents,
    _XsiwcDrawString,
    _XsiwcDrawImageString
};

#define lowercase(c)        ((isalpha(c) && isupper(c)) ? tolower(c) : c)

#if defined(MUSTCOPY)
static void
SetFontInfo(des, src)
XFontStruct *des, *src ;
{
    des->ext_data = src->ext_data;    
    des->fid = src->fid;              
    des->direction = src->direction;  
    des->min_char_or_byte2 = src->min_char_or_byte2;
    des->max_char_or_byte2 = src->max_char_or_byte2;
    des->min_byte1 = src->min_byte1;     
    des->max_byte1 = src->max_byte1;     
    des->all_chars_exist = src->all_chars_exist;  
    des->default_char = src->default_char;
    des->n_properties = src->n_properties;
    des->properties = src->properties;
    des->min_bounds = src->min_bounds;
    des->max_bounds = src->max_bounds;
    des->per_char = src->per_char; 
    des->ascent = src->ascent;  
    des->descent = src->descent;
}
#endif

static int
compareLowercase(s1, s2)
char    *s1;
char    *s2;
{
    register char c1, c2;

    while ((c1 = *s1) && (c2 = *s2)) {
        c1 = lowercase(c1);
        c2 = lowercase(c2);
        if (c1 != c2)
            break;
        s1++, s2++;
    }
    /*
     * do not use c1 and c2 here.
     */
    return (*s1 - *s2);
}

static int
compareNLowercase(s1, s2, n)
char    *s1;
char    *s2;
int      n;
{
    register char c1, c2;

    while ((n > 0) && (c1 = *s1) && (c2 = *s2)) {
        c1 = lowercase(c1);
        c2 = lowercase(c2);
        if (c1 != c2)
            break;
        s1++, s2++, n--;
    }
    /*
     * do not use c1 and c2 here.
     * in the case, if (c1 = *s1) is NULL, stop while, so no value of c2.
     */
    return (n == 0)? 0: (*s1 - *s2);
}

#ifdef XML
static char *
copyLowercase(s1, s2)
char    *s1, *s2;
{
    register char c;

    while (c = *s2++)
        *s1++ = lowercase(c);
    *s1 = 0;
    return s1;
}
#endif


static char **
reallocList(list, count)
char           *list[];
int             count;
{
    char          **ret;
    int             i, j;
    int		    datalen;
    char	   *data;

    ret = (char **) Xmalloc((unsigned)sizeof(char *) * (count + 1));
    if (!ret) return NULL;

    datalen = 0;
    for (i = 0, j = 0; j < count; i++) {
	if (list[i]) {
	    datalen += strlen(list[i]);
	    j++;
	}
    }
    data = (char *) Xmalloc ((unsigned)(datalen + count) * sizeof (char));
    if (!data) {
	Xfree ((char *) ret);
	return NULL;
    }
    for (i = 0, j = 0; j < count; i++) {
	if (list[i]) {
	    ret[j] = data;
	    strcpy(data, list[i]);
	    data += strlen(list[i]) + 1;
	    j++;
	}
    }
    ret[j] = NULL;

    return ret;
}

static XFontStruct **
reallocFontStruct(font, count)
XFontStruct    *font[];
int             count;
{
    XFontStruct   **ret;
    int             i, j;
#if !defined(PRELOAD) || defined(XML)
    XFontStruct	   *data;
#endif

    ret = (XFontStruct **) Xmalloc((unsigned)sizeof(XFontStruct *) * count);
    if (!ret) return NULL;
#if defined(PRELOAD) && !defined(XML)
    for (i = 0, j = 0; j < count; i++) {
	if (font[i]) {
	    if (font[i]->fid > 0) {
		ret[j] = font[i];
		j++;
	    }
	}
    }
#else
    for (i = 0, j = 0; j < count; i++) {
	if (font[i]) {
	    if (font[i]->fid > 0) {
		ret[j] = font[i];
	    } else {
		data = (XFontStruct *) Xmalloc (sizeof(XFontStruct));
		if (!data) {
		    for (j--; j >= 0; j--) {
			if (ret[j])
			    Xfree ((char *) ret[j]);
		    }
		    Xfree ((char *) ret);
		    return NULL;
		}
		ret[j] = data;
		bcopy((char *)font[i], (char *)data, sizeof (XFontStruct));
	    }
	    j++;
	}
    }
#endif
    return ret;
}

static int *
reallocCtidList(fn, list, count)
char   *fn[];
int	list[];
int	count;
{
    int		*ret;
    int		i, j;

    ret = (int *) Xmalloc((unsigned)sizeof(int) * count);
    if (!ret) return NULL;

    for (i = 0, j = 0; j < count; i++) {
	if (fn[i]) {
	    ret[j] = list[i];
	    j++;
	}
    }

    return ret;
}

static          Bool
isXLFDname(font_name)
char           *font_name;
{
    int             i;
    char           *ptr = font_name;

    if (*ptr == XFNEXT_PREFIX) {
        if ((ptr = index(ptr, XFNDELIM)) == NULL) {
            return False;
        }
    }
    if (*ptr != XFNDELIM) {
        return False;
    }
    for (i = 0; i < DELIM_COUNT; i++) {
        if ((ptr = index(ptr + 1, XFNDELIM)) == NULL) {
            return False;
        }
    }
    return True;
}


static Bool
isBaseFontName(font_name)
    char           *font_name;
{
    int             i;
    char           *ptr = font_name;

    if (*ptr == XFNEXT_PREFIX) {
        if ((ptr = index(ptr, XFNDELIM)) == NULL) {
            return False;
        }
    }
    if (*ptr != XFNDELIM) {
        return False;
    }
    for (i = 0; i < DELIM_COUNT-2; i++) {
        if ((ptr = index(ptr + 1, XFNDELIM)) == NULL) {
            return False;
        }
    }
    if ((ptr = index(ptr + 1, XFNDELIM)) == NULL) {
	return True;
    }
    return False;
}

static char    *
getXLFDName(dpy, info)
Display        *dpy;
XFontStruct    *info;
{
    char           *return_value;
    unsigned long   val;

    if (XGetFontProperty(info, XA_FONT, &val) == False) {
        dbg_printf("getXLFD: XGetFontProperty == False\n", 0, 0);
        return NULL;
    }
    return_value = XGetAtomName(dpy, val);
    if (isXLFDname(return_value)) {
        return return_value;
    } else {
        dbg_printf("getXLFD: isXLFDname == Not XLFD\n", 0, 0);
        Xfree(return_value);
        return NULL;
    }
}

static Bool
matchCharset(font_name, cset, GLorGR)
    char       *font_name,
               *cset;
    _CSID	GLorGR;
{
    char	*start = font_name;
    int		i;

    for (i = 0; i < CSETPOS; i++) {
        start = index(start, XFNDELIM);
        if (start == NULL) {
            return False;
        }
        start++;
    }
    if (compareLowercase(start, cset) == 0)
	return (True);

    /* jisx0208.1990-0 is super set of jisx0208.1983-0 */
    if (strlen(cset) == 15 && strlen(start) == 15 &&
	*(cset+14) == *(start+14) &&
	!strncmp(cset, "jisx0208.1983", 13)) {
         return ((compareNLowercase(start, "jisx0208.1990", 13) == 0) ? True : False);
    }
    /* GL of ISO8859.* is same as GL of ISO8859.1 */
    if (GLorGR == GL && !strncmp(cset, "iso8859-", 8)) {
         return ((compareNLowercase(start, cset, 8) == 0) ? True : False);
    }
    return (False);
}

static int
findFont(dpy, fname, xlocale, f_tmp, s_tmp, id_tmp)
    Display	*dpy;
    char        *fname;
    XLocale	 xlocale;
    char        *f_tmp[];
    XFontStruct *s_tmp[];
    int		 id_tmp[];
{
    XFontStruct	*info = NULL;
    char       **list;
    int		 count;
    int		 cset_count;
    char	*fn = NULL;
    int		 ret = 0;
    _CSID	 i;

    if (isXLFDname(fname)) {
	if(! (fn = Xmalloc((unsigned)strlen(fname) + 1)))
	    return 0;
	strcpy(fn, fname);
    } else {
	list = XListFontsWithInfo(dpy, fname, 1, &count, &info);
	if (count != 1) {
	    /* never go to here. */
	    goto _err_return;
	}
	if ((fn = getXLFDName(dpy, &info[0])) == NULL) {
	    goto _err_return;
	}
    }
    dbg_printf("findFont: %s\n", fn, 0);

    cset_count = _Xmbfsnum(xlocale);
    for (i = CODESET0; (int)i < cset_count; i++) {
	if (s_tmp[i] == NULL) {
	    Charset	*cset = _Xmbfscs(xlocale, i);
	    dbg_printf("findFont: charset[%d] = %s\n", i, cset->cs_name);

	    if (matchCharset(fn, cset->cs_name, cset->cs_GLorGR) == True) {
		dbg_printf("findFont: matched\n", 0, 0);
		ret++;
#ifdef PRELOAD
		if (!(s_tmp[i] = XLoadQueryFont(dpy, fn))) ret--;
#else /* PRELOAD */
		if (!info)
		    list = XListFontsWithInfo(dpy, fname, 1, &count, &info);
		    if (count != 1) {
			/* never go to here. */
			goto _err_return;
		    }
		if(! (s_tmp[i] = (XFontStruct *) Xmalloc(sizeof(XFontStruct)))){
		    goto _err_return;
		}
# ifndef MUSTCOPY
		s_tmp[i] = info[0];
# else /* MUSTCOPY */
		SetFontInfo(&s_tmp[i], &info[0]);
# endif /* MUSTCOPY */
		/* indicate the font has not loaded, only info. */
		s_tmp[i]->fid = 0;
#endif /* PRELOAD */
		id_tmp[i] = cset->cs_id;
		if(! (f_tmp[i] = (char *) Xmalloc(strlen(fn) + 1))) {
		    --ret;
		    goto _err_return;
		}
		strcpy(f_tmp[i], fn);
	    } else {
		f_tmp[i] = NULL;
	    }
	}
    }
_err_return:
    if (info) {
	XFreeFontInfo(list, info, count);
    }
    if (fn) Xfree((char *)fn);
    return ret;

}

#undef  MIN
#undef  MAX
#define MIN(a, b)   ((a) = (((a) > (b))? (b): (a)))
#define MAX(a, b)   ((a) = (((a) < (b))? (b): (a)))

/* _MinBounds: if any element of src XCharStruct is less than           */
/*             that of des, replace the des XCharStruct's               */
/*             corresponding element with that of src                   */
/*       des:  destination min_bound                                    */
/*       src:  source min_bound                                         */

static void
_MinBounds(des, src)
XCharStruct    *des;
XCharStruct    *src;
{
    MIN(des->lbearing, src->lbearing);
    MIN(des->rbearing, src->rbearing);
    MIN(des->width, src->width);
    MIN(des->ascent, src->ascent);
    MIN(des->descent, src->descent);
    MIN(des->attributes, src->attributes);
}

/* _MinBounds: if any element of src XCharStruct is greater than        */
/*             that of des, replace the des XCharStruct's               */
/*             corresponding element with that of src                   */
/*       des:  destination min_bound                                    */
/*       src:  source min_bound                                         */

static void
_MaxBounds(des, src)
XCharStruct    *des;
XCharStruct    *src;
{
    MAX(des->lbearing, src->lbearing);
    MAX(des->rbearing, src->rbearing);
    MAX(des->width, src->width);
    MAX(des->ascent, src->ascent);
    MAX(des->descent, src->descent);
    MAX(des->attributes, src->attributes);
}

static void
getFontCharStruct(font_strct, font_count,
          min_bounds, max_bounds, ascent, descent)
XFontStruct   **font_strct;
int             font_count;
XCharStruct    *min_bounds,
               *max_bounds;
int            *ascent,
               *descent;
{
    int		i;

    *min_bounds = font_strct[0]->min_bounds;
    *max_bounds = font_strct[0]->max_bounds;
    *ascent = font_strct[0]->ascent;
    *descent = font_strct[0]->descent;
    for (i = 1; i < font_count; i++) {
	_MinBounds(min_bounds, &font_strct[i]->min_bounds);
	_MaxBounds(max_bounds, &font_strct[i]->max_bounds);

	if (*ascent < font_strct[i]->ascent) {
	    *ascent = font_strct[i]->ascent;
	}
	if (*descent < font_strct[i]->descent) {
	    *descent = font_strct[i]->descent;
	}
    }
}


#ifdef XML
static int
AlreadyLoad(id_tmp, found, charset, GLorGR)
int		id_tmp[];
int		found;
char		*charset;
_CSID		GLorGR;
{
    int		i;
    if ((id_tmp[found] = _XcwNameGetGLorGRId(charset, GLorGR)) == ND)
	return 0;
    for (i = 0; i < found; i++) {
	if (id_tmp[i] == id_tmp[found])
	    return 0;
    }
    return 1;
}

static int
TryLoad(s_tmp, id_tmp, found, info, charset, GLorGR)
XFontStruct	*s_tmp[];
int		id_tmp[];
int		found;
XFontStruct     *info;
char		*charset;
_CSID		GLorGR;
{
    if (! (s_tmp[found] = (XFontStruct *)Xmalloc(sizeof(XFontStruct))))
	return 0;
#ifndef MUSTCOPY
    *s_tmp[found] = info[0];
#else /* MUSTCOPY */
    SetFontInfo(s_tmp[found], &info[0]);
#endif /* MUSTCOPY */
    s_tmp[found]->fid = 0;      /* make sure unloaded */
    return 1;
}

XFontSet
_XsimlCreateFontSet(lcd, dpy, base_font_name_list, font_list, font_count,
		    missing_charset_list, missing_charset_count)
    XLCd	    lcd;
    Display        *dpy;
    char           *base_font_name_list;
    char          **font_list;
    int             font_count;
    char         ***missing_charset_list;
    int            *missing_charset_count;
{
    XsiFontSet      font_set;
    char           *m_tmp[MAXCHARSETS],
                   *f_tmp[MAXCHARSETS];
    XFontStruct    *s_tmp[MAXCHARSETS];
    int             id_tmp[MAXCHARSETS];
    int             found = 1, miss = 0, loadASCIIFONT = 0;
    int             count, i, j, notfound;

    for (i = 0; i < font_count; i++) {
        char	**list;
	char	tmp[256];
	if (isBaseFontName(font_list[i])) {
	    strcpy(tmp, font_list[i]);
	    strcat(tmp, "-*-*");
	    list = XListFonts(dpy, tmp, 1024, &count);
	} else {
	    list = XListFonts(dpy, font_list[i], 1024, &count);
	}

        notfound = 1;
        for (j = 0; j < count; j++) {
	    XFontStruct  *info;
	    char	**info_list;
	    int		  count_list;
	    if (found >= (MAXCHARSETS - 1))
		break;
	    info = NULL;
	    if (isXLFDname(list[j]) ) {
		if (! (f_tmp[found] = Xmalloc((unsigned)strlen(list[j]) + 1))) {
		    return NULL;
		}
		strcpy(f_tmp[found], list[j]);
	    } else  {
		info_list = XListFontsWithInfo(dpy, list[j], 1, &count_list, &info);
		if (count_list != 1) {
		    /* never go to here. */
		    continue;
		}
		f_tmp[found] = getXLFDName(dpy, &info[0]);
	    }
	    if (f_tmp[found] != NULL) {
		char	charset[64];
		char	*start = f_tmp[found];
		int	k;

		for (k = 0; k < CSETPOS; k++) {
		    start = index(start, XFNDELIM);
		    if (start == NULL)
			/* not goes here */
			return NULL;
		    start++;
		}
		copyLowercase(charset, start);
		/* If font name is ISO8859.* Treate for ISO8859.1 GL */
		if (strncmp(charset, "iso8859-", 8) == 0) {
		    if(!loadASCIIFONT) {
			f_tmp[0] = f_tmp[found];
			s_tmp[0] = XLoadQueryFont(dpy, f_tmp[0]);
			id_tmp[0] = CODESET0;
			loadASCIIFONT = 1;
		    }
		} else {
		    if (AlreadyLoad(id_tmp, found, charset, GL)) {

			if (!info)
			    info_list = XListFontsWithInfo(dpy, list[j], 1, &count_list, &info);
			if (count_list != 1)
			    /* never go to here. */
			    continue;

			f_tmp[found+1] = f_tmp[found];
			found += TryLoad(s_tmp, id_tmp, found, info, charset, GL);
		    }
		}
		if (AlreadyLoad(id_tmp, found, charset, GR)) {
		    if (!info)
			info_list = XListFontsWithInfo(dpy, list[j], 1, &count_list, &info);
		    if (count_list != 1)
			/* never go to here. */
			continue;
		    found += TryLoad(s_tmp, id_tmp, found, info, charset, GR);
		}
		notfound = 0;
		if (info)
		    XFreeFontInfo(info_list, info, count_list);
	    }
	}
        if (notfound)
            m_tmp[miss++] = font_list[i];
	XFreeFontNames(list);
    }

    *missing_charset_list = reallocList(m_tmp, miss);
    *missing_charset_count = miss;

    if (miss == font_count) {
        return NULL;
    }

    if (!loadASCIIFONT) {
	if(! (f_tmp[0] = (char *)Xmalloc(strlen("fixed") + 1)))
	    return NULL;
	strcpy(f_tmp[0], "fixed");	/* use defalt font */
	s_tmp[0] = XLoadQueryFont(dpy, f_tmp[0]);
    }

    if(! (font_set = (XsiFontSet) Xmalloc(sizeof(XsiFontSetRec)))) {
	if (s_tmp[0] && s_tmp[0]->fid) {
	    XFreeFont(dpy, s_tmp[0]);
	}
	return NULL;
    }    
    XFreeStringList(font_list);

    font_set->methods = (XFontSetMethods) &fs_methods;
    font_set->core.base_name_list = base_font_name_list;
    font_set->core.num_of_fonts = found;
    font_set->core.lcd = lcd;
    font_set->core.font_name_list = reallocList(f_tmp, found);
    font_set->core.font_struct_list = reallocFontStruct(s_tmp, found);
    font_set->core.context_dependent = False;
    font_set->display = dpy;
    font_set->xlc = NULL;
    font_set->ctid = reallocCtidList(f_tmp, id_tmp, found);
    getFontCharStruct(font_set->core.font_struct_list, found,
		      &font_set->min_bounds, &font_set->max_bounds,
		      &font_set->ascent, &font_set->descent);
    font_set->core.default_string = NULL;
    for (i = 0; (int)i < found; i++) {
	Xfree((char *)f_tmp[i]);
    }

    /*
     * set the ink bounding box of font_set.
     */
    font_set->core.font_set_extents.max_ink_extent.x =
            font_set->min_bounds.lbearing;
    font_set->core.font_set_extents.max_ink_extent.y =
            - font_set->max_bounds.ascent;
    font_set->core.font_set_extents.max_ink_extent.width =
            font_set->max_bounds.rbearing - font_set->min_bounds.lbearing;
    font_set->core.font_set_extents.max_ink_extent.height =
            font_set->max_bounds.ascent + font_set->max_bounds.descent;
    /*
     * set the logical bounding box of font_set.
     * suppose the lbearing >= 0;
     */
    font_set->core.font_set_extents.max_logical_extent.x = 0;
    font_set->core.font_set_extents.max_logical_extent.y =
            - font_set->ascent;
    font_set->core.font_set_extents.max_logical_extent.width =
            font_set->max_bounds.width;
    font_set->core.font_set_extents.max_logical_extent.height =
            font_set->ascent + font_set->descent;

    return (XFontSet)font_set;
}

XFontSet
XmlCreateFontSet (dpy, base_font_name_list, missing_charset_list,
		  missing_charset_count, def_string)
    Display        *dpy;
    char           *base_font_name_list;
    char         ***missing_charset_list;
    int            *missing_charset_count;
    char          **def_string;
{
    XLCd lcd = _XlcCurrentLC();
    char *base_name;
    char **name_list;
    int count;
    XFontSet font_set;

    *missing_charset_list = NULL;
    *missing_charset_count = 0;
    if (!lcd)
	return NULL;
    base_name = (char *)Xmalloc(strlen(base_font_name_list) + 1);
    if (!base_name)
	return NULL;
    strcpy(base_name, base_font_name_list);
    name_list = _XParseBaseFontNameList(base_name, &count);
    if (!name_list) {
	Xfree(base_name);
        return NULL;
    }
    font_set = _XsimlCreateFontSet (lcd, dpy, base_name, name_list, count,
				    missing_charset_list,
				    missing_charset_count);
    if (!font_set) {
	XFreeStringList(name_list);
	Xfree(base_name);
    } else if (def_string) {
	*def_string = font_set->core.default_string;
	if (!*def_string)
	    *def_string = "";
    }
    return font_set;
}
#endif /* XML */

XFontSet
_XsiCreateFontSet(lcd, dpy, base_font_name_list, font_list, font_count,
		  missing_charset_list, missing_charset_count)
    XLCd	    lcd;
    Display        *dpy;
    char           *base_font_name_list;
    char          **font_list;
    int             font_count;
    char         ***missing_charset_list;
    int            *missing_charset_count;
{
    XsiFontSet      font_set;
    int             cset_count;
    char           *m_tmp[MAXCHARSETS],
                   *f_tmp[MAXCHARSETS];
    XFontStruct    *s_tmp[MAXCHARSETS];
    int		    id_tmp[MAXCHARSETS];
    int             found = 0, miss = 0, j;
    _CSID	    i;
    XLocale	    xlocale = ((XsiLCd)lcd)->xlc;

    cset_count = _Xmbfsnum(xlocale);
    for (i = 0; (int)i < cset_count; i++) {
	s_tmp[i] = NULL;
    }
    for (j = 0; j < font_count && found < cset_count; j++) {
        char  **list;
        int	k, count;
	char	tmp[256];

	if (isBaseFontName(font_list[j])) {
	    strcpy(tmp, font_list[j]);
	    strcat(tmp, "-*-*");
	    list = XListFonts(dpy, tmp, 1024, &count);
	} else {
	    list = XListFonts(dpy, font_list[j], 1024, &count);
	}
	for (k = 0; k < count && found < cset_count; k++) {
	    dbg_printf("XCreateFontSet: font = %s\n", list[k], 0);
	    found += findFont(dpy, list[k], xlocale, f_tmp, s_tmp, id_tmp);
	}
	XFreeFontNames(list);
    }
    if (found < cset_count) {
	/* find missing charset */
	for (i = 0; (int)i < cset_count; i++) {
	    if (s_tmp[i] == NULL) {
		Charset	*cset = _Xmbfscs(xlocale, i);
		m_tmp[miss] = cset->cs_name;
		dbg_printf("XCreateFontSet: m_tmp[%d] = %s\n",
			   miss, m_tmp[miss]);
		miss++;
	    }
	}
    }

    *missing_charset_list = reallocList(m_tmp, miss);
    *missing_charset_count = miss;

    if (miss == cset_count) {
        return NULL;
    }
    if (! (font_set = (XsiFontSet)Xmalloc(sizeof(XsiFontSetRec)))) {
	for (i = 0; (int)i < found; i++) {
	    if (s_tmp[i] && s_tmp[i]->fid)
		XFreeFont(dpy, s_tmp[i]);
	}
        return NULL;
    }
    XFreeStringList(font_list);

#ifdef PRELOAD
    if (!miss && _Xmbtype(xlocale) == CDS_STATELESS && cset_count <= 2 &&
	xlocale->xlc_db->lc_codeset->cds_mblen[0] == 1 &&
	(cset_count == 1 || xlocale->xlc_db->lc_codeset->cds_mblen[1] == 1) &&
	found <= 2 && (found == 1 || !strcmp(f_tmp[0], f_tmp[1])))
	font_set->methods = (XFontSetMethods) &fs8_methods;
    else
#endif
	font_set->methods = (XFontSetMethods) &fs_methods;
    font_set->core.base_name_list = base_font_name_list;
    font_set->core.num_of_fonts = found;
    font_set->core.lcd = lcd;
    font_set->display = dpy;
    font_set->xlc = _XlcDupLocale(xlocale);
    font_set->core.font_name_list = reallocList(f_tmp, found);
    font_set->core.font_struct_list = reallocFontStruct(s_tmp, found);
    font_set->core.context_dependent = False;
    font_set->ctid = reallocCtidList(f_tmp, id_tmp, found);
    getFontCharStruct(font_set->core.font_struct_list, found,
		      &font_set->min_bounds, &font_set->max_bounds,
		      &font_set->ascent, &font_set->descent);
    font_set->core.default_string = NULL;
    for (i = 0; (int)i < found; i++) {
	if (f_tmp[i]) Xfree((char *)f_tmp[i]);
    }

    /*
     * set the ink bounding box of font_set.
     */
    font_set->core.font_set_extents.max_ink_extent.x = 
            font_set->min_bounds.lbearing;
    font_set->core.font_set_extents.max_ink_extent.y = 
            - font_set->max_bounds.ascent;
    font_set->core.font_set_extents.max_ink_extent.width = 
            font_set->max_bounds.rbearing - font_set->min_bounds.lbearing;
    font_set->core.font_set_extents.max_ink_extent.height = 
            font_set->max_bounds.ascent + font_set->max_bounds.descent;
    /*
     * set the logical bounding box of font_set.
     * suppose the lbearing >= 0;
     */
    font_set->core.font_set_extents.max_logical_extent.x = 0;
    font_set->core.font_set_extents.max_logical_extent.y = 
            - font_set->ascent;
    font_set->core.font_set_extents.max_logical_extent.width = 
            font_set->max_bounds.width;
    font_set->core.font_set_extents.max_logical_extent.height = 
            font_set->ascent + font_set->descent;

    return (XFontSet)font_set;
}


XFontStruct *
_XsiQueryFontSetFromId(font_set, ctid)
    XFontSet   font_set;
    int	       ctid;
{
    XFontStruct *font;
    int          i;

    for (i = 0; i < font_set->core.num_of_fonts; i++) {
	if (((XsiFontSet)font_set)->ctid[i] == ctid)
	    break;
    }
    if (i == font_set->core.num_of_fonts)
	return NULL;

    if (font_set->core.font_struct_list[i]->fid > 0)
        /* already loaded */
        return font_set->core.font_struct_list[i];

    if ((font = XLoadQueryFont(((XsiFontSet)font_set)->display,
			       font_set->core.font_name_list[i])) == NULL)
        return NULL;
    Xfree((char *)font_set->core.font_struct_list[i]);
    font_set->core.font_struct_list[i] = font;
    return font;
}


#ifdef XML
void
_XsiQueryFontSetWithName(font_set, font_name)
    XFontSet	font_set;
    char       *font_name; /* font name or charset name */
{
    Display    *dpy = ((XsiFontSet)font_set)->display;
    char       *fname;
    int		ctid;
    int		count, i;
    char	**list;
    char	tmp[256];
    char	charset[64];

    if (isBaseFontName(font_name)) {
	strcpy(tmp, font_name);
	strcat(tmp, "-*-*");
	list = XListFonts(dpy, tmp, 1024, &count);
    } else {
	list = XListFonts(dpy, font_name, 1024, &count);
    }
    if (count == 0) { /* charset name */
	copyLowercase(charset, font_name);
	if ((ctid = _XcwNameGetGLorGRId(charset, GL)) != ND) {
	    _XsiQueryFontSetFromId(font_set, ctid);
	}
	if ((ctid = _XcwNameGetGLorGRId(charset, GR)) != ND) {
	    _XsiQueryFontSetFromId(font_set, ctid);
	}
    } else {	/* font name */
	for (i = 0; i < count; i++) {
	    if (isXLFDname(list[i])) {
		fname = list[i];
	    } else  {
		char	**info_list;
		XFontStruct *font;
		info_list = XListFontsWithInfo(dpy, list[i],
					       1, &count, &font);
		if (count != 1)
		    /* not goes here */
		    return;
		fname = getXLFDName(dpy, font);
		XFreeFontInfo(info_list, font, count);
	    }
	    if (fname != NULL) {
		char	*start = fname;
		int	k;

		for (k = 0; k < CSETPOS; k++) {
		    start = index(start, XFNDELIM);
		    if (start == NULL)
			/* not goes here */
			return;
		    start++;
		}
		copyLowercase(charset, start);
		if ((ctid = _XcwNameGetGLorGRId(charset, GL)) != ND) {
		    _XsiQueryFontSetFromId(font_set, ctid);
		}
		if ((ctid = _XcwNameGetGLorGRId(charset, GR)) != ND) {
		    _XsiQueryFontSetFromId(font_set, ctid);
		}
	    }
	}
    }
    XFreeFontNames(list);
}
#endif /* XML */
