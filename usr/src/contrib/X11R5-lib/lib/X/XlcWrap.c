/*
 * $XConsortium: XlcWrap.c,v 11.9 92/04/24 16:07:40 rws Exp $
 */

/*
 * Copyright 1991 by the Massachusetts Institute of Technology
 * Copyright 1991 by the Open Software Foundation
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Open Software Foundation and M.I.T.
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Open Software
 * Foundation and M.I.T. make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *
 * OPEN SOFTWARE FOUNDATION AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD TO
 * THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS, IN NO EVENT SHALL OPEN SOFTWARE FOUNDATIONN OR M.I.T. BE
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 *		 M. Collins		OSF  
 */				

#include "Xlibint.h"
#include "Xlcint.h"
#include <X11/Xlocale.h>
#include <X11/Xos.h>

#if __STDC__
#define Const const
#else
#define Const /**/
#endif

#ifdef X_NOT_STDC_ENV
extern char *getenv();
#endif

extern XLCd _XlcDefaultLoader(
#if NeedFunctionPrototypes
    char*
#endif
);

#if NeedFunctionPrototypes
char *
XSetLocaleModifiers(
    _Xconst char   *modifiers
)
#else
char *
XSetLocaleModifiers(modifiers)
    char        *modifiers;
#endif
{
    XLCd lcd = _XlcCurrentLC();
    char *user_mods;

    if (!lcd)
	return (char *) NULL;
    if (!modifiers)
	return lcd->core.modifiers;
    user_mods = getenv("XMODIFIERS");
    modifiers = (*lcd->methods->map_modifiers) (lcd,
						user_mods, (char *)modifiers);
    if (modifiers)
	lcd->core.modifiers = (char *)modifiers;
    return (char *)modifiers;
}

Bool
XSupportsLocale()
{
    return _XlcCurrentLC() != (XLCd)NULL;
}

Bool _XlcValidModSyntax(mods, valid_mods)
    char *mods;
    char **valid_mods;
{
    int i;
    char **ptr;

    while (mods && (*mods == '@')) {
	mods++;
	if (*mods == '@')
	    break;
	for (ptr = valid_mods; *ptr; ptr++) {
	    i = strlen(*ptr);
	    if (strncmp(mods, *ptr, i) || (mods[i] != '='))
		continue;
	    mods = index(mods+i+1, '@');
	    break;
	}
    }
    return !mods || !*mods;
}

static Const char *im_valid[] = {"im", (char *)NULL};

/*ARGSUSED*/
char *
_XlcDefaultMapModifiers (lcd, user_mods, prog_mods)
    XLCd lcd;
    char *user_mods;
    char *prog_mods;
{
    int i;
    char *mods;

    if (!_XlcValidModSyntax(prog_mods, (char **)im_valid))
	return (char *)NULL;
    if (!_XlcValidModSyntax(user_mods, (char **)im_valid))
	return (char *)NULL;
    i = strlen(prog_mods) + 1;
    if (user_mods)
	i += strlen(user_mods);
    mods = Xmalloc(i);
    if (mods) {
	strcpy(mods, prog_mods);
	if (user_mods)
	    strcat(mods, user_mods);
    }
    return mods;
}

#ifndef	lint
static lock;
#endif /* lint */

static XLCd *lcd_list;
static XLCdLoadProc *loaders;

Bool
_XlcAddLoader (proc)
    XLCdLoadProc proc;
{
    if (!loaders) {
	loaders = (XLCdLoadProc *) Xmalloc (2 * sizeof(XLCdLoadProc));
	if (!loaders)
	    return False;
	loaders[0] = proc;
	loaders[1] = (XLCdLoadProc) NULL;
    } else {
    }
    return True;
}

/*
 * Get the XLCd for the current locale
 */

XLCd
_XlcCurrentLC ()
{
    char *name;
    XLCd lcd;
    int i, j;

    name = setlocale (LC_CTYPE, (char *)NULL);

    LockMutex(&lock);

    /*
     * if first-time, build a list and load the one needed
     */
    if (!lcd_list) {
	lcd_list = (XLCd *) Xmalloc (sizeof(XLCd));
	if (!lcd_list)
	    goto bad;
	lcd_list[0] = (XLCd) NULL;
    }

    /*
     * search for needed lcd, if found return it
     */
    for (i = 0; lcd_list[i]; i++) {
	lcd = lcd_list[i];
	if (!strcmp (lcd->core.name, name))
	    goto found;
    }

    if (!loaders && !_XlcAddLoader(_XlcDefaultLoader))
	goto bad;

    /*
     * not there, so try to get and add to list
     */
    for (j = 0; loaders[j]; j++) {
	lcd = (*loaders[j]) (name);
	if (lcd) {
	    XLCd *new_list;

	    new_list = (XLCd *) Xrealloc ((char *)lcd_list,
					  (sizeof (XLCd) * (i+2)));
	    if (!new_list)
		goto bad;
	    lcd_list = new_list;
	    lcd_list[i]   = lcd;
	    lcd_list[i+1] = (XLCd) NULL;
	    goto found;
	}
    }

bad:
    lcd = (XLCd) NULL;

found:
    UnlockMutex(&lock);
    return lcd;
}
