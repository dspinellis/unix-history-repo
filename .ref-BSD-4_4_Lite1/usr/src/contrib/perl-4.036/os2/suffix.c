/*
 * Suffix appending for in-place editing under MS-DOS and OS/2.
 *
 * Here are the rules:
 *
 * Style 0:  Append the suffix exactly as standard perl would do it.
 *           If the filesystem groks it, use it.  (HPFS will always
 *           grok it.  FAT will rarely accept it.)
 *
 * Style 1:  The suffix begins with a '.'.  The extension is replaced.
 *           If the name matches the original name, use the fallback method.
 *
 * Style 2:  The suffix is a single character, not a '.'.  Try to add the 
 *           suffix to the following places, using the first one that works.
 *               [1] Append to extension.  
 *               [2] Append to filename, 
 *               [3] Replace end of extension, 
 *               [4] Replace end of filename.
 *           If the name matches the original name, use the fallback method.
 *
 * Style 3:  Any other case:  Ignore the suffix completely and use the
 *           fallback method.
 *
 * Fallback method:  Change the extension to ".$$$".  If that matches the
 *           original name, then change the extension to ".~~~".
 *
 * If filename is more than 1000 characters long, we die a horrible
 * death.  Sorry.
 *
 * The filename restriction is a cheat so that we can use buf[] to store
 * assorted temporary goo.
 *
 * Examples, assuming style 0 failed.
 *
 * suffix = ".bak" (style 1)
 *                foo.bar => foo.bak
 *                foo.bak => foo.$$$	(fallback)
 *                foo.$$$ => foo.~~~	(fallback)
 *                makefile => makefile.bak
 *
 * suffix = "~" (style 2)
 *                foo.c => foo.c~
 *                foo.c~ => foo.c~~
 *                foo.c~~ => foo~.c~~
 *                foo~.c~~ => foo~~.c~~
 *                foo~~~~~.c~~ => foo~~~~~.$$$ (fallback)
 *
 *                foo.pas => foo~.pas
 *                makefile => makefile.~
 *                longname.fil => longname.fi~
 *                longname.fi~ => longnam~.fi~
 *                longnam~.fi~ => longnam~.$$$
 *                
 */

#include "EXTERN.h"
#include "perl.h"
#ifdef OS2
#define INCL_DOSFILEMGR
#define INCL_DOSERRORS
#include <os2.h>
#endif /* OS2 */

static char suffix1[] = ".$$$";
static char suffix2[] = ".~~~";

#define ext (&buf[1000])

add_suffix(str,suffix)
register STR *str;
register char *suffix;
{
    int baselen;
    int extlen;
    char *s, *t, *p;
    STRLEN slen;

    if (!(str->str_pok)) (void)str_2ptr(str);
    if (str->str_cur > 1000)
        fatal("Cannot do inplace edit on long filename (%d characters)", str->str_cur);

#ifdef OS2
    /* Style 0 */
    slen = str->str_cur;
    str_cat(str, suffix);
    if (valid_filename(str->str_ptr)) return;

    /* Fooey, style 0 failed.  Fix str before continuing. */
    str->str_ptr[str->str_cur = slen] = '\0';
#endif /* OS2 */

    slen = strlen(suffix);
    t = buf; baselen = 0; s = str->str_ptr;
    while ( (*t = *s) && *s != '.') {
	baselen++;
	if (*s == '\\' || *s == '/') baselen = 0;
 	s++; t++;
    }
    p = t;

    t = ext; extlen = 0;
    while (*t++ = *s++) extlen++;
    if (extlen == 0) { ext[0] = '.'; ext[1] = 0; extlen++; }

    if (*suffix == '.') {        /* Style 1 */
        if (strEQ(ext, suffix)) goto fallback;
	strcpy(p, suffix);
    } else if (suffix[1] == '\0') {  /* Style 2 */
        if (extlen < 4) { 
	    ext[extlen] = *suffix;
	    ext[++extlen] = '\0';
        } else if (baselen < 8) {
   	    *p++ = *suffix;
	} else if (ext[3] != *suffix) {
	    ext[3] = *suffix;
	} else if (buf[7] != *suffix) {
	    buf[7] = *suffix;
	} else goto fallback;
	strcpy(p, ext);
    } else { /* Style 3:  Panic */
fallback:
	(void)bcopy(strEQ(ext, suffix1) ? suffix2 : suffix1, p, 4+1);
    }
    str_set(str, buf);
}

#ifdef OS2
int 
valid_filename(s)
char *s;
{
    HFILE hf;
    USHORT usAction;

    switch(DosOpen(s, &hf, &usAction, 0L, 0, FILE_OPEN,
	OPEN_ACCESS_READONLY | OPEN_SHARE_DENYNONE, 0L)) {
    case ERROR_INVALID_NAME:
    case ERROR_FILENAME_EXCED_RANGE:
	return 0;
    case NO_ERROR:
	DosClose(hf);
	/*FALLTHROUGH*/
    default:
	return 1;
    }
}
#endif /* OS2 */
