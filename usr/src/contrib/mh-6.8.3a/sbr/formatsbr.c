/* formatsbr.c - format string interpretation */
#ifndef	lint
static char ident[] = "@(#)$Id: formatsbr.c,v 1.24 1993/08/20 15:48:14 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../h/addrsbr.h"
#include "../h/formatsbr.h"
#include "../zotnet/tws.h"
#include "../h/fmtcompile.h"
#include <ctype.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

/*  */


#define	NFMTS	MAXARGS
#define QUOTE '\\'

static char  *formats = 0;
extern char *formataddr ();	/* hook for custom address formatting */
#ifdef	LBL
struct msgs *fmt_current_folder; /* current folder (set by main program) */
#endif

static normalize();
static int   get_x400_comp();

extern int fmt_norm;		/* defined in sbr/formatdef.c = AD_NAME */
struct mailname fmt_mnull;


long	time ();

/*  */

/* MAJOR HACK:	See MHCHANGES for discussion */

char  *new_fs (form, format, def)
register char  *form,
               *format,
               *def;
{
    struct stat st;
    register    FILE *fp;

    if (formats)
	free (formats);

    if (form) {
	if ((fp = fopen (libpath (form), "r")) == NULL)
	    adios (form, "unable to open format file");

	if (fstat (fileno (fp), &st) == NOTOK)
	    adios (form, "unable to stat format file");

	if ((formats = malloc ((unsigned) st.st_size + 1)) == NULLCP)
	    adios (form, "unable to allocate space for format");

	if (read (fileno(fp), formats, (int) st.st_size) != st.st_size)
	    adios (form, "error reading format file");

	formats[st.st_size] = '\0';

	(void) fclose (fp);
    }
    else {
	formats = getcpy (format ? format : def);
    }

    normalize (formats);

    return formats;
}

/*  */

static  normalize (cp)
register char  *cp;
{
    register char  *dp;

    for (dp = cp; *cp; cp++)
	if (*cp != QUOTE)
	    *dp++ = *cp;
	else
	    switch (*++cp) {
#define	grot(y,z) case y: *dp++ = z; break;
		grot ('b', '\b');
		grot ('f', '\f');
		grot ('n', '\n');
		grot ('r', '\r');
		grot ('t', '\t');

		case '\n':
		    break;

		case 0: 
		    cp--;	/* fall */
		default: 
		    *dp++ = *cp;
		    break;
	    }

    *dp = 0;
}

/*  */
/*
 * test if string "sub" appears anywhere in string "str"
 * (case insensitive).
 */

static int match (str, sub)
register char  *str,
               *sub;
{
    register int    c1;
    register int    c2;
    register char   *s1;
    register char   *s2;

#ifdef LOCALE
    while (c1 = *sub) {
	c1 = (isalpha(c1) && isupper(c1)) ? tolower(c1) : c1;
	while ((c2 = *str++) && c1 != ((isalpha(c2) && isupper(c2)) ? tolower(c2) : c2))
	    ;
	if (! c2)
	    return 0;
	s1 = sub + 1; s2 = str;
	while ((c1 = *s1++) && ((isalpha(c1) && isupper(c1)) ? tolower(c1) : c1) == ((isalpha(c2 =*s2++) && isupper(c2)) ? tolower(c2) : c2))
	    ;
	if (! c1)
	    return 1;
    }
#else
    while (c1 = *sub) {
	while ((c2 = *str++) && (c1 | 040) != (c2 | 040))
	    ;
	if (! c2)
	    return 0;
	s1 = sub + 1; s2 = str;
	while ((c1 = *s1++) && (c1 | 040) == (*s2++ | 040))
	    ;
	if (! c1)
	    return 1;
    }
#endif
    return 1;
}
/*  */

/* macros to format data */

#define PUTDF(cp, num, wid, fill) if (cp + wid < ep){\
		if((i = (num))<0) i = -(num);\
		if((c = (wid))<0) c = -c;\
		sp = cp + c;\
		do {\
		    *--sp = (i % 10) + '0';\
		    i /= 10;\
		} while (i > 0 && sp > cp);\
		if (i > 0)\
		    *sp = '?';\
		else if ((num) < 0 && sp > cp)\
		    *--sp = '-';\
		while (sp > cp)\
		    *--sp = fill;\
		cp += c;\
		}
#define PUTD(cp, num) if (cp < ep){\
		if((i = (num))==0) *cp++ = '0';\
		else {\
		    if((i = (num))<0) \
			*cp++ = '-', i = -(num);\
		    c = 10;\
		    while (c <= i) \
			c *= 10;\
		    while (cp < ep && c > 1) {\
			c /= 10;\
			*cp++ = (i / c) + '0';\
			i %= c;\
			}\
		    }\
		}
#ifdef LOCALE
#define PUTSF(cp, str, wid, fill) {\
		ljust = 0;\
		if ((i = (wid)) < 0) {\
			i = -i;\
			ljust++;\
		}\
		if (sp = (str)) {\
			if (ljust) {\
				c = strlen(sp);\
				if (c > i)\
					sp += c - i;\
				else {\
					while( --i >= c && cp < ep)\
						*cp++ = fill;\
					i++;\
				}\
			} else {\
			    while ((c = *sp) && (iscntrl(c) || isspace(c)))\
				sp++;\
			}\
			while ((c = *sp++) && --i >= 0 && cp < ep)\
				if (isgraph(c)) \
				    *cp++ = c;\
				else {\
					while ((c = *sp) && (iscntrl(c) || isspace(c)))\
						sp++;\
					    *cp++ = ' ';\
				}\
		}\
		if (!ljust)\
		while( --i >= 0 && cp < ep)\
		    *cp++ = fill;\
	}
#define PUTS(cp, str) {\
		if (sp = (str)) {\
		    while ((c = *sp) && (iscntrl(c) || isspace(c)))\
			sp++;\
		    while((c = *sp++) && cp < ep)\
			if (isgraph(c)) \
			    *cp++ = c;\
			else {\
			    while ((c = *sp) && (iscntrl(c) || isspace(c)))\
				sp++;\
			    *cp++ = ' ';\
			}\
		}\
	}
#else /* LOCALE */
#define PUTSF(cp, str, wid, fill) {\
		ljust = 0;\
		if ((i = (wid)) < 0) {\
			i = -i;\
			ljust++;\
		}\
		if (sp = (str)) {\
			if (ljust) {\
				c = strlen(sp);\
				if (c > i)\
					sp += c - i;\
				else {\
					while( --i >= c && cp < ep)\
						*cp++ = fill;\
					i++;\
				}\
			} else {\
		    while ((c = *sp) && c <= 32)\
			sp++;\
			}\
			while ((c = *sp++) && --i >= 0 && cp < ep)\
				if (c > 32) \
			    *cp++ = c;\
			else {\
					while ((c = *sp) && c <= 32)\
				sp++;\
			    *cp++ = ' ';\
			}\
		}\
		if (!ljust)\
		while( --i >= 0 && cp < ep)\
		    *cp++ = fill;\
		}
#define PUTS(cp, str) {\
		if (sp = (str)) {\
		    while ((c = *sp) && c <= 32)\
			sp++;\
		    while( (c = *sp++) && cp < ep)\
			if ( c > 32 ) \
			    *cp++ = c;\
			else {\
			    while ( (c = *sp) && c <= 32 )\
				sp++;\
			    *cp++ = ' ';\
			}\
		}\
		}
#endif


static char *lmonth[] = { "January",  "February","March",   "April",
			  "May",      "June",    "July",    "August",
			  "September","October", "November","December" };

static char *get_x400_friendly (mbox, buffer)
char   *mbox,
       *buffer;
{
    char    given[BUFSIZ],
	    surname[BUFSIZ];

    if (mbox == NULLCP)
	return NULLCP;
    if (*mbox == '"')
	mbox++;
    if (*mbox != '/')
	return NULLCP;

    if (get_x400_comp (mbox, "/PN=", buffer)) {
	for (mbox = buffer; mbox = index (mbox, '.'); )
	    *mbox++ = ' ';

	return buffer;
    }

    if (!get_x400_comp (mbox, "/S=", surname))
	return NULLCP;

    if (get_x400_comp (mbox, "/G=", given))
	(void) sprintf (buffer, "%s %s", given, surname);
    else
	(void) strcpy (buffer, surname);

    return buffer;
}

static int get_x400_comp (mbox, key, buffer)
char   *mbox,
       *key,
       *buffer;
{
    int	    idx;
    char   *cp;

    if ((idx = stringdex (key, mbox)) < 0
	    || !(cp = index (mbox += idx + strlen (key), '/')))
	return 0;

    (void) sprintf (buffer, "%*.*s", cp - mbox, cp - mbox, mbox);
    return 1;
}

struct format *
fmtscan (format, scanl, width, dat)
	struct format *format;
	char	*scanl;
	int	width;
	int	dat[];
{
    register char	*cp = scanl;
    register char	*ep = scanl + width - 1;
    register struct format *fmt = format;
    register char	*str = NULLCP;
    register int	value = 0;
    register char	*sp;
    register int	i;
    register int	c;
    register struct comp *comp;
    register struct tws *tws;
    register struct mailname *mn;
    int ljust;
    long l;
    char        *savestr;
    char	buffer[BUFSIZ];

    while (cp < ep) {
	switch (fmt->f_type) {

	case FT_COMP:
	    PUTS (cp, fmt->f_comp->c_text);
	    break;
	case FT_COMPF:
	    PUTSF (cp, fmt->f_comp->c_text, fmt->f_width, fmt->f_fill);
	    break;

	case FT_LIT:
	    sp = fmt->f_text;
	    while( (c = *sp++) && cp < ep)
		*cp++ = c;
	    break;
	case FT_LITF:
	    sp = fmt->f_text;
	    ljust = 0;
	    i = fmt->f_width;
	    if (i < 0) {
		i = -i;
		ljust++;		/* XXX should do something with this */
	    }
	    while( (c = *sp++) && --i >= 0 && cp < ep)
		*cp++ = c;
	    while( --i >= 0 && cp < ep)
		*cp++ = fmt->f_fill;
	    break;

	case FT_STR:
	    PUTS (cp, str);
	    break;
	case FT_STRF:
	    PUTSF (cp, str, fmt->f_width, fmt->f_fill);
	    break;
	case FT_STRFW:
	    adios (NULLCP, "internal error (FT_STRFW)");

	case FT_NUM:
	    PUTD (cp, value);
	    break;
	case FT_NUMF:
	    PUTDF (cp, value, fmt->f_width, fmt->f_fill);
	    break;

	case FT_CHAR:
	    *cp++ = fmt->f_char;
	    break;

	case FT_DONE:
	    goto finished;

	case FT_IF_S:
	    if (!(value = (str && *str))) {
		fmt += fmt->f_skip;
		continue;
	    }
	    break;

	case FT_IF_S_NULL:
	    if (!(value = (str == NULLCP || *str == 0))) {
		fmt += fmt->f_skip;
		continue;
	    }
	    break;

	case FT_IF_V_EQ:
	    if (value != fmt->f_value) {
		fmt += fmt->f_skip;
		continue;
	    }
	    break;

	case FT_IF_V_NE:
	    if (value == fmt->f_value) {
		fmt += fmt->f_skip;
		continue;
	    }
	    break;

	case FT_IF_V_GT:
	    if (value <= fmt->f_value) {
		fmt += fmt->f_skip;
		continue;
	    }
	    break;

	case FT_IF_MATCH:
	    if (!(value = (str && match (str, fmt->f_text)))) {
		fmt += fmt->f_skip;
		continue;
	    }
	    break;

	case FT_V_MATCH:
	    if (str)
		value = match (str, fmt->f_text);
	    else
		value = 0;
	    break;

	case FT_IF_AMATCH:
	    if (!(value = (str && uprf (str, fmt->f_text)))) {
		fmt += fmt->f_skip;
		continue;
	    }
	    break;

	case FT_V_AMATCH:
	    value = uprf (str, fmt->f_text);
	    break;

	case FT_S_NONNULL:
	    value = (str != NULLCP && *str != 0);
	    break;

	case FT_S_NULL:
	    value = (str == NULLCP || *str == 0);
	    break;

	case FT_V_EQ:
	    value = (fmt->f_value == value);
	    break;

	case FT_V_NE:
	    value = (fmt->f_value != value);
	    break;

	case FT_V_GT:
	    value = (fmt->f_value > value);
	    break;

	case FT_GOTO:
	    fmt += fmt->f_skip;
	    continue;

	case FT_NOP:
	    break;

	case FT_LS_COMP:
	    str = fmt->f_comp->c_text;
	    break;
	case FT_LS_LIT:
	    str = fmt->f_text;
	    break;
	case FT_LS_GETENV:
	    if (!(str = getenv (fmt->f_text)))
		str = "";
	    break;
	case FT_LS_MFIND:
	    if (!(str = m_find (fmt->f_text)))
		str = "";
	    break;
	case FT_LS_TRIM:
	    if (str) {
		    register char *xp;

		    (void) strcpy(buffer, str);
		    str = buffer;
		    while (isspace(*str))
			    str++;
		    ljust = 0;
		    if ((i = fmt->f_width) < 0) {
			    i = -i;
			    ljust++;
		    }

		    if (!ljust && i > 0 && strlen(str) > i)
			    str[i] = '\0';
		    xp = str;
		    xp += strlen(str) - 1;
		    while (xp > str && isspace(*xp))
			    *xp-- = '\0';
		    if (ljust && i > 0 && strlen(str) > i)
			str += strlen(str) - i;
	    }
	    break;

	case FT_LV_COMPFLAG:
	    value = fmt->f_comp->c_flags;
	    break;
	case FT_LV_COMP:
	    value = (comp = fmt->f_comp)->c_text ? atoi(comp->c_text) : 0;
	    break;
	case FT_LV_LIT:
	    value = fmt->f_value;
	    break;
	case FT_LV_DAT:
	    value = dat[fmt->f_value];
	    break;
	case FT_LV_STRLEN:
	    value = strlen(str);
	    break;
	case FT_LV_CHAR_LEFT:
	    value = width - (cp - scanl);
	    break;
	case FT_LV_PLUS_L:
	    value += fmt->f_value;
	    break;
	case FT_LV_MINUS_L:
	    value = fmt->f_value - value;
	    break;
	case FT_LV_DIVIDE_L:
	    if (fmt->f_value)
		value = value / fmt->f_value;
	    else
		value = 0;
	    break;
	case FT_LV_MODULO_L:
	    if (fmt->f_value)
		value = value % fmt->f_value;
	    else
		value = 0;
	    break;
	case FT_SAVESTR:
	    savestr = str;
	    break;

	case FT_LV_SEC:
	    value = fmt->f_comp->c_tws->tw_sec;
	    break;
	case FT_LV_MIN:
	    value = fmt->f_comp->c_tws->tw_min;
	    break;
	case FT_LV_HOUR:
	    value = fmt->f_comp->c_tws->tw_hour;
	    break;
	case FT_LV_MDAY:
	    value = fmt->f_comp->c_tws->tw_mday;
	    break;
	case FT_LV_MON:
	    value = fmt->f_comp->c_tws->tw_mon + 1;
	    break;
	case FT_LS_MONTH:
	    str = tw_moty[fmt->f_comp->c_tws->tw_mon];
	    break;
	case FT_LS_LMONTH:
	    str = lmonth[fmt->f_comp->c_tws->tw_mon];
	    break;
	case FT_LS_ZONE:
	    str = dtwszone (fmt->f_comp->c_tws);
	    break;
	case FT_LV_YEAR:
#ifndef	YEARMOD
	    value = fmt->f_comp->c_tws->tw_year;
#else	/* YEARMOD */
	    value = (fmt->f_comp->c_tws->tw_year) % 100;
#endif	/* YEARMOD */
	    break;
	case FT_LV_WDAY:
	    if (!(((tws = fmt->f_comp->c_tws)->tw_flags) & (TW_SEXP|TW_SIMP)))
		set_dotw (tws);
	    value = tws->tw_wday;
	    break;
	case FT_LS_DAY:
	    if (!(((tws = fmt->f_comp->c_tws)->tw_flags) & (TW_SEXP|TW_SIMP)))
		set_dotw (tws);
	    str = tw_dotw[tws->tw_wday];
	    break;
	case FT_LS_WEEKDAY:
	    if (!(((tws = fmt->f_comp->c_tws)->tw_flags) & (TW_SEXP|TW_SIMP)))
		set_dotw (tws);
	    str = tw_ldotw[tws->tw_wday];
	    break;
	case FT_LV_YDAY:
	    value = fmt->f_comp->c_tws->tw_yday;
	    break;
	case FT_LV_ZONE:
	    value = fmt->f_comp->c_tws->tw_zone;
	    break;
	case FT_LV_CLOCK:
	    if ((value = fmt->f_comp->c_tws->tw_clock) == 0)
		value = twclock(fmt->f_comp->c_tws);
	    break;
	case FT_LV_RCLOCK:
	    if ((value = fmt->f_comp->c_tws->tw_clock) == 0)
		value = twclock(fmt->f_comp->c_tws);
	    value = time((long *) 0) - value;
	    break;
	case FT_LV_DAYF:
	    if (!(((tws = fmt->f_comp->c_tws)->tw_flags) & (TW_SEXP|TW_SIMP)))
		set_dotw (tws);
	    switch (fmt->f_comp->c_tws->tw_flags & TW_SDAY) {
		case TW_SEXP:
		    value = 1; break;
		case TW_SIMP:
		    value = 0; break;
		default:
		    value = -1; break;
	    }
	case FT_LV_ZONEF:
	    if ((fmt->f_comp->c_tws->tw_flags & TW_SZONE) == TW_SZEXP)
		    value = 1;
	    else
		    value = -1;
	    break;
	case FT_LV_DST:
	    value = fmt->f_comp->c_tws->tw_flags & TW_DST;
	    break;
	case FT_LS_822DATE:
	    str = dasctime ( fmt->f_comp->c_tws , TW_ZONE);
	    break;
	case FT_LS_PRETTY:
	    str = dasctime ( fmt->f_comp->c_tws, TW_NULL);
	    break;

	case FT_LS_PERS:
	    str = fmt->f_comp->c_mn->m_pers;
	    break;
	case FT_LS_MBOX:
	    str = fmt->f_comp->c_mn->m_mbox;
	    break;
	case FT_LS_HOST:
	    str = fmt->f_comp->c_mn->m_host;
	    break;
	case FT_LS_PATH:
	    str = fmt->f_comp->c_mn->m_path;
	    break;
	case FT_LS_GNAME:
	    str = fmt->f_comp->c_mn->m_gname;
	    break;
	case FT_LS_NOTE:
	    str = fmt->f_comp->c_mn->m_note;
	    break;
	case FT_LS_822ADDR:
	    str = adrformat( fmt->f_comp->c_mn );
	    break;
	case FT_LV_HOSTTYPE:
	    value = fmt->f_comp->c_mn->m_type;
	    break;
	case FT_LV_INGRPF:
	    value = fmt->f_comp->c_mn->m_ingrp;
	    break;
	case FT_LV_NOHOSTF:
	    value = fmt->f_comp->c_mn->m_nohost;
	    break;
	case FT_LS_ADDR:
	case FT_LS_FRIENDLY:
#ifdef BERK
	    str = fmt->f_comp->c_mn->m_mbox;
#else	/* not BERK */
	    if ((mn = fmt -> f_comp -> c_mn) == &fmt_mnull) {
		str = fmt -> f_comp -> c_text;
		break;
	    }
	    if (fmt -> f_type == FT_LS_ADDR)
		goto unfriendly;
	    if ((str = mn -> m_pers) == NULL)
	        if ((str = mn -> m_note)) {
	            (void) strcpy (buffer, str);
	            str = buffer;
	            if (*str == '(')
	            	str++;
	            sp = str + strlen(str) - 1;
	            if (*sp == ')') {
	            	*sp-- = '\0';
	        	while (sp >= str)
	            	    if (*sp == ' ')
	            		*sp-- = '\0';
	            	    else
	            		break;
	            }
	        } else if (!(str = get_x400_friendly (mn -> m_mbox, buffer))) {
	unfriendly: ;
		  switch (mn -> m_type) {
		    case LOCALHOST:
			str = mn -> m_mbox;
			break;
		    case UUCPHOST:
			(void) sprintf (buffer, "%s!%s",
					mn -> m_host, mn -> m_mbox);
			str = buffer;
			break;
		    default:
			if (mn -> m_mbox) {
			    (void) sprintf (buffer, "%s@%s",
					    mn -> m_mbox, mn -> m_host);
			    str= buffer;
			}
			else
			    str = mn -> m_text;
			break;
		  }
		}
#endif	/* BERK */
	    break;

	case FT_LOCALDATE:
	    comp = fmt->f_comp;
	    if ((l = comp->c_tws->tw_clock) == 0)
		l = twclock(comp->c_tws);
	    tws = dlocaltime(&l);
	    *comp->c_tws = *tws;
	    break;

	case FT_GMTDATE:
	    comp = fmt->f_comp;
	    if ((l = comp->c_tws->tw_clock) == 0)
		l = twclock(comp->c_tws);
	    tws = dgmtime(&l);
	    *comp->c_tws = *tws;
	    break;

	case FT_PARSEDATE:
	    comp = fmt->f_comp;
	    if ((sp = comp->c_text) && (tws = dparsetime(sp))) {
		*comp->c_tws = *tws;
		comp->c_flags = 0;
	    } else if (comp->c_flags >= 0) {
		bzero ((char *) comp -> c_tws, sizeof *comp -> c_tws);
		comp->c_flags = 1;
	    }
	    break;

	case FT_FORMATADDR:
	    /* hook for custom address list formatting (see replsbr.c) */
	    str = formataddr (savestr, str);
	    break;

	case FT_PUTADDR:
	    /* output the str register as an address component,
	     * splitting it into multiple lines if necessary.  The
	     * value reg. contains the max line length.  The lit.
	     * field may contain a string to prepend to the result
	     * (e.g., "To: ")
	     */
	    {
	    register char *lp = str;
	    register int indent;
	    register int wid = value;
	    register int len = strlen (str);
	    register char *lastb;

	    sp = fmt->f_text;
	    indent = strlen (sp);
	    wid -= indent;
	    while( (c = *sp++) && cp < ep)
		*cp++ = c;
	    while (len > wid) {
		/* try to break at a comma; failing that, break at a
		 * space, failing that, just split the line.
		 */
		lastb = 0; sp = lp + wid;
		while (sp > lp && (c = *--sp) != ',') {
		    if (! lastb && isspace(c))
			lastb = sp - 1;
		}
		if (sp == lp)
		    if (! (sp = lastb))
			sp = lp + wid - 1;
		len -= sp - lp + 1;
		while (cp < ep && lp <= sp)
		    *cp++ = *lp++;
		*cp++ = '\n';
		for (i=indent; cp < ep && i > 0; i--)
		    *cp++ = ' ';
		while (isspace(*lp))
		    lp++, len--;
	    }
	    PUTS (cp, lp);
	    }
	    break;

	case FT_PARSEADDR:
	    comp = fmt->f_comp;
	    if (comp->c_mn != &fmt_mnull)
		mnfree (comp->c_mn);
	    if ((sp = comp->c_text) && (sp = getname(sp)) &&
		(mn = getm (sp, NULLCP, 0, fmt_norm, NULLCP))) {
		comp->c_mn = mn;
		while (getname(""))
		    ;
	    } else {
		while (getname(""))		/* XXX */
		    ;
		comp->c_mn = &fmt_mnull;
	    }
	    break;
	    
	case FT_MYMBOX:
	    /*
	     * if there's no component, we say true.  Otherwise we
	     * say "true" only if we can parse the address and it
	     * matches one of our addresses.
	     */
	    comp = fmt->f_comp;
	    if (comp->c_mn != &fmt_mnull)
		mnfree (comp->c_mn);
	    if ((sp = comp->c_text) && (sp = getname(sp)) &&
		(mn = getm (sp, NULLCP, 0, AD_NAME, NULLCP))) {
		comp->c_mn = mn;
		comp->c_flags = ismymbox(mn);
		while (sp = getname(sp))
		    if (comp->c_flags == 0 &&
			(mn = getm (sp, NULLCP, 0, AD_NAME, NULLCP)))
			comp->c_flags |= ismymbox(mn);
	    } else {
		while (getname(""))		/* XXX */
		    ;
		comp->c_flags = (comp->c_text == 0);
		comp->c_mn = &fmt_mnull;
	    }
	    break;

	case FT_ADDTOSEQ:
#ifdef	LBL
	    /* If we're working on a folder (as opposed to a file), add the
	     * current msg to sequence given in literal field.  Don't
	     * disturb string or value registers.
	     */
	    if (fmt_current_folder)
		    (void)m_seqadd(fmt_current_folder, fmt->f_text, dat[0], -1);
#endif
	    break;
	}
	fmt++;
    }
#ifndef JLR
    finished:;
    if (cp[-1] != '\n')
	*cp++ = '\n';
    *cp   = 0;
    return ((struct format *)0);
#else	/* JLR */
    if (cp[-1] != '\n')
	*cp++ = '\n';
    while (fmt->f_type != FT_DONE)
	fmt++;

    finished:;    
    *cp = '\0';
    return (fmt -> f_value ? ++fmt : (struct format *)0);

#endif /* JLR */
}
