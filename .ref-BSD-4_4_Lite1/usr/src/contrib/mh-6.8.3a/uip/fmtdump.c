/* fmtdump.c - compile format file and dump out instructions */
#ifndef lint
static char ident[] = "@(#)$Id: fmtdump.c,v 1.10 1992/11/04 00:42:31 jromine Exp $";
#endif

#include "../h/mh.h"
#include "../h/formatsbr.h"
#include "../h/fmtcompile.h"
#include "../h/scansbr.h"
#include <stdio.h>
#ifdef LOCALE
#include	<locale.h>
#endif


static struct swit switches[] = {
#define	FORMSW	0
    "form formatfile", 0,
#define	FMTSW	1
    "format string", 5,

#define	HELPSW	2
    "help", 4,

    NULL, 0
};


/* for assignlabel */
static 	struct format *lvec[128];
static 	lused = 0;

static	char *f_typestr(), *c_typestr();
static	void  fmt_dump(), dumpone(), assignlabel(), litputs(), litputc();
static 	int   findlabel();

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char   *argv[];
{
    int    ncomps;
    char   *cp,
	   *form = NULL,
	   *format = NULL,
            buf[100],
          **ap,
          **argp,
           *nfs,
           *arguments[MAXARGS];
    struct format *fmt;

#ifdef LOCALE
    setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

/*  */

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown", cp);
		case HELPSW: 
		    (void) sprintf (buf, "%s [switches]",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case FORMSW: 
		    if (!(form = *argp++) || *form == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    format = NULL;
		    continue;
		case FMTSW: 
		    if (!(format = *argp++) || *format == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    form = NULL;
		    continue;

	    }
	if (form)
	    adios (NULLCP, "only one form at a time!");
	else
	    form = cp;
    }

    nfs = new_fs (form, format, FORMAT);	/* must be before chdir() */
    ncomps = fmt_compile(nfs, &fmt);

    fmt_dump(fmt);
    done(0);
}

static void
fmt_dump (fmth)
register struct format *fmth;
{
	int i;
	register struct format *fmt, *addr;

	/* Assign labels */
	for (fmt = fmth; fmt; ++fmt) {
		i = fmt->f_type;
		if (i == FT_IF_S ||
		    i == FT_IF_S_NULL ||
		    i == FT_IF_V_EQ ||
		    i == FT_IF_V_NE ||
		    i == FT_IF_V_GT ||
		    i == FT_IF_MATCH ||
		    i == FT_IF_AMATCH ||
		    i == FT_GOTO) {
			addr = fmt + fmt->f_skip;
			if (findlabel(addr) < 0)
				assignlabel(addr);
		}
		if (fmt->f_type == FT_DONE && fmt->f_value == 0)
			break;
	}

	/* Dump them out! */
	for (fmt = fmth; fmt; ++fmt) {
		dumpone(fmt);
		if (fmt->f_type == FT_DONE && fmt->f_value == 0)
			break;
	}
}

static void
dumpone(fmt)
	register struct format *fmt;
{
	register int i;

	if ((i = findlabel(fmt)) >= 0)
		printf("L%d:", i);
	putchar('\t');

	fputs(f_typestr((int)fmt->f_type), stdout);

	switch (fmt->f_type) {

	case FT_COMP:
	case FT_LS_COMP:
	case FT_LV_COMPFLAG:
	case FT_LV_COMP:
		printf(", comp ");
		litputs(fmt->f_comp->c_name);
		if (fmt->f_comp->c_type)
			printf(", c_type %s", c_typestr(fmt->f_comp->c_type));
		if (fmt->f_comp->c_flags)
			printf(", c_flags %d", fmt->f_comp->c_flags);
		break;

	case FT_LV_SEC:
	case FT_LV_MIN:
	case FT_LV_HOUR:
	case FT_LV_MDAY:
	case FT_LV_MON:
	case FT_LS_MONTH:
	case FT_LS_LMONTH:
	case FT_LS_ZONE:
	case FT_LV_YEAR:
	case FT_LV_WDAY:
	case FT_LS_DAY:
	case FT_LS_WEEKDAY:
	case FT_LV_YDAY:
	case FT_LV_ZONE:
	case FT_LV_CLOCK:
	case FT_LV_RCLOCK:
	case FT_LV_DAYF:
	case FT_LV_ZONEF:
	case FT_LV_DST:
	case FT_LS_822DATE:
	case FT_LS_PRETTY:
	case FT_LOCALDATE:
	case FT_GMTDATE:
	case FT_PARSEDATE:
		printf(", c_name ");
		litputs(fmt->f_comp->c_name);
		if (fmt->f_comp->c_type)
			printf(", c_type %s", c_typestr(fmt->f_comp->c_type));
		if (fmt->f_comp->c_flags)
			printf(", c_flags %d", fmt->f_comp->c_flags);
		break;

	case FT_LS_ADDR:
	case FT_LS_PERS:
	case FT_LS_MBOX:
	case FT_LS_HOST:
	case FT_LS_PATH:
	case FT_LS_GNAME:
	case FT_LS_NOTE:
	case FT_LS_822ADDR:
	case FT_LV_HOSTTYPE:
	case FT_LV_INGRPF:
	case FT_LV_NOHOSTF:
	case FT_LS_FRIENDLY:
	case FT_PARSEADDR:
	case FT_MYMBOX:
		printf(", c_name ");
		litputs(fmt->f_comp->c_name);
		if (fmt->f_comp->c_type)
			printf(", c_type %s", c_typestr(fmt->f_comp->c_type));
		if (fmt->f_comp->c_flags)
			printf(", c_flags %d", fmt->f_comp->c_flags);
		break;

	case FT_COMPF:
		printf(", width %d, fill '", fmt->f_width);
		litputc(fmt->f_fill);
		printf("' name ");
		litputs(fmt->f_comp->c_name);
		if (fmt->f_comp->c_type)
			printf(", c_type %s", c_typestr(fmt->f_comp->c_type));
		if (fmt->f_comp->c_flags)
			printf(", c_flags %d", fmt->f_comp->c_flags);
		break;

	case FT_STRF:
	case FT_NUMF:
		printf(", width %d, fill '", fmt->f_width);
		litputc(fmt->f_fill);
		putchar('\'');
		break;

	case FT_LIT:
#ifdef FT_LIT_FORCE
	case FT_LIT_FORCE:
#endif
		putchar(' ');
		litputs(fmt->f_text);
		break;

	case FT_LITF:
		printf(", width %d, fill '", fmt->f_width);
		litputc(fmt->f_fill);
		printf("' ");
		litputs(fmt->f_text);
		break;

	case FT_CHAR:
		putchar(' ');
		putchar('\'');
		litputc(fmt->f_char);
		putchar('\'');
		break;


	case FT_IF_S:
	case FT_IF_S_NULL:
	case FT_IF_MATCH:
	case FT_IF_AMATCH:
		printf(" continue else goto");
	case FT_GOTO:
		i = findlabel(fmt + fmt->f_skip);
		printf(" L%d", i);
		break;

	case FT_IF_V_EQ:
	case FT_IF_V_NE:
	case FT_IF_V_GT:
		i = findlabel(fmt + fmt->f_skip);
		printf(" %d continue else goto L%d", fmt->f_value, i);
		break;

	case FT_V_EQ:
	case FT_V_NE:
	case FT_V_GT:
	case FT_LV_LIT:
	case FT_LV_PLUS_L:
	case FT_LV_MINUS_L:
	case FT_LV_DIVIDE_L:
	case FT_LV_MODULO_L:
		printf(" value %d", fmt->f_value);
		break;

	case FT_LS_LIT:
		printf(" str ");
		litputs(fmt->f_text);
		break;

	case FT_LS_GETENV:
		printf(" getenv ");
		litputs(fmt->f_text);
		break;

	case FT_LS_TRIM:
		printf(", width %d", fmt->f_width);
		break;

	case FT_LV_DAT:
		printf(", value dat[%d]", fmt->f_value);
		break;
	}
	putchar('\n');
}

static int
findlabel(addr)
	register struct format *addr;
{
	register int i;

	for (i = 0; i < lused; ++i)
		if (addr == lvec[i])
			return(i);
	return(-1);
}

static void
assignlabel(addr)
	register struct format *addr;
{
	lvec[lused++] = addr;
}

static char *
f_typestr(t)
	int t;
{
	static char buf[32];

	switch (t) {
	case FT_COMP: return("COMP");
	case FT_COMPF: return("COMPF");
	case FT_LIT: return("LIT");
	case FT_LITF: return("LITF");
#ifdef	FT_LIT_FORCE
	case FT_LIT_FORCE: return("LIT_FORCE");
#endif
	case FT_CHAR: return("CHAR");
	case FT_NUM: return("NUM");
	case FT_NUMF: return("NUMF");
	case FT_STR: return("STR");
	case FT_STRF: return("STRF");
	case FT_STRFW: return("STRFW");
	case FT_PUTADDR: return("PUTADDR");
	case FT_LS_COMP: return("LS_COMP");
	case FT_LS_LIT: return("LS_LIT");
	case FT_LS_GETENV: return("LS_GETENV");
	case FT_LS_TRIM: return("LS_TRIM");
	case FT_LV_COMP: return("LV_COMP");
	case FT_LV_COMPFLAG: return("LV_COMPFLAG");
	case FT_LV_LIT: return("LV_LIT");
	case FT_LV_DAT: return("LV_DAT");
	case FT_LV_STRLEN: return("LV_STRLEN");
	case FT_LV_PLUS_L: return("LV_PLUS_L");
	case FT_LV_MINUS_L: return("LV_MINUS_L");
	case FT_LV_DIVIDE_L: return("LV_DIVIDE_L");
	case FT_LV_MODULO_L: return("LV_MODULO_L");
	case FT_LV_CHAR_LEFT: return("LV_CHAR_LEFT");
	case FT_LS_MONTH: return("LS_MONTH");
	case FT_LS_LMONTH: return("LS_LMONTH");
	case FT_LS_ZONE: return("LS_ZONE");
	case FT_LS_DAY: return("LS_DAY");
	case FT_LS_WEEKDAY: return("LS_WEEKDAY");
	case FT_LS_822DATE: return("LS_822DATE");
	case FT_LS_PRETTY: return("LS_PRETTY");
	case FT_LV_SEC: return("LV_SEC");
	case FT_LV_MIN: return("LV_MIN");
	case FT_LV_HOUR: return("LV_HOUR");
	case FT_LV_MDAY: return("LV_MDAY");
	case FT_LV_MON: return("LV_MON");
	case FT_LV_YEAR: return("LV_YEAR");
	case FT_LV_YDAY: return("LV_YDAY");
	case FT_LV_WDAY: return("LV_WDAY");
	case FT_LV_ZONE: return("LV_ZONE");
	case FT_LV_CLOCK: return("LV_CLOCK");
	case FT_LV_RCLOCK: return("LV_RCLOCK");
	case FT_LV_DAYF: return("LV_DAYF");
	case FT_LV_DST: return("LV_DST");
	case FT_LV_ZONEF: return("LV_ZONEF");
	case FT_LS_ADDR: return("LS_ADDR");
	case FT_LS_PERS: return("LS_PERS");
	case FT_LS_MBOX: return("LS_MBOX");
	case FT_LS_HOST: return("LS_HOST");
	case FT_LS_PATH: return("LS_PATH");
	case FT_LS_GNAME: return("LS_GNAME");
	case FT_LS_NOTE: return("LS_NOTE");
	case FT_LS_822ADDR: return("LS_822ADDR");
	case FT_LS_FRIENDLY: return("LS_FRIENDLY");
	case FT_LV_HOSTTYPE: return("LV_HOSTTYPE");
	case FT_LV_INGRPF: return("LV_INGRPF");
	case FT_LV_NOHOSTF: return("LV_NOHOSTF");
	case FT_LOCALDATE: return("LOCALDATE");
	case FT_GMTDATE: return("GMTDATE");
	case FT_PARSEDATE: return("PARSEDATE");
	case FT_PARSEADDR: return("PARSEADDR");
	case FT_FORMATADDR: return("FORMATADDR");
	case FT_MYMBOX: return("MYMBOX");
#ifdef	FT_ADDTOSEQ
	case FT_ADDTOSEQ: return("ADDTOSEQ");
#endif
	case FT_SAVESTR: return("SAVESTR");
#ifdef	FT_PAUSE
	case FT_PAUSE: return ("PAUSE");
#endif
	case FT_DONE: return("DONE");
	case FT_NOP: return("NOP");
	case FT_GOTO: return("GOTO");
	case FT_IF_S_NULL: return("IF_S_NULL");
	case FT_IF_S: return("IF_S");
	case FT_IF_V_EQ: return("IF_V_EQ");
	case FT_IF_V_NE: return("IF_V_NE");
	case FT_IF_V_GT: return("IF_V_GT");
	case FT_IF_MATCH: return("IF_MATCH");
	case FT_IF_AMATCH: return("IF_AMATCH");
	case FT_S_NULL: return("S_NULL");
	case FT_S_NONNULL: return("S_NONNULL");
	case FT_V_EQ: return("V_EQ");
	case FT_V_NE: return("V_NE");
	case FT_V_GT: return("V_GT");
	case FT_V_MATCH: return("V_MATCH");
	case FT_V_AMATCH: return("V_AMATCH");
	default:
		(void)sprintf(buf, "/* ??? #%d */", t);
		return(buf);
	}
}

#define FNORD(v, s) if (t & (v)) { \
	if (i++ > 0) \
		strcat(buf, "|"); \
	strcat(buf, s); }

static char *
c_typestr(t)
	int t;
{
	register int i;
	static char buf[64];

	buf[0] = '\0';
	if (t & ~(CT_ADDR|CT_DATE|CT_MYMBOX|CT_ADDRPARSE))
		(void)sprintf(buf, "0x%x ", t);
	strcat(buf, "<");
	i = 0;
	FNORD(CT_ADDR, "ADDR");
	FNORD(CT_DATE, "DATE");
	FNORD(CT_MYMBOX, "MYMBOX");
	FNORD(CT_ADDRPARSE, "ADDRPARSE");
	strcat(buf, ">");
	return(buf);
#undef FNORD
}

static void
litputs(s)
	register char *s;
{
	if (s) {
		putc('"', stdout);
		while (*s)
			litputc(*s++);
		putc('"', stdout);
	} else
		fputs("<nil>", stdout);
}

static void
litputc(c)
	char c;
{
	if (c & ~ 0177) {
		putc('M', stdout);
		putc('-', stdout);
		c &= 0177;
	}
	if (c < 0x20 || c == 0177) {
		if (c == '\b') {
			putc('\\', stdout);
			putc('b', stdout);
		} else if (c == '\f') {
			putc('\\', stdout);
			putc('f', stdout);
		} else if (c == '\n') {
			putc('\\', stdout);
			putc('n', stdout);
		} else if (c == '\r') {
			putc('\\', stdout);
			putc('r', stdout);
		} else if (c == '\t') {
			putc('\\', stdout);
			putc('t', stdout);
		} else {
			putc('^', stdout);
			putc(c ^ 0x40, stdout);	/* DEL to ?, others to alpha */
		}
	} else
		putc(c, stdout);
}
