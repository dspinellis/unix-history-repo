/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "fp.h"
#include "termcap.h"
#include "ctype.h"
#include "disp.h"

#ifdef	MAC
# include  "mac.h"
#else
# ifdef	STDARGS
#  include <stdarg.h>
# else
#  include <varargs.h>
# endif
#endif

private void
	doformat proto((File *, const char *, va_list)),
	outld proto((long, int)),
	pad proto((int, int));

char	mesgbuf[MESG_SIZE];

void
format(buf, len, fmt, ap)
char	*buf;
const char	*fmt;
size_t	len;
va_list	ap;
{
	File	strbuf;

	strbuf.f_ptr = strbuf.f_base = buf;
	strbuf.f_fd = -1;		/* Not legit for files */
	strbuf.f_bufsize = strbuf.f_cnt = len;
	strbuf.f_flags = F_STRING;

	doformat(&strbuf, fmt, ap);
	jputc('\0', &strbuf);	/* jputc will place this, even if overflow */
}

#ifdef	IBMPC
int	specialmap = 0,	/* ??? never used? -- DHR */
	specialkey = 0;

#define Empty ""

const char *const altseq[133] = {
Empty, Empty, Empty, "Ctrl-@", Empty, Empty, Empty, Empty,
Empty, Empty, Empty, Empty, Empty, Empty, Empty, "Left",
"Alt-Q", "Alt-W", "Alt-E", "Alt-R", "Alt-T", "Alt-Y", "Alt-U", "Alt-I",
"Alt-O", "Alt-P", Empty, Empty, Empty, Empty, "Alt-A", "Alt-S",
"Alt-D", "Alt-F", "Alt-G", "Alt-H", "Alt-J", "Alt-K", "Alt-L", Empty,
Empty, Empty, Empty, Empty, "Alt-Z", "Alt-X", "Alt-C", "Alt-V",
"Alt-B", "Alt-N", "Alt-M", Empty, Empty, Empty, Empty, Empty,
Empty, Empty, Empty, "F1", "F2", "F3", "F4", "F5",
"F6", "F7", "F8", "F9", "F10", Empty, Empty, "Home",
"Up", "PageUp", Empty, "Left", Empty, "Right", Empty, "End",
"Down", "PageDown", "Ins", "Del", "Shift F1", "Shift F2", "Shift F3", "Shift F4",
"Shift F5", "Shift F6", "Shift F7", "Shift F8", "Shift F9", "Shift F10", "Ctrl F1", "Ctrl F2",
"Ctrl F3", "Ctrl F4", "Ctrl F5", "Ctrl F6", "Ctrl F7", "Ctrl F8", "Ctrl F9", "Ctrl F10",
"Alt F1", "Alt F2", "Alt F3", "Alt F4", "Alt F5", "Alt F6", "Alt F7", "Alt F8",
"Alt F9", "Alt F10", "Ctrl PrtSc", "Ctrl Left", "Ctrl Right", "Ctrl End", "Ctrl PageDown", "Ctrl Home",
"Alt 1", "Alt 2", "Alt 3", "Alt 4", "Alt 5", "Alt 6", "Alt 7", "Alt 8",
"Alt 9", "Alt 0", "Alt Minus", "Alt Equals", "Ctrl PageUp"
};
#endif


private void
PPchar(c, str, size)
int	c;
char	*str;
size_t	size;
{
	char	*cp = str;

#ifdef	IBMPC
	if (specialmap || specialkey) {
		if (c < 0 || c > 132)
			c = 0;
		strcpy(cp, altseq[c]);
	} else if (c == '\377') {
		*cp = '\0';	/* this character was invisible */
	} else
#endif
	if (c == '\033') {
		strcpy(cp, "ESC");
	} else if (c < ' ') {
		swritef(cp, size, "C-%c", c + '@');
	} else if (c == '\177') {
		strcpy(cp, "^?");
	} else {
		swritef(cp, size, "%c", c);
	}
}

private struct fmt_state {
	int	precision,
		width,
		leftadj;
	char	padc;
	File	*iop;
} current_fmt;

private void
putld(d, base)
long	d;
int	base;
{
	int	len = 1;
	long	tmpd = d;

	if (current_fmt.width == 0 && current_fmt.precision) {
		current_fmt.width = current_fmt.precision;
		current_fmt.padc = '0';
	}
	while ((tmpd = (tmpd / base)) != 0)
		len += 1;
	if (d < 0)
		len += 1;
	if (!current_fmt.leftadj)
		pad(current_fmt.padc, current_fmt.width - len);
	if (d < 0) {
		jputc('-', current_fmt.iop);
		d = -d;
	}
	outld(d, base);
	if (current_fmt.leftadj)
		pad(current_fmt.padc, current_fmt.width - len);
}

private void
outld(d, base)
long	d;
int	base;
{
	register long	n;
	static const char	chars[] = {'0', '1', '2', '3', '4', '5', '6',
				    '7', '8', '9', 'a', 'b', 'c', 'd',
				    'e', 'f'};

	if ((n = (d / base)) != 0)
		outld(n, base);
	jputc((int) (chars[(int) (d % base)]), current_fmt.iop);
}

private void
jputs(str)
char	*str;
{
	int	len;
	register char	*cp;

	if (str == NULL)
		str = "(null)";
	len = strlen(str);
	if (current_fmt.precision == 0 || len < current_fmt.precision)
		current_fmt.precision = len;
	else
		len = current_fmt.precision;
	cp = str;
	if (!current_fmt.leftadj)
		pad(' ', current_fmt.width - len);
	while (--current_fmt.precision >= 0)
		jputc(*cp++, current_fmt.iop);
	if (current_fmt.leftadj)
		pad(' ', current_fmt.width - len);
}

private void
pad(c, amount)
register int	c,
		amount;
{
	while (--amount >= 0)
		jputc(c, current_fmt.iop);
}

private void
doformat(sp, fmt, ap)
register File	*sp;
register const char	*fmt;
va_list	ap;
{
	register char	c;
	struct fmt_state	prev_fmt;

	prev_fmt = current_fmt;
	current_fmt.iop = sp;

	while ((c = *fmt++) != '\0') {
		if (c != '%') {
			jputc(c, current_fmt.iop);
			continue;
		}

		current_fmt.padc = ' ';
		current_fmt.precision = current_fmt.leftadj = current_fmt.width = 0;
		c = *fmt++;
		if (c == '-') {
			current_fmt.leftadj = YES;
			c = *fmt++;
		}
		if (c == '0') {
			current_fmt.padc = '0';
			c = *fmt++;
		}
		while (c >= '0' && c <= '9') {
			current_fmt.width = current_fmt.width * 10 + (c - '0');
			c = *fmt++;
		}
		if (c == '*') {
			current_fmt.width = va_arg(ap, int);
			c = *fmt++;
		}
		if (c == '.') {
			c = *fmt++;
			while (c >= '0' && c <= '9') {
				current_fmt.precision = current_fmt.precision * 10 + (c - '0');
				c = *fmt++;
			}
			if (c == '*') {
				current_fmt.precision = va_arg(ap, int);
				c = *fmt++;
			}
		}
	reswitch:
		/* At this point, fmt points at one past the format letter. */
		switch (c) {
		case '%':
			jputc('%', current_fmt.iop);
			break;

		case 'O':
		case 'D':
		case 'X':
			putld(va_arg(ap, long), (c == 'O') ? 8 :
						(c == 'D') ? 10 : 16);
			break;

		case 'b':
		    {
			Buffer	*b = va_arg(ap, Buffer *);

			jputs(b->b_name);
			break;
		    }

		case 'c':
			jputc(va_arg(ap, int), current_fmt.iop);
			break;

		case 'o':
		case 'd':
		case 'x':
			putld((long) va_arg(ap, int), (c == 'o') ? 8 :
						(c == 'd') ? 10 : 16);
			break;

		case 'f':	/* current command name gets inserted here! */
			jputs(LastCmd->Name);
			break;

		case 'l':
			c = CharUpcase(*++fmt);
			goto reswitch;

		case 'n':
			if (va_arg(ap, int) != 1)
				jputs("s");
			break;

		case 'p':
		    {
			char	cbuf[20];

			PPchar(va_arg(ap, int), cbuf, sizeof(cbuf));
			jputs(cbuf);
			break;
		    }

		case 's':
			jputs(va_arg(ap, char *));
			break;

		default:
			complain("Unknown format directive: \"%%%c\"", c);
		}
	}
	current_fmt = prev_fmt;
}

#ifdef	STDARGS
char *
sprint(const char *fmt, ...)
#else
/*VARARGS1*/ char *
sprint(fmt, va_alist)
	const char	*fmt;
	va_dcl
#endif
{
	va_list	ap;
	static char	line[100];

	va_init(ap, fmt);
	format(line, sizeof line, fmt, ap);
	va_end(ap);
	return line;
}

#ifdef	STDARGS
void
writef(const char *fmt, ...)
#else
/*VARARGS1*/ void
writef(fmt, va_alist)
	const char	*fmt;
	va_dcl
#endif
{
	va_list	ap;

	va_init(ap, fmt);
#ifndef	IBMPC
	doformat(stdout, fmt, ap);
#else	/* IBMPC */
	write_em(sprint(fmt, ap));
	/* doformat(stdout, fmt, ap); */
#endif	/* IBMPC */
	va_end(ap);
}

#ifdef	STDARGS
void
fwritef(File *fp, const char *fmt, ...)
#else
/*VARARGS2*/ void
fwritef(fp, fmt, va_alist)
	File	*fp;
	const char	*fmt;
	va_dcl
#endif
{
	va_list	ap;

	va_init(ap, fmt);
	doformat(fp, fmt, ap);
	va_end(ap);
}

#ifdef	STDARGS
void
swritef(char *str, size_t size, const char *fmt, ...)
#else
/*VARARGS3*/ void
swritef(str, size, fmt, va_alist)
	char	*str;
	size_t	size;
	const char	*fmt;
	va_dcl
#endif
{
	va_list	ap;

	va_init(ap, fmt);
	format(str, size, fmt, ap);
	va_end(ap);
}

#ifdef	STDARGS
void
s_mess(const char *fmt, ...)
#else
/*VARARGS1*/ void
s_mess(fmt, va_alist)
	const char	*fmt;
	va_dcl
#endif
{
	va_list	ap;

	if (InJoverc)
		return;
	va_init(ap, fmt);
	format(mesgbuf, sizeof mesgbuf, fmt, ap);
	va_end(ap);
	message(mesgbuf);
}

#ifdef	STDARGS
void
f_mess(const char *fmt, ...)
#else
/*VARARGS1*/ void
f_mess(fmt, va_alist)
	const char	*fmt;
	va_dcl
#endif
{
	va_list	ap;

	va_init(ap, fmt);
	format(mesgbuf, sizeof mesgbuf, fmt, ap);
	va_end(ap);
	DrawMesg(NO);
	errormsg = NO;
	UpdMesg = YES;	/* still needs updating (for convenience) */
}

#ifdef	STDARGS
void
add_mess(const char *fmt, ...)
#else
/*VARARGS1*/ void
add_mess(fmt, va_alist)
	const char	*fmt;
	va_dcl
#endif
{
	int	mesg_len = strlen(mesgbuf);
	va_list	ap;

	if (InJoverc)
		return;
	va_init(ap, fmt);
	format(&mesgbuf[mesg_len], (sizeof mesgbuf) - mesg_len, fmt, ap);
	va_end(ap);
	message(mesgbuf);
}
