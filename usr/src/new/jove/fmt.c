/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "io.h"
#include "termcap.h"

#ifdef MAC
#	include  "mac.h"
#else
#	include <varargs.h>
#endif

#ifdef MAC
#	undef private
#	define private
#endif

#ifdef	LINT_ARGS
private void
	doformat(File *, char *, ...),
	outld(long, int),
	pad(int, int),
	PPchar(int, char *),
	putld(long, int),
	puts(char *);
#else
private void
	doformat(),
	outld(),
	pad(),
	PPchar(),
	putld(),
	puts();
#endif	/* LINT_ARGS */

#ifdef MAC
#	undef private
#	define private static
#endif


char	mesgbuf[MESG_SIZE];

/* VARARGS2 */

void
format(buf, len, fmt, ap)
char	*buf,
	*fmt;
va_list	ap;
{
	File	strbuf,
		*sp = &strbuf;

 	sp->f_ptr = sp->f_base = buf;
	sp->f_fd = -1;		/* Not legit for files */
	sp->f_cnt = len;
	sp->f_flags = F_STRING;
	sp->f_bufsize = len;

	doformat(sp, fmt, ap);
	putc('\0', sp);
}

#ifdef IBMPC
int	specialmap = 0,
	specialkey = 0;

#define Empty ""

char *altseq[133] = {
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
PPchar(c, str)
int	c;
char	*str;
{
	char	*cp = str;

#ifdef IBMPC
	if (specialmap || specialkey) {
		if (c < 0 || c > 132)
			c = 0;
		strcpy(cp, altseq[c]);
	} else
#endif	
	if (c == '\033')
		strcpy(cp, "ESC");
#ifdef IBMPC				/* this character is invisible */
	else if (c == '\377') {
			*cp = 0;
	}
#endif /* IBMPC */
	else if (c < ' ')
		sprintf(cp, "C-%c", c + '@');
	else if (c == '\177')
		strcpy(cp, "^?");
	else
		sprintf(cp, "%c", c);
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
{
	int	length = 1;
	long	tmpd = d;

	if (current_fmt.width == 0 && current_fmt.precision) {
		current_fmt.width = current_fmt.precision;
		current_fmt.padc = '0';
	}
	while (tmpd = (tmpd / base))
		length += 1;
	if (d < 0)
		length += 1;
	if (!current_fmt.leftadj)
		pad(current_fmt.padc, current_fmt.width - length);
	if (d < 0) {
		putc('-', current_fmt.iop);
		d = -d;
	}
	outld(d, base);
	if (current_fmt.leftadj)
		pad(current_fmt.padc, current_fmt.width - length);
}

private void
outld(d, base)
long	d;
{
	register long	n;
	static char	chars[] = {'0', '1', '2', '3', '4', '5', '6',
				    '7', '8', '9', 'a', 'b', 'c', 'd',
				    'e', 'f'};

	if (n = (d / base))
		outld(n, base);
	putc((int) (chars[(int) (d % base)]), current_fmt.iop);
}

private void
puts(str)
char	*str;
{
	int	length;
	register char	*cp;

	if (str == 0)
#if pyr
		str = "";
#else
		str = "(null)";
#endif
	length = strlen(str);
	if (current_fmt.precision == 0 || length < current_fmt.precision)
		current_fmt.precision = length;
	else
		length = current_fmt.precision;
	cp = str;
	if (!current_fmt.leftadj)
		pad(' ', current_fmt.width - length);
	while (--current_fmt.precision >= 0)
		putc(*cp++, current_fmt.iop);
	if (current_fmt.leftadj)
		pad(' ', current_fmt.width - length);
}

private void
pad(c, amount)
register int	c,
		amount;
{
	while (--amount >= 0)
		putc(c, current_fmt.iop);
}

private void
doformat(sp, fmt, ap)
register File	*sp;
register char	*fmt;
va_list	ap;
{
	register char	c;
	struct fmt_state	prev_fmt;

	prev_fmt = current_fmt;
	current_fmt.iop = sp;

	while (c = *fmt++) {
		if (c != '%') {
			putc(c, current_fmt.iop);
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
			putc('%', current_fmt.iop);
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

			puts(b->b_name);
			break;
		    }

		case 'c':
			putc(va_arg(ap, int), current_fmt.iop);
			break;
	
		case 'o':
		case 'd':
		case 'x':
			putld((long) va_arg(ap, int), (c == 'o') ? 8 :
						(c == 'd') ? 10 : 16);
			break;
	
		case 'f':	/* current command name gets inserted here! */
			puts(LastCmd->Name);
			break;

		case 'l':
			c = CharUpcase(*++fmt);
			goto reswitch;
	
		case 'n':
			if (va_arg(ap, int) != 1)
				puts("s");
			break;

		case 'p':
		    {
		    	char	cbuf[20];

		    	PPchar(va_arg(ap, int), cbuf);
		    	puts(cbuf);
		    	break;
		    }

		case 's':
			puts(va_arg(ap, char *));
			break;
		
		default:
			complain("Unknown format directive: \"%%%c\"", c);
		}
	}
	current_fmt = prev_fmt;
}

/* VARARGS1 */

char *
sprint(fmt, va_alist)
char	*fmt;
va_dcl
{
	va_list	ap;
	static char	line[100];

	va_start(ap);
	format(line, sizeof line, fmt, ap);
	va_end(ap);
	return line;
}

/* VARARGS1 */

void
printf(fmt, va_alist)
char	*fmt;
va_dcl
{
	va_list	ap;

	va_start(ap);
#ifndef IBMPC
	doformat(stdout, fmt, ap);
#else /* IBMPC */
	write_em(sprint(fmt, ap));
	/* doformat(stdout, fmt, ap); */
#endif /* IBMPC */
	va_end(ap);
}

/* VARARGS1 */

void
fprintf(fp, fmt, va_alist)
File	*fp;
char	*fmt;
va_dcl
{
	va_list	ap;

	va_start(ap);
	doformat(fp, fmt, ap);
	va_end(ap);
}

/* VARARGS2 */

void
sprintf(str, fmt, va_alist)
char	*str,
	*fmt;
va_dcl
{
	va_list	ap;

	va_start(ap);
	format(str, 130, fmt, ap);
	va_end(ap);
}

/* VARARGS1 */

void
s_mess(fmt, va_alist)
char	*fmt;
va_dcl
{
	va_list	ap;

	if (InJoverc)
		return;
	va_start(ap);
	format(mesgbuf, sizeof mesgbuf, fmt, ap);
	va_end(ap);
	message(mesgbuf);
}

/* VARARGS1 */

void
f_mess(fmt, va_alist)
char	*fmt;
va_dcl
{
	va_list	ap;

	va_start(ap);
	format(mesgbuf, sizeof mesgbuf, fmt, ap);
	va_end(ap);
	DrawMesg(NO);
	UpdMesg = YES;	/* still needs updating (for convenience) */
}

/* VARARGS1 */

void
add_mess(fmt, va_alist)
char	*fmt;
va_dcl
{
	int	mesg_len = strlen(mesgbuf);
	va_list	ap;

	if (InJoverc)
		return;
	va_start(ap);
	format(&mesgbuf[mesg_len], (sizeof mesgbuf) - mesg_len, fmt, ap);
	va_end(ap);
	message(mesgbuf);
}
