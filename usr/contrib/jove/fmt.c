/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"
#include "io.h"
#include "termcap.h"

#include <varargs.h>

char	mesgbuf[MESG_SIZE];

/* VARARGS2 */

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

static char	padc = ' ';
static File	*curiop = 0;

static
PPchar(c, str)
int	c;
char	*str;
{
	char	*cp = str;

	if (c == '\033')
		strcpy(cp, "ESC");
	else if (c < ' ')
		sprintf(cp, "C-%c", c + '@');
	else if (c == '\177')
		strcpy(cp, "^?");
	else
		sprintf(cp, "%c", c);
}

static
putld(leftadj, width, d, base)
long	d;
{
	int	length = 1;
	long	tmpd = d;

	while (tmpd = (tmpd / base))
		length++;
	if (d < 0)
		length++;
	if (!leftadj)
		pad(padc, width - length);
	if (d < 0) {
		putc('-', curiop);
		d = -d;
	}
	outld(d, base);
	if (leftadj)
		pad(padc, width - length);
}

static
outld(d, base)
long	d;
{
	long	n;

	if (n = (d / base))
		outld(n, base);
	putc((int) ('0' + (int) (d % base)), curiop);
}

static
puts(leftadj, width, str)
char	*str;
{
	int	length;
	register char	*cp,
			c;

	if (str == 0)
#if pyr
		str = "";
#else
		str = "(null)";
#endif
	length = strlen(str);
	cp = str;
	if (!leftadj)
		pad(' ', width - length);
	while (c = *cp++)
		putc(c, curiop);
	if (leftadj)
		pad(' ', width - length);
}

static
pad(c, amount)
register int	c,
		amount;
{
	while (--amount >= 0)
		putc(c, curiop);
}

static
doformat(sp, fmt, ap)
register File	*sp;
register char	*fmt;
va_list	ap;
{
	register char	c;
	int	leftadj,
		width;
	File	*pushiop = curiop;

	curiop = sp;

	while (c = *fmt++) {
		if (c != '%') {
			putc(c, sp);
			continue;
		}

		padc = ' ';
		leftadj = width = 0;
		c = *fmt++;
		if (c == '-') {
			leftadj++;
			c = *fmt++;
		}
		if (c == '0') {
			padc = '0';
			c = *fmt++;
		}
		while (c >= '0' && c <= '9') {
			width = width * 10 + (c - '0');
			c = *fmt++;
		}
		if (c == '*') {
			width = va_arg(ap, int);
			c = *fmt++;
		}
	reswitch:
		/* At this point, fmt points at one past the format letter. */
		switch (c) {
		case 'l':
			c = Upper(*++fmt);
			goto reswitch;
	
		case '%':
			putc('%', curiop);
			break;
	
		case 'o':
			putld(leftadj, width, (long) va_arg(ap, int), 8);
			break;
	
		case 'd':
			putld(leftadj, width, (long) va_arg(ap, int), 10);
			break;
	
		case 'D':
			putld(leftadj, width, va_arg(ap, long), 10);
			break;
	
		case 'p':
		    {
		    	char	cbuf[20];

		    	PPchar(va_arg(ap, int), cbuf);
		    	puts(leftadj, width, cbuf);
		    	break;
		    }

		case 'n':
			if (va_arg(ap, int) != 1)
				puts(leftadj, width, "s");
			break;

		case 's':
			puts(leftadj, width, va_arg(ap, char *));
			break;
		
		case 'c':
			putc(va_arg(ap, int), curiop);
			break;
	
		case 'f':	/* current command name gets inserted here! */
			puts(leftadj, width, LastCmd->Name);
			break;

		default:
			complain("%%%c?", c);
		}
	}
	curiop = pushiop;
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

printf(fmt, va_alist)
char	*fmt;
va_dcl
{
	va_list	ap;

	va_start(ap);
	doformat(stdout, fmt, ap);
	va_end(ap);
}

/* VARARGS1 */

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

f_mess(fmt, va_alist)
char	*fmt;
va_dcl
{
	va_list	ap;

	va_start(ap);
	format(mesgbuf, sizeof mesgbuf, fmt, ap);
	va_end(ap);
	DrawMesg(NO);
	UpdMesg++;	/* Still needs updating (for convenience) */
}

/* VARARGS1 */

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
