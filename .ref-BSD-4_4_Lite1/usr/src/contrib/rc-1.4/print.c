/* print.c -- formatted printing routines (Paul Haahr, 12/91) */

#include "rc.h"
#include <setjmp.h>

#define	PRINT_ALLOCSIZE	((size_t)64)
#define	SPRINT_BUFSIZ	((size_t)1024)

#define	MAXCONV 256

/*
 * conversion functions
 *	true return -> flag changes only, not a conversion
 */

#define Flag(name, flag) \
static bool name(Format *format, int c) { \
	format->flags |= flag; \
	return TRUE; \
}

Flag(uconv,	FMT_unsigned)
Flag(hconv,	FMT_short)
Flag(lconv,	FMT_long)
Flag(altconv,	FMT_altform)
Flag(leftconv,	FMT_leftside)
Flag(dotconv,	FMT_f2set)

static bool digitconv(Format *format, int c) {
	if (format->flags & FMT_f2set)
		format->f2 = 10 * format->f2 + c - '0';
	else {
		format->flags |= FMT_f1set;
		format->f1 = 10 * format->f1 + c - '0';
	}
	return TRUE;
}

static bool zeroconv(Format *format, int c) {
	if (format->flags & (FMT_f1set | FMT_f2set))
		return digitconv(format, '0');
	format->flags |= FMT_zeropad;
	return TRUE;
}

static void pad(Format *format, size_t len, int c) {
	while (len-- != 0)
		fmtputc(format, c);
}

static bool sconv(Format *format, int c) {
	char *s = va_arg(format->args, char *);
	if ((format->flags & FMT_f1set) == 0)
		fmtcat(format, s);
	else {
		size_t len = strlen(s), width = format->f1 - len;
		if (format->flags & FMT_leftside) {
			fmtappend(format, s, len);
			pad(format, width, ' ');
		} else {
			pad(format, width, ' ');
			fmtappend(format, s, len);
		}
	}
	return FALSE;
}

static char *utoa(unsigned long u, char *t, unsigned int radix, const char *digit) {
	if (u >= radix) {
		t = utoa(u / radix, t, radix, digit);
		u %= radix;
	}
	*t++ = digit[u];
	return t;
}

static void intconv(Format *format, unsigned int radix, int upper, const char *altform) {
	static const char * const table[] = {
		"0123456789abcdefghijklmnopqrstuvwxyz",
		"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",
	};
	char padchar;
	size_t len, pre, zeroes, padding, width;
	long n, flags;
	unsigned long u;
	char number[64], prefix[20];

	if (radix > 36)
		return;

	flags = format->flags;
	if (flags & FMT_long)
		n = va_arg(format->args, long);
	else if (flags & FMT_short)
		n = va_arg(format->args, short);
	else
		n = va_arg(format->args, int);

	pre = 0;
	if ((flags & FMT_unsigned) || n >= 0)
		u = n;
	else {
		prefix[pre++] = '-';
		u = -n;
	}

	if (flags & FMT_altform)
		while (*altform != '\0')
			prefix[pre++] = *altform++;

	len = utoa(u, number, radix, table[upper]) - number;
	if ((flags & FMT_f2set) && (size_t) format->f2 > len)
		zeroes = format->f2 - len;
	else
		zeroes = 0;

	width = pre + zeroes + len;
	if ((flags & FMT_f1set) && (size_t) format->f1 > width) {
		padding = format->f1 - width;
	} else
		padding = 0;

	padchar = ' ';
	if (padding > 0 && flags & FMT_zeropad) {
		padchar = '0';
		if ((flags & FMT_leftside) == 0) {
			zeroes += padding;
			padding = 0;
		}
	}


	if ((flags & FMT_leftside) == 0)
		pad(format, padding, padchar);
	fmtappend(format, prefix, pre);
	pad(format, zeroes, '0');
	fmtappend(format, number, len);
	if (flags & FMT_leftside)
		pad(format, padding, padchar);
}

static bool cconv(Format *format, int c) {
	fmtputc(format, va_arg(format->args, int));
	return FALSE;
}

static bool dconv(Format *format, int c) {
	intconv(format, 10, 0, "");
	return FALSE;
}

static bool oconv(Format *format, int c) {
	intconv(format, 8, 0, "0");
	return FALSE;
}

static bool xconv(Format *format, int c) {
	intconv(format, 16, 0, "0x");
	return FALSE;
}

static bool pctconv(Format *format, int c) {
	fmtputc(format, '%');
	return FALSE;
}

static bool badconv(Format *format, int c) {
	panic("bad conversion character in printfmt");
	/* NOTREACHED */
	return FALSE; /* hush up gcc -Wall */
}


/*
 * conversion table management
 */

static Conv fmttab[MAXCONV];

static void inittab(void) {
	int i;
	for (i = 0; i < MAXCONV; i++)
		fmttab[i] = badconv;

	fmttab['s'] = sconv;
	fmttab['c'] = cconv;
	fmttab['d'] = dconv;
	fmttab['o'] = oconv;
	fmttab['x'] = xconv;
	fmttab['%'] = pctconv;

	fmttab['u'] = uconv;
	fmttab['h'] = hconv;
	fmttab['l'] = lconv;
	fmttab['#'] = altconv;
	fmttab['-'] = leftconv;
	fmttab['.'] = dotconv;

	fmttab['0'] = zeroconv;
	for (i = '1'; i <= '9'; i++)
		fmttab[i] = digitconv;
}

extern bool (*fmtinstall(int c, bool (*f)(Format *, int)))(Format *, int) {
/*Conv fmtinstall(int c, Conv f) {*/
	Conv oldf;
	if (fmttab[0] == NULL)
		inittab();
	c &= MAXCONV - 1;
	oldf = fmttab[c];
	if (f != NULL)
		fmttab[c] = f;
	return oldf;
}


/*
 * functions for inserting strings in the format buffer
 */

extern void fmtappend(Format *format, const char *s, size_t len) {
	while (format->buf + len > format->bufend) {
		size_t split = format->bufend - format->buf;
		memcpy(format->buf, s, split);
		format->buf += split;
		s += split;
		len -= split;
		(*format->grow)(format, len);
	}
	memcpy(format->buf, s, len);
	format->buf += len;
}

extern void fmtcat(Format *format, const char *s) {
	fmtappend(format, s, strlen(s));
}

/*
 * printfmt -- the driver routine
 */

extern int printfmt(Format *format, const char *fmt) {
	unsigned const char *s = (unsigned const char *) fmt;

	if (fmttab[0] == NULL)
		inittab();

	for (;;) {
		int c = *s++;
		switch (c) {
		case '%':
			format->flags = format->f1 = format->f2 = 0;
			do
				c = *s++;
			while ((*fmttab[c])(format, c));
			break;
		case '\0':
			return format->buf - format->bufbegin + format->flushed;
		default:
			fmtputc(format, c);
			break;
		}
	}
}


/*
 * the public entry points
 */

extern int fmtprint(Format *format, const char *fmt,...) {
	int n = -format->flushed;
	va_list saveargs = format->args;

	va_start(format->args, fmt);
	n += printfmt(format, fmt);
	va_end(format->args);
	format->args = saveargs;

	return n + format->flushed;
}

static void fprint_flush(Format *format, size_t more) {
	size_t n = format->buf - format->bufbegin;
	char *buf = format->bufbegin;

	format->flushed += n;
	format->buf = format->bufbegin;
	writeall(format->u.n, buf, n);
}

extern int fprint(int fd, const char *fmt,...) {
	char buf[1024];
	Format format;

	format.buf	= buf;
	format.bufbegin	= buf;
	format.bufend	= buf + sizeof buf;
	format.grow	= fprint_flush;
	format.flushed	= 0;
	format.u.n	= fd;

	va_start(format.args, fmt);
	printfmt(&format, fmt);
	va_end(format.args);

	fprint_flush(&format, (size_t) 0);
	return format.flushed;
}

static void memprint_grow(Format *format, size_t more) {
	char *buf;
	size_t len = format->bufend - format->bufbegin + 1;
	len = (len >= more)
		? len * 2
		: ((len + more) + PRINT_ALLOCSIZE) &~ (PRINT_ALLOCSIZE - 1);
	if (format->u.n)
		buf = erealloc(format->bufbegin, len);
	else {
		size_t used = format->buf - format->bufbegin;
		buf = nalloc(len);
		memcpy(buf, format->bufbegin, used);
	}
	format->buf	 = buf + (format->buf - format->bufbegin);
	format->bufbegin = buf;
	format->bufend	 = buf + len - 1;
}

static char *memprint(Format *format, const char *fmt, char *buf, size_t len) {
	format->buf	 = buf;
	format->bufbegin = buf;
	format->bufend	 = buf + len - 1;
	format->grow	 = memprint_grow;
	format->flushed	 = 0;
	printfmt(format, fmt);
	*format->buf = '\0';
	return format->bufbegin;
}

extern char *mprint(const char *fmt,...) {
	Format format;
	char *result;
	format.u.n = 1;
	va_start(format.args, fmt);
	result = memprint(&format, fmt, ealloc(PRINT_ALLOCSIZE), PRINT_ALLOCSIZE);
	va_end(format.args);
	return result;
}

extern char *nprint(const char *fmt,...) {
	Format format;
	char *result;
	format.u.n = 0;
	va_start(format.args, fmt);
	result = memprint(&format, fmt, nalloc(PRINT_ALLOCSIZE), PRINT_ALLOCSIZE);
	va_end(format.args);
	return result;
}
