/* Copyright (c) 1979 Regents of the University of California */
#ifdef TRACE
#include <stdio.h>
#undef putchar
#endif

printf(fmt, args)
{

	_doprnt(fmt, &args, 0);
}

_strout(count, string, adjust, flail, fillch)
register char *string;
register count;
register int adjust;
#ifdef TRACE
register struct _iobuf *flail;
#else
register struct {int i; } *flail;
#endif
{
	if (adjust < 0) {
		if (*string=='-' && fillch=='0') {
#ifdef TRACE
			if (flail)
				putc(*string++, flail);
			else
#endif
				putchar(*string++);
			count--;
		}
		adjust= -adjust;
		while (--adjust>=0)
#ifdef TRACE
			if (flail)
				putc(fillch, flail);
			else
#endif
				putchar(fillch);
	}
	while (--count>=0)
#ifdef TRACE
		if (flail)
			putc(*string++, flail);
		else
#endif
			putchar(*string++);
	while (--adjust>=0)
#ifdef TRACE
		if (flail)
			putc(fillch, flail);
		else
#endif
			putchar(fillch);
}
