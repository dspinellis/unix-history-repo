/* Copyright (c) 1979 Regents of the University of California */
#ifdef TRACE
#include <stdio.h>
#undef putchar
#endif

printf(fmt, args)
{

	_doprnt(fmt, &args, 0);
}

_strout(string, count, adjust, file, fillch)
register char *string;
register int count;
int adjust;
register struct { int a; } *file;
{

	while (adjust < 0) {
		if (*string=='-' && fillch=='0') {
#ifdef TRACE
			if (file)
				putc(*string++, file);
			else
#endif
				putchar(*string++);
			count--;
		}
#ifdef TRACE
		if (file)
			putc(fillch, file);
		else
#endif
			putchar(fillch);
		adjust++;
	}
	while (--count>=0)
#ifdef TRACE
		if (file)
			putc(*string++, file);
		else
#endif
			putchar(*string++);
	while (adjust) {
#ifdef TRACE
		if (file)
			putc(fillch, file);
		else
#endif
			putchar(fillch);
		adjust--;
	}
}
