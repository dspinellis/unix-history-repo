/* Copyright (c) 1979 Regents of the University of California */
/*
 * Hacked "printf" which prints through putchar.
 * DONT USE WITH STDIO!
 */
printf(fmt, args)
char *fmt;
{
	_doprnt(fmt, &args, 0);
}

_strout(count, string, adjust, foo, fillch)
register char *string;
register int count;
int adjust;
register struct { int a[6]; } *foo;
{

	while (adjust < 0) {
		if (*string=='-' && fillch=='0') {
			if (foo)
				fputc(*string++, foo);
			else
				putchar(*string++);
			count--;
		}
		if (foo)
			fputc(fillch, foo);
		else
			putchar(fillch);
		adjust++;
	}
	while (--count>=0)
		if (foo)
			fputc(*string++, foo);
		else
			putchar(*string++);
	while (adjust) {
		if (foo)
			fputc(fillch, foo);
		else
			putchar(fillch);
		adjust--;
	}
}
