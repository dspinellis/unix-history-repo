/*
 * Warning: extremely XXX
 * fake syslog to write to stderr, for standalone test version of init
 */
#include <stdarg.h>
#include <stdio.h>


static FILE	*log = stderr;


void
openlog()
{
	log = fopen("/dev/tty", "w");
}


void
syslog(int pri, const char *fmt, ...)
{
va_list		ap;

	va_start(ap, fmt);
	vfprintf(log, fmt, ap);
	va_end(ap);
	fprintf(log, "\n");
}

void
vsyslog(int pri, const char *fmt, va_list ap)
{
	vfprintf(log, fmt, ap);
	fprintf(log, "\n");
}


void
closelog(void)
{
	fclose(log);
}
