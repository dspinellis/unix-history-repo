/*
 * Operating system dependent routines.
 *
 * Most of the stuff in here is based on Unix, but an attempt
 * has been made to make things work on other operating systems.
 * This will sometimes result in a loss of functionality, unless
 * someone rewrites code specifically for the new operating system.
 *
 * The makefile provides defines to decide whether various
 * Unix features are present.
 */

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include "less.h"

/*
 * BSD setjmp() saves (and longjmp() restores) the signal mask.
 * This costs a system call or two per setjmp(), so if possible we clear the
 * signal mask with sigsetmask(), and use _setjmp()/_longjmp() instead.
 * On other systems, setjmp() doesn't affect the signal mask and so
 * _setjmp() does not exist; we just use setjmp().
 */
#if HAS__SETJMP && SIGSETMASK
#define SET_JUMP	_setjmp
#define LONG_JUMP	_longjmp
#else
#define SET_JUMP	setjmp
#define LONG_JUMP	longjmp
#endif

extern char *getenv();

public int reading;

static jmp_buf read_label;

/*
 * Like read() system call, but is deliberately interruptible.
 * A call to intread() from a signal handler will interrupt
 * any pending iread().
 */
	public int
iread(fd, buf, len)
	int fd;
	char *buf;
	unsigned int len;
{
	register int n;

	if (SET_JUMP(read_label))
	{
		/*
		 * We jumped here from intread.
		 */
		reading = 0;
#if SIGSETMASK
		sigsetmask(0);
#endif
		return (READ_INTR);
	}

	flush();
	reading = 1;
	n = read(fd, buf, len);
	reading = 0;
	if (n < 0)
		return (-1);
	return (n);
}

/*
 * Interrupt a pending iread().
 */
	public void
intread()
{
	LONG_JUMP(read_label, 1);
}

#if GET_TIME
	public long
get_time()
{
	long t;

	time(&t);
	return (t);
}
#endif

/*
 * errno_message: Return an error message based on the value of "errno".
 */

#if PERROR

extern char *sys_errlist[];
extern int sys_nerr;
extern int errno;

	public char *
errno_message(filename)
	char *filename;
{
	register char *p;
	register char *m;
	char msg[16];

	if (errno < sys_nerr)
		p = sys_errlist[errno];
	else
	{
		sprintf(msg, "Error %d", errno);
		p = msg;
	}
	m = (char *) ecalloc(strlen(filename) + strlen(p) + 3, sizeof(char));
	sprintf(m, "%s: %s", filename, p);
	return (m);
}

#else

	public char *
errno_message(filename)
	char *filename;
{
	register char *m;
	static char msg[] = ": cannot open";

	m = (char *) ecalloc(strlen(filename) + sizeof(msg), sizeof(char));
	strcpy(m, filename);
	strcat(m, msg);
	return (m);
}

#endif
