/* utils.c: functions of general utility */

#include <errno.h>
#include <setjmp.h>
#include "rc.h"
#include "jbwrap.h"

/* print error with line number on noninteractive shells (i.e., scripts) */

extern void pr_error(char *s) {
	if (s != NULL) {
		if (interactive)
			fprint(2, "%s\n", s);
		else
			fprint(2, "line %d: %s\n", lineno - 1, s);
	}
}

/* our perror */

extern void uerror(char *s) {
	extern int sys_nerr;
	extern char *sys_errlist[];
	if (errno > sys_nerr)
		return;
	if (s != NULL)
		fprint(2, "%s: %s\n", s, sys_errlist[errno]);
	else
		fprint(2, "%s\n", sys_errlist[errno]);
}

/* Die horribly. This should never get called. Please let me know if it does. */

#define PANICMSG "rc panic: "

extern void panic(char *s) {
	write(2, PANICMSG, conststrlen(PANICMSG));
	write(2, s, strlen(s));
	write(2, "!\n", 2);
	exit(1);
}

/* ascii -> unsigned conversion routines. -1 indicates conversion error. */

extern int n2u(char *s, unsigned int base) {
	unsigned int i;
	for (i = 0; *s != '\0'; s++) {
		unsigned int j = (unsigned int) *s - '0';
		if (j >= base) /* small hack with unsigned ints -- one compare for range test */
			return -1;
		i = i * base + j;
	}
	return (int) i;
}

/* The last word in portable ANSI: a strcmp wrapper for qsort */

extern int starstrcmp(const void *s1, const void *s2) {
	return strcmp(*(char **)s1, *(char **)s2);
}

/* tests to see if pathname begins with "/", "./", or "../" */

extern bool isabsolute(char *path) {
	return path[0] == '/' || (path[0] == '.' && (path[1] == '/' || (path[1] == '.' && path[2] == '/')));
}

/* signal-safe read and write (for BSD slow devices). writeall also allows partial writes */

extern void writeall(int fd, char *buf, size_t remain) {
	int i;
	for (i = 0; remain > 0; buf += i, remain -= i) {
		interrupt_happened = FALSE;
		if (!setjmp(slowbuf.j)) {
			slow = TRUE;
			if (interrupt_happened)
				break;
			else if ((i = write(fd, buf, remain)) <= 0)
				break; /* abort silently on errors in write() */
		} else
			break;
		slow = FALSE;
	}
	slow = FALSE;
	SIGCHK;
}

extern int rc_read(int fd, char *buf, size_t n) {
	long /*ssize_t*/ r;
	interrupt_happened = FALSE;
	if (!setjmp(slowbuf.j)) {
		slow = TRUE;
		if (!interrupt_happened)
			r = read(fd, buf, n);
		else
			r = -2;
	} else
		r = -2;
	slow = FALSE;
	if (r == -2) {
		errno = EINTR;
		r = -1;
	}
	SIGCHK;
	return r;
}

/* clear out z bytes from character string s */

extern char *clear(char *s, size_t z) {
	while (z != 0)
		s[--z] = 0;
	return s;
}

/* duplicate a fd and close the old one only if necessary */

extern int mvfd(int i, int j) {
	if (i != j) {
		int s = dup2(i, j);
		close(i);
		return s;
	}
	return 0;
}
