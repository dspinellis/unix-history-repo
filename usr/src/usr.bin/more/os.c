/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mark Nudleman.
 * 
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)os.c	5.4 (Berkeley) %G%";
#endif /* not lint */

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

char *getenv();

public int reading;

extern int screen_trashed;

static jmp_buf read_label;

/*
 * Pass the specified command to a shell to be executed.
 * Like plain "system()", but handles resetting terminal modes, etc.
 */
	public void
lsystem(cmd)
	char *cmd;
{
	int inp;
	char cmdbuf[256];
	char *shell;

	/*
	 * Print the command which is to be executed,
	 * unless the command starts with a "-".
	 */
	if (cmd[0] == '-')
		cmd++;
	else
	{
		lower_left();
		clear_eol();
		putstr("!");
		putstr(cmd);
		putstr("\n");
	}

	/*
	 * De-initialize the terminal and take out of raw mode.
	 */
	deinit();
	flush();
	raw_mode(0);

	/*
	 * Restore signals to their defaults.
	 */
	init_signals(0);

	/*
	 * Force standard input to be the terminal, "/dev/tty",
	 * even if less's standard input is coming from a pipe.
	 */
	inp = dup(0);
	close(0);
	if (open("/dev/tty", 0) < 0)
		dup(inp);

	/*
	 * Pass the command to the system to be executed.
	 * If we have a SHELL environment variable, use
	 * <$SHELL -c "command"> instead of just <command>.
	 * If the command is empty, just invoke a shell.
	 */
	if ((shell = getenv("SHELL")) != NULL && *shell != '\0')
	{
		if (*cmd == '\0')
			cmd = shell;
		else
		{
			sprintf(cmdbuf, "%s -c \"%s\"", shell, cmd);
			cmd = cmdbuf;
		}
	}
	if (*cmd == '\0')
		cmd = "sh";

	system(cmd);

	/*
	 * Restore standard input, reset signals, raw mode, etc.
	 */
	close(0);
	dup(inp);
	close(inp);

	init_signals(1);
	raw_mode(1);
	init();
	screen_trashed = 1;
#if defined(SIGWINCH) || defined(SIGWIND)
	/*
	 * Since we were ignoring window change signals while we executed
	 * the system command, we must assume the window changed.
	 */
	winch();
#endif
}

/*
 * Like read() system call, but is deliberately interruptable.
 * A call to intread() from a signal handler will interrupt
 * any pending iread().
 */
	public int
iread(fd, buf, len)
	int fd;
	char *buf;
	int len;
{
	register int n;

	if (setjmp(read_label))
		/*
		 * We jumped here from intread.
		 */
		return (READ_INTR);

	flush();
	reading = 1;
	n = read(fd, buf, len);
	reading = 0;
	if (n < 0)
		return (-1);
	return (n);
}

	public void
intread()
{
#if SIGSETMASK
	sigsetmask(0);
#endif
	longjmp(read_label, 1);
}

	public long
get_time()
{
	time_t time();

	return(time((long *)NULL));
}

/*
 * Expand a filename, substituting any environment variables, etc.
 * The implementation of this is necessarily very operating system
 * dependent.  This implementation is unabashedly only for Unix systems.
 */
#if GLOB

FILE *popen();

	public char *
glob(filename)
	char *filename;
{
	FILE *f;
	char *p;
	int ch;
	char *cmd;
	static char buffer[FILENAME];

	if (filename[0] == '#')
		return (filename);

	/*
	 * We get the shell to expand the filename for us by passing
	 * an "echo" command to the shell and reading its output.
	 */
	p = getenv("SHELL");
	if (p == NULL || *p == '\0')
	{
		/*
		 * Read the output of <echo filename>.
		 */
		cmd = calloc(strlen(filename)+8, sizeof(char));
		if (cmd == NULL)
			return (filename);
		sprintf(cmd, "echo \"%s\"", filename);
	} else
	{
		/*
		 * Read the output of <$SHELL -c "echo filename">.
		 */
		cmd = calloc(strlen(p)+12);
		if (cmd == NULL)
			return (filename);
		sprintf(cmd, "%s -c \"echo %s\"", p, filename);
	}

	if ((f = popen(cmd, "r")) == NULL)
		return (filename);
	free(cmd);

	for (p = buffer;  p < &buffer[sizeof(buffer)-1];  p++)
	{
		if ((ch = getc(f)) == '\n' || ch == EOF)
			break;
		*p = ch;
	}
	*p = '\0';
	pclose(f);
	return (buffer);
}

#else

	public char *
glob(filename)
	char *filename;
{
	return (filename);
}

#endif


/*
 * Returns NULL if the file can be opened and
 * is an ordinary file, otherwise an error message
 * (if it cannot be opened or is a directory, etc.)
 */

#include <sys/types.h>
#include <sys/stat.h>

	public char *
bad_file(filename, message, len)
	char *filename;
	char *message;
	unsigned int len;
{
	struct stat statbuf;

	if (stat(filename, &statbuf) < 0)
		return (errno_message(filename, message, len));

	if ((statbuf.st_mode & S_IFMT) == S_IFDIR)
	{
		static char is_dir[] = " is a directory";
		strtcpy(message, filename, len-sizeof(is_dir)-1);
		strcat(message, is_dir);
		return (message);
	}
	if ((statbuf.st_mode & S_IFMT) != S_IFREG)
	{
		static char not_reg[] = " is not a regular file";
		strtcpy(message, filename, len-sizeof(not_reg)-1);
		strcat(message, not_reg);
		return (message);
	}
	return (NULL);
}

/*
 * errno_message: Return an error message based on the value of "errno".
 * okreadfail: Return true if the previous failure of a read
 *	(on the input tty) should be considered ok.
 */

extern char *sys_errlist[];
extern int sys_nerr;
extern int errno;

	public char *
errno_message(filename, message, len)
	char *filename;
	char *message;
	unsigned int len;
{
	char *p;
	char msg[16];

	if (errno < sys_nerr)
		p = sys_errlist[errno];
	else
	{
		sprintf(msg, "Error %d", errno);
		p = msg;
	}
	strtcpy(message, filename, len-strlen(p)-3);
	strcat(message, ": ");
	strcat(message, p);
	return (message);
}
