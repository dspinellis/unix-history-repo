/*
 * Routines dealing with getting input from the keyboard (i.e. from the user).
 */

#include "less.h"
#if __MSDOS__
#include <io.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#endif

static int tty;

/*
 * Open keyboard for input.
 */
	public void
open_getchr()
{
#if __MDDOS__
	/*
	 * Open a new handle to CON: in binary mode 
	 * for unbuffered keyboard read.
	 */
	tty = open("CON", O_RDONLY|O_BINARY);
#else
	/*
	 * Try /dev/tty.
	 * If that doesn't work, use file descriptor 2,
	 * which in Unix is usually attached to the screen,
	 * but also usually lets you read from the keyboard.
	 */
	tty = open("/dev/tty", 0);
	if (tty < 0)
		tty = 2;
#endif
}

/*
 * Get a character from the keyboard.
 */
	public int
getchr()
{
	char c;
	int result;

	do
	{
		result = iread(tty, &c, sizeof(char));
		if (result == READ_INTR)
			return (READ_INTR);
		if (result < 0)
		{
			/*
			 * Don't call error() here,
			 * because error calls getchr!
			 */
			quit(1);
		}
#if __MSDOS__
		/*
		 * In raw read, we don't see ^C so look here for it.
		 */
		if (c == '\003')
			raise(SIGINT);
#endif
		/*
		 * Various parts of the program cannot handle
		 * an input character of '\0'.
		 * If a '\0' was actually typed, convert it to '\200' here.
		 */
		if (c == '\0')
			c = '\200';
	} while (result != 1);

	return (c);
}
