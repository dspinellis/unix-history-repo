#include <sys/cdefs.h>

#include <curses.h>

int
addnstr(s, n)
	const char *s;
	int n;
{
	int ch;

	while (n-- && (ch = *s++))
		addch(ch);
	return (OK);
}
