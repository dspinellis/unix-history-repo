#include <sgtty.h>

/* is unit c a tty? */
isatty(c)
	int c;
{
	struct sgttyb tty;

	return (gtty(c, &tty) == 0);
}
