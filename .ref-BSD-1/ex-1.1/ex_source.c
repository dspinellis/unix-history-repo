#
/*
 * Ex - a text editor
 * Bill Joy UCB September 1977
 */

#include "ex.h"
#include "ex_io.h"

STATIC	int slevel;

int	onintr();

source(file, okfail)
	char *file;
	int okfail;
{
	int osetexit[3], tty[3];
	register int saveinp, ointty, oerrno;
	char reenter;

	signal(INTR, 1);
	saveinp = dup(0);
	if (saveinp < 0)
		error("Too many nested sources");
	close(0);
	if (open(file, 0) < 0) {
		oerrno = errno;
		if (ruptible)
			signal(INTR, onintr);
		dup(saveinp);
		close(saveinp);
		errno = oerrno;
		if (!okfail)
			filioerr(file);
		return;
	}
	slevel++;
	ointty = intty;
	intty = gtty(0, tty) == 0;
	reenter = 0;
	getexit(osetexit);
	setexit();
	if (ruptible)
		signal(INTR, onintr);
	if (reenter == 0) {
		reenter++;
		commands(1, 1);
	} else if (slevel > 1) {
		close(0);
		dup(saveinp);
		close(saveinp);
		slevel--;
		resexit(osetexit);
		reset();
	}
	intty = ointty;
	close(0);
	dup(saveinp);
	close(saveinp);
	slevel--;
	resexit(osetexit);
}
