#
/*
 * Ex - a text editor
 * Bill Joy UCB September, 1977
 */
int	mask	0377;		/* Standard V6 - only 256 users */

#include "ex.h"
#include "ex_tty.h"
#include "ex_io.h"


#define	UCASE	04
#define	TABS	02
#define	CRFLG	020

char	TTYNAM[]	"/dev/ttyx";

initoptions(f)
	char f;
{
	int uid, ttyno, rcio;
	char Home[40];
	register char *H;

	intty = gTTY(0) == 0;
	if (!intty && f == 0) {
unknown:
		setterm("u");
		return;
	}
	if (gTTY(1)) {
		if (f == 0 || gTTY(2))
			goto unknown;
		f = 2;
	} else
		f = 1;
	UPPERCASE = (tty[2] & UCASE) != 0;
	PT = (tty[2] & TABS) == 0;
	NOCR = (tty[2] & CRFLG) == 0;
	ttyno = ttyn(f);
	if (ttyno != 'x') {
		if (hget(ttyno) == 0)
			isetterm(hsgettype());
	}
	uid = getuid() & mask;
	H = hgethome();
	if (uid == 0)
		strcpy(H, "/");
	else if (uid != hgetuid())
		return;
	TTYNAM[8] = ttyno;
	gettmode();
	strcpy(home, H);
	strcpy(Home, H);
	strcat(Home, "/.exrc");
	source(Home, 1);
}

isetterm(type)
	char *type;
{
	char reenter;
	int osetexit[3];

	getexit(osetexit);
	reenter = 0;
	setexit();
	if (reenter == 0) {
		reenter++;
		Setterm(type, 0);
	}
	resexit(osetexit);
}

gettmode()
{
	struct stb stbuf;

	stat(TTYNAM, &stbuf);
	TMODE = stbuf.flags & 0777;
}
