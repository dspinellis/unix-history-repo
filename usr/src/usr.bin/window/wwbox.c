#ifndef lint
static	char *sccsid = "@(#)wwbox.c	3.1 83/08/18";
#endif

#include "ww.h"
#include "tt.h"

wwbox(w, r, c, nr, nc)
register struct ww *w;
register r, c;
int nr, nc;
{
	register r1, c1;
	register i;

	r1 = r + nr - 1;
	c1 = c + nc - 1;
	wwframec(w, r, c, WWF_D|WWF_R, 0);
	for (i = c + 1; i < c1; i++)
		wwframec(w, r, i, WWF_L|WWF_R, 0);
	wwframec(w, r, i, WWF_L|WWF_D, 0);
	for (i = r + 1; i < r1; i++)
		wwframec(w, i, c1, WWF_U|WWF_D, 0);
	wwframec(w, i, c1, WWF_U|WWF_L, 0);
	for (i = c1 - 1; i > c; i--)
		wwframec(w, r1, i, WWF_R|WWF_L, 0);
	wwframec(w, r1, i, WWF_R|WWF_U, 0);
	for (i = r1 - 1; i > r; i--)
		wwframec(w, i, c, WWF_D|WWF_U, 0);
}

wwunbox(w)
struct ww *w;
{
	wwunframe1(w, 0);
}
