#ifndef lint
static	char *sccsid = "@(#)wwdump.c	3.2 83/08/11";
#endif

#include "ww.h"

static char cmap[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

wwdumpcov(w)
register struct ww *w;
{
	register i, j;

	(*tt.tt_setinsert)(0);
	(*tt.tt_setmodes)(0);
	(*tt.tt_clear)();
	for (i = 0; i < w->ww_w.nr; i++) {
		(*tt.tt_move)(w->ww_w.t + i, w->ww_w.l);
		for (j = 0; j < w->ww_w.nc; j++)
			(*tt.tt_putc)(cmap[w->ww_cov[i][j]]);
	}
}

wwdumpwin(w)
register struct ww *w;
{
	register i, j;

	(*tt.tt_setinsert)(0);
	(*tt.tt_setmodes)(0);
	(*tt.tt_clear)();
	for (i = 0; i < w->ww_w.nr; i++) {
		(*tt.tt_move)(w->ww_w.t + i, w->ww_w.l);
		for (j = 0; j < w->ww_w.nc; j++)
			(*tt.tt_putc)(w->ww_win[i][j] & WWM_COV ? 'C' : ' ');
	}
}

wwdumpnvis(w)
register struct ww *w;
{
	register i;

	(*tt.tt_setinsert)(0);
	(*tt.tt_setmodes)(0);
	(*tt.tt_clear)();
	for (i = 0; i < w->ww_w.nr; i++) {
		(*tt.tt_move)(w->ww_w.t + i, w->ww_w.l);
		(*tt.tt_putc)(w->ww_nvis[i] / 100 % 10 + '0');
		(*tt.tt_putc)(w->ww_nvis[i] / 10 % 10 + '0');
		(*tt.tt_putc)(w->ww_nvis[i] % 10 + '0');
	}
}

wwdumpsmap()
{
	register i, j;

	(*tt.tt_setinsert)(0);
	(*tt.tt_setmodes)(0);
	(*tt.tt_clear)();
	for (i = 0; i < wwnrow; i++) {
		(*tt.tt_move)(i, 0);
		for (j = 0; j < wwncol; j++)
			(*tt.tt_putc)(cmap[wwsmap[i][j]]);
	}
}

/*
wwdumpns()
{
	register i, j;

	(*tt.tt_setinsert)(0);
	(*tt.tt_clear)();
	for (i = 0; i < wwnrow; i++) {
		(*tt.tt_move)(i, 0);
		for (j = 0; j < wwncol; j++) {
			(*tt.tt_setmodes)(wwns[i][j].c_m);
			(*tt.tt_putc)(wwns[i][j].c_c);
		}
	}
}

wwdumpos()
{
	register i, j;

	(*tt.tt_setinsert)(0);
	(*tt.tt_clreos)();
	for (i = 0; i < wwnrow; i++) {
		(*tt.tt_move)(i, 0);
		for (j = 0; j < wwncol; j++) {
			(*tt.tt_setmodes)(wwns[i][j].c_m);
			(*tt.tt_putc)(wwns[i][j].c_c);
		}
	}
}
*/
