#ifndef lint
static	char *sccsid = "@(#)wwdump.c	3.7 83/09/15";
#endif

#include "ww.h"
#include "tt.h"

static char cmap[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

wwdumpcov(w)
register struct ww *w;
{
	register i, j;

	tt.tt_nmodes = 0;
	(*tt.tt_clear)();
	for (i = w->ww_i.t; i < w->ww_i.b; i++) {
		(*tt.tt_move)(i, w->ww_i.l);
		for (j = w->ww_i.l; j < w->ww_i.r; j++)
			(*tt.tt_putc)(cmap[w->ww_cov[i][j]]);
	}
}

wwdumpwin(w)
register struct ww *w;
{
	register i, j;

	tt.tt_nmodes = 0;
	(*tt.tt_clear)();
	for (i = w->ww_i.t; i < w->ww_i.b; i++) {
		(*tt.tt_move)(i, w->ww_i.l);
		for (j = w->ww_i.l; j < w->ww_i.r; j++)
			(*tt.tt_putc)(w->ww_win[i][j] & WWM_COV ? 'C' : ' ');
	}
}

wwdumpnvis(w)
register struct ww *w;
{
	register i;

	tt.tt_nmodes = 0;
	(*tt.tt_clear)();
	for (i = 0; i < w->ww_w.nr; i++) {
		(*tt.tt_move)(i, w->ww_w.l);
		(*tt.tt_putc)(w->ww_nvis[i] / 100 % 10 + '0');
		(*tt.tt_putc)(w->ww_nvis[i] / 10 % 10 + '0');
		(*tt.tt_putc)(w->ww_nvis[i] % 10 + '0');
	}
}

wwdumpsmap()
{
	register i, j;

	tt.tt_nmodes = 0;
	(*tt.tt_clear)();
	for (i = 0; i < wwnrow; i++) {
		(*tt.tt_move)(i, 0);
		for (j = 0; j < wwncol; j++)
			(*tt.tt_putc)(cmap[wwsmap[i][j]]);
	}
}

wwdumpns()
{
	register i, j;

	(*tt.tt_clear)();
	for (i = 0; i < wwnrow; i++) {
		(*tt.tt_move)(i, 0);
		for (j = 0; j < wwncol; j++) {
			tt.tt_nmodes = wwns[i][j].c_m & tt.tt_availmodes;
			(*tt.tt_putc)(wwns[i][j].c_c);
		}
	}
}

wwdumpos()
{
	register i, j;

	(*tt.tt_clear)();
	for (i = 0; i < wwnrow; i++) {
		(*tt.tt_move)(i, 0);
		for (j = 0; j < wwncol; j++) {
			tt.tt_nmodes = wwos[i][j].c_m & tt.tt_availmodes;
			(*tt.tt_putc)(wwns[i][j].c_c);
		}
	}
}
