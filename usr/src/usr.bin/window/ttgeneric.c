#ifndef lint
static	char *sccsid = "@(#)ttgeneric.c	3.11 83/08/18";
#endif

#include "ww.h"
#include "tt.h"

char *tgoto();

char gen_frame[16] = {
	' ', '|', '-', '+',
	'|', '|', '+', '+',
	'-', '+', '-', '+',
	'+', '+', '+', '+'
};

int gen_row, gen_col;
char gen_modes;
char gen_insert;

char *gen_CM;
char *gen_IM;
char *gen_IC;
char *gen_IP;
char *gen_EI;
char *gen_DC;
char *gen_AL;
char *gen_DL;
char *gen_CE;
char *gen_CD;
char *gen_CL;
char *gen_VS;
char *gen_VE;
char *gen_SO;
char *gen_SE;
char *gen_US;
char *gen_UE;
char *gen_UP;
char *gen_PC;
char *gen_BC;
char *gen_ND;
char *gen_HO;
char *gen_NL;
char gen_MI;
char gen_MS;
char gen_AM;
char gen_OS;
char gen_BS;
int gen_CO;
int gen_LI;

#define pc(c) putchar('c')
#define ps(s) fputs((s), stdout)

gen_setinsert(new)
char new;
{
	if (gen_insert == new)
		return;
	if (new) {
		if (gen_IM)
			ps(gen_IM);
	} else
		if (gen_EI)
			ps(gen_EI);
	gen_insert = new;
}

gen_setmodes(new)
register new;
{
	register diff;

	new &= tt.tt_availmodes;
	if ((diff = new ^ gen_modes) == 0)
		return;
	if (diff & WWM_REV) {
		if (new & WWM_REV) {
			if (gen_SO)
				ps(gen_SO);
		} else
			if (gen_SE)
				ps(gen_SE);
	}
	if (diff & WWM_UL) {
		if (new & WWM_UL) {
			if (gen_US)
				ps(gen_US);
		} else
			if (gen_UE)
				ps(gen_UE);
	}
	gen_modes = new;
}

gen_insline()
{
	if (gen_AL)
		tt_tputs(gen_AL, gen_LI - gen_row);
}

gen_delline()
{
	if (gen_DL)
		tt_tputs(gen_DL, gen_LI - gen_row);
}

gen_putc(c)
register char c;
{
	if (gen_insert) {
		if (gen_IC)
			tt_tputs(gen_IC, gen_CO - gen_col);
		putchar(c);
		if (gen_IP)
			tt_tputs(gen_IP, gen_CO - gen_col);
	} else
		putchar(c);
	if (++gen_col == gen_CO)
		if (gen_AM)
			gen_col = 0, gen_row++;
		else
			gen_col--;
}

gen_write(start, end)
register char *start, *end;
{
	if (gen_insert) {
		while (start <= end) {
			if (gen_IC)
				tt_tputs(gen_IC, gen_CO - gen_col);
			putchar(*start++);
			if (gen_IP)
				tt_tputs(gen_IP, gen_CO - gen_col);
			gen_col++;
		}
	} else {
		gen_col += end - start + 1;
		while (start <= end)
			putchar(*start++);
	}
	if (gen_col == gen_CO)
		if (gen_AM)
			gen_col = 0, gen_row++;
		else
			gen_col--;
}

gen_blank(n)
register n;
{
	if (n <= 0)
		return;
	if (gen_insert) {
		while (--n >= 0) {
			if (gen_IC)
				tt_tputs(gen_IC, gen_CO - gen_col);
			putchar(' ');
			if (gen_IP)
				tt_tputs(gen_IP, gen_CO - gen_col);
			gen_col++;
		}
	} else {
		gen_col += n;
		while (--n >= 0)
			putchar(' ');
	}
	if (gen_col == gen_CO)
		if (gen_AM)
			gen_col = 0, gen_row++;
		else
			gen_col--;
}

gen_move(row, col)
register char row, col;
{
	if (gen_row == row && gen_col == col)
		return;
	if (!gen_MI && gen_insert)
		if (gen_EI)
			ps(gen_EI);
	if (!gen_MS && gen_modes & WWM_REV)
		if (gen_SE)
			ps(gen_SE);
	if (gen_row == row) {
		if (gen_col == col)
			return;
		if (gen_col == col - 1) {
			if (gen_ND) {
				ps(gen_ND);
				goto out;
			}
		} else if (gen_col == col + 1) {
			if (gen_BC) {
				ps(gen_BC);
				goto out;
			}
		}
	}
	if (gen_col == col) {
		if (gen_row == row + 1) {
			if (gen_UP) {
				ps(gen_UP);
				goto out;
			}
		} else if (gen_row == row + 1) {
			if (gen_NL) {
				ps(gen_NL);
				goto out;
			}
		}
	}
	if (gen_HO && col == 0 && row == 0) {
		ps(gen_HO);
		goto out;
	}
	ps(tgoto(gen_CM, col, row));
out:
	gen_col = col;
	gen_row = row;
	if (!gen_MI && gen_insert)
		if (gen_IM)
			ps(gen_IM);
	if (!gen_MS && gen_modes & WWM_REV)
		if (gen_SO)
			ps(gen_SO);
}

gen_init()
{
	if (gen_VS)
		ps(gen_VS);
	if (gen_CL)
		ps(gen_CL);
	gen_col = gen_row = 0;
	gen_insert = 0;
	gen_modes = 0;
}

gen_end()
{
	gen_setmodes(0);
	gen_setinsert(0);
	if (gen_VE)
		ps(gen_VE);
}

gen_clreol()
{
	if (gen_CE)
		tt_tputs(gen_CE, gen_CO - gen_col);
}

gen_clreos()
{
	if (gen_CD)
		tt_tputs(gen_CD, gen_LI - gen_row);
}

gen_clear()
{
	if (gen_CL)
		ps(gen_CL);
}

gen_delchar()
{
	if (gen_DC)
		tt_tputs(gen_DC, gen_CO - gen_col);
}

tt_generic()
{
	gen_CM = tt_xgetstr("cm");		/* may not work */
	gen_IM = tt_xgetstr("im");
	gen_IC = tt_tgetstr("ic");
	gen_IP = tt_tgetstr("ip");
	gen_EI = tt_xgetstr("ei");
	gen_DC = tt_tgetstr("dc");
	gen_AL = tt_tgetstr("al");
	gen_DL = tt_tgetstr("dl");
	gen_CE = tt_tgetstr("ce");
	gen_CD = tt_tgetstr("cd");
	gen_CL = tt_xgetstr("cl");
	gen_VS = tt_xgetstr("vs");
	gen_VE = tt_xgetstr("ve");
	gen_SO = tt_xgetstr("so");
	gen_SE = tt_xgetstr("se");
	gen_US = tt_xgetstr("us");
	gen_UE = tt_xgetstr("ue");
	gen_UP = tt_xgetstr("up");
	gen_PC = tt_tgetstr("pc");
	gen_BC = tt_xgetstr("bc");
	gen_ND = tt_xgetstr("nd");
	gen_HO = tt_xgetstr("ho");
	gen_NL = tt_xgetstr("nl");
	gen_MI = tgetflag("mi");
	gen_MS = tgetflag("ms");
	gen_AM = tgetflag("am");
	gen_OS = tgetflag("os");
	gen_BS = tgetflag("bs");
	gen_CO = tgetnum("co");
	gen_LI = tgetnum("li");

	if (gen_CL == 0 || gen_CM == 0 || gen_OS)
		return -1;

	if (gen_NL == 0)
		gen_NL = "\n";
	if (gen_BC == 0 && gen_BS)
		gen_BC == "\b";

	{
		extern char PC, *BC, *UP;
		extern short ospeed;

		PC = gen_PC ? *gen_PC : 0;
		BC = gen_BC;
		UP = gen_UP;
		ospeed = wwoldtty.ww_sgttyb.sg_ospeed;
	}

	if (gen_IM)
		tt.tt_setinsert = gen_setinsert;
	if (gen_DC)
		tt.tt_delchar = gen_delchar;
	if (gen_AL)
		tt.tt_insline = gen_insline;
	if (gen_DL)
		tt.tt_delline = gen_delline;
	if (gen_CE)
		tt.tt_clreol = gen_clreol;
	if (gen_CD)
		tt.tt_clreos = gen_clreos;
	if (gen_CL)
		tt.tt_clear = gen_clear;
	if (gen_SO)
		tt.tt_availmodes |= WWM_REV;
	if (gen_US)
		tt.tt_availmodes |= WWM_UL;
	tt.tt_wrap = gen_AM;
	tt.tt_ncol = gen_CO;
	tt.tt_nrow = gen_LI;
	tt.tt_init = gen_init;
	tt.tt_end = gen_end;
	tt.tt_setmodes = gen_setmodes;
	tt.tt_blank = gen_blank;
	tt.tt_write = gen_write;
	tt.tt_putc = gen_putc;
	tt.tt_move = gen_move;
	tt.tt_frame = gen_frame;
	return 0;
}
