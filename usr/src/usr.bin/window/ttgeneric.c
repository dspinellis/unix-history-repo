#ifndef lint
static	char *sccsid = "@(#)ttgeneric.c	3.6 83/08/15";
#endif

#include "ww.h"
#include "tt.h"

char *tgetstr();
char *tgoto();

char gen_frame[16] = {
	' ', '|', '-', '+',
	'|', '|', '+', '+',
	'-', '+', '-', '+',
	'+', '+', '+', '+'
};

int gen_row, gen_col;
char gen_modes;
char gen_availmodes;
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
char gen_strings[1024];
char *gen_strp = gen_strings;

#define pc(c) putchar('c')
#define ps(s) fputs((s), stdout)

gen_pc(c)
{
	putchar(c);
}

gen_sc(c)
{
	*gen_strp++ = c;
}

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

	new &= gen_availmodes;
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
		tputs(gen_AL, gen_LI - gen_row, gen_pc);
}

gen_delline()
{
	if (gen_DL)
		tputs(gen_DL, gen_LI - gen_row, gen_pc);
}

gen_putc(c)
register char c;
{
	if (gen_insert) {
		if (gen_IC)
			tputs(gen_IC, gen_CO - gen_col, gen_pc);
		putchar(c);
		if (gen_IP)
			tputs(gen_IP, gen_CO - gen_col, gen_pc);
	} else
		putchar(c);
	gen_col++;
}

gen_write(start, end)
register char *start, *end;
{
	if (gen_insert) {
		while (start <= end) {
			if (gen_IC)
				tputs(gen_IC, gen_CO - gen_col, gen_pc);
			putchar(*start++);
			if (gen_IP)
				tputs(gen_IP, gen_CO - gen_col, gen_pc);
			gen_col++;
		}
	} else {
		gen_col += end - start + 1;
		while (start <= end)
			putchar(*start++);
	}
}

gen_blank(n)
register n;
{
	if (n <= 0)
		return;
	if (gen_insert) {
		while (--n >= 0) {
			if (gen_IC)
				tputs(gen_IC, gen_CO - gen_col, gen_pc);
			putchar(' ');
			if (gen_IP)
				tputs(gen_IP, gen_CO - gen_col, gen_pc);
			gen_col++;
		}
	} else {
		gen_col += n;
		while (--n >= 0)
			putchar(' ');
	}
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
	tputs(tgoto(gen_CM, col, row), 1, gen_pc);
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
		tputs(gen_CE, gen_CO - gen_col, gen_pc);
}

gen_clreos()
{
	if (gen_CD)
		tputs(gen_CD, gen_LI - gen_row, gen_pc);
}

gen_clear()
{
	if (gen_CL)
		ps(gen_CL);
}

gen_delchar()
{
	if (gen_DC)
		tputs(gen_DC, gen_CO - gen_col, gen_pc);
}

char *
gen_getstr(str)
char *str;
{
	char buf[100];
	char *bufp = buf;

	str = tgetstr(str, &bufp);
	if (str == 0)
		return 0;
	str = gen_strp;
	tputs(buf, 1, gen_sc);
	gen_sc(0);
	return str;
}

tt_generic()
{
	gen_CM = tgetstr("cm", &gen_strp);
	gen_IM = gen_getstr("im");
	gen_IC = tgetstr("ic", &gen_strp);
	gen_IP = tgetstr("ip", &gen_strp);
	gen_EI = gen_getstr("ei");
	gen_DC = tgetstr("dc", &gen_strp);
	gen_AL = tgetstr("al", &gen_strp);
	gen_DL = tgetstr("dl", &gen_strp);
	gen_CE = tgetstr("ce", &gen_strp);
	gen_CD = tgetstr("cd", &gen_strp);
	gen_CL = gen_getstr("cl");
	gen_VS = gen_getstr("vs");
	gen_VE = gen_getstr("ve");
	gen_SO = gen_getstr("so");
	gen_SE = gen_getstr("se");
	gen_US = gen_getstr("us");
	gen_UE = gen_getstr("ue");
	gen_UP = gen_getstr("up");
	gen_PC = tgetstr("pc", &gen_strp);
	gen_BC = gen_getstr("bc");
	gen_ND = gen_getstr("nd");
	gen_HO = gen_getstr("ho");
	gen_NL = gen_getstr("nl");
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

	if (gen_SO)
		gen_availmodes |= WWM_REV;
	if (gen_US)
		gen_availmodes |= WWM_UL;
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
	tt.tt_ncol = gen_CO;
	if (gen_AM)
		tt.tt_ncol--;
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
