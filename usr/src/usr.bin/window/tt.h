/*
 * @(#)tt.h	3.17 %G%
 */

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

/*
 * Interface structure for the terminal drivers.
 */
struct tt {
		/* startup and cleanup */
	int (*tt_init)();
	int (*tt_end)();

		/* terminal functions */
	int (*tt_move)();
	int (*tt_insline)();
	int (*tt_delline)();
	int (*tt_delchar)();
	int (*tt_write)();		/* write a whole block */
	int (*tt_putc)();		/* write one character */
	int (*tt_clreol)();
	int (*tt_clreos)();
	int (*tt_clear)();
	int (*tt_setinsert)();		/* set insert mode */
	int (*tt_setmodes)();		/* set display modes */

		/* internal variables */
	char tt_modes;			/* the current display modes */
	char tt_nmodes;			/* the new modes for next write */
	char tt_insert;			/* currently in insert mode */
	char tt_ninsert;		/* insert mode on next write */
	int tt_row;			/* cursor row */
	int tt_col;			/* cursor column */

		/* terminal info */
	int tt_nrow;			/* number of display rows */
	int tt_ncol;			/* number of display columns */
	char tt_hasinsert;		/* has insert character */
	char tt_availmodes;		/* the display modes supported */
	char tt_wrap;			/* has auto wrap around */
	char tt_retain;			/* can retain below (db flag) */
	char tt_noscroll;		/* terminal doesn't scroll (ns flag) */

		/* the frame characters */
	short *tt_frame;
};
struct tt tt;

/*
 * List of terminal drivers.
 */
struct tt_tab {
	char *tt_name;
	int tt_len;
	int (*tt_func)();
};
extern struct tt_tab tt_tab[];

/*
 * Clean interface to termcap routines.
 * Too may t's.
 */
char tt_strings[1024];		/* string buffer */
char *tt_strp;			/* pointer for it */

struct tt_str {
	char *ts_str;
	int ts_n;
};

struct tt_str *tttgetstr();
struct tt_str *ttxgetstr();	/* tgetstr() and expand delays */

int tttputc();
#define tttputs(s, n)	tputs((s)->ts_str, (n), tttputc)
#define ttxputs(s)	ttwrite((s)->ts_str, (s)->ts_n)

/*
 * Buffered output without stdio.
 * These variables have different meanings from the ww_ob* variabels.
 * But I'm too lazy to think up different names.
 */
char tt_ob[512];
char *tt_obp;
char *tt_obe;
#define ttputc(c)	(tt_obp < tt_obe ? (*tt_obp++ = (c)) \
				: (ttflush(), *tt_obp++ = (c)))
