/*
 *	@(#)tt.h	3.8 83/09/15
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

		/* the frame characters */
	char *tt_frame;
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
struct tt_tab tt_tab[];

/*
 * Clean interface to termcap routines.
 */
char tt_strings[1024];		/* string buffer */
char *tt_strp;			/* pointer for it */

int tt_pc();			/* just putchar() */
int tt_sc();			/* *tt_strp++ = c */
char *tt_xgetstr();		/* tgetstr() and expand delays */

#define tt_tgetstr(s)	tgetstr((s), &tt_strp)
#define tt_tputs(s, n)	tputs((s), (n), tt_pc)
