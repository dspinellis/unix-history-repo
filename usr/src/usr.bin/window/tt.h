/*
 *	@(#)tt.h	3.6 83/08/17
 */

struct tt {
	int (*tt_init)();
	int (*tt_end)();
	int (*tt_setinsert)();
	int (*tt_setmodes)();
	int (*tt_move)();
	int (*tt_insline)();
	int (*tt_delline)();
	int (*tt_delchar)();
	int (*tt_write)();
	int (*tt_putc)();
	int (*tt_blank)();
	int (*tt_clreol)();
	int (*tt_clreos)();
	int (*tt_clear)();
	int tt_nrow;
	int tt_ncol;
	char tt_availmodes;
	char tt_wrap;			/* has auto wrap around */
	char *tt_frame;
};

struct tt tt;

struct tt_tab {
	char *tt_name;
	int tt_len;
	int (*tt_func)();
};

struct tt_tab tt_tab[];

/*
 * nicer interface to termcap routines
 */
char tt_strings[1024];		/* string buffer */
char *tt_strp;			/* pointer for it */

int tt_pc();			/* just putchar() */
int tt_sc();			/* *tt_strp++ = c */
char *tt_xgetstr();		/* tgetstr() and expand delays */

#define tt_tgetstr(s)	tgetstr((s), &tt_strp)
#define tt_tputs(s, n)	tputs((s), (n), tt_pc)
