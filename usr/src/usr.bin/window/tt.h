/*
 *	@(#)tt.h	3.2 83/08/11
 */

struct tt {
	int (*tt_init)();
	int (*tt_reset)();
	int (*tt_cleanup)();
	int (*tt_move)();
	int (*tt_setinsert)();
	int (*tt_setmodes)();
	int (*tt_insline)();
	int (*tt_delline)();
	int (*tt_delchar)();
	int (*tt_write)();
	int (*tt_putc)();
	int (*tt_blank)();
	int (*tt_clreol)();
	int (*tt_clreos)();
	int (*tt_clear)();
	int (*tt_window)();
	int (*tt_flash)();
	int tt_nrow;
	int tt_ncol;
	char *tt_frame;
};

struct tt tt;

struct tt_tab {
	char *tt_name;
	int tt_len;
	int (*tt_func)();
};

struct tt_tab tt_tab[];
