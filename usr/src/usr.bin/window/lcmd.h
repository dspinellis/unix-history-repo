/*
 *	@(#)lcmd.h	3.2 84/03/23
 */

#define LCMD_NARG 20			/* maximum number of arguments */

struct lcmd_tab {
	char *lc_name;
	int lc_minlen;
	int (*lc_func)();
	struct lcmd_arg *lc_arg;
};

struct lcmd_arg {
	char *arg_name;
	int arg_minlen;
	char arg_type;
};

#define ARG_ANY 0
#define ARG_NUM 1
#define ARG_STR 2

struct lcmd_tab *lcmd_lookup();
