/*
 *	@(#)lcmd.h	3.1 83/11/22
 */

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
	struct value arg_val;
};
#define arg_num arg_val.v_num
#define arg_str arg_val.v_str
#define arg_vtype arg_val.v_type

#define ARG_ANY 0
#define ARG_NUM 1
#define ARG_STR 2

struct lcmd_tab *lcmd_lookup();
