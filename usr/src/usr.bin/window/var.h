/*
 *	@(#)var.h	3.1 83/11/22
 */

struct var {
	struct var *r_left;
	struct var *r_right;
	char *r_name;
	struct value r_val;
};

struct var *var_set();
struct var *var_lookup();

struct var *var_head;		/* secret, shhh */
