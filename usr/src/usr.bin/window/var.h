/*
 *	@(#)var.h	3.2 83/12/06
 */

struct var {
	struct var *r_left;
	struct var *r_right;
	char *r_name;
	struct value r_val;
};

struct var *var_set();
struct var *var_setstr();
struct var *var_setnum();
struct var *var_lookup();

#define var_walk(f) var_walk1(var_head, (f))

struct var *var_head;		/* secret, shhh */
