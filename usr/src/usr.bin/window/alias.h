/*
 *	@(#)alias.h	3.1 84/05/06
 */

#define alias var
#define a_name r_name
#define a_buf r_val.v_str

#define alias_set(n, s)		var_setstr1(&alias_head, n, s)
#define alias_walk(f, a)	var_walk1(alias_head, f, a)
#define alias_unset(n)		var_unset1(&alias_head, n)
#define alias_lookup(n)		(*var_lookup1(&alias_head, n))

struct var *alias_head;
