#ifndef lint
static	char *sccsid = "@(#)parser2.c	3.1 84/01/12";
#endif

#include "parser.h"

/*
 * name == 0 means we don't have a function name but
 * want to parse the arguments anyway.  flag == 0 in this case.
 */
p_function(name, v, flag)
char *name;
register struct value *v;
{
	struct value t;
	register struct lcmd_tab *c;
	register struct lcmd_arg *ap;
	register i;

	if (name != 0) {
		if ((c = lcmd_lookup(name)) == 0) {
			p_error("%s: No such command.", name);
			flag = 0;
		}
	} else
		c = 0;

	if (c != 0)
		for (ap = c->lc_arg; ap->arg_name != 0; ap++)
			ap->arg_val.v_type = V_ERR;

	for (i = 0;;) {
		ap = 0;
		if (p_expr0(&t, flag) < 0)
			if (!p_synerred() && token == T_MUL) {
				if (c != 0)
					if (c->lc_arg[i].arg_name == 0)
						p_error("%s: Too many arguments.", c->lc_name);
					else
						i++;
				(void) s_gettok();
				continue;
			} else
				break;
		if (t.v_type == V_ERR)
			flag = 0;
		if (token != T_ASSIGN) {
			if (c != 0) {
				ap = &c->lc_arg[i];
				if (ap->arg_name == 0) {
					p_error("%s: Too many arguments.",
						c->lc_name);
					val_free(t);
					ap = 0;
					flag = 0;
				} else
					i++;
			}
		} else {
			char *tmp;
			switch (t.v_type) {
			case V_ERR:
				tmp = 0;
				break;
			case V_NUM:
				if ((tmp = str_itoa(t.v_num)) == 0) {
					p_memerror();
					goto abort;
				}
				break;
			case V_STR:
				tmp = t.v_str;
				break;
			}
			(void) s_gettok();
			if (p_expr(&t, flag) < 0) {
				if (tmp)
					str_free(tmp);
				p_synerror();
				goto abort;
			}
			if (t.v_type == V_ERR)
				flag = 0;
			if (tmp) {
				/* we know c != 0 */
				for (ap = c->lc_arg; ap->arg_name != 0; ap++)
					if (str_match(tmp, ap->arg_name,
							ap->arg_minlen))
						break;
				if (ap->arg_name == 0) {
					p_error("%s: Unknown argument \"%s\".",
						c->lc_name, tmp);
					val_free(t);
					flag = 0;
					ap = 0;
				}
				str_free(tmp);
			}
		}
		if (ap != 0) {
			if (ap->arg_val.v_type != V_ERR) {
				p_error("%s: Argument %d (%s) duplicated.",
					c->lc_name, ap - c->lc_arg + 1,
					ap->arg_name);
				val_free(t);
				flag = 0;
			} else if (t.v_type == V_ERR) {
				/* do nothing */
			} else if (ap->arg_type == ARG_NUM && t.v_type != V_NUM
			    || ap->arg_type == ARG_STR && t.v_type != V_STR) {
				p_error("%s: Argument %d (%s) type mismatch.",
					c->lc_name, ap - c->lc_arg + 1,
					ap->arg_name);
				val_free(t);
				flag = 0;
			} else
				ap->arg_val = t;
		}
		if (token == T_COMMA)
			(void) s_gettok();
	}

	if (p_erred())
		flag = 0;
	if (token != T_RP && token != T_EOL && token != T_EOF)
		flag = 0;		/* look ahead a bit */
	v->v_type = V_ERR;
	if (flag)
		(*c->lc_func)(v);
	if (c != 0)
		for (ap = c->lc_arg; ap->arg_name != 0; ap++)
			val_free(ap->arg_val);
	return 0;
abort:
	if (c != 0)
		for (ap = c->lc_arg; ap->arg_name != 0; ap++)
			val_free(ap->arg_val);
	return -1;
}

p_assign(name, v, flag)
char *name;
struct value *v;
char flag;
{
	(void) s_gettok();

	if (p_expr(v, flag) < 0) {
		p_synerror();
		return -1;
	}
	switch (v->v_type) {
	case V_STR:
	case V_NUM:
		if (flag && var_set(name, v) == 0) {
			p_memerror();
			val_free(*v);
			return -1;
		}
		break;
	}
	return 0;
}
