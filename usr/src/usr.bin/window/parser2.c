#ifndef lint
static	char *sccsid = "@(#)parser2.c	3.4 84/04/06";
#endif

#include "parser.h"
#include "var.h"
#include "lcmd.h"

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
	struct value av[LCMD_NARG + 1];
	register struct value *vp;

	if (name != 0) {
		if ((c = lcmd_lookup(name)) == 0) {
			p_error("%s: No such command.", name);
			flag = 0;
		}
	} else
		c = 0;

	if (c != 0)
		for (vp = av; vp < &av[LCMD_NARG + 1]; vp++)
			vp->v_type = V_ERR;

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
			if (c != 0)
				if (i >= LCMD_NARG || c->lc_arg != 0
				    && (ap = c->lc_arg + i)->arg_name == 0) {
					p_error("%s: Too many arguments.",
						c->lc_name);
					ap = 0;
					vp = 0;
					flag = 0;
				} else
					vp = &av[i++];
		} else {
			char *tmp;
			switch (t.v_type) {
			case V_ERR:
				tmp = 0;
				break;
			case V_NUM:
				if (p_convstr(&t) < 0)
					goto abort;
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
				for (ap = c->lc_arg, vp = av;
				     ap != 0 && ap->arg_name != 0; ap++, vp++)
					if (str_match(tmp, ap->arg_name,
							ap->arg_minlen))
						break;
				if (ap == 0 || ap->arg_name == 0) {
					p_error("%s: Unknown argument \"%s\".",
						c->lc_name, tmp);
					flag = 0;
					ap = 0;
					vp = 0;
				}
				str_free(tmp);
			}
		}
		if (ap != 0) {
			if (vp->v_type != V_ERR) {
				p_error("%s: Argument %d (%s) duplicated.",
					c->lc_name, ap - c->lc_arg + 1,
					ap->arg_name);
				flag = 0;
				vp = 0;
			} else if (t.v_type == V_ERR) {
				/* do nothing */
			} else if (ap->arg_type == ARG_NUM && t.v_type != V_NUM
			    || ap->arg_type == ARG_STR && t.v_type != V_STR) {
				p_error("%s: Argument %d (%s) type mismatch.",
					c->lc_name, ap - c->lc_arg + 1,
					ap->arg_name);
				flag = 0;
				vp = 0;
			}
		}
		if (vp != 0)
			*vp = t;
		else
			val_free(t);
		if (token == T_COMMA)
			(void) s_gettok();
	}

	if (p_erred())
		flag = 0;
	if (token != T_RP && token != T_EOL && token != T_EOF)
		flag = 0;		/* look for legal follow set */
	v->v_type = V_ERR;
	if (flag)
		(*c->lc_func)(v, av);
	if (c != 0)
		for (vp = av; vp < &av[LCMD_NARG]; vp++)
			val_free(*vp);
	return 0;
abort:
	if (c != 0)
		for (vp = av; vp < &av[LCMD_NARG]; vp++)
			val_free(*vp);
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
