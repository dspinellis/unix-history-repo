#ifndef lint
static	char *sccsid = "@(#)parser2.c	3.5 84/05/06";
#endif

#include "parser.h"
#include "var.h"
#include "lcmd.h"
#include "alias.h"

/*
 * name == 0 means we don't have a function name but
 * want to parse the arguments anyway.  flag == 0 in this case.
 */
p_function(name, v, flag)
char *name;
register struct value *v;
{
	struct value t;
	register struct lcmd_tab *c = 0;
	register struct alias *a = 0;
	register struct lcmd_arg *ap;
	register i;
	struct value av[LCMD_NARG + 1];
	register struct value *vp;

	if (name != 0)
		if (c = lcmd_lookup(name))
			name = c->lc_name;
		else if (a = alias_lookup(name))
			name = a->a_name;
		else {
			p_error("%s: No such command or alias.", name);
			flag = 0;
		}

	for (vp = av; vp < &av[LCMD_NARG + 1]; vp++)
		vp->v_type = V_ERR;

	for (i = 0;;) {
		ap = 0;
		vp = 0;
		if (p_expr0(&t, flag) < 0)
			if (!p_synerred() && token == T_MUL) {
				if (c != 0)
					if (c->lc_arg[i].arg_name == 0)
						p_error("%s: Too many arguments.", name);
					else
						i++;
				(void) s_gettok();
				continue;
			} else
				break;
		if (t.v_type == V_ERR)
			flag = 0;
		if (token != T_ASSIGN) {
			if (i >= LCMD_NARG || c != 0 && c->lc_arg != 0
			    && (ap = c->lc_arg + i)->arg_name == 0) {
				p_error("%s: Too many arguments.", name);
				flag = 0;
			} else
				vp = &av[i++];
		} else {
			char *tmp;
			if (p_convstr(&t) < 0)
				goto abort;
			tmp = t.v_type == V_STR ? t.v_str : 0;
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
				if (c == 0) {
					/* an aliase */
					p_error("%s: Bad alias syntax.", name);
					flag = 0;
				} else {
					for (ap = c->lc_arg, vp = av;
					     ap != 0 && ap->arg_name != 0 &&
						!str_match(tmp, ap->arg_name,
							ap->arg_minlen);
					     ap++, vp++)
						;
					if (ap == 0 || ap->arg_name == 0) {
						p_error("%s: Unknown argument \"%s\".",
							name, tmp);
						flag = 0;
						ap = 0;
						vp = 0;
					}
				}
				str_free(tmp);
			}
		}
		if (ap != 0) {
			if (vp->v_type != V_ERR) {
				p_error("%s: Argument %d (%s) duplicated.",
					name, ap - c->lc_arg + 1,
					ap->arg_name);
				flag = 0;
				vp = 0;
			} else if (t.v_type == V_ERR) {
				/* do nothing */
			} else if (ap->arg_type == ARG_NUM && t.v_type != V_NUM
			    || ap->arg_type == ARG_STR && t.v_type != V_STR) {
				p_error("%s: Argument %d (%s) type mismatch.",
					name, ap - c->lc_arg + 1,
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
		if (c != 0)
			(*c->lc_func)(v, av);
		else
			if (dolongcmd(a->a_buf, av, i) < 0)
				p_memerror();
	if (p_abort()) {
		val_free(*v);
		v->v_type = V_ERR;
		goto abort;
	}
	for (vp = av; vp < &av[LCMD_NARG]; vp++)
		val_free(*vp);
	return 0;
abort:
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
