/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)parser3.c	3.9 (Berkeley) %G%";
#endif /* not lint */

#include "parser.h"

/*
 * =
 * ? :
 * ||
 * &&
 * |
 * ^
 * &
 * == !=
 * <= >=
 * << >>
 * + -
 * * / %
 * unary - + ~ !
 */
p_expr(v, flag)
register struct value *v;
char flag;
{
	struct value t;
	int ret;

	if (p_expr0(&t, flag) < 0)
		return -1;

	if (token != T_ASSIGN) {
		*v = t;
		return 0;
	}
	switch (t.v_type) {
	case V_NUM:
		p_error("%d: Not a variable.", t.v_num);
	case V_ERR:
		t.v_str = 0;
		break;
	}
	ret = p_assign(t.v_str, v, flag);
	if (t.v_str != 0)
		str_free(t.v_str);
	return ret;
}

/*
 * ? :
 */
p_expr0(v, flag)
register struct value *v;
char flag;
{
	struct value t;
	char true;

	if (p_expr1(v, flag) < 0)
		return -1;
	if (token != T_QUEST)
		return 0;
	switch (v->v_type) {
	case V_NUM:
		true = v->v_num != 0;
		break;
	case V_STR:
		p_error("?: Numeric left operand required.");
		str_free(v->v_str);
		v->v_type = V_ERR;
	case V_ERR:
		flag = 0;
		break;
	}
	(void) s_gettok();
	v->v_type = V_ERR;
	if ((flag && true ? p_expr1(v, 1) : p_expr1(&t, 0)) < 0)
		return -1;
	if (token != T_COLON) {
		val_free(*v);
		p_synerror();
		return -1;
	}
	(void) s_gettok();
	return flag && !true ? p_expr1(v, 1) : p_expr1(&t, 0);
}

/*
 * ||
 */
p_expr1(v, flag)
register struct value *v;
char flag;
{
	char true = 0;

	if (p_expr2(v, flag) < 0)
		return -1;
	if (token != T_OROR)
		return 0;
	for (;;) {
		switch (v->v_type) {
		case V_NUM:
			v->v_num = true = true || v->v_num != 0;
			break;
		case V_STR:
			p_error("||: Numeric operands required.");
			str_free(v->v_str);
			v->v_type = V_ERR;
		case V_ERR:
			flag = 0;
			break;
		}
		if (token != T_OROR)
			return 0;
		(void) s_gettok();
		if (p_expr2(v, flag && !true) < 0)
			return -1;
	}
}

/*
 * &&
 */
p_expr2(v, flag)
register struct value *v;
char flag;
{
	char true = 1;

	if (p_expr3_10(3, v, flag) < 0)
		return -1;
	if (token != T_ANDAND)
		return 0;
	for (;;) {
		switch (v->v_type) {
		case V_NUM:
			v->v_num = true = true && v->v_num != 0;
			break;
		case V_STR:
			p_error("&&: Numeric operands required.");
			str_free(v->v_str);
			v->v_type = V_ERR;
		case V_ERR:
			flag = 0;
			break;
		}
		if (token != T_ANDAND)
			return 0;
		(void) s_gettok();
		if (p_expr3_10(3, v, flag && true) < 0)
			return -1;
	}
	/*NOTREACHED*/
}
