#ifndef lint
static	char *sccsid = "@(#)parser4.c	3.1 84/01/12";
#endif

#include "parser.h"

/*
 * |		3
 * ^		4
 * &		5
 * == !=	6
 * < <= > >=	7
 * << >>	8
 * + -		9
 * * / %	10
 */
p_expr3_10(level, v, flag)
register struct value *v;
char flag;
{
	struct value t;
	int op;
	char *opname;

	if (level == 10) {
		if (p_expr11(v, flag) < 0)
			return -1;
	} else {
		if (p_expr3_10(level + 1, v, flag) < 0)
			return -1;
	}
	for (;;) {
		switch (level) {
		case 3:
			if (token != T_OR)
				return 0;
			opname = "|";
			break;
		case 4:
			if (token != T_XOR)
				return 0;
			opname = "^";
			break;
		case 5:
			if (token != T_AND)
				return 0;
			opname = "&";
			break;
		case 6:
			if (token == T_EQ)
				opname = "==";
			else if (token == T_NE)
				opname = "!=";
			else
				return 0;
			break;
		case 7:
			switch (token) {
			case T_LT:
				opname = "<";
				break;
			case T_LE:
				opname = "<=";
				break;
			case T_GT:
				opname = ">";
				break;
			case T_GE:
				opname = ">=";
				break;
			default:
				return 0;
			}
			break;
		case 8:
			if (token == T_LS)
				opname = "<<";
			else if (token == T_RS)
				opname = ">>";
			else
				return 0;
			break;
		case 9:
			if (token == T_PLUS)
				opname = "+";
			else if (token == T_MINUS)
				opname = "-";
			else
				return 0;
			break;
		case 10:
			switch (token) {
			case T_MUL:
				opname = "*";
				break;
			case T_DIV:
				opname = "/";
				break;
			case T_MOD:
				opname = "%";
				break;
			default:
				return 0;
			}
			break;
		}
		if (v->v_type == V_ERR)
			flag = 0;

		op = token;
		(void) s_gettok();
		if (level == 10) {
			if (p_expr11(&t, flag) < 0) {
				p_synerror();
				val_free(*v);
				return -1;
			}
		} else {
			if (p_expr3_10(level + 1, &t, flag) < 0) {
				p_synerror();
				val_free(*v);
				return -1;
			}
		}

		if (t.v_type == V_ERR)
			flag = 0;
		else if (t.v_type != v->v_type) {
			p_error("Type mismatch.");
			flag = 0;
		} else switch (op) {
		case T_EQ:
		case T_NE:
		case T_LT:
		case T_LE:
		case T_GT:
		case T_GE:
			if (v->v_type == V_STR) {
				int tmp;
				tmp = strcmp(v->v_str, t.v_str);
				str_free(v->v_str);
				str_free(t.v_str);
				v->v_type = V_NUM;
				v->v_num = tmp;
				t.v_num = 0;
			}
			break;
		default:
			if (v->v_type == V_STR) {
				p_error("Numeric value required for %s.",
					opname);
				flag = 0;
			}
		}

		if (!flag) {
			val_free(*v);
			val_free(t);
			v->v_type = V_ERR;
			continue;
		}

		switch (op) {
		case T_OR:
			v->v_num |= t.v_num;
			break;
		case T_XOR:
			v->v_num ^= t.v_num;
			break;
		case T_AND:
			v->v_num &= t.v_num;
			break;
		case T_EQ:
			v->v_num = v->v_num == t.v_num;
			break;
		case T_NE:
			v->v_num = v->v_num != t.v_num;
			break;
		case T_LT:
			v->v_num = v->v_num < t.v_num;
			break;
		case T_LE:
			v->v_num = v->v_num <= t.v_num;
			break;
		case T_GT:
			v->v_num = v->v_num > t.v_num;
			break;
		case T_GE:
			v->v_num = v->v_num >= t.v_num;
			break;
		case T_LS:
			v->v_num <<= t.v_num;
			break;
		case T_RS:
			v->v_num >>= t.v_num;
			break;
		case T_PLUS:
			v->v_num += t.v_num;
			break;
		case T_MINUS:
			v->v_num -= t.v_num;
			break;
		case T_MUL:
			v->v_num *= t.v_num;
			break;
		case T_DIV:
			v->v_num /= t.v_num;
			break;
		case T_MOD:
			v->v_num %= t.v_num;
			break;
		}
	}
	/*NOTREACHED*/
}
