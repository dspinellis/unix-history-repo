#ifndef lint
static	char *sccsid = "@(#)parser1.c	3.9 84/01/12";
#endif

#include <stdio.h>
#include "value.h"
#include "context.h"
#include "token.h"
#include "string.h"
#include "lcmd.h"
#include "var.h"

#define p_erred()	(cx.x_erred)
#define p_synerred()	(cx.x_synerred)
#define p_clearerr()	(cx.x_erred = cx.x_synerred = 0)
#define p_abort()	(cx.x_abort)

p_start()
{
	char flag = 1;

	(void) s_gettok();
	for (;;) {
		p_statementlist(flag);
		if (token == T_EOF || p_abort())
			break;
		flag = 0;
		p_synerror();
		while (token != T_EOL && token != T_EOF) {
			if (token == T_STR)
				str_free(token_str);
			(void) s_gettok();
		}
		if (token == T_EOL)
			(void) s_gettok();
		p_clearerr();
	}
}

p_statementlist(flag)
char flag;
{
	for (; p_statement(flag) >= 0; p_clearerr())
		;
}

p_statement(flag)
char flag;
{
#ifdef DEBUG
	error("statement: %d.", flag);
#endif
	switch (token) {
	case T_EOL:
#ifdef DEBUG
		error("statement: EOL.", flag);
#endif
		(void) s_gettok();
		return 0;
	case T_IF:
#ifdef DEBUG
		error("statement: IF.", flag);
#endif
		return p_if(flag);
	default:
#ifdef DEBUG
		error("statement: command.", flag);
#endif
		return p_command(flag);
	}
}

p_if(flag)
char flag;
{
	struct value t;
	char true = 0;

top:
	(void) s_gettok();

	if (p_expr(&t, flag) < 0) {
		p_synerror();
		return -1;
	}
	switch (t.v_type) {
	case V_NUM:
		true = !true && t.v_num != 0;
		break;
	case V_STR:
		p_error("Numeric value required for if.");
		str_free(t.v_str);
	case V_ERR:
		flag = 0;
		break;
	}

	if (token != T_THEN) {
		p_synerror();
		return -1;
	}

	(void) s_gettok();
	p_statementlist(flag && true);
	if (p_erred())
		return -1;

	if (token == T_ELSIF)
		goto top;

	if (token == T_ELSE) {
		(void) s_gettok();
		p_statementlist(flag && !true);
		if (p_erred())
			return -1;
	}

	if (token == T_ENDIF) {
		(void) s_gettok();
		return 0;
	}

	p_synerror();
	return -1;
}

p_command(flag)
char flag;
{
	struct value t;
	char *cmd;

#ifdef DEBUG
	error("command: %d.", flag);
#endif
	switch (token) {
	case T_MOD:
		t.v_type = V_STR;
		t.v_str = str_cpy("%");
		(void) s_gettok();
		break;
	case T_NUM:
		t.v_type = V_NUM;
		t.v_num = token_num;
		(void) s_gettok();
		break;
	case T_STR:
		t.v_type = V_STR;
		t.v_str = token_str;
		(void) s_gettok();
		break;
	default:
		if (p_expr(&t, flag) < 0)
			return -1;
		if (token == T_EOF) {
#ifdef DEBUG
			error("command: expression.");
#endif
			val_free(t);
			return 0;
		}
	}
	switch (t.v_type) {
	case V_ERR:
		cmd = 0;
		break;
	case V_STR:
		cmd = t.v_str;
		break;
	case V_NUM:
		if ((cmd = str_itoa(t.v_num)) == 0) {
			p_memerror();
			return -1;
		}
	}
	if (token == T_ASSIGN) {
#ifdef DEBUG
		error("command: assignment %s.", cmd == 0 ? "ERR" : cmd);
#endif
		if (p_assign(cmd, &t, flag) < 0) {
			str_free(cmd);
			return -1;
		}
	} else {
#ifdef DEBUG
		error("command: function %s.", cmd == 0 ? "ERR" : cmd);
#endif
		if (p_function(cmd, &t, flag) < 0) {
			str_free(cmd);
			return -1;
		}
	}
	str_free(cmd);
	val_free(t);
	if (token == T_EOL)
		(void) s_gettok();
	else if (token != T_EOF) {
		p_synerror();
		return -1;
	}
	return 0;
}

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
				if (c->lc_arg[i].arg_name == 0)
					p_error("%s: Too many arguments.",
						c->lc_name);
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

#ifdef DEBUG
	error("expr: %d.", flag);
#endif
	if (p_expr0(&t, flag) < 0)
		return -1;

	if (token != T_ASSIGN) {
		*v = t;
		return 0;
	}
	switch (t.v_type) {
	case V_ERR:
		t.v_str = 0;
		break;
	case V_NUM:
		if ((t.v_str = str_itoa(t.v_num)) == 0) {
			p_memerror();
			return -1;
		}
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
		p_error("Numeric value required for ?.");
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
			p_error("Numeric value required for ||.");
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
			p_error("Numeric value required for &&.");
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

/*
 * unary $ + - ! ~
 */
p_expr11(v, flag)
register struct value *v;
char flag;
{
	int op;
	char *opname;

	switch (token) {
	case T_DOLLAR:
		opname = "$";
		break;
	case T_PLUS:
		opname = "unary +";
		break;
	case T_MINUS:
		opname = "unary -";
		break;
	case T_NOT:
		opname = "!";
		break;
	case T_COMP:
		opname = "~";
		break;
	default:
		return p_expr12(v, flag);
	}
	op = token;
	(void) s_gettok();
	if (p_expr11(v, flag) < 0)
		return -1;
	switch (v->v_type) {
	case V_NUM:
		if (op == T_DOLLAR && (v->v_str = str_itoa(v->v_num)) == 0) {
			p_memerror();
			return -1;
		}
		break;
	case V_STR:
		switch (op) {
		case T_MINUS:
		case T_NOT:
		case T_COMP:
			p_error("Numeric value required for %s.", opname);
			str_free(v->v_str);
			v->v_type = V_ERR;
			return 0;
		}
		break;
	case V_ERR:
		return 0;
	}
	switch (op) {
	case T_DOLLAR: {
		struct var *r;
		if ((r = var_lookup(v->v_str)) == 0) {
			p_error("%s: Undefined variable.", v->v_str);
			str_free(v->v_str);
			v->v_type = V_ERR;
			return 0;
		}
		str_free(v->v_str);
		if (flag) {
			*v = r->r_val;
			if (v->v_type == V_STR
			    && (v->v_str = str_cpy(v->v_str)) == 0) {
				p_memerror();
				return -1;
			}
		}
		break;
		}
	case T_MINUS:
		v->v_num = - v->v_num;
		break;
	case T_NOT:
		v->v_num = ! v->v_num;
		break;
	case T_COMP:
		v->v_num = ~ v->v_num;
		break;
	}
	return 0;
}

/*
 * string, number, ( expr )
 * Plus function calls.
 * Also we map % into string.
 *
 * Always return v_type == V_ERR when flag == 0.
 */
p_expr12(v, flag)
register struct value *v;
char flag;
{
	v->v_type = V_ERR;
#ifdef DEBUG
	error("expr12: %d.", flag);
#endif
	switch (token) {
	case T_MOD:
#ifdef DEBUG
		error("expr12: %.");
#endif
		if (flag) {
			v->v_type = V_STR;
			v->v_str = str_cpy("%");
		}
		(void) s_gettok();
		break;
	case T_NUM:
#ifdef DEBUG
		error("expr12: NUM %d.", token_num);
#endif
		if (flag) {
			v->v_type = V_NUM;
			v->v_num = token_num;
		}
		(void) s_gettok();
		break;
	case T_STR:
#ifdef DEBUG
		error("expr12: STR %s.", token_str);
#endif
		if (flag) {
			v->v_type = V_STR;
			v->v_str = token_str;
		} else
			str_free(token_str);
		(void) s_gettok();
		break;
	case T_LP:
		(void) s_gettok();
		if (p_expr(v, flag) < 0) {
			p_synerror();
			return -1;
		}
		if (token != T_RP) {
			p_synerror();
			val_free(*v);
			return -1;
		}
		(void) s_gettok();
		break;
	default:
#ifdef DEBUG
		error("expr12: token %d.", token);
#endif
		return -1;
	}
	while (token == T_LP) {
		char *cmd;

		(void) s_gettok();
		switch (v->v_type) {
		case V_STR:
			cmd = v->v_str;
			break;
		case V_ERR:
			flag = 0;
			cmd = 0;
			break;
		case V_NUM:
			if ((cmd = str_itoa(v->v_num)) == 0) {
				p_memerror();
				return -1;
			}
			break;
		}
#ifdef DEBUG
		error("expr12: function %s.", cmd);
#endif
		if (p_function(cmd, v, flag) < 0) {
			str_free(cmd);
			return -1;
		}
		str_free(cmd);
		if (token != T_RP) {
			p_synerror();
			val_free(*v);
			return -1;
		}
		(void) s_gettok();
	}
	return 0;
}

p_synerror()
{
	if (!cx.x_synerred) {
		cx.x_synerred = cx.x_erred = 1;
		error("Syntax error.");
	}
}

/*VARARGS1*/
p_error(msg, a, b, c)
char *msg;
{
	if (!cx.x_erred) {
		cx.x_erred = 1;
		error(msg, a, b, c);
	}
}

p_memerror()
{
	cx.x_erred = cx.x_abort = 1;
	error("Out of memory.");
}
