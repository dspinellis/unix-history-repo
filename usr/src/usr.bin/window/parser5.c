#ifndef lint
static	char *sccsid = "@(#)parser5.c	3.1 84/01/12";
#endif

#include "parser.h"

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
