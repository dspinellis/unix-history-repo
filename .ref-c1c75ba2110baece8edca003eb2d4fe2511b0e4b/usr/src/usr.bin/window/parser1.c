/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)parser1.c	3.20 (Berkeley) %G%";
#endif /* not lint */

#include "parser.h"

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
	switch (token) {
	case T_EOL:
		(void) s_gettok();
		return 0;
	case T_IF:
		return p_if(flag);
	default:
		return p_expression(flag);
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
		p_error("if: Numeric value required.");
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

p_expression(flag)
char flag;
{
	struct value t;
	char *cmd;
	int p_function(), p_assign();

	switch (token) {
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
			val_free(t);
			return 0;
		}
	}
	if (token != T_ASSIGN && p_convstr(&t) < 0)
		return -1;
	cmd = t.v_type == V_STR ? t.v_str : 0;
	if ((*(token == T_ASSIGN ? p_assign : p_function))(cmd, &t, flag) < 0) {
		if (cmd)
			str_free(cmd);
		return -1;
	}
	if (cmd)
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

p_convstr(v)
register struct value *v;
{
	if (v->v_type != V_NUM)
		return 0;
	if ((v->v_str = str_itoa(v->v_num)) == 0) {
		p_memerror();
		v->v_type = V_ERR;
		return -1;
	}
	v->v_type = V_STR;
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
