/*
 * @(#)token.h	3.4 4/24/85
 */

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#define token		(cx.x_token)
#define token_num	(cx.x_val.v_num)
#define token_str	(cx.x_val.v_str)

#define T_EOL		1
#define T_EOF		2
#define T_COMP		3
#define T_PLUS		4
#define T_MINUS		5
#define T_MUL		6
#define T_DIV		7
#define T_LP		8
#define T_RP		9
#define T_DOLLAR	10
#define T_COMMA		11
#define T_QUEST		12
#define T_COLON		13
#define T_CHAR		14
#define T_STR		15
#define T_NUM		16
#define T_MOD		17
#define T_XOR		18
#define T_DQ		19		/* $? */
#define T_GE		20
#define T_RS		21
#define T_GT		22
#define T_LE		23
#define T_LS		24
#define T_LT		25
#define T_EQ		26
#define T_ASSIGN	27
#define T_NE		28
#define T_NOT		29
#define T_ANDAND	30
#define T_AND		31
#define T_OROR		32
#define T_OR		33
#define T_IF		34
#define T_THEN		35
#define T_ELSIF		36
#define T_ELSE		37
#define T_ENDIF		38
