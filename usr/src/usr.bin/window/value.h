/*
 *	@(#)value.h	3.1 83/11/22
 */

struct value {
	char v_type;
	struct value *v_link;
	union {
		int V_num;
		char *V_str;
		int V_tok;
	} v_un;
};
#define v_num	v_un.V_num
#define v_str	v_un.V_str
#define v_tok   v_un.V_tok

#define V_NUM	1
#define V_STR	2
#define V_TOK	3
#define V_ERR	4
