/*
 *	@(#)value.h	3.3 84/01/12
 */

struct value {
	char v_type;
	union {
		int V_num;
		char *V_str;
	} v_un;
};
#define v_num	v_un.V_num
#define v_str	v_un.V_str

#define V_NUM	1
#define V_STR	2
#define V_ERR	3

#define val_free(v)	((v).v_type == V_STR ? str_free((v).v_str) : 0)
