#ifndef lint
static	char *sccsid = "@(#)var.c	3.1 83/11/22";
#endif

#include "value.h"
#include "var.h"
#include "string.h"

char *malloc();

struct var *
var_set(name, v)
char *name;
struct value *v;
{
	register struct var *r;

	if ((r = var_lookup(name)) == 0) {
		r = (struct var *) malloc(sizeof (struct var));
		if (r == 0)
			return 0;
		if ((r->r_name = str_cpy(name)) == 0) {
			free((char *) r);
			return 0;
		}
		var_add(r);
	}
	r->r_val = *v;
	if (v->v_type == V_STR) {
		if ((r->r_val.v_str = str_cpy(v->v_str)) == 0) {
			free((char *) r);
			str_free(r->r_name);
			return 0;
		}
	}
	return r;
}

struct var *
var_lookup(name)
char *name;
{
	register struct var *r;
	register cmp;

	for (r = var_head; r != 0;) {
		if ((cmp = strcmp(name, r->r_name)) < 0)
			r = r->r_left;
		else if (cmp > 0)
			r = r->r_right;
		else
			break;
	}
	return r;
}

var_walk(func)
int (*func)();
{
	var_walk1(var_head, func);
}

var_walk1(r, func)
register struct var *r;
int (*func)();
{
	if (r == 0)
		return;
	var_walk1(r->r_left, func);
	(*func)();
	var_walk1(r->r_right, func);
}

var_add(r)
register struct var *r;
{
	register struct var **p;

	for (p = &var_head; *p != 0;) {
		/* don't care about duplicate entries */
		if (strcmp(r->r_name, (*p)->r_name) < 0)
			p = &(*p)->r_left;
		else
			p = &(*p)->r_right;
	}
	*p = r;
}
