/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Configuration routines.
 */

#include "calc.h"


/*
 * Configuration parameter name and type.
 */
typedef struct {
	char *name;	/* name of configuration string */
	int type;	/* type for configuration */
} CONFIG;


/*
 * Table of configuration types that can be set or read.
 */
static CONFIG configs[] = {
	"trace",	CONFIG_TRACE,
	"display",	CONFIG_DISPLAY,
	"epsilon",	CONFIG_EPSILON,
	"mode",		CONFIG_MODE,
	"maxprint",	CONFIG_MAXPRINT,
	"mul2",		CONFIG_MUL2,
	"sq2",		CONFIG_SQ2,
	"pow2",		CONFIG_POW2,
	"redc2",	CONFIG_REDC2,
	"tilde",	CONFIG_TILDE,
	"tab",		CONFIG_TAB,
	NULL,		0
};


/*
 * Possible output modes.
 */
static CONFIG modes[] = {
	"frac",		MODE_FRAC,
	"decimal",	MODE_FRAC,
	"dec",		MODE_FRAC,
	"int",		MODE_INT,
	"real",		MODE_REAL,
	"exp",		MODE_EXP,
	"hexadecimal",	MODE_HEX,
	"hex",		MODE_HEX,
	"octal",	MODE_OCTAL,
	"oct",		MODE_OCTAL,
	"binary",	MODE_BINARY,
	"bin",		MODE_BINARY,
	NULL,		0
};


/*
 * Possible binary config state values
 */
static CONFIG truth[] = {
	"y",		TRUE,
	"n",		FALSE,
	"yes",		TRUE,
	"no",		FALSE,
	"set",		TRUE,
	"unset",	FALSE,
	"on",		TRUE,
	"off",		FALSE,
	"true",		TRUE,
	"false",	FALSE,
	"t",		TRUE,
	"f",		FALSE,
	"1",		TRUE,
	"0",		FALSE,
	NULL,		0
};


/*
 * Given a string value which represents a configuration name, return
 * the configuration type for that string.  Returns negative type if
 * the string is unknown.
 */
int
configtype(name)
	char *name;		/* configuration name */
{
	CONFIG *cp;		/* current config pointer */

	for (cp = configs; cp->name; cp++) {
		if (strcmp(cp->name, name) == 0)
			return cp->type;
	}
	return -1;
}


/*
 * Given the name of a mode, convert it to the internal format.
 * Returns -1 if the string is unknown.
 */
static int
modetype(name)
	char *name;		/* mode name */
{
	CONFIG *cp;		/* current config pointer */

	for (cp = modes; cp->name; cp++) {
		if (strcmp(cp->name, name) == 0)
			return cp->type;
	}
	return -1;
}


/*
 * Given the name of a truth value, convert it to a BOOL or -1.
 * Returns -1 if the string is unknown.
 */
static int
truthtype(name)
	char *name;		/* mode name */
{
	CONFIG *cp;		/* current config pointer */

	for (cp = truth; cp->name; cp++) {
		if (strcmp(cp->name, name) == 0)
			return cp->type;
	}
	return -1;
}


/*
 * Given the mode type, convert it to a string representing that mode.
 * Where there are multiple strings representing the same mode, the first
 * one in the table is used.  Returns NULL if the node type is unknown.
 * The returned string cannot be modified.
 */
static char *
modename(type)
	int type;
{
	CONFIG *cp;		/* current config pointer */

	for (cp = modes; cp->name; cp++) {
		if (type == cp->type)
			return cp->name;
	}
	return NULL;
}


/*
 * Set the specified configuration type to the specified value.
 * An error is generated if the type number or value is illegal.
 */
void
setconfig(type, vp)
	int type;
	VALUE *vp;
{
	NUMBER *q;
	long temp;

	switch (type) {
		case CONFIG_TRACE:
			if (vp->v_type != V_NUM)
				math_error("Non-numeric for trace");
			q = vp->v_num;
			temp = qtoi(q);
			if (qisfrac(q) || !zistiny(q->num) ||
				((unsigned long) temp > TRACE_MAX))
					math_error("Bad trace value");
			traceflags = (FLAG)temp;
			break;

		case CONFIG_DISPLAY:
			if (vp->v_type != V_NUM)
				math_error("Non-numeric for display");
			q = vp->v_num;
			temp = qtoi(q);
			if (qisfrac(q) || qisneg(q) || !zistiny(q->num))
				temp = -1;
			math_setdigits(temp);
			break;

		case CONFIG_MODE:
			if (vp->v_type != V_STR)
				math_error("Non-string for mode");
			temp = modetype(vp->v_str);
			if (temp < 0)
				math_error("Unknown mode \"%s\"", vp->v_str);
			math_setmode((int) temp);
			break;

		case CONFIG_EPSILON:
			if (vp->v_type != V_NUM)
				math_error("Non-numeric for epsilon");
			setepsilon(vp->v_num);
			break;

		case CONFIG_MAXPRINT:
			if (vp->v_type != V_NUM)
				math_error("Non-numeric for maxprint");
			q = vp->v_num;
			temp = qtoi(q);
			if (qisfrac(q) || qisneg(q) || !zistiny(q->num))
				temp = -1;
			if (temp < 0)
				math_error("Maxprint value is out of range");
			maxprint = temp;
			break;

		case CONFIG_MUL2:
			if (vp->v_type != V_NUM)
				math_error("Non-numeric for mul2");
			q = vp->v_num;
			temp = qtoi(q);
			if (qisfrac(q) || qisneg(q))
				temp = -1;
			if (temp == 0)
				temp = MUL_ALG2;
			if (temp < 2)
				math_error("Illegal mul2 value");
			_mul2_ = temp;
			break;

		case CONFIG_SQ2:
			if (vp->v_type != V_NUM)
				math_error("Non-numeric for sq2");
			q = vp->v_num;
			temp = qtoi(q);
			if (qisfrac(q) || qisneg(q))
				temp = -1;
			if (temp == 0)
				temp = SQ_ALG2;
			if (temp < 2)
				math_error("Illegal sq2 value");
			_sq2_ = temp;
			break;

		case CONFIG_POW2:
			if (vp->v_type != V_NUM)
				math_error("Non-numeric for pow2");
			q = vp->v_num;
			temp = qtoi(q);
			if (qisfrac(q) || qisneg(q))
				temp = -1;
			if (temp == 0)
				temp = POW_ALG2;
			if (temp < 1)
				math_error("Illegal pow2 value");
			_pow2_ = temp;
			break;

		case CONFIG_REDC2:
			if (vp->v_type != V_NUM)
				math_error("Non-numeric for redc2");
			q = vp->v_num;
			temp = qtoi(q);
			if (qisfrac(q) || qisneg(q))
				temp = -1;
			if (temp == 0)
				temp = REDC_ALG2;
			if (temp < 1)
				math_error("Illegal redc2 value");
			_redc2_ = temp;
			break;


		case CONFIG_TILDE:
			if (vp->v_type == V_NUM) {
				q = vp->v_num;
				tilde_ok = !qiszero(q);
			} else if (vp->v_type == V_STR) {
				temp = truthtype(vp->v_str);
				if (temp < 0) {
					math_error("Illegal truth value");
				}
				tilde_ok = temp;
			}
			break;
		case CONFIG_TAB:
			if (vp->v_type == V_NUM) {
				q = vp->v_num;
				tab_ok = !qiszero(q);
			} else if (vp->v_type == V_STR) {
				temp = truthtype(vp->v_str);
				if (temp < 0) {
					math_error("Illegal truth value");
				}
				tab_ok = temp;
			}
			break;

		default:
			math_error("Setting illegal config parameter");
	}
}


/*
 * Get the current value of the specified configuration type.
 * An error is generated if the type number is illegal.
 */
void
getconfig(type, vp)
	int type;
	VALUE *vp;
{
	switch (type) {
		case CONFIG_TRACE:
			vp->v_type = V_NUM;
			vp->v_num = itoq((long) traceflags);
			break;

		case CONFIG_DISPLAY:
			vp->v_type = V_NUM;
			vp->v_num = itoq(_outdigits_);
			break;

		case CONFIG_MODE:
			vp->v_type = V_STR;
			vp->v_subtype = V_STRLITERAL;
			vp->v_str = modename(_outmode_);
			break;

		case CONFIG_EPSILON:
			vp->v_type = V_NUM;
			vp->v_num = qlink(_epsilon_);
			break;

		case CONFIG_MAXPRINT:
			vp->v_type = V_NUM;
			vp->v_num = itoq(maxprint);
			break;

		case CONFIG_MUL2:
			vp->v_type = V_NUM;
			vp->v_num = itoq(_mul2_);
			break;

		case CONFIG_SQ2:
			vp->v_type = V_NUM;
			vp->v_num = itoq(_sq2_);
			break;

		case CONFIG_POW2:
			vp->v_type = V_NUM;
			vp->v_num = itoq(_pow2_);
			break;

		case CONFIG_REDC2:
			vp->v_type = V_NUM;
			vp->v_num = itoq(_redc2_);
			break;
		
		case CONFIG_TILDE:
			vp->v_type = V_NUM;
			vp->v_num = itoq(tilde_ok);
		
		case CONFIG_TAB:
			vp->v_type = V_NUM;
			vp->v_num = itoq(tab_ok);
			break;

		default:
			math_error("Getting illegal config parameter");
	}
}

/* END CODE */
