/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * "Object" handling primatives.
 * This simply means that user-specified routines are called to perform
 * the indicated operations.
 */

#include "calc.h"
#include "opcodes.h"
#include "func.h"
#include "symbol.h"
#include "string.h"


/*
 * Types of values returned by calling object routines.
 */
#define A_VALUE	0	/* returns arbitrary value */
#define A_INT	1	/* returns integer value */
#define A_UNDEF	2	/* returns no value */

/*
 * Error handling actions for when the function is undefined.
 */
#define E_NONE	0	/* no special action */
#define E_PRINT	1	/* print element */
#define E_CMP	2	/* compare two values */
#define E_TEST	3	/* test value for nonzero */
#define E_POW	4	/* call generic power routine */
#define E_ONE	5	/* return number 1 */
#define E_INC	6	/* increment by one */
#define E_DEC	7	/* decrement by one */
#define E_SQUARE 8	/* square value */


static struct objectinfo {
	short args;	/* number of arguments */
	short retval;	/* type of return value */
	short error;	/* special action on errors */
	char *name;	/* name of function to call */
	char *comment;	/* useful comment if any */
} objectinfo[] = {
	1, A_UNDEF, E_PRINT, "print",	"print value, default prints elements",
	1, A_VALUE, E_ONE,   "one",	"multiplicative identity, default is 1",
	1, A_INT,   E_TEST,  "test",	"logical test (false,true => 0,1), default tests elements",
	2, A_VALUE, E_NONE,  "add",	NULL,
	2, A_VALUE, E_NONE,  "sub",	NULL,
	1, A_VALUE, E_NONE,  "neg",	"negative",
	2, A_VALUE, E_NONE,  "mul",	NULL,
	2, A_VALUE, E_NONE,  "div",	"non-integral division",
	1, A_VALUE, E_NONE,  "inv",	"multiplicative inverse",
	2, A_VALUE, E_NONE,  "abs",	"absolute value within given error",
	1, A_VALUE, E_NONE,  "norm",	"square of absolute value",
	1, A_VALUE, E_NONE,  "conj",	"conjugate",
	2, A_VALUE, E_POW,   "pow",	"integer power, default does multiply, square, inverse",
	1, A_INT,   E_NONE,  "sgn",	"sign of value (-1, 0, 1)",
	2, A_INT,   E_CMP,   "cmp",	"equality (equal,nonequal => 0,1), default tests elements",
	2, A_INT,   E_NONE,  "rel",	"inequality (less,equal,greater => -1,0,1)",
	2, A_VALUE, E_NONE,  "quo",	"integer quotient",
	2, A_VALUE, E_NONE,  "mod",	"remainder of division",
	1, A_VALUE, E_NONE,  "int",	"integer part",
	1, A_VALUE, E_NONE,  "frac",	"fractional part",
	1, A_VALUE, E_INC,   "inc",	"increment, default adds 1",
	1, A_VALUE, E_DEC,   "dec",	"decrement, default subtracts 1",
	1, A_VALUE, E_SQUARE,"square",	"default multiplies by itself",
	2, A_VALUE, E_NONE,  "scale",	"multiply by power of 2",
	2, A_VALUE, E_NONE,  "shift",	"shift left by n bits (right if negative)",
	2, A_VALUE, E_NONE,  "round",	"round to given number of decimal places",
	2, A_VALUE, E_NONE,  "bround",	"round to given number of binary places",
	3, A_VALUE, E_NONE,  "root",	"root of value within given error",
	2, A_VALUE, E_NONE,  "sqrt",	"square root within given error",
	0, 0, 0, NULL
};


static STRINGHEAD objectnames;	/* names of objects */
static STRINGHEAD elements;	/* element names for parts of objects */
static OBJECTACTIONS *objects[MAXOBJECTS]; /* table of actions for objects */


/*
 * Free list of usual small objects.
 */
static FREELIST	freelist = {
	sizeof(OBJECT),		/* size of typical objects */
	100			/* number of free objects to keep */
};


static VALUE objpowi MATH_PROTO((VALUE *vp, NUMBER *q));
static BOOL objtest MATH_PROTO((OBJECT *op));
static BOOL objcmp MATH_PROTO((OBJECT *op1, OBJECT *op2));
static void objprint MATH_PROTO((OBJECT *op));


/*
 * Show all the routine names available for objects.
 */
void
showobjfuncs()
{
	register struct objectinfo *oip;

	printf("\nThe following object routines are definable.\n");
	printf("Note: xx represents the actual object type name.\n\n");
	printf("Name	Args	Comments\n");
	for (oip = objectinfo; oip->name; oip++) {
		printf("xx_%-8s %d	%s\n", oip->name, oip->args,
			oip->comment ? oip->comment : "");
	}
	printf("\n");
}


/*
 * Call the appropriate user-defined routine to handle an object action.
 * Returns the value that the routine returned.
 */
VALUE
objcall(action, v1, v2, v3)
	int action;
	VALUE *v1, *v2, *v3;
{
	FUNC *fp;		/* function to call */
	static OBJECTACTIONS *oap; /* object to call for */
	struct objectinfo *oip;	/* information about action */
	long index;		/* index of function (negative if undefined) */
	VALUE val;		/* return value */
	VALUE tmp;		/* temp value */
	char name[SYMBOLSIZE+1];	/* full name of user routine to call */

	if ((unsigned)action > OBJ_MAXFUNC)
		math_error("Illegal action for object call");
	oip = &objectinfo[action];
	if (v1->v_type == V_OBJ)
		oap = v1->v_obj->o_actions;
	else if (v2->v_type == V_OBJ)
		oap = v2->v_obj->o_actions;
	else
		math_error("Object routine called with non-object");
	index = oap->actions[action];
	if (index == 0) {
		strcpy(name, oap->name);
		strcat(name, "_");
		strcat(name, oip->name);
		index = adduserfunc(name);
		oap->actions[action] = index;
	}
	fp = NULL;
	if (index > 0)
		fp = findfunc(index);
	if (fp == NULL) {
		switch (oip->error) {
			case E_PRINT:
				objprint(v1->v_obj);
				val.v_type = V_NULL;
				break;
			case E_CMP:
				val.v_type = V_INT;
				if (v1->v_type != v2->v_type) {
					val.v_int = 1;
					return val;
				}
				val.v_int = objcmp(v1->v_obj, v2->v_obj);
				break;
			case E_TEST:
				val.v_type = V_INT;
				val.v_int = objtest(v1->v_obj);
				break;
			case E_POW:
				if (v2->v_type != V_NUM)
					math_error("Non-real power");
				val = objpowi(v1, v2->v_num);
				break;
			case E_ONE:
				val.v_type = V_NUM;
				val.v_num = qlink(&_qone_);
				break;
			case E_INC:
				tmp.v_type = V_NUM;
				tmp.v_num = &_qone_;
				val = objcall(OBJ_ADD, v1, &tmp, NULL_VALUE);
				break;
			case E_DEC:
				tmp.v_type = V_NUM;
				tmp.v_num = &_qone_;
				val = objcall(OBJ_SUB, v1, &tmp, NULL_VALUE);
				break;
			case E_SQUARE:
				val = objcall(OBJ_MUL, v1, v1, NULL_VALUE);
				break;
			default:
				math_error("Function \"%s\" is undefined", namefunc(index));
		}
		return val;
	}
	switch (oip->args) {
		case 0:
			break;
		case 1:
			++stack;
			stack->v_addr = v1;
			stack->v_type = V_ADDR;
			break;
		case 2:
			++stack;
			stack->v_addr = v1;
			stack->v_type = V_ADDR;
			++stack;
			stack->v_addr = v2;
			stack->v_type = V_ADDR;
			break;
		case 3:
			++stack;
			stack->v_addr = v1;
			stack->v_type = V_ADDR;
			++stack;
			stack->v_addr = v2;
			stack->v_type = V_ADDR;
			++stack;
			stack->v_addr = v3;
			stack->v_type = V_ADDR;
			break;
		default:
			math_error("Bad number of args to calculate");
	}
	calculate(fp, oip->args);
	switch (oip->retval) {
		case A_VALUE:
			return *stack--;
		case A_UNDEF:
			freevalue(stack--);
			val.v_type = V_NULL;
			break;
		case A_INT:
			if ((stack->v_type != V_NUM) || qisfrac(stack->v_num))
				math_error("Integer return value required");
			index = qtoi(stack->v_num);
			qfree(stack->v_num);
			stack--;
			val.v_type = V_INT;
			val.v_int = index;
			break;
		default:
			math_error("Bad object return");
	}
	return val;
}


/*
 * Routine called to clear the cache of known undefined functions for
 * the objects.  This changes negative indices back into positive ones
 * so that they will all be checked for existence again.
 */
void
objuncache()
{
	register int *ip;
	int i, j;

	i = objectnames.h_count;
	while (--i >= 0) {
		ip = objects[i]->actions;
		for (j = OBJ_MAXFUNC; j-- >= 0; ip++)
			if (*ip < 0)
				*ip = -*ip;
	}
}


/*
 * Print the elements of an object in short and unambiguous format.
 * This is the default routine if the user's is not defined.
 */
static void
objprint(op)
	OBJECT *op;		/* object being printed */
{
	int count;		/* number of elements */
	int i;			/* index */

	count = op->o_actions->count;
	math_fmt("obj %s {", op->o_actions->name);
	for (i = 0; i < count; i++) {
		if (i)
			math_str(", ");
		printvalue(&op->o_table[i], PRINT_SHORT | PRINT_UNAMBIG);
	}
	math_chr('}');
}


/*
 * Test an object for being "nonzero".
 * This is the default routine if the user's is not defined.
 * Returns TRUE if any of the elements are "nonzero".
 */
static BOOL
objtest(op)
	OBJECT *op;
{
	int i;			/* loop counter */

	i = op->o_actions->count;
	while (--i >= 0) {
		if (testvalue(&op->o_table[i]))
			return TRUE;
	}
	return FALSE;
}


/*
 * Compare two objects for equality, returning TRUE if they differ.
 * This is the default routine if the user's is not defined.
 * For equality, all elements must be equal.
 */
static BOOL
objcmp(op1, op2)
	OBJECT *op1, *op2;
{
	int i;			/* loop counter */

	if (op1->o_actions != op2->o_actions)
		return TRUE;
	i = op1->o_actions->count;
	while (--i >= 0) {
		if (comparevalue(&op1->o_table[i], &op2->o_table[i]))
			return TRUE;
	}
	return FALSE;
}


/*
 * Raise an object to an integral power.
 * This is the default routine if the user's is not defined.
 * Negative powers mean the positive power of the inverse.
 * Zero means the multiplicative identity.
 */
static VALUE
objpowi(vp, q)
	VALUE *vp;		/* value to be powered */
	NUMBER *q;		/* power to raise number to */
{
	VALUE res, tmp;
	long power;		/* power to raise to */
	unsigned long bit;	/* current bit value */

	if (qisfrac(q))
		math_error("Raising object to non-integral power");
	if (zisbig(q->num))
		math_error("Raising object to very large power");
	power = (zistiny(q->num) ? z1tol(q->num) : z2tol(q->num));
	if (qisneg(q))
		power = -power;
	/*
	 * Handle some low powers specially
	 */
	if ((power <= 2) && (power >= -2)) {
		switch ((int) power) {
			case 0:
				return objcall(OBJ_ONE, vp, NULL_VALUE, NULL_VALUE);
			case 1:
				res.v_obj = objcopy(vp->v_obj);
				res.v_type = V_OBJ;
				return res;
			case -1:
				return objcall(OBJ_INV, vp, NULL_VALUE, NULL_VALUE);
			case 2:
				return objcall(OBJ_SQUARE, vp, NULL_VALUE, NULL_VALUE);
		}
	}
	if (power < 0)
		power = -power;
	/*
	 * Compute the power by squaring and multiplying.
	 * This uses the left to right method of power raising.
	 */
	bit = TOPFULL;
	while ((bit & power) == 0)
		bit >>= 1L;
	bit >>= 1L;
	res = objcall(OBJ_SQUARE, vp, NULL_VALUE, NULL_VALUE);
	if (bit & power) {
		tmp = objcall(OBJ_MUL, &res, vp, NULL_VALUE);
		objfree(res.v_obj);
		res = tmp;
	}
	bit >>= 1L;
	while (bit) {
		tmp = objcall(OBJ_SQUARE, &res, NULL_VALUE, NULL_VALUE);
		objfree(res.v_obj);
		res = tmp;
		if (bit & power) {
			tmp = objcall(OBJ_MUL, &res, vp, NULL_VALUE);
			objfree(res.v_obj);
			res = tmp;
		}
		bit >>= 1L;
	}
	if (qisneg(q)) {
		tmp = objcall(OBJ_INV, &res, NULL_VALUE, NULL_VALUE);
		objfree(res.v_obj);
		return tmp;
	}
	return res;
}


/*
 * Define a (possibly) new class of objects.
 * The list of indexes for the element names is also specified here,
 * and the number of elements defined for the object.
 */
void
defineobject(name, indices, count)
	char *name;		/* name of object type */
	int indices[];		/* table of indices for elements */
	int count;
{
	OBJECTACTIONS *oap;	/* object definition structure */
	STRINGHEAD *hp;
	int index;

	hp = &objectnames;
	if (hp->h_list == NULL)
		initstr(hp);
	index = findstr(hp, name);
	if (index >= 0) {
		/*
		 * Object is already defined.  Give an error unless this
		 * new definition is exactly the same as the old one.
		 */
		oap = objects[index];
		if (oap->count == count) {
			for (index = 0; ; index++) {
				if (index >= count)
					return;
				if (oap->elements[index] != indices[index])
					break;
			}
		}
		math_error("Object type \"%s\" is already defined", name);
	}

	if (hp->h_count >= MAXOBJECTS)
		math_error("Too many object types in use");
	oap = (OBJECTACTIONS *) malloc(objectactionsize(count));
	if (oap)
		name = addstr(hp, name);
	if ((oap == NULL) || (name == NULL))
		math_error("Cannot allocate object type");
	oap->name = name;
	oap->count = count;
	for (index = OBJ_MAXFUNC; index >= 0; index--)
		oap->actions[index] = 0;
	for (index = 0; index < count; index++)
		oap->elements[index] = indices[index];
	index = findstr(hp, name);
	objects[index] = oap;
	return;
}


/*
 * Check an object name to see if it is currently defined.
 * If so, the index for the object type is returned.
 * If the object name is currently unknown, then -1 is returned.
 */
int
checkobject(name)
	char *name;
{
	STRINGHEAD *hp;

	hp = &objectnames;
	if (hp->h_list == NULL)
		return -1;
	return findstr(hp, name);
}


/*
 * Define a (possibly) new element name for an object.
 * Returns an index which identifies the element name.
 */
int
addelement(name)
	char *name;
{
	STRINGHEAD *hp;
	int index;

	hp = &elements;
	if (hp->h_list == NULL)
		initstr(hp);
	index = findstr(hp, name);
	if (index >= 0)
		return index;
	if (addstr(hp, name) == NULL)
		math_error("Cannot allocate element name");
	return findstr(hp, name);
}


/*
 * Return the index which identifies an element name.
 * Returns minus one if the element name is unknown.
 */
int
findelement(name)
	char *name;		/* element name */
{
	if (elements.h_list == NULL)
		return -1;
	return findstr(&elements, name);
}


/*
 * Return the value table offset to be used for an object element name.
 * This converts the element index from the element table into an offset
 * into the object value array.  Returns -1 if the element index is unknown.
 */
int
objoffset(op, index)
	OBJECT *op;
	long index;
{
	register OBJECTACTIONS *oap;
	int offset;			/* offset into value array */

	oap = op->o_actions;
	for (offset = oap->count - 1; offset >= 0; offset--) {
		if (oap->elements[offset] == index)
			return offset;
	}
	return -1;
}


/*
 * Allocate a new object structure with the specified index.
 */
OBJECT *
objalloc(index)
	long index;
{
	OBJECTACTIONS *oap;
	OBJECT *op;
	VALUE *vp;
	int i;

	if ((unsigned) index >= MAXOBJECTS)
		math_error("Allocating bad object index");
	oap = objects[index];
	if (oap == NULL)
		math_error("Object type not defined");
	i = oap->count;
	if (i < USUAL_ELEMENTS)
		i = USUAL_ELEMENTS;
	if (i == USUAL_ELEMENTS)
		op = (OBJECT *) allocitem(&freelist);
	else
		op = (OBJECT *) malloc(objectsize(i));
	if (op == NULL)
		math_error("Cannot allocate object");
	op->o_actions = oap;
	vp = op->o_table;
	for (i = oap->count; i-- > 0; vp++) {
		vp->v_num = qlink(&_qzero_);
		vp->v_type = V_NUM;
	}
	return op;
}


/*
 * Free an object structure.
 */
void
objfree(op)
	register OBJECT *op;
{
	VALUE *vp;
	int i;

	vp = op->o_table;
	for (i = op->o_actions->count; i-- > 0; vp++) {
		if (vp->v_type == V_NUM) {
			qfree(vp->v_num);
		} else
			freevalue(vp);
	}
	if (op->o_actions->count <= USUAL_ELEMENTS)
		freeitem(&freelist, (FREEITEM *) op);
	else
		free((char *) op);
}


/*
 * Copy an object value
 */
OBJECT *
objcopy(op)
	OBJECT *op;
{
	VALUE *v1, *v2;
	OBJECT *np;
	int i;

	i = op->o_actions->count;
	if (i < USUAL_ELEMENTS)
		i = USUAL_ELEMENTS;
	if (i == USUAL_ELEMENTS)
		np = (OBJECT *) allocitem(&freelist);
	else
		np = (OBJECT *) malloc(objectsize(i));
	if (np == NULL)
		math_error("Cannot allocate object");
	np->o_actions = op->o_actions;
	v1 = op->o_table;
	v2 = np->o_table;
	for (i = op->o_actions->count; i-- > 0; v1++, v2++) {
		if (v1->v_type == V_NUM) {
			v2->v_num = qlink(v1->v_num);
			v2->v_type = V_NUM;
		} else
			copyvalue(v1, v2);
	}
	return np;
}


/*
 * Return a trivial hash value for an object.
 */
HASH
objhash(op)
	OBJECT *op;
{
	HASH hash;
	int i;

	hash = 0;
	i = op->o_actions->count;
	while (--i >= 0)
		hash = hash * 4000037 + hashvalue(&op->o_table[i]);
	return hash;
}

/* END CODE */
