/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Generic value manipulation routines.
 */

#include "value.h"
#include "opcodes.h"
#include "func.h"
#include "symbol.h"
#include "string.h"


/*
 * Free a value and set its type to undefined.
 */
void
freevalue(vp)
	register VALUE *vp;	/* value to be freed */
{
	int type;		/* type of value being freed */

	type = vp->v_type;
	vp->v_type = V_NULL;
	switch (type) {
		case V_NULL:
		case V_ADDR:
		case V_FILE:
			break;
		case V_STR:
			if (vp->v_subtype == V_STRALLOC)
				free(vp->v_str);
			break;
		case V_NUM:
			qfree(vp->v_num);
			break;
		case V_COM:
			comfree(vp->v_com);
			break;
		case V_MAT:
			matfree(vp->v_mat);
			break;
		case V_LIST:
			listfree(vp->v_list);
			break;
		case V_ASSOC:
			assocfree(vp->v_assoc);
			break;
		case V_OBJ:
			objfree(vp->v_obj);
			break;
		default:
			math_error("Freeing unknown value type");
	}
	vp->v_subtype = V_NOSUBTYPE;
}


/*
 * Copy a value from one location to another.
 * This overwrites the specified new value without checking it.
 */
void
copyvalue(oldvp, newvp)
	register VALUE *oldvp;		/* value to be copied from */
	register VALUE *newvp;		/* value to be copied into */
{
	newvp->v_type = V_NULL;
	switch (oldvp->v_type) {
		case V_NULL:
			break;
		case V_FILE:
			newvp->v_file = oldvp->v_file;
			break;
		case V_NUM:
			newvp->v_num = qlink(oldvp->v_num);
			break;
		case V_COM:
			newvp->v_com = clink(oldvp->v_com);
			break;
		case V_STR:
			newvp->v_str = oldvp->v_str;
			if (oldvp->v_subtype == V_STRALLOC) {
				newvp->v_str = (char *)malloc(strlen(oldvp->v_str) + 1);
				if (newvp->v_str == NULL)
					math_error("Cannot get memory for string copy");
				strcpy(newvp->v_str, oldvp->v_str);
			}
			break;
		case V_MAT:
			newvp->v_mat = matcopy(oldvp->v_mat);
			break;
		case V_LIST:
			newvp->v_list = listcopy(oldvp->v_list);
			break;
		case V_ASSOC:
			newvp->v_assoc = assoccopy(oldvp->v_assoc);
			break;
		case V_ADDR:
			newvp->v_addr = oldvp->v_addr;
			break;
		case V_OBJ:
			newvp->v_obj = objcopy(oldvp->v_obj);
			break;
		default:
			math_error("Copying unknown value type");
	}
	if (oldvp->v_type == V_STR) {
		newvp->v_subtype = oldvp->v_subtype;
	} else {
		newvp->v_subtype = V_NOSUBTYPE;
	}
	newvp->v_type = oldvp->v_type;

}


/*
 * Negate an arbitrary value.
 * Result is placed in the indicated location.
 */
void
negvalue(vp, vres)
	VALUE *vp, *vres;
{
	vres->v_type = V_NULL;
	switch (vp->v_type) {
		case V_NUM:
			vres->v_num = qneg(vp->v_num);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			vres->v_com = cneg(vp->v_com);
			vres->v_type = V_COM;
			return;
		case V_MAT:
			vres->v_mat = matneg(vp->v_mat);
			vres->v_type = V_MAT;
			return;
		case V_OBJ:
			*vres = objcall(OBJ_NEG, vp, NULL_VALUE, NULL_VALUE);
			return;
		default:
			math_error("Illegal value for negation");
	}
}


/*
 * Add two arbitrary values together.
 * Result is placed in the indicated location.
 */
void
addvalue(v1, v2, vres)
	VALUE *v1, *v2, *vres;
{
	COMPLEX *c;

	vres->v_type = V_NULL;
	switch (TWOVAL(v1->v_type, v2->v_type)) {
		case TWOVAL(V_NUM, V_NUM):
			vres->v_num = qadd(v1->v_num, v2->v_num);
			vres->v_type = V_NUM;
			return;
		case TWOVAL(V_COM, V_NUM):
			vres->v_com = caddq(v1->v_com, v2->v_num);
			vres->v_type = V_COM;
			return;
		case TWOVAL(V_NUM, V_COM):
			vres->v_com = caddq(v2->v_com, v1->v_num);
			vres->v_type = V_COM;
			return;
		case TWOVAL(V_COM, V_COM):
			vres->v_com = cadd(v1->v_com, v2->v_com);
			vres->v_type = V_COM;
			c = vres->v_com;
			if (!cisreal(c))
				return;
			vres->v_num = qlink(c->real);
			vres->v_type = V_NUM;
			comfree(c);
			return;
		case TWOVAL(V_MAT, V_MAT):
			vres->v_mat = matadd(v1->v_mat, v2->v_mat);
			vres->v_type = V_MAT;
			return;
		default:
			if ((v1->v_type != V_OBJ) && (v2->v_type != V_OBJ))
				math_error("Non-compatible values for add");
			*vres = objcall(OBJ_ADD, v1, v2, NULL_VALUE);
			return;
	}
}


/*
 * Subtract one arbitrary value from another one.
 * Result is placed in the indicated location.
 */
void
subvalue(v1, v2, vres)
	VALUE *v1, *v2, *vres;
{
	COMPLEX *c;

	vres->v_type = V_NULL;
	switch (TWOVAL(v1->v_type, v2->v_type)) {
		case TWOVAL(V_NUM, V_NUM):
			vres->v_num = qsub(v1->v_num, v2->v_num);
			vres->v_type = V_NUM;
			return;
		case TWOVAL(V_COM, V_NUM):
			vres->v_com = csubq(v1->v_com, v2->v_num);
			vres->v_type = V_COM;
			return;
		case TWOVAL(V_NUM, V_COM):
			c = csubq(v2->v_com, v1->v_num);
			vres->v_com = cneg(c);
			comfree(c);
			vres->v_type = V_COM;
			return;
		case TWOVAL(V_COM, V_COM):
			vres->v_com = csub(v1->v_com, v2->v_com);
			vres->v_type = V_COM;
			c = vres->v_com;
			if (!cisreal(c))
				return;
			vres->v_num = qlink(c->real);
			vres->v_type = V_NUM;
			comfree(c);
			return;
		case TWOVAL(V_MAT, V_MAT):
			vres->v_mat = matsub(v1->v_mat, v2->v_mat);
			vres->v_type = V_MAT;
			return;
		default:
			if ((v1->v_type != V_OBJ) && (v2->v_type != V_OBJ))
				math_error("Non-compatible values for subtract");
			*vres = objcall(OBJ_SUB, v1, v2, NULL_VALUE);
			return;
	}
}


/*
 * Multiply two arbitrary values together.
 * Result is placed in the indicated location.
 */
void
mulvalue(v1, v2, vres)
	VALUE *v1, *v2, *vres;
{
	COMPLEX *c;

	vres->v_type = V_NULL;
	switch (TWOVAL(v1->v_type, v2->v_type)) {
		case TWOVAL(V_NUM, V_NUM):
			vres->v_num = qmul(v1->v_num, v2->v_num);
			vres->v_type = V_NUM;
			return;
		case TWOVAL(V_COM, V_NUM):
			vres->v_com = cmulq(v1->v_com, v2->v_num);
			vres->v_type = V_COM;
			break;
		case TWOVAL(V_NUM, V_COM):
			vres->v_com = cmulq(v2->v_com, v1->v_num);
			vres->v_type = V_COM;
			break;
		case TWOVAL(V_COM, V_COM):
			vres->v_com = cmul(v1->v_com, v2->v_com);
			vres->v_type = V_COM;
			break;
		case TWOVAL(V_MAT, V_MAT):
			vres->v_mat = matmul(v1->v_mat, v2->v_mat);
			vres->v_type = V_MAT;
			return;
		case TWOVAL(V_MAT, V_NUM):
		case TWOVAL(V_MAT, V_COM):
			vres->v_mat = matmulval(v1->v_mat, v2);
			vres->v_type = V_MAT;
			return;
		case TWOVAL(V_NUM, V_MAT):
		case TWOVAL(V_COM, V_MAT):
			vres->v_mat = matmulval(v2->v_mat, v1);
			vres->v_type = V_MAT;
			return;
		default:
			if ((v1->v_type != V_OBJ) && (v2->v_type != V_OBJ))
				math_error("Non-compatible values for multiply");
			*vres = objcall(OBJ_MUL, v1, v2, NULL_VALUE);
			return;
	}
	c = vres->v_com;
	if (cisreal(c)) {
		vres->v_num = qlink(c->real);
		vres->v_type = V_NUM;
		comfree(c);
	}
}


/*
 * Square an arbitrary value.
 * Result is placed in the indicated location.
 */
void
squarevalue(vp, vres)
	VALUE *vp, *vres;
{
	COMPLEX *c;

	vres->v_type = V_NULL;
	switch (vp->v_type) {
		case V_NUM:
			vres->v_num = qsquare(vp->v_num);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			vres->v_com = csquare(vp->v_com);
			vres->v_type = V_COM;
			c = vres->v_com;
			if (!cisreal(c))
				return;
			vres->v_num = qlink(c->real);
			vres->v_type = V_NUM;
			comfree(c);
			return;
		case V_MAT:
			vres->v_mat = matsquare(vp->v_mat);
			vres->v_type = V_MAT;
			return;
		case V_OBJ:
			*vres = objcall(OBJ_SQUARE, vp, NULL_VALUE, NULL_VALUE);
			return;
		default:
			math_error("Illegal value for squaring");
	}
}


/*
 * Invert an arbitrary value.
 * Result is placed in the indicated location.
 */
void
invertvalue(vp, vres)
	VALUE *vp, *vres;
{
	vres->v_type = V_NULL;
	switch (vp->v_type) {
		case V_NUM:
			vres->v_num = qinv(vp->v_num);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			vres->v_com = cinv(vp->v_com);
			vres->v_type = V_COM;
			return;
		case V_MAT:
			vres->v_mat = matinv(vp->v_mat);
			vres->v_type = V_MAT;
			return;
		case V_OBJ:
			*vres = objcall(OBJ_INV, vp, NULL_VALUE, NULL_VALUE);
			return;
		default:
			math_error("Illegal value for inverting");
	}
}


/*
 * Round an arbitrary value to the specified number of decimal places.
 * Result is placed in the indicated location.
 */
void
roundvalue(v1, v2, vres)
	VALUE *v1, *v2, *vres;
{
	long places = -1;
	NUMBER *q;
	COMPLEX *c;

	switch (v2->v_type) {
		case V_NUM:
			q = v2->v_num;
			if (qisfrac(q) || zisbig(q->num))
				math_error("Bad number of places for round");
			places = qtoi(q);
			break;
		case V_INT:
			places = v2->v_int;
			break;
		default:
			math_error("Bad value type for places in round");
	}
	if (places < 0)
		math_error("Negative number of places in round");
	vres->v_type = V_NULL;
	switch (v1->v_type) {
		case V_NUM:
			if (qisint(v1->v_num))
				vres->v_num = qlink(v1->v_num);
			else
				vres->v_num = qround(v1->v_num, places);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			if (cisint(v1->v_com)) {
				vres->v_com = clink(v1->v_com);
				vres->v_type = V_COM;
				return;
			}
			vres->v_com = cround(v1->v_com, places);
			vres->v_type = V_COM;
			c = vres->v_com;
			if (cisreal(c)) {
				vres->v_num = qlink(c->real);
				vres->v_type = V_NUM;
				comfree(c);
			}
			return;
		case V_MAT:
			vres->v_mat = matround(v1->v_mat, places);
			vres->v_type = V_MAT;
			return;
		case V_OBJ:
			*vres = objcall(OBJ_ROUND, v1, v2, NULL_VALUE);
			return;
		default:
			math_error("Illegal value for round");
	}
}


/*
 * Round an arbitrary value to the specified number of binary places.
 * Result is placed in the indicated location.
 */
void
broundvalue(v1, v2, vres)
	VALUE *v1, *v2, *vres;
{
	long places = -1;
	NUMBER *q;
	COMPLEX *c;

	switch (v2->v_type) {
		case V_NUM:
			q = v2->v_num;
			if (qisfrac(q) || zisbig(q->num))
				math_error("Bad number of places for bround");
			places = qtoi(q);
			break;
		case V_INT:
			places = v2->v_int;
			break;
		default:
			math_error("Bad value type for places in bround");
	}
	if (places < 0)
		math_error("Negative number of places in bround");
	vres->v_type = V_NULL;
	switch (v1->v_type) {
		case V_NUM:
			if (qisint(v1->v_num))
				vres->v_num = qlink(v1->v_num);
			else
				vres->v_num = qbround(v1->v_num, places);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			if (cisint(v1->v_com)) {
				vres->v_com = clink(v1->v_com);
				vres->v_type = V_COM;
				return;
			}
			vres->v_com = cbround(v1->v_com, places);
			vres->v_type = V_COM;
			c = vres->v_com;
			if (cisreal(c)) {
				vres->v_num = qlink(c->real);
				vres->v_type = V_NUM;
				comfree(c);
			}
			return;
		case V_MAT:
			vres->v_mat = matbround(v1->v_mat, places);
			vres->v_type = V_MAT;
			return;
		case V_OBJ:
			*vres = objcall(OBJ_BROUND, v1, v2, NULL_VALUE);
			return;
		default:
			math_error("Illegal value for bround");
	}
}


/*
 * Take the integer part of an arbitrary value.
 * Result is placed in the indicated location.
 */
void
intvalue(vp, vres)
	VALUE *vp, *vres;
{
	COMPLEX *c;

	vres->v_type = V_NULL;
	switch (vp->v_type) {
		case V_NUM:
			if (qisint(vp->v_num))
				vres->v_num = qlink(vp->v_num);
			else
				vres->v_num = qint(vp->v_num);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			if (cisint(vp->v_com)) {
				vres->v_com = clink(vp->v_com);
				vres->v_type = V_COM;
				return;
			}
			vres->v_com = cint(vp->v_com);
			vres->v_type = V_COM;
			c = vres->v_com;
			if (cisreal(c)) {
				vres->v_num = qlink(c->real);
				vres->v_type = V_NUM;
				comfree(c);
			}
			return;
		case V_MAT:
			vres->v_mat = matint(vp->v_mat);
			vres->v_type = V_MAT;
			return;
		case V_OBJ:
			*vres = objcall(OBJ_INT, vp, NULL_VALUE, NULL_VALUE);
			return;
		default:
			math_error("Illegal value for int");
	}
}


/*
 * Take the fractional part of an arbitrary value.
 * Result is placed in the indicated location.
 */
void
fracvalue(vp, vres)
	VALUE *vp, *vres;
{
	vres->v_type = V_NULL;
	switch (vp->v_type) {
		case V_NUM:
			if (qisint(vp->v_num))
				vres->v_num = qlink(&_qzero_);
			else
				vres->v_num = qfrac(vp->v_num);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			if (cisint(vp->v_com)) {
				vres->v_num = clink(&_qzero_);
				vres->v_type = V_NUM;
				return;
			}
			vres->v_com = cfrac(vp->v_com);
			vres->v_type = V_COM;
			return;
		case V_MAT:
			vres->v_mat = matfrac(vp->v_mat);
			vres->v_type = V_MAT;
			return;
		case V_OBJ:
			*vres = objcall(OBJ_FRAC, vp, NULL_VALUE, NULL_VALUE);
			return;
		default:
			math_error("Illegal value for frac function");
	}
}


/*
 * Increment an arbitrary value by one.
 * Result is placed in the indicated location.
 */
void
incvalue(vp, vres)
	VALUE *vp, *vres;
{
	switch (vp->v_type) {
		case V_NUM:
			vres->v_num = qinc(vp->v_num);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			vres->v_com = caddq(vp->v_com, &_qone_);
			vres->v_type = V_COM;
			return;
		case V_OBJ:
			*vres = objcall(OBJ_INC, vp, NULL_VALUE, NULL_VALUE);
			return;
		default:
			math_error("Illegal value for incrementing");
	}
}


/*
 * Decrement an arbitrary value by one.
 * Result is placed in the indicated location.
 */
void
decvalue(vp, vres)
	VALUE *vp, *vres;
{
	switch (vp->v_type) {
		case V_NUM:
			vres->v_num = qdec(vp->v_num);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			vres->v_com = caddq(vp->v_com, &_qnegone_);
			vres->v_type = V_COM;
			return;
		case V_OBJ:
			*vres = objcall(OBJ_DEC, vp, NULL_VALUE, NULL_VALUE);
			return;
		default:
			math_error("Illegal value for decrementing");
	}
}


/*
 * Produce the 'conjugate' of an arbitrary value.
 * Result is placed in the indicated location.
 * (Example: complex conjugate.)
 */
void
conjvalue(vp, vres)
	VALUE *vp, *vres;
{
	vres->v_type = V_NULL;
	switch (vp->v_type) {
		case V_NUM:
			vres->v_num = qlink(vp->v_num);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			vres->v_com = comalloc();
			vres->v_com->real = qlink(vp->v_com->real);
			vres->v_com->imag = qneg(vp->v_com->imag);
			vres->v_type = V_COM;
			return;
		case V_MAT:
			vres->v_mat = matconj(vp->v_mat);
			vres->v_type = V_MAT;
			return;
		case V_OBJ:
			*vres = objcall(OBJ_CONJ, vp, NULL_VALUE, NULL_VALUE);
			return;
		default:
			math_error("Illegal value for conjugation");
	}
}


/*
 * Take the square root of an arbitrary value within the specified error.
 * Result is placed in the indicated location.
 */
void
sqrtvalue(v1, v2, vres)
	VALUE *v1, *v2, *vres;
{
	NUMBER *q, *tmp;
	COMPLEX *c;

	if (v2->v_type != V_NUM)
		math_error("Non-real epsilon for sqrt");
	q = v2->v_num;
	if (qisneg(q) || qiszero(q))
		math_error("Illegal epsilon value for sqrt");
	switch (v1->v_type) {
		case V_NUM:
			if (!qisneg(v1->v_num)) {
				vres->v_num = qsqrt(v1->v_num, q);
				vres->v_type = V_NUM;
				return;
			}
			tmp = qneg(v1->v_num);
			c = comalloc();
			c->imag = qsqrt(tmp, q);
			qfree(tmp);
			vres->v_com = c;
			vres->v_type = V_COM;
			break;
		case V_COM:
			vres->v_com = csqrt(v1->v_com, q);
			vres->v_type = V_COM;
			break;
		case V_OBJ:
			*vres = objcall(OBJ_SQRT, v1, v2, NULL_VALUE);
			return;
		default:
			math_error("Bad value for taking square root");
	}
	c = vres->v_com;
	if (cisreal(c)) {
		vres->v_num = qlink(c->real);
		vres->v_type = V_NUM;
		comfree(c);
	}
}


/*
 * Take the Nth root of an arbitrary value within the specified error.
 * Result is placed in the indicated location.
 */
void
rootvalue(v1, v2, v3, vres)
	VALUE *v1;		/* value to take root of */
	VALUE *v2;		/* value specifying root to take */
	VALUE *v3;		/* value specifying error */
	VALUE *vres;
{
	NUMBER *q1, *q2;
	COMPLEX ctmp;

	if ((v2->v_type != V_NUM) || (v3->v_type != V_NUM))
		math_error("Non-real arguments for root");
	q1 = v2->v_num;
	q2 = v3->v_num;
	if (qisneg(q1) || qiszero(q1) || qisfrac(q1))
		math_error("Non-positive or non-integral root");
	if (qisneg(q2) || qiszero(q2))
		math_error("Non-positive epsilon for root");
	switch (v1->v_type) {
		case V_NUM:
			if (!qisneg(v1->v_num) || zisodd(q1->num)) {
				vres->v_num = qroot(v1->v_num, q1, q2);
				vres->v_type = V_NUM;
				return;
			}
			ctmp.real = v1->v_num;
			ctmp.imag = &_qzero_;
			ctmp.links = 1;
			vres->v_com = croot(&ctmp, q1, q2);
			vres->v_type = V_COM;
			return;
		case V_COM:
			vres->v_com = croot(v1->v_com, q1, q2);
			vres->v_type = V_COM;
			return;
		case V_OBJ:
			*vres = objcall(OBJ_ROOT, v1, v2, v3);
			return;
		default:
			math_error("Taking root of bad value");
	}
}


/*
 * Take the absolute value of an arbitrary value within the specified error.
 * Result is placed in the indicated location.
 */
void
absvalue(v1, v2, vres)
	VALUE *v1, *v2, *vres;
{
	static NUMBER *q;
	NUMBER *epsilon;

	if (v2->v_type != V_NUM)
		math_error("Bad epsilon type for abs");
	epsilon = v2->v_num;
	if (qiszero(epsilon) || qisneg(epsilon))
		math_error("Non-positive epsilon for abs");
	switch (v1->v_type) {
		case V_NUM:
			if (qisneg(v1->v_num))
				q = qneg(v1->v_num);
			else
				q = qlink(v1->v_num);
			break;
		case V_COM:
			q = qhypot(v1->v_com->real, v1->v_com->imag, epsilon);
			break;
		case V_OBJ:
			*vres = objcall(OBJ_ABS, v1, v2, NULL_VALUE);
			return;
		default:
			math_error("Illegal value for absolute value");
	}
	vres->v_num = q;
	vres->v_type = V_NUM;
}


/*
 * Calculate the norm of an arbitrary value.
 * Result is placed in the indicated location.
 * The norm is the square of the absolute value.
 */
void
normvalue(vp, vres)
	VALUE *vp, *vres;
{
	NUMBER *q1, *q2;

	vres->v_type = V_NULL;
	switch (vp->v_type) {
		case V_NUM:
			vres->v_num = qsquare(vp->v_num);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			q1 = qsquare(vp->v_com->real);
			q2 = qsquare(vp->v_com->imag);
			vres->v_num = qadd(q1, q2);
			vres->v_type = V_NUM;
			qfree(q1);
			qfree(q2);
			return;
		case V_OBJ:
			*vres = objcall(OBJ_NORM, vp, NULL_VALUE, NULL_VALUE);
			return;
		default:
			math_error("Illegal value for norm");
	}
}


/*
 * Shift a value left or right by the specified number of bits.
 * Negative shift value means shift the direction opposite the selected dir.
 * Right shifts are defined to lose bits off the low end of the number.
 * Result is placed in the indicated location.
 */
void
shiftvalue(v1, v2, rightshift, vres)
	VALUE *v1, *v2, *vres;
	BOOL rightshift;	/* TRUE if shift right instead of left */
{
	COMPLEX *c;
	long n = 0;
	VALUE tmp;

	if (v2->v_type != V_NUM)
		math_error("Non-real shift value");
 	if (qisfrac(v2->v_num))
		math_error("Non-integral shift value");
	if (v1->v_type != V_OBJ) {
		if (zisbig(v2->v_num->num))
			math_error("Very large shift value");
		n = qtoi(v2->v_num);
	}
	if (rightshift)
		n = -n;
	switch (v1->v_type) {
		case V_NUM:
			vres->v_num = qshift(v1->v_num, n);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			c = cshift(v1->v_com, n);
			if (!cisreal(c)) {
				vres->v_com = c;
				vres->v_type = V_COM;
				return;
			}
			vres->v_num = qlink(c->real);
			vres->v_type = V_NUM;
			comfree(c);
			return;
		case V_MAT:
			vres->v_mat = matshift(v1->v_mat, n);
			vres->v_type = V_MAT;
			return;
		case V_OBJ:
			if (!rightshift) {
				*vres = objcall(OBJ_SHIFT, v1, v2, NULL_VALUE);
				return;
			}
			tmp.v_num = qneg(v2->v_num);
			tmp.v_type = V_NUM;
			*vres = objcall(OBJ_SHIFT, v1, &tmp, NULL_VALUE);
			qfree(tmp.v_num);
			return;
		default:
			math_error("Bad value for shifting");
	}
}


/*
 * Scale a value by a power of two.
 * Result is placed in the indicated location.
 */
void
scalevalue(v1, v2, vres)
	VALUE *v1, *v2, *vres;
{
	long n = 0;

	if (v2->v_type != V_NUM)
		math_error("Non-real scaling factor");
	if (qisfrac(v2->v_num))
		math_error("Non-integral scaling factor");
	if (v1->v_type != V_OBJ) {
		if (zisbig(v2->v_num->num))
			math_error("Very large scaling factor");
		n = qtoi(v2->v_num);
	}
	switch (v1->v_type) {
		case V_NUM:
			vres->v_num = qscale(v1->v_num, n);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			vres->v_com = cscale(v1->v_com, n);
			vres->v_type = V_NUM;
			return;
		case V_MAT:
			vres->v_mat = matscale(v1->v_mat, n);
			vres->v_type = V_MAT;
			return;
		case V_OBJ:
			*vres = objcall(OBJ_SCALE, v1, v2, NULL_VALUE);
			return;
		default:
			math_error("Bad value for scaling");
	}
}


/*
 * Raise a value to an integral power.
 * Result is placed in the indicated location.
 */
void
powivalue(v1, v2, vres)
	VALUE *v1, *v2, *vres;
{
	NUMBER *q;
	COMPLEX *c;

	vres->v_type = V_NULL;
	if (v2->v_type != V_NUM)
		math_error("Raising value to non-real power");
	q = v2->v_num;
	if (qisfrac(q))
		math_error("Raising value to non-integral power");
	switch (v1->v_type) {
		case V_NUM:
			vres->v_num = qpowi(v1->v_num, q);
			vres->v_type = V_NUM;
			return;
		case V_COM:
			vres->v_com = cpowi(v1->v_com, q);
			vres->v_type = V_COM;
			c = vres->v_com;
			if (!cisreal(c))
				return;
			vres->v_num = qlink(c->real);
			vres->v_type = V_NUM;
			comfree(c);
			return;
		case V_MAT:
			vres->v_mat = matpowi(v1->v_mat, q);
			vres->v_type = V_MAT;
			return;
		case V_OBJ:
			*vres = objcall(OBJ_POW, v1, v2, NULL_VALUE);
			return;
		default:
			math_error("Illegal value for raising to integer power");
	}
}


/*
 * Raise one value to another value's power, within the specified error.
 * Result is placed in the indicated location.
 */
void
powervalue(v1, v2, v3, vres)
	VALUE *v1, *v2, *v3, *vres;
{
	NUMBER *epsilon;
	COMPLEX *c, ctmp;

	vres->v_type = V_NULL;
	if (v3->v_type != V_NUM)
		math_error("Non-real epsilon value for power");
	epsilon = v3->v_num;
	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Non-positive epsilon value for power");
	switch (TWOVAL(v1->v_type, v2->v_type)) {
		case TWOVAL(V_NUM, V_NUM):
			vres->v_num = qpower(v1->v_num, v2->v_num, epsilon);
			vres->v_type = V_NUM;
			return;
		case TWOVAL(V_NUM, V_COM):
			ctmp.real = v1->v_num;
			ctmp.imag = &_qzero_;
			ctmp.links = 1;
			vres->v_com = cpower(&ctmp, v2->v_com, epsilon);
			break;
		case TWOVAL(V_COM, V_NUM):
			ctmp.real = v2->v_num;
			ctmp.imag = &_qzero_;
			ctmp.links = 1;
			vres->v_com = cpower(v1->v_com, &ctmp, epsilon);
			break;
		case TWOVAL(V_COM, V_COM):
			vres->v_com = cpower(v1->v_com, v2->v_com, epsilon);
			break;
		default:
			math_error("Illegal value for raising to power");
	}
	/*
	 * Here for any complex result.
	 */
	vres->v_type = V_COM;
	c = vres->v_com;
	if (!cisreal(c))
		return;
	vres->v_num = qlink(c->real);
	vres->v_type = V_NUM;
	comfree(c);
}


/*
 * Divide one arbitrary value by another one.
 * Result is placed in the indicated location.
 */
void
divvalue(v1, v2, vres)
	VALUE *v1, *v2, *vres;
{
	COMPLEX *c;
	COMPLEX ctmp;
	VALUE tmpval;

	vres->v_type = V_NULL;
	switch (TWOVAL(v1->v_type, v2->v_type)) {
		case TWOVAL(V_NUM, V_NUM):
			vres->v_num = qdiv(v1->v_num, v2->v_num);
			vres->v_type = V_NUM;
			return;
		case TWOVAL(V_COM, V_NUM):
			vres->v_com = cdivq(v1->v_com, v2->v_num);
			vres->v_type = V_COM;
			return;
		case TWOVAL(V_NUM, V_COM):
			if (qiszero(v1->v_num)) {
				vres->v_num = qlink(&_qzero_);
				vres->v_type = V_NUM;
				return;
			}
			ctmp.real = v1->v_num;
			ctmp.imag = &_qzero_;
			ctmp.links = 1;
			vres->v_com = cdiv(&ctmp, v2->v_com);
			vres->v_type = V_COM;
			return;
		case TWOVAL(V_COM, V_COM):
			vres->v_com = cdiv(v1->v_com, v2->v_com);
			vres->v_type = V_COM;
			c = vres->v_com;
			if (cisreal(c)) {
				vres->v_num = qlink(c->real);
				vres->v_type = V_NUM;
				comfree(c);
			}
			return;
		case TWOVAL(V_MAT, V_NUM):
		case TWOVAL(V_MAT, V_COM):
			invertvalue(v2, &tmpval);
			vres->v_mat = matmulval(v1->v_mat, &tmpval);
			vres->v_type = V_MAT;
			freevalue(&tmpval);
			return;
		default:
			if ((v1->v_type != V_OBJ) && (v2->v_type != V_OBJ))
				math_error("Non-compatible values for divide");
			*vres = objcall(OBJ_DIV, v1, v2, NULL_VALUE);
			return;
	}
}


/*
 * Divide one arbitrary value by another one keeping only the integer part.
 * Result is placed in the indicated location.
 */
void
quovalue(v1, v2, vres)
	VALUE *v1, *v2, *vres;
{
	COMPLEX *c;

	vres->v_type = V_NULL;
	switch (TWOVAL(v1->v_type, v2->v_type)) {
		case TWOVAL(V_NUM, V_NUM):
			vres->v_num = qquo(v1->v_num, v2->v_num);
			vres->v_type = V_NUM;
			return;
		case TWOVAL(V_COM, V_NUM):
			vres->v_com = cquoq(v1->v_com, v2->v_num);
			vres->v_type = V_COM;
			c = vres->v_com;
			if (cisreal(c)) {
				vres->v_num = qlink(c->real);
				vres->v_type = V_NUM;
				comfree(c);
			}
			return;
		case TWOVAL(V_MAT, V_NUM):
		case TWOVAL(V_MAT, V_COM):
			vres->v_mat = matquoval(v1->v_mat, v2);
			vres->v_type = V_MAT;
			return;
		default:
			if ((v1->v_type != V_OBJ) && (v2->v_type != V_OBJ))
				math_error("Non-compatible values for quotient");
			*vres = objcall(OBJ_QUO, v1, v2, NULL_VALUE);
			return;
	}
}


/*
 * Divide one arbitrary value by another one keeping only the remainder.
 * Result is placed in the indicated location.
 */
void
modvalue(v1, v2, vres)
	VALUE *v1, *v2, *vres;
{
	COMPLEX *c;

	vres->v_type = V_NULL;
	switch (TWOVAL(v1->v_type, v2->v_type)) {
		case TWOVAL(V_NUM, V_NUM):
			vres->v_num = qmod(v1->v_num, v2->v_num);
			vres->v_type = V_NUM;
			return;
		case TWOVAL(V_COM, V_NUM):
			vres->v_com = cmodq(v1->v_com, v2->v_num);
			vres->v_type = V_COM;
			c = vres->v_com;
			if (cisreal(c)) {
				vres->v_num = qlink(c->real);
				vres->v_type = V_NUM;
				comfree(c);
			}
			return;
		case TWOVAL(V_MAT, V_NUM):
		case TWOVAL(V_MAT, V_COM):
			vres->v_mat = matmodval(v1->v_mat, v2);
			vres->v_type = V_MAT;
			return;
		default:
			if ((v1->v_type != V_OBJ) && (v2->v_type != V_OBJ))
				math_error("Non-compatible values for mod");
			*vres = objcall(OBJ_MOD, v1, v2, NULL_VALUE);
			return;
	}
}


/*
 * Test an arbitrary value to see if it is equal to "zero".
 * The definition of zero varies depending on the value type.  For example,
 * the null string is "zero", and a matrix with zero values is "zero".
 * Returns TRUE if value is not equal to zero.
 */
BOOL
testvalue(vp)
	VALUE *vp;
{
	VALUE val;

	switch (vp->v_type) {
		case V_NUM:
			return !qiszero(vp->v_num);
		case V_COM:
			return !ciszero(vp->v_com);
		case V_STR:
			return (vp->v_str[0] != '\0');
		case V_MAT:
			return mattest(vp->v_mat);
		case V_LIST:
			return (vp->v_list->l_count != 0);
		case V_ASSOC:
			return (vp->v_assoc->a_count != 0);
		case V_FILE:
			return validid(vp->v_file);
		case V_NULL:
			return FALSE;
		case V_OBJ:
			val = objcall(OBJ_TEST, vp, NULL_VALUE, NULL_VALUE);
			return (val.v_int != 0);
		default:
			return TRUE;
	}
}


/*
 * Compare two values for equality.
 * Returns TRUE if the two values differ.
 */
BOOL
comparevalue(v1, v2)
	VALUE *v1, *v2;
{
	int r = FALSE;
	VALUE val;

	if ((v1->v_type == V_OBJ) || (v2->v_type == V_OBJ)) {
		val = objcall(OBJ_CMP, v1, v2, NULL_VALUE);
		return (val.v_int != 0);
	}
	if (v1 == v2)
		return FALSE;
	if (v1->v_type != v2->v_type)
		return TRUE;
	switch (v1->v_type) {
		case V_NUM:
			r = qcmp(v1->v_num, v2->v_num);
			break;
		case V_COM:
			r = ccmp(v1->v_com, v2->v_com);
			break;
		case V_STR:
			r = ((v1->v_str != v2->v_str) &&
				((v1->v_str[0] - v2->v_str[0]) ||
				strcmp(v1->v_str, v2->v_str)));
			break;
		case V_MAT:
			r = matcmp(v1->v_mat, v2->v_mat);
			break;
		case V_LIST:
			r = listcmp(v1->v_list, v2->v_list);
			break;
		case V_ASSOC:
			r = assoccmp(v1->v_assoc, v2->v_assoc);
			break;
		case V_NULL:
			break;
		case V_FILE:
			r = (v1->v_file != v2->v_file);
			break;
		default:
			math_error("Illegal values for comparevalue");
	}
	return (r != 0);
}


/*
 * Compare two values for their relative values.
 * Returns minus one if the first value is less than the second one,
 * one if the first value is greater than the second one, and
 * zero if they are equal.
 */
FLAG
relvalue(v1, v2)
	VALUE *v1, *v2;
{
	int r = 0;
	VALUE val;

	if ((v1->v_type == V_OBJ) || (v2->v_type == V_OBJ)) {
		val = objcall(OBJ_REL, v1, v2, NULL_VALUE);
		return val.v_int;
	}
	if (v1 == v2)
		return 0;
	if (v1->v_type != v2->v_type)
		math_error("Relative comparison of differing types");
	switch (v1->v_type) {
		case V_NUM:
			r = qrel(v1->v_num, v2->v_num);
			break;
		case V_STR:
			r = strcmp(v1->v_str, v2->v_str);
			break;
		case V_NULL:
			break;
		default:
			math_error("Illegal value for relative comparison");
	}
	if (r < 0)
		return -1;
	return (r != 0);
}


/*
 * Calculate a hash value for a value.
 * The hash does not have to be a perfect one, it is only used for
 * making associations faster.
 */
HASH
hashvalue(vp)
	VALUE *vp;
{
	switch (vp->v_type) {
		case V_INT:
			return ((long) vp->v_int);
		case V_NUM:
			return qhash(vp->v_num);
		case V_COM:
			return chash(vp->v_com);
		case V_STR:
			return hashstr(vp->v_str);
		case V_NULL:
			return 0;
		case V_OBJ:
			return objhash(vp->v_obj);
		case V_LIST:
			return listhash(vp->v_list);
		case V_ASSOC:
			return assochash(vp->v_assoc);
		case V_MAT:
			return mathash(vp->v_mat);
		case V_FILE:
			return ((long) vp->v_file);
		default:
			math_error("Hashing unknown value");
	}
	return 0;
}


/*
 * Print the value of a descriptor in one of several formats.
 * If flags contains PRINT_SHORT, then elements of arrays and lists
 * will not be printed.  If flags contains PRINT_UNAMBIG, then quotes
 * are placed around strings and the null value is explicitly printed.
 */
void
printvalue(vp, flags)
	VALUE *vp;
	int flags;
{
	switch (vp->v_type) {
		case V_NUM:
			qprintnum(vp->v_num, MODE_DEFAULT);
			break;
		case V_COM:
			comprint(vp->v_com);
			break;
		case V_STR:
			if (flags & PRINT_UNAMBIG)
				math_chr('\"');
			math_str(vp->v_str);
			if (flags & PRINT_UNAMBIG)
				math_chr('\"');
			break;
		case V_NULL:
			if (flags & PRINT_UNAMBIG)
				math_str("NULL");
			break;
		case V_OBJ:
			(void) objcall(OBJ_PRINT, vp, NULL_VALUE, NULL_VALUE);
			break;
		case V_LIST:
			listprint(vp->v_list,
				((flags & PRINT_SHORT) ? 0L : maxprint));
			break;
		case V_ASSOC:
			assocprint(vp->v_assoc,
				((flags & PRINT_SHORT) ? 0L : maxprint));
			break;
		case V_MAT:
			matprint(vp->v_mat,
				((flags & PRINT_SHORT) ? 0L : maxprint));
			break;
		case V_FILE:
			printid(vp->v_file, flags);
			break;
		default:
			math_error("Printing unknown value");
	}
}

/* END CODE */
