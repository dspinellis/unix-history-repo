/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Extended precision rational arithmetic matrix functions.
 * Matrices can contain arbitrary types of elements.
 */

#include "value.h"


static void matswaprow MATH_PROTO((MATRIX *m, long r1, long r2));
static void matsubrow MATH_PROTO((MATRIX *m, long oprow, long baserow,
	VALUE *mulval));
static void matmulrow MATH_PROTO((MATRIX *m, long row, VALUE *mulval));
static MATRIX *matident MATH_PROTO((MATRIX *m));



/*
 * Add two compatible matrices.
 */
MATRIX *
matadd(m1, m2)
	MATRIX *m1, *m2;
{
	int dim;

	long min1, min2, max1, max2, index;
	VALUE *v1, *v2, *vres;
	MATRIX *res;
	MATRIX tmp;

	if (m1->m_dim != m2->m_dim)
		math_error("Incompatible matrix dimensions for add");
	tmp.m_dim = m1->m_dim;
	tmp.m_size = m1->m_size;
	for (dim = 0; dim < m1->m_dim; dim++) {
		min1 = m1->m_min[dim];
		max1 = m1->m_max[dim];
		min2 = m2->m_min[dim];
		max2 = m2->m_max[dim];
		if ((min1 && min2 && (min1 != min2)) || ((max1-min1) != (max2-min2)))
			math_error("Incompatible matrix bounds for add");
		tmp.m_min[dim] = (min1 ? min1 : min2);
		tmp.m_max[dim] = tmp.m_min[dim] + (max1 - min1);
	}
	res = matalloc(m1->m_size);
	*res = tmp;
	v1 = m1->m_table;
	v2 = m2->m_table;
	vres = res->m_table;
	for (index = m1->m_size; index > 0; index--)
		addvalue(v1++, v2++, vres++);
	return res;
}


/*
 * Subtract two compatible matrices.
 */
MATRIX *
matsub(m1, m2)
	MATRIX *m1, *m2;
{
	int dim;
	long min1, min2, max1, max2, index;
	VALUE *v1, *v2, *vres;
	MATRIX *res;
	MATRIX tmp;

	if (m1->m_dim != m2->m_dim)
		math_error("Incompatible matrix dimensions for sub");
	tmp.m_dim = m1->m_dim;
	tmp.m_size = m1->m_size;
	for (dim = 0; dim < m1->m_dim; dim++) {
		min1 = m1->m_min[dim];
		max1 = m1->m_max[dim];
		min2 = m2->m_min[dim];
		max2 = m2->m_max[dim];
		if ((min1 && min2 && (min1 != min2)) || ((max1-min1) != (max2-min2)))
			math_error("Incompatible matrix bounds for sub");
		tmp.m_min[dim] = (min1 ? min1 : min2);
		tmp.m_max[dim] = tmp.m_min[dim] + (max1 - min1);
	}
	res = matalloc(m1->m_size);
	*res = tmp;
	v1 = m1->m_table;
	v2 = m2->m_table;
	vres = res->m_table;
	for (index = m1->m_size; index > 0; index--)
		subvalue(v1++, v2++, vres++);
	return res;
}


/*
 * Produce the negative of a matrix.
 */
MATRIX *
matneg(m)
	MATRIX *m;
{
	register VALUE *val, *vres;
	long index;
	MATRIX *res;

	res = matalloc(m->m_size);
	*res = *m;
	val = m->m_table;
	vres = res->m_table;
	for (index = m->m_size; index > 0; index--)
		negvalue(val++, vres++);
	return res;
}


/*
 * Multiply two compatible matrices.
 */
MATRIX *
matmul(m1, m2)
	MATRIX *m1, *m2;
{
	register MATRIX *res;
	long i1, i2, max1, max2, index, maxindex;
	VALUE *v1, *v2;
	VALUE sum, tmp1, tmp2;

	if ((m1->m_dim != 2) || (m2->m_dim != 2))
		math_error("Matrix dimension must be two for mul");
	if ((m1->m_max[1] - m1->m_min[1]) != (m2->m_max[0] - m2->m_min[0]))
		math_error("Incompatible bounds for matrix mul");
	max1 = (m1->m_max[0] - m1->m_min[0] + 1);
	max2 = (m2->m_max[1] - m2->m_min[1] + 1);
	maxindex = (m1->m_max[1] - m1->m_min[1] + 1);
	res = matalloc(max1 * max2);
	res->m_dim = 2;
	res->m_min[0] = m1->m_min[0];
	res->m_max[0] = m1->m_max[0];
	res->m_min[1] = m2->m_min[1];
	res->m_max[1] = m2->m_max[1];
	for (i1 = 0; i1 < max1; i1++) {
		for (i2 = 0; i2 < max2; i2++) {
			sum.v_num = qlink(&_qzero_);
			sum.v_type = V_NUM;
			v1 = &m1->m_table[i1 * maxindex];
			v2 = &m2->m_table[i2];
			for (index = 0; index < maxindex; index++) {
				mulvalue(v1, v2, &tmp1);
				addvalue(&sum, &tmp1, &tmp2);
				freevalue(&tmp1);
				freevalue(&sum);
				sum = tmp2;
				v1++;
				v2 += max2;
			}
			index = (i1 * max2) + i2;
			res->m_table[index] = sum;
		}
	}
	return res;
}


/*
 * Square a matrix.
 */
MATRIX *
matsquare(m)
	MATRIX *m;
{
	register MATRIX *res;
	long i1, i2, max, index;
	VALUE *v1, *v2;
	VALUE sum, tmp1, tmp2;

	if (m->m_dim != 2)
		math_error("Matrix dimension must be two for square");
	if ((m->m_max[0] - m->m_min[0]) != (m->m_max[1] - m->m_min[1]))
		math_error("Squaring non-square matrix");
	max = (m->m_max[0] - m->m_min[0] + 1);
	res = matalloc(max * max);
	res->m_dim = 2;
	res->m_min[0] = m->m_min[0];
	res->m_max[0] = m->m_max[0];
	res->m_min[1] = m->m_min[1];
	res->m_max[1] = m->m_max[1];
	for (i1 = 0; i1 < max; i1++) {
		for (i2 = 0; i2 < max; i2++) {
			sum.v_num = qlink(&_qzero_);
			sum.v_type = V_NUM;
			v1 = &m->m_table[i1 * max];
			v2 = &m->m_table[i2];
			for (index = 0; index < max; index++) {
				mulvalue(v1, v2, &tmp1);
				addvalue(&sum, &tmp1, &tmp2);
				freevalue(&tmp1);
				freevalue(&sum);
				sum = tmp2;
				v1++;
				v2 += max;
			}
			index = (i1 * max) + i2;
			res->m_table[index] = sum;
		}
	}
	return res;
}


/*
 * Compute the result of raising a square matrix to an integer power.
 * Negative powers mean the positive power of the inverse.
 * Note: This calculation could someday be improved for large powers
 * by using the characteristic polynomial of the matrix.
 */
MATRIX *
matpowi(m, q)
	MATRIX *m;		/* matrix to be raised */
	NUMBER *q;		/* power to raise it to */
{
	MATRIX *res, *tmp;
	long power;		/* power to raise to */
	unsigned long bit;	/* current bit value */

	if (m->m_dim != 2)
		math_error("Matrix dimension must be two for power");
	if ((m->m_max[0] - m->m_min[0]) != (m->m_max[1] - m->m_min[1]))
		math_error("Raising non-square matrix to a power");
	if (qisfrac(q))
		math_error("Raising matrix to non-integral power");
	if (zisbig(q->num))
		math_error("Raising matrix to very large power");
	power = (zistiny(q->num) ? z1tol(q->num) : z2tol(q->num));
	if (qisneg(q))
		power = -power;
	/*
	 * Handle some low powers specially
	 */
	if ((power <= 4) && (power >= -2)) {
		switch ((int) power) {
			case 0:
				return matident(m);
			case 1:
				return matcopy(m);
			case -1:
				return matinv(m);
			case 2:
				return matsquare(m);
			case -2:
				tmp = matinv(m);
				res = matsquare(tmp);
				matfree(tmp);
				return res;
			case 3:
				tmp = matsquare(m);
				res = matmul(m, tmp);
				matfree(tmp);
				return res;
			case 4:
				tmp = matsquare(m);
				res = matsquare(tmp);
				matfree(tmp);
				return res;
		}
	}
	if (power < 0) {
		m = matinv(m);
		power = -power;
	}
	/*
	 * Compute the power by squaring and multiplying.
	 * This uses the left to right method of power raising.
	 */
	bit = TOPFULL;
	while ((bit & power) == 0)
		bit >>= 1L;
	bit >>= 1L;
	res = matsquare(m);
	if (bit & power) {
		tmp = matmul(res, m);
		matfree(res);
		res = tmp;
	}
	bit >>= 1L;
	while (bit) {
		tmp = matsquare(res);
		matfree(res);
		res = tmp;
		if (bit & power) {
			tmp = matmul(res, m);
			matfree(res);
			res = tmp;
		}
		bit >>= 1L;
	}
	if (qisneg(q))
		matfree(m);
	return res;
}


/*
 * Calculate the cross product of two one dimensional matrices each
 * with three components.
 *	m3 = matcross(m1, m2);
 */
MATRIX *
matcross(m1, m2)
	MATRIX *m1, *m2;
{
	MATRIX *res;
	VALUE *v1, *v2, *vr;
	VALUE tmp1, tmp2;

	if ((m1->m_dim != 1) || (m2->m_dim != 1))
		math_error("Matrix not 1d for cross product");
	if ((m1->m_size != 3) || (m2->m_size != 3))
		math_error("Matrix not size 3 for cross product");
	res = matalloc(3L);
	res->m_dim = 1;
	res->m_min[0] = 0;
	res->m_max[0] = 2;
	v1 = m1->m_table;
	v2 = m2->m_table;
	vr = res->m_table;
	mulvalue(v1 + 1, v2 + 2, &tmp1);
	mulvalue(v1 + 2, v2 + 1, &tmp2);
	subvalue(&tmp1, &tmp2, vr + 0);
	freevalue(&tmp1);
	freevalue(&tmp2);
	mulvalue(v1 + 2, v2 + 0, &tmp1);
	mulvalue(v1 + 0, v2 + 2, &tmp2);
	subvalue(&tmp1, &tmp2, vr + 1);
	freevalue(&tmp1);
	freevalue(&tmp2);
	mulvalue(v1 + 0, v2 + 1, &tmp1);
	mulvalue(v1 + 1, v2 + 0, &tmp2);
	subvalue(&tmp1, &tmp2, vr + 2);
	freevalue(&tmp1);
	freevalue(&tmp2);
	return res;
}


/*
 * Return the dot product of two matrices.
 *	result = matdot(m1, m2);
 */
VALUE
matdot(m1, m2)
	MATRIX *m1, *m2;
{
	VALUE *v1, *v2;
	VALUE result, tmp1, tmp2;
	long len;

	if ((m1->m_dim != 1) || (m2->m_dim != 1))
		math_error("Matrix not 1d for dot product");
	if (m1->m_size != m2->m_size)
		math_error("Incompatible matrix sizes for dot product");
	v1 = m1->m_table;
	v2 = m2->m_table;
	mulvalue(v1, v2, &result);
	len = m1->m_size;
	while (--len > 0) {
		mulvalue(++v1, ++v2, &tmp1);
		addvalue(&result, &tmp1, &tmp2);
		freevalue(&tmp1);
		freevalue(&result);
		result = tmp2;
	}
	return result;
}


/*
 * Scale the elements of a matrix by a specified power of two.
 */
MATRIX *
matscale(m, n)
	MATRIX *m;		/* matrix to be scaled */
	long n;
{
	register VALUE *val, *vres;
	VALUE num;
	long index;
	MATRIX *res;		/* resulting matrix */

	if (n == 0)
		return matcopy(m);
	num.v_type = V_NUM;
	num.v_num = itoq(n);
	res = matalloc(m->m_size);
	*res = *m;
	val = m->m_table;
	vres = res->m_table;
	for (index = m->m_size; index > 0; index--)
		scalevalue(val++, &num, vres++);
	qfree(num.v_num);
	return res;
}


/*
 * Shift the elements of a matrix by the specified number of bits.
 * Positive shift means leftwards, negative shift rightwards.
 */
MATRIX *
matshift(m, n)
	MATRIX *m;		/* matrix to be scaled */
	long n;
{
	register VALUE *val, *vres;
	VALUE num;
	long index;
	MATRIX *res;		/* resulting matrix */

	if (n == 0)
		return matcopy(m);
	num.v_type = V_NUM;
	num.v_num = itoq(n);
	res = matalloc(m->m_size);
	*res = *m;
	val = m->m_table;
	vres = res->m_table;
	for (index = m->m_size; index > 0; index--)
		shiftvalue(val++, &num, FALSE, vres++);
	qfree(num.v_num);
	return res;
}


/*
 * Multiply the elements of a matrix by a specified value.
 */
MATRIX *
matmulval(m, vp)
	MATRIX *m;		/* matrix to be multiplied */
	VALUE *vp;		/* value to multiply by */
{
	register VALUE *val, *vres;
	long index;
	MATRIX *res;

	res = matalloc(m->m_size);
	*res = *m;
	val = m->m_table;
	vres = res->m_table;
	for (index = m->m_size; index > 0; index--)
		mulvalue(val++, vp, vres++);
	return res;
}


/*
 * Divide the elements of a matrix by a specified value, keeping
 * only the integer quotient.
 */
MATRIX *
matquoval(m, vp)
	MATRIX *m;		/* matrix to be divided */
	VALUE *vp;		/* value to divide by */
{
	register VALUE *val, *vres;
	long index;
	MATRIX *res;

	if ((vp->v_type == V_NUM) && qiszero(vp->v_num))
		math_error("Division by zero");
	res = matalloc(m->m_size);
	*res = *m;
	val = m->m_table;
	vres = res->m_table;
	for (index = m->m_size; index > 0; index--)
		quovalue(val++, vp, vres++);
	return res;
}


/*
 * Divide the elements of a matrix by a specified value, keeping
 * only the remainder of the division.
 */
MATRIX *
matmodval(m, vp)
	MATRIX *m;		/* matrix to be divided */
	VALUE *vp;		/* value to divide by */
{
	register VALUE *val, *vres;
	long index;
	MATRIX *res;

	if ((vp->v_type == V_NUM) && qiszero(vp->v_num))
		math_error("Division by zero");
	res = matalloc(m->m_size);
	*res = *m;
	val = m->m_table;
	vres = res->m_table;
	for (index = m->m_size; index > 0; index--)
		modvalue(val++, vp, vres++);
	return res;
}


MATRIX *
mattrans(m)
	MATRIX *m;			/* matrix to be transposed */
{
	register VALUE *v1, *v2;	/* current values */
	long rows, cols;		/* rows and columns in new matrix */
	long row, col;			/* current row and column */
	MATRIX *res;

	if (m->m_dim != 2)
		math_error("Matrix dimension must be two for transpose");
	res = matalloc(m->m_size);
	res->m_dim = 2;
	res->m_min[0] = m->m_min[1];
	res->m_max[0] = m->m_max[1];
	res->m_min[1] = m->m_min[0];
	res->m_max[1] = m->m_max[0];
	rows = (m->m_max[1] - m->m_min[1] + 1);
	cols = (m->m_max[0] - m->m_min[0] + 1);
	v1 = res->m_table;
	for (row = 0; row < rows; row++) {
		v2 = &m->m_table[row];
		for (col = 0; col < cols; col++) {
			copyvalue(v2, v1);
			v1++;
			v2 += rows;
		}
	}
	return res;
}


/*
 * Produce a matrix with values all of which are conjugated.
 */
MATRIX *
matconj(m)
	MATRIX *m;
{
	register VALUE *val, *vres;
	long index;
	MATRIX *res;

	res = matalloc(m->m_size);
	*res = *m;
	val = m->m_table;
	vres = res->m_table;
	for (index = m->m_size; index > 0; index--)
		conjvalue(val++, vres++);
	return res;
}


/*
 * Produce a matrix with values all of which have been rounded to the
 * specified number of decimal places.
 */
MATRIX *
matround(m, places)
	MATRIX *m;
	long places;
{
	register VALUE *val, *vres;
	VALUE tmp;
	long index;
	MATRIX *res;

	if (places < 0)
		math_error("Negative number of places for matround");
	res = matalloc(m->m_size);
	*res = *m;
	tmp.v_type = V_INT;
	tmp.v_int = places;
	val = m->m_table;
	vres = res->m_table;
	for (index = m->m_size; index > 0; index--)
		roundvalue(val++, &tmp, vres++);
	return res;
}


/*
 * Produce a matrix with values all of which have been rounded to the
 * specified number of binary places.
 */
MATRIX *
matbround(m, places)
	MATRIX *m;
	long places;
{
	register VALUE *val, *vres;
	VALUE tmp;
	long index;
	MATRIX *res;

	if (places < 0)
		math_error("Negative number of places for matbround");
	res = matalloc(m->m_size);
	*res = *m;
	tmp.v_type = V_INT;
	tmp.v_int = places;
	val = m->m_table;
	vres = res->m_table;
	for (index = m->m_size; index > 0; index--)
		broundvalue(val++, &tmp, vres++);
	return res;
}


/*
 * Produce a matrix with values all of which have been truncated to integers.
 */
MATRIX *
matint(m)
	MATRIX *m;
{
	register VALUE *val, *vres;
	long index;
	MATRIX *res;

	res = matalloc(m->m_size);
	*res = *m;
	val = m->m_table;
	vres = res->m_table;
	for (index = m->m_size; index > 0; index--)
		intvalue(val++, vres++);
	return res;
}


/*
 * Produce a matrix with values all of which have only the fraction part left.
 */
MATRIX *
matfrac(m)
	MATRIX *m;
{
	register VALUE *val, *vres;
	long index;
	MATRIX *res;

	res = matalloc(m->m_size);
	*res = *m;
	val = m->m_table;
	vres = res->m_table;
	for (index = m->m_size; index > 0; index--)
		fracvalue(val++, vres++);
	return res;
}


/*
 * Index a matrix normally by the specified set of index values.
 * Returns the address of the matrix element if it is valid, or generates
 * an error if the index values are out of range.  The create flag is TRUE
 * if the element is to be written, but this is ignored here.
 */
/*ARGSUSED*/
VALUE *
matindex(mp, create, dim, indices)
	MATRIX *mp;
	BOOL create;
	long dim;		/* dimension of the indexing */
	VALUE *indices;		/* table of values being indexed by */
{
	NUMBER *q;		/* index value */
	long index;		/* index value as an integer */
	long offset;		/* current offset into array */
	int i;			/* loop counter */

	if ((dim <= 0) || (dim > MAXDIM))
		math_error("Bad dimension %ld for matrix", dim);
	if (mp->m_dim != dim)
		math_error("Indexing a %ldd matrix as a %ldd matrix", mp->m_dim, dim);
	offset = 0;
	for (i = 0; i < dim; i++) {
		if (indices->v_type != V_NUM)
			math_error("Non-numeric index for matrix");
		q = indices->v_num;
		if (qisfrac(q))
			math_error("Non-integral index for matrix");
		index = qtoi(q);
		if (zisbig(q->num) || (index < mp->m_min[i]) || (index > mp->m_max[i]))
			math_error("Index out of bounds for matrix");
		offset *= (mp->m_max[i] - mp->m_min[i] + 1);
		offset += (index - mp->m_min[i]);
		indices++;
	}
	return mp->m_table + offset;
}


/*
 * Search a matrix for the specified value, starting with the specified index.
 * Returns the index of the found value, or -1 if the value was not found.
 */
long
matsearch(m, vp, index)
	MATRIX *m;
	VALUE *vp;
	long index;
{
	register VALUE *val;

	if (index < 0)
		index = 0;
	val = &m->m_table[index];
	while (index < m->m_size) {
		if (!comparevalue(vp, val))
			return index;
		index++;
		val++;
	}
	return -1;
}


/*
 * Search a matrix backwards for the specified value, starting with the
 * specified index.  Returns the index of the found value, or -1 if the
 * value was not found.
 */
long
matrsearch(m, vp, index)
	MATRIX *m;
	VALUE *vp;
	long index;
{
	register VALUE *val;

	if (index >= m->m_size)
		index = m->m_size - 1;
	val = &m->m_table[index];
	while (index >= 0) {
		if (!comparevalue(vp, val))
			return index;
		index--;
		val--;
	}
	return -1;
}


/*
 * Fill all of the elements of a matrix with one of two specified values.
 * All entries are filled with the first specified value, except that if
 * the matrix is square and the second value pointer is non-NULL, then
 * all diagonal entries are filled with the second value.  This routine
 * affects the supplied matrix directly, and doesn't return a copy.
 */
void
matfill(m, v1, v2)
	MATRIX *m;		/* matrix to be filled */
	VALUE *v1;		/* value to fill most of matrix with */
	VALUE *v2;		/* value for diagonal entries (or NULL) */
{
	register VALUE *val;
	long row, col;
	long rows;
	long index;

	if (v2 && ((m->m_dim != 2) ||
		((m->m_max[0] - m->m_min[0]) != (m->m_max[1] - m->m_min[1]))))
			math_error("Filling diagonals of non-square matrix");
	val = m->m_table;
	for (index = m->m_size; index > 0; index--)
		freevalue(val++);
	val = m->m_table;
	if (v2 == NULL) {
		for (index = m->m_size; index > 0; index--)
			copyvalue(v1, val++);
		return;
	}
	rows = m->m_max[0] - m->m_min[0] + 1;
	for (row = 0; row < rows; row++) {
		for (col = 0; col < rows; col++) {
			copyvalue(((row != col) ? v1 : v2), val++);
		}
	}
}


/*
 * Set a copy of a square matrix to the identity matrix.
 */
static MATRIX *
matident(m)
	MATRIX *m;
{
	register VALUE *val;	/* current value */
	long row, col;		/* current row and column */
	long rows;		/* number of rows */
	MATRIX *res;		/* resulting matrix */

	if (m->m_dim != 2)
		math_error("Matrix dimension must be two for setting to identity");
	if ((m->m_max[0] - m->m_min[0]) != (m->m_max[1] - m->m_min[1]))
		math_error("Matrix must be square for setting to identity");
	res = matalloc(m->m_size);
	*res = *m;
	val = res->m_table;
	rows = (res->m_max[0] - res->m_min[0] + 1);
	for (row = 0; row < rows; row++) {
		for (col = 0; col < rows; col++) {
			val->v_type = V_NUM;
			val->v_num = ((row == col) ? qlink(&_qone_) : qlink(&_qzero_));
			val++;
		}
	}
	return res;
}


/*
 * Calculate the inverse of a matrix if it exists.
 * This is done by using transformations on the supplied matrix to convert
 * it to the identity matrix, and simultaneously applying the same set of
 * transformations to the identity matrix.
 */
MATRIX *
matinv(m)
	MATRIX *m;
{
	MATRIX *res;		/* matrix to become the inverse */
	long rows;		/* number of rows */
	long cur;		/* current row being worked on */
	long row, col;		/* temp row and column values */
	VALUE *val;		/* current value in matrix*/
	VALUE mulval;		/* value to multiply rows by */
	VALUE tmpval;		/* temporary value */

	if (m->m_dim != 2)
		math_error("Matrix dimension must be two for inverse");
	if ((m->m_max[0] - m->m_min[0]) != (m->m_max[1] - m->m_min[1]))
		math_error("Inverting non-square matrix");
	/*
	 * Begin by creating the identity matrix with the same attributes.
	 */
	res = matalloc(m->m_size);
	*res = *m;
	rows = (m->m_max[0] - m->m_min[0] + 1);
	val = res->m_table;
	for (row = 0; row < rows; row++) {
		for (col = 0; col < rows; col++) {
			if (row == col)
				val->v_num = qlink(&_qone_);
			else
				val->v_num = qlink(&_qzero_);
			val->v_type = V_NUM;
			val++;
		}
	}
	/*
	 * Now loop over each row, and eliminate all entries in the
	 * corresponding column by using row operations.  Do the same
	 * operations on the resulting matrix.  Copy the original matrix
	 * so that we don't destroy it.
	 */
	m = matcopy(m);
	for (cur = 0; cur < rows; cur++) {
		/*
		 * Find the first nonzero value in the rest of the column
		 * downwards from [cur,cur].  If there is no such value, then
		 * the matrix is not invertible.  If the first nonzero entry
		 * is not the current row, then swap the two rows to make the
		 * current one nonzero.
		 */
		row = cur;
		val = &m->m_table[(row * rows) + row];
		while (testvalue(val) == 0) {
			if (++row >= rows) {
				matfree(m);
				matfree(res);
				math_error("Matrix is not invertible");
			}
			val += rows;
		}
		invertvalue(val, &mulval);
		if (row != cur) {
			matswaprow(m, row, cur);
			matswaprow(res, row, cur);
		}
		/*
		 * Now for every other nonzero entry in the current column, subtract
		 * the appropriate multiple of the current row to force that entry
		 * to become zero.
		 */
		val = &m->m_table[cur];
		/* ignore Saber-C warning about bad pointer val */
		for (row = 0; row < rows; row++, val += rows) {
			if ((row == cur) || (testvalue(val) == 0))
				continue;
			mulvalue(val, &mulval, &tmpval);
			matsubrow(m, row, cur, &tmpval);
			matsubrow(res, row, cur, &tmpval);
			freevalue(&tmpval);
		}
		freevalue(&mulval);
	}
	/*
	 * Now the original matrix has nonzero entries only on its main diagonal.
	 * Scale the rows of the result matrix by the inverse of those entries.
	 */
	val = m->m_table;
	for (row = 0; row < rows; row++) {
		if ((val->v_type != V_NUM) || !qisone(val->v_num)) {
			invertvalue(val, &mulval);
			matmulrow(res, row, &mulval);
			freevalue(&mulval);
		}
		/* ignore Saber-C warning about bad pointer val */
		val += (rows + 1);
	}
	matfree(m);
	return res;
}


/*
 * Calculate the determinant of a square matrix.
 * This is done using row operations to create an upper-diagonal matrix.
 */
VALUE
matdet(m)
	MATRIX *m;
{
	long rows;		/* number of rows */
	long cur;		/* current row being worked on */
	long row;		/* temp row values */
	int neg;		/* whether to negate determinant */
	VALUE *val;		/* current value */
	VALUE mulval, tmpval;	/* other values */

	if (m->m_dim != 2)
		math_error("Matrix dimension must be two for determinant");
	if ((m->m_max[0] - m->m_min[0]) != (m->m_max[1] - m->m_min[1]))
		math_error("Non-square matrix for determinant");
	/*
	 * Loop over each row, and eliminate all lower entries in the
	 * corresponding column by using row operations.  Copy the original
	 * matrix so that we don't destroy it.
	 */
	neg = 0;
	m = matcopy(m);
	rows = (m->m_max[0] - m->m_min[0] + 1);
	for (cur = 0; cur < rows; cur++) {
		/*
		 * Find the first nonzero value in the rest of the column
		 * downwards from [cur,cur].  If there is no such value, then
		 * the determinant is zero.  If the first nonzero entry is not
		 * the current row, then swap the two rows to make the current
		 * one nonzero, and remember that the determinant changes sign.
		 */
		row = cur;
		val = &m->m_table[(row * rows) + row];
		while (testvalue(val) == 0) {
			if (++row >= rows) {
				matfree(m);
				mulval.v_type = V_NUM;
				mulval.v_num = qlink(&_qzero_);
				return mulval;
			}
			val += rows;
		}
		invertvalue(val, &mulval);
		if (row != cur) {
			matswaprow(m, row, cur);
			neg = !neg;
		}
		/*
		 * Now for every other nonzero entry lower down in the current column,
		 * subtract the appropriate multiple of the current row to force that
		 * entry to become zero.
		 */
		row = cur + 1;
		/* ignore Saber-C warning about bad pointer into val */
		val = &m->m_table[(row * rows) + cur];
		/* ignore Saber-C warning about bad pointer into val */
		for (; row < rows; row++, val += rows) {
			if (testvalue(val) == 0)
				continue;
			mulvalue(val, &mulval, &tmpval);
			matsubrow(m, row, cur, &tmpval);
			freevalue(&tmpval);
		}
		freevalue(&mulval);
	}
	/*
	 * Now the matrix is upper-diagonal, and the determinant is the
	 * product of the main diagonal entries, and is possibly negated.
	 */
	val = m->m_table;
	mulval.v_type = V_NUM;
	mulval.v_num = qlink(&_qone_);
	for (row = 0; row < rows; row++) {
		mulvalue(&mulval, val, &tmpval);
		freevalue(&mulval);
		mulval = tmpval;
		/* ignore Saber-C warning about bad pointer into val */
		val += (rows + 1);
	}
	matfree(m);
	if (neg) {
		negvalue(&mulval, &tmpval);
		freevalue(&mulval);
		return tmpval;
	}
	return mulval;
}


/*
 * Local utility routine to swap two rows of a square matrix.
 * No checks are made to verify the legality of the arguments.
 */
static void
matswaprow(m, r1, r2)
	MATRIX *m;
	long r1, r2;
{
	register VALUE *v1, *v2;
	register long rows;
	VALUE tmp;

	if (r1 == r2)
		return;
	rows = (m->m_max[0] - m->m_min[0] + 1);
	v1 = &m->m_table[r1 * rows];
	v2 = &m->m_table[r2 * rows];
	while (rows-- > 0) {
		tmp = *v1;
		*v1 = *v2;
		*v2 = tmp;
		v1++;
		v2++;
	}
}


/*
 * Local utility routine to subtract a multiple of one row to another one.
 * The row to be changed is oprow, the row to be subtracted is baserow.
 * No checks are made to verify the legality of the arguments.
 */
static void
matsubrow(m, oprow, baserow, mulval)
	MATRIX *m;
	long oprow, baserow;
	VALUE *mulval;
{
	register VALUE *vop, *vbase;
	register long entries;
	VALUE tmp1, tmp2;

	entries = (m->m_max[0] - m->m_min[0] + 1);
	vop = &m->m_table[oprow * entries];
	vbase = &m->m_table[baserow * entries];
	while (entries-- > 0) {
		mulvalue(vbase, mulval, &tmp1);
		subvalue(vop, &tmp1, &tmp2);
		freevalue(&tmp1);
		freevalue(vop);
		*vop = tmp2;
		vop++;
		vbase++;
	}
}


/*
 * Local utility routine to multiply a row by a specified number.
 * No checks are made to verify the legality of the arguments.
 */
static void
matmulrow(m, row, mulval)
	MATRIX *m;
	long row;
	VALUE *mulval;
{
	register VALUE *val;
	register long rows;
	VALUE tmp;

	rows = (m->m_max[0] - m->m_min[0] + 1);
	val = &m->m_table[row * rows];
	while (rows-- > 0) {
		mulvalue(val, mulval, &tmp);
		freevalue(val);
		*val = tmp;
		val++;
	}
}


/*
 * Make a full copy of a matrix.
 */
MATRIX *
matcopy(m)
	MATRIX *m;
{
	MATRIX *res;
	register VALUE *v1, *v2;
	register long i;

	res = matalloc(m->m_size);
	*res = *m;
	v1 = m->m_table;
	v2 = res->m_table;
	i = m->m_size;
	while (i-- > 0) {
		if (v1->v_type == V_NUM) {
			v2->v_type = V_NUM;
			v2->v_num = qlink(v1->v_num);
		} else
			copyvalue(v1, v2);
		v1++;
		v2++;
	}
	return res;
}


/*
 * Allocate a matrix with the specified number of elements.
 */
MATRIX *
matalloc(size)
	long size;
{
	MATRIX *m;

	m = (MATRIX *) malloc(matsize(size));
	if (m == NULL)
		math_error("Cannot get memory to allocate matrix of size %d", size);
	m->m_size = size;
	return m;
}


/*
 * Free a matrix, along with all of its element values.
 */
void
matfree(m)
	MATRIX *m;
{
	register VALUE *vp;
	register long i;

	vp = m->m_table;
	i = m->m_size;
	while (i-- > 0) {
		if (vp->v_type == V_NUM) {
			vp->v_type = V_NULL;
			qfree(vp->v_num);
		} else
			freevalue(vp);
		vp++;
	}
	free(m);
}


/*
 * Test whether a matrix has any nonzero values.
 * Returns TRUE if so.
 */
BOOL
mattest(m)
	MATRIX *m;
{
	register VALUE *vp;
	register long i;

	vp = m->m_table;
	i = m->m_size;
	while (i-- > 0) {
		if ((vp->v_type != V_NUM) || (!qiszero(vp->v_num)))
			return TRUE;
		vp++;
	}
	return FALSE;
}


/*
 * Test whether or not two matrices are equal.
 * Equality is determined by the shape and values of the matrices,
 * but not by their index bounds.  Returns TRUE if they differ.
 */
BOOL
matcmp(m1, m2)
	MATRIX *m1, *m2;
{
	VALUE *v1, *v2;
	long i;

	if (m1 == m2)
		return FALSE;
	if ((m1->m_dim != m2->m_dim) || (m1->m_size != m2->m_size))
		return TRUE;
	for (i = 0; i < m1->m_dim; i++) {
		if ((m1->m_max[i] - m1->m_min[i]) != (m2->m_max[i] - m2->m_min[i]))
		return TRUE;
	}
	v1 = m1->m_table;
	v2 = m2->m_table;
	i = m1->m_size;
	while (i-- > 0) {
		if (comparevalue(v1++, v2++))
			return TRUE;
	}
	return FALSE;
}


#if 0
/*
 * Test whether or not a matrix is the identity matrix.
 * Returns TRUE if so.
 */
BOOL
matisident(m)
	MATRIX *m;
{
	register VALUE *val;	/* current value */
	long row, col;		/* row and column numbers */

	if ((m->m_dim != 2) ||
		((m->m_max[0] - m->m_min[0]) != (m->m_max[1] - m->m_min[1])))
			return FALSE;
	val = m->m_table;
	for (row = 0; row < m->m_size; row++) {
		for (col = 0; col < m->m_size; col++) {
			if (val->v_type != V_NUM)
				return FALSE;
			if (row == col) {
				if (!qisone(val->v_num))
					return FALSE;
			} else {
				if (!qiszero(val->v_num))
					return FALSE;
			}
			val++;
		}
	}
	return TRUE;
}
#endif


/*
 * Return a trivial hash value for a matrix.
 */
HASH
mathash(m)
	MATRIX *m;
{
	HASH hash;
	long fullsize;
	long skip;
	int i;
	VALUE *vp;

	hash = m->m_dim * 500009;
	fullsize = 1;
	for (i = m->m_dim - 1; i >= 0; i--) {
		hash = hash * 500029 + m->m_max[i];
		fullsize *= (m->m_max[i] - m->m_min[i] + 1);
	}
	hash = hash * 500041 + fullsize;
	vp = m->m_table;
	for (i = 0; ((i < fullsize) && (i < 16)); i++)
		hash = hash * 500057 + hashvalue(vp++);
	i = 16;
	vp = &m->m_table[16];
	skip = (fullsize / 11) + 1;
	while (i < fullsize) {
		hash = hash * 500069 + hashvalue(vp);
		i += skip;
		vp += skip;
	}
	return hash;
}


/*
 * Print a matrix and possibly few of its elements.
 * The argument supplied specifies how many elements to allow printing.
 * If any elements are printed, they are printed in short form.
 */
void
matprint(m, max_print)
	MATRIX *m;
	long max_print;
{
	VALUE *vp;
	long fullsize, count, index, num;
	int dim, i;
	char *msg;
	long sizes[MAXDIM];

	dim = m->m_dim;
	fullsize = 1;
	for (i = dim - 1; i >= 0; i--) {
		sizes[i] = fullsize;
		fullsize *= (m->m_max[i] - m->m_min[i] + 1);
	}
	msg = ((max_print > 0) ? "\nmat [" : "mat [");
	for (i = 0; i < dim; i++) {
		if (m->m_min[i])
			math_fmt("%s%ld:%ld", msg, m->m_min[i], m->m_max[i]);
		else
			math_fmt("%s%ld", msg, m->m_max[i] + 1);
		msg = ",";
	}
	if (max_print > fullsize)
		max_print = fullsize;
	vp = m->m_table;
	count = 0;
	for (index = 0; index < fullsize; index++) {
		if ((vp->v_type != V_NUM) || !qiszero(vp->v_num))
			count++;
		vp++;
	}
	math_fmt("] (%ld element%s, %ld nonzero)",
		fullsize, (fullsize == 1) ? "" : "s", count);
	if (max_print <= 0)
		return;

	/*
	 * Now print the first few elements of the matrix in short
	 * and unambigous format.
	 */
	math_str(":\n");
	vp = m->m_table;
	for (index = 0; index < max_print; index++) {
		msg = "  [";
		num = index;
		for (i = 0; i < dim; i++) {
			math_fmt("%s%ld", msg, m->m_min[i] + (num / sizes[i]));
			num %= sizes[i];
			msg = ",";
		}
		math_str("] = ");
		printvalue(vp++, PRINT_SHORT | PRINT_UNAMBIG);
		math_str("\n");
	}
	if (max_print < fullsize)
		math_str("  ...\n");
}

/* END CODE */
