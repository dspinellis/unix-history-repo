/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Definitions of general values and related routines used by the calculator.
 */

#ifndef	VALUE_H
#define	VALUE_H

#include "cmath.h"


#define MAXDIM		4	/* maximum number of dimensions in matrices */
#define USUAL_ELEMENTS	4	/* usual number of elements for objects */


/*
 * Flags to modify results from the printvalue routine.
 * These flags are OR'd together.
 */
#define	PRINT_NORMAL	0x00	/* print in normal manner */
#define	PRINT_SHORT	0x01	/* print in short format (no elements) */
#define	PRINT_UNAMBIG	0x02	/* print in non-ambiguous manner */


/*
 * Definition of values of various types.
 */
typedef struct value VALUE;
typedef struct object OBJECT;
typedef struct matrix MATRIX;
typedef struct list LIST;
typedef	struct assoc ASSOC;
typedef	long FILEID;


struct value {
	short v_type;			/* type of value */
	short v_subtype;		/* other data related to some types */
	union {
		long vv_int;		/* small integer value */
		FILEID vv_file;		/* id of opened file */
		NUMBER *vv_num;		/* arbitrary sized numeric value */
		COMPLEX *vv_com;	/* complex number */
		VALUE *vv_addr;		/* address of variable value */
		MATRIX *vv_mat;		/* address of matrix */
		LIST *vv_list;		/* address of list */
		ASSOC *vv_assoc;	/* address of association */
		OBJECT *vv_obj;		/* address of object */
		char *vv_str;		/* string value */
	} v_union;
};


/*
 * For ease in referencing
 */
#define v_int	v_union.vv_int
#define	v_file	v_union.vv_file
#define v_num	v_union.vv_num
#define v_com	v_union.vv_com
#define v_addr	v_union.vv_addr
#define v_str	v_union.vv_str
#define v_mat	v_union.vv_mat
#define	v_list	v_union.vv_list
#define	v_assoc	v_union.vv_assoc
#define v_obj	v_union.vv_obj
#define	v_valid	v_union.vv_int


/*
 * Value types.
 */
#define V_NULL	0	/* null value */
#define V_INT	1	/* normal integer */
#define V_NUM	2	/* number */
#define V_COM	3	/* complex number */
#define V_ADDR	4	/* address of variable value */
#define V_STR	5	/* address of string */
#define V_MAT	6	/* address of matrix structure */
#define	V_LIST	7	/* address of list structure */
#define	V_ASSOC	8	/* address of association structure */
#define V_OBJ	9	/* address of object structure */
#define	V_FILE	10	/* opened file id */
#define V_MAX	10	/* highest legal value */

#define V_NOSUBTYPE	0	/* subtype has no meaning */
#define V_STRLITERAL	1	/* string subtype for literal str */
#define V_STRALLOC	2	/* string subtype for allocated str */

#define TWOVAL(a,b) ((a) * (V_MAX+1) + (b))	/* for switch of two values */

#define	NULL_VALUE	((VALUE *) 0)


extern void freevalue MATH_PROTO((VALUE *vp));
extern void copyvalue MATH_PROTO((VALUE *vp, VALUE *vres));
extern void negvalue MATH_PROTO((VALUE *vp, VALUE *vres));
extern void addvalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *vres));
extern void subvalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *vres));
extern void mulvalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *vres));
extern void squarevalue MATH_PROTO((VALUE *vp, VALUE *vres));
extern void invertvalue MATH_PROTO((VALUE *vp, VALUE *vres));
extern void roundvalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *vres));
extern void broundvalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *vres));
extern void intvalue MATH_PROTO((VALUE *vp, VALUE *vres));
extern void fracvalue MATH_PROTO((VALUE *vp, VALUE *vres));
extern void incvalue MATH_PROTO((VALUE *vp, VALUE *vres));
extern void decvalue MATH_PROTO((VALUE *vp, VALUE *vres));
extern void conjvalue MATH_PROTO((VALUE *vp, VALUE *vres));
extern void sqrtvalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *vres));
extern void rootvalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *v3,
	VALUE *vres));
extern void absvalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *vres));
extern void normvalue MATH_PROTO((VALUE *vp, VALUE *vres));
extern void shiftvalue MATH_PROTO((VALUE *v1, VALUE *v2, BOOL rightshift,
	VALUE *vres));
extern void scalevalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *vres));
extern void powivalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *vres));
extern void powervalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *v3,
	VALUE *vres));
extern void divvalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *vres));
extern void quovalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *vres));
extern void modvalue MATH_PROTO((VALUE *v1, VALUE *v2, VALUE *vres));
extern BOOL testvalue MATH_PROTO((VALUE *vp));
extern BOOL comparevalue MATH_PROTO((VALUE *v1, VALUE *v2));
extern FLAG relvalue MATH_PROTO((VALUE *v1, VALUE *v2));
extern HASH hashvalue MATH_PROTO((VALUE *vp));
extern void printvalue MATH_PROTO((VALUE *vp, int flags));



/*
 * Structure of a matrix.
 */
struct matrix {
	long m_dim;		/* dimension of matrix */
	long m_size;		/* total number of elements */
	long m_min[MAXDIM];	/* minimum bound for indices */
	long m_max[MAXDIM];	/* maximum bound for indices */
	VALUE m_table[1];	/* actually varying length table */
};

#define matsize(n) (sizeof(MATRIX) - sizeof(VALUE) + ((n) * sizeof(VALUE)))


extern MATRIX *matadd MATH_PROTO((MATRIX *m1, MATRIX *m2));
extern MATRIX *matsub MATH_PROTO((MATRIX *m1, MATRIX *m2));
extern MATRIX *matmul MATH_PROTO((MATRIX *m1, MATRIX *m2));
extern MATRIX *matneg MATH_PROTO((MATRIX *m));
extern MATRIX *matalloc MATH_PROTO((long size));
extern MATRIX *matcopy MATH_PROTO((MATRIX *m));
extern MATRIX *matsquare MATH_PROTO((MATRIX *m));
extern MATRIX *matinv MATH_PROTO((MATRIX *m));
extern MATRIX *matscale MATH_PROTO((MATRIX *m, long n));
extern MATRIX *matshift MATH_PROTO((MATRIX *m, long n));
extern MATRIX *matmulval MATH_PROTO((MATRIX *m, VALUE *vp));
extern MATRIX *matpowi MATH_PROTO((MATRIX *m, NUMBER *q));
extern MATRIX *matconj MATH_PROTO((MATRIX *m));
extern MATRIX *matquoval MATH_PROTO((MATRIX *m, VALUE *vp));
extern MATRIX *matmodval MATH_PROTO((MATRIX *m, VALUE *vp));
extern MATRIX *matint MATH_PROTO((MATRIX *m));
extern MATRIX *matfrac MATH_PROTO((MATRIX *m));
extern MATRIX *matround MATH_PROTO((MATRIX *m, long places));
extern MATRIX *matbround MATH_PROTO((MATRIX *m, long places));
extern MATRIX *mattrans MATH_PROTO((MATRIX *m));
extern MATRIX *matcross MATH_PROTO((MATRIX *m1, MATRIX *m2));
extern BOOL mattest MATH_PROTO((MATRIX *m));
extern BOOL matcmp MATH_PROTO((MATRIX *m1, MATRIX *m2));
extern long matsearch MATH_PROTO((MATRIX *m, VALUE *vp, long index));
extern long matrsearch MATH_PROTO((MATRIX *m, VALUE *vp, long index));
extern HASH mathash MATH_PROTO((MATRIX *m));
extern VALUE matdet MATH_PROTO((MATRIX *m));
extern VALUE matdot MATH_PROTO((MATRIX *m1, MATRIX *m2));
extern void matfill MATH_PROTO((MATRIX *m, VALUE *v1, VALUE *v2));
extern void matfree MATH_PROTO((MATRIX *m));
extern void matprint MATH_PROTO((MATRIX *m, long max_print));
extern VALUE *matindex MATH_PROTO((MATRIX *mp, BOOL create, long dim,
	VALUE *indices));


#if 0
extern BOOL matisident MATH_PROTO((MATRIX *m));
#endif



/*
 * List definitions.
 * An individual list element.
 */
typedef struct listelem LISTELEM;
struct listelem {
	LISTELEM *e_next;	/* next element in list (or NULL) */
	LISTELEM *e_prev;	/* previous element in list (or NULL) */
	VALUE e_value;		/* value of this element */
};


/*
 * Structure for a list of elements.
 */
struct list {
	LISTELEM *l_first;	/* first list element (or NULL) */
	LISTELEM *l_last;	/* last list element (or NULL) */
	LISTELEM *l_cache;	/* cached list element (or NULL) */
	long l_cacheindex;	/* index of cached element (or undefined) */
	long l_count;		/* total number of elements in the list */
};


extern void insertlistfirst MATH_PROTO((LIST *lp, VALUE *vp));
extern void insertlistlast MATH_PROTO((LIST *lp, VALUE *vp));
extern void insertlistmiddle MATH_PROTO((LIST *lp, long index, VALUE *vp));
extern void removelistfirst MATH_PROTO((LIST *lp, VALUE *vp));
extern void removelistlast MATH_PROTO((LIST *lp, VALUE *vp));
extern void removelistmiddle MATH_PROTO((LIST *lp, long index, VALUE *vp));
extern void listfree MATH_PROTO((LIST *lp));
extern void listprint MATH_PROTO((LIST *lp, long max_print));
extern long listsearch MATH_PROTO((LIST *lp, VALUE *vp, long index));
extern long listrsearch MATH_PROTO((LIST *lp, VALUE *vp, long index));
extern HASH listhash MATH_PROTO((LIST *lp));
extern BOOL listcmp MATH_PROTO((LIST *lp1, LIST *lp2));
extern VALUE *listfindex MATH_PROTO((LIST *lp, long index));
extern LIST *listalloc MATH_PROTO((void));
extern LIST *listcopy MATH_PROTO((LIST *lp));



/*
 * Structures for associations.
 * Associations are "indexed" by one or more arbitrary values, and are
 * stored in a hash table with their hash values for quick indexing.
 */
typedef	struct assocelem ASSOCELEM;
struct assocelem {
	ASSOCELEM *e_next;	/* next element in list (or NULL) */
	long e_dim;		/* dimension of indexing for this element */
	HASH e_hash;		/* hash value for this element */
	VALUE e_value;		/* value of association */
	VALUE e_indices[1];	/* index values (variable length) */
};


struct assoc {
	long a_count;		/* number of elements in the association */
	long a_size;		/* current size of association hash table */
	ASSOCELEM **a_table;	/* current hash table for elements */
};


extern ASSOC *assocalloc MATH_PROTO((long initsize));
extern ASSOC *assoccopy MATH_PROTO((ASSOC *ap));
extern void assocfree MATH_PROTO((ASSOC *ap));
extern void assocprint MATH_PROTO((ASSOC *ap, long max_print));
extern long assocsearch MATH_PROTO((ASSOC *ap, VALUE *vp, long index));
extern long assocrsearch MATH_PROTO((ASSOC *ap, VALUE *vp, long index));
extern HASH assochash MATH_PROTO((ASSOC *ap));
extern BOOL assoccmp MATH_PROTO((ASSOC *ap1, ASSOC *ap2));
extern VALUE *assocfindex MATH_PROTO((ASSOC *ap, long index));
extern VALUE *associndex MATH_PROTO((ASSOC *ap, BOOL create, long dim,
	VALUE *indices));


/*
 * Object actions.
 */
#define OBJ_PRINT	0	/* print the value */
#define OBJ_ONE		1	/* create the multiplicative identity */
#define OBJ_TEST	2	/* test a value for "zero" */
#define OBJ_ADD		3	/* add two values */
#define OBJ_SUB		4	/* subtrace one value from another */
#define OBJ_NEG		5	/* negate a value */
#define OBJ_MUL		6	/* multiply two values */
#define OBJ_DIV		7	/* divide one value by another */
#define OBJ_INV		8	/* invert a value */
#define OBJ_ABS		9	/* take absolute value of value */
#define OBJ_NORM	10	/* take the norm of a value */
#define OBJ_CONJ	11	/* take the conjugate of a value */
#define OBJ_POW		12	/* take the power function */
#define OBJ_SGN		13	/* return the sign of a value */
#define OBJ_CMP		14	/* compare two values for equality */
#define OBJ_REL		15	/* compare two values for inequality */
#define OBJ_QUO		16	/* integer quotient of values */
#define OBJ_MOD		17	/* remainder of division of values */
#define OBJ_INT		18	/* integer part of */
#define OBJ_FRAC	19	/* fractional part of */
#define OBJ_INC		20	/* increment by one */
#define OBJ_DEC		21	/* decrement by one */
#define OBJ_SQUARE	22	/* square value */
#define OBJ_SCALE	23	/* scale by power of two */
#define OBJ_SHIFT	24	/* shift left (or right) by number of bits */
#define OBJ_ROUND	25	/* round to specified decimal places */
#define OBJ_BROUND	26	/* round to specified binary places */
#define OBJ_ROOT	27	/* take nth root of value */
#define OBJ_SQRT	28	/* take square root of value */
#define OBJ_MAXFUNC	28	/* highest function */


/*
 * Definition of an object type.
 * This is actually a varying sized structure.
 */
typedef struct {
	char *name;			/* name of object */
	int count;			/* number of elements defined */
	int actions[OBJ_MAXFUNC+1];	/* function indices for actions */
	int elements[1];		/* element indexes (MUST BE LAST) */
} OBJECTACTIONS;

#define objectactionsize(elements) \
	(sizeof(OBJECTACTIONS) + ((elements) - 1) * sizeof(int))


/*
 * Structure of an object.
 * This is actually a varying sized structure.
 * However, there are always at least USUAL_ELEMENTS values in the object.
 */
struct object {
	OBJECTACTIONS *o_actions;	/* action table for this object */
	VALUE o_table[USUAL_ELEMENTS];	/* object values (MUST BE LAST) */
};

#define objectsize(elements) \
	(sizeof(OBJECT) + ((elements) - USUAL_ELEMENTS) * sizeof(VALUE))


extern OBJECT *objcopy MATH_PROTO((OBJECT *op));
extern OBJECT *objalloc MATH_PROTO((long index));
extern VALUE objcall MATH_PROTO((int action, VALUE *v1, VALUE *v2, VALUE *v3));
extern void objfree MATH_PROTO((OBJECT *op));
extern void objuncache MATH_PROTO((void));
extern int addelement MATH_PROTO((char *name));
extern void defineobject MATH_PROTO((char *name, int indices[], int count));
extern int checkobject MATH_PROTO((char *name));
extern void showobjfuncs MATH_PROTO((void));
extern int findelement MATH_PROTO((char *name));
extern int objoffset MATH_PROTO((OBJECT *op, long index));
extern HASH objhash MATH_PROTO((OBJECT *op));

#endif

/* END CODE */
