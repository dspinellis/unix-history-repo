/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)tree.c 1.3 %G%";

/*
 * This module contains the interface between the SYM routines and
 * the parse tree routines.  It would be nice if such a crude
 * interface were not necessary, but some parts of tree building are
 * language and hence SYM-representation dependent.  It's probably
 * better to have tree-representation dependent code here than vice versa.
 */

#include "defs.h"
#include "tree.h"
#include "sym.h"
#include "btypes.h"
#include "classes.h"
#include "sym.rep"
#include "tree/tree.rep"

typedef char *ARGLIST;

#define nextarg(arglist, type)  ((type *) (arglist += sizeof(type)))[-1]

LOCAL SYM *mkstring();
LOCAL SYM *namenode();

/*
 * Determine the type of a parse tree.  While we're at, check
 * the parse tree out.
 */

SYM *treetype(p, ap)
register NODE *p;
register ARGLIST ap;
{
    switch(p->op) {
	case O_NAME: {
	    SYM *s;

	    s = nextarg(ap, SYM *);
	    s = which(s);
	    return namenode(p, s);
	    /* NOTREACHED */
	}

	case O_WHICH:
	    p->nameval = nextarg(ap, SYM *);
	    p->nameval = which(p->nameval);
	    return NIL;

	case O_LCON:
	    return t_int;

	case O_FCON:
	    return t_real;

	case O_SCON: {
	    char *cpy;
	    SYM *s;

	    cpy = strdup(p->sconval);
	    p->sconval = cpy;
	    s = mkstring(p->sconval);
	    if (s == t_char) {
		p->op = O_LCON;
		p->lconval = p->sconval[0];
	    }
	    return s;
	}

	case O_INDIR:
	    p->left = nextarg(ap, NODE *);
	    chkclass(p->left, PTR);
	    return rtype(p->left->nodetype)->type;

	case O_RVAL: {
	    NODE *p1, *q;

	    p1 = p->left;
	    p->nodetype = p1->nodetype;
	    if (p1->op == O_NAME) {
		if (p1->nodetype->class == FUNC) {
		    p->op = O_CALL;
		    p->right = NIL;
		} else if (p1->nameval->class == CONST) {
		    if (p1->nameval->type == t_real->type) {
			p->op = O_FCON;
			p->fconval = p1->nameval->symvalue.fconval;
			p->nodetype = t_real;
			dispose(p1);
		    } else {
			p->op = O_LCON;
			p->lconval = p1->nameval->symvalue.iconval;
			p->nodetype = p1->nameval->type;
			dispose(p1);
		    }
		}
	    }
	    return p->nodetype;
	    /* NOTREACHED */
	}

	case O_CALL: {
	    SYM *s;

	    p->left = nextarg(ap, NODE *);
	    p->right = nextarg(ap, NODE *);
	    s = p->left->nodetype;
	    if (isblock(s) && isbuiltin(s)) {
		p->op = (OP) s->symvalue.token.tokval;
		tfree(p->left);
		p->left = p->right;
		p->right = NIL;
	    }
	    return s->type;
	}

	case O_ITOF:
	    return t_real;

	case O_NEG: {
	    SYM *s;

	    p->left = nextarg(ap, NODE *);
	    s = p->left->nodetype;
	    if (!compatible(s, t_int)) {
		if (!compatible(s, t_real)) {
		    trerror("%t is improper type", p->left);
		} else {
		    p->op = O_NEGF;
		}
	    }
	    return s;
	}

	case O_ADD:
	case O_SUB:
	case O_MUL:
	case O_LT:
	case O_LE:
	case O_GT:
	case O_GE:
	case O_EQ:
	case O_NE:
	{
	    BOOLEAN t1real, t2real;
	    SYM *t1, *t2;

	    p->left = nextarg(ap, NODE *);
	    p->right = nextarg(ap, NODE *);
	    t1 = rtype(p->left->nodetype);
	    t2 = rtype(p->right->nodetype);
	    t1real = (t1 == t_real);
	    t2real = (t2 == t_real);
	    if (t1real || t2real) {
		p->op++;
		if (!t1real) {
		    p->left = build(O_ITOF, p->left);
		} else if (!t2real) {
		    p->right = build(O_ITOF, p->right);
		}
	    } else {
		if (t1real) {
		    convert(&p->left, t_int, O_NOP);
		}
		if (t2real) {
		    convert(&p->right, t_int, O_NOP);
		}
	    }
	    if (p->op >= O_LT) {
		return t_boolean;
	    } else {
		if (t1real || t2real) {
		    return t_real;
		} else {
		    return t_int;
		}
	    }
	    /* NOTREACHED */
	}

	case O_DIVF:
	    p->left = nextarg(ap, NODE *);
	    p->right = nextarg(ap, NODE *);
	    convert(&p->left, t_real, O_ITOF);
	    convert(&p->right, t_real, O_ITOF);
	    return t_real;

	case O_DIV:
	case O_MOD:
	    p->left = nextarg(ap, NODE *);
	    p->right = nextarg(ap, NODE *);
	    convert(&p->left, t_int, O_NOP);
	    convert(&p->right, t_int, O_NOP);
	    return t_int;

	case O_AND:
	case O_OR:
	    p->left = nextarg(ap, NODE *);
	    p->right = nextarg(ap, NODE *);
	    chkboolean(p->left);
	    chkboolean(p->right);
	    return t_boolean;

	default:
	    return NIL;
    }
}

/*
 * Create a node for a name.  The symbol for the name has already
 * been chosen, either implicitly with "which" or explicitly from
 * the dot routine.
 */

LOCAL SYM *namenode(p, s)
NODE *p;
SYM *s;
{
    NODE *np;

    p->nameval = s;
    if (s->class == REF) {
	np = alloc(1, NODE);
	*np = *p;
	p->op = O_INDIR;
	p->left = np;
	np->nodetype = s;
    }
    if (s->class == CONST || s->class == VAR || s->class == FVAR) {
	return s->type;
    } else {
	return s;
    }
}

/*
 * Convert a tree to a type via a conversion operator;
 * if this isn't possible generate an error.
 *
 * Note the tree is call by address, hence the #define below.
 */

LOCAL convert(tp, typeto, op)
NODE **tp;
SYM *typeto;
OP op;
{
#define tree    (*tp)

    SYM *s;

    s = rtype(tree->nodetype);
    typeto = rtype(typeto);
    if (typeto == t_real && compatible(s, t_int)) {
	tree = build(op, tree);
    } else if (!compatible(s, typeto)) {
	trerror("%t is improper type");
    } else if (op != O_NOP && s != typeto) {
	tree = build(op, tree);
    }

#undef tree
}

/*
 * Construct a node for the Pascal dot operator.
 *
 * If the left operand is not a record, but rather a procedure
 * or function, then we interpret the "." as referencing an
 * "invisible" variable; i.e. a variable within a dynamically
 * active block but not within the static scope of the current procedure.
 */

NODE *dot(record, field)
NODE *record;
SYM *field;
{
    register NODE *p;
    register SYM *s;

    if (isblock(record->nodetype)) {
	s = findsym(field, record->nodetype);
	if (s == NIL) {
	    error("\"%s\" is not defined in \"%s\"",
		field->symbol, record->nodetype->symbol);
	}
	p = alloc(1, NODE);
	p->op = O_NAME;
	p->nodetype = namenode(p, s);
    } else {
	s = findclass(field, FIELD);
	if (s == NIL) {
	    error("\"%s\" is not a field", field->symbol);
	}
	field = s;
	chkfield(record, field);
	p = alloc(1, NODE);
	p->op = O_ADD;
	p->nodetype = field->type;
	p->left = record;
	p->left->nodetype = t_int;
	p->right = build(O_LCON, (long) field->symvalue.offset);
    }
    return p;
}

/*
 * Return a tree corresponding to an array reference and do the
 * error checking.
 */

NODE *subscript(a, slist)
NODE *a, *slist;
{
    register SYM *t;
    register NODE *p;
    SYM *etype, *atype, *eltype;
    NODE *esub, *olda;

    olda = a;
    t = rtype(a->nodetype);
    if (t->class != ARRAY) {
	trerror("%t is not an array");
    }
    eltype = t->type;
    p = slist;
    t = t->chain;
    for (; p != NIL && t != NIL; p = p->right, t = t->chain) {
	esub = p->left;
	etype = rtype(esub->nodetype);
	atype = rtype(t);
	if (!compatible(atype, etype)) {
	    trerror("subscript %t is the wrong type", esub);
	}
	a = build(O_INDEX, a, esub);
	a->nodetype = eltype;
    }
    if (p != NIL) {
	trerror("too many subscripts for %t", olda);
    } else if (t != NIL) {
	trerror("not enough subscripts for %t", olda);
    }
    return(a);
}

/*
 * Evaluate a subscript index.
 */

evalindex(arraytype, index)
SYM *arraytype;
long index;
{
    long lb, ub;
    SYM *indextype;

    indextype = arraytype->chain;
    lb = indextype->symvalue.rangev.lower;
    ub = indextype->symvalue.rangev.upper;
    if (index < lb || index > ub) {
	error("subscript out of range");
    }
    return(index - lb);
}

/*
 * Check that a record.field usage is proper.
 */

LOCAL chkfield(r, f)
NODE *r;
SYM *f;
{
    register SYM *s;

    chkclass(r, RECORD);

    /*
     * Don't do this for compiled code.
     */
#   if (!isvax)
	for (s = r->nodetype->chain; s != NIL; s = s->chain) {
	    if (s == f) {
		break;
	    }
	}
	if (s == NIL) {
	    error("\"%s\" is not a field in specified record", f->symbol);
	}
#   endif
}

/*
 * Check to see if a tree is boolean-valued, if not it's an error.
 */

chkboolean(p)
register NODE *p;
{
    if (p->nodetype != t_boolean) {
	trerror("found %t, expected boolean expression");
    }
}

/*
 * Check to make sure the given tree has a type of the given class.
 */

LOCAL chkclass(p, class)
NODE *p;
int class;
{
    SYM tmpsym;

    tmpsym.class = class;
    if (p->nodetype->class != class) {
	trerror("%t is not a %s", p, classname(&tmpsym));
    }
}

/*
 * Construct a node for the type of a string.  While we're at it,
 * scan the string for '' that collapse to ', and chop off the ends.
 */

LOCAL SYM *mkstring(str)
char *str;
{
    register char *p, *q;
    SYM *s, *t;
    static SYM zerosym;

    p = str;
    q = str + 1;
    while (*q != '\0') {
	if (q[0] != '\'' || q[1] != '\'') {
	    *p = *q;
	    p++;
	}
	q++;
    }
    *--p = '\0';
    if (p == str + 1) {
	return t_char;
    }
    s = alloc(1, SYM);
    *s = zerosym;
    s->class = ARRAY;
    s->type = t_char;
    s->chain = alloc(1, SYM);
    t = s->chain;
    *t = zerosym;
    t->class = RANGE;
    t->type = t_int;
    t->symvalue.rangev.lower = 1;
    t->symvalue.rangev.upper = p - str + 1;
    return s;
}

/*
 * Free up the space allocated for a string type.
 */

unmkstring(s)
SYM *s;
{
    dispose(s->chain);
}
