#include "awk.def"
#include "awk.h"
#include "stdio.h"
node *ALLOC(n)
{	node *x;
	x = (node *)malloc(sizeof(node)+n*sizeof(node *));
	if (x == NULL)
		error(FATAL, "out of space in ALLOC");
	return(x);
}
node *exptostat(a) node *a;
{
	a->ntype = NSTAT;
	return(a);
}
node	*nullstat;
node *node0(a)
{	node *x;
	x=ALLOC(0);
	x->nnext = NULL;
	x->nobj=a;
	return(x);
}
node *node1(a,b) node *b;
{	node *x;
	x=ALLOC(1);
	x->nnext = NULL;
	x->nobj=a;
	x->narg[0]=b;
	return(x);
}
node *node2(a,b,c) node *b, *c;
{	node *x;
	x = ALLOC(2);
	x->nnext = NULL;
	x->nobj = a;
	x->narg[0] = b;
	x->narg[1] = c;
	return(x);
}
node *node3(a,b,c,d) node *b, *c, *d;
{	node *x;
	x = ALLOC(3);
	x->nnext = NULL;
	x->nobj = a;
	x->narg[0] = b;
	x->narg[1] = c;
	x->narg[2] = d;
	return(x);
}
node *node4(a,b,c,d,e) node *b, *c, *d, *e;
{	node *x;
	x = ALLOC(4);
	x->nnext = NULL;
	x->nobj = a;
	x->narg[0] = b;
	x->narg[1] = c;
	x->narg[2] = d;
	x->narg[3] = e;
	return(x);
}
node *stat3(a,b,c,d) node *b, *c, *d;
{	node *x;
	x = node3(a,b,c,d);
	x->ntype = NSTAT;
	return(x);
}
node *op2(a,b,c) node *b, *c;
{	node *x;
	x = node2(a,b,c);
	x->ntype = NEXPR;
	return(x);
}
node *op1(a,b) node *b;
{	node *x;
	x = node1(a,b);
	x->ntype = NEXPR;
	return(x);
}
node *stat1(a,b) node *b;
{	node *x;
	x = node1(a,b);
	x->ntype = NSTAT;
	return(x);
}
node *op3(a,b,c,d) node *b, *c, *d;
{	node *x;
	x = node3(a,b,c,d);
	x->ntype = NEXPR;
	return(x);
}
node *stat2(a,b,c) node *b, *c;
{	node *x;
	x = node2(a,b,c);
	x->ntype = NSTAT;
	return(x);
}
node *stat4(a,b,c,d,e) node *b, *c, *d, *e;
{	node *x;
	x = node4(a,b,c,d,e);
	x->ntype = NSTAT;
	return(x);
}
node *valtonode(a, b) cell *a;
{	node *x;
	x = node0(a);
	x->ntype = NVALUE;
	x->subtype = b;
	return(x);
}
node *genjump(a)
{	node *x;
	x = node0(a);
	x->ntype = NSTAT;
	return(x);
}
node *pa2stat(a,b,c) node *a, *b, *c;
{	node *x;
	x = node3(paircnt++, a, b, c);
	x->ntype = NPA2;
	return(x);
}
node *linkum(a,b) node *a, *b;
{	node *c;
	if(a == NULL) return(b);
	else if(b == NULL) return(a);
	for(c=a; c->nnext != NULL; c=c->nnext);
	c->nnext = b;
	return(a);
}
node *genprint()
{	node *x;
	x = stat2(PRINT,valtonode(lookup("$record", symtab), CFLD), nullstat);
	return(x);
}
