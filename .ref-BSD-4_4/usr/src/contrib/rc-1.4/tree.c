/* tree.c: functions for manipulating parse-trees. (create, copy, delete) */

#include "rc.h"

/* make a new node, pass it back to yyparse. Used to generate the parsetree. */

extern Node *mk(int /*nodetype*/ t,...) {
	va_list ap;
	Node *n;
	va_start(ap, t);
	switch (t) {
	default:
		panic("unexpected node in mk");
		/* NOTREACHED */
	case nDup:
		n = nalloc(offsetof(Node, u[3]));
		n->u[0].i = va_arg(ap, int);
		n->u[1].i = va_arg(ap, int);
		n->u[2].i = va_arg(ap, int);
		break;
	case nWord: case nQword:
		n = nalloc(offsetof(Node, u[2]));
		n->u[0].s = va_arg(ap, char *);
		n->u[1].s = va_arg(ap, char *);
		break;
	case nBang: case nNowait:
	case nCount: case nFlat: case nRmfn: case nSubshell:
	case nVar: case nCase:
		n = nalloc(offsetof(Node, u[1]));
		n->u[0].p = va_arg(ap, Node *);
		break;
	case nAndalso: case nAssign: case nBackq: case nBody: case nBrace: case nConcat:
	case nElse: case nEpilog: case nIf: case nNewfn: case nCbody:
	case nOrelse: case nPre: case nArgs: case nSwitch:
	case nMatch: case nVarsub: case nWhile: case nLappend:
		n = nalloc(offsetof(Node, u[2]));
		n->u[0].p = va_arg(ap, Node *);
		n->u[1].p = va_arg(ap, Node *);
		break;
	case nForin:
		n = nalloc(offsetof(Node, u[3]));
		n->u[0].p = va_arg(ap, Node *);
		n->u[1].p = va_arg(ap, Node *);
		n->u[2].p = va_arg(ap, Node *);
		break;
	case nPipe:
		n = nalloc(offsetof(Node, u[4]));
		n->u[0].i = va_arg(ap, int);
		n->u[1].i = va_arg(ap, int);
		n->u[2].p = va_arg(ap, Node *);
		n->u[3].p = va_arg(ap, Node *);
		break;
	case nRedir:
	case nNmpipe:
		n = nalloc(offsetof(Node, u[3]));
		n->u[0].i = va_arg(ap, int);
		n->u[1].i = va_arg(ap, int);
		n->u[2].p = va_arg(ap, Node *);
		break;
 	}
	n->type = t;
	va_end(ap);
	return n;
}

/* copy a tree to malloc space. Used when storing the definition of a function */

extern Node *treecpy(Node *s, void *(*alloc)(size_t)) {
	Node *n;
	if (s == NULL)
		return NULL;
	switch (s->type) {
	default:
		panic("unexpected node in treecpy");
		/* NOTREACHED */
	case nDup:
		n = (*alloc)(offsetof(Node, u[3]));
		n->u[0].i = s->u[0].i;
		n->u[1].i = s->u[1].i;
		n->u[2].i = s->u[2].i;
		break;
	case nWord: case nQword:
		n = (*alloc)(offsetof(Node, u[2]));
		n->u[0].s = strcpy((char *) (*alloc)(strlen(s->u[0].s) + 1), s->u[0].s);
		if (s->u[1].s != NULL) {
			size_t i = strlen(s->u[0].s);
			n->u[1].s = (*alloc)(i);
			memcpy(n->u[1].s, s->u[1].s, i);
		} else
			n->u[1].s = NULL;
		break;
	case nBang: case nNowait: case nCase:
	case nCount: case nFlat: case nRmfn: case nSubshell: case nVar:
		n = (*alloc)(offsetof(Node, u[1]));
		n->u[0].p = treecpy(s->u[0].p, alloc);
		break;
	case nAndalso: case nAssign: case nBackq: case nBody: case nBrace: case nConcat:
	case nElse: case nEpilog: case nIf: case nNewfn: case nCbody:
	case nOrelse: case nPre: case nArgs: case nSwitch:
	case nMatch: case nVarsub: case nWhile: case nLappend:
		n = (*alloc)(offsetof(Node, u[2]));
		n->u[0].p = treecpy(s->u[0].p, alloc);
		n->u[1].p = treecpy(s->u[1].p, alloc);
		break;
	case nForin:
		n = (*alloc)(offsetof(Node, u[3]));
		n->u[0].p = treecpy(s->u[0].p, alloc);
		n->u[1].p = treecpy(s->u[1].p, alloc);
		n->u[2].p = treecpy(s->u[2].p, alloc);
		break;
	case nPipe:
		n = (*alloc)(offsetof(Node, u[4]));
		n->u[0].i = s->u[0].i;
		n->u[1].i = s->u[1].i;
		n->u[2].p = treecpy(s->u[2].p, alloc);
		n->u[3].p = treecpy(s->u[3].p, alloc);
		break;
	case nRedir:
	case nNmpipe:
		n = (*alloc)(offsetof(Node, u[3]));
		n->u[0].i = s->u[0].i;
		n->u[1].i = s->u[1].i;
		n->u[2].p = treecpy(s->u[2].p, alloc);
		break;
	}
	n->type = s->type;
	return n;
}

/* free a function definition that is no longer needed */

extern void treefree(Node *s) {
	if (s == NULL)
		return;
	switch (s->type) {
	default:
		panic("unexpected node in treefree");
		/* NOTREACHED */
	case nDup:
		break;
	case nWord: case nQword:
		efree(s->u[0].s);
		efree(s->u[1].s);
		break;
	case nBang: case nNowait:
	case nCount: case nFlat: case nRmfn:
	case nSubshell: case nVar: case nCase:
		treefree(s->u[0].p);
		break;
	case nAndalso: case nAssign: case nBackq: case nBody: case nBrace: case nConcat:
	case nElse: case nEpilog: case nIf: case nNewfn:
	case nOrelse: case nPre: case nArgs: case nCbody:
	case nSwitch: case nMatch:  case nVarsub: case nWhile:
	case nLappend:
		treefree(s->u[1].p);
		treefree(s->u[0].p);
		break;
	case nForin:
		treefree(s->u[2].p);
		treefree(s->u[1].p);
		treefree(s->u[0].p);
		break;
	case nPipe:
		treefree(s->u[2].p);
		treefree(s->u[3].p);
		break;
	case nRedir:
	case nNmpipe:
		treefree(s->u[2].p);
	}
	efree(s);
}
