/*
   footobar.c: a collection of functions to convert internal representations of
   variables and functions to external representations, and vice versa
*/

#include "rc.h"

#define FSCHAR '\1'
#define FSSTRING "\1"

static char *getenvw(char *, bool);

static bool Fconv(Format *f, int ignore) {	/* protect an exported name from brain-dead shells */
	int c;
	unsigned const char *s = va_arg(f->args, unsigned const char *);

	while ((c = *s++) != '\0')
		if (dnw[c] || c == '*' || (c == '_' && *s == '_'))
			fmtprint(f, "__%02x", c);
		else
			fmtputc(f, c);
	return FALSE;
}

/* used to turn a function in Node * form into something we can export to the environment */

extern char *fun2str(char *name, Node *n) {
	return mprint("fn_%F={%T}", name, n);
}

/* convert a redirection to a printable form */

static bool Dconv(Format *f, int ignore) {
	const char *name = "?";
	int n = va_arg(f->args, int);
	switch (n) {
	case rCreate:		name = ">";	break;
	case rAppend:		name = ">>";	break;
	case rFrom:		name = "<";	break;
	case rHeredoc:		name = "<<";	break;
	case rHerestring:	name = "<<<";	break;
	}
	fmtcat(f, name);
	return FALSE;
}

/* defaultfd -- return the default fd for a given redirection operation */

extern int defaultfd(int op) {
	return (op == rCreate || op == rAppend) ? 1 : 0;
}

/* convert a function in Node * form into something rc can parse (and humans can read?) */

static bool Tconv(Format *f, int ignore) {
	Node *n = va_arg(f->args, Node *);
	if (n == NULL) {
		fmtprint(f, "()");
		return FALSE;
	}
	switch (n->type) {
	case nWord:	fmtprint(f, "%S", n->u[0].s);				break;
	case nQword:	fmtprint(f, "%#S", n->u[0].s);				break;
	case nBang:	fmtprint(f, "! %T", n->u[0].p);				break;
	case nCase:	fmtprint(f, "case %T", n->u[0].p);			break;
	case nNowait:	fmtprint(f, "%T&", n->u[0].p);				break;
	case nCount:	fmtprint(f, "$#%T", n->u[0].p);				break;
	case nFlat:	fmtprint(f, "$^%T", n->u[0].p);				break;
	case nRmfn:	fmtprint(f, "fn %T", n->u[0].p);			break;
	case nSubshell:	fmtprint(f, "@ %T", n->u[0].p);				break;
	case nVar:	fmtprint(f, "$%T", n->u[0].p);				break;
	case nAndalso:	fmtprint(f, "%T&&%T", n->u[0].p, n->u[1].p);		break;
	case nAssign:	fmtprint(f, "%T=%T", n->u[0].p, n->u[1].p);		break;
	case nConcat:	fmtprint(f, "%T^%T", n->u[0].p, n->u[1].p);		break;
	case nElse:	fmtprint(f, "{%T}else %T", n->u[0].p, n->u[1].p);	break;
	case nNewfn:	fmtprint(f, "fn %T {%T}", n->u[0].p, n->u[1].p);	break;
	case nIf:	fmtprint(f, "if(%T)%T", n->u[0].p, n->u[1].p);		break;
	case nOrelse:	fmtprint(f, "%T||%T", n->u[0].p, n->u[1].p);		break;
	case nArgs:	fmtprint(f, "%T %T", n->u[0].p, n->u[1].p);		break;
	case nSwitch:	fmtprint(f, "switch(%T){%T}", n->u[0].p, n->u[1].p);	break;
	case nMatch:	fmtprint(f, "~ %T %T", n->u[0].p, n->u[1].p);		break;
	case nVarsub:	fmtprint(f, "$%T(%T)", n->u[0].p, n->u[1].p);		break;
	case nWhile:	fmtprint(f, "while(%T)%T", n->u[0].p, n->u[1].p);	break;
	case nLappend:	fmtprint(f, "(%T %T)", n->u[0].p, n->u[1].p);		break;
	case nForin:	fmtprint(f, "for(%T in %T)%T", n->u[0].p, n->u[1].p, n->u[2].p); break;
	case nDup:
		if (n->u[2].i != -1)
			fmtprint(f, "%D[%d=%d]", n->u[0].i, n->u[1].i, n->u[2].i);
		else
			fmtprint(f, "%D[%d=]", n->u[0].i, n->u[1].i);
		break;
	case nBackq: {
		Node *n0 = n->u[0].p, *n00;
		if (n0 != NULL && n0->type == nVar
		    && (n00 = n0->u[0].p) != NULL && n00->type == nWord && streq(n00->u[0].s, "ifs"))
			fmtprint(f, "`");
		else
			fmtprint(f, "``%T", n0);
		fmtprint(f, "{%T}", n->u[1].p);
		break;
	}
	case nCbody:
	case nBody: {
		Node *n0 = n->u[0].p;
		if (n0 != NULL)
			fmtprint(f, "%T", n->u[0].p);
		if (n->u[1].p != NULL) {
			if (n0 != NULL && n0->type != nNowait)
				fmtprint(f, ";");
			fmtprint(f, "%T", n->u[1].p);
		}
		break;
	}
	case nBrace:
		fmtprint(f, "{%T}", n->u[0].p);
		if (n->u[1].p != NULL)
			fmtprint(f, "%T", n->u[1].p);
		break;
	case nEpilog:
	case nPre:
		fmtprint(f, "%T", n->u[0].p);
		if (n->u[1].p != NULL)
			fmtprint(f, " %T", n->u[1].p);
		break;
	case nPipe: {
		int ofd = n->u[0].i, ifd = n->u[1].i;
		fmtprint(f, "%T|", n->u[2].p);
		if (ifd != 0)
			fmtprint(f, "[%d=%d]", ofd, ifd);
		else if (ofd != 1)
			fmtprint(f, "[%d]", ofd);
		fmtprint(f, "%T", n->u[3].p);
		break;
	}
	case nRedir: {
		int op = n->u[0].i;
		fmtprint(f, "%D", op);
		if (n->u[1].i != defaultfd(op))
			fmtprint(f, "[%d]", n->u[1].i);
		fmtprint(f, "%T", n->u[2].p);
		break;
	}
	case nNmpipe: {
		int op = n->u[0].i;
		fmtprint(f, "%D", op);
		if (n->u[1].i != defaultfd(op))
			fmtprint(f, "[%d]", n->u[1].i);
		fmtprint(f, "{%T}", n->u[2].p);
		break;
	}
 	}
	return FALSE;
}

/* convert a List to a string, separating it with ^A characters. Used for exporting variables to the environment */

extern char *list2str(char *name, List *s) {
	size_t size, step;
	List *t;
	char *w, *x;
	name = nprint("%F", name);
	size = strlen(name) + listlen(s);
	w = ealloc(size + 2);
	t = s;
	x = w;
	strcpy(x, name);
	strcpy(x += strlen(name), "=");
	strcpy(x += conststrlen("="), t->w);
	for (x += strlen(t->w), s = s->n; s != NULL; s = s->n) {
		memcpy(x, FSSTRING, step = conststrlen(FSSTRING));
		x += step;
		memcpy(x, s->w, step = strlen(s->w));
		x += step;
	}
	*x = '\0';
	return w;
}

/* convert a List to an array, for execve() */

extern char **list2array(List *s, bool print) {
	char **av;
	int i;

	/* 4 == 1 for the null terminator + 2 for the fake execve() + 1 for defaulting to sh */
	av = nalloc((listnel(s) + 4) * sizeof (char *));
	av += 3; /* hide the two free spots from rc (two for #! emulation, one for defaulting to sh) */
	if (print)
		fprint(2, "%L\n", s, " ");
	for (i = 0; s != NULL; i++) {
		av[i] = s->w;
		s = s->n;
	}
	av[i] = NULL;
	return av;
}

/* figure out the name of a variable given an environment string. copy this into malloc space */

extern char *get_name(char *s) {
	int c;
	size_t i;
	char *r, *namebuf;
	for (i = 0; s[i] != '\0' && s[i] != '='; i++)
		;
	if (s[i] == '\0')
		return NULL;
	r = namebuf = ealloc(i + 1);
	while (1)
		switch (c = *s++) {
		case '=':
			*r++ = '\0';
			return namebuf;
		case '_':
			if (*s == '_') {
				static const char hexchar[] = "0123456789abcdef";
				char *h1 = strchr(hexchar, s[1]);
				char *h2 = strchr(hexchar, s[2]);
				if (h1 != NULL && h2 != NULL) {
					*r++ = ((h1 - hexchar) << 4) | (h2 - hexchar);
					s += 3;
					break;
				}
			}
			/* FALLTHROUGH */
		default:
			*r++ = c;
			break;
		}
}

/* get the next word from a variable's value as represented in the environment. */

static char *getenvw(char *s, bool saw_alpha) {
	size_t i;
	char *r;
	for (i = 0; s[i] != '\0' && s[i] != FSCHAR; i++)
		;
	if (i == 0) {
		if (s[i] == '\0' && !saw_alpha)
			return NULL;
		else
			return clear(enew(char), (size_t) 1);
	}
	r = strncpy(ealloc(i + 1), s, i);
	r[i] = '\0';
	return r;
}

/* take an environment entry for a variable (elements ^A separated) and turn it into a List */

extern List *parse_var(char *name, char *extdef) {
	List *r, *top;
	char *f;
	bool saw_alpha;
	top = r = enew(List);
	extdef = strchr(extdef, '=') + 1;
	if ((f = getenvw(extdef, FALSE)) == NULL) {
		r->w = "";
		r->m = NULL;
		r->n = NULL;
	} else {
		while (1) {
			r->w = f;
			r->m = NULL;
			extdef += strlen(f);
			if (*extdef == FSCHAR) {
				extdef++;
				saw_alpha = TRUE;
			} else {
				saw_alpha = FALSE;
			}
			if ((f = getenvw(extdef, saw_alpha)) == NULL) {
				r->n = NULL;
				break;
			}
			r = r->n = enew(List);
		}
	}
	return top;
}

/* get an environment entry for a function and have rc parse it. */

#define PREFIX "fn x"
#define PRELEN conststrlen(PREFIX)
extern Node *parse_fn(char *name, char *extdef) {
	Node *def;
	char *s, old[PRELEN];
	if ((s = strchr(extdef, '=')) == NULL)
		return NULL;
	memcpy(old, s -= (PRELEN-1), PRELEN);
	memcpy(s, PREFIX, PRELEN);
	def = parseline(s);
	memcpy(s, old, PRELEN);
	return (def == NULL || def->type != nNewfn) ? NULL : def->u[1].p;
}

static bool Aconv(Format *f, int c) {
	char **a = va_arg(f->args, char **);
	if (*a != NULL) {
		fmtcat(f, *a);
		while (*++a != NULL)
			fmtprint(f, " %s", *a);
	}
	return FALSE;
}

static bool Lconv(Format *f, int c) {
	List *l = va_arg(f->args, List *);
	char *sep = va_arg(f->args, char *);
	char *fmt = (f->flags & FMT_leftside) ? "%s%s" : "%-S%s";
	if (l == NULL && (f->flags & FMT_leftside) == 0)
		fmtprint(f, "()");
	else {
		List *s;
		for (s = l; s != NULL; s = s->n)
			fmtprint(f, fmt, s->w, s->n == NULL ? "" : sep);
	}
	return FALSE;
}

#define	ISMETA(c)	(c == '*' || c == '?' || c == '[')

static bool Sconv(Format *f, int ignore) {
	int c;
	unsigned char *s = va_arg(f->args, unsigned char *), *t = s;
	bool quoted    = (f->flags & FMT_altform)  != 0;	/* '#' */
	bool metaquote = (f->flags & FMT_leftside) != 0;	/* '-' */
	if (*s == '\0') {
		fmtprint(f, "''");
		return FALSE;
	}
	if (!quoted) {
		while ((c = *t++) != '\0')
			if (nw[c] == 1 || (metaquote && ISMETA(c)))
				goto quoteit;
		fmtprint(f, "%s", s);
		return FALSE;
	}
quoteit:
	fmtputc(f, '\'');
	while ((c = *s++) != '\0') {
		fmtputc(f, c);
		if (c == '\'')
			fmtputc(f, '\'');

	}
	fmtputc(f, '\'');
	return FALSE;
}

void initprint(void) {
	fmtinstall('A', Aconv);
	fmtinstall('L', Lconv);
	fmtinstall('S', Sconv);
	fmtinstall('T', Tconv);
	fmtinstall('D', Dconv);
	fmtinstall('F', Fconv);
}
