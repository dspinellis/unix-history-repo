/* glom.c: builds an argument list out of words, variables, etc. */

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include "rc.h"

static List *backq(Node *, Node *);
static List *bqinput(List *, int);
static List *count(List *);
static List *mkcmdarg(Node *);

Rq *redirq = NULL;

extern List *word(char *w, char *m) {
	List *s = NULL;
	if (w != NULL) {
		s = nnew(List);
		s->w = w;
		s->m = m;
		s->n = NULL;
	}
	return s;
}

/*
   Append list s2 to list s1 by copying s1 and making the new copy
   point at s2.
*/

extern List *append(List *s1, List *s2) {
	List *r, *top;
	if (s1 == NULL)
		return s2;
	if (s2 == NULL)
		return s1;
	for (r = top = nnew(List); 1; r = r->n = nnew(List)) {
		r->w = s1->w;
		r->m = s1->m;
		if ((s1 = s1->n) == NULL)
			break;
	}
	r->n = s2;
	return top;
}

extern List *concat(List *s1, List *s2) {
	int n1, n2;
	List *r, *top;
	if (s1 == NULL)
		return s2;
	if (s2 == NULL)
		return s1;
	if ((n1 = listnel(s1)) != (n2 = listnel(s2)) && n1 != 1 && n2 != 1)
		rc_error("bad concatenation");
	for (r = top = nnew(List); 1; r = r->n = nnew(List)) {
		size_t x = strlen(s1->w);
		size_t y = strlen(s2->w);
		size_t z = x + y + 1;
		r->w = nalloc(z);
		strcpy(r->w, s1->w);
		strcat(r->w, s2->w);
		if (s1->m == NULL && s2->m == NULL) {
			r->m = NULL;
		} else {
			r->m = nalloc(z);
			if (s1->m == NULL)
				clear(r->m, x);
			else
				memcpy(r->m, s1->m, x);
			if (s2->m == NULL)
				clear(&r->m[x], y);
			else
				memcpy(&r->m[x], s2->m, y);
			r->m[z] = 0;
		}
		if (n1 > 1)
			s1 = s1->n;
		if (n2 > 1)
			s2 = s2->n;
		if (s1 == NULL || s2 == NULL || (n1 == 1 && n2 == 1))
			break;
	}
	r->n = NULL;
	return top;
}

extern List *varsub(List *var, List *subs) {
	List *r, *top;
	int n = listnel(var);
	for (top = r = NULL; subs != NULL; subs = subs->n) {
		int i = a2u(subs->w);
		if (i < 1)
			rc_error("bad subscript");
		if (i <= n) {
			List *sub = var;
			while (--i)
				sub = sub->n; /* loop until sub == var(i) */
			if (top == NULL)
				top = r = nnew(List);
			else
				r = r->n = nnew(List);
			r->w = sub->w;
			r->m = sub->m;
		}
	}
	if (top != NULL)
		r->n = NULL;
	return top;
}

extern List *flatten(List *s) {
	List *r;
	size_t step;
	char *f;
	if (s == NULL || s->n == NULL)
		return s;
	r = nnew(List);
	f = r->w = nalloc(listlen(s) + 1);
	r->m = NULL; /* flattened lists come from variables, so no meta */
	r->n = NULL;
	strcpy(f, s->w);
	f += strlen(s->w);
	do {
		*f++ = ' ';
		s = s->n;
		step = strlen(s->w);
		memcpy(f, s->w, step);
		f += step;
	} while (s->n != NULL);
	*f = '\0';
	return r;
}

static List *count(List *l) {
	List *s = nnew(List);
	s->w = nprint("%d", listnel(l));
	s->n = NULL;
	s->m = NULL;
	return s;
}

extern void assign(List *s1, List *s2, bool stack) {
	List *val = s2;
	if (s1 == NULL)
		rc_error("null variable name");
	if (s1->n != NULL)
		rc_error("multi-word variable name");
	if (*s1->w == '\0')
		rc_error("zero-length variable name");
	if (a2u(s1->w) != -1)
		rc_error("numeric variable name");
	if (strchr(s1->w, '=') != NULL)
		rc_error("'=' in variable name");
	if (*s1->w == '*' && s1->w[1] == '\0')
		val = append(varlookup("0"), s2); /* preserve $0 when * is assigned explicitly */
	if (s2 != NULL || stack) {
		if (dashex)
			prettyprint_var(2, s1->w, val);
		varassign(s1->w, val, stack);
		alias(s1->w, varlookup(s1->w), stack);
	} else {
		if (dashex)
			prettyprint_var(2, s1->w, NULL);
		varrm(s1->w, stack);
	}
}

/*
   The following two functions are by the courtesy of Paul Haahr,
   who could not stand the incompetence of my own backquote implementation.
*/

#define BUFSIZE	((size_t) 1000)

static List *bqinput(List *ifs, int fd) {
	char *end, *bufend, *s;
	List *r, *top, *prev;
	size_t remain, bufsize;
	char isifs[256];
	int n, state; /* a simple FSA is used to read in data */

	clear(isifs, sizeof isifs);
	for (isifs['\0'] = TRUE; ifs != NULL; ifs = ifs->n)
		for (s = ifs->w; *s != '\0'; s++)
			isifs[*(unsigned char *)s] = TRUE;
	remain = bufsize = BUFSIZE;
	top = r = nnew(List);
	r->w = end = nalloc(bufsize + 1);
	r->m = NULL;
	state = 0;
	prev = NULL;

	while (1) {
		if (remain == 0) { /* is the string bigger than the buffer? */
			size_t m = end - r->w;
			char *buf;
			while (bufsize < m + BUFSIZE)
				bufsize *= 2;
			buf = nalloc(bufsize + 1);
			memcpy(buf, r->w, m);
			r->w = buf;
			end = &buf[m];
			remain = bufsize - m;
		}
		if ((n = rc_read(fd, end, remain)) <= 0) {
			if (n == 0)
	/* break */		break;
			uerror("backquote read");
			rc_error(NULL);
		}
		remain -= n;
		for (bufend = &end[n]; end < bufend; end++)
			if (state == 0) {
				if (!isifs[*(unsigned char *)end]) {
					state = 1;
					r->w = end;
				}
			} else {
				if (isifs[*(unsigned char *)end]) {
					state = 0;
					*end = '\0';
					prev = r;
					r = r->n = nnew(List);
					r->w = end+1;
					r->m = NULL;
				}
			}
	}
	if (state == 1) { /* terminate last string */
		*end = '\0';
		r->n = NULL;
	} else {
		if (prev == NULL) /* no input at all? */
			top = NULL;
		else
			prev->n = NULL; /* else terminate list */
	}
	return top;
}

static List *backq(Node *ifs, Node *n) {
	int p[2], pid, sp;
	List *bq;
	if (n == NULL)
		return NULL;
	if (pipe(p) < 0) {
		uerror("pipe");
		rc_error(NULL);
	}
	if ((pid = rc_fork()) == 0) {
		setsigdefaults(FALSE);
		mvfd(p[1], 1);
		close(p[0]);
		redirq = NULL;
		walk(n, FALSE);
		exit(getstatus());
	}
	close(p[1]);
	bq = bqinput(glom(ifs), p[0]);
	close(p[0]);
	rc_wait4(pid, &sp, TRUE);
	statprint(-1, sp);
	SIGCHK;
	return bq;
}

extern void qredir(Node *n) {
	Rq *next;
	if (redirq == NULL) {
		next = redirq = nnew(Rq);
	} else {
		for (next = redirq; next->n != NULL; next = next->n)
			;
		next->n = nnew(Rq);
		next = next->n;
	}
	next->r = n;
	next->n = NULL;
}

static List *mkcmdarg(Node *n) {
	char *name;
	List *ret = nnew(List);
	Estack *e = nnew(Estack);
	Edata efd;
	int p[2];
	if (pipe(p) < 0) {
		uerror("pipe");
		return NULL;
	}
	if (rc_fork() == 0) {
		setsigdefaults(FALSE);
		if (mvfd(p[n->u[0].i == rFrom], n->u[0].i == rFrom) < 0) /* stupid hack */
			exit(1);
		close(p[n->u[0].i != rFrom]);
		redirq = NULL;
		walk(n->u[2].p, FALSE);
		exit(getstatus());
	}
	name = nprint("/dev/fd/%d", p[n->u[0].i != rFrom]);
	efd.fd = p[n->u[0].i != rFrom];
	except(eFd, efd, e);
	close(p[n->u[0].i == rFrom]);
	ret->w = name;
	ret->m = NULL;
	ret->n = NULL;
	return ret;
}

extern List *glom(Node *n) {
	List *v, *head, *tail;
	Node *words;
	if (n == NULL)
		return NULL;
	switch (n->type) {
	case nArgs:
	case nLappend:
		words = n->u[0].p;
		tail = NULL;
		while (words != NULL && (words->type == nArgs || words->type == nLappend)) {
			if (words->u[1].p != NULL && words->u[1].p->type != nWord && words->u[1].p->type != nQword)
				break;
			head = glom(words->u[1].p);
			if (head != NULL) {
				head->n = tail;
				tail = head;
			}
			words = words->u[0].p;
		}
		v = append(glom(words), tail); /* force left to right evaluation */
		return append(v, glom(n->u[1].p));
	case nBackq:
		return backq(n->u[0].p, n->u[1].p);
	case nConcat:
		head = glom(n->u[0].p); /* force left-to-right evaluation */
		return concat(head, glom(n->u[1].p));
	case nDup:
	case nRedir:
		qredir(n);
		return NULL;
	case nWord:
	case nQword:
		return word(n->u[0].s, n->u[1].s);
	case nNmpipe:
		return mkcmdarg(n);
	default:
		/*
		   The next four operations depend on the left-child of glom
		   to be a variable name. Therefore the variable is looked up
		   here.
		*/
		if ((v = glom(n->u[0].p)) == NULL)
			rc_error("null variable name");
		if (v->n != NULL)
			rc_error("multi-word variable name");
		if (*v->w == '\0')
			rc_error("zero-length variable name");
		v = (*v->w == '*' && v->w[1] == '\0') ? varlookup(v->w)->n : varlookup(v->w);
		switch (n->type) {
		default:
			panic("unexpected node in glom");
			exit(1);
			/* NOTREACHED */
		case nCount:
			return count(v);
		case nFlat:
			return flatten(v);
		case nVar:
			return v;
		case nVarsub:
			return varsub(v, glom(n->u[1].p));
		}
	}
}
