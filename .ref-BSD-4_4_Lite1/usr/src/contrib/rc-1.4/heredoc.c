/* heredoc.c: heredoc slurping is done here */

#include "rc.h"

struct Hq {
	Node *doc;
	char *name;
	Hq *n;
	bool quoted;
} *hq;

static bool dead = FALSE;

/*
 * read in a heredocument. A clever trick: skip over any partially matched end-of-file
 * marker storing only the number of characters matched. If the whole marker is matched,
 * return from readheredoc(). If only part of the marker is matched, copy that part into
 * the heredocument.
 *
 * BUG: if the eof string contains a newline, the state can get confused, and the
 * heredoc may continue past where it should.  on the other hand, /bin/sh seems to
 * never get out of its readheredoc() when the heredoc string contains a newline
 */

static char *readheredoc(char *eof) {
	int c;
	char *t, *buf, *bufend;
	unsigned char *s;
	size_t bufsize;
	t = buf = nalloc(bufsize = 512);
	bufend = &buf[bufsize];
	dead = FALSE;
#define	RESIZE(extra) { \
		char *nbuf; \
		bufsize = bufsize * 2 + extra; \
		nbuf = nalloc(bufsize); \
		memcpy(nbuf, buf, (size_t) (t - buf)); \
		t = nbuf + (t - buf); \
		buf = nbuf; \
		bufend = &buf[bufsize]; \
	}
	for (;;) {
		print_prompt2();
		for (s = (unsigned char *) eof; (c = gchar()) == *s; s++)
			;
		if (*s == '\0' && (c == '\n' || c == EOF)) {
			*t++ = '\0';
			return buf;
		}
		if (s != (unsigned char *) eof) {
			size_t len = s - (unsigned char *) eof;
			if (t + len >= bufend)
				RESIZE(len);
			memcpy(t, eof, len);
			t += len;
		}
		for (;; c = gchar()) {
			if (c == EOF) {
				yyerror("heredoc incomplete");
				dead = TRUE;
				return NULL;
			}
			if (t + 1 >= bufend)
				RESIZE(0);
			*t++ = c;
			if (c == '\n')
				break;
		}
	}
}

/* parseheredoc -- turn a heredoc with variable references into a node chain */

static Node *parseheredoc(char *s) {
	int c = *s;
	Node *result = NULL;
	while (TRUE) {
		Node *node;
		switch (c) {
		default: {
			char *begin = s;
			while ((c = *s++) != '\0' && c != '$')
				;
			*--s = '\0';
			node = mk(nQword, begin, NULL);
			break;
		}
		case '$': {
			char *begin = ++s, *var;
			c = *s++;
			if (c == '$') {
				node = mk(nQword, "$", NULL);
				c = *s;
			} else {
				size_t len = 0;
				do
					len++;
				while (!dnw[c = *(unsigned char *) s++]);
				if (c == '^')
					c = *s;
				else
					s--;
				var = nalloc(len + 1);
				var[len] = '\0';
				memcpy(var, begin, len);
				node = mk(nFlat, mk(nWord, var, NULL));
			}
			break;
		}
		case '\0':
			return result;
		}
		result = (result == NULL) ? node : mk(nConcat, result, node);
	}
}

/* read in heredocs when yyparse hits a newline. called from yyparse */

extern int heredoc(int end) {
	Hq *here;
	if ((here = hq) != NULL) {
		hq = NULL;
		if (end) {
			yyerror("heredoc incomplete");
			return FALSE;
		}
		do {
			Node *n = here->doc;
			char *s = readheredoc(here->name);
			if (dead)
				return FALSE;
			n->u[2].p = here->quoted ? mk(nQword, s, NULL) : parseheredoc(s);
			n->u[0].i = rHerestring;
		} while ((here = here->n) != NULL);
	}
	return TRUE;
}

/* queue pending heredocs into a queue. called from yyparse */

extern int qdoc(Node *name, Node *n) {
	Hq *new, **prev;
	if (name->type != nWord && name->type != nQword) {
		yyerror("eof-marker not a single literal word");
		flushu();
		return FALSE;
	}
	for (prev = &hq; (new = *prev) != NULL; prev = &new->n)
		;
	*prev = new = nnew(Hq);
	new->name = name->u[0].s;
	new->quoted = (name->type == nQword);
	new->doc = n;
	new->n = NULL;
	return TRUE;
}
