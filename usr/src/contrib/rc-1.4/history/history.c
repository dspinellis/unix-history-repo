/*
	history.c -- primitive history mechanism

	Paul Haahr & Byron Rakitzis, July 1991.

	This program mimics the att v8 = and == history programs.
	The edit() algorithm was adapted from a similar program
	that Boyd Roberts wrote, but otherwise all the code has
	been written from scratch.

	edit() was subsequently redone by Hugh Redelmeier in order
	to correctly deal with tab characters in the source line.

	BUGS:
	There is an implicit assumption that commands are no
	more than 1k characters long. 
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *id = "@(#) history.c  8/91";

#undef FALSE
#undef TRUE
typedef enum { FALSE, TRUE } bool;

#define CHUNKSIZE 65536

static struct {
	char *old, *new;
} *replace;

static char **search, *progname, *history;
static char me;	/* typically ':' or '-' */
static bool editit = FALSE, printit = FALSE;
static int nreplace = 0, nsearch = 0;
static FILE *fp;

static void *ealloc(size_t n) {
	void *p = (void *) malloc(n);
	if (p == NULL) {
		perror("malloc");
		exit(1);
	}
	return p;
}

static void *erealloc(void *p, size_t n) {
	p = (void *) realloc(p, n);
	if (p == NULL) {
		perror("realloc");
		exit(1);
	}
	return p;
}

static char *newstr() {
	return ealloc((size_t)1024);
}

static char *basename(char *s) {
	char *t = strrchr(s, '/');
	return (t == NULL) ? s : t + 1;
}

/* stupid O(n^2) substring matching routine */

static char *isin(char *target, char *pattern) {
	size_t plen = strlen(pattern);
	size_t tlen = strlen(target);
	for (; tlen >= plen; target++, --tlen)
		if (strncmp(target, pattern, plen) == 0)
			return target;
	return NULL;
}

/* replace the first match in the string with "new" */
static char *sub(char *s, char *old, char *new) {
	char *t, *u;

	t = isin(s, old);
	u = newstr();

	*t = '\0';
	while (*old != '\0')
		old++, t++;
	strcpy(u, s);
	strcat(u, new);
	strcat(u, t);
	return u;
}

static char *edit(char *s) {
	char *final, *f, *end;
	int col;
	bool ins;
	
start:
	fprintf(stderr, "%s\n", s);	
	f = final = newstr();
	end = s + strlen(s);
	col = 0;
	ins = FALSE;
	
	for (;; col++) {
		int	c = getchar();

		if (c == me && col == 0) {
			int	peekc = getchar();
			if (peekc == '\n')
				return NULL;
			ungetc(peekc, stdin);
		}
		if (c == '\n') {
			if (col == 0)
				return s;
			
			while (s < end) /* copy remainder of string */
				*f++ = *s++;
			*f = '\0';
			s = final;
			goto start;
		} else if (ins || s>=end) {
			/* col need not be accurate -- tabs need not be interpreted */
			*f++ = c;
		} else {
			switch (c) {
			case '+':
				while (s < end)
					*f++ = *s++;
				*f = '\0';
				continue;
			case '%':
				c = ' ';
				/* FALLTHROUGH */
			default:
				*f++ = c;
				break;	
			case EOF:
				exit(1);
				/* NOTREACHED */
			case ' ':
				if (*s == '\t') {
					int	oldcol = col;

					for (;; col++) {
						int	peekc;

						if ((col&07) == 07) {
							*f++ = '\t';	/* we spaced past a tab */
							break;
						}
						peekc = getchar();
						if (peekc != ' ') {
							ungetc(peekc, stdin);
							if (peekc != '\n') {
								/* we spaced partially into a tab */
								do {
									*f++ = ' ';
									oldcol++;
								} while (oldcol <= col);
							}
							break;
						}
					}
				} else {
					*f++ = *s;
				}
				break;
			case '#':
				break;
			case '$':
				end = s;	/* truncate s */
				continue;	/* skip incrementing s */
			case '^':
				ins = TRUE;
				continue;	/* skip incrementing s */
			case '\t':
				for (;; col++) {
					if ((*f++ = s<end? *s++ : '\t') == '\t') {
						col = col | 07;	/* advance to before next tabstop */
					}
					if ((col&07) == 07)	/* stop before tabstop */
						break;
				}
				continue;	/* skip incrementing s */
			}
			if (s<end && (*s!='\t' || (col&07)==07))
				s++;
		}
	}
}

static char *readhistoryfile(char **last) {
	char *buf;
	size_t count, size;
	long nread;

	if ((history = getenv("history")) == NULL) {
		fprintf(stderr, "$history not set\n");
		exit(1);
	}
	fp = fopen(history, "r+");
	if (fp == NULL) {
		perror(history);
		exit(1);
	}

	size = 0;
	count = 0;
	buf = ealloc(size = CHUNKSIZE);
	while ((nread = fread(buf + count, sizeof (char), size - count, fp)) > 0) {
		count += nread;
		if (size - count == 0)
			buf = erealloc(buf, size *= 4);
	}
	if (nread == -1) {
		perror(history);
		exit(1);
	}
	*last = buf + count;
	return buf;
}

static char *getcommand() {
	char *s, *t;
	static char *hist = NULL, *last;

	if (hist == NULL) {
		hist = readhistoryfile(&last);
		*--last = '\0';		/* trim final newline */
	}

again:	s = last;
	if (s < hist)
		return NULL;
	while (*--s != '\n')
		if (s <= hist) {
			last = hist - 1;
			return hist;
		}
	*s = '\0';
	last = s++;

	/*
	 * if the command contains the "me" character at the start of the line
	 * or after any of [`{|()] then try again
	 */

	for (t = s; *t != '\0'; t++)
		if (*t == me) {
			char *u = t - 1;
			while (u >= s && (*u == ' ' || *u == '\t'))
				--u;
			if (u < s || *u == '`' || *u == '{' || *u == '|' || *u == '(' || *u == ')')
				goto again;
		}
	return s;
}

int main(int argc, char **argv) {
	int i;
	char *s;

	s = progname = basename(argv[0]);
	me = *s++;
	if (*s == me) {
		s++;
		editit = TRUE;
	}
	if (*s == 'p') {
		s++;
		printit = TRUE;
	}
/* Nahh...
	if (*s != '\0') {
		fprintf(stderr, "\"%s\": bad name for history program\n", progname);
		exit(1);
	}
*/

	if (argc > 1) {
		replace = ealloc((argc - 1) * sizeof *replace);
		search = ealloc((argc - 1) * sizeof *search);
	}
	for (i = 1; i < argc; i++)
		if ((s = strchr(argv[i], ':')) == NULL)
			search[nsearch++] = argv[i];
		else {
			*(char *)s = '\0';	/* do we confuse ps too much? */
			replace[nreplace].old = argv[i];
			replace[nreplace].new = s + 1;
			nreplace++;
		}

next:	s = getcommand();
	if (s == NULL) {
		fprintf(stderr, "command not matched\n");
		return 1;
	}
	for (i = 0; i < nsearch; i++)
		if (!isin(s, search[i]))
			goto next;
	for (i = 0; i < nreplace; i++)
		if (!isin(s, replace[i].old))
			goto next;
		else
			s = sub(s, replace[i].old, replace[i].new);
	if (editit) {
		s = edit(s);
		if (s == NULL)
			goto next;
	}
	fseek(fp, 0, 2); /* 2 == end of file. i.e., append command to $history */
	fprintf(fp, "%s\n", s);
	fclose(fp);
	if (printit)
		printf("%s\n", s);
	else {
		char *shell = getenv("SHELL");

		if (!editit)
			fprintf(stderr, "%s\n", s);
		if (shell == NULL)
			shell = "/bin/sh";
		execl(shell, basename(shell), "-c", s, NULL);
		perror(shell);
		exit(1);
	}
	return 0;
}
