/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1980, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pmerge.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>

#define PRGFILE 0
#define LABELFILE 1
#define CONSTFILE 2
#define TYPEFILE 3
#define VARFILE 4
#define RTNFILE 5
#define BODYFILE 6
#define NUMFILES 7

#define TRUE 1
#define FALSE 0
#define MAXINCL 9
#define MAXNAM 75
#define TMPNAME "/usr/tmp/MGXXXXXX"

FILE	*files[NUMFILES];
char	*names[NUMFILES];
FILE	*curfile;		/* current output file */
char	labelopen = FALSE, constopen = FALSE, typeopen = FALSE, varopen = FALSE;

/*
 * Remove temporary files if interrupted
 */
void
onintr(unused)
{
	int i;

	for (i = 0; i < NUMFILES; i++)
		if (files[i] != NULL)
			unlink(names[i]);
}

/*
 * Program to merge separately compiled pascal modules into a
 * single standard Pascal program.
 */
main(argc, argv)
	long argc;
	char **argv;
{
	FILE	*incl[MAXINCL];	/* include stack */
	long	inclcnt = 0;	/* incl index */
	char	*name[MAXNAM];	/* include names seen so far */
	long	namcnt = 0;	/* next name ptr slot available */
	char	*nambuf;	/* string table for names */
	char	line[BUFSIZ];	/* input line buffer */
	char	*next;		/* next name space available */
	FILE	*input = stdin;	/* current input file */
	long	ac = 0;		/* argv index */
	char	**cpp, *cp, *fp;/* char ptrs */
	char	quote;		/* include quote character */
	int	i;		/* index var */

	for (i = 0; i < MAXNAM ; i++)
		name[i] = 0;

	signal(SIGINT, onintr);

	curfile = files[PRGFILE] =
		fopen(names[PRGFILE] = mktemp(strdup(TMPNAME)), "w");
	files[LABELFILE] =
		fopen(names[LABELFILE] = mktemp(strdup(TMPNAME)), "w");
	files[CONSTFILE] =
		fopen(names[CONSTFILE] = mktemp(strdup(TMPNAME)), "w");
	files[TYPEFILE] = fopen(names[TYPEFILE] = mktemp(strdup(TMPNAME)), "w");
	files[VARFILE] = fopen(names[VARFILE] = mktemp(strdup(TMPNAME)), "w");
	files[RTNFILE] = fopen(names[RTNFILE] = mktemp(strdup(TMPNAME)), "w");
	files[BODYFILE] = fopen(names[BODYFILE] = mktemp(strdup(TMPNAME)), "w");

	for (i = 0; i < NUMFILES; i++)
		if (files[i] == NULL)
			quit(names[i]);
	if ((nambuf = malloc(BUFSIZ)) == NULL) {
		fputs("no space for string table\n", stderr);
		quit(NULL);
	}
	next = nambuf;
	name[namcnt] = next;
	for(;;) {
		if (inclcnt > 0) {
			inclcnt--;
			fclose(input);
			input = incl[inclcnt];
		} else if (++ac < argc) {
			input = freopen(argv[ac], "r", input);
			if (input == NULL)
				quit(argv[ac]);
		} else {
			printout();
			onintr(0);
			exit(0);
		}
		fgets(line, BUFSIZ, input);
		while (!feof(input)) {
			if (line[0] != '#') {
				split(line);
				fgets(line, BUFSIZ, input);
				continue;
			}
			for (cp = &line[1]; isspace(*cp); cp++)
				/* void */;
			if (strncmp("include", cp, 7))
				goto bad;
			for (cp += 7; isspace(*cp); cp++)
				/* void */;
			if (*cp != '\'' && *cp != '"')
				goto bad;
			if (&nambuf[BUFSIZ] < next + strlen(cp)) {
				if ((nambuf = malloc(BUFSIZ)) == NULL) {
					fputs("no space for string table\n",
						stderr);
					quit(NULL);
				}
				next = nambuf;
				name[namcnt] = next;
			}
			for (fp = next, quote = *cp++;
			     *cp != '\0' && *cp != quote; )
				*fp++ = *cp++;
			if (*cp != quote &&
			    (fp[-1] != 'i' || fp[-1] != 'h') &&
			    (fp[-2] != '.'))
				goto bad;
			*fp++ = '\0';
			for (cpp = name; *cpp < next && strcmp(*cpp, next); )
				cpp++;
			if (*cpp == next) {
				if (inclcnt == MAXINCL) {
					fputs("include table overflow\n",
						stderr);
					quit(NULL);
				}
				if (++namcnt == MAXNAM) {
					fputs("include name table overflow\n",
						stderr);
					quit(NULL);
				}
				incl[inclcnt] = input;
				inclcnt++;
				input = fopen(next, "r");
				if (input == NULL)
					quit(next);
				next = fp;
				name[namcnt] = next;
			}
			fgets(line, BUFSIZ, input);
		}
	}
bad:
	fputs("bad include format:", stderr);
	fputs(line, stderr);
	quit(NULL);
}

/*
 * Split up output into the approprite files
 */
char incom = FALSE;	/* TRUE => in comment */
char incur = FALSE;	/* TRUE => in (* *) style comment */
char inbrac = FALSE;	/* TRUE => in { } style comment */
char instr = FALSE;	/* TRUE => in quoted string */
char inprog = FALSE;	/* TRUE => program statement has been found */
int  beginnest = 0;	/* routine nesting level */
int  nest = 0;		/* begin block nesting level */
int  paren_level = 0;	/* nesting level of parentheses */

split(line)
	char *line;
{
	char ch1, *cp;		/* input window */
	char *word;		/* ptr to current word */
	int len;		/* length of current word */
	char prt = TRUE;	/* TRUE => print current word */

	ch1 = ' ';
	cp = line;
	while (*cp) {
		switch(*cp) {
		case '(':
			if (incom)
				break;
			if (*(cp+1) == '*') {
				fputc(*cp, curfile);
				cp++;
				incom = TRUE;
				incur = TRUE;
			} else {
				paren_level++;
			}
			break;
		case ')':
			if (incur && ch1 == '*') {
				incom = FALSE;
				incur = FALSE;
			} else if (!incom) {
				paren_level--;
			}
			break;
		case '{':
			if (!incom) {
				inbrac = TRUE;
				incom = TRUE;
			}
			break;
		case '}':
			if (inbrac) {
				inbrac = FALSE;
				incom = FALSE;
			}
			break;
		case '\'':
			if (!incom) {
				incom = TRUE;
				instr = TRUE;
			} else if (instr) {
				incom = FALSE;
				instr = FALSE;
			}
			break;
		}
		if (incom || !isalpha(*cp)) {
			fputc(*cp, curfile);
			ch1 = *cp++;
			continue;
		}
		word = cp;
		while (isalnum(*cp))
			cp++;
		len = cp - word;
		switch (*word) {
		case 'b':
			if (len == 5 && !strncmp(word, "begin", 5)) {
				if (nest == 0 && beginnest == 0) {
					if (inprog != 1) {
						fprintf(stderr,
						    "improper program body");
						quit(NULL);
					}
					curfile = files[BODYFILE];
				} else {
					beginnest++;
				}
			}
			break;
		case 'c':
			if (len == 4 && !strncmp(word, "case", 4)) {
				if (beginnest > 0) {
					beginnest++;
				}
				break;
			}
			if (len == 5 && !strncmp(word, "const", 5)) {
				if (nest == 0) {
					prt = FALSE;
					if (!constopen) {
						constopen = TRUE;
						prt = TRUE;
					}
					curfile = files[CONSTFILE];
				}
			}
			break;
		case 'e':
			if (len == 3 && !strncmp(word, "end", 3)) {
				if (beginnest == 1) {
					nest--;
				}
				if (beginnest > 0) {
					beginnest--;
				}
				if (nest < 0) {
					if (inprog == 1) {
						inprog = 0;
						nest = 0;
					} else {
						fprintf(stderr, "too many end statements");
						quit(NULL);
					}
				}
				break;
			}
			if (len == 8 && !strncmp(word, "external", 8)) {
				fputs("forward", curfile);
				prt = FALSE;
				if (paren_level == 0) {
					nest--;
				}
			}
			break;
		case 'f':
			if (len == 8 && !strncmp(word, "function", 8)) {
				if (nest == 0) {
					curfile = files[RTNFILE];
				}
				if (paren_level == 0) {
					nest++;
				}
				break;
			}
			if (len == 7 && !strncmp(word, "forward", 7)) {
				if (paren_level == 0) {
					nest--;
				}
			}
			break;
		case 'l':
			if (len == 5 && !strncmp(word, "label", 5)) {
				if (nest == 0) {
					prt = FALSE;
					if (!labelopen) {
						labelopen = TRUE;
						prt = TRUE;
					}
					curfile = files[LABELFILE];
				}
			}
			break;
		case 'p':
			if (len == 9 && !strncmp(word, "procedure", 9)) {
				if (nest == 0) {
					curfile = files[RTNFILE];
				}
				if (paren_level == 0) {
					nest++;
				}
				break;
			}
			if (len == 7 && !strncmp(word, "program", 7)) {
				if (nest != 0) {
					fprintf(stderr, "improper program nesting");
					quit(NULL);
				}
				inprog = 1;
				curfile = files[PRGFILE];
			}
			break;
		case 't':
			if (len == 4 && !strncmp(word, "type", 4)) {
				if (nest == 0) {
					prt = FALSE;
					if (!typeopen) {
						typeopen = TRUE;
						prt = TRUE;
					}
					curfile = files[TYPEFILE];
				}
			}
			break;
		case 'v':
			if (len == 3 && !strncmp(word, "var", 3)) {
				if (nest == 0) {
					prt = FALSE;
					if (!varopen) {
						varopen = TRUE;
						prt = TRUE;
					}
					curfile = files[VARFILE];
				}
			}
			break;
		}
		if (prt)
			fprintf(curfile, "%.*s", len, word);
		prt = TRUE;
		ch1 = ' ';
	}
}

/*
 * Print out the merged result
 */
printout()
{
	FILE *fp;
	int i;
	char ch;

	for(i = 0; i < NUMFILES; i++) {
		fp = freopen(names[i], "r", files[i]);
		if (fp == NULL)
			quit(names[i]);
		ch = getc(fp);
		while (!feof(fp)) {
			putc(ch,stdout);
			ch = getc(fp);
		}
	}
}

/*
 * Die gracefully
 */
quit(fp)
	char *fp;
{
	if (fp != NULL)
		perror(fp);
	onintr(0);
	exit(1);
}
