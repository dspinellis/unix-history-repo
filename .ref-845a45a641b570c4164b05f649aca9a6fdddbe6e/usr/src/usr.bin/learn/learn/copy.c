/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)copy.c	4.6 (Berkeley) %G%";
#endif /* not lint */

#include "stdio.h"
#include "signal.h"
#include "lrnref.h"

char togo[50];
char last[100];
char logf[100];
char subdir[100];
extern char *ctime();
extern int review;
int noclobber;

copy(prompt, fin)
int prompt;
FILE *fin;
{
	FILE *fout, *f;
	char s[100], t[100], s1[100], nm[30];
	char *r, *tod, c;
	int *p, tv[2];
	int *action();
	int nmatch = 0;
	long mark;
	extern void intrpt();
	extern char *wordb();

	if (subdir[0]==0)
		sprintf(subdir, "%s/%s", direct, sname);
	for (;;) {
		if (pgets(s, prompt, fin) == 0)
			if (fin == stdin) {
				fprintf(stderr, "Type \"bye\" if you want to leave learn.\n");
				fflush(stderr);
				clearerr(stdin);
				continue;
			} else
				break;
		trim(s);		/* trim newline */
		/* change the sequence %s to lesson directory */
		/* if needed */
		for (r = s; *r; r++)
			if (*r == '%') {
				sprintf(s1, s, subdir, subdir, subdir);
				strcpy(s, s1);
				break;
			}
		r = wordb(s, t);	/* t = first token, r = rest */
		p = action(t);		/* p = token class */
		if (p && *p == ONCE) {	/* some actions done only once per script */
			if (wrong && !review) {	/* we are on 2nd time */
				scopy(fin, NULL);
				continue;
			}
			strcpy(s, r);
			r = wordb(s, t);
			p = action(t);
		}
		if (p == 0) {
			if (comfile >= 0) {	/* if #pipe in effect ... */
				write(comfile, s, strlen(s));
				write(comfile, "\n", 1);
			}
			else {		/* else must be UNIX command ... */
				signal(SIGINT, SIG_IGN);
				status = mysys(s);
				signal(SIGINT, intrpt);
			}
			if (incopy) {
				fprintf(incopy, "%s\n", s);
				strcpy(last, s);
			}
			continue;
		}
		switch (*p) {
		case READY:
			if (incopy && r) {
				fprintf(incopy, "%s\n", r);
				strcpy(last, r);
			}
			return;
		case PRINT:
			if (wrong)
				scopy(fin, NULL);	/* don't repeat message */
			else if (r)
				list(r);
			else
				scopy(fin, stdout);
			break;
		case HINT:
			mark = ftell(scrin);
			if (r)
				rewind(scrin);
			while ((int)(c=fgetc(scrin)) != EOF)
				putchar(c);
			fflush(stdout);
			fseek(scrin, mark, 0);
			break;
		case NOP:
			break;
		case MATCH:
			if (nmatch > 0)	/* we have already passed */
				scopy(fin, NULL);
			else if ((status = strcmp(r, last)) == 0) {	/* did we pass this time? */
				nmatch++;
				scopy(fin, stdout);
			} else
				scopy(fin, NULL);
			break;
		case BAD:
			if (strcmp(r, last) == 0) {
				scopy(fin, stdout);
			} else
				scopy(fin, NULL);
			break;
		case SUCCEED:
			scopy(fin, (status == 0) ? stdout : NULL);
			break;
		case FAIL:
			scopy(fin, (status != 0) ? stdout : NULL);
			break;
		case CREATE:
			if (noclobber)
				fout = NULL;
			else
				fout = fopen(r, "w");
			scopy(fin, fout);
			if (!noclobber)
				fclose(fout);
			break;
		case CMP:
			status = cmp(r);	/* contains two file names */
			break;
		case MV:
			sprintf(nm, "%s/L%s.%s", subdir, todo, r);
			fcopy(r, nm);
			break;
		case USER:
		case NEXT:
			if (noclobber)
				noclobber = 0;
			more = 1;
			return;
		/* "again previous_lesson" has a hard-to-reproduce bug */
		case AGAIN:
			review = 0;
			if (!r) {
				r = todo;
				noclobber = 1;
				review = 1;
			}
			again = 1;
			strcpy(togo, r);
			unhook();
			return;
		case SKIP:
			skip = 1;
			unhook();
			return;
		case COPYIN:
			incopy = fopen(".copy", "w");
			break;
		case UNCOPIN:
			fclose(incopy);
			incopy = NULL;
			break;
		case COPYOUT:
			teed = maktee();
			break;
		case UNCOPOUT:
			untee();
			teed = 0;
			break;
		case PIPE:
			comfile = makpipe();
			break;
		case UNPIPE:
			close(comfile);
			wait(0);
			comfile = -1;
			break;
		case YES:
		case NO:
			if (incopy) {
				fprintf(incopy, "%s\n", s);
				strcpy(last, s);
			}
			return;
		case WHERE:
			printf("You are in lesson %s of \"%s\" with a speed rating of %d.\n", todo, sname, speed);
			printf("You have completed %d out of a possible %d lessons.\n", sequence-1, total);
			if (r)
				tellwhich();
			fflush(stdout);
			break;
		case BYE:
			more=0;
			return;
		case CHDIR:
			printf("cd not allowed\n");
			fflush(stdout);
			break;
		case LEARN:
			printf("You are already in learn.\n");
			fflush(stdout);
			break;
		case LOG:	/* logfiles should be created mode 666 */
			if (!logging)
				break;
			if (logf[0] == 0)
				sprintf(logf, "%s/log/%s", direct, sname);
			f = fopen((r ? r : logf), "a");
			if (f == NULL)
				break;
			time(tv);
			tod = ctime(tv);
			tod[24] = 0;
			fprintf(f, "%s L%-6s %s %2d %s\n", tod,
				todo, status? "fail" : "pass", speed, pwline);
			fclose(f);
			break;
		}
	}
	return;
}

pgets(s, prompt, f)
char *s;
int prompt;
FILE *f;
{
	if (prompt) {
		if (comfile < 0)
			fputs("% ", stdout);
		fflush(stdout);
	}
	if (fgets(s, 100,f))
		return(1);
	else
		return(0);
}

trim(s)
char *s;
{
	while (*s)
		s++;
	if (*--s == '\n')
		*s=0;
}

scopy(fi, fo)	/* copy fi to fo until a line with #
		 * sequence "#\n" means a line not ending with \n
		 * control-M's are filtered out */
FILE *fi, *fo;
{
	int c;

	while ((c = getc(fi)) != '#' && c != EOF) {
		do {
			if (c == '#')   {
				c = getc(fi);
				if (c == '\n')
					break;
				if (c == EOF)   {
					if (fo != NULL)
						fflush(fo);
					return;
				}
				if (fo != NULL)
					putc('#', fo);
			}
			if (c == '\r')
				break;
			if (fo != NULL)
				putc(c, fo);
			if (c == '\n')
				break;
		} while ((c = getc(fi)) != EOF);
	}
	if (c == '#')
		ungetc(c, fi);
	if (fo != NULL)
		fflush(fo);
}

cmp(r)	/* compare two files for status; #cmp f1 f2 [ firstnlinesonly ] */
char *r;
{
	char *s, *h;
	FILE *f1, *f2;
	int c1, c2, stat, n;

	for (s = r; *s != ' ' && *s != '\0'; s++)
		;
	*s++ = 0;	/* r contains file 1 */
	while (*s == ' ')
		s++;
	for (h = s; *h != ' ' && *h != '\0'; h++)
		;
	if (*h) {
		*h++ = 0;
		while (*h == ' ')
			h++;
		n = atoi(h);
	}
	else
		n = 077777;
	f1 = fopen(r, "r");
	f2 = fopen(s, "r");
	if (f1 == NULL || f2 == NULL)
		return(1);	/* failure */
	stat = 0;
	for (;;) {
		c1 = getc(f1);
		c2 = getc(f2);
		if (c1 != c2) {
			stat = 1;
			break;
		}
		if (*h && c1 == '\n')
			if (--n)
				break;
		if (c1 == EOF || c2 == EOF)
			break;
	}
	fclose(f1);
	fclose(f2);
	return(stat);
}

char *
wordb(s, t)	/* in s, t is prefix; return tail */
char *s, *t;
{
	int c;

	while (c = *s++) {
		if (c == ' ' || c == '\t')
			break;
		*t++ = c;
	}
	*t = 0;
	while (*s == ' ' || *s == '\t')
		s++;
	return(c ? s : NULL);
}

unhook()
{
	if (incopy) {
		fclose(incopy);
		incopy = NULL;
	}
	if (comfile >= 0) {
		close(comfile);
		wait(0);
		comfile = -1;
	}
	if (teed) {
		teed = 0;
		untee();
	}
	fclose(scrin);
	scrin = NULL;
}
