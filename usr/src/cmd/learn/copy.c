#include "stdio.h"
#include "signal.h"
#include "lrnref"

char last[100];
char logf[100];
char subdir[100];
extern char * ctime();

copy(prompt, fin)
FILE *fin;
{
	FILE *fout, *f;
	char s[100], t[100], s1[100], *r, *tod;
	char nm[30];
	int *p, tv[2];
	extern int intrpt(), *action();
	extern char *wordb();
	int nmatch = 0;

	if (subdir[0]==0)
		sprintf(subdir, "../../%s", sname);
	for (;;) {
		if (pgets(s, prompt, fin) == 0)
			if (fin == stdin) {
				/* fprintf(stderr, "Don't type control-D\n"); */
				/* this didn't work out very well */
				continue;
			} else
				break;
		trim(s);
		/* change the sequence %s to lesson directory */
		/* if needed */
		for (r = s; *r; r++)
			if (*r == '%') {
				sprintf(s1, s, subdir, subdir, subdir);
				strcpy(s, s1);
				break;
			}
		r = wordb(s, t);
		p = action(t);
		if (*p == ONCE) {	/* some actions done only once per script */
			if (wrong) {	/* we are on 2nd time */
				scopy(fin, NULL);
				continue;
			}
			strcpy(s, r);
			r = wordb(s, t);
			p = action(t);
		}
		if (p == 0) {
			if (comfile >= 0) {
				write(comfile, s, strlen(s));
				write(comfile, "\n", 1);
			}
			else {
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
			fout = fopen(r, "w");
			scopy(fin, fout);
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
			more = 1;
			return;
		case COPYIN:
			incopy = fopen(".copy", "w");
			break;
		case UNCOPIN:
			fclose(incopy);
			incopy = NULL;
			break;
		case COPYOUT:
			maktee();
			break;
		case UNCOPOUT:
			untee();
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
			printf("You are in lesson %s\n", todo);
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
		case LOG:
			if (!logging)
				break;
			if (logf[0] == 0)
				sprintf(logf, "%s/log/%s", direct, sname);
			f = fopen( (r? r : logf), "a");
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
FILE *f;
{
	if (prompt) {
		if (comfile < 0)
			printf("$ ");
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

scopy(fi, fo)	/* copy fi to fo until a line with # */
FILE *fi, *fo;
{
	int c;

	while ((c = getc(fi)) != '#' && c != EOF) {
		do {
			if (fo != NULL)
				putc(c, fo);
			if (c == '\n')
				break;
		} while ((c = getc(fi)) != EOF);
	}
	if (c == '#')
		ungetc(c, fi);
	fflush(fo);
}

cmp(r)	/* compare two files for status */
char *r;
{
	char *s;
	FILE *f1, *f2;
	int c1, c2, stat;

	for (s = r; *s != ' ' && *s != '\0'; s++)
		;
	*s++ = 0;	/* r contains file 1 */
	while (*s == ' ')
		s++;
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
