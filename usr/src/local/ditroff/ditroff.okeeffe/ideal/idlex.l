/*	idlex.l	(CWI)	1.1	85/03/01	*/
%{
#include "ideal.h"
#include "y.tab.h"
extern int lineno, yylval;
extern boolean flyback;

#define	BUFLEN	100

struct filenode {
	struct filenode *next;
	FILE *file;
	int lineno;
	char *name;
	char buf[BUFLEN];
	int head;
};

struct filenode *filestack = NULL;

FILE *curfile;
#undef	input
#undef	unput

boolean file_start = TRUE;
boolean just_popped = FALSE;

void filepush ();
void filepop ();
char input ();
void unput ();
%}
%Start	PROGRAM COMMENT OUTSIDE LIBRARY INCLUDE
%%
%{
	int numitems, i;
	char item[5][20];
	char cmd[20];
	static int ISnest, bracenest, commnest, omode;
	if (file_start) {
		BEGIN OUTSIDE;
		ISnest = 0;
	};
	file_start = FALSE;
%}
<OUTSIDE>^\.IS.*\n	{
			BEGIN PROGRAM;
			if (!ISnest)
				printf ("%s", yytext);
			ISnest ++;
			bracenest = commnest = 0;
			}
<OUTSIDE>[ \t]		{
			if (just_popped) {
				if (omode == LIBFIL)
					BEGIN LIBRARY;
				else if (omode == CHATTY)
					BEGIN INCLUDE;
				else impossible ("idlex");
				just_popped = FALSE;
			} else
				printf ("%s", yytext);
			}
<OUTSIDE>.		{
			printf ("%s", yytext);
			}
<OUTSIDE>\n		{
			if (just_popped) {
				BEGIN PROGRAM;
				just_popped = FALSE;
			} else {
				printf ("\n");
			}
			}
<PROGRAM>box		return(yylval = BOX);
<PROGRAM>var		return(yylval = VAR);
<PROGRAM>bdlist		return(yylval = BDLIST);
<PROGRAM>boundary	return(yylval = BDLIST);
<PROGRAM>put		return(yylval = PUT);
<PROGRAM>conn		return(yylval = CONN);
<PROGRAM>to		return(yylval = TO);
<PROGRAM>using		return(yylval = USING);
<PROGRAM>construct	return(yylval = CONSTRUCT);
<PROGRAM>draw		return(yylval = DRAW);
<PROGRAM>opaque		return(yylval = OPAQUE);
<PROGRAM>left		return(yylval = LEFT);
<PROGRAM>text		return(yylval = CENTER);
<PROGRAM>right		return(yylval = RIGHT);
<PROGRAM>spline		return(yylval = SPLINE);
<PROGRAM>at		return(yylval = AT);
<PROGRAM>interior	return(yylval = INTERIOR);
<PROGRAM>exterior	return(yylval = EXTERIOR);
<PROGRAM>[a-zA-Z][a-zA-Z0-9]*	{
			yylval = lookup(&yytext[0]);
			return(NAME);
			}
<PROGRAM>[0-9]+|[0-9]*\.[0-9]+	{
			float temp;
			sscanf(yytext, "%f", &temp);
			yylval = (int) fextlgen(temp);
			return(CONST);
			}
<PROGRAM>^\.\.\.forget.*	{
			numitems = sscanf (yytext, "%s %s %s %s %s %s",
				cmd,
				&item[0][0],
				&item[1][0],
				&item[2][0],
				&item[3][0],
				&item[4][0]
			);
			numitems --;
			for (i = 0; i < numitems; i ++)
				forget (lookup (&item[i][0]));
			}
<PROGRAM>^\.\.\.radians.*	radflag = TRUE;
<PROGRAM>^\.\.\.degrees.*	radflag = FALSE;
<PROGRAM>^\.\.\.libfile	{
			omode = LIBFIL;
			BEGIN LIBRARY;
			}
<LIBRARY>[ \t]+		;
<LIBRARY>[^ \t\n]+	{
			idinclude (yytext, LIBFIL);
			BEGIN OUTSIDE + 1;
			}
<LIBRARY>\n		{
			BEGIN PROGRAM + 1;
			}
<PROGRAM>^\.\.\.include	{
			omode = CHATTY;
			BEGIN INCLUDE;
			}
<INCLUDE>[ \t]+		;
<INCLUDE>[^ \t\n]+	{
			idinclude (yytext, CHATTY);
			BEGIN OUTSIDE + 1;
			}
<INCLUDE>\n		{
			BEGIN PROGRAM + 1;
			}
<PROGRAM>^\.I[EF].*\n	{
			interpret ();
			ISnest --;
			if (!ISnest)
				printf ("%s", yytext);
			if (bracenest > 0)
				fprintf (stderr, "ideal: excess {\n");
			BEGIN OUTSIDE;
			}
<PROGRAM>^\..*		{
			sscanf (yytext, "%s", cmd);
			if (strcmp (cmd, "...libfile") && strcmp (cmd, "...include"))
				printf ("%s\n", yytext);
			else
				REJECT;
			}
<PROGRAM>#.*		;
<PROGRAM>"/*"		{
			commnest = 1;
			BEGIN COMMENT;
			}
<COMMENT>"/*"		{
			commnest ++;
			}
<COMMENT>"*/"		{
			commnest --;
			if (!commnest)
				BEGIN PROGRAM;
			}
<COMMENT>.		;
<COMMENT>\n		{
			}
<PROGRAM>"'"[^']*	{
			if (yytext[yyleng-1] == '\\') {
				yytext[yyleng-1] = yytext[yyleng];
				yyleng --;
				yymore();
			} else {
				char *temp;
				temp = malloc((unsigned) yyleng+1);
				yytext[yyleng] = '\0';
				strcpy(temp, &yytext[1]);
				yylval = (int) temp;
				input();
				return(STRING);
			}
			}
<PROGRAM>\"[^\"]*	{
			if (yytext[yyleng-1] == '\\') {
				yytext[yyleng-1] = yytext[yyleng];
				yyleng --;
				yymore();
			} else {
				char *temp;
				temp = malloc((unsigned) yyleng+1);
				yytext[yyleng] = '\0';
				strcpy(temp, &yytext[1]);
				yylval = (int) temp;
				input();
				return(STRING);
			}
			}
<PROGRAM>[ \t]+		{
			if (just_popped) {
				if (omode == LIBFIL)
					BEGIN LIBRARY;
				else if (omode == CHATTY)
					BEGIN INCLUDE;
				else impossible ("idlex");
				just_popped = FALSE;
			}
			}
<PROGRAM>\n		{
			}
<PROGRAM>"{"		{
			bracenest ++;
			return (yylval = LBRACE);
			}				
<PROGRAM>"}"		{
			bracenest --;
			if (bracenest < 0)
				fprintf (stderr, "ideal: excess }\n");
			return (yylval = RBRACE);
			}
<PROGRAM>[\<\>\(\)\[\]\+\-\*\/\=\,\;\:\.\^\~]	return(yytext[0]);
<PROGRAM>.		fprintf(stderr, "ideal: unknown input token %s flushed\n", &yytext[0]);
%%
void filepush (f)
FILE *f;
{
	struct filenode *newfile;
	newfile = (struct filenode *) calloc (1, sizeof (struct filenode));
	if (newfile) {
		newfile->next = filestack;
		newfile->file = curfile = f;
		newfile->head = -1;
		if (filestack)
			filestack->lineno = lineno;
		newfile->name = malloc ((unsigned)strlen(filename)+1);
		strcpy (newfile->name, filename);
		filestack = newfile;
		lineno = 1;
	} else {
		fprintf (stderr, "ideal: no room for file descriptor\n");
		exit (1);
	}
}

void filepop ()
{
	struct filenode *oldfile;
	if (filestack) {
		if (filestack->head > -1)
			fprintf (stderr, "ideal: characters ignored in file %s\n", filename);
		oldfile = filestack;
		filestack = filestack->next;
		fclose (oldfile->file);
		if (filestack) {
			curfile = filestack->file;
			lineno = filestack->lineno;
			filename = filestack->name;
		} else
			curfile = NULL;
		free ((char *)oldfile);
	} else {
		fprintf (stderr, "ideal: file stack botch\n");
		exit (1);
	}
}

int idgetc (f)
struct filenode *f;
{
	int c;
	if (!f)
		return (EOF);
	if (f->head > -1) {
		c = f->buf[f->head--];
	} else
		c = getc(f->file);
	return (c);
}

char input ()
{
	int c;
	c = 0;
	if (filestack)
		c = idgetc(filestack);
	while (c == EOF && filestack) {
		filepop();
		if (filestack) {
			c = idgetc(filestack);
			just_popped = TRUE;
		}
		else
			c = EOF;
	}
	if (c == '\n')
		lineno++;
	return ((c == EOF)?0:c);
}

void unput (c)
char c;
{
	struct filenode *f;
	if (f = filestack) {
		if (f->head < BUFLEN) {
			f->buf[++f->head] = c;
			if (c == '\n')
				-- lineno;
		} else {
			fprintf (stderr, "ideal: out of pushback space\n");
			exit (1);
		}
	}
}
