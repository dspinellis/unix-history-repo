/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)ctags.x	5.1 (Berkeley) 10/22/88";
#endif not lint

#include <sys/types.h>
#include <sys/file.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>

/*
 * ctags: create a tags file
 */

#define	bool	char

#define	YES		1
#define	NO		0
#define	EOS		'\0'
#define	MAXTOKEN	250		/* max size of single token */
#define	SETLINE		{++lineno;lineftell = ftell(inf);}

#define	iswhite(arg)	(_wht[arg])	/* T if char is white */
#define	begtoken(arg)	(_btk[arg])	/* T if char can start token */
#define	intoken(arg)	(_itk[arg])	/* T if char can be in token */
#define	endtoken(arg)	(_etk[arg])	/* T if char ends tokens */
#define	isgood(arg)	(_gd[arg])	/* T if char can be after ')' */

typedef struct nd_st {			/* sorting structure */
	struct nd_st	*left,
			*right;		/* left and right sons */
	char	*entry,			/* function or type name */
		*file,			/* file name */
		*pat;			/* search pattern */
	int	lno;			/* for -x option */
	bool	been_warned;		/* set if noticed dup */
} NODE;
NODE	*head;			/* head of the sorted binary tree */

				/* boolean "func" (see init()) */
bool	_wht[0177],_etk[0177],_itk[0177],_btk[0177],_gd[0177];

FILE	*inf,			/* ioptr for current input file */
	*outf;			/* ioptr for tags file */

long	lineftell;		/* ftell after getc( inf ) == '\n' */

int	lineno,			/* line number of current line */
	aflag,			/* -a: append to tags */
	dflag,			/* -d: non-macro defines */
	tflag,			/* -t: create tags for typedefs */
	uflag,			/* -u: update tags */
	wflag,			/* -w: suppress warnings */
	vflag,			/* -v: vgrind style index output */
	xflag;			/* -x: cxref style output */

char	*curfile,		/* current input file name */
	searchar = '/',		/* use /.../ searches by default */
	line[4*BUFSIZ],		/* current input line */
	lbuf[BUFSIZ];

main(argc,argv)
	int	argc;
	char	**argv;
{
	extern char	*optarg;		/* getopt arguments */
	extern int	optind;
	static char	*outfile = "tags";	/* output file */
	int	exit_val,			/* exit value */
		step,				/* step through args */
		ch;				/* getopts char */
	char	cmd[100],			/* too ugly to explain */
		*savestr();

	while ((ch = getopt(argc,argv,"BFadf:tuwvx")) != EOF)
		switch((char)ch) {
			case 'B':
				searchar = '?';
				break;
			case 'F':
				searchar = '/';
				break;
			case 'a':
				aflag++;
				break;
			case 'd':
				dflag++;
				break;
			case 'f':
				outfile = optarg;
				break;
			case 't':
				tflag++;
				break;
			case 'u':
				uflag++;
				break;
			case 'w':
				wflag++;
				break;
			case 'v':
				vflag++;
			case 'x':
				xflag++;
				break;
			case '?':
			default:
				goto usage;
		}
	argv += optind;
	argc -= optind;
	if (!argc) {
usage:		puts("Usage: ctags [-BFadtuwvx] [-f tagsfile] file ...");
		exit(1);
	}

	init();

	for (exit_val = step = 0;step < argc;++step) {
		curfile = savestr(argv[step]);
		if (!(inf = fopen(argv[step],"r"))) {
			perror(argv[step]);
			exit_val = 1;
		}
		else
			find_entries(argv[step]);
	}

	if (xflag) {
		put_entries(head);
		exit(exit_val);
	}
	if (uflag) {
		for (step = 0;step < argc;step++) {
			(void)sprintf(cmd,"mv %s OTAGS;fgrep -v '\t%s\t' OTAGS >%s;rm OTAGS",outfile,argv[step],outfile);
			system(cmd);
		}
		++aflag;
	}
	outf = fopen(outfile, aflag ? "a" : "w");
	if (!outf) {
		perror(outfile);
		exit(exit_val);
	}
	put_entries(head);
	(void)fclose(outf);
	if (uflag) {
		(void)sprintf(cmd,"sort %s -o %s",outfile,outfile);
		system(cmd);
	}
	exit(exit_val);
}

/*
 * init --
 *	this routine sets up the boolean psuedo-functions which work by
 *	setting boolean flags dependent upon the corresponding character.
 *	Every char which is NOT in that string is false with respect to
 *	the pseudo-function.  Therefore, all of the array "_wht" is NO
 *	by default and then the elements subscripted by the chars in
 *	CWHITE are set to YES.  Thus, "_wht" of a char is YES if it is in
 *	the string CWHITE, else NO.
 */
init()
{
	register int	i;
	register char	*sp;

	for (i = 0; i < 0177; i++) {
		_wht[i] = _etk[i] = _itk[i] = _btk[i] = NO;
		_gd[i] = YES;
	}
#define	CWHITE	" \f\t\n"
	for (sp = CWHITE; *sp; sp++)	/* white space chars */
		_wht[*sp] = YES;
#define	CTOKEN	" \t\n\"'#()[]{}=-+%*/&|^~!<>;,.:?"
	for (sp = CTOKEN; *sp; sp++)	/* token ending chars */
		_etk[*sp] = YES;
#define	CINTOK	"ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz0123456789"
	for (sp = CINTOK; *sp; sp++)	/* valid in-token chars */
		_itk[*sp] = YES;
#define	CBEGIN	"ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"
	for (sp = CBEGIN; *sp; sp++)	/* token starting chars */
		_btk[*sp] = YES;
#define	CNOTGD	",;"
	for (sp = CNOTGD; *sp; sp++)	/* invalid after-function chars */
		_gd[*sp] = NO;
}

/*
 * find_entries --
 *	this routine opens the specified file and calls the function
 *	which searches the file.
 */
find_entries(file)
	char	*file;
{
	register char	*cp;

	lineno = 0;				/* should be 1 ?? KB */
	if (cp = rindex(file, '.')) {
		if (cp[1] == 'l' && !cp[2]) {
/* lisp */		if (index(";([",(char)first_char())) {
				L_funcs();
				goto done;
			}
/* lex */		else {
				toss_yysec();
				getline();
				pfnote("yylex",lineno);
				toss_yysec();
			}
		}
/* yacc */	else if (cp[1] == 'y' && !cp[2])
			Y_entries();
/* fortran */	else if ((cp[1] != 'c' && cp[1] != 'h') && !cp[2]) {
			if (PF_funcs())
				goto done;
			rewind(inf);
		}
	}
	c_entries();		/* default: try C */
done:	(void)fclose(inf);
}

/*
 * pfnote --
 *	enter a new node in the tree
 */
pfnote(name,ln)
	char	*name;
	int	ln;
{
	register NODE	*np;
	register char	*fp;
	char	nbuf[BUFSIZ],
		*malloc(), *savestr();

	if (!(np = (NODE *)malloc(sizeof(NODE)))) {
		fputs("ctags: too many entries to sort\n",stderr);
		put_entries(head);
		free_tree(head);
		if (!(np = (NODE *)malloc(sizeof(NODE)))) {
			fputs("ctags: out of space.\n",stderr);
			exit(1);
		}
		head = np;
	}
	if (!xflag && !bcmp(name,"main",4)) {
		if (!(fp = rindex(curfile,'/')))
			fp = curfile;
		else
			++fp;
		(void)sprintf(nbuf,"M%s",fp);
		fp = rindex(nbuf,'.');
		if (fp && !fp[2])
			*fp = 0;
		name = nbuf;
	}
	np->entry = savestr(name);
	np->file = curfile;
	np->lno = ln;
	np->left = np->right = 0;
	if (!xflag) {
		lbuf[50] = 0;
		(void)strcat(lbuf,"$");
		lbuf[50] = 0;
	}
	np->pat = savestr(lbuf);
	if (!head)
		head = np;
	else
		add_node(np, head);
}

/*
 * c_entries --
 *	read .c and .h files and call appropriate routines
 */
c_entries()
{
	register int	c,		/* current character */
			level;		/* brace level */
	register char	*sp;		/* buffer pointer */
	int	token,			/* if reading a token */
		t_def,			/* if reading a typedef */
		t_level;		/* typedef's brace level */
	char	tok[MAXTOKEN];		/* token buffer */

	lineftell = ftell(inf);
	sp = tok; token = t_def = NO; t_level = -1; level = 0; lineno = 1;
	while ((c = getc(inf)) != EOF) {

	switch ((char)c) {
		/*
		 * Here's where it DOESN'T handle:
		 *	foo(a)
		 *	{
		 *	#ifdef notdef
		 *		}
		 *	#endif
		 *		if (a)
		 *			puts("hello, world");
		 *	}
		 */
		case '{':
			++level;
			goto endtok;
		case '}':
			/*
			 * if level goes below zero, try and fix
			 * it, even though we've already messed up
			 */
			if (--level < 0)
				level = 0;
			goto endtok;

		case '\n':
			SETLINE;
			/*
			 * the above 3 cases are similar in that they
			 * are special characters that also end tokens.
			 */
endtok:			if (sp > tok) {
				*sp = EOS;
				token = YES;
				sp = tok;
			}
			else
				token = NO;
			continue;

		/* we ignore quoted strings and comments in their entirety */
		case '"':
		case '\'':
			skip_key(c);
			break;
		case '/':
			if ((c = getc(inf)) == '*') {
				skip_comment();
				break;
			}
			(void)ungetc(c,inf);
			c = '/';
			goto storec;

		/* hash marks are interesting if they start #define's. */
		case '#':
			if (sp == tok) {
				hash_entry();
				break;
			}
			goto storec;

		/*
	 	 * if we have a current token, parenthesis on
		 * level zero indicates a function.
		 */
		case '(':
			if (!level && token) {
				int	curline;

				if (sp != tok)
					*sp = EOS;
				/*
				 * grab the line immediately, we may
				 * already be wrong, for example,
				 *	foo
				 *	(arg1,
				 */
				getline();
				curline = lineno;
				if (func_entry()) {
					++level;
					pfnote(tok,curline);
				}
				break;
			}
			goto storec;

		/*
		 * semi-colons are interesting in that they indicate the end
		 * of a typedef; if we find a typedef we search for the next
		 * semi-colon of the same level as the typedef.  They are
		 * fairly tough, can be:
		 *
		 *	"typedef long time_t;"
		 *	"typedef unsigned int u_int;"
		 *	"typedef unsigned int u_int [10];"
		 *
		 * If looking at a typedef, we save a copy of the last token
		 * found.  Then, when we find the ';' we take the current
		 * token if it starts with a valid token name, else we take
		 * the one we saved.  There's probably some reasonable
		 * alternative to this...
		 */
		case ';':
			if (t_def && level == t_level) {
				t_def = NO;
				getline();
				if (sp != tok)
					*sp = EOS;
				pfnote(tok,lineno);
				break;
			}
			goto storec;

		/*
		 * store characters until one that can't be part of a token
		 * comes along; check the current token against certain
		 * reserved words.
		 */
		default:
storec:			if (!intoken(c)) {
				if (sp == tok)
					break;
				*sp = EOS;
				if (tflag) {
					/* no typedefs inside typedefs */
					if (!t_def && !bcmp(tok,"typedef",8)) {
						t_def = YES;
						t_level = level;
						break;
					}
					/* catch "typedef struct" */
					if ((!t_def || t_level < level)
					    && (!bcmp(tok,"struct",7)
					    || !bcmp(tok,"union",6)
					    || !bcmp(tok,"enum",5))) {
						/*
						 * get line immediately;
						 * may change before '{'
						 */
						getline();
						if (str_entry(c))
							++level;
						break;
					}
				}
				sp = tok;
			}
			else if (sp != tok || begtoken(c)) {
				*sp++ = c;
				token = YES;
			}
			continue;
		}
		sp = tok;
		token = NO;
	}
}

/*
 * func_entry --
 *	handle a function reference
 */
func_entry()
{
	register int	c;		/* current character */

	skip_key((int)')');
	while ((c = getc(inf)) != EOF && iswhite(c))
		if (c == (int)'\n')
			SETLINE;
	if (!intoken(c) && c != (int)'{')
		return(NO);
	if (c != (int)'{')
		skip_key((int)'{');
	return(YES);
}

/*
 * hash_entry --
 *	handle a line starting with a '#'
 */
hash_entry()
{
	register int	c,		/* character read */
			curline;	/* line started on */
	register char	*sp;		/* buffer pointer */
	char	tok[MAXTOKEN];		/* storage buffer */

	curline = lineno;
	for (sp = tok;;) {		/* get next token */
		if ((c = getc(inf)) == EOF)
			return;
		if (iswhite(c))
			break;
		*sp++ = c;
	}
	*sp = EOS;
	if (bcmp(tok,"define",6))	/* only interested in #define's */
		goto skip;
	for (;;) {			/* this doesn't handle "#define \n" */
		if ((c = getc(inf)) == EOF)
			return;
		if (!iswhite(c))
			break;
	}
	for (sp = tok;;) {		/* get next token */
		*sp++ = c;
		if ((c = getc(inf)) == EOF)
			return;
		/*
		 * this is where it DOESN'T handle
		 * "#define \n"
		 */
		if (!intoken(c))
			break;
	}
	*sp = EOS;
	if (dflag || c == (int)'(') {	/* only want macros */
		getline();
		pfnote(tok,curline);
	}
skip:	if (c == (int)'\n') {		/* get rid of rest of define */
		SETLINE
		if (*(sp - 1) != '\\')
			return;
	}
	skip_line();
}

/*
 * str_entry --
 *	handle a struct, union or enum entry
 */
str_entry(c)
	register int	c;		/* current character */
{
	register char	*sp;		/* buffer pointer */
	int	curline;		/* line started on */
	char	tok[BUFSIZ];		/* storage buffer */

	curline = lineno;
	while (iswhite(c))
		if ((c = getc(inf)) == EOF)
			return(NO);
	if (c == (int)'{')		/* it was "struct {" */
		return(YES);
	for (sp = tok;;) {		/* get next token */
		*sp++ = c;
		if ((c = getc(inf)) == EOF)
			return(NO);
		if (!intoken(c))
			break;
	}
	switch ((char)c) {
		case '{':		/* it was "struct foo{" */
			--sp;
			break;
		case '\n':		/* it was "struct foo\n" */
			SETLINE;
			/*FALLTHROUGH*/
		default:		/* probably "struct foo " */
			while ((c = getc(inf)) != EOF)
				if (!iswhite(c))
					break;
			if (c != (int)'{')
				return(NO);
	}
	*sp = EOS;
	pfnote(tok,curline);
	return(YES);
}

/*
 * skip_line --
 *	skip to next line
 */
skip_line()
{
	register int	c,
			savec;

	for (savec = '\0';(c = getc(inf)) != EOF;savec = c)
		if (c == (int)'\n') {
			SETLINE;
			if (savec != (int)'\\')
				return;
		}
}

/*
 * skip_key --
 *	skip to next char "key"
 */
skip_key(key)
	register int	key;
{
	register int	c;

	while((c = getc(inf)) != EOF && c != key)
		if (c == (int)'\n')
			SETLINE;
}

/*
 * skip_comment --
 *	skip over comment
 */
skip_comment()
{
	register int	c,
			level,
			star,
			slash;

	for (level = 1,slash = star = 0;(c = getc(inf)) != EOF;)
		switch((char)c) {
			case '*':
				if (slash) {
					++level;
					slash = 0;
				}
				else
					++star;
				break;
			case '/':
				if (star) {
					if (!--level)
						return;
					star = 0;
				}
				else
					++slash;
				break;
			case '\n':
				SETLINE;
			default:
				slash = star = 0;
		}
}

/*
 * Y_entries:
 *	Find the yacc tags and put them in.
 */
Y_entries()
{
	register char	*sp,
			*orig_sp;
	register int	brace;
	register bool	in_rule,
			toklen;
	char	tok[BUFSIZ],
		*toss_comment();

	toss_yysec();
	brace = 0;
	getline();
	pfnote("yyparse",lineno);
	while (fgets(line,sizeof(line),inf))
	    for (sp = line;*sp;++sp)
		switch (*sp) {
		case '\n':
			lineno++;
			/* FALLTHROUGH */
		case ' ':
		case '\t':
		case '\f':
		case '\r':
			break;
		case '"':
			do {
				while (*++sp != '"')
					continue;
			} while (sp[-1] == '\\');
			break;
		case '\'':
			do {
				while (*++sp != '\'')
					continue;
			} while (sp[-1] == '\\');
			break;
		case '/':
			if (*++sp == '*')
				sp = toss_comment(sp);
			else
				--sp;
			break;
		case '{':
			brace++;
			break;
		case '}':
			brace--;
			break;
		case '%':
			if (sp[1] == '%' && sp == line)
				return;
			break;
		case '|':
		case ';':
			in_rule = NO;
			break;
		default:
			if (!brace && !in_rule && (isalpha(*sp) ||
			    *sp == '.' || *sp == '_')) {
				orig_sp = sp;
				++sp;
				while (isalnum(*sp) || *sp == '_' ||
				    *sp == '.')
					sp++;
				toklen = sp - orig_sp;
				while (isspace(*sp))
					sp++;
				if (*sp == ':' || (*sp == '\0' &&
				    first_char() == ':')) {
					(void)strncpy(tok, orig_sp, toklen);
					tok[toklen] = '\0';
					(void)strcpy(lbuf, line);
					lbuf[strlen(lbuf) - 1] = '\0';
					pfnote(tok,lineno);
					in_rule = YES;
				}
				else
					sp--;
			}
			break;
		}
}

char *
toss_comment(start)
	char	*start;
{
	register char	*sp;

	/*
	 * first, see if the end-of-comment is on the same line
	 */
	do {
		while ((sp = index(start,'*')))
			if (sp[1] == '/')
				return(++sp);
			else
				start = ++sp;
		start = line;
		lineno++;
	} while(fgets(line,sizeof(line),inf));
}

/*
 * getline --
 *	get the line the token of interest occurred on
 */
getline()
{
	register char	*cp;
	long	saveftell;

	saveftell = ftell(inf);
	fseek(inf,lineftell,L_SET);
	(void)fgets(lbuf,sizeof(lbuf),inf);
	if (cp = index(lbuf,'\n'))
		*cp = '\0';
	fseek(inf,saveftell,L_SET);
}

free_tree(node)
	register NODE	*node;
{
	while (node) {
		free_tree(node->right);
		cfree(node);
		node = node->left;
	}
}

add_node(node,cur_node)
	register NODE	*node,
			*cur_node;
{
	register int	dif;

	dif = strcmp(node->entry,cur_node->entry);
	if (!dif) {
		if (node->file == cur_node->file) {
			if (!wflag)
				fprintf(stderr,"Duplicate entry in file %s, line %d: %s\nSecond entry ignored\n",node->file,lineno,node->entry);
			return;
		}
		if (!cur_node->been_warned)
			if (!wflag)
				fprintf(stderr,"Duplicate entry in files %s and %s: %s (Warning only)\n",node->file,cur_node->file,node->entry);
		cur_node->been_warned = YES;
	}
	else if (dif < 0)
		if (cur_node->left)
			add_node(node,cur_node->left);
		else
			cur_node->left = node;
	else if (cur_node->right)
		add_node(node,cur_node->right);
	else
		cur_node->right = node;
}

/*
 * put_entries --
 *	write out the tags
 */
put_entries(node)
	register NODE	*node;
{
	register char	*sp;

	if (!node)
		return;
	put_entries(node->left);
	if (!xflag) {
		fprintf(outf,"%s\t%s\t%c^",node->entry,node->file,searchar);
		for (sp = node->pat; *sp; sp++)
			if (*sp == '\\')
				fputs("\\\\",outf);
			else if (*sp == searchar)
				fprintf(outf,"\\%c",searchar);
			else
				putc(*sp,outf);
		fprintf(outf,"%c\n",searchar);
	}
	else if (vflag)
		printf("%s %s %d\n",node->entry,node->file,(node->lno+63)/64);
	else
		printf("%-16s%4d %-16s %s\n",node->entry,node->lno,node->file,node->pat);
	put_entries(node->right);
}

char	*dbp = lbuf;
int	pfcnt;

PF_funcs()
{
	pfcnt = 0;
	while (fgets(lbuf, sizeof(lbuf), inf)) {
		lineno++;
		dbp = lbuf;
		if ( *dbp == '%' ) dbp++ ;	/* Ratfor escape to fortran */
		while (isspace(*dbp))
			dbp++;
		if (*dbp == 0)
			continue;
		switch (*dbp |' ') {

		  case 'i':
			if (tail("integer"))
				takeprec();
			break;
		  case 'r':
			if (tail("real"))
				takeprec();
			break;
		  case 'l':
			if (tail("logical"))
				takeprec();
			break;
		  case 'c':
			if (tail("complex") || tail("character"))
				takeprec();
			break;
		  case 'd':
			if (tail("double")) {
				while (isspace(*dbp))
					dbp++;
				if (*dbp == 0)
					continue;
				if (tail("precision"))
					break;
				continue;
			}
			break;
		}
		while (isspace(*dbp))
			dbp++;
		if (*dbp == 0)
			continue;
		switch (*dbp|' ') {

		  case 'f':
			if (tail("function"))
				getit();
			continue;
		  case 's':
			if (tail("subroutine"))
				getit();
			continue;
		  case 'p':
			if (tail("program")) {
				getit();
				continue;
			}
			if (tail("procedure"))
				getit();
			continue;
		}
	}
	return (pfcnt);
}

tail(cp)
	char *cp;
{
	register int len = 0;

	while (*cp && (*cp&~' ') == ((*(dbp+len))&~' '))
		cp++, len++;
	if (*cp == 0) {
		dbp += len;
		return (1);
	}
	return (0);
}

takeprec()
{
	while (isspace(*dbp))
		dbp++;
	if (*dbp != '*')
		return;
	dbp++;
	while (isspace(*dbp))
		dbp++;
	if (!isdigit(*dbp)) {
		--dbp;		/* force failure */
		return;
	}
	do
		dbp++;
	while (isdigit(*dbp));
}

getit()
{
	register char *cp;
	char c;
	char	nambuf[BUFSIZ];

	for (cp = lbuf; *cp; cp++)
		;
	*--cp = 0;	/* zap newline */
	while (isspace(*dbp))
		dbp++;
	if (*dbp == 0 || !isalpha(*dbp))
		return;
	for (cp = dbp+1; *cp && (isalpha(*cp) || isdigit(*cp)); cp++)
		continue;
	c = cp[0];
	cp[0] = 0;
	(void)strcpy(nambuf, dbp);
	cp[0] = c;
	pfnote(nambuf, lineno);
	pfcnt++;
}

char *
savestr(str)
	char	*str;
{
	register char	*space;
	char	*malloc();

	if (!(space = malloc((u_int)(strlen(str) + 1)))) {
		fputs("ctags: no more space.\n",stderr);
		exit(1);
	}
	return(strcpy(space,str));
}

/*
 * lisp tag functions
 * just look for (def or (DEF
 */
L_funcs()
{
	register int	special;

	pfcnt = 0;
	while (fgets(lbuf, sizeof(lbuf), inf)) {
		lineno++;
		dbp = lbuf;
		if (dbp[0] == '(' &&
		   (dbp[1] == 'D' || dbp[1] == 'd') &&
		   (dbp[2] == 'E' || dbp[2] == 'e') &&
		   (dbp[3] == 'F' || dbp[3] == 'f')) {
			dbp += 4;
			if (striccmp(dbp, "method") == 0 ||
			    striccmp(dbp, "wrapper") == 0 ||
			    striccmp(dbp, "whopper") == 0)
				special = YES;
			else
				special = NO;
			while (!isspace(*dbp))
				++dbp;
			while (isspace(*dbp))
				++dbp;
			L_getit(special);
		}
	}
}

L_getit(special)
	int	special;
{
	register char	*cp,
			c;
	char	nambuf[BUFSIZ];

	for (cp = lbuf;*cp;cp++);
	*--cp = 0;		/* zap newline */
	if (!*dbp)
		return;
	if (special) {
		if (!(cp = index(dbp,')')))
			return;
		for (;cp >= dbp && *cp != ':';--cp);
		if (cp < dbp)
			return;
		dbp = cp;
		for (;*cp && *cp != ')' && *cp != ' ';++cp);
	}
	else
		for (cp = dbp + 1; *cp && *cp != '(' && *cp != ' '; cp++);
	c = cp[0];
	cp[0] = 0;
	(void)strcpy(nambuf,dbp);
	cp[0] = c;
	pfnote(nambuf,lineno);
	++pfcnt;
}

/*
 * striccmp:
 *	Compare two strings over the length of the second, ignoring
 *	case distinctions.  If they are the same, return 0.  If they
 *	are different, return the difference of the first two different
 *	characters.  It is assumed that the pattern (second string) is
 *	completely lower case.
 */
striccmp(str, pat)
	register char	*str, *pat;
{
	register int	c1;

	while (*pat) {
		if (isupper(*str))
			c1 = tolower(*str);
		else
			c1 = *str;
		if (c1 != *pat)
			return c1 - *pat;
		pat++;
		str++;
	}
	return 0;
}

/*
 * first_char --
 *	return the next non-blank character in the file.  After finding
 *	it, rewind the input file to the starting position.
 */
first_char()
{
	register int	c;
	register long	off;

	off = ftell(inf);
	while ((c = getc(inf)) != EOF)
		if (!iswhite(c)) {
			(void)fseek(inf,off,L_SET);
			return(c);
		}
	(void)fseek(inf,off,L_SET);
	return(EOF);
}

/*
 * toss_yysec --
 *	toss away code until the next "%%" line.
 */
toss_yysec()
{
	char	buf[BUFSIZ];

	for (;;) {
		lineftell = ftell(inf);
		if (!fgets(buf,sizeof(buf),inf))
			return;
		++lineno;
		if (!strncmp(buf,"%%",2))
			return;
	}
}
