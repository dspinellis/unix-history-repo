#include	<stdio.h>

/*
 *	This program examines each of its arguments for C function
 * definitions, and puts them in a file "tags" for use by the editor
 * (and anyone else who wants to).
 */

/*
 *	program history:
 *	ken arnold wrote this program.  ask him.
 *	brought over to the vax by peter b. kessler 7/79
 *	who disavows any knowledge of its actions,
 *	except for the stuff related to the construction
 *	of the search patterns.
 *	Some additional enhancements made by Mark Horton, involving
 *	the options and special treatment of "main", "}" at beginning
 *	of line, and a few bug fixes.
 */

#define	reg	register
#define	logical	char

#define	TRUE	(1)
#define	FALSE	(0)

#define	iswhite(arg)	(_wht[arg])	/* T if char is white		*/
#define	begtoken(arg)	(_btk[arg])	/* T if char can start token	*/
#define	intoken(arg)	(_itk[arg])	/* T if char can be in token	*/
#define	endtoken(arg)	(_etk[arg])	/* T if char ends tokens	*/
#define	isgood(arg)	(_gd[arg])	/* T if char can be after ')'	*/

#define	max(I1,I2)	(I1 > I2 ? I1 : I2)

struct	nd_st {			/* sorting structure			*/
	char	*func;			/* function name		*/
	char	*file;			/* file name			*/
	char	*pat;			/* search pattern		*/
	logical	been_warned;		/* set if noticed dup		*/
	struct	nd_st	*left,*right;	/* left and right sons		*/
};

long	ftell();
#ifdef DEBUG
char	*unctrl();
#endif
typedef	struct	nd_st	NODE;

logical	number,				/* T if on line starting with #	*/
	term	= FALSE,		/* T if print on terminal	*/
	makefile= TRUE,			/* T if to creat "tags" file	*/
	gotone,				/* found a func already on line	*/
					/* boolean "func" (see init)	*/
	_wht[0177],_etk[0177],_itk[0177],_btk[0177],_gd[0177];

char	searchar = '?';			/* use ?...? searches 		*/
#define	MAXPATTERN	50		/* according to bill		*/

int	lineno;				/* line number of current line */
char	line[256],		/* current input line			*/
	*curfile,		/* current input file name		*/
	*outfile= "tags",	/* output file				*/
	*white	= " \f\t\n",	/* white chars				*/
	*endtk	= " \t\n\"'#()[]{}=-+%*/&|^~!<>;,.:?",
				/* token ending chars			*/
	*begtk	= "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz",
				/* token starting chars			*/
	*intk	= "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz0123456789",				/* valid in-token chars			*/
	*notgd	= ",;";		/* non-valid after-function chars	*/

int	file_num;		/* current file number			*/
int	aflag;			/* -a: append to tags */
int	uflag;			/* -u: update tags */
int	wflag;			/* -w: suppress warnings */

FILE	*inf,			/* ioptr for current input file		*/
	*outf;			/* ioptr for tags file			*/

long	lineftell;		/* ftell after getc( inf ) == '\n' 	*/

NODE	*head;			/* the head of the sorted binary tree	*/

main(ac,av)
int	ac;
char	*av[];
{
	char cmd[100];
	int i;

	while (ac > 1 && av[1][0] == '-') {
		for (i=1; av[1][i]; i++) {
			switch(av[1][i]) {
				case 'a':
					aflag++;
					break;
				case 'u':
					uflag++;
					break;
				case 'w':
					wflag++;
					break;

				default:
					goto usage;
			}
		}
		ac--; av++;
	}

	if (ac <= 1) {
		usage: printf("Usage: ctags [-au] file ...\n");
		exit(1);
	}

	init();			/* set up boolean "functions"		*/
	/*
	 * loop through files finding functions
	 */
	for (file_num = 1; file_num < ac; file_num++)
		find_funcs(av[file_num]);

	if (uflag) {
		for (i=1; i<ac; i++) {
			sprintf(cmd, "mv %s OTAGS ; fgrep -v '\t%s\t' OTAGS > %s ; rm OTAGS", outfile, av[i], outfile);
			system(cmd);
		}
		aflag++;
	}

	if ((outf = fopen(outfile, aflag ? "a" : "w")) == NULL) {
		perror(outfile);
		exit(1);
	}
	put_funcs(head);	/* put the data in "tags"		*/
	exit(0);
}

/*
 *	This routine sets up the boolean psuedo-functions which work
 * by seting boolean flags dependent upon the corresponding character

 * Every char which is NOT in that string is not a white char.  Therefore,
 * all of the array "_wht" is set to FALSE, and then the elements
 * subscripted by the chars in "white" are set to TRUE.  Thus "_wht"
 * of a char is TRUE if it is the string "white", else FALSE.
 * It also open up the "tags" output file.
 */
init()
{

	reg	char	*sp;
	reg	int	i;

	for (i = 0; i < 0177; i++) {
		_wht[i] = _etk[i] = _itk[i] = _btk[i] = FALSE;
		_gd[i] = TRUE;
	}
	for (sp = white; *sp; sp++)
		_wht[*sp] = TRUE;
	for (sp = endtk; *sp; sp++)
		_etk[*sp] = TRUE;
	for (sp = intk; *sp; sp++)
		_itk[*sp] = TRUE;
	for (sp = begtk; *sp; sp++)
		_btk[*sp] = TRUE;
	for (sp = notgd; *sp; sp++)
		_gd[*sp] = FALSE;
}

/*
 *	This program opens the specified file and calls the function
 * which finds the function defenitions.
 */
find_funcs(file)
char	*file;
{

	if ((inf=fopen(file,"r")) == NULL) {
		perror(file);
		return;
	}

	curfile = (char *) calloc(strlen(file)+1,1);
	strcpy(curfile, file);
	lineno = 1;
	C_funcs();		/* find the C-style functions		*/
	fclose(inf);
}

/*
 *	This routine finds functions in C syntax and adds them
 * to the list.
 */
C_funcs()
{

	reg	char	c,		/* current input char		*/
			*token,		/* start of current token	*/
			*tp;		/* end of current token		*/
	logical	incom,			/* T if inside a comment	*/
		inquote,		/* T if inside a quoted string	*/
		inchar,			/* T if inside a single char '	*/
		midtoken;		/* T if in middle of token	*/
	char	*sp;			/* current input char		*/
	char	tok[100];
	long	insub;			/* level of "{}"s deep		*/

	/*
	 * init boolean flags, counters, and pointers
	 */

	number = gotone = midtoken = inquote = inchar = incom = FALSE;
	insub = 0L;
	sp = tp = token = line;
#ifdef DEBUG
	printf("           t  s c m q c g n\n");
	printf("     s  t  k  u o i u h o u\n");
	printf(" c   p  p  n  b m d o r t m\n");
#endif
	while ((*sp=c=getc(inf)) != EOF) {
#ifdef DEBUG
		printf("%2.2s: ",unctrl(c));
		printf("%2.2s ",unctrl(*sp));
		printf("%2.2s ",unctrl(*tp));
		printf("%2.2s ",unctrl(*token));
		printf("%2ld %d %d %d %d %d %d\n",insub,incom,midtoken,inquote,inchar,gotone,number);
#endif
		/*
		 * action based on mixture of character type, *sp,
		 * and logical flags
		 */

		if (c == '\\') {
			c = *++sp = getc(inf);
			/*
			 * Handling of backslash is very naive.
			 * We do, however, turn escaped newlines
			 * into spaces.
			 */
			if (c = '\n')
				c = ' ';
		}
		else if (incom) {
			if (c == '*') {
				while ((*++sp=c=getc(inf)) == '*') {
#ifdef DEBUG
					printf("%2.2s- ",unctrl(c));
					printf("%2.2s ",unctrl(*sp));
					printf("%2.2s ",unctrl(*tp));
					printf("%2.2s ",unctrl(*token));
					printf("%2ld %d %d %d %d %d %d\n",insub,incom,midtoken,inquote,inchar,gotone,number);
#endif
					continue;
				}
#ifdef DEBUG
				printf("%2.2s- ",unctrl(c));
				printf("%2.2s ",unctrl(*sp));
				printf("%2.2s ",unctrl(*tp));
				printf("%2.2s ",unctrl(*token));
				printf("%2ld %d %d %d %d %d %d\n",insub,incom,midtoken,inquote,inchar,gotone,number);
#endif
				if (c == '/')
					incom = FALSE;
			}
		}
		else if (inquote) {
			/*
			 * Too dumb to know about \" not being magic, but
			 * they usually occur in pairs anyway.
			 */
			if ( c == '"' )
				inquote = FALSE;
			continue;
		     }
		else if (inchar) {
			if ( c == '\'' )
				inchar = FALSE;
			continue;
		     }
		else if (c == '"')
			inquote = TRUE;
		else if (c == '\'')
			inchar = TRUE;
		else if (c == '/')
			if ((*++sp=c=getc(inf)) == '*')
				incom = TRUE;
			else
				ungetc(*sp,inf);
		else if (c == '#' && sp == line)
			number = TRUE;
		else if (c == '{')
			insub++;
		else if (c == '}')
			if (sp == line)
				/*
				 * Kludge to get back in sync after getting confused.
				 * We really shouldn't be looking at indenting style,
				 * but tricking with the preprocessor can get us off,
				 * and most people indent this way anyway.
				 * This resets level of indenting to zero if '}' is
				 * found at beginning of line.
				 */
				insub = 0;
			else
				insub--;
		else if (!insub && !inquote && !inchar && !gotone) {
			if (midtoken) {
				if (endtoken(c)) {
					if (start_func(&sp,token,tp)) {
						strncpy(tok,token,tp-token+1);
						tok[tp-token+1] = 0;
						add_func(tok);
						gotone = TRUE;
					}
					midtoken = FALSE;
					token = sp;
				}
				else if (intoken(c))
					tp++;
			}
			else if (begtoken(c)) {
				token = tp = sp;
				midtoken = TRUE;
			}
		}

		/*
		 * move on to next char, and set flags accordingly
		 */

		sp++;
		if (c == '\n') {
			tp = token = sp = line;
			lineftell = ftell( inf );
#ifdef DEBUG
			printf("lineftell saved as %ld\n",lineftell);
#endif
			number = gotone = midtoken = inquote = inchar = FALSE;
			lineno++;
		}
	}
}

/*
 *	This routine  checks to see if the current token is
 * at the start of a function.  It updates the input line
 * so that the '(' will be in it when it returns.
 */
start_func(lp,token,tp)
char	**lp,*token,*tp;
{

	reg	char	c,*sp,*tsp;
	static	logical	found;
	logical	firsttok;		/* T if have seen first token in ()'s */
	int	bad;

	sp = *lp;
	c = *sp;
	bad = FALSE;
	if (!number)		/* space is not allowed in macro defs	*/
		while (iswhite(c)) {
			*++sp = c = getc(inf);
#ifdef DEBUG
			printf("%2.2s:\n",unctrl(c));
#endif
		}
	/* the following tries to make it so that a #define a b(c)	*/
	/* doesn't count as a define of b.				*/
	else {
		logical	define;

		define = TRUE;
		for (tsp = "define"; *tsp && token < tp; tsp++)
			if (*tsp != *token++) {
				define = FALSE;
				break;
			}
		if (define)
			found = 0;
		else
			found++;
		if (found >= 2) {
			gotone = TRUE;
badone:			bad = TRUE;
			goto ret;
		}
	}
	if (c != '(')
		goto badone;
	firsttok = FALSE;
	while ((*++sp=c=getc(inf)) != ')') {
		/*
		 * This line used to confuse ctags:
		 *	int	(*oldhup)();
		 * This fixes it. A nonwhite char before the first
		 * token, other than a / (in case of a comment in there)
		 * makes this not a declaration.
		 */
		if (begtoken(c) || c=='/') firsttok++;
		else if (!iswhite(c) && !firsttok) goto badone;
#ifdef DEBUG
		printf("%2.2s:\n",unctrl(c));
#endif
	}
#ifdef DEBUG
	printf("%2.2s:\n",unctrl(c));
#endif
	while (iswhite(*++sp=c=getc(inf)))
#ifdef DEBUG
		printf("%2.2s:\n",unctrl(c))
#endif
		;
#ifdef DEBUG
	printf("%2.2s:\n",unctrl(c));
#endif
ret:
	*lp = --sp;
	ungetc(c,inf);
	return !bad && isgood(c);
}

/*
 *	This routine adds a function to the list
 */
add_func(token)
char *token;
{
	reg	char	*fp,*pp;
	reg	NODE	*np;

	if ((np = (NODE *) calloc(1,sizeof (NODE))) == NULL) {
		printf("too many functions to sort\n");
		put_funcs(head);
		free_tree(head);
		head = np = (NODE *) calloc(1,sizeof (NODE));
	}
	if (strcmp(token,"main") == 0) {
		/*
		 * Since there are so many directories with lots of
		 * misc. complete programs in them, main tends to get
		 * redefined a lot. So we change all mains to instead
		 * refer to the name of the file, without leading
		 * pathname components and without a trailing .c.
		 */
		fp = curfile;
		for (pp=curfile; *pp; pp++)
			if (*pp == '/')
				fp = pp+1;
		*token = 'M';
		strcpy(token+1, fp);
		pp = &token[strlen(token)-2];
		if (*pp == '.')
			*pp = 0;
	}
	fp = np->func = (char *) calloc(strlen(token)+1,sizeof (char));
	np->file = curfile;
	strcpy(fp, token);
	{	/*
		 * this change to make the whole line the pattern
		 */
	    long	saveftell = ftell( inf );
	    int		patlen;
	    char	ch;

	    patlen = 0;
	    fseek( inf , lineftell , 0 );
#ifdef DEBUG
	    printf("saveftell=%ld, lseek back to %ld\n",saveftell,lineftell);
#endif
	    ch = getc( inf );
	    while ( ch != '\n' && ch != searchar && patlen < MAXPATTERN ) {
		patlen ++;
		ch = getc( inf );
	    }
	    pp = np -> pat = (char *) calloc( patlen + 2 , sizeof( char ) );
	    fseek( inf , lineftell , 0 );
	    ch = getc( inf );
	    while ( patlen -- ) {
		*pp ++ = ch;
		ch = getc( inf );
	    }
	    if ( ch == '\n' )
		*pp ++ = '$';
	    *pp = '\0';
	    fseek( inf , saveftell , 0 );
#ifdef DEBUG
	    printf("seek back to %ld, ftell is now %ld\n",saveftell,ftell(inf));
#endif
	}
#ifdef DEBUG
	printf("\"%s\"\t\"%s\"\t\"%s\"\n",np->func,np->file,np->pat);
#endif
	if (head == NULL)
		head = np;
	else
		add_node(np,head);
}

/*
 *	This routine cfrees the entire tree from the node down.
 */
free_tree(node)
NODE	*node;
{

	while (node) {
		free_tree(node->right);
		cfree(node);
		node = node->left;
	}
}

/*
 *	This routine finds the node where the new function node
 * should be added.
 */
add_node(node,cur_node)
NODE	*node,*cur_node;
{

	reg	int	dif;

	dif = strcmp(node->func,cur_node->func);
#ifdef DEBUG
	printf("strcmp(\"%s\",\"%s\") == %d\n",node->func,cur_node->func,dif);
#endif
	if (dif == 0) {
		if (node->file == cur_node->file) {
			if (!wflag) {
				fprintf(stderr,"Duplicate function in file \"%s\", line %d: %s\n",node->file,lineno,node->func);
				fprintf(stderr,"Second entry ignored\n");
			}
			return;
		}
		else {
			if (!cur_node->been_warned)
				if (!wflag)
					fprintf(stderr,"Duplicate function name in files %s and %s: %s (Warning only)\n",
						node->file, cur_node->file, node->func);
			cur_node->been_warned = TRUE;
		}
	}
	if (dif < 0)
		if (cur_node->left != NULL)
			add_node(node,cur_node->left);
		else {
#ifdef DEBUG
			printf("adding to left branch\n");
#endif
			cur_node->left = node;
		}
	else
		if (cur_node->right != NULL)
			add_node(node,cur_node->right);
		else {
#ifdef DEBUG
			printf("adding to right branch\n");
#endif
			cur_node->right = node;
		}
}

/*
 *	This routine puts the functions in the file.
 */
put_funcs(node)
NODE	*node;
{

	if (node == NULL)
		return;
	put_funcs(node->left);
	fprintf(outf,"%s\t%s\t%c^%s%c\n",node->func,node->file
	       ,searchar,node->pat,searchar);
	put_funcs(node->right);
}

#ifdef DEBUG
char *
unctrl(c)
char c;
{
	static char buf[3];
	if (c>=' ' && c<='~') {
		buf[0] = c;
		buf[1] = 0;
	} else if (c > '~') {
		buf[0] = '^';
		buf[1] = '?';
		buf[2] = 0;
	} else if (c < 0) {
		buf[0] = buf[1] = '?';
		buf[2] = 0;
	} else {
		buf[0] = '\\';
		buf[2] = 0;
		switch(c) {
		case '\b':
			buf[1] = 'b';
			break;
		case '\t':
			buf[1] = 't';
			break;
		case '\n':
			buf[1] = 'n';
			break;
		default:
			buf[0] = '^';
			buf[1] = c + 64;
		}
	}
	return(buf);
}
#endif
