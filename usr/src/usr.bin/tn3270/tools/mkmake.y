%{
typedef struct string {
    int
	hashval,
	length;
    char
	*string;
    struct string
	*next;
} string_t;

typedef struct same {
    int
	myname,
	values,		/* If a variable, the values */
	action;		/* If a target, the action number */
    struct same
	*moretokens,
	*depends;
    string_t
	*string;
} same_t;

same_t
    *add_depends(),
    *assign(),
    *token_cat(),
    *token_item();
%}

%union {
    string_t *string;
    same_t *same;
    }

%token <string> TOKEN
%token END_OF_FILE NL
%type <same> target assignment actions action tokens token
%%
makefile : lines END_OF_FILE;

lines : line
    | lines line
    ;

line : NL
    | target NL
    | assignment NL
    | actions NL
    ;

target : tokens ':' tokens
    {
	$$ = add_depends($1, $3);
    }
    ;

assignment : token '=' tokens
    {
	$$ = assign($1, $3);
    }
    ;

actions: action
    | actions action
    {
	$$ = token_cat($1, $2);
    }
    ;

action:	'\t' tokens
    {
	$$ = $2;
    }
    ;

tokens : token
    | tokens token
    {
	$$ = token_cat($1, $2);
    }
    ;
token: TOKEN
    {
	$$ = token_item($1);
    }
    ;
%%
#include <stdio.h>

static int last_char, last_saved = 0;
static int column = 0, lineno = 1;


static string_t *strings = 0;
static same_t
    *variables = 0,
    *targets = 0,
    *actions = 0;

extern char *malloc();

main()
{
#define	YYDEBUG
    extern int yydebug;

    return yyparse();
}

yyerror(s)
char *s;
{
    fprintf(stderr, "line %d, column %d: %s\n", lineno, column, s);
}

same_t *
add_depends(list1, list2)
same_t
    *list1,
    *list2;
{
}

same_t *
assign(variable, value)
same_t
    *variable,
    *value;
{
}


same_t *
token_cat(tokens, token)
same_t
    *tokens,
    *token;
{
    same_t *ptr;

    if (tokens->moretokens == 0) {
	tokens->moretokens = token;
    } else {
	for (ptr = tokens; ptr->moretokens; ptr = ptr->moretokens) {
	    ;
	}
	ptr->moretokens = token;
    }
    return tokens;
}

same_t *
token_item(string)
string_t *string;
{
    same_t *ptr;

    if ((ptr = (same_t *)malloc(sizeof *ptr)) == 0) {
	fprintf(stderr, "No more space for tokens!\n");
	exit(1);
    }

    ptr->string = string;
    ptr->moretokens = 0;
    return ptr;
}

int
Getchar()
{
    if (last_saved) {
	last_saved = 0;
	return last_char;
    } else {
	int c;
	c = getchar();
	switch (c) {
	case '\n':
	    lineno++;
	    column = 0;
	    break;
	default:
	    column++;
	}
	return c;
    }
}

int
hashof(string, length)
char *string;
int length;
{
    register int i = 0;

    while (length--) {
	i = (i<<3) + *string ^ ((i>>28)&0x7);
    }
    return i;
}

string_t *
lookup(string)
char *string;
{
    int hashval;
    int length = strlen(string);
    string_t *ptr;

    hashval = hashof(string, length);

    for (ptr = strings; ptr; ptr = ptr->next) {
	if ((ptr->hashval == hashval) && (ptr->length == length)) {
	    if (memcmp(string, ptr->string, length) == 0) {
		return ptr;
	    }
	}
    }
    if ((ptr = (string_t *)malloc(sizeof *ptr)) == 0) {
	fprintf(stderr, "No space to add string *%s*!\n", string);
	exit(1);
    }
    ptr->hashval = hashval;
    ptr->length = length;
    if ((ptr->string = malloc(length+1)) == 0) {
	fprintf(stderr, "No space to add literal *%s*!\n", string);
	exit(1);
    }
    memcpy(ptr->string, string, length+1);
    ptr->next = strings;
    strings = ptr;
    return ptr;
}


yylex()
{
#define	ret_token(c)	if (bufptr != buffer) { \
			    save(c); \
			    *bufptr = 0; \
			    bufptr = buffer; \
			    yylval.string = lookup(buffer); \
			    return TOKEN; \
			}
#define	save(c)	{ last_char = c; last_saved = 1; }
#define	Return(c)	if (yydebug) { \
			    printf("[%d]", c); \
			    fflush(stdout); \
			} \
			return c;

    static char buffer[100], *bufptr = buffer;
    static int eof_found = 0;
    int c;

    if (eof_found != 0) {
	eof_found++;
	if (eof_found > 2) {
	    fprintf(stderr, "End of file ignored.\n");
	    exit(1);
	}
	Return(END_OF_FILE);
    }
    while ((c = Getchar()) != EOF) {
	switch (c) {
	case '#':
	    ret_token(c);
	    while (((c = Getchar()) != EOF) && (c != '\n')) {
		;
	    }
	    save(c);
	    break;
	case ' ':
	    ret_token(' ');
	    break;
	case '\t':
	    ret_token(c);
	    if (column == 1) {
		Return(c);
	    }
	    break;
	case ':':
	case '=':
	    ret_token(c);
	    Return(c);
	case '\n':
	    if (bufptr != buffer) {
		if (bufptr[-1] == '\\') {
		    bufptr--;
		    break;
		}
	    }
	    ret_token(c);
	    Return(NL);
	default:
	    *bufptr++ = c;
	    break;
	}
    }

    eof_found = 1;

    ret_token(' ');
    Return(END_OF_FILE);
}
