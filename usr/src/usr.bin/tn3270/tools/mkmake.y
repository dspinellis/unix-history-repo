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

/*
 * The deal with these is that they exist on various lists.
 *
 * First off, they are on a temporary list during the time they
 * are in the active focus of the parser.
 *
 * Secondly, they live on one of three queues:
 *	1.  Variables
 *	2.  Targets
 *	3.  Actions
 * (and, we restrict any given one to live on one and only one such list)
 *
 * Also, they may live on the list of values for someone else's variable,
 * or as someone's dependancy.
 */

typedef struct same {
    string_t
	*string;			/* My name */
    struct same
	*nexttoken,			/* Next pointer */
	*lasttoken,			/* Back pointer */
	*depend_list,			/* If target, dependancies */
	*action_list,			/* If target, actions */
	*value_list;			/* If variable, value list */
} same_t;

%}

%union {
    string_t *string;
    same_t *same;
    int	intval;
    }

%token <string> TOKEN QUOTED_STRING
%token <intval> MACRO_CHAR BREAK_CHAR WHITE_SPACE NL END_OF_FILE
%token <intval> ':' '=' '$' '{' '}'
%type <same> target assignment actions action tokens token
%type <intval> special_chars white_space macro_char
%%

makefile : lines END_OF_FILE;

lines : line
    | lines line
    ;

line : maybe_white_space NL
    | assignment
    | target_action
    ;

assignment :
    token maybe_white_space '=' maybe_white_space tokens maybe_white_space NL
    {
	assign($1, $5);
    }
    | token maybe_white_space '=' maybe_white_space NL
    {
	assign($1, same_copy(&null));
    }
    ;

target_action: target actions
    {
	add_targets_actions($1, $2);
    }
    | target
    {
	add_targets_actions($1, same_copy(&null));
    }
    ;

target :
    tokens maybe_white_space ':' maybe_white_space tokens maybe_white_space NL
    {
	$$ = add_depends($1, $5);
    }
    | tokens maybe_white_space ':' maybe_white_space NL
    {
	$$ = add_depends($1, same_copy(&null));
    }
    ;

actions: action
    | actions action
    {
	$$ = same_cat($1, $2);
    }
    ;

action:	white_space tokens NL
    {
	$$ = $2;
    }
    ;

tokens : token
    | tokens token
    {
	$$ = same_cat($1, $2);
    }
    ;

token: TOKEN
    {
	$$ = same_item($1);
    }
    | QUOTED_STRING
    {
	$$ = same_item($1);
    }
    | '$' macro_char
    {
	char buffer[3];

	buffer[0] = '$';
	buffer[1] = $2;
	buffer[2] = 0;

	$$ = same_item(string_lookup(buffer));
    }
    | special_chars
    {
	char buffer[2];

	buffer[0] = $1;
	buffer[1] = 0;

	$$ = same_item(string_lookup(buffer));
    }
    | '$' '{' token '}'
    {
	$$ = same_copy(value_of($3));
    }
    ;

macro_char: MACRO_CHAR
    | '$'
    ;

special_chars : BREAK_CHAR
    | MACRO_CHAR
    | white_space
    ;

maybe_white_space:
    | white_space;

white_space : WHITE_SPACE
    | white_space WHITE_SPACE
    ;

%%
#include <stdio.h>

static int last_char, last_saved = 0;
static int column = 0, lineno = 1;


static string_t
    *strings = 0;
static same_t
    variables = { 0 },
    targets = { 0 },
    actions = { 0 };

static same_t
    null = { 0 },
    blank = { 0 },
    newline = { 0 };

extern char *malloc();

yyerror(s)
char *s;
{
    fprintf(stderr, "line %d, character %d: %s\n", lineno, column, s);
    do_dump();
}

int
string_hashof(string, length)
char *string;
int length;
{
    register int i = 0;

    while (length--) {
	i = (i<<3) + *string ^ ((i>>28)&0x7);
    }
    return i;
}

int
string_same(s1, s2)
string_t
    *s1, *s2;
{
    if ((s1->hashval == s2->hashval) && (s1->length == s2->length)
		&& (memcmp(s1->string, s2->string, s1->length) == 0)) {
	return 1;
    } else {
	return 0;
    }
}

string_t *
string_lookup(string)
char *string;
{
    string_t ours;
    string_t *ptr;

    ours.length = strlen(string);
    ours.hashval = string_hashof(string, ours.length);
    ours.string = string;

    for (ptr = strings; ptr; ptr = ptr->next) {
	if (string_same(&ours, ptr)) {
	    return ptr;
	}
    }
    if ((ptr = (string_t *)malloc(sizeof *ptr)) == 0) {
	fprintf(stderr, "No space to add string *%s*!\n", string);
	exit(1);
    }
    ptr->hashval = ours.hashval;
    ptr->length = ours.length;
    if ((ptr->string = malloc(ours.length+1)) == 0) {
	fprintf(stderr, "No space to add literal *%s*!\n", string);
	exit(1);
    }
    memcpy(ptr->string, string, ours.length+1);
    ptr->next = strings;
    strings = ptr;
    return ptr;
}

same_t *
same_search(list, token)
same_t
    *list,
    *token;
{
    same_t *ptr;

    for (ptr = list; ptr; ptr = ptr->nexttoken) {
	string_t *string;

	string = ptr->string;
	if (string_same(string, token->string)) {
	    return ptr;
	}
    }
    return 0;
}

same_t *
same_cat(list, tokens)
same_t
    *list,
    *tokens;
{
    same_t *last;

    last = tokens->lasttoken;
    tokens->lasttoken = list->lasttoken;
    list->lasttoken = last;
    } else {
	list->lasttoken = tokens;
    }
    if (list->nexttoken == 0) {
	list->nexttoken = tokens;
    } else {
	tokens->lasttoken->nexttoken = tokens;
    }
    return list;
}

same_t *
same_item(string)
string_t *string;
{
    same_t *ptr;

    if ((ptr = (same_t *)malloc(sizeof *ptr)) == 0) {
	fprintf(stderr, "No more space for tokens!\n");
	exit(1);
    }
    memset((char *)ptr, 0, sizeof *ptr);
    ptr->string = string;
    return ptr;
}

same_t *
same_copy(same)
same_t *same;
{
    same_t *head;

    head = same_item(same->string);
    for (same = same->nexttoken; same; same = same->nexttoken) {
	same_t *ptr;

	ptr = same_item(same->string);
	same_cat(head, ptr);
    }
    return head;
}

void
same_free(token)
same_t *token;
{
    same_t *ptr;

    while (token) {
	ptr = token->nexttoken;
	(void) free((char *)token);
	token = ptr;
    }
}

void
same_unlink(token, base)
same_t
    *token,
    *base;
{
    if (token->lasttoken) {
	token->lasttoken->nexttoken = token->nexttoken;
    }
    if (token->nexttoken) {
	token->nexttoken->lasttoken = token->lasttoken;
    }
    if (base->nexttoken == token) {
	base->nexttoken = token->nexttoken;
    }
    if (base->lasttoken == token) {
	base->lasttoken = token->lasttoken;
    }
    (void) free((char *) token);
}

same_t *
add_target(target)
same_t
    *target;
{
    same_t *ptr;

    if ((ptr = same_search(&targets, target)) == 0) {
	same_cat(&targets, target);
    } else {
	same_cat(ptr->action_list, target->action_list);
	same_cat(ptr->depend_list, target->depend_list);
    }
    return 0;
}

same_t *
add_targets_actions(target, actions)
same_t
    *target,
    *actions;
{
    same_t *next;

    while (target) {
	next = target->lasttoken;
	target->lasttoken = target->lasttoken = 0;
	target->action_list = actions;
	add_target(target);
	target= next;
    }
    return 0;
}

same_t *
add_depends(target, depends)
same_t
    *target,
    *depends;
{
    same_t *original = target;

    same_cat(depends, blank);			/* Separator */

    while (target) {
	target->depend_list = depends;
	target = target->nexttoken;
    }
    return original;
}


/*
 * We know that variable is a singleton
 */

void
assign(variable, value)
same_t
    *variable,
    *value;
{
    same_t *ptr;

    if ((ptr = same_search(&variables, variable)) != 0) {
	same_free(ptr->value_list);
	same_unlink(ptr, &variables);
    }
    variable->value_list = value;
    same_cat(&variables, variable);
}

same_t *
value_of(variable)
same_t *variable;
{
    same_t *ptr = same_search(&variables, variable);

    if (ptr == 0) {
	return &null;
    } else {
	return ptr->value_list;
    }
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


yylex()
{
#define	ret_token(c)	if (bufptr != buffer) { \
			    save(c); \
			    *bufptr = 0; \
			    bufptr = buffer; \
			    yylval.string = string_lookup(buffer); \
			    return TOKEN; \
			}
#define	save(c)	{ last_char = c; last_saved = 1; }
#if	defined(YYDEBUG)
#define	Return(c)	if (yydebug) { \
			    printf("[%d]", c); \
			    fflush(stdout); \
			} \
			yyval.intval = c; \
			return c;
#else	/* defined(YYDEBUG) */
#define	Return(y,c)	{ yylval.intval = c; return y; }
#endif	/* defined(YYDEBUG) */


    static char buffer[500], *bufptr = buffer;
    static int eof_found = 0;
    int c;

    if (eof_found != 0) {
	eof_found++;
	if (eof_found > 2) {
	    fprintf(stderr, "End of file ignored.\n");
	    exit(1);
	}
	Return(END_OF_FILE,0);
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
	case '@':
	case '<':
	case '?':
	    ret_token(c);
	    Return(MACRO_CHAR, c);
	case '-':
	case '(':
	case ')':
	case ';':
	    ret_token(c);
	    Return(BREAK_CHAR, c);
	case '\t':
	case ' ':
	    ret_token(c);
	    Return(WHITE_SPACE, c);
	case ':':
	case '=':
	case '$':
	case '{':
	case '}':
	    ret_token(c);
	    Return(c,c);
	case '\'':
	case '"':
	    {
		int newc;

		ret_token(c);
		*bufptr++ = c;
		while (((newc = Getchar()) != EOF) && (newc != c)) {
		    *bufptr++ = newc;
		}
		*bufptr++ = c;
		*bufptr = 0;
		bufptr = buffer;
		yylval.string = string_lookup(buffer);
		return QUOTED_STRING;
	    }
	case '\n':
	    if (bufptr != buffer) {
		if (bufptr[-1] == '\\') {
		    bufptr--;
		    if ((c = Getchar()) != '\t') {
			yyerror("continuation line doesn't begin with a tab");
			save(c);
		    }
		    ret_token(c);
		    Return(WHITE_SPACE, c);
		}
	    }
	    ret_token(c);
	    Return(NL, 0);
	default:
	    *bufptr++ = c;
	    break;
	}
    }

    eof_found = 1;

    ret_token(' ');
    Return(END_OF_FILE, 0);
}

main()
{
#define	YYDEBUG
    extern int yydebug;


    null.string = string_lookup("");
    newline.string = string_lookup("\n");
    blank.string = string_lookup(" ");

    return yyparse();
}

#if	defined(YYDEBUG)
do_dump()
{
    string_t *string;
    same_t *same, *same2;

    if (yydebug > 1) {
	printf("strings...\n");
	for (string = strings; string; string = string->next) {
	    printf("\t%s\n", string->string);
	}
    }

    printf("variables...\n");
    for (same = variables.nexttoken; same; same = same->nexttoken) {
	printf("\t%s =\t", same->string->string);
	for (same2 = same->value_list; same2; same2 = same2->nexttoken) {
	    printf(same2->string->string);
	}
	printf("\n");
    }

    printf("targets...\n");
    for (same = targets.nexttoken; same; same = same->nexttoken) {
	printf("\t%s :\t", same->string->string);
	for (same2 = same->depend_list; same2; same2 = same2->nexttoken) {
	    printf(same2->string->string);
	}
	if (same->action_list) {
	    printf("\n\t\t");
	    for (same2 = same->action_list; same2; same2 = same2->nexttoken) {
		printf(same2->string->string);
		if (same2->string->string[0] == '\n') {
		    printf("\t\t");
		}
	    }
	    printf("\n");
	}
    }
}
#endif	/* YYDEBUG */
