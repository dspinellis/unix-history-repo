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
	*value_list,			/* If variable, value list */
	*shell_item;			/* If a shell variable, current value */
} same_t;

%}

%union {
    string_t *string;
    same_t *same;
    int	intval;
    }

%start makefile
%token <string> TOKEN QUOTED_STRING
%token <intval>	FOR IN DO DONE
%token <intval> MACRO_CHAR NL WHITE_SPACE
%token <intval> ':' '=' '$' '{' '}' ';' '-' '@' '(' ')' ' ' '\t'
%type <same> target target1 assignment assign1 actions action
%type <same> command_list command list
%type <same> for_statement maybe_at_minus tokens token
%type <same> maybe_white_space
%type <intval> white_space macro_char
%%

makefile : lines;

lines : line
    | lines line
    ;

line : NL
    | assignment
    | target_action
    ;

assignment :
    assign1 tokens NL
    {
	assign($1, $2);
    }
    | assign1 NL
    {
	assign($1, same_copy(null));
    }
    ;

assign1: tokens maybe_white_space '=' maybe_white_space
    ;

target_action: target actions
    {
	add_targets_actions($1, $2);
    }
    | target
    {
	add_targets_actions($1, same_copy(null));
    }
    ;

target :
    target1 tokens NL
    {
	$$ = add_depends($1, $2);
    }
    | target1 NL
    {
	$$ = add_depends($1, same_copy(null));
    }
    ;

target1: tokens maybe_white_space ':' maybe_white_space
    ;

actions: action
    | actions action
    {
	$$ = same_cat($1, $2);
    }
    ;

action:	white_space command_list NL
    {
	$$ = $2;
    }
    | white_space for_statement do command_list semi_colon done NL
    {
	$$ = $2;
    }
    ;

for_statement: maybe_at_minus FOR white_space token
		in tokens semi_colon
    {
	$$ = for_statement($1, $4, expand_variables($6, 0));
    }
    ;

in:	white_space IN white_space
do:	white_space DO white_space
    ;

done:	white_space DONE
    ;

semi_colon:	';'
    ;

command_list: list
    | '(' list maybe_white_space ')'
    {
	$$ = $2;
    }
    ;

list: command
    {
	$$ = $1;
    }
    | list semi_colon white_space command
    {
	$$ = same_cat($1, same_cat(same_char('\n'), $4));
    }
    ;

command: tokens
    ;

maybe_at_minus: /* empty */
    {
	$$ = same_copy(null);
    }
    | '@'
    {
	char buffer[2];

	buffer[0] = $1;
	buffer[1] = 0;
	$$ = same_item(string_lookup(buffer));
    }
    | '-'
    {
	char buffer[2];

	buffer[0] = $1;
	buffer[1] = 0;
	$$ = same_item(string_lookup(buffer));
    }
    ;

tokens : token
    | tokens maybe_white_space token
    {
	$$ = same_cat($1, same_cat($2, $3));
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
    | '$' '$' token
    {
	$$ = shell_variable($3);
    }
    | MACRO_CHAR
    {
	$$ = same_char($1);
    }
    | '$' '{' token '}'
    {
	$$ = variable($3);
    }
    | '-'
    {
	$$ = same_char('-');
    }
    | '@'
    {
	$$ = same_char('@');
    }
    ;

macro_char: MACRO_CHAR
    | '@'
    ;

maybe_white_space:
    {
	$$ = same_copy(null);
    }
    | white_space
    {
	$$ = same_char($1);
    }
    ;

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
    *shell_variables = 0,
    *shell_special = 0,
    *variables = 0,
    *targets = 0,
    *actions = 0;

static same_t
    *null,
    *blank,
    *newline;

extern char *malloc();

static unsigned int
	clock = -1;

struct {
    same_t *first;
    int next;
} visit_stack[20];		/* 20 maximum */

#define	visit(what,via) \
	(visit_stack[++clock].next = 0, visit_stack[clock].first = via = what)
#define	visited(via)	(visitcheck(via) || ((via) == 0) \
	|| (visit_stack[clock].next && (via == visit_stack[clock].first)))
#define	visit_next(via)	(visit_stack[clock].next = 1, (via) = (via)->nexttoken)
#define	visit_end()	(clock--)

yyerror(s)
char *s;
{
    fprintf(stderr, "line %d, character %d: %s\n", lineno, column, s);
    do_dump();
}

int
visitcheck(same)
same_t *same;
{
    if (same->string == 0) {
	yyerror("BUG - freed 'same' in use...");
	exit(1);
    }
    return 0;
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

    ptr = list;
    for (visit(list, ptr); !visited(ptr); visit_next(ptr)) {
	string_t *string;

	string = ptr->string;
	if (string_same(string, token->string)) {
	    visit_end();
	    return ptr;
	}
    }
    visit_end();
    return 0;
}

same_t *
same_cat(list, tokens)
same_t
    *list,
    *tokens;
{
    same_t *last;

    if (tokens == 0) {
	return list;
    }
    if (list) {
	last = tokens->lasttoken;
	tokens->lasttoken = list->lasttoken;
	list->lasttoken = last;
	tokens->lasttoken->nexttoken = tokens;
	last->nexttoken = list;
	return list;
    } else {
	return tokens;
    }
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
    ptr->nexttoken = ptr->lasttoken = ptr;
    ptr->string = string;
    return ptr;
}

same_t *
same_copy(same)
same_t *same;
{
    same_t *head, *copy;

    head = 0;
    for (visit(same, copy); !visited(copy); visit_next(copy)) {
	same_t *ptr;

	ptr = same_item(copy->string);
	head = same_cat(head, ptr);
    }
    visit_end();
    return head;
}

void
same_free(list)
same_t *list;
{
    same_t *token, *ptr;

    token = list;
    do {
	ptr = token->nexttoken;
	token->string = 0;
	(void) free((char *)token);
	token = ptr;
    } while (token != list);
}

same_t *
same_unlink(token)
same_t
    *token;
{
    same_t *tmp;

    if (token == 0) {
	return 0;
    }
    if ((tmp = token->nexttoken) == token) {
	tmp = 0;
    }
    token->lasttoken->nexttoken = token->nexttoken;
    token->nexttoken->lasttoken = token->lasttoken;
    token->nexttoken = token->lasttoken = token;
    return tmp;
}

same_t *
same_char(ch)
char ch;
{
    char buffer[2];

    buffer[0] = ch;
    buffer[1] = 0;

    return same_item(string_lookup(buffer));
}

same_t *
add_target(target)
same_t
    *target;
{
    same_t *ptr;

    if ((ptr = same_search(targets, target)) == 0) {
	targets = same_cat(targets, target);
	return target;
    } else {
	ptr->action_list = same_cat(ptr->action_list, target->action_list);
	ptr->depend_list = same_cat(ptr->depend_list, target->depend_list);
	return ptr;
    }
}

same_t *
add_targets_actions(target, actions)
same_t
    *target,
    *actions;
{
    same_t *ptr;

    if (target == 0) {
	return 0;
    }
    do {
	target->action_list = same_cat(target->action_list,
						same_copy(actions));
	ptr = same_unlink(target);
	add_target(target);
	target = ptr;
    } while (target);

    same_free(actions);
    return 0;
}

same_t *
add_depends(target, depends)
same_t
    *target,
    *depends;
{
    same_t *original = target;

    depends = same_cat(depends, same_copy(blank));	/* Separator */

    for (visit(original, target); !visited(target); visit_next(target)) {
	target->depend_list = same_cat(target->depend_list, same_copy(depends));
    }
    visit_end();
    same_free(depends);

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

    if ((ptr = same_search(variables, variable)) != 0) {
	same_free(ptr->value_list);
	(void) same_unlink(ptr);
	same_free(ptr);
    }
    variable->value_list = value;
    variables = same_cat(variables, variable);
}

same_t *
value_of(variable)
same_t *variable;
{
    same_t *ptr = same_search(variables, variable);

    if (ptr == 0) {
	return same_copy(null);
    } else {
	return same_copy(ptr->value_list);
    }
}


same_t *
expand_variables(token, free)
same_t *token;
int	free;
{
    same_t *head = 0;

    if (!free) {
	token = same_copy(token);		/* Get our private copy */
    }

    while (token) {
	char *string = token->string->string;
	same_t *tmp = same_unlink(token);

	if ((string[0] == '$') && (string[1] == '{')) {/* '}' Expand time */
	    int len = strlen(string);

	    string[len-1] = 0;
	    head = same_cat(head, expand_variables(
			value_of(same_item(string_lookup(string+2))), 1));
	    string[len-1] = '}';
	} else {
	    head = same_cat(head, token);
	}
	token = tmp;
    }
    return head;
}

same_t *
variable(var_name)
same_t *var_name;
{
    int length = strlen(var_name->string->string);
    same_t *resolved;
    char *newname;

    if ((newname = malloc(length+1+3)) == 0) {
	fprintf("Out of space for a variable name.\n");
	exit(1);
    }
    newname[0] = '$';
    newname[1] = '{';
    strcpy(newname+2, var_name->string->string);
    strcat(newname, "}");
    resolved = same_item(string_lookup(newname));
    free(newname);

    return resolved;
}


same_t *
shell_variable(name)
same_t *name;
{
    same_t *shell;

    if ((shell = same_search(shell_variables, name)) == 0) {
	char buffer[100];
	sprintf(buffer, "Unknown shell variable %s.", name->string->string);
	yyerror(buffer);
	exit(1);
    }
    return same_copy(shell->shell_item);
}

same_t *
for_statement(special, variable, list)
same_t
    *special,
    *variable,
    *list;
{
    shell_special = special;
    variable->value_list = list;
    shell_variables = variable;
    return same_copy(null);
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
token_type(string)
char *string;
{
    switch (string[0]) {
    case 'f':
	if (strcmp(string, "for") == 0) {
	    return FOR;
	}
	break;
    case 'd':
	if (string[1] == 'o') {
	    if (strcmp(string, "do") == 0) {
		return DO;
	    } else if (strcmp(string, "done") == 0) {
		return DONE;
	    }
	}
	break;
    case 'i':
	if (strcmp(string, "in") == 0) {
	    return IN;
	}
	break;
    default:
	break;
    }
    return TOKEN;
}


yylex()
{
#define	ret_token(c)	if (bufptr != buffer) { \
			    save(c); \
			    *bufptr = 0; \
			    bufptr = buffer; \
			    yylval.string = string_lookup(buffer); \
			    return token_type(buffer); \
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
	Return(EOF,0);
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
	case '<':
	case '?':
	    ret_token(c);
	    Return(MACRO_CHAR, c);
	case '\t':
	case ' ':
	    ret_token(c);
	    Return(WHITE_SPACE, c);
	case '-':
	case '@':
	case ':':
	case ';':
	case '=':
	case '$':
	case '{':
	case '}':
	case '(':
	case ')':
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
    Return(EOF, 0);
}

main()
{
#define	YYDEBUG
    extern int yydebug;

    null = same_item(string_lookup(""));
    newline = same_item(string_lookup("\n"));
    blank = same_item(string_lookup(" "));

    yyparse();

    do_dump();
}

#if	defined(YYDEBUG)
dump_same(same)
same_t *same;
{
    same_t *same2;

    for (visit(same, same2); !visited(same2); visit_next(same2)) {
	printf(same2->string->string);
    }
    visit_end();
}
#endif	/* YYDEBUG */

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

    printf("# variables...\n");
    for (visit(variables, same); !visited(same); visit_next(same)) {
	printf("%s =\t", same->string->string);
	for (visit(same->value_list, same2); !visited(same2);
						visit_next(same2)) {
	    printf(same2->string->string);
	}
	visit_end();
	printf("\n");
    }
    visit_end();

    printf("#targets...\n");
    for (visit(targets, same); !visited(same); visit_next(same)) {
	printf("%s :\t", same->string->string);
	for (visit(same->depend_list, same2); !visited(same2);
						visit_next(same2)) {
	    printf(same2->string->string);
	}
	visit_end();
	printf("\n\t");
	for (visit(same->action_list, same2); !visited(same2);
					    visit_next(same2)) {
	    printf(same2->string->string);
	    if (same2->string->string[0] == '\n') {
		printf("\t");
	    }
	}
	visit_end();
	printf("\n");
    }
    visit_end();
}
