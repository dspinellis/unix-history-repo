/* $Header: gram.y,v 10.4 86/11/30 17:04:55 jg Rel $ */
/*
 *			COPYRIGHT 1985, 1986
 *		   DIGITAL EQUIPMENT CORPORATION
 *		       MAYNARD, MASSACHUSETTS
 *			ALL RIGHTS RESERVED.
 *
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
 * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
 * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITIBILITY OF THIS SOFTWARE FOR
 * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 *
 * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT RIGHTS,
 * APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN ADDITION TO THAT
 * SET FORTH ABOVE.
 *
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting documentation,
 * and that the name of Digital Equipment Corporation not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission.
 *
 */

/*
 * MODIFICATION HISTORY
 *
 * 000 -- M. Gancarz, DEC Ultrix Engineering Group
 */

%{
#ifndef lint
static char *sccsid = "@(#)gram.y	3.8	1/24/86";
#endif

#include "uwm.h"

/*
 * Values returned by complex expression parser.
 */
#define C_STRING	1	/* IsString. */
#define C_MENU		2	/* IsMenu. */
#define C_MAP		3	/* IsMap. */
#define C_MENUMAP	4	/* IsMenuMap. */

static int ki;				/* Keyword index. */
static short bkmask;			/* Button/key mask. */
static int cmask;			/* Context mask. */
static char msg[BUFSIZ];		/* Error message buffer. */
static char *menu_name;			/* Menu name. */
static MenuInfo *menu_info;		/* Menu info. */
static MenuLine *ml_ptr;		/* Temporary menu line pointer. */
static char *hcolors[4];		/* Color values used in menu hdrs. */
static char *mcolors[2];		/* Color values used in menus. */
MenuLink *menu_link;			/* Temporary menu link pointer. */

char *calloc();

%}

%union {
    char *sval;
    int ival;
    short shval;
    struct _menuline *mlval;
    struct _menuinfo *mival;
    char **cval;
}

%token NL
%token <sval> STRING
%token <ival> COMMENT
%type <ival> keyword
%type <ival> compexpr
%type <shval> keyexpr
%type <shval> kmask
%type <ival> contexpr
%type <ival> contmask
%type <shval> buttmodexpr
%type <shval> buttmodifier
%type <shval> buttexpr
%type <sval> menuname
%type <sval> strings
%type <sval> color
%type <cval> color2
%type <cval> color4
%type <mlval> menuexpr
%type <mlval> menulist
%type <mlval> menuline
%type <mlval> menuaction

%%	/* beginning of rules section */

input:	|	input command
	|	input error command { yyerrok; }
	;

command:	boolvar term
	|	expr term
	|	COMMENT	{ Lineno++; }
	|	term
	;

term:		NL	{ Lineno++; }
	|	';'
	;

expr:		keyword '=' compexpr
			{
			    switch (KeywordTable[$1].type) {
			        case IsString:
			            if ($3 == C_STRING) {
			                strcpy(KeywordTable[$1].sptr,
			                        yylval.sval);
			            } else {
			                yyerror("illegal construct");
			            }
			            free(yylval.sval);
			            break;
			        case IsNumeric:
			            if ($3 == C_STRING) {
			                *(KeywordTable[$1].nptr) =
			                                   y_atoi(yylval.sval);
			            } else yyerror("illegal construct");
			            free(yylval.sval);
			            break;
			        case IsBoolTrue:
			        case IsBoolFalse:
			            yyerror("illegal value assignment");
			            break;
			        case IsQuitFunction:
			        case IsFunction:
			            if ($3 == C_MAP) {
			                bind($1, bkmask, cmask, NULL);
			            } else yyerror("illegal construct");
			            break;
			        case IsDownFunction:
			            if (bkmask & ButtonUp) {
			                sprintf(msg,
			                        "cannot bind %s to button up",
			                        KeywordTable[$1].name);
			                yyerror(msg);
			            }
			            if ($3 == C_MAP) {
			                bind($1, bkmask, cmask, NULL);
			            } else yyerror("illegal construct");
			            break;
			        case IsMenuMap:
			            if (bkmask & ButtonUp) {
			                sprintf(msg,
			                        "cannot bind %s to button up",
			                        KeywordTable[$1].name);
			                yyerror(msg);
			            }
			            if ($3 == C_MENUMAP) {
			                bind($1, bkmask, cmask, menu_name);
			            } else yyerror("illegal construct");
			            break;
			        case IsMenu:
			            if ($3 == C_MENU) {
			                menu_info = stashmenuinfo(menu_name, ml_ptr, hcolors);
			                menu_link = stashmenulink(menu_info);
			                Menus = appendmenulink(Menus, menu_link);
			            } else yyerror("illegal menu construct");
			            break;
			        default:
			            yyerror("internal binding error");
			            break;
			    }
			}
	;

compexpr:	keyexpr ':' contexpr ':' buttexpr
			{
			    $$ = C_MAP;
			    bkmask = $1 | $5;
			    cmask = $3;
			}
	|	keyexpr ':' contexpr ':' buttexpr ':' menuname
			{
			    $$ = C_MENUMAP;
			    bkmask = $1 | $5;
			    cmask = $3;
			    menu_name = $7;
			}
	|	STRING color4 menuexpr
			{
			    $$ = C_MENU;
			    menu_name = $1;
			    ml_ptr = $3;
			}
	|	STRING
			{ $$ = C_STRING; }
	;

boolvar:	STRING
			{
			    ki = keywordlookup(yylval.sval);
			    switch (KeywordTable[ki].type) {
			    case IsBoolTrue:
			        *(KeywordTable[ki].bptr) = TRUE;
			        break;
			    case IsBoolFalse:
			        *(KeywordTable[ki].bptr) = FALSE;
			        break;
			    case IsParser:
			        (*KeywordTable[ki].fptr)();
			        break;
			    default:
			        yyerror("keyword error");
			    }
			}
	;

keyword:	STRING	{
			    $$ = keywordlookup(yylval.sval);
			}
	;

keyexpr:	/* empty */
			{ $$ = 0; }
	|	kmask
			{ $$ = $1; }
	|	kmask '|' kmask
			{ $$ = $1 | $3; }
	;

contexpr:	/* empty */
			{ $$ = ROOT | WINDOW | ICON; }
	|	contmask
			{ $$ = $1; }
	|	contmask '|' contmask
			{ $$ = $1 | $3; }
	|	contmask '|' contmask '|' contmask
			{ $$ = $1 | $3 | $5; }
	;

buttexpr:	buttmodexpr
			{ $$ = CheckButtonState($1); }
	;

kmask:		STRING { $$ = keyexprlookup(yylval.sval); }

contmask:	STRING { $$ = contexprlookup(yylval.sval); }

buttmodexpr: 	buttmodifier
			{ $$ = $1; }
	|	buttmodexpr buttmodifier
			{ $$ = $1 | $2; }
	;

buttmodifier:	STRING
			{ $$ = buttexprlookup(yylval.sval); }
	;

menuname:	STRING
			{ $$ = $1; }
	;

menuexpr:	'{' menulist '}'
			{ $$ = $2; }
	;

menulist:	menuline
			{ $$ = $1; }
	|	menulist menuline
			{ $$ = appendmenuline($1, $2); }
	|	menulist COMMENT
			{
			    Lineno++;
			    $$ = $1;
			}
	|	COMMENT
			{
			    Lineno++;
			    $$ = NULL;
			}
	|	term
			{ $$ = NULL; }
	|	menulist term
			{ $$ = $1; }
	|	error term
			{
			  $$ = NULL;
			  yyerrok;
			}
	;

menuline:	strings ':' color2 menuaction term
			{
			    $4->name = $1;
			    $4->foreground = mcolors[0];
			    $4->background = mcolors[1];
			    $$ = $4;
			}
	;

menuaction:	STRING
			{
			    ki = keywordlookup(yylval.sval);
			    if ((ki != -1) &&
			        (KeywordTable[ki].type != IsFunction) &&
			        (KeywordTable[ki].type != IsQuitFunction) &&
			        (KeywordTable[ki].type != IsDownFunction)) {
			        sprintf(msg,
			                "menu action \"%s\" not a function",
				        KeywordTable[ki].name);
			        yyerror(msg);
			    }
			    ml_ptr = AllocMenuLine();
			    if (KeywordTable[ki].type == IsQuitFunction)
			        ml_ptr->type = IsImmFunction;
			    else ml_ptr->type = IsUwmFunction;
			    ml_ptr->func = KeywordTable[ki].fptr;
			    $$ = ml_ptr;
			}
	|	STRING ':' menuname
			{
			    ki = keywordlookup($1);
			    if (ki != -1 &&
			        KeywordTable[ki].type != IsMenuMap) {
			        sprintf(msg,
			               "menu action \"%s\" not a menu function",
				        KeywordTable[ki].name);
			        yyerror(msg);
			    }
			    ml_ptr = AllocMenuLine();
			    ml_ptr->type = IsMenuFunction;
			    ml_ptr->text = $3;
			    $$ = ml_ptr;
			}
	|	'!' strings
			{
			    $$ = StashMenuLine(IsShellCommand, $2);
			}
	|	'^' strings
			{
			    $$ = StashMenuLine(IsTextNL, $2);
			}
	|	'|' strings
			{
			    $$ = StashMenuLine(IsText, $2);
			}
	;

strings:	STRING	{ $$ = yylval.sval; }
	|	strings STRING
			{ $$ = strconcat($1, $2); }
	;

color4:		'(' color ':' color ':' color ':' color ')'
			{
			    hcolors[0] = $2;
			    hcolors[1] = $4;
			    hcolors[2] = $6;
			    hcolors[3] = $8;
			    $$ = hcolors;
			}
	|	/* empty */
			{
			    hcolors[0] = NULL;
			    hcolors[1] = NULL;
			    hcolors[2] = NULL;
			    hcolors[3] = NULL;
			    $$ = hcolors;
			}
	;

color2:		'(' color ':' color ')' ':'
			{
			    mcolors[0] = $2;
			    mcolors[1] = $4;
			    $$ = mcolors;
			}
	|	/* empty */
			{
			    mcolors[0] = NULL;
			    mcolors[1] = NULL;
			    $$ = mcolors;
			}
	;

color:		STRING	{ $$ = yylval.sval; }
	|	/* empty */	{ $$ = NULL; }
	;
%%

/*
 * Look up a string in the keyword table and return its index, else
 * return -1.
 */
keywordlookup(string)
char *string;
{
    int i;

    for (i = 0; KeywordTable[i].name; i++) {
        if (!strcmp(KeywordTable[i].name, string)) {
            free(string);
            return(i);
        }
    }
    sprintf(msg,"keyword error: \"%s\"", string);
    yyerror(msg);
    free(string);
    return(-1);
}

/*
 * Look up a string in the key expression table and return its mask, else
 * return -1.
 */
short keyexprlookup(string)
char *string;
{
    int i;

    for (i = 0; KeyExprTbl[i].name; i++) {
        if (!strcmp(KeyExprTbl[i].name, string)) {
            free(string);
            return(KeyExprTbl[i].mask);
        }
    }
    sprintf(msg,"key expression error: \"%s\"", string);
    yyerror(msg);
    free(string);
    return(-1);
}

/*
 * Look up a string in the context expression table and return its mask, else
 * return -1.
 */
contexprlookup(string)
char *string;
{
    int i;

    for (i = 0; ContExprTbl[i].name; i++) {
        if (!strcmp(ContExprTbl[i].name, string)) {
            free(string);
            return(ContExprTbl[i].mask);
        }
    }
    sprintf(msg,"context expression error: \"%s\"", string);
    yyerror(msg);
    free(string);
    return(-1);
}
/*
 * Look up a string in the button expression table and return its mask, else
 * return -1.
 */
buttexprlookup(string)
char *string;
{
    int i;

    for (i = 0; ButtModTbl[i].name; i++) {
        if (!strcmp(ButtModTbl[i].name, string)) {
            free(string);
            return(ButtModTbl[i].mask);
        }
    }
    sprintf(msg,"button modifier error: \"%s\"", string);
    yyerror(msg);
    free(string);
    return(-1);
}

/*
 * Scan a string and return an integer.  Report an error if any
 * non-numeric characters are found.
 */
y_atoi(s)
char *s;
{
    int n = 0;

    while (*s) {
        if (*s >= '0' && *s <= '9')
            n = 10 * n + *s - '0';
        else {
            yyerror("non-numeric argument");
            return(-1);
        }
        s++;
    }
    return(n);
}

/*
 * Append s2 to s1, extending s1 as necessary.
 */
char *
strconcat(s1, s2)
char *s1, *s2;
{
    char *malloc();
    char *p;

    p = malloc(strlen(s1) + strlen(s2) + 2);
    sprintf(p, "%s %s", s1, s2);
    free(s1);
    free(s2);
    s1 = p;
    return(s1);
}

/*
 * Check a button expression for errors.
 */
short
CheckButtonState(expr)
short expr;
{
    /*
     * Check for one (and only one) button.
     */
    switch (expr & (LeftMask | MiddleMask | RightMask)) {
    case 0:
        yyerror("no button specified");
        break;
    case LeftMask:
        break;
    case MiddleMask:
        break;
    case RightMask:
        break;
    default:
        yyerror("more than one button specified");
    }

    /*
     * Check for one (and only one) up/down/motion modifier.
     */
    switch (expr & (ButtonUp | ButtonDown | DeltaMotion)) {
    case 0:
        yyerror("no button action specified");
        break;
    case ButtonUp:
        break;
    case ButtonDown:
        break;
    case DeltaMotion:
        break;
    default:
        yyerror("only one of up/down/motion may be specified");
    }
    return(expr);
}

/*
 * Bind button/key/context to a function.
 */
bind(index, mask, context, name)
int index;		/* Index into keyword table. */
short mask;		/* Button/key/modifier mask. */
int context;		/* ROOT, WINDOW, or ICON. */
char *name;		/* Menu, if needed. */
{
    if (context & ROOT)
        setbinding(ROOT, index, mask, name);
    if (context & ICON)
        setbinding(ICON, index, mask, name);
    if (context & WINDOW)
        setbinding(WINDOW, index, mask, name);
}

/*
 * Allocate a Binding type and return a pointer.
 */
Binding *
AllocBinding()
{
    Binding *ptr;

    if (!(ptr = (Binding *)calloc(1, sizeof(Binding)))) {
        fprintf(stderr, "Can't allocate binding--out of space\n");
        exit(1);
    }
    return(ptr);
}

/*
 * Stash the data in a Binding.
 */
setbinding(cont, i, m, mname)
int cont;		/* Context: ROOT, WINDOW, or ICON. */
int i;			/* Keyword table index. */
short m;		/* Key/button/modifier mask. */
char *mname;		/* Pointer to menu name, if needed. */
{
    Binding *ptr;

    ptr = AllocBinding();
    ptr->context = cont;
    ptr->mask = m;
    ptr->func = KeywordTable[i].fptr;
    ptr->menuname = mname;

    switch (m & (LeftMask | MiddleMask | RightMask)) {
    case LeftMask:
        ptr->button = LeftButton;
        break;
    case MiddleMask:
        ptr->button = MiddleButton;
        break;
    case RightMask:
        ptr->button = RightButton;
        break;
    }
    appendbinding(ptr);
}

/*
 * Append a Binding to the Bindings list.
 */
appendbinding(binding)
Binding *binding;
{
    Binding *ptr;

    if (Blist == NULL)
        Blist = binding;
    else {
        for(ptr = Blist; ptr->next; ptr = ptr->next) /* NULL */;
        ptr->next = binding;
        ptr = ptr->next;
        ptr->next = NULL;
    }
}

/*
 * Allocate a menu line and return a pointer.
 */
MenuLine *
AllocMenuLine()
{
    MenuLine *ptr;

    if (!(ptr = (MenuLine *)calloc(1, sizeof(MenuLine)))) {
        fprintf(stderr, "Can't allocate menu line--out of space\n");
        exit(1);
    }
    return(ptr);
}

/*
 * Allocate a MenuInfo structure and return a pointer.
 */
MenuInfo *
AllocMenuInfo()
{
    MenuInfo *ptr;

    if (!(ptr = (MenuInfo *)calloc(1, sizeof(MenuInfo)))) {
        fprintf(stderr, "Can't allocate menu storage--out of space\n");
        exit(1);
    }
    return(ptr);
}

/*
 * Allocate a MenuLink structure and return a pointer.
 */
MenuLink *
AllocMenuLink()
{
    MenuLink *ptr;

    if (!(ptr = (MenuLink *)calloc(1, sizeof(MenuLink)))) {
        fprintf(stderr, "Can't allocate menu linked list storage--out of space\n");
        exit(1);
    }
    return(ptr);
}

/*
 * Stash the data in a menu line.
 */
MenuLine *
StashMenuLine(type, string)
int type;
char *string;
{
    MenuLine *ptr;

    ptr = AllocMenuLine();
    ptr->type = type;
    ptr->text = string;
    return(ptr);
}

/*
 * Stash menu data in a MenuInfo structure;
 */
MenuInfo *
stashmenuinfo(name, line, colors)
char *name;
MenuLine *line;
char *colors[];
{
    MenuInfo *ptr;

    ptr = AllocMenuInfo();
    ptr->name = name;
    ptr->line = line;
    ptr->foreground = colors[1];
    ptr->background = colors[0];
    ptr->fghighlight = colors[2];
    ptr->bghighlight = colors[3];
    return(ptr);
}

/*
 * Stash menu info data in a MenuLink structure;
 */
MenuLink *
stashmenulink(menuinfo)
MenuInfo *menuinfo;
{
    MenuLink *ptr;

    ptr = AllocMenuLink();
    ptr->next = NULL;
    ptr->menu = menuinfo;
    return(ptr);
}

/*
 * Append a menu line to a linked list of menu lines.
 */
MenuLine *
appendmenuline(list, line)
MenuLine *list;
MenuLine *line;
{
    MenuLine *ptr;

    if (list == NULL)
        list = line;
    else {
        for(ptr = list; ptr->next; ptr = ptr->next) /* NULL */;
        ptr->next = line;
        ptr = ptr->next;
        ptr->next = NULL;
    }
    return(list);
}

/*
 * Append a menu to a linked list of menus.
 */
MenuLink *
appendmenulink(list, link)
MenuLink *list;
MenuLink *link;
{
    MenuLink *ptr;

    if (list == NULL)
        list = link;
    else {
        for(ptr = list; ptr->next; ptr = ptr->next) /* NULL */;
        ptr->next = link;
        ptr = ptr->next;
        ptr->next = NULL;
    }
    return(list);
}

/*
 * Reset all previous bindings and free the space allocated to them.
 */
Bool ResetBindings()
{
    Binding *ptr, *nextptr;

    for(ptr = Blist; ptr; ptr = nextptr) {
        if(ptr->menuname) free(ptr->menuname);
        nextptr = ptr->next;
        free(ptr);
    }
    Blist = NULL;
}

/*
 * De-allocate all menus.
 */
Bool ResetMenus()
{
    MenuLink *mptr, *next_mptr;
    register MenuLine *lptr, *next_lptr;

    for(mptr = Menus; mptr; mptr = next_mptr) {
        free(mptr->menu->name);
        for(lptr = mptr->menu->line; lptr; lptr = next_lptr) {
            free(lptr->name);
            if (lptr->text) free(lptr->text);
            next_lptr = lptr->next;
            free(lptr);
        }
        next_mptr = mptr->next;
        free(mptr);
    }
    Menus = NULL;
}

/*
 * Set all numeric variables to zero and all boolean variables to FALSE.
 */
Bool ResetVariables()
{
    register int i;

    for (i = 0; KeywordTable[i].name; i++) {
        switch (KeywordTable[i].type) {
        case IsBoolTrue:
        case IsBoolFalse:
            *(KeywordTable[i].bptr) = FALSE;
            break;
        case IsNumeric:
            *(KeywordTable[i].nptr) = 0;
            break;
        default:
            break;
        }
    }
}
