%{
#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/xd/RCS/conf_read.y,v 7.2 91/02/22 09:32:44 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/xd/RCS/conf_read.y,v 7.2 91/02/22 09:32:44 mrose Interim $
 */

/*
 * $Log:	conf_read.y,v $
 * Revision 7.2  91/02/22  09:32:44  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:50:23  mrose
 * sync
 * 
 * Revision 7.0  90/06/12  13:12:18  mrose
 * *** empty log message ***
 * 
 * Revision 1.5  90/04/26  10:22:36  emsrssn
 * Installation fixed
 * 
 * 
 * Revision 1.4  90/04/25  17:28:06  emsrssn
 * Lint tidy up
 * 
 * 
 * Revision 1.3  90/04/19  13:54:07  emsrssn
 * keyboard accelerator now activates button highlight.
 * 
 * search types available is dependent on current position
 * to prevent unreasonable searches.
 * 
 * the help popup changes automatically depending on the 
 * position of the cursor
 * 
 * buttons remain a fixed size when the application is
 * resized
 * 
 * command line options are now handled properly
 * 
 * logging added
 * 
 * "reads" are now sorted to show mail address at top etc.
 * 
 * 
 * Revision 1.2  90/03/16  11:32:01  emsrdsm
 * *** empty log message ***
 * 
 * Revision 1.1  90/03/09  16:49:53  emsrdsm
 * Initial revision
 * 
 * Revision 1.1  90/03/09  12:14:12  emsrdsm
 * Initial revision
 * 
 * Revision 1.1  90/03/09  11:43:15  emsrdsm
 * Initial revision
 * 
 */

#include <stdio.h>
#include <ctype.h>
#include "quipu/util.h"
#include "filt.h"
#include "symtab.h"

extern make_type();
extern filt_struct *make_item_filter();
extern filt_struct *link_filters();
extern filt_struct *make_parent_filter();
extern put_symbol_value();

extern FILE *file;
extern int curr_filt;
extern char **file_names;
extern table_entry symtab;
%}
%start type_spec

%union {
  	filt_struct *filt;
	char strval[255];
	int symbol;
}

%token NUMBER NAME DEFAULT STRING OID AND OR NOT APPROX EQUAL ITEM

%type <filt> filter filter_list assertion filter_item
%type <symbol> filt_type match
%token <symbol> NOT AND OR APPROX EQUAL SUBSTRING
%token <symbol> '"' ':' '(' ')'
%token <strval> STRING OID
%type  <strval> name default

%%
 type_spec    :	name filter		{make_type($1, $2);}
  	      ;	

 name         :	NAME ':' STRING			{(void) strcpy($$, $3);}
	      ;

 default      :	DEFAULT ':' STRING		{(void) strcpy($$, $3);}
              |					{(void) strcpy($$, "\0");}
	      ;

 assertion    : '(' filt_type filter filter filter_list ')' 	{$$ = make_parent_filter($2, $3, $4, $5);}
	      | '(' NOT filter ')' 				{$$ = make_parent_filter($2, $3, (filt_struct *) 0,(filt_struct *) 0);}
	      | filter_item					{$$ = $1;}
	      ;

 filter_list  : filter filter_list 			{$$ = link_filters($1, $2);}
	      | filter					{$$ = $1;}
  	      |						{$$ = (filt_struct *) 0;}
	      ;

 filter       : filter_item			{$$ = $1;}
	      | assertion			{$$ = $1;}
	      ;

 filter_item  : '(' OID match STRING ')'	{$$ = make_item_filter($2, $3, $4);}
	      ;

 match        : APPROX				{$$ = $1;}
	      | EQUAL				{$$ = $1;}
	      | SUBSTRING                       {$$ = $1;}
  	      ;

 filt_type    : AND				{$$ = $1;}
	      | OR				{$$ = $1;}
	      ;
%%

yylex()
{
	int c, count = 0;
	char lexeme[255];

	while(iswspace(c = getc(file)))
	  	if (c == EOF) return(0);

        lexeme[count++] = c;
	
	switch(c) {
	      case '#':
	  	while (getc(file) != '\n');
		return(yylex());

              case '"':
		count = 0;
		while ((c = getc(file)) != '"')
		  	lexeme[count++] = c;
		lexeme[count] = '\0';
		(void) strcpy(yylval.strval, lexeme);
		return STRING;

	      case '$':
		while (!iswspace((c = getc(file))) && !issymbol(c))
		  	lexeme[count++] = c;
		lexeme[count] = '\0';
		put_symbol_value(symtab, lexeme+1, (char *) 0);
		(void) strcpy(yylval.strval, lexeme);
                return STRING;

	      case '(':
		return (int) c;
	      case ')':
		return (int) c;
	      case ':':
		return (int) c;
	      case '&':
		yylval.symbol = AND;
		return AND;
	      case '|':
		yylval.symbol = OR;
		return OR;
	      case '!':
		yylval.symbol = NOT;
		return NOT;
	      case '*':
		lexeme[count] = '\0';
		(void) strcpy(yylval.strval, lexeme);
		return STRING;
	      case '~':
		if((lexeme[count] = getc(file)) == '=') {
		  	yylval.symbol = APPROX;
			return APPROX;
		      }		  	
		break;
	      case '%':
		if((lexeme[count] = getc(file)) == '=') {
                        yylval.symbol = SUBSTRING;
			return SUBSTRING;
		      }
		
	      case '=':
		yylval.symbol = EQUAL;
		return EQUAL;
	      }

  	while(!iswspace(c = getc(file)) && c != '\0' && !issymbol(c))
	  	if (c != EOF)
		  	lexeme[count++] = c;
		else
	  		return(0);

	(void) fseek(file,(long) -1, 1);

	lexeme[count] = '\0';
	switch(*lexeme) {
	      case 'd':
	      case 'D':
		if(!strcmp(lexeme, "default") || !strcmp(lexeme, "DEFAULT"))
			return DEFAULT;
		else {
		        (void) strcpy(yylval.strval, lexeme);
			return STRING;
		}

	      case 'n':
	      case 'N':
		if(!strcmp(lexeme, "name") || !strcmp(lexeme, "NAME"))
		  	return NAME;
		else {
                        (void) strcpy(yylval.strval, lexeme);
		  	return STRING;
		}

	      case '0':
	      case '1':
	      case '2':
	      case '3':
	      case '4':
	      case '5':
	      case '6':
	      case '7':
	      case '8':
	      case '9':
		count = 0;
		while (isdigit(lexeme[count]) || lexeme[count] == '.') count++;
		if (lexeme[count] == '\0') {
		  	(void) strcpy(yylval.strval, lexeme);
			return OID;
		} else {
		  	(void) strcpy(yylval.strval, lexeme);
			return STRING;
		}

	      default:
		(void) strcpy(yylval.strval, lexeme);
		return STRING;
	}
}

yyerror(str)
char *str;
{
        (void) fprintf(stderr, "%s: ", str);
	(void) fprintf(stderr, "Parse error in -\n");
}

int
issymbol(c)
char c;
{
	switch(c) {
	      case '#':
                return 1;
              case '"':
                return 1;
	      case '(':
		return 1;
	      case ')':
		return 1;
	      case ':':
		return 1;
	      case '&':
		return 1;
	      case '|':
		return 1;
	      case '!':
		return 1;
	      case '*':
		return 1;
	      case '~':
		return 1;
	      case '=':
		return 1;
	      case '$':
		return 1;
	      case '%':
		return 1;
	}
	return 0;
}

int
iswspace(c)
char c;
{
  	switch(c) {
	      case ' ':
	  	return 1;
	      case '\n':
		return 1;
	      case '\t':
		return 1;
	}
	return 0;
}



