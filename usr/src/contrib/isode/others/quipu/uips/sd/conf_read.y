%{
#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/sd/RCS/conf_read.y,v 7.2 91/02/22 09:32:14 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/sd/RCS/conf_read.y,v 7.2 91/02/22 09:32:14 mrose Interim $
 */


#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "filt.h"

void exit();

extern FILE *config_file;
extern unsigned int curr_filt;
extern char *file_names[], *filttype[];
extern filt_struct *filt_arr[];
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
  register int c, count = 0;
  char lexeme[255];
  
  while(isspace(c = getc(config_file)))
    if (c == EOF) return(0);
  
  lexeme[count++] = c;
  
  switch(c) {
  case '#':
    while (getc(config_file) != '\n');
    return(yylex());
  case '"':
    count = 0;
    while ((c = getc(config_file)) != '"')
      lexeme[count++] = c;
    lexeme[count] = '\0';
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
    if((lexeme[count] = getc(config_file)) == '=') {
      yylval.symbol = APPROX;
      return APPROX;
    }		  	
    break;
  case '%':
    if((lexeme[count] = getc(config_file)) == '=') {
      yylval.symbol = SUBSTRING;
      return SUBSTRING;
    }
    break;
  case '=':
    yylval.symbol = EQUAL;
    return EQUAL;
  }
  
  while(!isspace(c = getc(config_file)) && c != '\0' && !issymbol(c))
    if (c != EOF)
      lexeme[count++] = c;
    else
      return(0);
  
  (void) fseek(config_file,(long) -1, 1);
  
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

yyerror(err)
     char *err;
{
  (void) fprintf(stderr,
                 "Parse error in '%s'. Exiting.\n",
                 (char *) file_names[curr_filt]);
  if (filttype[curr_filt]) {
    free(filttype[curr_filt]);
    filttype[curr_filt] = (char *) 0;
  }

  if (filt_arr[curr_filt]) free_filt(filt_arr[curr_filt]);
  exit(1);
}

int issymbol(c) 
     char c;
{
  switch(c) {
  case '#':
  case '"':
  case '(':
  case ')':
  case ':':
  case '&':
  case '|':
  case '!':
  case '*':
  case '~':
  case '=':
  case '%':
    return 1;
  }
  return 0;
}
