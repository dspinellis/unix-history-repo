%{
#include <stdio.h>
#define yywrap() Next()
#undef  input()
#undef  unput

%}
%%
[^0-9A-Z"("")"-]*";"		{ if (SendAll) {REJECT;}
				     else {  yylval = ';';
				             return(';');
					     }
							}

[^0-9A-Z"("")"";"-]+		{ yylval = Concat(yytext,0);
				  return(BLANK);	}

[A-Z"("")"-]			{ yylval = yytext[0];
				  return(*yytext);	}

[0-9]				{ yylval = yytext[0];
				  return(DIGIT);	}

";"				{ yylval = ';';
				  return(';');		}
%%
