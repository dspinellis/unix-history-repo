2,/^extern int yylineno;$/c\
static int start_cond = 0;\
#define BEGIN start_cond =
/^struct yysvf \*yyestate;$/,/^extern struct yysvf yysvec/d
/^# define YYNEWLINE /,/^int nstr;/d
/^while((nstr = yylook()/,/^if(yywrap()) /d
/^case -1:$/,/^fprintf(yyout,"bad switch yylook /c\
	default: return(0);
/^struct yysvf *yybgin = yysvec+1;$/d
/^int yylineno /,$d
