#include "awk.h"
struct tok
{	char *tnm;
	int yval;
} tok[]	= {
"FIRSTTOKEN", 257,
"FINAL", 258,
"FATAL", 259,
"LT", 260,
"LE", 261,
"GT", 262,
"GE", 263,
"EQ", 264,
"NE", 265,
"MATCH", 266,
"NOTMATCH", 267,
"APPEND", 268,
"ADD", 269,
"MINUS", 270,
"MULT", 271,
"DIVIDE", 272,
"MOD", 273,
"UMINUS", 274,
"ASSIGN", 275,
"ADDEQ", 276,
"SUBEQ", 277,
"MULTEQ", 278,
"DIVEQ", 279,
"MODEQ", 280,
"JUMP", 281,
"XBEGIN", 282,
"XEND", 283,
"NL", 284,
"PRINT", 285,
"PRINTF", 286,
"SPRINTF", 287,
"SPLIT", 288,
"IF", 289,
"ELSE", 290,
"WHILE", 291,
"FOR", 292,
"IN", 293,
"NEXT", 294,
"EXIT", 295,
"BREAK", 296,
"CONTINUE", 297,
"PROGRAM", 298,
"PASTAT", 299,
"PASTAT2", 300,
"ASGNOP", 301,
"BOR", 302,
"AND", 303,
"NOT", 304,
"NUMBER", 305,
"VAR", 306,
"ARRAY", 307,
"FNCN", 308,
"SUBSTR", 309,
"LSUBSTR", 310,
"INDEX", 311,
"RELOP", 312,
"MATCHOP", 313,
"OR", 314,
"STRING", 315,
"DOT", 316,
"CCL", 317,
"NCCL", 318,
"CHAR", 319,
"CAT", 320,
"STAR", 321,
"PLUS", 322,
"QUEST", 323,
"POSTINCR", 324,
"PREINCR", 325,
"POSTDECR", 326,
"PREDECR", 327,
"INCR", 328,
"DECR", 329,
"FIELD", 330,
"INDIRECT", 331,
"LASTTOKEN", 332,
};
ptoken(n)
{
	if(n<128) printf("lex: %c\n",n);
	else	if(n<=256) printf("lex:? %o\n",n);
	else	if(n<LASTTOKEN) printf("lex: %s\n",tok[n-257].tnm);
	else	printf("lex:? %o\n",n);
	return;
}

char *tokname(n)
{
	if (n<=256 || n >= LASTTOKEN)
		n = 257;
	return(tok[n-257].tnm);
}
