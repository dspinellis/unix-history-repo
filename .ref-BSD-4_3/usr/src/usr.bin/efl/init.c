#include "defs"
#include "tokdefs"

kwinit()
{
struct key { char *keyn; int keyval; } *p;
static struct key keys[] = {
	"common", COMMON,
	"internal", INTERNAL,
	"external", EXTERNAL,
	"automatic", AUTOMATIC,
	"static", STATIC,
	"value", VALUE,
	"procedure", PROCEDURE,
	"blockdata", BLOCKDATA,
	"subroutine", PROCEDURE	/* NB */,
	"function", PROCEDURE		/* NB */,
	"option", OPTION,
	"include", INCLUDE,
	"define", DEFINE,
	"end", END,
	"integer", INTEGER,
	"real", REAL,
	"complex", COMPLEX,
	"logical", LOGICAL,
	"character", CHARACTER,
	"struct", STRUCT,
	"field", FIELD,
	"array", ARRAY,
	"dimension", DIMENSION,
	"long", LONG,
	"short", SHORT,
	"initial", INITIAL,
	"equivalence", EQUIVALENCE,
	"implicit", IMPLICIT,
	"debug", DEBUG,
	"if", IF,
	"else", ELSE,
	"while", WHILE,
	"until", UNTIL,
	"repeat", REPEAT,
	"do", DO,
	"for", FOR,
	"switch", SWITCH,
	"select", SWITCH,
	"case", CASE,
	"default", DEFAULT,
	"go", GO,
	"goto", GOTO,
	"break", BREAK,
	"exit", EXIT,
	"next", NEXT,
	"return", RETURN,
	"continue", CONTINUE,
	"call", CALL,
	"double", DOUBLE,
	"precision", PRECISION,
	"doubleprecision", DOUBLEPRECISION,
	"sizeof", SIZEOF,
	"lengthof", LENGTHOF,
	"read", READ,
	"write", WRITE,
	"readbin", READBIN,
	"writebin", WRITEBIN,
	"true", TRUE,
	"false", FALSE,
	0, 0 } ;

for(p = keys ; p->keyn ; ++p)
	mkkeywd(p->keyn, p->keyval);
}



geninit()
{
struct gen { char *genn; int restype; char *specn; int argtype; } *p;
static struct gen gens[] = {
	"abs", TYINT, "iabs", TYINT,
	"abs", TYREAL, "abs", TYREAL,
	"abs", TYLREAL, "dabs", TYLREAL,
	"abs", TYCOMPLEX, "cabs", TYREAL,

	"sin", TYREAL, "sin", TYREAL,
	"sin", TYLREAL, "dsin", TYLREAL,
	"sin", TYCOMPLEX, "csin", TYCOMPLEX,

	"cos", TYREAL, "cos", TYREAL,
	"cos", TYLREAL, "dcos", TYLREAL,
	"cos", TYCOMPLEX, "ccos", TYCOMPLEX,

	"atan", TYREAL, "atan", TYREAL,
	"atan", TYLREAL, "datan", TYLREAL,

	"atan2", TYREAL, "atan2", TYREAL,
	"atan2", TYLREAL, "datan2", TYLREAL,

	"sqrt", TYREAL, "sqrt", TYREAL,
	"sqrt", TYLREAL, "dsqrt", TYLREAL,
	"sqrt", TYCOMPLEX, "csqrt", TYCOMPLEX,

	"log", TYREAL, "alog", TYREAL,
	"log", TYLREAL, "dlog", TYLREAL,
	"log", TYCOMPLEX, "clog", TYCOMPLEX,

	"log10", TYREAL, "alog10", TYREAL,
	"log10", TYLREAL, "dlog10", TYLREAL,

	"exp", TYREAL, "exp", TYREAL,
	"exp", TYLREAL, "dexp", TYLREAL,
	"exp", TYCOMPLEX, "cexp", TYCOMPLEX,

	"int", TYREAL, "int", TYINT,
	"int", TYLREAL, "idint", TYINT,

	"mod", TYINT, "mod", TYINT,
	"mod", TYREAL, "amod", TYREAL,
	"mod", TYLREAL, "dmod", TYLREAL,

	"min", TYINT, "min0", TYINT,
	"min", TYREAL, "amin1", TYREAL,
	"min", TYLREAL, "dmin1", TYLREAL,

	"max", TYINT, "max0", TYINT,
	"max", TYREAL, "amax1", TYREAL,
	"max", TYLREAL, "dmax1", TYLREAL,

	"sign", TYREAL, "sign", TYREAL,
	"sign", TYINT, "isign", TYINT,
	"sign", TYLREAL, "dsign", TYLREAL,
	0, 0, 0, 0 } ;

for(p = gens ; p->genn ; ++p)
	mkgeneric(p->genn, p->restype, p->specn, p->argtype);
}

knowninit()
{
struct known { char *knownn; int knowntype; } *p;
static struct known knowns[ ] = {
	"abs", TYREAL,
	"iabs", TYINT,
	"dabs", TYLREAL,
	"aint", TYREAL,
	"int", TYINT,
	"idint", TYINT,
	"amod", TYREAL,
	"mod", TYINT,
	"amax0", TYREAL,
	"amax1", TYREAL,
	"max0", TYINT,
	"max1", TYINT,
	"dmax1", TYLREAL,
	"amin0", TYREAL,
	"amin1", TYREAL,
	"min0", TYINT,
	"min1", TYINT,
	"dmin1", TYLREAL,
	"float", TYREAL,
	"ifix", TYINT,
	"sign", TYREAL,
	"isign", TYINT,
	"dsign", TYLREAL,
	"dim", TYREAL,
	"idim", TYINT,
	"sngl", TYREAL,
	"real", TYREAL,
	"aimag", TYREAL,
	"dble", TYLREAL,
	"cmplx", TYCOMPLEX,
	"conjg", TYCOMPLEX,
	"exp", TYREAL,
	"dexp", TYLREAL,
	"cexp", TYCOMPLEX,
	"alog", TYREAL,
	"dlog", TYLREAL,
	"clog", TYCOMPLEX,
	"alog10", TYREAL,
	"dlog10", TYLREAL,
	"sin", TYREAL,
	"dsin", TYLREAL,
	"csin", TYCOMPLEX,
	"cos", TYREAL,
	"dcos", TYLREAL,
	"ccos", TYCOMPLEX,
	"tanh", TYREAL,
	"sqrt", TYREAL,
	"dsqrt", TYLREAL,
	"csqrt", TYCOMPLEX,
	"atan", TYREAL,
	"datan", TYLREAL,
	"atan2", TYREAL,
	"datan2", TYLREAL,
	"dmod", TYLREAL,
	"cabs", TYREAL,
	0, 0 };

for(p = knowns ; p->knownn ; ++p)
	mkknown(p->knownn, p->knowntype);
}
