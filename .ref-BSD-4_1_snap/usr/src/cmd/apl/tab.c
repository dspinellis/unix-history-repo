#define	B	0100000
struct tab
{
	int	input;
	int	lexval;
	int	retval;
} tab[] =
{
/*
 * one of a kind
 */

	':',	unk,lpar,
	'"',	unk,rpar,
	';',	unk,lbkt,
	'\'',	unk,rbkt,
	'/',	COM,com,
	0200  , COM0,com0,
	'?',	EXD,com,
	0201  , EXD0,com0,
	'K',	unk,strng,
	'J',	unk,null,
	'.',	IPROD,dot,
	'L',	QUAD,quad,
	0202  , QQUAD,quad,
	0203  , CQUAD,quad,
	'<',	unk,semi,
	'>',	unk,cln,
	0204  , COMNT,comnt,
	'C',	COMNT,comnt,
	']',	BRAN0,tran,

/*
 * dyadic scalars
 *	op2 op1 v (dyadic op)
 */

	'#',	LT,dscal,
	'&',	GT,dscal,
	'$',	LE,dscal,
	'^',	GE,dscal,
	'%',	EQ,dscal,
	'*',	NE,dscal,
	')',	AND,dscal,
	'(',	OR,dscal,
	0205  , NAND,dscal,
	0206  , NOR,dscal,

/*
 * monadic or dyadic scalars
 *	op2 op1 v (dyadic op)
 *	op1 v+1 (monadic op)
 */

	'-',	ADD,mdscal,
	'_',	SUB,mdscal,
	'=',	MUL,mdscal,
	'+',	DIV,mdscal,
	'M',	MOD,mdscal,
	'D',	MIN,mdscal,
	'S',	MAX,mdscal,
	'P',	PWR,mdscal,
	0207  , LOG,mdscal,
	'O',	CIR,mdscal,
	0210  , COMB,mdscal,

/*
 * monadic
 *	op1 v (monadic op)
 */

	'T',	NOT,m,
/*
 * dyadic
 *	op2 op1 v (dyadic op)
 */

	'N',	REP,d,
	'Y',	TAKE,d,
	'U',	DROP,d,
	'[',	ASGN,asg,

/*
 * monadic or dyadic
 *	op2 op1 v (dyadic op)
 *	op1 v+1 (monadic op)
 */

	'E',    EPS,md,
	'B',    BASE,md,
	'Q',	DEAL,md,
	'R',	DRHO,md,
	'I',	DIOT,md,
	0211  , ROT0,md,
	0212  , DTRN,md,
	0213  , DIBM,md,
	0214  , DDOM,md,



/*
 * monadic with optional subscript
 *	op1 v (monadic op)
 *	op1 sub v+1 (subscripted monadic op)
 */

	0215  , GDU,msub,
	0216  , GDD,msub,

/*
 * dyadic with optional subscript
 *	op2 op1 v (dyadic op)
 *	op2 op1 sub v+1 (subscripted dyadic op)
 */


/*
 * monadic or dyadic with optional subscript
 *	op2 op1 v (dyadic op)
 *	op1 v+1 (monadic op)
 *	op2 op1 sub v+2 (subscripted dyadic op)
 *	op1 sub v+3 (subscripted monadic op)
 */

	0217  , ROT,mdsub,
	',',	CAT,mdsub,

/*
 * end of list
 */

	0
};

int	comtab[] =
{
	"ascii",	comnull,	ASCII,
	"apl",		comnull,	APL,
	"clear",	comnull,	CLEAR,
	"continue",	comnull,	CONTIN,
	"copy",		comnam,		COPY,
	"csh",		comnull,	CSH,
	"debug",	comnull,	DEBUG,
	"digits",	comexpr,	DIGITS,
	"drop",		comnam,		DROPC,
	"edit",		comnam,		ED_IT,
	"ex",		comnam,		EX_IT,
	"erase",	comnam,		ERASE,
	"fns",		comnull,	FNS,
	"fuzz",		comexpr,	FUZZ,
	"lib",		comnull,	LIB,
	"load",		comnam,		LOAD,
	"off",		comnull,	OFF,
	"origin",	comexpr,	ORIGIN,
	"read",		comnam,		READ,
	"save",		comnam,		SAVE,
	"vars",		comnull,	VARS,
	"vi",		comnam,		EX_VI,
	"width",	comexpr,	WIDTH,
#ifdef SOMED
	"symbols",	comnull,	SYMBOLS,
#endif
	0,		unk
};
	struct charbox 	chartab[] =
{
	'/','_',
	'?','_',
	'K','L',
	'L','O',
	'C','J',
	')','T',
	'(','T',
	'O','P',
	'.','K',
	'O','_',
	'?','O',
	'B','N',
	'+','L',
	'H','M',
	'G','M',
	'M','O',
	'F','a',
	'F','b',
	'F','c',
	'F','d',
	'F','e',
	'F','f',
	'F','g',
	'F','h',
	'F','i',
	'F','j',
	'F','k',
	'F','l',
	'F','m',
	'F','n',
	'F','o',
	'F','p',
	'F','q',
	'F','r',
	'F','s',
	'F','t',
	'F','u',
	'F','v',
	'F','w',
	'F','x',
	'F','y',
	'F','z'
};
