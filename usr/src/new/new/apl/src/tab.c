static char tab_c_Sccsid[] = "tab.c @(#)tab.c	1.3	10/5/82 Berkeley ";

/*
 * This table defines the operators in APL\11.
 * The first entry is the character representing
 * the operator, the second is the unique operator
 * identifier (which should give you a hint as
 * to what the operator is), and the third is
 * the operator type, of interest only to the
 * interpreter.
 * Those characters represented by octal numbers are actually
 * two-character overstrikes.  Ignore the leading "2", and
 * the rest of the number is an index into "chartab", below,
 * which lists the two-character overstrikes.  Overstrikes
 * may be in either order.
 *
 * Note: What isn't shown here is that unary minus
 * is ` (backwards apostrophe).  This is handled in lex.c, a0.c
 * and a2.c (both input and output).
 */
struct tab
{
	int	input;
	int	lexval;
	int	retval;
} tab[] = {

/*
 * one of a kind
 */

	'(',	unk,lpar,
	')',	unk,rpar,
	'[',	unk,lbkt,
	']',	unk,rbkt,
	'/',	COM,com,
	0200  , COM0,com0,
	'\\',	EXD,com,
	0201  , EXD0,com0,
	'\'',	unk,strng,
	'J',	unk,null,
	'.',	IPROD,dot,
	'L',	QUAD,Quad,
	0202  , QQUAD,Quad,
	0203  , CQUAD,Quad,
	';',	unk,semi,
	':',	unk,cln,
	0204  , COMNT,comnt,
	'C',	COMNT,comnt,
	'}',	BRAN0,tran,

/*
 * dyadic scalars
 *	op2 op1 v (dyadic op)
 */

	'<',	LT,dscal,
	'>',	GT,dscal,
	'$',	LE,dscal,
	0220,	LE,dscal,
	'&',	GE,dscal,
	0221,	GE,dscal,
	'=',	EQ,dscal,
	'#',	NE,dscal,
	0222,	NE,dscal,
	'^',	AND,dscal,
	'A',	AND,dscal,
	'Q',	OR,dscal,
	'V',	OR,dscal,
	0205  , NAND,dscal,
	0231,	NAND,dscal,
	0206  , NOR,dscal,
	0223,	NAND,dscal,
	0224,	NOR,dscal,

/*
 * monadic or dyadic scalars
 *	op2 op1 v (dyadic op)
 *	op1 v+1 (monadic op)
 */

	'+',	ADD,mdscal,
	'-',	SUB,mdscal,
	'M',	MUL,mdscal,
	'X',	MUL,mdscal,
	0225,	MUL,mdscal,
	'P',	DIV,mdscal,
	0240,	DIV,mdscal,
	'%',	DIV,mdscal,
	0226,	DIV,mdscal,
	'|',	MOD,mdscal,
	'D',	MIN,mdscal,
	'S',	MAX,mdscal,
	'*',	PWR,mdscal,
	0207  , LOG,mdscal,
	'O',	CIR,mdscal,
	0210  , COMB,mdscal,
	'!',	COMB,mdscal,

/*
 * monadic
 *	op1 v (monadic op)
 */

	'~',	NOT,m,
	0241,	EPS+1,m,
/*
 * dyadic
 *	op2 op1 v (dyadic op)
 */

	'N',	REP,d,
	'Y',	TAKE,d,
	'U',	DROP,d,
	'_',	ASGN,asg,
	'{',	ASGN,asg,

/*
 * monadic or dyadic
 *	op2 op1 v (dyadic op)
 *	op1 v+1 (monadic op)
 */

	'E',    EPS,md,
	'B',    BASE,md,
	'?',	DEAL,md,
	'R',	DRHO,md,
	'I',	DIOT,md,
	0211  , ROT0,md,
	0212  , DTRN,md,
	0213  , DIBM,md,
	0214  , DDOM,md,
	0242,	DFMT,md,



/*
 * monadic with optional subscript
 *	op1 v (monadic op)
 *	op1 sub v+1 (subscripted monadic op)
 */

	0215  , GDU,msub,
	0216  , GDD,msub,
	0227,	GDU,msub,
	0230,	GDD,msub,

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
 *	ISP and PSI
 */
	0232,	PSI,d,
	0233,	ISP,d,

/*
 *	other, non-function
 */
	0234,	unk,null,
	0235,	unk,null,
	0236,	unk,null,
	0237,	unk,null,
	'@',	unk,null,

/*
 * end of list
 */

	0
};

struct {
	char	*ct_name;		/* command name string */
	int	ct_ytype;		/* command type */
	int	ct_ylval;		/* "yylval" value */
} comtab[] = {
	"clear",	comnull,	CLEAR,
	"continue",	comnull,	CONTIN,
	"copy",		comnam,		COPY,
	"debug",	comnull,	DEBUG,
	"digits",	comexpr,	DIGITS,
	"drop",		comlist,	DROPC,
	"edit",		comnam,		EDIT,
	"editf",	comnam,		EDITF,
	"write",	comnam,		WRITE,
	"trace",	comnull,	TRACE,
	"untrace",	comnull,	UNTRACE,
	"erase",	comlist,	ERASE,
	"fns",		comnull,	FNS,
	"fuzz",		comexpr,	FUZZ,
	"lib",		comnull,	LIB,
	"load",		comnam,		LOAD,
	"off",		comnull,	OFF,
	"origin",	comexpr,	ORIGIN,
	"read",		comnam,		READ,
	"save",		comnam,		SAVE,
	"vars",		comnull,	VARS,
	"width",	comexpr,	WIDTH,
	"vsave",	comnam,		VSAVE,
	"script",	comnam,		SCRIPT,
	"reset",	comnull,	RESET,
	"si",		comnull,	SICOM,
	"code",		comnam,		CODE,
	"del",		comnam,		DEL,
	"shell",	comnull,	SHELL,
	"list",		comnam,		LIST,
	"prws",		comnull,	PRWS,
	0,		unk
};

/*
 * List of two-character escapes.  Indexed by 02XX entries
 * in "tab", above.  Entries must be in lexical order, i.e.
 * 'V~' will work, '~V' will not (since overstrikes are
 * sorted before they are looked up).  'V~' is 6 down in
 * the table, and thus corresponds to 0206,
 * which "tab" shows to be NOR.
 */
int     chartab[] = {

	'-/',		/* 0200 comprs */
	'-\\',		/* 0201 expand */
	'\'L',		/* 0202 quote quad */
	'LO',		/* 0203 circle quad */
	'CJ',		/* 0204 lamp */
	'^~',		/* 0205 nand */
	'V~',		/* 0206 nor */
	'*O',		/* 0207 log */
	'\'.',		/* 0210 comb/fact ('!') */
	'-O',		/* 0211 rotate */
	'O\\',		/* 0212 transpose */
	'BN',		/* 0213 i beam */
	'%L',		/* 0214 domino */
	'A|',		/* 0215 grade up */
	'V|',		/* 0216 grade dn */
	'O|',		/* 0217 rotate */
	'<=',		/* 0220 less eq */
	'=>',		/* 0221 greater eq */
	'/=',		/* 0222 not eq */
	'A~',		/* 0223 nand */
	'Q~',		/* 0224 nor */
	'/\\',		/* 0225 multiply */
	'-:',		/* 0226 divide */
	'H|',		/* 0227 another grade up */
	'G|',		/* 0230 another dgrade dn */
	'&~',		/* 0231 yet another nand */
	'U|',		/* 0232 PSI */
	'C|',		/* 0233 ISP */
	'Y~',		/* 0234 bracket 1 */
	'U~',		/* 0235 bracket 2 */
	'-U',		/* 0236 another bracket 2 */
	'-Y',		/* 0237 another bracket 2 */
	'//',		/* 0240 alternate divide */
	'BJ',		/* 0241 standard execute */
	'JN',		/* 0242 format */
/*
 *	function alpha() in lex.c must be changed whenever this
 *	table is updated.  It must know the index of the alternate
 *	character set (currently 0243)
 */
	'Fa',		/* alternate character set */
	'Fb',
	'Fc',
	'Fd',
	'Fe',
	'Ff',
	'Fg',
	'Fh',
	'Fi',
	'Fj',
	'Fk',
	'Fl',
	'Fm',
	'Fn',
	'Fo',
	'Fp',
	'Fq',
	'Fr',
	'Fs',
	'Ft',
	'Fu',
	'Fv',
	'Fw',
	'Fx',
	'Fy',
	'Fz',
	0
};

/*
 *	qtab -- table of valid quad functions
 *	the format of the qtab is the similar to tab, above
 *
 */
struct qtab{
	char	*qname;
	int	qtype;
	int	rtype;
} qtab[] = {

	"lx",	XQUAD,	Quad,
	"width", QWID,	Quad,
	"run",	QRUN,	m,
	"fuzz",	QFUZZ,	Quad,
	"fork",	QFORK,	m,
	"wait",	QWAIT,	m,
	"exec",	QEXEC,	m,
	"cr",	QCRP,	m,
	"fx",	FDEF,	m,
	"exit",	QEXIT,	m,
	"pipe",	QPIPE,	m,
	"chdir",QCHDIR,	m,
	"open",	QOPEN,	d,
	"close", QCLOSE, m,
	"read",	QREAD,	d,
	"write",QWRITE,	d,
	"creat",QCREAT,	d,
	"seek",	QSEEK,	m,
	"kill",	QKILL,	d,
	"rd",	QRD,	m,
	"rm",	QUNLNK,	m,
	"dup",	QDUP,	m,
	"ap",	QAP,	d,
	"rline",QRD,	m,
	"nc",	QNC,	m,
	"sig",	QSIGNL,	d,
	"float",QFLOAT,	m,
	"nl"	,QNL,	m,
	0};
