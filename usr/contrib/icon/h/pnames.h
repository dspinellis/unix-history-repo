/*
 * Structure for mapping string names of procedures to block addresses.
 */
struct pstrnm {
   char *pstrep;
   struct b_proc *pblock;
   };

extern struct b_proc
#define PDEF(p) B/**/p,
#include "../h/pdef.h"
	interp;	/* Hack to avoid ,; in expansion */
#undef PDEF

extern struct b_proc
   Basgn,
   Bbang,
   Bcat,
   Bcompl,
   Bdiff,
   Bdiv,
   Beqv,
   Binter,
   Blconcat,
   Blexeq,
   Blexge,
   Blexgt,
   Blexle,
   Blexlt,
   Blexne,
   Bminus,
   Bmod,
   Bmult,
   Bneg,
   Bneqv,
   Bnonnull,
   Bnull,
   Bnumber,
   Bnumeq,
   Bnumge,
   Bnumgt,
   Bnumle,
   Bnumlt,
   Bnumne,
   Bplus,
   Bpower,
   Brandom,
   Brasgn,
   Brefresh,
   Brswap,
   Bsect,
   Bsize,
   Bsubsc,
   Bswap,
   Btabmat,
   Btoby,
   Bunioncs,
   Bvalue;

struct pstrnm pntab[] = {
#define PDEF(p) "p", &B/**/p,
#include "../h/pdef.h"
#undef PDEF
	":=",		&Basgn,
	"!",		&Bbang,
	"||",		&Bcat,
	"~",		&Bcompl,
	"--",		&Bdiff,
	"/",		&Bdiv,
	"===",		&Beqv,
	"**",		&Binter,
	"|||",		&Blconcat,
	"==",		&Blexeq,
	">>=",		&Blexge,
	">>",		&Blexgt,
	"<<=",		&Blexle,
	"<<",		&Blexlt,
	"~==",		&Blexne,
	"-",		&Bminus,
	"%",		&Bmod,
	"*",		&Bmult,
	"-",		&Bneg,
	"~===",		&Bneqv,
	"\\",		&Bnonnull,
	"/",		&Bnull,
	"+",		&Bnumber,
	"=",		&Bnumeq,
	">=",		&Bnumge,
	">",		&Bnumgt,
	"<=",		&Bnumle,
	"<",		&Bnumlt,
	"~=",		&Bnumne,
	"+",		&Bplus,
	"^",		&Bpower,
	"?",		&Brandom,
	"<-",		&Brasgn,
	"^",		&Brefresh,
	"<->",		&Brswap,
	":",		&Bsect,
	"*",		&Bsize,
	"[]",		&Bsubsc,
	":=:",		&Bswap,
	"=",		&Btabmat,
	"...",		&Btoby,
	"++",		&Bunioncs,
	".",		&Bvalue,
	0,		0
	};
