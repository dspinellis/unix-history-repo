/*
 *	Copyright (c) 1982 Regents of the University of California
 *	@(#)asscan.h 4.9 6/30/83
 */
/*
 *	The character scanner is called to fill up one token buffer
 *
 *	However, once the tokens are filled up by the
 *	character scanner, they are used in both the first and the second
 *	pass.  Holes created by .stab removal are replaced
 *	with 'skip' tokens that direct the second pass to ignore the
 *	following tokens.
 */

#define TOKBUFLG		4096
#define MAXTAHOE			32		
#define SAFETY			16

#define AVAILTOKS		TOKBUFLG -\
		sizeof(int) -\
		sizeof (struct tokbufdesc *) -\
		MAXTAHOE - SAFETY

struct tokbufdesc{
	int		tok_count;		/*absolute byte length*/
	struct		tokbufdesc *tok_next;
	char		toks[AVAILTOKS];
	char		bufovf[MAXTAHOE + SAFETY];
};
/*
 *	Definitions for handling tokens in the intermediate file
 *	buffers.
 *
 *	We want to have the compiler produce the efficient auto increment
 *	instruction for stepping through the buffer of tokens.  We must
 *	fool the type checker into thinking that a pointer can point
 *	to various size things.
 */

typedef int inttoktype;
/* typedef char inttoktype; */
typedef char bytetoktype;

typedef char *ptrall;			/*all uses will be type cast*/
typedef u_short lgtype;			/*for storing length of strings or skiping*/
/*
 *	defintions for putting various typed values
 *	into the intermediate buffers
 *	ptr will ALWAYS be of type ptrall
 */

#define ptype(type, ptr, val)	\
		(ptr) = (char *)((int)((ptr) + sizeof(type)-1)&~(sizeof(type)-1)), \
		*(type *)(ptr) = (val),	(ptr) += sizeof(type)

#define	pchar(ptr, val)		*(ptr)++  = (val)
#define	puchar(ptr, val)	*(ptr)++  = (val)

#define	pshort(ptr, val)	ptype (short, ptr, val)
#define	plgtype(ptr, val)	ptype (lgtype, ptr, val)
#define	pushort(ptr, val)	ptype (u_short, ptr, val)
#define	pint(ptr, val)		ptype (int, ptr, val)
#define	puint(ptr, val)		ptype (u_int, ptr, val)
#define	plong(ptr, val)		ptype (long, ptr, val)
#define	pulong(ptr, val)	ptype (u_int, ptr, val)
#define	pnumber(ptr, val)	ptype (Bignum, ptr, val)
#define	pptr(ptr, val)		ptype (int, ptr, val)
#define	popcode(ptr, val)	*(ptr)++  = (val)
#define	ptoken(ptr, val)	*(ptr)++  = (val)
#define	pskiplg(ptr, val)	ptype (lgtype, ptr, val)


#define gtype(type, ptr, val)	\
		(ptr) = (char *)((int)((ptr) + sizeof(type)-1)&~(sizeof(type)-1)), \
		(val) = *(type *)(ptr)  ,	(ptr) += sizeof(type)

#define	gchar(val, ptr)		(val) = *(ptr)++
#define	guchar(val, ptr)	(val) = *(ptr)++

#define	gshort(val, ptr)	gtype (short, ptr, val)
#define	glgtype(ptr, val)	gtype (lgtype, ptr, val)
#define	gushort(val, ptr)	gtype (u_short, ptr, val)
#define	gint(val, ptr)		gtype (int, ptr, val)
#define	guint(val, ptr)		gtype (u_int, ptr, val)
#define	glong(val, ptr)		gtype (long, ptr, val)
#define	gulong(val, ptr)	gtype (u_int, ptr, val)
#define	gnumber(val, ptr)	gtype (Bignum, ptr, val)
#define	gptr(val, ptr)		gtype (int, ptr, val)
#define	gopcode(val, ptr)	(val) = *(ptr)++
#define	gtoken(val, ptr)	(val) = *(ptr)++
#define	gskiplg(val, ptr)	gtype (lgtype, ptr, val)


extern	ptrall tokptr;	/*the next token to consume, call by copy*/
extern	ptrall tokub;	/*current upper bound in the current buffer*/
