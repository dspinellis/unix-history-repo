/*
 *	Copyright (c) 1982 Regents of the University of California
 *	@(#)asscan.h 4.4 %G%
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

#define TOKBUFLG		BUFSIZ
#define MAXVAX			32		
#define SAFETY			16

#define AVAILTOKS		TOKBUFLG -\
		sizeof(int) -\
		sizeof (struct tokbufdesc *) -\
		MAXVAX - SAFETY

struct tokbufdesc{
	int		tok_count;		/*absolute byte length*/
	struct		tokbufdesc *tok_next;
	char		toks[AVAILTOKS];
	char		bufovf[MAXVAX + SAFETY];
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
typedef char bytetoktype;

typedef char *ptrall;			/*all uses will be type cast*/
typedef short lgtype;			/*for storing length of strings or skiping*/
/*
 *	defintions for putting various typed values
 *	into the intermediate buffers
 *	ptr will ALWAYS be of type ptrall
 */

#define	pchar(ptr,val)		*ptr++  = val
#define	puchar(ptr,val)		*ptr++  = val

#define	pshort(ptr,val)		*(short *)ptr=val,	ptr += sizeof(short)
#define	pushort(ptr,val)	*(u_short *)ptr=val,	ptr += sizeof(short)
#define	pint(ptr,val)		*(int *)ptr  = val,	ptr += sizeof(int)
#define	puint(ptr,val)		*(u_int int *)ptr=val,	ptr += sizeof(int)
#define	plong(ptr,val)		*(long *)ptr  = val,	ptr += sizeof(long)
#define	pulong(ptr,val)		*(u_int long *)ptr=val,	ptr += sizeof(long)
#define	pnumber(ptr,val)	*(Bignum*)ptr=val,	ptr += sizeof(Bignum)
#define	popcode(ptr,val)	*(struct Opcode*)ptr=val,	ptr += sizeof(struct Opcode)

#define	pptr(ptr,val)		*(int *)ptr  = (val),	ptr += sizeof(ptrall)
#define	ptoken(ptr,val)		*ptr++  = val
#define	pstrlg(ptr,val)		*(lgtype *)ptr  = val,	ptr += sizeof(short)
#define	pskiplg(ptr,val)	*(lgtype *)ptr  = val,	ptr += sizeof(short)

#define	gchar(val, ptr)		val = *ptr++
#define	guchar(val, ptr)	val = *ptr++

#define	gshort(val, ptr)	val = *(short *)ptr , ptr += sizeof (short)
#define	gushort(val, ptr)	val = *(u_short *)ptr , ptr += sizeof (short)
#define	gint(val, ptr)		val = *(int *)ptr, ptr += sizeof (int)
#define	guint(val, ptr)		val = *(u_int *)ptr, ptr += sizeof (int)
#define	glong(val, ptr)		val = *(long *)ptr, ptr += sizeof (long)
#define	gulong(val, ptr)	val = *(u_int *)ptr, ptr += sizeof (long)
#define	gnumber(val, ptr)	val = *(Bignum *)ptr, ptr += sizeof(Bignum)
#define	gopcode(val, ptr)	val = *(struct Opcode *)ptr, ptr += sizeof(struct Opcode)

#define	gptr(val, ptr)		val = *(int *)ptr, ptr += sizeof (ptrall)
#define	gtoken(val, ptr)	val = *ptr++
#define	gstrlg(val, ptr)	val = *(lgtype *)ptr, ptr += sizeof (short)
#define	gskiplg(val, ptr)	val = *(lgtype *)ptr, ptr += sizeof (short)


extern	ptrall tokptr;	/*the next token to consume, call by copy*/
extern	ptrall tokub;	/*current upper bound in the current buffer*/

/*
 *	Strings are known for their characters and for their length.
 *	We cannot use a normal zero termination byte, because strings
 *	can contain anything.
 *
 *	We have two "strings", so that an input string that is too long can be
 *	split across two string buffers, and not confuse the yacc grammar.
 *	(This is probably superflous)
 *
 *	We have a third string of nulls so that the .skip can be 
 *	handled in the same way as strings.
 */
#define MAXSTRLG	127

struct strdesc{
	char		str_lg;
	char		str[MAXSTRLG];
};

extern	struct 	strdesc		strbuf[3];
extern	struct 	strdesc		*strptr;	/*points to the current string*/
extern	int			strno;		/*the current string being filled*/
char	*savestr();
