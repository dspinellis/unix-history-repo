/* Copyright (c) 1979 Regents of the University of California */

/*
 *	The character scanner is called to fill up one token buffer
 *
 *	In the first pass, the tokens in this buffer may be overwriten
 *	to eliminate .stabs, and to change fully assemblable instructions
 *	into ascii strings.  However, once the tokens are filled up by the
 *	character scanner, they are used in both the first and the second
 *	pass.  Holes created by .stab removal and preassembly are replaced
 *	with 'skip' tokens that direct the second pass to ignore the
 *	following tokens.
 *
 *	While the first pass could write a second version of the intermediate
 *	file and really purge the .stabs and such, the buffering required
 *	to do this seems to be too complex and too slow.
 */

#define TOKBUFLG		2*BUFSIZ
#define MAXVAX			32		
#define SAFETY			2*NCPS

#define AVAILTOKS		TOKBUFLG -\
		sizeof(short) -\
		sizeof (struct tokbufdesc *) -\
		MAXVAX - SAFETY

struct tokbufdesc{
	short		tok_count;		/*absolute byte length*/
	struct		tokbufdesc *tok_next;
	char		toks[AVAILTOKS];
	char		bufovf[MAXVAX + SAFETY];
};

/*
 *	All variables handling these resources are local to astmpfil.c;
 *	we must have the structure defnitions here so that
 *	asscan.c can touch the stuff in a token buffer
 */

/*
 *	Definitions for handling tokens in the intermediate file
 *	buffers.
 *
 *	We want to have the compiler produce the efficient auto increment
 *	instruction for stepping through the buffer of tokens.  We must
 *	fool the type checker into thinking that a pointer can point
 *	to various size things.
 */

typedef char toktype;

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
#define	pushort(ptr,val)	*(unsigned short *)ptr=val, ptr += sizeof(short)
#define	pint(ptr,val)		*(int *)ptr  = val,	ptr += sizeof(int)
#define	puint(ptr,val)		*(unsigned int *)ptr=val, ptr += sizeof(int)
#define	plong(ptr,val)		*(long *)ptr  = val,	ptr += sizeof(long)
#define	pulong(ptr,val)		*(unsigned long *)ptr=val,ptr += sizeof(long)
#define	pfloat(ptr,val)		*(float *)ptr  = val,	ptr += sizeof (float)
#define	pdouble(ptr,val)	*(double *)ptr  = val,	ptr += sizeof (double)
#define	pptr(ptr,val)		*(int *)ptr  = (val),	ptr += sizeof(ptrall)
#define	ptoken(ptr,val)		*ptr++  = val
#define	pstrlg(ptr,val)		*(lgtype *)ptr  = val,	ptr += sizeof(short)
#define	pskiplg(ptr,val)	*(lgtype *)ptr  = val,	ptr += sizeof(short)

#define	gchar(val, ptr)		val = *ptr++
#define	guchar(val, ptr)	val = *ptr++

#define	gshort(val, ptr)	val = *(short *)ptr , ptr += sizeof (short)
#define	gushort(val, ptr)	val = *(unsigned short *)ptr , ptr += sizeof (short)
#define	gint(val, ptr)		val = *(int *)ptr, ptr += sizeof (int)
#define	guint(val, ptr)		val = *(unsigend int *)ptr, ptr += sizeof (int)
#define	glong(val, ptr)		val = *(long *)ptr, ptr += sizeof (long)
#define	gulong(val, ptr)	val = *(unsigned long *)ptr, ptr += sizeof (long)
#define	gfloat(val, ptr)	val = *(float *)ptr, ptr += sizeof (float)
#define	gdouble(val, ptr)	val = *(double *)ptr, ptr += sizeof (double)
#define	gptr(val, ptr)		val = *(int *)ptr, ptr += sizeof (ptrall)
#define	gtoken(val, ptr)	val = *ptr++
#define	gstrlg(val, ptr)	val = *(lgtype *)ptr, ptr += sizeof (short)
#define	gskiplg(val, ptr)	val = *(lgtype *)ptr, ptr += sizeof (short)


ptrall tokptr;	/*the next token to consume, call by copy*/
ptrall tokub;	/*current upper bound in the current buffer*/

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

struct 	strdesc		strbuf[3];
struct 	strdesc		*strptr;	/*points to the current string*/
int			strno;		/*the current string being filled*/
