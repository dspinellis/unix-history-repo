/* "@(#)cdefs.h 4.2 %G%" */
/* type modifiers */

# define PTR  020
# define FTN  040
# define ARY  060

/* type packing constants */

# define TMASK 060
# define TMASK1 0300
# define TMASK2  0360
# define BTMASK 017
# define BTSHIFT 4
# define TSHIFT 2

/*	macros	*/

# define BTYPE(x)  (x&BTMASK)   /* basic type of x */
# define ISUNSIGNED(x) ((x)<=ULONG&&(x)>=UCHAR)
# define UNSIGNABLE(x) ((x)<=LONG&&(x)>=CHAR)
# define ENUNSIGN(x) ((x)+(UNSIGNED-INT))
# define DEUNSIGN(x) ((x)+(INT-UNSIGNED))
# define ISPTR(x) ((x&TMASK)==PTR)
# define ISFTN(x)  ((x&TMASK)==FTN)  /* is x a function type */
# define ISARY(x)   ((x&TMASK)==ARY)   /* is x an array type */
# define INCREF(x) (((x&~BTMASK)<<TSHIFT)|PTR|(x&BTMASK))
# define DECREF(x) (((x>>TSHIFT)&~BTMASK&0x3fff)|(x&BTMASK))
	/* pack and unpack field descriptors (size and offset) */
# define PKFIELD(s,o) ((o<<6)|s)
# define UPKFSZ(v)  (v&077)
# define UPKFOFF(v) (v>>6)

