/* Copyright (c) 1982, Regents, University of California */
#define ADDQ 5
#define ADD 13
#define IMMED 074
/* These structures are here for looks, only */
struct add { short op:4, reg:3, mode:3, ea:6; } x;
struct addq { short op:4, data:3, size:3, ea:6; } y;
nargs(arg)
long arg; /* this is only here for address calculation */
{
	register long *a5;
	register handy;
	register char reg, mode, ea;
#define size mode
#define data reg

	a5 = (&arg) - 2; /* this points to old a6 */
	a5 = (long *) *a5; /* a5 now = my parents a6 */
	a5 = (long *) a5[1]; /* pick up return address into a5 */

	handy = * (short *)a5;
	ea = handy & 077; handy >>= 6;
	mode = handy & 07; handy >>= 3;
	reg = handy & 07; handy >>= 3;
	/* op = handy & 017; */
	switch(handy & 017) {
	case ADD:
		if(reg!=7)
			return(0); /* this instruction doesn't adjust the sp */
		if(ea!=IMMED)
			return(0); /* too hard to decode adjustment */
		handy = (long) (1 + (short *) a5);
		if(mode==03) {  /* addw #n,a7 */
			handy = *(short *)handy;
			return(handy >> 2);
		}
		if(mode==07) { /* addl #n,a7 */
			handy = *(long *)handy;
			return(handy >> 2);
		}
		else return(0); /* this was doing something to d7 */
	case ADDQ:
		if(ea!=017)
			return(0); /* this doesn't adjust a7 */
		if(size!=02)
			return(0); /* should complain -- we are doing
				      addq[bw] something,a7 */
		switch(data) {
		case 0: return(2);
		case 4: return(1);
		}
	}
	return(0);
}
