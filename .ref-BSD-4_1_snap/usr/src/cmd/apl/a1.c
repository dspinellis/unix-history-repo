#include "apl.h"

char	*continu = "continue";

execute(s)
char *s;
{
	register i;
	register data *dp;
	register struct item *p;
	struct item *p1;
	int j;
	data (*f)(), d;

#ifdef SOMED
	if(debug)
		dump(s);
#endif

loop:
	i = *s++;
#ifdef FULLD
	if(debug) {
	    extern char *opname[];
		if(i==-1)
			aprintf("exec eof\n");
		else if(0<=i&&i<103) {
			aprintf("exec "); aprintf(opname[i]); aputchar('\n');
		   } else
			aprintf("exec %d\n",i);
	}
#endif
#ifdef SHORTD
	if(debug)
		aprintf("exec %d\n", i);
#endif
	switch(i) {

	default:
		error("exec B");

	case EOF:
		return;

	case EOL:
		pop();
		goto loop;

	case COMNT:
		push(newdat(DA,1,0));
		goto loop;

	case ADD:
	case SUB:
	case MUL:
	case DIV:
	case MOD:
	case MIN:
	case MAX:
	case PWR:
	case LOG:
	case CIR:
	case COMB:
	case AND:
	case OR:
	case NAND:
	case NOR:
	case LT:
	case LE:
	case EQ:
	case GE:
	case GT:
	case NE:
		f = exop[i];
		p = fetch2();
		p1 = sp[-2];
		if(p->type!=DA||p1->type!=DA) {
			if(p->type==CH&&p1->type==CH) {
				charfun(i, p, p1);
				goto loop;
			} else
				error("dyadic T E");
		}
		if(!p->rank||p->rank==1&&p->size==1) {
			d = p->datap[0];
			pop();
			p = p1;
			dp = p->datap;
			for(i=0; i<p->size; i++) {
				*dp = (*f)(d, *dp);
				dp++;
			}
			goto loop;
		}
		if(!p1->rank||p1->rank==1&&p1->size==1) {
			sp--;
			d = p1->datap[0];
			pop();
			push(p);
			dp = p->datap;
			for(i=0; i<p->size; i++) {
				*dp = (*f)(*dp, d);
				dp++;
			}
			goto loop;
		}
		if(p1->rank != p->rank)
			error("dyadic C E");
		for(i=0; i<p->rank; i++)
			if(p->dim[i] != p1->dim[i])
				error("dyadic C E");
		dp = p1->datap;
		for(i=0; i<p->size; i++) {
			*dp = (*f)(p->datap[i], *dp);
			dp++;
		}
		pop();
		goto loop;



	case PLUS:
	case MINUS:
	case SGN:
	case RECIP:
	case ABS:
	case FLOOR:
	case CEIL:
	case EXP:
	case LOGE:
	case PI:
	case RAND:
	case FAC:
	case NOT:
		f = exop[i];
		p = fetch1();
		if(p->type != DA)
			error("monadic T E");
		dp = p->datap;
		for(i=0; i<p->size; i++) {
			*dp = (*f)(*dp);
			dp++;
		}
		goto loop;

	case MEPS:      /*      execute         */
	case MENC:      /*      monadic encode  */
	case DRHO:
	case DIOT:
	case EPS:
	case REP:
	case BASE:
	case DEAL:
	case DTRN:
	case CAT:
	case CATK:
	case TAKE:
	case DROP:
	case DDOM:
	case MDOM:
	case GDU:
	case GDUK:
	case GDD:
	case GDDK:
	case COM:
	case COM0:
	case COMK:
	case EXD:
	case EXD0:
	case EXDK:
	case ROT:
	case ROT0:
	case ROTK:
	case MRHO:
	case MTRN:
	case RAV:
	case RAVK:
	case RED:
	case RED0:
	case REDK:
	case SCAN:
	case SCANK:
	case SCAN0:
	case REV:
	case REV0:
	case REVK:
	case ASGN:
	case INDEX:
	case ELID:
	case IPROD:
	case OPROD:
	case IMMED:
	case HPRINT:
	case PRINT:
	case MIOT:
	case MIBM:
	case DIBM:
	case BRAN0:
	case BRAN:
	case FUN:
	case ARG1:
	case ARG2:
	case AUTO:
	case REST:
		pcp = s;
		(*exop[i])();
		s = pcp;
		goto loop;

	case NAME:
		s += copy(IN, s, sp, 1);
		sp++;
		if(sp>staktop)
			newstak();
		goto loop;

	case QUOT:
		j = CH;
		goto con;

	case CONST:
		j = DA;

	con:
		i = *s++;
		p = newdat(j, i==1?0:1, i);
		s += copy(j, s, p->datap, i);
		push(p);
		goto loop;

	case QUAD:
		push(newdat(QD,0,0));
		goto loop;

	case QQUAD:
		push(newdat(QQ,0,0));
		goto loop;

	case CQUAD:
		push(newdat(QC,0,0));
		goto loop;
	}
}

static int comop;

charfun(op, p, p1)
struct	item *p, *p1;
{
register char c, *cxi;
register double *dxi;
	 int i;

	comop = op;
	switch(op) {
	   default:
		error("Y D E");
	   case LT:
	   case LE:
	   case EQ:
	   case GE:
	   case GT:
	   case NE:
		/* OK */;
	}
	if(!p->rank) {
		c = *((char*)(p->datap));
		cxi = (char*)(p1->datap);
		push(newdat(DA,p1->rank,p1->size));
		copy(IN, p1->dim, sp[-1]->dim, p1->rank);
		dxi = sp[-1]->datap;
		for(i=0; i<p1->size; i++)
			*dxi++ = (double)charcom(c,*cxi++);
		goto done;
	}
	if(!p1->rank) {
		c = ((char*)(p1->datap))[0];
		cxi = (char*)(p->datap);
		push(newdat(DA,p->rank,p->size));
		copy(IN, p->dim, sp[-1]->dim, p->rank);
		dxi = sp[-1]->datap;
		for(i=0; i<p->size; i++)
			*dxi++ = (double)charcom(*cxi++,c);
		goto done;
	}
	if(p1->rank != p->rank)
		error("dyadic Y C E");
	for(i=0; i<p->rank; i++)
		if(p->dim[i]!=p1->dim[i])
			error("dyadic Y C E");
	cxi = (char*)(p1->datap);
	push(newdat(DA,p->rank,p->size));
	copy(IN, p->dim, sp[-1]->dim, p->rank);
	dxi = sp[-1]->datap;
	for(i=0; i<p->size; i++)
		*dxi++ = (double)charcom(((char*)(p->datap))[i],*cxi++);
done:	dealloc(sp[-2]);
	dealloc(sp[-3]);
	sp[-3] = sp[-1];
	sp -= 2;
	return;
}

charcom(c1, c2)
register char c1, c2;
{
	switch(comop) {
	   case LE:
		return c1<=c2;
	   case LT:
		return c1<c2;
	   case EQ:
		return c1==c2;
	   case NE:
		return c1!=c2;
	   case GT:
		return c1>c2;
	   case GE:
		return c1>=c2;
	}
	error("Y B");		/*  "Cannot happen"  */
}

int	ex_add(),	ex_plus(),	ex_sub(),	ex_minus(),
	ex_mul(),	ex_sgn(),	ex_div(),	ex_recip(),
	ex_mod(),	ex_abs(),	ex_min(),	ex_floor(),
	ex_max(),	ex_ceil(),	ex_pwr(),	ex_exp(),
	ex_log(),	ex_loge(),	ex_cir(),	ex_pi(),
	ex_comb(),	ex_fac(),	ex_deal(),	ex_rand(),
	ex_drho(),	ex_mrho(),	ex_diot(),	ex_miot(),
	ex_rot0(),	ex_rev0(),	ex_dtrn(),	ex_mtrn(),
	ex_dibm(),	ex_mibm(),	ex_gdu(),	ex_gduk(),
	ex_gdd(),	ex_gddk(),	ex_exd(),	ex_scan(),
	ex_exdk(),	ex_scnk(),	ex_iprod(),	ex_oprod(),
	ex_br0(),	ex_br(),	ex_ddom(),	ex_mdom(),
	ex_com(),	ex_red(),	ex_comk(),	ex_redk(),
	ex_rot(),	ex_rev(),	ex_rotk(),	ex_revk(),
	ex_cat(),	ex_rav(),	ex_catk(),	ex_ravk(),
	ex_print(),	ex_elid(),	ex_index(),	ex_hprint(),
	ex_lt(),	ex_le(),	ex_gt(),	ex_ge(),
	ex_eq(),	ex_ne(),	ex_and(),	ex_or(),
	ex_nand(),	ex_nor(),	ex_not(),	ex_eps(),
	ex_meps(),	ex_rep(),	ex_take(),	ex_drop(),
	ex_exd0(),	ex_asgn(),	ex_immed(),	ex_fun(),
	ex_arg1(),	ex_arg2(),	ex_auto(),	ex_rest(),
	ex_com0(),	ex_red0(),	ex_exd0(),	ex_scn0(),
	ex_base(),	ex_menc();

int (*exop[])() =
{
	0,		/* 0 */
	ex_add,		/* 1 */
	ex_plus,	/* 2 */
	ex_sub,		/* 3 */
	ex_minus,	/* 4 */
	ex_mul,		/* 5 */
	ex_sgn,		/* 6 */
	ex_div,		/* 7 */
	ex_recip,	/* 8 */
	ex_mod,		/* 9 */
	ex_abs,		/* 10 */
	ex_min,		/* 11 */
	ex_floor,	/* 12 */
	ex_max,		/* 13 */
	ex_ceil,	/* 14 */
	ex_pwr,		/* 15 */
	ex_exp,		/* 16 */
	ex_log,		/* 17 */
	ex_loge,	/* 18 */
	ex_cir,		/* 19 */
	ex_pi,		/* 20 */
	ex_comb,	/* 21 */
	ex_fac,		/* 22 */
	ex_deal,	/* 23 */
	ex_rand,	/* 24 */
	ex_drho,	/* 25 */
	ex_mrho,	/* 26 */
	ex_diot,	/* 27 */
	ex_miot,	/* 28 */
	ex_rot0,	/* 29 */
	ex_rev0,	/* 30 */
	ex_dtrn,	/* 31 */
	ex_mtrn,	/* 32 */
	ex_dibm,	/* 33 */
	ex_mibm,	/* 34 */
	ex_gdu,		/* 35 */
	ex_gduk,	/* 36 */
	ex_gdd,		/* 37 */
	ex_gddk,	/* 38 */
	ex_exd,		/* 39 */
	ex_scan,	/* 40 */
	ex_exdk,	/* 41 */
	ex_scnk,	/* 42 */
	ex_iprod,	/* 43 */
	ex_oprod,	/* 44 */
	0,		/* 45 */
	0,		/* 46 */
	ex_br0,		/* 47 */
	ex_br,		/* 48 */
	ex_ddom,	/* 49 */
	ex_mdom,	/* 50 */
	ex_com,		/* 51 */
	ex_red,		/* 52 */
	ex_comk,	/* 53 */
	ex_redk,	/* 54 */
	ex_rot,		/* 55 */
	ex_rev,		/* 56 */
	ex_rotk,	/* 57 */
	ex_revk,	/* 58 */
	ex_cat,		/* 59 */
	ex_rav,		/* 60 */
	ex_catk,	/* 61 */
	ex_ravk,	/* 62 */
	ex_print,	/* 63 */
	0,		/* 64 */
	ex_elid,	/* 65 */
	0,		/* 66 */
	0,		/* 67 */
	ex_index,	/* 68 */
	ex_hprint,	/* 69 */
	0,		/* 70 */
	ex_lt,		/* 71 */
	ex_le,		/* 72 */
	ex_gt,		/* 73 */
	ex_ge,		/* 74 */
	ex_eq,		/* 75 */
	ex_ne,		/* 76 */
	ex_and,		/* 77 */
	ex_or,		/* 78 */
	ex_nand,	/* 79 */
	ex_nor,		/* 80 */
	ex_not,		/* 81 */
	ex_eps,		/* 82 */
	ex_meps,	/* 83 */
	ex_rep,		/* 84 */
	ex_take,	/* 85 */
	ex_drop,	/* 86 */
	ex_exd0,	/* 87 */
	ex_asgn,	/* 88 */
	ex_immed,	/* 89 */
	0,		/* 90 */
	0,		/* 91 */
	ex_fun,		/* 92 */
	ex_arg1,	/* 93 */
	ex_arg2,	/* 94 */
	ex_auto,	/* 95 */
	ex_rest,	/* 96 */
	ex_com0,	/* 97 */
	ex_red0,	/* 98 */
	ex_exd0,	/* 99 */
	ex_scn0,	/*100 */
	ex_base,	/*101 */
	ex_menc,        /*102 */        /*      monadic encod   */
};
