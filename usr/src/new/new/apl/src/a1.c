static char Sccsid[] = "a1.c @(#)a1.c	1.1	10/1/82 Berkeley ";
#include "apl.h"

execute(s)
char *s;
{
	register i;
	register data *dp;
	register struct item *p;
	struct item *p1;
	int j;
	data (*f)(), d;
	extern char *opname[];
	char *psiskp();

	if(debug)
		dump(s,0);

loop:
	i = *s++;
	if(i != EOF)
		i &= 0377;
	lastop = i;
	if(debug && i >= 0)
		printf("	exec %s\n", opname[i]);
	switch(i) {

	default:
		error("exec B");

	case EOF:
		return;

	case EOL:
		pop();
		goto loop;

	case COMNT:
		*sp++ = newdat(DA, 1, 0);
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
		f = exop[i];
		p = fetch2();
		p1 = sp[-2];
		ex_dscal(0, f, p, p1);
		goto loop;


	case LT:
	case LE:
	case EQ:
	case GE:
	case GT:
	case NE:
		f = exop[i];
		p = fetch2();
		p1 = sp[-2];
		ex_dscal(1, f, p, p1);
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
			error("monadic T");
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
	case QRUN:
	case QEXEC:
	case FDEF:
	case QFORK:
	case QEXIT:
	case QWAIT:
	case QREAD:
	case QWRITE:
	case QUNLNK:
	case QRD:
	case QDUP:
	case QAP:
	case QKILL:
	case QSEEK:
	case QOPEN:
	case QCREAT:
	case QCLOSE:
	case QCHDIR:
	case QPIPE:
	case QCRP:
	case MFMT:
	case DFMT:
	case QNC:
	case NILRET:
	case LABEL:
	case SICLR:
	case SICLR0:
	case QSIGNL:
	case QFLOAT:
	case QNL:
		pcp = s;
		(*exop[i])();
		s = pcp;
		goto loop;

	case RVAL:		/* de-referenced LVAL */
		s += copy(IN, s, &p1, 1);
		if(((struct nlist *)p1)->use != DA)
			ex_nilret();		/* no fn rslt */
		else
			*sp++ = fetch(p1);
		goto loop;

	case NAME:
		s += copy(IN, s, sp, 1);
		sp++;
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
		*sp++ = p;
		goto loop;

	case QUAD:
		*sp++ = newdat(QD, 0, 0);
		goto loop;

	case XQUAD:
		*sp++ = newdat(QX, 0, 0);
		goto loop;

	case QQUAD:
		*sp++ = newdat(QQ, 0, 0);
		goto loop;

	case CQUAD:
		*sp++ = newdat(QC, 0, 0);
		goto loop;

	case PSI1:
		p = fetch1();
		if (p->size != 0){
			pop();
			goto loop;
		}
		else  s = psiskp (s);
			goto loop;
	case ISP1:
		p = fetch1();
		if (p->size == 0){
			pop();
			goto loop;
		}
		else  s = psiskp (s);
		goto loop;

	case PSI2:
	case ISP2:
		goto loop;
	}
}

char *
psiskp (s)
char *s;
{
	register i;
	register struct item *p;
	register cnt;

	pop();
	cnt = 1;
psilp:
	i = *s++;
	switch (i){
	default:
		goto psilp;
	case  NAME:
		s += copy(IN,s,sp,1);
		sp++;
		pop();
		goto psilp;
	case  QUOT:
		i = *s++;
		s += i;
		goto psilp;
	case  CONST:
		i = *s++;
		s += i * SDAT;
		goto psilp;
	case  PSI1:
	case  ISP1:
		cnt++;
		goto psilp;

	case  PSI2:
	case  ISP2:
		if((--cnt) == 0) {
			*sp++ = newdat (DA, 1, 0);
			return (s);
		}
		goto psilp;
	}
}

ex_dscal(m, f, p1, p2)
int (*f)();
struct item *p1, *p2;
{
	if(p1->type != p2->type)
			error("dyadic C");
	if(p1->type == CH )
		if(m)
			ex_cdyad(f, p1, p2);
		else
			error("dyadic T");
	else
		ex_ddyad(f, p1, p2);
}

ex_ddyad(f, ap, ap1)
data (*f)();
struct item *ap, *ap1;
{
	register i;
	register struct item *p;
	register data *dp;
	struct item *p1;
	data d;


	/* Conform arguments to function if necessary.  If they
	 * do not conform and one argument is a scalar, extend
	 * it into an array with the same dimensions as the
	 * other argument.  If neither argument is a scalar, but
	 * one is a 1-element vector, extend its shape to match
	 * the other argument.
	 */

	p = ap;
	p1 = ap1;

	if(p->rank < 2 && p->size == 1 && p1->rank != 0){
		d = p->datap[0];
		pop();
		p = p1;
		dp = p->datap;
		for(i=0; i<p->size; i++) {
			*dp = (*f)(d, *dp);
			dp++;
		}
		return;
	}
	if(p1->rank < 2 && p1->size == 1) {
		sp--;
		d = p1->datap[0];
		pop();
		*sp++ = p;
		dp = p->datap;
		for(i=0; i<p->size; i++) {
			*dp = (*f)(*dp, d);
			dp++;
		}
		return;
	}
	if(p1->rank != p->rank)
		error("dyadic C");
	for(i=0; i<p->rank; i++)
		if(p->dim[i] != p1->dim[i])
			error("dyadic C");
	dp = p1->datap;
	for(i=0; i<p->size; i++) {
		*dp = (*f)(p->datap[i], *dp);
		dp++;
	}
	pop();
}

ex_cdyad(f, ap, ap1)
data (*f)();
struct item *ap, *ap1;
{
	register i;
	register struct item *p;
	register char *cp;
	struct item *p1;
	data d1, d2;

	p = ap;
	p1 = ap1;
	if(p->rank == 0 || p->size == 1) {
		d1 = ((struct chrstrct *)p->datap)->c[0];
		pop();
		p = p1;
		cp = (char *)p->datap;
		for(i=0; i<p->size; i++) {
			d2 = *cp;
			*cp = (*f)(d1, d2);
			cp++;
		}
	} else if(p1->rank == 0 || p1->size == 1) {
		sp--;
		d1 = ((struct chrstrct *)p1->datap)->c[0];
		pop();
		*sp++ = p;
		cp = (char *)p->datap;
		for(i=0; i<p->size; i++) {
			d2 = *cp;
			*cp = (*f)(d2, d1);
			cp++;
		}
	} else {
		if(p1->rank != p->rank)
			error("dyadic C");
		for(i=0; i<p->rank; i++)
			if(p->dim[i] != p1->dim[i])
				error("dyadic C");
		cp = (char *)p1->datap;
		for(i=0; i<p->size; i++) {
			d1 = ((struct chrstrct *)p->datap)->c[i];
			d2 = *cp;
			*cp = (*f)(d1, d2);
			cp++;
		}
		p = p1;
		pop();
	}
	/*
	 * now convert the character vector to
	 * a numeric array.  Someday, we can make this a
	 * call to whomever creates "logical" type data.
	 */
	p1 = p;
	cp = (char *)p->datap;
	p = newdat(DA, p->rank, p->size);
	for(i=0; i<p->rank; i++)
		p->dim[i] = p1->dim[i];
	for(i=0; i<p->size; i++)
		p->datap[i] = (*cp++) & 0377;
	pop();
	*sp++ = p;
}

/*
 *   exop[] moved to seperate file "at.c"
 *   (a1.c had a "symbol table overflow".)
 */

ex_botch()
{
	error("exec P E");
}
