#ifndef lint
static char sccsid[] = "@(#)c21.c	1.5 (Berkeley/CCI) %G%";
#endif

/*
 * C object code improver-- second part
 */

#include "c2.h"
#include <stdio.h>
#include <ctype.h>

int bitsize[] = {0,8,16,32,64,32,64}; /* index by type codes */

redun3(p) register struct node *p; {
/* check for 3 addr instr which should be 2 addr */
	if (has3ops(p)) {
		if (equstr(regs[RT1],regs[RT3])
		  && (p->op==ADD || p->op==MUL || p->op==AND || p->op==OR || p->op==XOR)) {
			register char *t=regs[RT1]; regs[RT1]=regs[RT2]; regs[RT2]=t;
		}
		if (equstr(regs[RT2],regs[RT3])) {
			p->subop=(p->subop&0xF)|(OP2<<4); p->pop=0;
			lastrand=regs[RT2]; *regs[RT3]=0; return(1);
		}
	} return(0);
}

bmove() {
	register struct node *p, *lastp; register char *cp1,*cp2; register int r;
	struct node *flops();

	refcount();
	for (p=lastp= &first; 0!=(p=p->forw); lastp=p);
	clearreg(); clearuse();
	for (p=lastp; p!= &first; p=p->back) {
	if (debug) {
		printf("Uses: ");
		if (useacc)
			printf("acc: %s\n",useacc->code? useacc->code:"");
		for (r=NUSE;--r>=0;) if (uses[r])
			printf("%d: %s\n",r,uses[r]->code? uses[r]->code:"");
		printf("-\n");
	}
	r=(p->subop>>4)&0xF;
	splitrand(p);
	if (OP3==r && 0!=redun3(p)) {newcode(p); redunm++;}
		/* ops that do nothing */
	if(p->op==MOV && equstr(regs[RT1], regs[RT2]))
		if(p->forw->op!=CBR) {
			delnode(p); redunm++; continue;
		} else {
			p->op=TST; p->pop=0;
			while(*p->code++ != ',');
			redunm++;
		}
	else if((cp1=p->code, *cp1++)=='$' &&
	 (*cp1=='0' || *cp1=='1' || *cp1++=='-' && *cp1=='1') && cp1[1]==',') {
		switch((p->code[1]<<8)|ord(p->op)) {
		case (('0'<<8)|ord(ADD)):
		case (('0'<<8)|ord(SUB)):
		case (('-'<<8)|ord(AND)):
		case (('0'<<8)|ord(OR)):
		case (('0'<<8)|ord(XOR)):
		case (('1'<<8)|ord(MUL)):
		case (('1'<<8)|ord(DIV)):
		case (('0'<<8)|ord(SHAL)):
		case (('0'<<8)|ord(SHAR)):
		case (('0'<<8)|ord(SHL)):
		case (('0'<<8)|ord(SHR)):
			if(r == OP2) {
				if(p->forw->op!=CBR) {
					delnode(p); redunm++; continue;
				} else {
					p->op=TST; p->subop&=0xF; p->pop=0;
					while(*p->code++ != ',');
					redunm++;
				}
			} else {	/* OP3 or shift */
				p->op=MOV; p->subop&=0xF; p->pop=0;
				while(*p->code++ != ',');
				p = p->forw; redunm++; continue;
			}
			break;
		case (('0'<<8)|ord(MUL)):
		case (('0'<<8)|ord(AND)):
			p->op=CLR; p->subop&=0xF; p->pop=0;
			while(*p->code++ != ',');
			if(r == OP3)
				while(*p->code++ != ',');
			redunm++;
		}
	}
	switch (p->op) {
	case LABEL: case DLABEL:
		for (r=NUSE; --r>=0;)
			if (uses[r]) p->ref=(struct node *) (((int)p->ref)|(1<<r));
			if (useacc) p->ref=(struct node *) (((int)p->ref)|(1<<NUSE));
		break;
	case CALLS:
	case CALLF:
		clearuse(); goto std;
	case NIL:
		clearuse(); break;
	case CVT:
		{ long n;
		if ((p->subop&0xF)!=LONG) goto std; cp1=p->code;
		if (*cp1++!='$') goto std; splitrand(p);
		n = getnum(&regs[RT1][1]);
		if(r==BYTE && (n<-128 || n>127)) goto std;
		if(r==WORD && (n<-32768 || n>32767)) goto std;
		p->op = MOV; p->subop = r; p->pop = 0;
		} goto std;

	case SUB:
		if ((p->subop&0xF)!=LONG) goto std; cp1=p->code;
		if (*cp1++!='$') goto std; splitrand(p);
#ifdef MOVAFASTER
		if (equstr(regs[RT2],"fp") && !indexa(regs[RT1])) {/* address comp. */
			char buf[C2_ASIZE]; cp2=buf; *cp2++='-'; 
			cp1=regs[RT1]+1; while (*cp2++= *cp1++); --cp2;
			cp1="(fp),"; while (*cp2++= *cp1++); --cp2;
			cp1=regs[RT3]; while (*cp2++= *cp1++);
			p->code=copy(buf); p->op = MOVA; p->subop = BYTE; p->pop=0;
		} else
#endif MOVAFASTER
		if (*cp1++=='-' && 0==(r=getnum(cp1)) &&
		!checkexpr(cp1)) {
			p->op=ADD; p->pop=0; *--cp1='$'; p->code=cp1;
		} goto std;
	case ADD:
		if ((p->subop&0xF)!=LONG) goto std; cp1=p->code;
		if (*cp1++!='$') goto std; splitrand(p);
		if (isstatic(cp1) && tempreg(regs[RT2],r) && uses[r]==p->forw)
		{
			/* address comp:
			**	addl2	$_foo,r0  \	movab	_foo[r0],bar
			**	movl	r0,bar	  /
			*/
			register struct	node	*pnext = p->forw;
			char	buf[C2_ASIZE];

			if (pnext->op == MOV && pnext->subop == LONG)
			{
				cp1 = &regs[RT1][1]; cp2 = &buf[0];
				while (*cp2++ = *cp1++) ; cp2--;
				splitrand(pnext);
				if (r == isreg(regs[RT1]))
				{
					delnode(p); p = pnext;
					p->op = MOVA; p->subop = BYTE;
					p->pop = 0;
					cp1 = regs[RT1]; *cp2++ = '[';
					while (*cp2++ = *cp1++) ; cp2--;
					*cp2++ = ']'; *cp2++ = ',';
					cp1 = regs[RT2];
					while (*cp2++ = *cp1++) ;
					p->code = copy(buf);
				}
			}
		}
		else
#ifdef MOVAFASTER
		if (equstr(regs[RT2],"fp") && !indexa(regs[RT1])) {/* address comp. */
			cp2=cp1-1; cp1=regs[RT1]+1; while (*cp2++= *cp1++); --cp2;
			cp1="(fp)"; while (*cp2++= *cp1++); *--cp2=',';
			p->op = MOVA; p->subop = BYTE; p->pop=0;
		} else
#endif MOVAFASTER
		if (*cp1++=='-' && 0==(r=getnum(cp1)) &&
		!checkexpr(cp1)) {
			p->op=SUB; p->pop=0; *--cp1='$'; p->code=cp1;
		}
		/* fall thru ... */
	case CASE:
	default: std:
		p=bflow(p); break;

	case MUL:
		/*
		** Change multiplication
		** by constant powers of 2 to shifts.
		*/
		splitrand(p);
		if (regs[RT1][0] != '$' || regs[RT1][1] == '-') goto std;
		if ((r = ispow2(getnum(&regs[RT1][1]))) <= 0) goto std;
 		/* mull2 $2,x */
		if(r == 1 && p->subop == U(LONG, OP2)) {
			strcpy(regs[RT1], regs[RT2]);
			p->op = ADD; p->pop = 0; newcode(p);
			goto std;
		}
		if (p->subop == U(LONG,OP2))
			strcpy(regs[RT3], regs[RT2]);
		sprintf(regs[RT1], "$%d", r);
		p->op = SHL; p->subop = LONG;
		p->pop = 0; newcode(p);
		goto std;

	case SHAL:
	case SHL:
	{
		/* bit tests:
		**	shll	A,$1,rC    \
		**	bitl	B,rC	    >	jbc	A,B,D
		**	jeql	D	   /
		**
		** address comp:
		**	shll	$1,bar,r0  \	movl	bar,r0
		**	movab	_foo[r0]   /	movaw	_foo[r0]
		**
		**	shll	$2,r0,r0   \	moval	_foo[r0]
		**	movab	_foo[r0]   /
		*/
		register struct	node	*pf;
		register struct	node	*pn;
		register int	shfrom, shto;
		long	shcnt;
		char	*regfrom;

		splitrand(p);
		if (regs[RT1][0] != '$') {
			if(isreg(regs[RT1]) < 0) goto std; /* alignment */
			if (regs[RT2][0] != '$') goto std;
			if (getnum(&regs[RT2][1]) != 1) goto std;
			if (!tempreg(regs[RT3],r)) goto std;
			if ((pf = p->forw)->op != BIT && pf->op!=AND) goto std;
			if (uses[r] && uses[r] != pf) goto std;
			splitrand(pf);
			if (r == isreg(regs[RT1])) cp2 = regs[RT2];
			else if (r == isreg(regs[RT2])) cp2 = regs[RT1];
			else goto std;
			if (*cp2 == '$') goto std;
			if ((pn = pf->forw)->op != CBR) goto std;
			if (pn->subop != JEQ && pn->subop != JNE) goto std;
			delnode(p); delnode(pf);
			pn->subop = (pn->subop == JEQ) ? JBC : JBS;
			for(cp1=p->code; *cp1++!=',';);
			while (*cp1++= *cp2++);
			pn->code = p->code; pn->pop = NULL;
			uses[r] = NULL;
			nbj++;
			p = pn;
			goto std;
		}
		if ((shcnt = getnum(&regs[RT1][1])) < 1 || shcnt > 2) goto std;
		if ((shfrom = isreg(regs[RT2])) >= 0)
			regfrom = copy(regs[RT2]);
		if (tempreg(regs[RT3],shto))
		{
			int	regnum;

			if (uses[shto] != (pf = p->forw)) goto ashadd;
			if (pf->op != MOVA && pf->op != PUSHA) goto ashadd;
			if (pf->subop != BYTE) goto ashadd;
			splitrand(pf);
			if (!indexa(regs[RT1])) goto std;
			cp2 = regs[RT1];
			if(!isstatic(cp2)) goto std;
			while (*cp2++ != '[') ;
			if (*cp2++ != 'r' || !isdigit(*cp2)) goto std;
			regnum = *cp2++ - '0';
			if (isdigit(*cp2))
			{
				if (cp2[1] != ']') goto std;
				regnum *= 10; regnum += *cp2 - '0';
			}
			if (regnum != shto) goto std;
			if (shfrom >= 0)	/* shll $N,r*,r0 */
			{
				delnode(p);
				p = pf;
				if (shfrom != shto)
				{
					uses[shto] = NULL; splitrand(pf);
					cp2=regs[RT1]; while (*cp2++!='[');
					cp1=regfrom; while (*cp2++= *cp1++);
					cp2[-1] = ']'; *cp2 = 0;
					newcode(pf);
				}
			}
			else
			{
				p->op = MOV; splitrand(p);
				strcpy(regs[RT1], regs[RT2]);
				strcpy(regs[RT2], regs[RT3]);
				regs[RT3][0] = '\0';
				p->pop = 0; newcode(p);
			}
			switch (shcnt)
			{
			case 1:	pf->subop = WORD; break;
			case 2:	pf->subop = LONG; break;
			}
			redunm++; nsaddr++;
		}
		goto std;
ashadd:
		/* at this point, RT2 and RT3 are guaranteed to be simple regs*/
		if (shcnt == 1) {
			/*
			** quickie:
			**	shll	$1,A,A	>	addl2	A,A
			**	shll	$1,A,B	>	addl3	A,A,B
			*/
			p->op = ADD;
			strcpy(regs[RT1], regs[RT2]);
			if(equstr(regs[RT2], regs[RT3])) {
				p->subop = U(LONG,OP2);
				regs[RT3][0] = '\0';
			} else
				p->subop = U(LONG,OP3);
			p->pop = 0;
			newcode(p);
		}
		goto std;
	}

	case SHAR:
	case SHR:
	{
		/* bit tests:
		**	shrl	A,B,rC     \
		**	bitl	$1,rC	    >	jbc	A,B,D
		**	jeql	D	   /
		*/
		register struct	node	*pf;	/* forward node */
		register struct	node	*pn;	/* next node (after pf) */
		register int	extreg;		/* reg extracted to */

		splitrand(p);
		if(isreg(regs[RT1]) < 0) goto std; /* alignment */
		if (!tempreg(regs[RT3],extreg)) goto std;
		if ((pf = p->forw)->op != BIT) goto std;
		if (uses[extreg] && uses[extreg] != pf) goto std;
		splitrand(pf);
		if (regs[RT1][0] != '$') goto std;
		if (getnum(&regs[RT1][1]) != 1) goto std;
		if (extreg != isreg(regs[RT2])) goto std;
		if ((pn = pf->forw)->op != CBR) goto std;
		if (pn->subop != JEQ && pn->subop != JNE) goto std;
		delnode(p); delnode(pf);
		pn->subop = (pn->subop == JEQ) ? JBC : JBS;
		for(cp1=p->code; *cp1++!=',';);
		while (*cp1!=',') cp1++; *cp1='\0';
		pn->code = p->code; pn->pop = NULL;
		uses[extreg] = NULL; nbj++;
		p = pn;
		goto std;
	}

	case AND:
	{
		/* unsigned conversion:
		**	cvtbl	A,B;	andl2	$255,B > movzbl	A,B
		**
		** also byte- and word-size fields:
		**	shrl	$(3-n)*8,A,B; andl2	$255,B	>	movzbl	n+A,B
		**	shrl	$(1-n)*16,A,B; andl2	$65535,B >	movzwl	n+A,B
		*/
		char src[C2_ASIZE];
		register int f;	/* field length */
		register struct	node	*pb = p->back;	/* backward node */

		if (p->subop != U(LONG,OP2))
			goto std;
		splitrand(p); cp1=regs[RT1];
		if (*cp1++!='$' || (f=getnum(cp1))!=0xff && f!=0xffff)
			goto std;
		f = f==0xff ? 8 : 16;
		if (pb->op!=CVT && pb->op!=MOVZ && pb->op!=SHAR && pb->op!=SHR)
			goto std;
		/* save source of ANDL in 'src' */
		strcpy(src, regs[RT2]);
		splitrand(pb);
		if (!equstr(src,lastrand))
			goto std;
		if (pb->op==CVT || pb->op==MOVZ) {
			if (!(bitsize[pb->subop&0xF]==f
			  && bitsize[pb->subop>>4]>=f)) /* good CVT */
				goto std;
			strcpy(src, regs[RT1]);
		} else {
			register int	boff;	/* bit offset */

			if (regs[RT1][0] != '$') goto std;
			if ((boff = getnum(&regs[RT1][1])) < 0) goto std;
			if (isreg(regs[RT2])>=0 || !natural(regs[RT2])) goto std;
			if ((boff & (f-1)) != 0) goto std;
			boff = (32-boff-f) / 8;
			if (boff == 0)
				strcpy(src, regs[RT2]);
			else
				sprintf(src, "%d%s%s", boff, regs[RT2][0]=='(' ? "":"+",
					regs[RT2]);
		}
		delnode(pb);
		p->op = MOVZ;
		p->subop = U((f==8 ? BYTE : WORD), LONG);
		sprintf(line,"%s,%s",src,lastrand);
		p->pop=0;
		p->code = copy(line);
		goto std;
	}

	case CMP:
	{
		/* comparison to -63 to -1:
		**	cmpl	r0,$-1	>	incl	r0
		**	jeql	...
		**
		**	cmpl	r0,$-63	>	addl2	$63,r0
		**	jeql	...
		*/
		register int	num;
		register int	reg;
		register struct	node	*regp = p->back;

		if (p->forw->op != CBR) goto std;
		if (p->forw->subop != JEQ && p->forw->subop != JNE) goto std;
		splitrand(p);
		if (strncmp(regs[RT2], "$-", 2) != 0) goto std;
		reg = r = isreg(regs[RT1]);
		if (r < 0) goto std;
		if (r < NUSE && uses[r] != 0) goto std;
		if (r >= NUSE && regp->op == MOV && p->subop == regp->subop)
		{
			if (*regp->code != 'r') goto std;
			reg = regp->code[1] - '0';
			if (isdigit(regp->code[2]) || reg >= NUSE || uses[reg])
				goto std;
		}
		if (r >= NUSE) goto std;
		if (reg != r)
			sprintf(regs[RT1], "r%d", reg);
		if ((num = getnum(&regs[RT2][2])) <= 0 || num > 63) goto std;
		if (num == 1)
		{
			p->op = INC; regs[RT2][0] = '\0';
		}
		else
		{
			register char	*t;

			t=regs[RT1];regs[RT1]=regs[RT2];regs[RT2]=t;
			p->op = ADD; p->subop = U(p->subop, OP2);
			for (t = &regs[RT1][2]; t[-1] = *t; t++) ;
		}
		p->pop = 0; newcode(p);
		goto std;
	}

	case JBR: case JMP:
		clearuse();
		if ((p->subop&0xF)==RET) {
			switch((p->subop>>4)&0xF) {
				case 2: uses[1]=p; regs[1][0]= -1;
				case 1: uses[0]=p; regs[0][0]= -1;
			}
			break;
		}
		if (p->ref==0) goto std;	/* jmp (r0) */
		/* fall through */
	case CBR:
		if (p->ref->ref!=0) {
			for (r=NUSE;--r>=0;)
				if ((1<<r) & (int)p->ref->ref) {uses[r]=p; regs[r][0]= -1;}
			if ((1<<NUSE) & (int)p->ref->ref) useacc=p;
		}
		break;
	case LNF:
		/* lnf a; addf b ==> ldf b; subf a */
		{ register struct node	*pf = p->forw;
		if(pf->op==ADDF && p->subop==pf->subop) {
			p->op = LDF;
			p->pop = 0;
			pf->op = SUBF;
			pf->pop = 0;
			cp1 = p->code;
			p->code = pf->code;
			pf->code = cp1;
			p = pf->forw;
			break;
		}}
	case LDF: case LDFD: case CVLF: /* destroy acc */
		useacc = 0;
		goto std;
	case STF:
		{ register struct node	*pf;
		if((pf=flops(p)) != p) {
			p = pf; /* usually p->forw; */
			break;
		}}
		if(ldmov(p)) {
			p = p->forw;
			break;
		}
		if(useacc == 0)
			useacc = p;
		goto std;
	case ADDF: case MULF: /* commutatives - create clients for flops */
		/* stf a; ldf b; addf a => stf a; ldf a; addf b */
		{ register struct node	*pb = p->back;
		register struct node	*pbb = pb->back;
		if(pb->op==LDF && pb->subop==p->subop &&
		 pbb && pbb->op==STF && pbb->subop==p->subop &&
		 equstr(pbb->code, p->code)) {
			cp1 = p->code;
			p->code = pb->code;
			pb->code = cp1;
		}}
	/* use acc and regs */
	case CMPF: case CVFL: case SUBF: case DIVF:
		if(useacc == 0)
			useacc = p;
		goto std;
	case TSTF: 
		break;
	case PUSHD:
		if(ldmov(p)) {
			p = p->forw;
			break;
		}
	case CVDF: case NEGF: /* use only acc */
	case SINF: case COSF: case ATANF: case LOGF: case SQRTF: case EXPF:
		if(useacc == 0)
			useacc = p;
	case EROU: case JSW:
	case TEXT: case DATA: case BSS: case ALIGN: case WGEN: case END: ;
	}
	}
	for (p= &first; p!=0; p=p->forw)
		if (p->op==LABEL || p->op==DLABEL) p->ref=0;	/* erase our tracks */
}

char *
byondrd(p) register struct node *p; {
/* return pointer to register which is "beyond last read/modify operand" */
	if (has2ops(p)) return(regs[RT3]);
	switch (p->op) {
		case MFPR:
		case PUSHA:
		case TST: case INC: case DEC: case PUSH:
		case LDF: case LNF: case CVLF: case LDFD:
		case ADDF: case SUBF: case MULF: case DIVF:
		case CMPF:
			return(regs[RT2]);
		case MTPR:
#ifndef EMOD
		case EDIV:
#endif EMOD
		case CBR: /* must be JBC/JBS */
		case BIT: case CMP: case CALLS:	case CALLF:
		case CMPF2:
			return(regs[RT3]);
		case EMUL:
		case PROBE:
		case MOVBLK:
		case CASE: 
			return(regs[RT4]);
	}
	return(lastrand);
}

struct node *
bflow(p)
	register struct node *p;
{
	register char *cp1,*cp2,**preg;
	register int r, fr, dblflg=0;
	int flow= -1;
	struct node *olduse=0, *olduse1=0;

	if (p->subop==QUAD || p->subop==DOUBLE || (p->subop&0xF0)==DOUBLE<<4)
		dblflg |= 1;	/* double dest */
	if ((p->subop&0xF)==DOUBLE || p->subop==QUAD)
		dblflg |= 2;	/* double src */
	splitrand(p);
	if (p->op!=PUSH &&
#ifndef EMOD
	p->op!=EDIV &&
#endif EMOD
	p->op!=EMUL &&
	p->subop && tempreg(lastrand,r) && uses[r]==p->forw) {
	if (equtype(p->subop,regs[r][0]) ||
	    ((p->op==CVT || p->op==MOVZ || p->op==CVFL) &&
	     (regs[r][0]&0xf) && compat((p->subop>>4)&0xf,regs[r][0])) ||
	    p->op==MOVA && compat(LONG, regs[r][0])) {
		register int r2;

		if (regs[r][1]!=0) {	/* send directly to destination */
			if (p->op==INC || p->op==DEC) {
				p->op = (p->op==DEC) ? SUB : ADD;
				/* use 2 now, convert to 3 later */
				p->subop=(OP2<<4)+(p->subop&0xF);
				p->pop=0;
				cp1=lastrand; cp2=regs[RT2];
				while (*cp2++= *cp1++)	/* copy reg */
					;
				cp1=lastrand; *cp1++='$'; *cp1++='1'; *cp1=0;
			}
			cp1=regs[r]+1; cp2=lastrand;
			if (has2ops(p)) {
				/* use 3 operand form of instruction */
				p->pop=0;
				p->subop += (OP3-OP2)<<4;
				lastrand = cp2 = regs[RT3];
			}
			while (*cp2++= *cp1++)
				;
			if (p->op==MOVA && p->forw->op==PUSH) {
				p->op=PUSHA;
				*regs[RT2]=0; p->pop=0;
			} else if ((p->op==MOV || p->op==CVT) &&
			    p->forw->op==PUSH) {
				p->op=PUSH; p->subop &= 0xF;
				*regs[RT2]=0; p->pop=0;
			}
			delnode(p->forw);
			if (tempreg(lastrand,r2))
				uses[r2]=uses[r], uses[r]=0;
			redun3(p);
			newcode(p); redunm++; flow=r;
		} else if (p->op==MOV) {	/* superfluous fetch */
			int nmatch;
			char src[C2_ASIZE];
	movit:
			for (cp2=src, cp1=regs[RT1]; *cp2++= *cp1++;)
				;
			splitrand(p->forw);
			if (p->forw->op != INC && p->forw->op != DEC)
				lastrand=byondrd(p->forw);
			nmatch=0;
			for (preg=regs+RT1;*preg!=lastrand;preg++)
				if (r==isreg(*preg)) {
					cp2= *preg; cp1=src;
					while (*cp2++= *cp1++)
						;
					++nmatch;
				}
			if (nmatch==1) {
				if (has2ops(p->forw) && equstr(src,regs[RT2])) {
					p->forw->pop=0;
					p->forw->subop += (OP3-OP2)<<4;
					cp1=regs[RT3];
					*cp1++ = 'r'; *cp1++ = r+'0'; *cp1=0;
				}
				delnode(p);
				p=p->forw;
				if (tempreg(src,r2))
					uses[r2]=uses[r], uses[r]=0;
				redun3(p);
				newcode(p); redunm++; flow=r;
			} else
				splitrand(p);
		}
	} else if (p->op==MOV && (p->forw->op==CVT || p->forw->op==MOVZ) &&
	    p->forw->subop&0xf && 	/* if base or index, then forget it */
	    compat(p->subop,p->forw->subop) && !indexa(cp1=regs[RT1]))
		goto movit;
	}
	/* adjust 'lastrand' past any 'read' or 'modify' operands. */
	lastrand=byondrd(p);
	/* a 'write' clobbers the register. */
	if (tempreg(lastrand,r) ||
	    (has2ops(p) && tempreg(regs[RT2],r) && uses[r]==0)) {
		/*
		 * Writing a dead register is useless,
		 * but watch side effects
		 */
		switch (p->op) {
#ifndef EMOD
		case EDIV:
#endif EMOD
		case EMUL:
		case AOBLEQ: case AOBLSS:
			break;
		default:
			/*
			 * If no direct uses, check for
			 * use of condition codes
			 */
			if (uses[r]==0 && ((dblflg&1)==0 || uses[r+1]==0)) {
				register struct node *q = p;

				while ((q = nonlab(q->forw))->op==JBR &&
				    q->subop==0)
					q=q->ref; /* cc unused, unchanged */
				if (q->op!=CBR && q->op!=ADDA && q->op!=SUBA) {
					/* ... and destroyed */
					preg=regs+RT1;
					while (cp1 = *preg++) {
						if (cp1==lastrand &&
						    p->op != CLR &&
						    p->op != CVFL) {
							redunm++;
							delnode(p);
							return(p->forw);
						}
						if (equstr(cp1,lastrand))
							break;
					}
				}
			}
			flow=r;
		}
	}
	if ((r=flow) >= 0) {
		olduse=uses[r], uses[r]=0;
		*(short *)(regs[r])=0;
		/* if r0 destroyed, dont keep r1 */
		if (dblflg&1) {
			olduse1=uses[++r], uses[r]=0;
			*(short *)(regs[r])=0;
		}
	}
	/* now look for 'read' or 'modify' (read & write) uses */
	preg=regs+RT1; 
	while (*(cp1= *preg++)) {
		/* check for  r  */
		if (lastrand!=cp1 && tempreg(cp1,r)) {
			int isunused;
			if (isunused=(uses[r]==0)) {
				uses[r]=p;
				cp2=regs[r]; *cp2++=p->subop;
				if ((p->op==SHAL || p->op==SHAR ||
				    p->op==SHL || p->op==SHR) &&
				    cp1==regs[RT1])
					cp2[-1] = BYTE;
				if (p->op==CBR && (p->subop==JBC || p->subop==JBS))
					cp2[-1] = LONG;
				if (p->op==MOVA && cp1==regs[RT2])
					cp2[-1]=LONG;
			}
			/* ediv/emod's 2nd operand is quad */
			if (((p->op==EDIV
#ifdef EMOD
			   || p->op==EMOD
#endif EMOD
			   ) && cp1==regs[RT2] || (dblflg&2)) &&
			   ++r<NUSE && uses[r]==0) {
				if (isunused)
					*cp2=0;
				uses[r]=p;
				cp2=regs[r]; *cp2++=p->subop;
				if (!isunused)
					*cp2=0;
			}
			if (!isunused)
				continue;
			if (p->op==MOV || p->op==PUSH || p->op==CVT ||
			    p->op==MOVZ || p->op==COM || p->op==NEG ||
			    p->op==STF) {
				if (p->op!=PUSH) {
					cp1=regs[RT2];
					if (tempreg(cp1,r)) {
						/*
						 * reincarnation!!
						 * (as in  addl2 r0,r1;
						 *  movl r1,r0;  ret)
						 */
						if (uses[r]==0)
							uses[r]=olduse;
						if ((dblflg&1) && uses[r+1]==0)
							uses[r+1]=olduse1;
					}
					if (p->op!=MOV)
						cp1=0;
				} else
					cp1="-(sp)";
				if (cp1)
					while (*cp2++= *cp1++)
						;
				else
					*cp2=0;
			} else
				*cp2=0;
			continue;
		}
		/* check for (r),[r] */
		do {
			if (*cp1=='(' || *cp1=='[') { /* get register number */
				char t;
				for (cp2= ++cp1; *++cp1!=')' && *cp1!=']';)
					;
				t= *cp1; *cp1=0;
				if (tempreg(cp2,r) &&
				    (uses[r]==0 || uses[r]==p)) {
					uses[r]=p;
					regs[r][0] =
					    (*--cp2=='[' ? OPX<<4 : OPB<<4);
				}
				*cp1=t;
			}
		} while (*++cp1);
	}
#ifdef MOVAFASTER
	/* pushax or movax possibility? */
	cp1=regs[RT1];
	if (*cp1++=='$' && isstatic(cp1)) {
		if (p->op==MOV && p->subop==LONG) {
			if (regs[RT1][1]=='L' && 0!=(p->labno=getnum(regs[RT1]+2))) {
				cp1=p->code; while (*cp1++!=','); p->code= --cp1;
			}
			p->op = MOVA; p->subop = BYTE; ++p->code; p->pop=0;
		} else if (p->op==PUSH && p->subop==LONG) {
			p->op = PUSHA; p->subop = BYTE; ++p->code; p->pop=0;
		} else if (p->op==ADD && p->subop==U(LONG,OP3)
				 && 0<=(r=isreg(regs[RT2]))) {
			cp1=cp2=p->code; ++cp1;
			do *cp2++= *cp1; while (*cp1++!=','); cp2[-1]='[';
			do *cp2++= *cp1; while (*cp1++!=','); cp2[-1]=']';
			if (!equstr(regs[RT3],"-(sp)")){ p->op = MOVA; p->subop = BYTE;}
			else {p->op = PUSHA; p->subop = BYTE; *cp2=0;}
			if (uses[r]==0) {uses[r]=p; regs[r][0]=OPX<<4;}
			p->pop=0;
		}
	}
#endif MOVAFASTER
	return (p);
}

/* try to eliminate STF's */
struct node *
flops(q)
register struct node *q;
{
	register struct node *p;
	register int r;

	if(q->op!=STF || !tempreg(q->code,r))
		return(q);
	if(uses[r]) {
		/* see if anyone destroys acc between us */
		for(p=q->forw; p!=uses[r]; p=p->forw)
			switch(p->op) {
			case LABEL:
			case LDF: case LNF: case CVLF: case LDFD:
			case CVDF: case NEGF: case ADDF: case SUBF:
			case MULF: case DIVF: case SINF: case COSF:
			case ATANF: case LOGF: case SQRTF: case EXPF:
				return(q);
			}
		
		if(q->subop == p->subop)
			switch(p->op) {	/* do it in the accumulator */
			case LDF:	/* redundant load */
				delnode(p); nld++;
				p = p->forw;
				break;
			case LNF: /* stf r; lnf r ==> negf */
				p->op = NEGF;
				p->pop = 0;
				p->code = 0;
				break;
			case CMPF2: /* stf r; cmpf2 r,x ==> cmpf x */
				{ register char *s;
				register struct node *p1=p->forw;
				for(s=p->code; *s!=','; s++);
				*s = 0;
				if(isreg(p->code) == r)
					p->code = s+1;
				else {
					if(p1->op != CBR || isreg(s+1) != r) {
						*s = ',';
						return(q);
					}
					if(p1->subop > JNE) {
						p1->subop ^= 1;
						p1->pop = 0;
						nrevbr++;
					}
				}
				p->op = CMPF;
				p->pop = 0;
				}
				break;
			default:
				return(q);
			}
		else if(p->subop==LONG) {
			switch(p->op) {
			case TST:	/* stf r; tstl r ==> tstf */
				p->op = TSTF;
				p->code = 0;
				break;
			/* send directly to destination */
			case MOV:	/* stf r; movl r,x ==> stf x */
			case PUSH:	/* stf r; pushl r ==> stf -(sp)/pushd */
				if(q->subop == DOUBLE) {
					register struct node *b = p->back;
					/* assume b's 2nd arg is ok */
					if(!(b==uses[r+1] && b->op==p->op && b->subop==LONG))
						return(q);
					delnode(b); redunm++;
				}
				if(p->op==PUSH) {
					if(q->subop == DOUBLE) {
						p->op = PUSHD;
						p->code = 0;
					} else {
						p->op = q->op;
						p->code = copy("-(sp)");
					}
				} else {
					p->op = q->op;
					while(*p->code++ != ',');
				}
				break;
			default:
				return(q);
			}
			p->pop = 0;
			p->subop = q->subop;
		} else
			return(q);
		uses[r] = 0;
		if(q->subop == DOUBLE)
			uses[r+1] = 0;
		{ /* undo any effect on uses in the area between p and q,
		   * as we are going over it again */
			register struct node *b;
			for(b=p; b!=q; b=b->back) {
				for(r=0; r<NUSE; r++) {
					if(uses[r] == b)
						uses[r] = 0;
					if(useacc == b)
						useacc = 0;
				}
			}
		}
		return(p->forw); /* make p the next for bflow */
	}
	/* it's a store to reg which isnt used elsewhere */
	if((p=q->forw)->op == CBR) {
		q->op = TSTF;
		q->pop = 0;
		q->code = 0;
	} else {
		delnode(q); nst++;
		if(p->op ==STF || p->op==TSTF || p->op==PUSHD) {
			if(useacc == p)
				useacc = 0;
			return(p->forw);	/* so ldmov can be used on p */
		}
	}
	return(p);
}

/* try to change load/store sequences to movl */
ldmov(q)
register struct node *q;
{
	register struct node *p;
	register char *s, *pcod, *cp;
	char *dlsw();

	p = q->back;
	if(!(useacc==0 && (q->op==STF || q->op==TSTF || q->op==PUSHD)
	 && ((p->op==LDF && p->subop==q->subop) || (p->op==LDFD && q->subop==DOUBLE))))
		return(0);
	pcod = p->code;
 	cp = p->code;
	/* prepare args for movl/pushl */
	if(q->op!=TSTF && q->subop==DOUBLE) {
		if(p->op == LDF) {
			if((s = dlsw(p->code)) == NULL)
				return(0);

			strcpy(line, s);
			if(q->op == STF) {
				strcat(line, ",");
				if((s = dlsw(q->code)) == NULL)
					return(0);
				strcat(line, s);
				p->op = MOV;
			} else
				p->op = PUSH;
		} else { /* LDFD */
			if(q->op == STF) {
				if((s = dlsw(q->code)) == NULL)
					return(0);
			} else
				s = "-(sp)";
			strcpy(line, s);
			p->op = CLR;
		}
		p->pop = 0;
		p->subop = LONG;
		p->code = copy(line);
	} else
		{
 		if ((p->op == LDF) && (p->subop == DOUBLE) &&
 	           (indexa(cp)))  return(0);
		delnode(p);
		}
	strcpy(line, pcod);
	if(q->op == STF) {	/* ldf x; stf y ==> movl x,y */
		strcat(line, ",");
		strcat(line, q->code);
		q->op = MOV;
		nst++;
	} else if(q->op == TSTF)	/* ldf x; tstf ==> tstl x */
		q->op = TST;
	else	/* ldd x; pushd ==> pushl x+4; pushl x */
		q->op = PUSH;
	q->pop = 0;
	q->subop = LONG;
	q->code = copy(line);
	nld++;
	return(1);
}

/* reconstruct the address of l.s.w. of a double operand */
char *
dlsw(d)
	register char *d;
{
	register char *s, *t, *c;
	register int r;
	static char lsw[C2_ASIZE];

	if(d[0] == '*' || d[0] == '$')
		return(NULL);
	if (((strncmp(d, "(r", 2)) == 0) && isdigit(d[2]))
		return(NULL);
	t = lsw;
	if((r=isreg(d)) >= 0)
		sprintf(t, "r%d", r+1);
	else {
		for(s=d; *s && *s!='('; *t++ = *s++)
			if(*s == '[')
				return(NULL);
		if(s!=d)
			*t++ = '+';
		*t++ = '4';
		while(*t++ = *s)
			if(*s++ == '[' )
			{
				return(NULL);
			}
	}
	return(lsw);
}
checkexpr(p)
register char *p;
{

	while(*p && *p != ','){
	if ((*p == '+' ) || (*p == '-'))
		return(1);
	*p++;
	}
	return(0);
}
