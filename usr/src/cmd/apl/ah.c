#include "apl.h"

ex_immed()
{
	register i;
	register struct item *p;
	register struct nlist *n;
	double f;

	i = *pcp++;
	switch(i) {

	default:
		error("immed B");
	
	case APL:
		setterm(0);
		return;

	case ASCII:
		setterm(1);
		return;

	case CSH:
		system("/bin/csh");
		return;

	case DEBUG:
		debug = ~debug;
		return;

	case DIGITS:
		i = topfix();
		if(i < 1 || i > 20)
			error("digits D");
		aprintf("was %d\n",thread.digits);
		thread.digits = i;
		return;

	case ED_IT:
		funedit(EDIT_ED);
		return;

	case EX_IT:
		funedit(EDIT_EX);
		return;

	case EX_VI:
		funedit(EDIT_VI);
		return;
	
	case FUZZ:
		i = topfix();
		if(i <= 0) {
			thread.fuzz = 0.;
			return;
		}
		f = i;
		thread.fuzz = exp(-f*2.3025851);
		return;

	case ORIGIN:
		aprintf("was %d\n",thread.iorg);
		thread.iorg = topfix();
		return;

	case WIDTH:
		i = topfix();
		if(i < 1)
			error("width D");
		aprintf("was %d\n",thread.width);
		thread.width = i;
		return;

	case READ:
		funload(0);
		return;

	case ERASE:
		p = sp[-1];
		sp--;
		erase(p);
		return;

	case CONTIN:
		if((i=creat("continue",0644)) < 0)
			error("cannot create");
		wssave(i);
		aprintf(" continue");

	case OFF:
		term();

	case VARS:
		for(n=nlist; n->namep; n++)
			if(n->itemp && n->use == DA) {
				if(column+8 >= thread.width)
					aprintf("\n\t");
				aprintf(n->namep);
				aputchar('\t');
			}
		aputchar('\n');
		return;

/*#ifdef SOMED*/
	case SYMBOLS:
	    {
	      int typkey, ii;
		for(n=nlist; n->namep; n++) {
			aputchar('\n'); aprintf(n->namep); aprintf(">\n  use>\t");
			prtype(n->use);
			aprintf("  type>\t");
			prtype(n->type);
			aprintf("  labl>\t%d\n", n->label);
			aprintf("  rank>\t%d\n", n->itemp->rank);
			aprintf("  type>\t"); prtype(n->itemp->type);
			aprintf("  size>\t%d\n", n->itemp->size);
			aprintf("  indx>\t%d\n", n->itemp->index);
			if(n->itemp->datap)
				aprintf("  ival>\t%d\n", (int)*n->itemp->datap);
			aprintf("  dims>\n");
			for(ii=0; ii<n->itemp->rank; ++ii)
				aprintf("   ;%d'>\t%d\n",ii,n->itemp->dim[ii]);
		}
	    }
/*#endif*/

	case FNS:
		for(n=nlist; n->namep; n++)
			if(n->use == DF || n->use == MF || n->use == NF) {
				if(column+8 >= thread.width)
					aprintf("\n\t");
				aprintf(n->namep);
				aputchar('\t');
			}
		aputchar('\n');
		return;

	case CLEAR:
		clear();
		aprintf("clear ws\n");
		break;

	case LIB:
		listdir();
		return;

	case LOAD:
		funload(2);
		break;

	case COPY:
		funload(1);
		return;

	case DROPC:
		i = 1;
		goto drcom;

	case SAVE:
		i = 0;
drcom:
		n = sp[-1];
		sp--;
		if(n->type != LV)
			error("save B");
		if(i) {
			unlink(n->namep);
			return;
		}
		i = creat(n->namep,0644);
		if(i < 0)
			error("cannot create");
		wssave(i);
		aputchar('\n');
		return;
	}
	/* special return for after clear */
	sp = stack;
	reset();
}

/*#ifdef SOMED*/

prtype(type)
{
int	typkey;

#define TYPCASE(type,print) case type: typkey = print; break;

	switch(type) {
	    default:
		aprintf("%d\n", type);
		return;
	    TYPCASE(DA,'da')
	    TYPCASE(CH,'dh')
	    TYPCASE(LV,'lv')
	    TYPCASE(QD,'qd')
	    TYPCASE(QQ,'qq')
	    TYPCASE(IN,'in')
	    TYPCASE(EL,'el')
	    TYPCASE(NF,'nf')
	    TYPCASE(MF,'mf')
	    TYPCASE(DF,'df')
	    TYPCASE(QC,'qc')
	}
	aputchar(typkey.c[0]); aputchar(typkey.c[1]); aputchar('\n');
	return;
}

/*#endif*/
