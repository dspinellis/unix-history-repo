static char Sccsid[] = "ah.c @(#)ah.c	1.1	10/1/82 Berkeley ";
#include "apl.h"

ex_immed()
{
	register i;
	register struct item *p;
	register struct nlist *n;
	double f;
	char fname[64];			/* Array for filename */
	char *cp, *vfname();
	int *ip;

	i = *pcp++;
	switch(i) {

	default:
		error("immed B");

	case SCRIPT:
		if(protofile > 0)
			CLOSEF(protofile);
		protofile = 0;
		cp = vfname(fname);
		if(equal(cp, "off"))
			return;
		if((protofile = OPENF(cp, 1)) > 0){
			SEEKF(protofile, 0L, 2);  /* append to existing file */
			printf("[appending]\n");
		} else {
			/*
			 * create new file
			 */
			protofile = opn(cp, 0644);
			printf("[new file]\n");
		}
		WRITEF(protofile, "\t)script on\n", 12);
		return;

	case DEBUG:
		debug = ~debug;
		return;

	case DIGITS:
		i = topfix();
		if(i < 1 || i > 20)
			error("digits D");
		printf("was %d\n",thread.digits);
		thread.digits = i;
		return;
	case TRACE:
		funtrace = 1;
		return;

	case UNTRACE:
		funtrace = 0;
		return;

	case WRITE:
		funwrite(0);
		return;

	case DEL:
	case EDITF:
		*sp++ = sp[-1];	/* duplicate top of stack */
		funwrite(scr_file);
		funedit(scr_file, i);
		unlink(scr_file);
		return;


	case EDIT:
		funedit(0, i);
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
		printf("was %d\n",thread.iorg);
		thread.iorg = topfix();
		return;

	case WIDTH:
		i = topfix();
		if(i < 1)
			error("width D");
		printf("was %d\n",thread.width);
		thread.width = i;
		return;

	case READ:
		funread(0);
		return;

	case ERASE:
		p = sp[-1];
		sp--;
		erase(p);
		return;

	case CONTIN:
		i = opn("continue", 0644);
		wssave(i);
		printf(" continue");

	case OFF:
		term(0);

	case VARS:
		for(n=nlist; n->namep; n++)
			if(n->itemp && n->use == DA && n->namep[0] != 'L') {
				if(column+8 >= thread.width)
					printf("\n\t");
				printf(n->namep);
				putchar('\t');
			}
		putchar('\n');
		return;

	case FNS:
		for(n=nlist; n->namep; n++)
			if(n->use == DF || n->use == MF || n->use == NF) {
				if(column+8 >= thread.width)
					printf("\n\t");
				printf(n->namep);
				putchar('\t');
			}
		putchar('\n');
		return;

	case CODE:
		n = (struct nlist *)sp[-1];
		sp--;
		switch(n->use){
		default:
			error("not a fn");
		case NF:
		case MF:
		case DF:
			if(n->itemp == 0)
				funcomp(n);
			ip = (int *)n->itemp;
			for(i=0; i <= *ip; i++){
				printf(" [%d] ", i);
				dump(ip[i+1], 0);
			}
			putchar('\n');
		}
		return;

	case RESET:
		while(gsip)
			ex_ibr0();
		error("");

	case SICOM:
		tback(1);
		return;

	case CLEAR:
		clear();
		printf("clear ws\n");
		goto warp1;		/* four lines down, or so... */

	case LOAD:
		i = opn(vfname(fname), 0);
		clear();
		wsload(i);
		printf(" %s\n", fname);
		evLlx();	/* possible latent expr evaluation */
warp1:
		/*
		 * this garbage is necessary because clear()
		 * does a brk(&end), and the normal return & cleanup
		 * procedures are guaranteed to fail (miserably).
		 *		--jjb 1/78
		 */
		sp = stack;
		reset();

	case LIB:
		listdir();
		return;

	case COPY:
		if(gsip)
			error("si damage -- type ')reset'");
		wsload(opn(vfname(fname),0));
		printf(" copy %s\n", fname);
		return;

	case DROPC:
		cp = vfname(fname);
		if(unlink(cp) == -1)
			printf("[can't remove %s]\n", cp);
		return;

	case SAVE:
		i = opn(vfname(fname), 0644);
		wssave(i);
		printf(" saved %s\n", fname);
		return;

	case VSAVE:
		i = opn(vfname(fname), 0644);
		vsave(i);
		putchar('\n');
		return;


	case SHELL:
		ex_shell();
		return;

	case LIST:
		ex_list();
		return;

	case PRWS:
		ex_prws();
		return;

	}
}

char *
vfname(array)
char *array;
{
	register struct nlist *n;
	register char *p;

	n = (struct nlist *)sp[-1];
	sp--;
	if(n->type != LV)
		error("save B");
	p = n->namep;
	while(*array++ = *p++);
	return(n->namep);

}

/*
 * check for latent expr., and evaluate it if it is there:
 */
evLlx()
{
	register struct nlist *n;
	register struct item *p;

	if((n=nlook("Llx")) && n->itemp->type == CH && n->itemp->size){
		*sp++ = dupdat(n->itemp);
		ex_meps();
		p = sp[-1];
		if(p->type != EL && p->type != DU)
			ex_print();
		pop();
/*		error("");	*/
	}
}
