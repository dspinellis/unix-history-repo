#include "sh.h"

execute(t, pf1, pf2)
	int *t, *pf1, *pf2;
{
	int i, f, pv[2];
	register *t1;
	char *cp1, *cp2;
	char *scp;

	if (t == 0)
		return;
	switch(t[DTYP]) {
		case TCOM:
			i = 100;
			do
				if (func(t))
					return;
			while (--i && alias(&t[DCOM]));
			if (i == 0) {
				bferr2(t[DCOM], ": Alias loop detected");
				return;
			}
		case TPAR:
			t1 = t;
			f = t1[DFLG];
			i = 0;
			if ((f&FPAR) == 0)
				i = fork();
			if (i == -1) {
				err("No more processes");
				return;
			}
			if (i != 0) {
				if ((f&FPIN) != 0) {
					close(pf1[0]);
					close(pf1[1]);
				}
				if ((f&FPRS) != 0) {
					prn(i);
					prs("\n");
					set(pcs, putn(i));
				}
				if ((f&FAND) != 0)
					return;
				if ((f&FPOU) == 0)
					pwait(i);
				return;
			}
			set(prompt,"");
			unsetv(prompt);
			if ((cp1 = t1[DLEF]) != 0) {
				close(0);
				cp1 = globone(cp1);
				if (cp1 == 0)
					exit(1);
				strip(cp1);
				if (open(cp1, 0) < 0) {
					prs(cp1);
					err(": Cannot open");
					exit(1);
				}
			}
			if ((cp2 = t1[DRIT]) != 0) {
				while (*cp2)
					*cp2++ =& 0177;
				strip(cp2);
				cp2 = t1[DRIT];
				close(1);
				if ((f&FCAT) != 0 && open(cp2, 1) >= 0)
					seek(1, 0, 2);
				else if (creat(cp2, 0644) < 0) {
					prs(t1[DRIT]);
					err(": Cannot create");
					exit(1);
				}
			}
			if ((f&FPIN) != 0) {
				close(0);
				dup(pf1[0]);
				close(pf1[0]);
				close(pf1[1]);
			}
			if ((f&FPOU) != 0) {
				close(1);
				dup(pf2[1]);
				close(pf2[0]);
				close(pf2[1]);
			}
			if (f&FDIAG) {
				close(2);
				dup(1);
			}
			if ((f&FINT)!=0 && t1[DLEF]==0 && (f&FPIN)==0) {
				close(0);
				open("/dev/null", 0);
			}
			if ((f&FINT) == 0 && setintr) {
				signal(INTR, 0);
				signal(QUIT, 0);
			}
			if (t1[DTYP] == TPAR) {
				if (t1 = t1[DSPR])
					t1[DFLG] =| f&FINT;
				execute(t1);
				exit(1);
			}
			doexec(t1);
			/* no return */
		case TFIL:
			f = t[DFLG];
			pipe(pv);
			t1 = t[DLEF];
			t1[DFLG] =| FPOU | (f&(FPIN|FINT|FPRS|FDIAG));
			execute(t1, pf1, pv);
			t1 = t[DRIT];
			t1[DFLG] =| FPIN | (f&(FPOU|FINT|FAND|FPRS));
			execute(t1, pv, pf2);
			return;
		case TLST:
			f = t[DFLG]&FINT;
			if (t1 = t[DLEF])
				t1[DFLG] =| f;
			execute(t1);
			if (t1 = t[DRIT])
				t1[DFLG] =| f;
			execute(t1);
			return;
	}
}

strip(cp)
	register char *cp;
{

	while (*cp++ =& 0177)
		continue;
}
