#include "head.h"
#include <a.out.h>
#include "cdefs.h"
struct user u;
BKPTR	bkpthead;
/* initialize frame pointers to top of call stack */

struct proct *
initframe() {
	argp = *(ADDR *) (((ADDR) &u) + AP);
	frame = *(ADDR *) (((ADDR) &u) + FP);
	callpc = *(ADDR *) (((ADDR) &u) + PC);
	if ((frame == 0) || (frame & 0xf0000000 != 0x70000000))
		return(badproc);
	return(adrtoproc(callpc++));  /* ++ because UNIX backs up instrs */
}


struct proct *
nextframe() {
	callpc = get(frame+16, DSP);
	argp = get(frame+8, DSP);
	frame = get(frame+12, DSP) & EVEN;
	if ((frame == 0) || (frame & 0xf0000000 != 0x70000000))
		return(badproc);
	return(adrtoproc(callpc-1));
}

/* print call frame */
prframe() {
	int narg;
	char *p;
	struct proct *procp;
	
	if ((procp = initframe()) == badproc) return;
	do {
		if (get(frame+12, DSP) == 0) return;
		p = procp->pname;
		if (p[0] == '_')
			printf("%.7s(", p+1);
		else
			printf("%.8s(", p);
		narg = get(argp, DSP);
		if (narg & ~0xff) narg = 0;
		while (narg) {
			printf("%d", get(argp+=4, DSP));
			if (--narg != 0) printf(",");
		}
		printf(")");
		if (procp->sfptr != badfile)
			printf("   [%s:%d]", adrtofilep(callpc-1)->sfilename,
				adrtolineno(callpc-1));
		printf("\n");
	} while ((procp = nextframe()) != badproc);
}

STRING		signals[] = {
		"",
		"hangup",
		"interrupt",
		"quit",
		"illegal instruction",
		"trace/BPT",
		"IOT",
		"EMT",
		"floating exception",
		"killed",
		"bus error",
		"memory fault",
		"bad system call",
		"broken pipe",
		"alarm call",
		"terminated",
};
INT		signo;

sigprint() {
	printf("%s", signals[signo]);
}


/* returns core image address for variable */
formaddr(proc, class, addr)
register char *proc;
register char class;
ADDR addr; {
if (debug) printf("formaddr(%s, %o, %d)\n", proc,class & 0377,addr);
	switch(class & STABMASK) {
	case N_RSYM:
		if (getframe(proc) < 0) return(-1);
	case N_GSYM:
	case N_SSYM:
	case N_STSYM:
		return(addr);
		
	case N_PSYM:
		if (getframe(proc) < 0) return(-1);
		return(argp+addr);
		
	case N_LSYM:
		if (getframe(proc) < 0) return(-1);
		return(frame+addr);

	default:
		printf("Bad class in formaddr: 0%o: (%d, %d)\n",
			class & 0377, proc, addr);
		return(0);
	}
}

/* sets frame pointers to procedure proc */
getframe(proc)
register char *proc; {
	register struct proct *procp, *fprocp;
	
	procp = findproc(proc);
	if (procp == badproc) {
		printf("%s: Bad procedure name\n", proc);
		return(-1);
	}
	for (fprocp = initframe(); fprocp != badproc; fprocp = nextframe()) {
		if (procp == fprocp) return(0);
	}
	printf("%s: Not an active procedure\n", proc);
	return(-1);
}

char class;

/* returns address of proc:var. Sets externals class and subflag */
ADDR
varaddr(proc, var)
char *proc, *var; {
	register struct proct *procp;
	register ADDR addr;
	char *p;
	int localflag;
	
	if (debug) printf("varaddr(%s,%s)\n", proc, var);
	localflag = 0;
	subflag = 0;
	procp = initframe();
	do {
		if (eqstr(proc, procp->pname)) goto found;
	} while ((procp=nextframe()) != badproc);
	localflag = 1;
found:
	if (eqany(var[0], ".->")) {
		class = N_GSYM;
		addr = integ;
	}
	else {
		if (localflag || slookup(var, adrtostoffset(callpc-1)) == -1) {
			if (globallookup(var,findfile(curfile)->stf_offset) == -1) {
				if (localflag)
					printf("%.8s not found\n", var);
				else
					printf("%.8s:%s not found\n", proc, var);
				return(-1);
			}
		}
	class = sl_class & STABMASK;
	addr = (class == N_LSYM) ? -sl_addr : sl_addr;
	addr = formaddr(proc, class, addr);
	}
	if (addr == -1) return(-1);
	for (p=var; *p; p++) {
		if (*p == '.' && *(p+1) != '\0') {
			if (class == N_RSYM) {
				error("Not with a register variable");
				return(-1);
			}
			p++;
			if (localflag || slookup(p, adrtostoffset(callpc-1)) == -1) {
				if (globallookup(p, findfile(curfile)->stf_offset) == -1) {	
					if (localflag)
						printf("%s not found\n", var);
					else
						printf("%.8s:%s not found\n", proc, var);
					return(-1);
				}
			}
			if ((sl_class & STABMASK) != N_SSYM) {
				error("Not a structure element");
				return(-1);
			}
			addr += sl_addr;
			subflag = 0;
		}
		if (eqany(*p, "->") != '\0') {
			addr = getindir(class, addr, sl_type);
			class = N_GSYM;
			if (debug) printf("Address %d after getval\n", addr);
			for (; eqany(*p, "->"); p++) ;
			if (*p == '\0') break;
			if (localflag ||
				   slookup(p, adrtostoffset(callpc-1)) == -1) {
				if (globallookup(p, findfile(curfile)->stf_offset) == -1) {	
					if (localflag)
						printf("%s not found\n", var);
					else
						printf("%.8s:%s not found\n", proc, var);
					return(-1);
				}
			}
			addr += sl_addr;
			subflag = 0;
		}
		if (*p == '[' && *(p+1) != '\0') {
			long i;
			p++;
			i = readint(&p);
			if (debug) printf("Size %d\n", typetosize(sl_type, sl_size));
			addr = getindir(class, addr, sl_type);
			addr += typetosize(sl_type, sl_size)*i;  
			class = N_GSYM;
			subflag++;
		}
	}
	return(addr);
}

/* displays value of proc:var, returns its address */
ADDR
dispvar(proc, var, fmt)
char *proc, *var, *fmt; {
	ADDR addr;
	addr = varaddr(proc, var);
	if (addr == -1) return(-1);
	
	prvar(sl_type, addr, fmt, class, subflag);
	
	return(addr);
}

prvar(type, addr, fmt, class, subflag) 
ADDR addr;
char *fmt, class; short type; {

	dispf(addr, fmt, class, type, subflag);
}

prdebug() {
	register struct proct *procp;
	register struct filet *filep;
	
	printf("dot=%d\n", dot);
	printf("extstart = %d\n", extstart);
	for(filep=files;filep->sfilename[0];filep++)
		printf("%s offs %d @ %d flag %d addr %d\n", filep->sfilename, filep->stf_offset, filep, filep->lineflag, filep->faddr);
	for(procp=procs;procp->pname[0];procp++)
		printf("%8.8s addr %d; offs %d; sfptr %d; line %d\n",
			procp->pname, procp->paddr, procp->st_offset, procp->sfptr,
			procp->lineno);
}

/* display addr using format desc or class s */
char pd[] = "%x\n";
dispf(addr, desc, class, type, subflag)
char *desc; short type; ADDR addr; {
	int i;
	char *p;
	char dlen, dfmt;
	long value;
	union {
		struct {
			char c[WORDSIZE]; 
			};
		struct {
			int w;
		};
		struct {
			float f;
		}
	} word;
	union {
		struct{
			int w1, w2;
		};
		struct {
			double d;
		};
	} dbl;

	class &= STABMASK;
	if (desc[0]  == '\0') desc = typetodesc(type, subflag);
	odesc = desc;
	otype = type;
	oclass = class;
	if (debug) printf("dispf(%d,%s,0%o,0%o)\n", addr,desc,class,type);
	pd[1] = dfmt = 'd';
	dlen = '\0';
	for (p = desc; *p; p++) {
		switch (*p) {
			case 'l':
			case 'h':
			case 'b':
				dlen = *p;
				break;

			case 'c':
			case 'd':
			case 'o':
			case 'x':
			case 'u':
			case 's':
			case 'a':
			case 'f':
			case 'g':
				pd[1] = dfmt = *p;
				break;

			default:
				printf("Illegal descriptor: %c\n", *p);
				return;
			}
		}
		
		switch (dfmt) {
		default:
			if (class == N_RSYM) {
				if ((addr > 0 && addr < 6) || addr > 11) {
					printf("Bad register var %d\n", addr);
					return;
				}
				value = *(ADDR *)(((ADDR) &u) + R0 + (WORDSIZE)*addr);
			}
			else {
				value = getval(addr, dfmt);
			}

			switch (dfmt) {
			case 'u':
			case 'x':
			case 'o':
				switch (dlen) {
				case 'h':
					value = (unsigned short) value;
					break;
				case 'b':
					value = (unsigned char) value;
					break;
				case 'l':
					value = (unsigned long) value;
					break;
				}
				break;

			default:
				switch (dlen) {
				case 'h':
					value = (short) value;
					break;
				case 'b':
					value = (char) value;
					break;
				case 'l':
					value = (long) value;
					break;
				}
			}
			if (value > 0) {
				if (value > 9  &&  dfmt == 'x')
					printf("0x");
				else if (value > 7  &&  dfmt == 'o')
					printf("0");
			}
			if (dfmt == 'c') {
				if ((value & 0177) < ' ') 
					printf("^%c\n", value + ('A' - 1));
				else if ((value & 0177) == 0177)
					printf("^?\n");
				else
					printf(pd, value);
			} else
				printf(pd, value);
			return;
			
		case 'f':
			pd[1] = 'g';
			word.w = getval(addr,dfmt);
			printf(pd, word.f);
			return;

		case 'g':
			pd[1] = 'g';
			dbl.w1 = getval(addr, dfmt);
			dbl.w2 = getval(addr+WORDSIZE, dfmt);
			printf(pd, dbl.d);
			return;

		case 's':
			addr = getindir(class, addr, type);
				
		case 'a':
			for (;;) {
				word.w = getval(addr, 'd');
				for (i=0; i<WORDSIZE; i++) {
					if (word.c[i] == 0) goto l1;
					printf("%c", word.c[i]);
				}
				addr += WORDSIZE;
			}

	l1:	printf("\n");
		return;
	}
}

/* print breakpoints */
prbkpt() {
	register BKPTR bkptr;
	register int cnt;

	cnt = 0;
	
	for (bkptr = bkpthead; bkptr; bkptr=bkptr->nxtbkpt)
		if (bkptr->flag) {
			cnt++;
			printf("%.8s:%d\n", adrtoprocp(bkptr->loc)->pname,
				adrtolineno(bkptr->loc));
		}
	if (cnt == 0) 
		printf("No breakpoints set\n");
}

idbkpt() {
	register BKPTR bkptr;
	register int yesflg, cnt;
	register char c;
	
	cnt = 0;

	for (bkptr = bkpthead; bkptr; bkptr=bkptr->nxtbkpt)
		if (bkptr->flag) {
			printf("%.8s:%d ? ", adrtoprocp(bkptr->loc)->pname,
				adrtolineno(bkptr->loc));
			yesflg = 0;
			cnt++;
			do {
				c = getchar();
				if (c == 'y' || c == 'd') yesflg++;
			} while (c != '\n');
			if (yesflg)
				bkptr->flag = 0;
		}
	if (cnt == 0)
		printf("No breakpoints set\n");
}

char *
typetodesc(type, subflag)
short type; {
	register int ptr, ftn, ary;
	register char *desc;
	
	static char *typedesc[] = {
		"d",  /* undef */
		"d",  /* farg */
		"c",  /* char */
		"hd",  /* short */
		"d",  /* int */
		"ld",  /* long */
		"f",  /* float */
		"g",  /* double */
		"d",  /* strty */
		"d",  /* unionty */
		"d",  /* enumty */
		"d",  /* moety */
		"bu",  /* uchar */
		"hu",  /* ushort */
		"u",  /* unsigned */
		"lu",  /* ulong */
		"d"   /* ? */
	};
	
	ptr = ftn = ary = 0;
	
	for (;; type = DECREF(type)) {
		if (ISPTR(type)) ptr++;
		else if (ISFTN(type)) ftn++;
		else if (ISARY(type)) ary++;
		else {
			desc = typedesc[type];
			break;
		}
	}
	
	if ((ptr-subflag == 1  || ary-subflag == 1)  &&  desc[0] == 'c') return("s");
	if (debug) printf ("PTR %d; FTN %d; ARY %d; DESC %s\n",ptr,ftn,ary,desc);
	if (ptr) return("x");
	return(desc);
}

typetosize(type, stsize)
short type; {
	register int ptr, ftn, ary;
	register int size;
	
	static char typesize[] = {
		4,  /* undef */
		4,  /* farg */
		1,  /* char */
		2,  /* short */
		WORDSIZE,  /* int */
		4,  /* long */
		4,  /* float */
		8,  /* double */
		0,  /* strty */
		0,  /* unionty */
		4,  /* enumty */
		4,  /* moety */
		1,  /* uchar */
		2,  /* ushort */
		4,  /* unsigned */
		4,  /* ulong */
		4   /* ? */
	};
	
	ptr = ftn = ary = 0;
	
	for (;; type = DECREF(type)) {
		if (ISPTR(type)) ptr++;
		else if (ISFTN(type)) ftn++;
		else if (ISARY(type)) ary++;
		else {
			size = typesize[type];
			break;
		}
	}
	
	if (debug) printf ("PTR %d; FTN %d; ARY %d; SIZE %d; STSIZE %d\n",ptr,ftn,ary,size,stsize);
	if (ptr>1) return(4);
	if (size == 0) return(stsize ? stsize : 1);
	else return(size);
}
