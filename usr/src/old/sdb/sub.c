static	char sccsid[] = "@(#)sub.c 4.2 8/17/82";
#include "head.h"
#include <a.out.h>
#include <stab.h>
#include "cdefs.h"
#include <stdio.h>
struct user u;

char *
readline(f)
FILE *f; {
	static char buff[128];
	
	register char *p;
	register int i;
	
	p = buff;
	do {
		if ((i = getc(f)) == EOF) {
			*p++ = '\004';
			*p = '\n';
		}
		else *p = i;
	} while (*p++ != '\n');
	
	return(buff);
}

char *
cpname(p, q)
char *p, *q; {
	while(varchar(*q) || number(*q))
		*p++ = *q++;
	*p = '\0';
	return(q);
}

char *
cpall(p, q)
char *p, *q; {
	while (*q != '\n') 
		*p++ = *q++;
	*p = '\0';
	return(q);
}

eqany(c, s)
char c, *s; {
	while(*s != '\0')
		if (c == *s++) return(1);
	return(0);
}

error(s)
char *s; {
	printf("%s\n", s);
}

char *
cpstr(p,q)
char *p, *q; {
	do {
		*p++ = *q++;
	} while (*q != '\0');
	*p = '\0';
}
L_INT
round(a,b)
REG L_INT a, b;
{
	REG L_INT w;
	w = (a/b)*b;
	IF a!=w THEN w += b; FI
	return(w);
}

/* error handling */

chkerr()
{
	IF errflg ORF mkfault
	THEN	error(errflg);
		longjmp(env, 0);
	FI
}

eqstr(s1, s2)
	REG STRING	s1, s2;
{
#ifndef FLEXNAMES
	REG STRING	 es1;
#endif
	if (s2 == (STRING) -1) return(0);
#ifndef FLEXNAMES
	es1 = s1+8;
#endif
	WHILE *s1++ == *s2
#ifndef FLEXNAMES
	DO IF *s2++ == 0 ORF s1>=es1
#else
	DO IF *s2++ == 0
#endif
	   THEN return(1);
	   FI
	OD
	return(0);
}

longseek(f, a)
L_INT a;
{
	return(lseek(f,(long) a,0) != -1);
}


/* descriptor format to length */
dtol(d)
char d; {
	switch(d) {
	
	case 'a':
	case 's':
		return(0);
		
	case 'b':
	case 'c':
		return(1);
		
	case 'h':
		return(2);
		
	case 'l':
	case 'f':
		return(4);

	case 'g':
		return(8);

	default:
		return(WORDSIZE);
	}
}

/*
 * checks equality of pattern pat with str,
 * assuming str is tructaed at length 8
 */
eqpat(pat, str)
char *pat, *str; {
#ifndef FLEXNAMES
	return(eqpatr(pat, str, 0));
#else
	return(eqpatr(pat, str));
#endif
}

#ifndef FLEXNAMES
eqpatr(pat, str, cnt)
#else
eqpatr(pat, str)
#endif
char *pat, *str; {
	register int i;
	register char p, s;
	
	p = pat[0];
	s = str[0];
#ifndef FLEXNAMES
	if (cnt == 8) return(1);
#endif
	if (p == '?') {
		if (s == '\0') return(0);
#ifndef FLEXNAMES
		return(eqpatr(pat+1, str+1, cnt+1));
#else
		return(eqpatr(pat+1, str+1));
#endif
	}
	if (p == '*') {
		if (pat[1] == '\0') return(1);
#ifndef FLEXNAMES
		for(i=1; i<8-cnt; i++) {
			if (eqpatr(pat+1, str+i, cnt+i)) return(1);
#else
		for(i=1; ; i++) {
			if (eqpatr(pat+1, str+i)) return(1);
#endif
			if (str[i] == '\0') return(0);
		}
#ifndef FLEXNAMES
		return(0);
#else
		/*NOTREACHED*/
#endif
	}
	if ((eqany(p, ".[->") || p == '\0') && s == '\0') return(1);
	if (p != s) return(0);
#ifndef FLEXNAMES
	return(eqpatr(pat+1, str+1, cnt+1));
#else
	return(eqpatr(pat+1, str+1));
#endif
}

/* gets indirect address for pointers and subscripts */
getindir(class, addr, type) 
u_char class;
ADDR addr; {
	if (ISARY(type)) return(addr);
	if (class == N_RSYM)
		return(*(ADDR *)(((ADDR) &u) + R0 + (WORDSIZE)*addr));
	return(getval(addr, 'd', DSP));
}

long
readint(p)
char **p; {
	int sign;

	if (**p == '-') {
		sign = -1;
		(*p)++;
	} else {
		sign = 1;
	}
	if (**p == '0') {
		(*p)++;
		if (**p == 'x' || **p == 'X') {
			(*p)++;
			return(sign * rint(p, 16, hexdigit, hexconv));
		}
		else return(sign * rint(p, 8, octdigit, octconv));
	}
	else return(sign * rint(p, 10, decdigit, decconv));
}

long
rint(p, base, digit, conv)
char **p;
int (*digit)(), (*conv)(); {
	long value;
	
	value = 0;
	while ((*digit)(**p)) value = base*value + (*conv)(*(*p)++);  
	return(value);
}

octdigit(c) 
char c; {
	return(c >= '0' && c <= '7');
}

octconv(c)
char c; {
	return(c - '0');
}

decdigit(c)
char c; {
	return(c >= '0' && c <= '9');
}

decconv(c)
char c; {
	return(c - '0');
}

hexdigit(c)
char c; {
	return((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
		(c >= 'A' && c <= 'F'));
}

hexconv(c)
char c; {
	if (c >= '0' && c <= '9') return(c - '0');
	if (c >= 'a' && c <= 'f') return(c - 'a' + 10);
	if (c >= 'A' && c <= 'F') return(c - 'A' + 10);
	error("hex conversion error");
	return(0);
}

/* decodes number, character or variable */
long
argvalue(p)
char *p; {
	register char ch;
	register long value;
	register ADDR j;
	char var[30];

	ch = *p;
	if (ch == '\'') {
		value = *(p+1);
	} else if ((ch >= '0' && ch <= '9') || ch == '-') {
		value = readint(&p);
	} else if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
		ch == '_') {
		cpname(var, p);
		j = varaddr(curproc()->pname, var);
		if (j == -1) {
			printf("Unknown variable: %s\n", argsp);
			return(-1);
		}
		value = getval(j, typetodesc(sl_type, 0)[0], DSP);
		do {
			p++;
		} while (varchar(*p) || number(*p));
	}
	return(value);
}

prhex(v) 
long v; {
	if (v < 0)  {
		v = -v;
		printf("-");
	}
	if (v <= 9)
		printf("%d", v);
	else
		printf("0x%x", v);
}

/* print hex number in field of length 12 */
prhex12(v) 
long v; {
	if (v >= -9 && v <= 9)
		printf("%-12d", v);
	else
		printf("0x%-12x", v);
}

/* print line number followed by offset */
prlnoff(procp, v)
struct proct *procp; ADDR v; {
	int lineno, diff;
	char *name;
	name = procp->pname;
	if (name[0] == '_') {
#ifndef FLEXNAMES
		printf("%.7s", name+1);
#else
		printf("%s", name+1);
#endif
		lineno = -1;
	} else {
#ifndef FLEXNAMES
		printf("%8s", name);
#else
		printf("%s", name);
#endif
		lineno = adrtolineno((ADDR) v);
	}
	if (lineno == -1)
		diff = v - procp->paddr;
	else {
		printf(":%d", lineno);
		diff = v - lnfaddr;  /* set by adrtolineno() */
	}
	if (diff) {
		printf("+");
		prhex(diff);
	}
}
