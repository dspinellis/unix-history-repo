#include "head.h"
#include <a.out.h>
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
		reset();
	FI
}

eqstr(s1, s2)
	REG STRING	s1, s2;
{
	REG STRING	 es1;
	if (s2 == (STRING) -1) return(0);
	es1 = s1+8;
	WHILE *s1++ == *s2
	DO IF *s2++ == 0 ORF s1>=es1
	   THEN return(1);
	   FI
	OD
	return(0);
}

longseek(f, a)
L_INT a;
{
#ifndef EDDT
	return(lseek(f,(long) a,0) != -1);
#endif
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
	return(eqpatr(pat, str, 0));
}

eqpatr(pat, str, cnt)
char *pat, *str; {
	register int i;
	register char p, s;
	
	p = pat[0];
	s = str[0];
	if (cnt == 8) return(1);
	if (p == '?') return(eqpatr(pat+1, str+1, cnt+1));
	if (p == '*') {
		for(i=1; i<8-cnt; i++) {
			if (eqpatr(pat+1, str+i, cnt+i)) return(1);
			if (str[i] == '\0') return(0);
		}
		return(0);
	}
	if ((eqany(p, ".[->") || p == '\0') && s == '\0') return(1);
	if (p != s) return(0);
	return(eqpatr(pat+1, str+1, cnt+1));
}

/* gets indirect address for pointers and subscripts */
getindir(class, addr, type) 
ADDR addr; {
	if (ISARY(type)) return(addr);
	if (class == N_RSYM)
		return(*(ADDR *)(((ADDR) &u) + R0 + (WORDSIZE)*addr));
	return(getval(addr, 'd'));
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
		value = getval(j, typetodesc(sl_type, 0)[0]);
		do {
			p++;
		} while (varchar(*p) || number(*p));
	}
	return(value);
}

