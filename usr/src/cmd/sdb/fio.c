#include "head.h"

finit(name)
char *name; {
	register char *p, *q;
	
	if (fiobuf.fd) close(fiobuf.fd);
	q = name;
	for (p=fp; *q; *p++ = *q++) ;
	*p = 0;
	if ((fiobuf.fd = open(filework,0)) == -1) {
		perror(filework);
		return;
	}
	binit(&fiobuf);
	cpstr(curfile, name);
	for (p=fbuf;;p++) {
		if (bread(&fiobuf,p,1) <= 0) {
			printf("%s - No lines in file\n", name);
			return;
		}
		if (*p == '\n') break;
	}
	fline = 1;
	maxfline = 0;
}

fnext() {
	register char *p;
	
	for(p=fbuf;;p++) {
		if (bread(&fiobuf,p,1) <= 0) {
			p--;
			blseek(&fiobuf,0L,0);
			fline = 0;
			continue;
		}
		if (*p == '\n') break;
	}
	fline++;
}


fprev() {
	char c;
	register int i;
	
	for(i=0; i<3; i++) {
		for (;;) {
			if (bread(&fiobuf, &c+1, -1) <= 0) {
				if (maxfline) blseek(&fiobuf,0L,2);
				else {
					blseek(&fiobuf,0L,0);
					for(;;) {
						if (bread(&fiobuf,&c,1)<=0) break;
						if (c == '\n') maxfline++;
					}
				}
			}
			if (c == '\n') break;
		}
	}
	bread(&fiobuf, &c, 1);  /* eat the '\n' */
	
	fline -= 2;
	if (fline < 0) fline = maxfline - 1;
	
	fnext();
}


fprint() {
	register char *p;
	
	printf("%d: ", fline);
	p = fbuf;
	while(putchar(*p++) != '\n')
		;
}

ffind(num)
register int num; {
	register int i, ofline;
	
	ofline = fline;
	if (num>fline)
		for (i=fline; i<num; i++) {
			fnext();
			if (fline == 1) goto bad;
		}
	if (num<fline)
		for (i=num; i<ofline; i++) {
			fprev();
		}
		
	if (maxfline & num>maxfline) goto bad;
	
	return;

bad:	error("Not that many lines in file");
	ffind(ofline);
}

fback(n) {
	int i;
	
	for (i=0; i<n; i++) {
		if (fline == 1) return(i);
		fprev();
	}
	return(i);
}

fforward(n) {
	int i;
	
	for (i=0; i<n; i++) {
		fnext();
		if (fline == 1) {
			fprev();
			return(i);
		}
	}
	return(i);
}

fprintn(n) {
	int i;
	
	for (i=0; i<n; i++) {
		fprint();
		fnext();
		if (fline == 1) break;
	}
	fprev();
	return(i);
}
