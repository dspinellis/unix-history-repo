#include "head.h"

/*
 * These procedures manage the source files examined by sdb.
 */

/* Change the current source file to `name'. */
finit(name)
char *name; {
	register char *p, *q;
	
	if (fiobuf.fd) close(fiobuf.fd);
	q = name;
	for (p=fp; *q; *p++ = *q++) ;
	*p = 0;
	if ((fiobuf.fd = open(filework,0)) == -1) {
		nolines = 1;
		perror(filework);
		return;
	}
	binit(&fiobuf);
	cpstr(curfile, name);
	for (p=fbuf;;p++) {
		if (bread(&fiobuf,p,1) <= 0) {
			nolines = 1;
			printf("%s: No lines in file\n", name);
			return;
		}
		if (*p == '\n') break;
	}
	fline = 1;
	maxfline = 0;
	nolines = 0;
}

/* Make the next line current. */
fnext() {
	register char *p;
	
	if (nolines){
		return;
	}
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


/* Make the previous line current. */
fprev() {
	char c;
	register int i;
	
	if (nolines){
		return;
	}
	for(i=0; i<3; i++) {
		for (;;) {
			if (bread(&fiobuf, &c+1, -1) <= 0) {
				if (maxfline) blseek(&fiobuf,0L,2);
				else {
					blseek(&fiobuf,0L,0);
					for(;;) {
						if (bread(&fiobuf,&c,1)<=0)
							break;
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


/* Print the current line. */
fprint() {
	register char *p;
	
	if (nolines){
		error("No lines in file");
		return;
	}
	printf("%d: ", fline);
	p = fbuf;
	while(putchar(*p++) != '\n')
		;
}

/* Make line `num' current. */
ffind(num)
register int num; {
	register int i, ofline;
	
	if (nolines){
		return;
	}
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

/* Go back n lines. */
fback(n) {
	int i;
	
	if (nolines){
		return(0);
	}
	for (i=0; i<n; i++) {
		if (fline == 1) return(i);
		fprev();
	}
	return(i);
}

/* Go forwards n lines. */
fforward(n) {
	int i;
	
	if (nolines){
		return(0);
	}
	for (i=0; i<n; i++) {
		fnext();
		if (fline == 1) {
			fprev();
			return(i);
		}
	}
	return(i);
}

/* Print n lines. */
fprintn(n) {
	int i;
	
	if (nolines){
		error("No lines in file");
		return(0);
	}
	for (i=0; i<n; i++) {
		fprint();
		fnext();
		if (fline == 1) break;
	}
	fprev();
	return(i);
}
