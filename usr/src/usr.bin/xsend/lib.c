#include "xmail.h"
MINT *x, *b, *one, *c64, *t45, *z, *q, *r, *two, *t15;
MINT *a[42];
setup(s) char *s;
{	int pf[2];
	strncpy(buf, s, 10);
	while(*s) *s++ = 0;
	pipe(pf);
	if(fork()==0)
	{
		close(0);
		close(1);
		dup(pf[0]);
		dup(pf[1]);
		execl("/usr/lib/makekey", "-", 0);
		execl("/lib/makekey", "-", 0);
		exit(1);
	}
	write(pf[1], buf, 10);
	wait((int *)NULL);
	if(read(pf[0], buf, 13) != 13)
	{	fprintf(stderr, "enroll: cannot generate key\n");
		exit(1);
	}
}
mkx()
{	int i, j;
	for(i=0; i<4; i++)
	{	mult(x, t15, x);
		*z->val = (short)(rand() ^ buf[i]);
		madd(x, z, x);
	}
	mdiv(x, b, q, x);
	for(;;)
	{	gcd(b, x, q);
		if(q->len == 1 && q->val[0] == 1)
			return;
		madd(x, one, x);
	}
}
mka()
{	int i, j;
	for(i=0; i<42; i++)
		a[i] = itom(1);
	for(i=j=0; i<42; i++, j++)
	{	*z->val = (short)rand()&00;
		mult(a[i], z, a[i]);
		mult(a[i], t45, a[i]);
		rpow(two, j, q);
		if( i%14 == 6) j++;
		madd(a[i], q, a[i]);
		mult(a[i], t15, a[i]);
		*z->val = (short)rand()&0777;
		madd(a[i], z, a[i]);
		mdiv(a[i], b, q, a[i]);
	}
	for(i=0; i<42; i++)
		mult(a[i], x, a[i]);
	for(i=0; i<42; i++)
	{	mdiv(a[i], b, q, a[i]);
	}
}
mkb()
{	int i, c;
	unsigned seed;
	seed = 123;
	for(i=0; i<13; i++)
		seed = seed*buf[i] + i;
	srand(seed);
	*b->val = 04 + (rand()&03);
	for(i=0; i<11; i++)
	{	*z->val = (buf[i+2] + rand()) & 077;
		mult(b, c64, b);
		madd(b, z, b);
	}
}
comminit()
{	int i;
	x = itom(0);
	b = itom(1);
	one = itom(1);
	two = itom(2);
	c64 = itom(64);
	t45 = itom(1);
	t15 = itom(1);
	rpow(two, 45, t45);
	rpow(two, 15, t15);
	z = itom(1);
	q = itom(1);
}
#ifndef debug
nout(a, fd) MINT *a; FILE *fd;
{
	fwrite(&a->len, sizeof(int), 1, fd);
	fwrite(a->val, sizeof(short), a->len, fd);
}
nin(a, fd) MINT *a; FILE *fd;
{
	xfree(a);
	fread(&a->len, sizeof(int), 1, fd);
	a->val = xalloc(a->len, "nin");
	fread(a->val, sizeof(short), a->len, fd);
}
#endif
xfatal(s) char *s;
{
	fprintf(stderr, "%s\n", s);
	exit(1);
}
