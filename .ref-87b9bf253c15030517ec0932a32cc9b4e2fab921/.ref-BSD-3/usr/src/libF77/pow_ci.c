#include "complex"

pow_ci(p, a, b) 	/* p = a**b  */
complex *p, *a;
long int *b;
{
dcomplex p1, a1;

a1.dreal = a->real;
a1.dimag = a->imag;

pow_zi(&p1, &a1, b);

p->real = p1.dreal;
p->imag = p1.dimag;
}
