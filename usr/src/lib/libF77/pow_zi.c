#include "complex"

pow_zi(p, a, b) 	/* p = a**b  */
dcomplex *p, *a;
long int *b;
{
long int n;
double t;
dcomplex x;

n = *b;
p->dreal = 1;
p->dimag = 0;

if(n == 0)
	return;
if(n < 0)
	{
	n = -n;
	z_div(&x, a);
	}
else
	{
	x.dreal = a->dreal;
	x.dimag = a->dimag;
	}

for( ; ; )
	{
	if(n & 01)
		{
		t = p->dreal * x.dreal - p->dimag * x.dimag;
		p->dimag = p->dreal * x.dimag + p->dimag * x.dreal;
		p->dreal = t;
		}
	if(n >>= 1)
		{
		t = x.dreal * x.dreal - x.dimag * x.dimag;
		x.dimag = 2 * x.dreal * x.dimag;
		x.dreal = t;
		}
	else
		break;
	}
}
