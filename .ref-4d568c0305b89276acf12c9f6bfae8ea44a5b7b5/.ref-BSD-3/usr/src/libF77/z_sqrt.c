#include "complex"

z_sqrt(r, z)
dcomplex *r, *z;
{
double mag, sqrt(), cabs();

if( (mag = cabs(z->dreal, z->dimag)) == 0.)
	r->dreal = r->dimag = 0.;
else if(z->dreal > 0)
	{
	r->dreal = sqrt(0.5 * (mag + z->dreal) );
	r->dimag = z->dimag / r->dreal / 2;
	}
else
	{
	r->dimag = sqrt(0.5 * (mag - z->dreal) );
	if(z->dimag < 0)
		z->dimag = - z->dimag;
	r->dreal = z->dimag / r->dimag / 2;
	}
}
