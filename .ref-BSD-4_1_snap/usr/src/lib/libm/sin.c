/*
	C program for floating point sin/cos.
	Calls modf.
	There are no error exits.
	Coefficients are #3370 from Hart & Cheney (18.80D).
*/

static double twoopi	= 0.63661977236758134308;
static double p0	=  .1357884097877375669092680e8;
static double p1	= -.4942908100902844161158627e7;
static double p2	=  .4401030535375266501944918e6;
static double p3	= -.1384727249982452873054457e5;
static double p4	=  .1459688406665768722226959e3;
static double q0	=  .8644558652922534429915149e7;
static double q1	=  .4081792252343299749395779e6;
static double q2	=  .9463096101538208180571257e4;
static double q3	=  .1326534908786136358911494e3;

double
cos(arg)
double arg;
{
	double sinus();
	if(arg<0)
		arg = -arg;
	return(sinus(arg, 1));
}

double
sin(arg)
double arg;
{
	double sinus();
	return(sinus(arg, 0));
}

static double
sinus(arg, quad)
double arg;
int quad;
{
	double modf();
	double e, f;
	double ysq;
	double x,y;
	int k;
	double temp1, temp2;

	x = arg;
	if(x<0) {
		x = -x;
		quad = quad + 2;
	}
	x = x*twoopi;	/*underflow?*/
	if(x>32764){
		y = modf(x,&e);
		e = e + quad;
		modf(0.25*e,&f);
		quad = e - 4*f;
	}else{
		k = x;
		y = x - k;
		quad = (quad + k) & 03;
	}
	if (quad & 01)
		y = 1-y;
	if(quad > 1)
		y = -y;

	ysq = y*y;
	temp1 = ((((p4*ysq+p3)*ysq+p2)*ysq+p1)*ysq+p0)*y;
	temp2 = ((((ysq+q3)*ysq+q2)*ysq+q1)*ysq+q0);
	return(temp1/temp2);
}
