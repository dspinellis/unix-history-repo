/*	@(#)sin.c	4.1/4.2 	10/31/84 CCI-CPG  */


/*
 * Double-precision floating point sin/cos function.
 * Calls modf.
 * Pn coefficients are #3345 from Hart & etc.(18.79D).
 *
 * New version by Les Powers (3/26/85).
 */


static double twoopi	= 0.63661977236758134308;
static double piotwo	= 3.14159265358979323846264338327950288419716939937511/2.;
static double piotwols	= .572118872610983179676266255859012e-17;
static double piofour	= 3.14159265358979323846264338327950288419716939937511/4.;
static double threepio4	= 3.14159265358979323846264338327950288419716939937511*.75;


static double p0	=  .15707963267948966188272e1;
static double p1	= -.64596409750624619108547e0;
static double p2	=  .7969262624616543562977e-1;
static double p3	= -.468175413530264260121e-2;
static double p4	=  .1604411847068220716e-3;
static double p5	= -.359884300720869272e-5;
static double p6	=  .5692134872719023e-7;
static double p7	= -.66843217206396e-9;
static double p8	=  .587061098171e-11;

/*
 * Sn coefficients are #3043 from Hart & etc.(17.48D).
 * Cn coefficients are #3824 from Hart & etc.(19.45D).
 * The coefficients are multiplied by the appropriate power
 * of 4/pi using binary calculator with 40 digit arithmetic.
 * The following statements in comments indicate the
 * operations originally used to generate the coefficients.
 *
 * #define FP (4/3.14159265358979323846264338327950288419716939937511)
 * #define FS FP*FP
 *
 * static double s0	=  .785398163397448307014e0*FP;
 * static double s1	= -.80745512188280530192e-1*FP*FS;
 * static double s2	=  .2490394570188736117e-2*FP*FS*FS;
 * static double s3	= -.36576204158455695e-4*FP*FS*FS*FS;
 * static double s4	=  .313361621661904e-6*FP*FS*FS*FS*FS;
 * static double s5	= -.1757149292755e-8*FP*FS*FS*FS*FS*FS;
 * static double s6	=  .6877100349e-11*FP*FS*FS*FS*FS*FS*FS;
 *
 * static double c0	=  .99999999999999999996415e0;
 * static double c1	= -.30842513753404245242414e0*FS;
 * static double c2	=  .1585434424381541089754e-1*FS*FS;
 * static double c3	= -.32599188692668755044e-3*FS*FS*FS;
 * static double c4	=  .359086044588581953e-5*FS*FS*FS*FS;
 * static double c5	= -.2461136382637005e-7*FS*FS*FS*FS*FS;
 * static double c6	=  .11500497024263e-9*FS*FS*FS*FS*FS*FS;
 * static double c7	= -.38577620372e-12*FS*FS*FS*FS*FS*FS*FS;
 */

static double s0 = .9999999999999999966874625291130031549274;
static double s1 = -.1666666666666661475150697663344387409506;
static double s2 = .0083333333333200019685518812023464185712;
static double s3 = -.0001984126982840175394529286095367983768;
static double s4 = .0000027557313298885682473645051747116530;
static double s5 = -.0000000250507058263681718011320436448899;
static double s6 = .0000000001589413523004632819499530093565;

static double c0 = 0.99999999999999999996415;
static double c1 = -.4999999999999999928435829208978050890709;
static double c2 = .0416666666666664302573692446884954307432;
static double c3 = -.0013888888888858960434395194724966502603;
static double c4 = .0000248015872828994630247806807330993132;
static double c5 = -.0000002755731286569608222434728722627332;
static double c6 = .0000000020875555145677882874779379760684;
static double c7 = -.0000000000113521232057839395845871741059;


double
cos(arg)
double arg;
{
	double sinus();
	double s;
	union {
	  double d;
	  unsigned i;
	} u;
	if ( arg < 0 )
	  arg = -arg;
	u.d = arg;
	if ( u.i < 0x40490fdb ) {		/* arg < pi * 1/4 */
	  s = arg * arg;
	  return(c0+s*(c1+s*(c2+s*(c3+s*(c4+s*(c5+s*(c6+s*c7)))))));
	}
	if ( u.i < 0x4116cbe3 ) {		/* arg < pi * 3/4 */
	  arg = (piotwo - arg)+piotwols;
	  s = arg * arg;
	  return(arg*(s0+s*(s1+s*(s2+s*(s3+s*(s4+s*(s5+s*s6)))))));
	}
	return(sinus(arg, 1));
}

double
sin(arg)
double arg;
{
	double sinus();
	double s;
	union {
	  double d;
	  unsigned i;
	} u;
	if ( arg < 0 )
	  return( -sin(-arg) );
	u.d = arg;
	if ( u.i < 0x40490fdb ) {		/* arg < pi * 1/4 */
	  s = arg * arg;
	  return(arg*(s0+s*(s1+s*(s2+s*(s3+s*(s4+s*(s5+s*s6)))))));
	}
	if ( u.i < 0x4116cbe3 ) {		/* arg < pi * 3/4 */
	  arg = (piotwo - arg)+piotwols;
	  s = arg * arg;
	  return(c0+s*(c1+s*(c2+s*(c3+s*(c4+s*(c5+s*(c6+s*c7)))))));
	}
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
	if(x < 0) {
		x = -x;
		quad = quad + 2;
	}
	x = x * twoopi; /*underflow?*/
	if (x > 32764){
		y = modf(x,&e);
		e = e + quad;
		modf(0.25 * e,&f);
		quad = e - 4 * f;
	}else{
		k = x;
		y = x - k;
		quad = (quad + k) & 03;
	}
	if (quad & 01)
		y = 1-y;
	if (quad > 1)
		y = -y;

	ysq = y * y;
	temp1 = ((((((((p8*ysq+p7)*ysq+p6)*ysq+p5)*ysq+p4)*ysq+
		p3)*ysq+p2)*ysq+p1)*ysq+p0);
	return(temp1 * y);
}
