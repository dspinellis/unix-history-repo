#include <stdio.h>

#define NP 1000
#define INF 1.e37

struct proj { int lbf,ubf; float a,b,lb,ub,quant,mult,val[NP]; } x,y;
float *diag, *r;
float dx = 1.;
float ni = 100.;
int n;
int auta;
int periodic;
float konst = 0.0;
float zero = 0.;

/* Spline fit technique
let x,y be vectors of abscissas and ordinates
    h   be vector of differences h9i8=x9i8-x9i-1988
    y"  be vector of 2nd derivs of approx function
If the points are numbered 0,1,2,...,n+1 then y" satisfies
(R W Hamming, Numerical Methods for Engineers and Scientists,
2nd Ed, p349ff)
	h9i8y"9i-1988+2(h9i8+h9i+18)y"9i8+h9i+18y"9i+18
	
	= 6[(y9i+18-y9i8)/h9i+18-(y9i8-y9i-18)/h9i8]   i=1,2,...,n

where y"908 = y"9n+18 = 0
This is a symmetric tridiagonal system of the form

	| a918 h928               |  |y"918|      |b918|
	| h928 a928 h938            |  |y"928|      |b928|
	|    h938 a938 h948         |  |y"938|  =   |b938|
	|         .           |  | .|      | .|
	|            .        |  | .|      | .|
It can be triangularized into
	| d918 h928               |  |y"918|      |r918|
	|    d928 h938            |  |y"928|      |r928|
	|       d938 h948         |  |y"938|  =   |r938|
	|          .          |  | .|      | .|
	|             .       |  | .|      | .|
where
	d918 = a918

	r908 = 0

	d9i8 = a9i8 - h9i8829/d9i-18	1<i<_n

	r9i8 = b9i8 - h9i8r9i-18/d9i-1i8	1<_i<_n

the back solution is
	y"9n8 = r9n8/d9n8

	y"9i8 = (r9i8-h9i+18y"9i+18)/d9i8	1<_i<n

superficially, d9i8 and r9i8 don't have to be stored for they can be
recalculated backward by the formulas

	d9i-18 = h9i8829/(a9i8-d9i8)	1<i<_n

	r9i-18 = (b9i8-r9i8)d9i-18/h9i8	1<i<_n

unhappily it turns out that the recursion forward for d
is quite strongly geometrically convergent--and is wildly
unstable going backward.
There's similar trouble with r, so the intermediate
results must be kept.

Note that n-1 in the program below plays the role of n+1 in the theory

Other boundary conditions_________________________

The boundary conditions are easily generalized to handle

	y908" = ky918", y9n+18"   = ky9n8"

for some constant k.  The above analysis was for k = 0;
k = 1 fits parabolas perfectly as well as stright lines;
k = 1/2 has been recommended as somehow pleasant.

All that is necessary is to add h918 to a918 and h9n+18 to a9n8.


Periodic case_____________

To do this, add 1 more row and column thus

	| a918 h928            h918 |  |y918"|     |b918|
	| h928 a928 h938            |  |y928"|     |b928|
	|    h938 a948 h948         |  |y938"|     |b938|
	|                     |  | .|  =  | .|
	|             .       |  | .|     | .|
	| h918            h908 a908 |  | .|     | .|

where h908=_ h9n+18

The same diagonalization procedure works, except for
the effect of the 2 corner elements.  Let s9i8 be the part
of the last element in the i8th9 "diagonalized" row that
arises from the extra top corner element.

		s918 = h918

		s9i8 = -s9i-18h9i8/d9i-18	2<_i<_n+1

After "diagonalizing", the lower corner element remains.
Call t9i8 the bottom element that appears in the i8th9 colomn
as the bottom element to its left is eliminated

		t918 = h918

		t9i8 = -t9i-18h9i8/d9i-18

Evidently t9i8 = s9i8.
Elimination along the bottom row
introduces further corrections to the bottom right element
and to the last element of the right hand side.
Call these corrections u and v.

	u918 = v918 = 0

	u9i8 = u9i-18-s9i-18*t9i-18/d9i-18

	v9i8 = v9i-18-r9i-18*t9i-18/d9i-18	2<_i<_n+1

The back solution is now obtained as follows

	y"9n+18 = (r9n+18+v9n+18)/(d9n+18+s9n+18+t9n+18+u9n+18)

	y"9i8 = (r9i8-h9i+18*y9i+18-s9i8*y9n+18)/d9i8	1<_i<_n

Interpolation in the interval x9i8<_x<_x9i+18 is by the formula

	y = y9i8x9+8 + y9i+18x9-8 -(h8299i+18/6)[y"9i8(x9+8-x9+8839)+y"9i+18(x9-8-x9-8839)]
where
	x9+8 = x9i+18-x

	x9-8 = x-x9i8
*/

float
rhs(i){
	int i_;
	double zz;
	i_ = i==n-1?0:i;
	zz = (y.val[i]-y.val[i-1])/(x.val[i]-x.val[i-1]);
	return(6*((y.val[i_+1]-y.val[i_])/(x.val[i+1]-x.val[i]) - zz));
}

spline(){
	float d,s,u,v,hi,hi1;
	float h;
	float D2yi,D2yi1,D2yn1,x0,x1,yy,a;
	int end;
	float corr;
	int i,j,m;
	if(n<3) return(0);
	if(periodic) konst = 0;
	d = 1;
	r[0] = 0;
	s = periodic?-1:0;
	for(i=0;++i<n-!periodic;){	/* triangularize */
		hi = x.val[i]-x.val[i-1];
		hi1 = i==n-1?x.val[1]-x.val[0]:
			x.val[i+1]-x.val[i];
		if(hi1*hi<=0) return(0);
		u = i==1?zero:u-s*s/d;
		v = i==1?zero:v-s*r[i-1]/d;
		r[i] = rhs(i)-hi*r[i-1]/d;
		s = -hi*s/d;
		a = 2*(hi+hi1);
		if(i==1) a += konst*hi;
		if(i==n-2) a += konst*hi1;
		diag[i] = d = i==1? a:
		    a - hi*hi/d; 
		}
	D2yi = D2yn1 = 0;
	for(i=n-!periodic;--i>=0;){	/* back substitute */
		end = i==n-1;
		hi1 = end?x.val[1]-x.val[0]:
			x.val[i+1]-x.val[i];
		D2yi1 = D2yi;
		if(i>0){
			hi = x.val[i]-x.val[i-1];
			corr = end?2*s+u:zero;
			D2yi = (end*v+r[i]-hi1*D2yi1-s*D2yn1)/
				(diag[i]+corr);
			if(end) D2yn1 = D2yi;
			if(i>1){
				a = 2*(hi+hi1);
				if(i==1) a += konst*hi;
				if(i==n-2) a += konst*hi1;
				d = diag[i-1];
				s = -s*d/hi; 
			}}
		else D2yi = D2yn1;
		if(!periodic) {
			if(i==0) D2yi = konst*D2yi1;
			if(i==n-2) D2yi1 = konst*D2yi;
			}
		if(end) continue;
		m = hi1>0?ni:-ni;
		m = 1.001*m*hi1/(x.ub-x.lb);
		if(m<=0) m = 1;
		h = hi1/m;
		for(j=m;j>0||i==0&&j==0;j--){	/* interpolate */
			x0 = (m-j)*h/hi1;
			x1 = j*h/hi1;
			yy = D2yi*(x0-x0*x0*x0)+D2yi1*(x1-x1*x1*x1);
			yy = y.val[i]*x0+y.val[i+1]*x1 -hi1*hi1*yy/6;
			printf("%f ",x.val[i]+j*h);
			printf("%f\n",yy);
			}
		}
	return(1);
	}
readin() {
	for(n=0;n<NP;n++){
		if(auta) x.val[n] = n*dx+x.lb;
		else if(!getfloat(&x.val[n])) break;
		if(!getfloat(&y.val[n])) break; } }

getfloat(p)
	float *p;{
	char buf[30];
	register c;
	int i;
	extern double atof();
	for(;;){
		c = getchar();
		if (c==EOF) {
			*buf = '\0';
			return(0);
		}
		*buf = c;
		switch(*buf){
			case ' ':
			case '\t':
			case '\n':
				continue;}
		break;}
	for(i=1;i<30;i++){
		c = getchar();
		if (c==EOF) {
			buf[i] = '\0';
			break;
		}
		buf[i] = c;
		if('0'<=c && c<='9') continue;
		switch(c) {
			case '.':
			case '+':
			case '-':
			case 'E':
			case 'e':
				continue;}
		break; }
	buf[i] = ' ';
	*p = atof(buf);
	return(1); }

getlim(p)
	struct proj *p; {
	int i;
	for(i=0;i<n;i++) {
		if(!p->lbf && p->lb>(p->val[i])) p->lb = p->val[i];
		if(!p->ubf && p->ub<(p->val[i])) p->ub = p->val[i]; }
	}


main(argc,argv)
	char *argv[];{
	extern char *malloc();
	int i;
	x.lbf = x.ubf = y.lbf = y.ubf = 0;
	x.lb = INF;
	x.ub = -INF;
	y.lb = INF;
	y.ub = -INF;
	while(--argc > 0) {
		argv++;
again:		switch(argv[0][0]) {
		case '-':
			argv[0]++;
			goto again;
		case 'a':
			auta = 1;
			numb(&dx,&argc,&argv);
			break;
		case 'k':
			numb(&konst,&argc,&argv);
			break;
		case 'n':
			numb(&ni,&argc,&argv);
			break;
		case 'p':
			periodic = 1;
			break;
		case 'x':
			if(!numb(&x.lb,&argc,&argv)) break;
			x.lbf = 1;
			if(!numb(&x.ub,&argc,&argv)) break;
			x.ubf = 1;
			break;
		default:
			fprintf(stderr, "Bad agrument\n");
			exit(1);
		}
	}
	if(auta&&!x.lbf) x.lb = 0;
	readin();
	getlim(&x);
	getlim(&y);
	i = (n+1)*sizeof(dx);
	diag = (float *)malloc((unsigned)i);
	r = (float *)malloc((unsigned)i);
	if(r==NULL||!spline()) for(i=0;i<n;i++){
		printf("%f ",x.val[i]);
		printf("%f\n",y.val[i]); }
}
numb(np,argcp,argvp)
	int *argcp;
	float *np;
	char ***argvp;{
	double atof();
	char c;
	if(*argcp<=1) return(0);
	c = (*argvp)[1][0];
	if(!('0'<=c&&c<='9' || c=='-' || c== '.' )) return(0);
	*np = atof((*argvp)[1]);
	(*argcp)--;
	(*argvp)++; 
	return(1); }

