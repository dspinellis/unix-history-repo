subroutine  Buram(npts,mesh,fn,m,n,p,q,delk)
integer           npts,m,n;
real  mesh(npts),fn(npts),p(1),q(1),delk;
 
#    Buram is a double precision subroutine which finds a
#   a rational function which is the best approximation,
#   in the uniform or minimax sense, to a given discrete
#   function.  The rational function is represented as
#   the quotient of two polynomials each expanded in terms
#   of Tchebychev polynomials.  This routine is a shell
#   which in turn calls the routine  Burm1 with certain
#   default values for the initial approximation and  for
#   the stopping criteria.
 
#   Input:
#   npts   - the number of mesh points.
#   mesh   - the array of mesh points.
#   fn     - the array of function values.
#   m      - the degree of the desired numerator polynomial.
#   n      - the degree of the desired denominator polynomial.
 
#   Output:
#   p      - the array of coefficients for the numerator polynomial.
#   q      - the array of coefficients for the denominator polynomial.
#   delk   - the maximum error in the approximation.
 
#   Error States (asterisk indicates recoverable):
#   1  - invalid degree
#   2  - too few mesh points
#   3  - mesh is not strictly monotone
#   4* - approximation equals function
#   5* - no improvement in approximation
#   6* - reached 50 iterations
 
integer           nitr,maxitr,itol,Nerror,ier;
real  fnmax,fnmin;
logical           Smonor;
common  /dfccom/   nitr;
 
call Enter(1);
if (m<0  |  n<0)
    call Seterr(" Buram - invalid degree",23,1,2);
if (npts < m+n+2)
    call Seterr(" Buram - too few mesh points",28,2,2);
if (!(Smonor(mesh,npts,1)))
    call Seterr(" Buram - mesh is not strictly monotone",38,3,2);
 
#   Initialize the numerator and demoninator polynomials.
fnmax = fn(1);                    fnmin = fn(1);
do i=2,npts
    {
    if (fnmax < fn(i))
	fnmax = fn(i);
    else if (fn(i) < fnmin)
	fnmin = fn(i);
    }
call Setr(m+1,0.0e0,p);           p(1) = 0.5e0*(fnmax + fnmin);
call Setr(n+1,0.0e0,q);           q(1) = 1.0e0
delk = fnmax - p(1);              nitr = 0;
 
if (! (m==0 & n==0))
    {
    maxitr = 50;                  itol = 2;
    call  Burm1(npts,mesh,fn,maxitr,itol,m,n,p,q,delk);
    if (nerror(ier)!=0)
	{
	if (ier == 7)
	    call Newerr(" Buram - approximation equals function",39,4,1);
	else if (ier == 8)
	    call Newerr(" Buram - no improvement in approximation",40,5,1);
	else if (ier == 9)
	    call Newerr(" Buram - reached 50 iterations",30,6,1);
	else
	    call Eprint;
	}
    }
call Leave;
return
end
subroutine   Burm1(npts,mesh,fn,maxitr,itol,m,n,p,q,delk)
integer           npts,maxitr,itol,m,n;
real  mesh(npts),fn(npts),p(1),q(1),delk;
 
#    Burm1 is a double precision subroutine which finds a
#   a rational function which is the best approximation,
#   in the uniform or minimax sense, to a given discrete
#   function.  The rational function is represented as
#   the quotient of two polynomials each expanded in terms
#   of Tchebychev polynomials.  This routine starts from an
#   initial approximation and terminates for one of four
#   reasons: (1) the error curve equioscillates and the
#   alternating extrema match to ITOL digits, (2) the number
#   of iterations exceeds MAXITR, (3) the approximation
#   cannot be improved, or (4) the approximation is essentially
#   equal to the given discrete function.
 
#   Input:
#   npts   - the number of mesh points.
#   mesh   - the array of mesh points.
#   fn     - the array of function values.
#   maxitr - the maximum number of iterations.
#   itol   - the number of digits to which the extrema should match.
#   m      - the degree of the desired numerator polynomial.
#   n      - the degree of the desired denominator polynomial.
#   p      - the array of coefficients for the initial numerator.
#   q      - the array of coefficients for the initial denominator.
 
#   Output:
#   p      - the array of coefficients for the numerator polynomial.
#   q      - the array of coefficients for the denominator polynomial.
#   delk   - the maximum error in the approximation.
 
#   Error States (asterisk indicates recoverable):
#   1  - invalid degree
#   2  - too few mesh points
#   3  - mesh is not strictly monotone
#   4  - maxitr .lt. 0
#   5  - invalid accuracy request
#   6  - denominator is nonpositive
#   7* - approximation equals function
#   8* - no improvement in approximation
#   9* - reached maximum no. of iterations
 
integer           idig,Iflr,I1mach,Istkgt,npptr,nqptr,enptr,qkptr,iexptr;
real  R1mach,Float,qlrg;
logical           Smonor;
 
common  /cstak/   dstak(500)
long real  dstak
integer           istak(1000)
real              ws(1000)
 
equivalence dstak,istak
equivalence dstak,ws
call Enter(1);
if (m<0  |  n<0)
    call Seterr(" Burm1 - invalid degree",23,1,2);
if (npts < m+n+2)
    call Seterr(" Burm1 - too few mesh points",28,2,2);
if (!(Smonor(mesh,npts,1)))
    call Seterr(" Burm1 - mesh is not strictly monotone",38,3,2);
if (maxitr < 0)
    call Seterr(" Burm1 - maxitr .lt. 0",22,4,2);
idig = Iflr(R1mach(5)*Float(I1mach(11)));
if (itol < 1  |  idig < itol)
    call Seterr(" Burm1 - invalid accuracy request",36,5,2);
qlrg = Abs(q(1));
for (j=2, j<=n+1, j=j+1)
    if (qlrg < abs(q(j)))
        qlrg = abs(q(j));
if (qlrg == 0.e0)
    call Seterr(" Burm1 - denominator is nonpositive",35,6,2)
else
    {
    for (j=1, j<=n+1, j=j+1)
        q(j) = q(j)/qlrg;
    for (j=1, j<=m+1, j=j+1)
        p(j) = p(j)/qlrg;
    }
 
npptr  = Istkgt(m+1,3);
nqptr  = Istkgt(n+1,3);
enptr  = Istkgt(npts,3);
qkptr  = Istkgt(npts,3);
iexptr = Istkgt(npts,2);
 
call  B1rm1(npts,mesh,fn,maxitr,itol,m,n,p,q,delk,ws(npptr),ws(nqptr),
            ws(enptr),ws(qkptr),istak(iexptr));
 
call Leave;
return;
end
subroutine  B1rm1(npts,x,fn,maxitr,itol,m,n,p,q,delk,newp,newq,en,qk,iext)
integer           npts,maxitr,itol,m,n,iext(npts);
real  x(npts),fn(npts),p(1),q(1),delk,newp(1),newq(1),
                  en(npts),qk(npts);
 
integer           nitr,nex,imax,imin,ilrg,Lrgex ,Nerror,ier;
real  eps,bnd,R1mach,delnew;
common  /dfccom/  nitr;
 
eps = R1mach(4)*10.0e0**itol;
call Extrmr(npts,fn,nex,iext,imax,imin,ilrg);
bnd = Abs(fn(ilrg))*eps;
 
call  Enqk(npts,x,fn,m,n,p,q,qk,en);
do i=1,npts
    if (qk(i) <= 0.0e0)
        call Seterr(" Burm1 - denominator is nonpositive",35,6,2);
 
call Extrmr(npts,en,nex,iext,imax,imin,ilrg);
delk = Abs(en(ilrg));            delnew = delk;
call Movefr(m+1,p,newp);          call Movefr(n+1,q,newq);
 
for (nitr=0, nitr<maxitr, nitr=nitr+1)
    {
#   call Outpt3 (x,npts,p,q,delk,m,n,en,iext,nex)
    if (delk <= bnd)
        {
        call Seterr(" Burm1 - approximation equals function",39,7,1);
        return;
        }
    #   Test for optimal solution.
    if (Lrgex (npts,en,nex,iext,ilrg,itol) >= m+n+2)
        return;
 
    call  Lpstp(npts,x,fn,qk,delnew,m,n,newp,newq)
    if (Nerror(ier) != 0)
        call Erroff;
 
    call  Enqk(npts,x,fn,m,n,newp,newq,qk,en);
    call Extrmr(npts,en,nex,iext,imax,imin,ilrg);
    delnew = Abs(en(ilrg));
    if (delk <= delnew)
        {
        call Seterr(" Burm1 - no improvement in approximation",40,8,1);
        return;
        }
    call Movefr(m+1,newp,p);      call Movefr(n+1,newq,q);
    delk = delnew;
    }
call Seterr(" Burm1 - reached maximum no. of iterations",42,9,1);
 
return;
end
subroutine   Enqk( npts,X,fn,m,n,p,q,Qk,en)
integer           npts,m,n;
real  X(npts),fn(npts),p(1),q(1),Qk(npts),en(npts);
#
#   Subroutine  Enqk computes en & Qk.
#   en=error values at mesh points.
#   Qk=value of denominator polynomial at mesh points.
#
real  Tchbp,Pk;
  if (npts<=0 | m<0 | n<0)
    call seterr ("enQk-invalid dimension",22,1,2)
  do i=1,npts
   {
    Qk(i)=Tchbp(n,q,X(i),X(1),X(npts))
    if (Qk(i)==0.e0)
      call seterr ("enQk-divisor .eq. 0.",20,2,2)
    Pk=Tchbp(m,p,X(i),X(1),X(npts))
    en(i)=(fn(i)*Qk(i)-Pk)/Qk(i)
   }
  return
  end
integer function Lrgex (npts,en,nex,iext,ilrg,tol)
#
#    Function Lrgex  finds the no. of error extrema with magnitudes
#    within tolerance of magnitude of largest error.
#
  integer npts,nex,iext(nex),ilrg,j,k,L,tol
  real en(npts),hold
  if (npts<=0)
    call Seterr ("Lrgex -invalid dimension",24,1,2)
  if (nex<=0 | ilrg<=0)
    call Seterr ("Lrgex -invalid index",20,2,2)
  k=0
  do j=1,nex
   {
    L=iext(j)
    hold=Abs(en(ilrg))-Abs(en(L))
    if (hold<=10.**(-tol)*Abs(en(ilrg)))
      k=k+1
   }
  Lrgex =k
  return
  end
subroutine  Lpstp(npts,mesh,fn,Qk,delk,m,n,p,q)
integer           npts,m,n;
real  mesh(npts),fn(npts),Qk(npts),delk,p(1),q(1);
 
#    Lpstp defines the linear programming subproblem of the
#   Differential Correction algorithm.  It also provides
#   the interface to the general purpose linear programming
#   package.
 
#   Input:
#   npts   - the number of mesh points.
#   mesh   - the array of mesh points.
#   fn     - the array of function values.
#   Qk     - the array of current denominator values.
#   delk   - the current minimax error.
#   m      - the degree of the numerator polynomial.
#   n      - the degree of the denominator polynomial.
#   p      - the current numerator polynomial.
#   q      - the current denominator polynomial.
 
#   Output:
#   p      - the array of coefficients for the numerator polynomial.
#   q      - the array of coefficients for the denominator polynomial.
 
#   Error States (asterisk indicates fatal):
#   1* - invalid degree
#   2* - too few mesh points
#   3* - nonpositive delk
#   4  - no improvement in the lp subproblem
 
integer           aptr,bptr,cptr,xptr;
 
common  /cstak/   dstak(500)
long real  dstak
integer           istak(1000)
real              ws(1000)
 
equivalence dstak,istak
equivalence dstak,ws
 
call Enter(1);
if (m<0  |  n<0)
    call Seterr(" Lpstp - invalid degree",23,1,2);
if (npts < m+n+2)
    call Seterr(" Lpstp - too few mesh points",28,2,2);
 
aptr   = Istkgt((3*npts+1),3);
bptr   = Istkgt((2*(npts+n+1)),3);
cptr   = Istkgt((m+n+3),3);
xptr   = Istkgt((m+n+3),3);
 
call  L9stp(npts,mesh,fn,Qk,delk,m,n,p,q,ws(aptr),ws(bptr),
           ws(cptr),ws(xptr));
 
call Leave;
return;
end
subroutine  L9stp(npts,mesh,fn,Qk,delk,m,n,p,q,A,B,C,X)
integer           npts,m,n;
real  mesh(npts),fn(npts),Qk(npts),delk,p(1),q(1),
                  A(1),B(1),C(1),X(1);
 
integer           nptsc,mc,nc,i1,i2,i3,i4,mm,nn,Nerror,ierr;
real  ctx,ctxnew,qlrg,Float,R1mach;
external           Difmt;
common  /difcom/  nptsc,mc,nc,i1,i2,i3,i4;
 
 
nptsc = npts;                             mc = m;
nc = n;
i1 = npts;                                i2 = i1 + npts;
i3 = i2 + n + 1;                          i4 = i3 + n + 1;
mm = i4;                                  nn = m+n+3;
 
call Movefr(n+1,q,X);                     call Movefr(m+1,p,X(n+2));
X(nn) = 0.e0;
call Setr(i2,0.0e0,B);                    call Setr((i4-i2),-1.0e0,B(i2+1));
call Setr(nn,0.0e0,C);                    C(nn) = -1.0e0;
call Movefr(npts,mesh,A);                 call Movefr(npts,fn,A(npts+1));
call Movefr(npts,Qk,A(2*npts+1));
if (delk <= 0.0e0)
    call Seterr(" Lpstp - nonpositive delk",25,3,2)
A(3*npts+1) = delk;                       ctx = 0.0e0;
 
#   Solve the LP problem: max C(T)X subject to AX >= B.
#   The subroutine  Difmt derives the matrix A from
#   the data stored in the array A.
 
call Lpph2(A,mm,nn, Difmt,B,C,X,4*mm,ctxnew)
if (Nerror(ier) != 0)
    call Erroff;
if (ctx < ctxnew)
    {
    qlrg = 0.0e0;
    for (j=1, j<=n+1, j=j+1)
        if (qlrg < abs(X(j)))            qlrg = abs(X(j));
    for (j=1, j<=n+1, j=j+1)
        q(j) = X(j)/qlrg;
    i = 0;
    for (j=n+2, j<=m+n+2, j=j+1)
        {
        i = i+1;                          p(i) = X(j)/qlrg;
        }
    }
else
    call Seterr(" Lpstp - no improvement in the lp subproblem",44,4,1);
 
return;
end
subroutine  Difmt(inprod,A,mm,nn,irow,X,dinprd)
integer           mm,nn,irow;
real  A(1),X(nn),dinprd;
logical           inprod;
 
#    Difmt handles references by the LP routine to
#   the matrix for the linear programming subproblem.
 
integer           npts,m,n,i1,i2,i3,i4,irm1,irm2,irm3,zptr,
                  fnptr,qzptr,jp,maxmn;
real  fct,fdelk,delk,z,fn,qz,Tchbp;
common  /difcom/  npts,m,n,i1,i2,i3,i4;
 
call Enter(1);
if (mm != i4  |  nn ~= m+n+3)
    call Seterr(" Difmt - invalid dimension",26,1,2)
if (irow<0  | mm<irow)
    call Seterr(" Difmt - invalid index",22,2,2)
 
irm1 = irow - i1;
irm2 = irow - i2;
irm3 = irow - i3;
if (inprod  &  i2 < irow)
    {
    if (i3 < irow)
	dinprd = -X(irm3);
    else
	dinprd =  X(irm2);
    }
else if (i2 < irow)
   {
    call Setr(nn,0.0e0,X);
    if (i3 < irow)
	X(irm3) = -1.0e0;
    else
	X(irm2) =  1.0e0;
    }
else
    {
    if (i1 < irow)
	{
	fct = -1.0e0;		zptr = irm1;
	}
    else
	{
	fct =  1.0e0;		zptr = irow;
	}
    z     = A(zptr);
    fnptr = zptr+npts;		fn = A(fnptr);
    qzptr = fnptr+npts;		qz = A(qzptr);
    delk  = A(3*npts+1);	fdelk = fct*fn + delk;
    if ( inprod )
	dinprd = fdelk*Tchbp(n,X,z,A(1),A(npts)) -
		fct*Tchbp(m,X(n+2),z,A(1),A(npts)) + qz*X(nn);
    else
	{
	maxmn = Max0(m,n);
	call  Tchcf(z,A(1),A(npts),maxmn,X);
	for (j=m+1, 1<=j, j=j-1)
	    {
	    jp = j+n+1;		X(jp) = -fct*X(j);
	    }
	for (j=1, j<=n+1, j=j+1)
	    X(j) = fdelk*X(j);
	X(nn) = qz;
	}
    }
call Leave;
return;
end
subroutine  Tchcf (x,a,b,deg,XX)
#
#    Subroutine  Tchcf computes the deg+1 Tchebycheff
#    coefficients of the point x.
#
  integer deg,i
  real a,b,twoxx,x,XX(1)
  call enter(1)
  if (deg<0)
    call seterr (" Tchcf-invalid degree",21,1,2)
  XX(1)=1.e0
  if (deg>0)
    if (b<=a)
      call seterr (" Tchcf-invalid interval",23,2,2)
    else        #scale x to the interval (-1.e0,1.e0)
      XX(2)=2.e0*(x-(a+b)/2.e0)/(b-a)
  if (deg>1)
    twoxx=2.e0*XX(2)
    for (i=3,i<=deg+1,i=i+1)
      XX(i)=twoxx*XX(i-1)-XX(i-2)
  call leave
  return
  end
