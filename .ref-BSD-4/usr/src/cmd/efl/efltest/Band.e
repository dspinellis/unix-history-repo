  procedure bnds(n,ml,m,g,nb,b)
 
# to solve a*x = b, where a is a banded matrix, using gaussian
# elimination with partial pivoting.
 
# mnemonic - double precision band solution of a system of
#            linear algebraic equations.
 
# input -
 
#   n  - the order of the system.
#   ml - the number of nonzero elements of a on and below the diagonal.
#   m  - the total number of nonzero elements in each row of a.
#   g  - the matrix a, with g(i,j) = a(i,i+j-ml).
#   nb - the number of right-hand-sides b.
#   b  - the right-hand-sides.
 
# output -
 
#   g - has been clobbered.
#   b - the solution vectors, x.
 
# scratch space allocated - n*( (ml-1)*mu + 1 ) words.
 
# error states -
 
#   1 - n.lt.1.
#   2 - ml.lt.1.
#   3 - ml.gt.m.
#   4 - nb.lt.1.
#   5 - singular matrix. (recoverable)
  
  real g(n,m),b(n,nb)
  integer n,ml,m,nb
  
  include rstack
  
  integer il,iint,istkgt,nerror,nerr
  
# check the input for errors.
 
  if ( n < 1 ) { seterr(" bnds - n.lt.1",14,1,2) }
  if ( ml < 1 ) { seterr(" bnds - ml.lt.1",16,2,2) }
  if ( ml > m ) { seterr(" bnds - ml.gt.m",15,3,2) }
  if ( nb < 1 ) { seterr(" bnds - nb.lt.1",15,4,2) }
 
  enter(1)
  
  il = istkgt(max0(n*(ml-1),1),3) ; iint = istkgt(n,2)
  
  bndlu(n,ml,m,g,ws(il),is(iint))
  
  if ( nerror(nerr) == 0 ) { bndfb(n,ml,m,ws(il),g,is(iint),nb,b) }
 
  else { erroff() ; seterr(" bnds - singular matrix",23,5,1) }
  
  leave()
  
  return
  
  end
  procedure bndlu(n,ml,m,g,l,int)
 
# to obtain the lu decomposition of a banded matrix,
# using gaussian elimination with partial pivoting.
 
# mnemonic - double precision band lu decomposition.
 
# input -
 
#   n   - the order of the matrix.
#   ml  - the number of nonzero elements of a on and below the diagonal.
#   m   - the number of nonzero elements in each row of a.
#   g   - the matrix a, with g(i,j) = a(i,i+j-ml).
 
# output -
 
#   l   - the lower triangular banded factor of a.
#   g   - the upper triangular banded factor of a.
#   int - the row pivoting used.
 
# scratch storage allocated - none.
 
# error states -
 
#   1 - n.lt.1.
#   2 - ml.lt.1.
#   3 - m.lt.ml.
#   4 - singular matrix. (recoverable)
  
  real g(n,m),l(n,ml)    # l(n,ml-1).
  integer n,ml,m,int(n)
  
  real x,norm,eps,r1mach
  integer i,j,k,ll,m1,m2
  logical sing
  
# check the input for errors.
 
  if ( n < 1 ) { seterr(" bndlu - n.lt.1",15,1,2) }
  if ( ml < 1 ) { seterr(" bndlu - ml.lt.1",16,2,2) }
  if ( m < ml ) { seterr(" bndlu - m.lt.ml",16,3,2) }
 
  entsrc(i,0)    # protect against an existing error state.
 
  sing = .false. ; eps = r1mach(4)
  m1 = ml-1 ; m2 = m-ml
  
  ll = m1
  for ( i = 1 , i <= min0(m1,n) , i += 1 )    # set to 0 those elements
    {                                         # of g which are undefined.
    do j = ml+1-i , m 
		 { g(i,j-ll) = g(i,j) }
    ll = ll-1
    do j = m-ll , m 
		 { g(i,j) = 0.0e0 }
    }
  
  for ( i = 1 , i <= min0(m2,n) , i += 1 )    # zero out lower rhs wart.
    {
    do j = ml+i , m 
		 { g(n+1-i,j) = 0.0e0 }
    }
 
  norm = 0.0e0    # get || a || sub infinity.
  do i = 1 , n 
		
    {
    int(i) = i
    x = 0.0e0 ; do j = 1 , m 
		 { x += abs(g(i,j)) }
    norm = amax1(norm,x)
    }
  
  do k = 1 , n 
		
    {
    x = g(k,1) ; i = k
    
    ll = min0(m1+k,n)
    
    if ( k < ll )
      {
      do j = k+1 , ll 
		    # get the pivot row.
        { if ( abs(g(j,1)) > abs(x) ) { x = g(j,1) ; i = j } }
      }
    
    int(k) = i
    
    if ( x == 0.0e0 ) { sing = .true. ; g(k,1) = norm*eps }
 
    if ( ml == 1 | k == n ) { next }
    
    if ( i ~= k )    # need to interchange the rows.
      {
      do j = 1 , m 
		 { x = g(k,j) ; g(k,j) = g(i,j) ; g(i,j) = x }
      }
    
    if ( k >= ll ) { next }
    do i = k+1 , ll 
		
      {
      x = g(i,1)/g(k,1)
      l(k,i-k) = x
      
      do j = 2 , m 
		 { g(i,j-1) = g(i,j)-x*g(k,j) }
 
      g(i,m) = 0.0e0
      }
    }
  
  if ( sing ) { seterr(" bndlu - singular matrix",24,4,1) }
  
  return
  
  end
  procedure bndfb(n,ml,m,l,u,int,nb,b)
 
# to solve l*u*x = b, where l and u result from a call to bnds.
 
# mnemonic - double precision band forward elimination and
#            back-solve.
 
# input -
 
#   n   - the order of the system.
#   ml  - the number of nonzero entries of l on and below
#         the diagonal.
#   m   - the number of nonzero elements of u on and above
#         the diagonal.
#   l   - the lower triangular banded factor.
#   u   - the upper triangular banded factor.
#   int - the ordering of the rows of the system, due to pivoting.
#   nb  - the number of right-hand-sides.
#   b   - the right-hand-sides.
 
# output -
 
#   b - the solution vectors.
 
# scratch space allocated - none.
 
# error states -
 
#   1 - n.lt.1.
#   2 - ml.lt.1.
#   3 - m.lt.ml.
#   4 - nb.lt.1.
 
  real l(n,ml),u(n,m),b(n,nb)    # l(n,ml-1).
  integer n,ml,m,int(n),nb
 
  integer nerror,nerr
 
# check the input for errors.
 
  if ( n < 1 ) { seterr(" bndfb - n.lt.1",15,1,2) }
  if ( ml < 1 ) { seterr(" bndfb - ml.lt.1",16,2,2) }
  if ( m < ml ) { seterr(" bndfb - m.lt.ml",16,3,2) }
  if ( nb < 1 ) { seterr(" bndfb - nb.lt.1",16,4,2) }
  
  entsrc(nerr,0)    # protect against an existing error state.
  
  bndfe(n,ml,l,int,nb,b)
		    # do the forward-elimination.
 
  bndbs(n,m,u,nb,b)
		    # do the back-substitution.
  
  return
  
  end
