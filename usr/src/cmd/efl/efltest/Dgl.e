  Procedure DglsBP(Nu,Order,BC,E)
  
# To determine which ODE should use which boundary condition.
  
# Mnemonic - Double precision Galerkin's method for Linear Systems,
#            Boundary condition Placement.

# Scratch Space Allocated -

#       S(DglsBP) <= Nu*(4*Nu+15)

# Integer words.
  
  Integer Nu,Order(Nu,Nu,2),BC(Nu,2,2),E(Nu,2,2)
  
  Integer inow,inowold,i,j,l,iMaxord,iCE,Istkgt,iPPS
  Logical AllZero
  
  Struct Nodei { Integer bp,N,j,R(1) }
  
  Define Push  +1
  Define Search  0
  Define Pop  -1

# Define Node = Is(inow) -> Nodei
  
  Include dstack

# Check the input for errors.

  If ( Nu < 1 ) { Seterr("DglsBP - Nu.lt.1",16,1,2) }

  Do l = 1 , 2 
    {

    Do i = 1 , Nu 
      {
      AllZero = .True.    # Is Order(i,.,l) = (-1,...,-1)?
      Do j = 1 , Nu 
        {
        AllZero &= Order(i,j,l) == (-1)
        If ( Order(i,j,l) < (-1) | Order(i,j,l) > 2 )
          { Seterr("DglsBP - Order(i,j,l) not one of -1,0,1,2",41,2,2) }
        }
      If ( ( BC(i,1,l) ~= (-2) & BC(i,1,l) ~= 0 ) |
           ( BC(i,2,l) ~= (-2) & BC(i,2,l) ~= 1 ) )
        { Seterr("DglsBP - BC(i,.,l) not one of -2,0,1",36,3,2) }
      If ( AllZero )
        { Seterr("DglsBP - Order(i,.,l)=(-1,...,-1)",33,4,2) }
      }
    }
  
  Enter(1)
  
  iCE = Istkgt(Nu,2)    # Complement of E.

# Maxord(i,l) = Max over j=1,...,Nu Order(i,j,l).

  iMaxord = Istkgt(2*Nu,2)
  Seti(2*Nu,-1,Is(iMaxord))

  Do l = 1 , 2 
    {
    Do i = 1 , Nu 
      {
      Do j = 1 , Nu 
        {
        Is(iMaxord+i-1+(l-1)*Nu) = Max0(Is(iMaxord+i-1+(l-1)*Nu),
                                        Order(i,j,l))
        }
      }
    }
  
  i = 0 ; iPPS = Push

  While ( i < 4*Nu | iPPS ~= Push )
    {
    
    Switch ( iPPS )
      {

      Case Push:    # Make a node.

        inowold = inow
        i += 1 ; inow = Istkgt(Nu+3,2)
        Is(inow) -> Nodei.bp = inowold
      
#       Get the candidates for E(i).
      
        D6lsBP(i,Nu,Order,BC,E,
               Is(iMaxord),Is(iCE),Is(inow) -> Nodei.R,Is(inow) -> Nodei.N)
      
        Is(inow) -> Nodei.j = 0
        iPPS = Search ; Break

      Case Search:    # Searching a node.

        Is(inow) -> Nodei.j += 1
      
        If ( Is(inow) -> Nodei.j > Is(inow) -> Nodei.N )    # Back-up.
          { iPPS = Pop ; Next }
      
        E(i,1,1) = Is(inow) -> Nodei.R(Is(inow) -> Nodei.j)
        iPPS = Push ; Break

      Case Pop:    # Backing up a Node.

        inow = Is(inow) -> Nodei.bp ; Istkrl(1) ; i -= 1
        iPPS = Search ; Break

      }    # End Switch.

    If ( i == 0 )
      { Seterr("DglsBP - Improper Boundary Conditions",37,5,1) ; Break }

    }    # End While.

  Leave()
  
  Return
  
  End
  Procedure D6lsBP(i,Nu,Order,BC,E,
                   Maxord,CE,R,N)
  
  Integer i,Nu,Order(Nu,Nu,2),BC(1),E(1),    # BC(Nu,2,2),E(Nu,2,2),
          Maxord(Nu,2),CE(Nu),R(Nu),N    # E(i-1),R(N).
  
  Integer j,LR,DM,NBCs,l,ii

  If ( BC(i) < 0 ) { N = 1 ; R(N) = 0 ; Return }

# LR = 1 for left, LR = 2 for right.

  LR = 1+(i-1)/(2*Nu)

# DM = 1 for Dirichlet, DM = 2 for Mixed boundary conditions.

  DM = 1+Mod((i-1)/Nu,2)

  ii = Mod(i,Nu) ; If ( ii == 0 ) { ii = Nu }    # B(i) = B(ii,DM,LR).
  
  N = 0
  
  Do j = 1 , Nu  { CE(j) = j }    # CE = Complement of E.
  If ( i <= 2*Nu )
    {
    For ( j = 1 , j < i , j += 1 )
      {
      If ( BC(j) >= 0 ) { CE(E(j)) = 0 }
      }
    }
  Else
    {
    For ( j = 2*Nu+1 , j < i , j += 1 )
      {
      If ( BC(j) >= 0 ) { CE(E(j)) = 0 }
      }
    }
  
  Do j = 1 , Nu 
    {
    If ( CE(j) == 0 ) { Next }

    NBCs = 0
    
    For ( l = 1 , l < i , l += 1 )
      {
      If ( E(l) == j & BC(l) >= 0 ) { NBCs += 1 }
      }
    
    If ( ( DM == 1 & Maxord(j,LR) > BC(i) ) |
         ( DM == 2 & Order(j,ii,LR) > BC(i) ) )
      {
      If ( NBCs < Max0(Maxord(j,1),Maxord(j,2)) )
        { N += 1 ; R(N) = j }
      }
    }
  
  Return
  
  End
