      subroutine setup
      implicit double precision (a-h,o-z)
c
c
c     this routine drives the sparse matrix setup used by spice.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /miscel/ atime,aprog(3),adate,atitle(10),defl,defw,defad,
     1  defas,rstats(50),iwidth,lwidth,nopage
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /dc/ tcstar(2),tcstop(2),tcincr(2),icvflg,itcelm(2),kssop,
     1   kinel,kidin,kovar,kidout
      common /ac/ fstart,fstop,fincr,skw2,refprl,spw2,jacflg,idfreq,
     1   inoise,nosprt,nosout,nosin,idist,idprt
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      call second(t1)
      nstop=numnod+jelcnt(3)+jelcnt(6)+jelcnt(8)+jelcnt(9)+2*jelcnt(17)
c
c  reserve matrix locations for each element
c
      call matptr
      if (nogo.ne.0) go to 1000
c
c  reorder matrix pointers for minimal fill-in
c
      nttbr=0
      do 120 i=2,nstop
      loc=isr+i
  110 if (nodplc(loc).eq.0) go to 120
      loc=nodplc(loc)
      nttbr=nttbr+1
      go to 110
  120 continue
c...  add ground
      nttbr=nttbr+1
      call reordr
      if (nogo.ne.0) go to 1000
      nttar=nodplc(iur+nstop+1)-1+nodplc(ilc+nstop+1)-1+nstop
      ifill=nttar-nttbr
      perspa=100.0d0*(1.0d0-dfloat(nttar)/
     1  (dfloat(nstop)*dfloat(nstop)))
      iops=0
      do 130 i=2,nstop
      noffr=nodplc(iur+i+1)-nodplc(iur+i)
      noffc=nodplc(ilc+i+1)-nodplc(ilc+i)
      iops=iops+noffr+noffc*(noffr+2)+1
  130 continue
      rstats(20)=nstop
      rstats(21)=nttbr
      rstats(22)=nttar
      rstats(23)=ifill
      rstats(24)=0.0d0
      rstats(25)=nttar
      rstats(26)=iops
      rstats(27)=perspa
c
c  store matrix locations
c
      call matloc
      call clrmem(isr)
      call clrmem(iseq)
      call clrmem(iseq1)
      call clrmem(neqn)
      call clrmem(nodevs)
      call clrmem(ndiag)
      call clrmem(nmoffc)
      call clrmem(numoff)
      call clrmem(iequa)
      call clrmem(jmnode)
c
c  generate machine code
c
 1000 call second(t2)
      rstats(2)=rstats(2)+t2-t1
      return
      end
      subroutine matptr
      implicit double precision (a-h,o-z)
c
c     this routine (by calls to the routine reserve) establishes the
c nonzero-element structure of the circuit equation coefficient matrix.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c  allocate and initialize storage
c
      call getm4(isr,nstop+1)
      numvs=jelcnt(3)+jelcnt(6)+jelcnt(8)+jelcnt(9)+2*jelcnt(17)
      call getm4(iseq,numvs)
      call getm4(iseq1,numvs)
      call getm4(neqn,numvs)
      call getm4(nodevs,numnod)
      call getm4(ndiag,nstop)
      call getm4(nmoffc,nstop)
      call getm4(numoff,nstop)
      call crunch
c
      call zero4(nodplc(isr+1),nstop+1)
      call zero4(nodplc(iseq1+1),numvs)
      call zero4(nodplc(nodevs+1),numnod)
      call zero4(nodplc(ndiag+1),nstop)
      call zero4(nodplc(nmoffc+1),nstop)
      call zero4(nodplc(numoff+1),nstop)
c
      numvs=0
      nxtrm=0
      ndist=0
      ntlin=1
      ibr=numnod
c
c  resistors
c
      loc=locate(1)
  110 if (loc.eq.0) go to 120
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      call reserv(node1,node1)
      call reserv(node1,node2)
      call reserv(node2,node1)
      call reserv(node2,node2)
      loc=nodplc(loc)
      go to 110
c
c  capacitors
c
  120 loc=locate(2)
  130 if (loc.eq.0) go to 400
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      call reserv(node1,node2)
      call reserv(node2,node1)
      ntemp=nodplc(ndiag+node1)
      call reserv(node1,node1)
      nodplc(ndiag+node1)=ntemp
      ntemp=nodplc(ndiag+node2)
      call reserv(node2,node2)
      nodplc(ndiag+node2)=ntemp
      nodplc(loc+8)=nxtrm+1
      nxtrm=nxtrm+2
      loc=nodplc(loc)
      go to 130
c
c  inductors
c
  400 loc=locate(3)
  430 if (loc.eq.0) go to 440
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ibr=ibr+1
      nodplc(loc+5)=ibr
      call reserv(node1,ibr)
      call reserv(node2,ibr)
      call reserv(ibr,node1)
      call reserv(ibr,node2)
      ntemp=nodplc(ndiag+ibr)
      call reserv(ibr,ibr)
      nodplc(ndiag+ibr)=ntemp
      numvs=numvs+1
      nodplc(iseq+numvs)=loc
      nodplc(neqn+numvs)=ibr
      nodplc(nodevs+node1)=nodplc(nodevs+node1)+1
      nodplc(nodevs+node2)=nodplc(nodevs+node2)+1
      nodplc(loc+11)=nxtrm+1
      nxtrm=nxtrm+2
      loc=nodplc(loc)
      go to 430
c
c  mutual inductors
c
  440 loc=locate(4)
  450 if (loc.eq.0) go to 460
      nl1=nodplc(loc+2)
      nl2=nodplc(loc+3)
      nl1=nodplc(nl1+5)
      nl2=nodplc(nl2+5)
      call reserv(nl1,nl2)
      call reserv(nl2,nl1)
      loc=nodplc(loc)
      go to 450
c
c  nonlinear voltage-controlled current sources
c
  460 loc=locate(5)
  462 if (loc.eq.0) go to 464
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ndim=nodplc(loc+4)
      ndim2=ndim+ndim
      locn=nodplc(loc+6)
      do 463 i=1,ndim2
      node=nodplc(locn+i)
      call reserv(node1,node)
      call reserv(node2,node)
  463 continue
      nodplc(loc+12)=nxtrm+1
      nxtrm=nxtrm+1+ndim2
      loc=nodplc(loc)
      go to 462
c
c  nonlinear voltage controlled voltage sources
c
  464 loc=locate(6)
  466 if (loc.eq.0) go to 468
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ibr=ibr+1
      nodplc(loc+6)=ibr
      call reserv(node1,ibr)
      call reserv(node2,ibr)
      call reserv(ibr,node1)
      call reserv(ibr,node2)
      numvs=numvs+1
      nodplc(iseq+numvs)=loc
      nodplc(neqn+numvs)=ibr
      nodplc(nodevs+node1)=nodplc(nodevs+node1)+1
      nodplc(nodevs+node2)=nodplc(nodevs+node2)+1
      ndim=nodplc(loc+4)
      ndim2=ndim+ndim
      locn=nodplc(loc+7)
      do 467 i=1,ndim2
      node=nodplc(locn+i)
      call reserv(ibr,node)
  467 continue
      nodplc(loc+13)=nxtrm+1
      nxtrm=nxtrm+2+ndim2
      loc=nodplc(loc)
      go to 466
c
c  voltage sources
c
  468 loc=locate(9)
  470 if (loc.eq.0) go to 472
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ibr=ibr+1
      nodplc(loc+6)=ibr
      call reserv(node1,ibr)
      call reserv(node2,ibr)
      call reserv(ibr,node1)
      call reserv(ibr,node2)
      numvs=numvs+1
      nodplc(iseq+numvs)=loc
      nodplc(neqn+numvs)=ibr
      nodplc(nodevs+node1)=nodplc(nodevs+node1)+1
      nodplc(nodevs+node2)=nodplc(nodevs+node2)+1
      loc=nodplc(loc)
      go to 470
c
c  nonlinear current controlled current sources
c
  472 loc=locate(7)
  474 if (loc.eq.0) go to 476
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ndim=nodplc(loc+4)
      locvs=nodplc(loc+6)
      do 475 i=1,ndim
      locvst=nodplc(locvs+i)
      kbr=nodplc(locvst+6)
      call reserv(node1,kbr)
      call reserv(node2,kbr)
  475 continue
      nodplc(loc+12)=nxtrm+1
      nxtrm=nxtrm+1+ndim+ndim
      loc=nodplc(loc)
      go to 474
c
c  nonlinear current controlled voltage sources
c
  476 loc=locate(8)
  478 if (loc.eq.0) go to 500
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ibr=ibr+1
      nodplc(loc+6)=ibr
      call reserv(node1,ibr)
      call reserv(node2,ibr)
      call reserv(ibr,node1)
      call reserv(ibr,node2)
      numvs=numvs+1
      nodplc(iseq+numvs)=loc
      nodplc(neqn+numvs)=ibr
      nodplc(nodevs+node1)=nodplc(nodevs+node1)+1
      nodplc(nodevs+node2)=nodplc(nodevs+node2)+1
      ndim=nodplc(loc+4)
      locvs=nodplc(loc+7)
      do 479 i=1,ndim
      locvst=nodplc(locvs+i)
      kbr=nodplc(locvst+6)
      call reserv(ibr,kbr)
  479 continue
      nodplc(loc+13)=nxtrm+1
      nxtrm=nxtrm+2+ndim+ndim
      loc=nodplc(loc)
      go to 478
c
c  diodes
c
  500 loc=locate(11)
  510 if (loc.eq.0) go to 520
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      call reserv(node1,node1)
      call reserv(node2,node2)
      call reserv(node3,node3)
      call reserv(node1,node3)
      call reserv(node2,node3)
      call reserv(node3,node1)
      call reserv(node3,node2)
      nodplc(loc+11)=nxtrm+1
      nxtrm=nxtrm+5
      nodplc(loc+12)=ndist+1
      ndist=ndist+7
      loc=nodplc(loc)
      go to 510
c
c  transistors
c
  520 loc=locate(12)
  530 if (loc.eq.0) go to 540
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      node6=nodplc(loc+7)
      node7=nodplc(loc+30)
      call reserv(node1,node1)
      call reserv(node2,node2)
      call reserv(node3,node3)
      call reserv(node4,node4)
      call reserv(node5,node5)
      call reserv(node6,node6)
      call reserv(node1,node4)
      call reserv(node2,node5)
      call reserv(node3,node6)
      call reserv(node4,node5)
      call reserv(node4,node6)
      call reserv(node5,node6)
      call reserv(node4,node1)
      call reserv(node5,node2)
      call reserv(node6,node3)
      call reserv(node5,node4)
      call reserv(node6,node4)
      call reserv(node6,node5)
      call reserv(node7,node7)
      call reserv(node4,node7)
      call reserv(node7,node4)
      call reserv(node2,node4)
      call reserv(node4,node2)
      nodplc(loc+22)=nxtrm+1
      nxtrm=nxtrm+18
      nodplc(loc+23)=ndist+1
      ndist=ndist+21
      loc=nodplc(loc)
      go to 530
c
c  jfets
c
  540 loc=locate(13)
  550 if (loc.eq.0) go to 560
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      call reserv(node1,node1)
      call reserv(node2,node2)
      call reserv(node3,node3)
      call reserv(node4,node4)
      call reserv(node5,node5)
      call reserv(node1,node4)
      call reserv(node2,node4)
      call reserv(node2,node5)
      call reserv(node3,node5)
      call reserv(node4,node5)
      call reserv(node4,node1)
      call reserv(node4,node2)
      call reserv(node5,node2)
      call reserv(node5,node3)
      call reserv(node5,node4)
      nodplc(loc+19)=nxtrm+1
      nxtrm=nxtrm+13
      loc=nodplc(loc)
      go to 550
c
c  mosfets
c
  560 loc=locate(14)
  570 if (loc.eq.0) go to 600
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      node6=nodplc(loc+7)
      call reserv(node1,node1)
      call reserv(node2,node2)
      call reserv(node3,node3)
      call reserv(node4,node4)
      call reserv(node5,node5)
      call reserv(node6,node6)
      call reserv(node1,node5)
      call reserv(node2,node4)
      call reserv(node2,node5)
      call reserv(node2,node6)
      call reserv(node3,node6)
      call reserv(node4,node5)
      call reserv(node4,node6)
      call reserv(node5,node6)
      call reserv(node5,node1)
      call reserv(node4,node2)
      call reserv(node5,node2)
      call reserv(node6,node2)
      call reserv(node6,node3)
      call reserv(node5,node4)
      call reserv(node6,node4)
      call reserv(node6,node5)
      nodplc(loc+26)=nxtrm+1
      nxtrm=nxtrm+18
      loc=nodplc(loc)
      go to 570
c
c  transmission lines
c
  600 loc=locate(17)
  610 if (loc.eq.0) go to 1000
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      ni1=nodplc(loc+6)
      ni2=nodplc(loc+7)
      ibr1=ibr+1
      ibr2=ibr+2
      ibr=ibr+2
      nodplc(loc+8)=ibr1
      nodplc(loc+9)=ibr2
      call reserv(node1,node1)
      call reserv(node1,ni1)
      call reserv(node2,ibr1)
      call reserv(node3,node3)
      call reserv(node4,ibr2)
      call reserv(ni1,node1)
      call reserv(ni1,ni1)
      call reserv(ni1,ibr1)
      call reserv(ni2,ni2)
      call reserv(ni2,ibr2)
      call reserv(ibr1,node2)
      call reserv(ibr1,node3)
      call reserv(ibr1,node4)
      call reserv(ibr1,ni1)
      call reserv(ibr1,ibr2)
      call reserv(ibr2,node1)
      call reserv(ibr2,node2)
      call reserv(ibr2,node4)
      call reserv(ibr2,ni2)
      call reserv(ibr2,ibr1)
      call reserv(node3,ni2)
      call reserv(ni2,node3)
      numvs=numvs+1
      nodplc(iseq+numvs)=loc
      nodplc(iseq1+numvs)=1
      nodplc(neqn+numvs)=ibr1
      nodplc(nodevs+ni1)=nodplc(nodevs+ni1)+1
      nodplc(nodevs+node2)=nodplc(nodevs+node2)+1
      numvs=numvs+1
      nodplc(iseq+numvs)=loc
      nodplc(iseq1+numvs)=2
      nodplc(neqn+numvs)=ibr2
      nodplc(nodevs+ni2)=nodplc(nodevs+ni2)+1
      nodplc(nodevs+node4)=nodplc(nodevs+node4)+1
      nodplc(loc+30)=ntlin+1
      ntlin=ntlin+2
      loc=nodplc(loc)
      go to 610
c
c  finished
c
 1000 return
      end
      subroutine reserv (node1,node2)
      implicit double precision (a-h,o-z)
c
c     this routine records the fact that the (node1, node2) element of
c the circuit equation coefficient matrix is nonzero.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      if (nogo.ne.0) go to 300
c...  test for ground
      if (node1.eq.1) go to 300
      if (node2.eq.1) go to 300
c
c  test for (node1,node2) matrix element
c
      loc=isr+node1
  100 if (nodplc(loc).eq.0) go to 110
      loc=nodplc(loc)
      if (nodplc(loc+1).eq.node2) go to 300
      go to 100
c
c  reserve (node1,node2) matrix element
c
  110 nodplc(numoff+node1)=nodplc(numoff+node1)+1
      nodplc(nmoffc+node2)=nodplc(nmoffc+node2)+1
      call sizmem(numoff,isize)
      newloc=numoff+isize+1
      nodplc(loc)=newloc
      call extmem(numoff,2)
      nodplc(newloc)=0
      nodplc(newloc+1)=node2
c
c  mark diagonal
c
      if (node1.ne.node2) go to 300
      nodplc(ndiag+node1)=1
c
c  finished
c
  300 return
      end
      subroutine reordr
      implicit double precision (a-h,o-z)
c
c     this routine swaps rows in the coefficient matrix to eliminate
c singularity problems which can be recognized by examining the circuit
c topology.  it then reorders the unknowns to minimize fillin terms
c which occur during lu factorization. (to maximize sparsity).
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c  allocate and initialize storage
c
      call getm4(iswap,nstop)
      call getm4(iequa,nstop)
      call getm4(iorder,nstop)
      call getm4(jmnode,nstop)
      call getm4(iur,nstop+1)
      call getm4(ilc,nstop+1)
      call getm4(iuc,0)
      call getm4(ilr,0)
c
      do 10 i=1,nstop
      nodplc(iswap+i)=i
   10 continue
      call copy4(nodplc(iswap+1),nodplc(iequa+1),nstop)
      call copy4(nodplc(iswap+1),nodplc(iorder+1),nstop)
      call copy4(nodplc(iswap+1),nodplc(jmnode+1),nstop)
c
c  swap current equations into admittance part of equation matrix
c
      nextv=1
c
c  find suitable voltage source
c
  100 if (nextv.gt.numvs) go to 150
      ix=0
      do 130 i=nextv,numvs
      loc=nodplc(iseq+i)
      node=nodplc(loc+2)
      nflag=nodplc(iseq1+i)
      if (nflag.eq.1) node=nodplc(loc+6)
      if (nflag.eq.2) node=nodplc(loc+7)
      if (node.eq.1) go to 110
      if (nodplc(nodevs+node).ge.2) go to 110
      if (nodplc(ndiag+node).eq.0) go to 140
      ix=i
      locx=loc
      nodex=node
  110 node=nodplc(loc+3)
      if (nflag.eq.2) node=nodplc(loc+5)
      if (node.eq.1) go to 130
      if (nodplc(nodevs+node).ge.2) go to 130
  120 if (nodplc(ndiag+node).eq.0) go to 140
      ix=i
      locx=loc
      nodex=node
  130 continue
      if (ix.eq.0) go to 590
      i=ix
      loc=locx
      node=nodex
c
c  resequence voltage sources
c
  140 nodplc(iseq+i)=nodplc(iseq+nextv)
      nodplc(iseq+nextv)=loc
      ltemp=nodplc(iseq1+i)
      nodplc(iseq1+i)=nodplc(iseq1+nextv)
      nodplc(iseq1+nextv)=ltemp
      ibr=nodplc(neqn+i)
      nodplc(neqn+i)=nodplc(neqn+nextv)
      nodplc(neqn+nextv)=ibr
      node1=nodplc(loc+2)
      if (ltemp.eq.1) node1=nodplc(loc+6)
      if (ltemp.eq.2) node1=nodplc(loc+7)
      node2=nodplc(loc+3)
      if (ltemp.eq.1) node2=nodplc(loc+3)
      if (ltemp.eq.2) node2=nodplc(loc+5)
      nodplc(nodevs+node1)=nodplc(nodevs+node1)-1
      nodplc(nodevs+node2)=nodplc(nodevs+node2)-1
c
c  set row swap indicators
c
      l=nodplc(iswap+ibr)
      j=nodplc(iequa+node)
      nodplc(iswap+j)=l
      nodplc(iequa+l)=j
      nodplc(iswap+ibr)=node
      nodplc(iequa+node)=ibr
      nextv=nextv+1
      go to 100
c
c  initialize matrix pointers
c
  150 nexnod=2
      nut=0
      nlt=0
  160 nodplc(iur+nexnod)=nut+1
      nodplc(ilc+nexnod)=nlt+1
      if (nexnod.ge.nstop) go to 500
c
c  select row for reordering
c
      load=nodplc(iorder+nexnod)
      ir=nodplc(iswap+load)
      imin=nodplc(numoff+ir)*nodplc(nmoffc+load)
      nstart=nexnod+1
      do 200 i=nstart,nstop
      lc=nodplc(iorder+i)
      ir=nodplc(iswap+lc)
      nrc=nodplc(numoff+ir)*nodplc(nmoffc+lc)
      if (nrc.ge.imin) go to 200
      imin=nrc
      load=lc
  200 continue
c
c  set reorder indicators
c
      ir=nodplc(iswap+load)
      nodplc(numoff+ir)=nodplc(numoff+ir)-1
      nodplc(nmoffc+load)=nodplc(nmoffc+load)-1
      lc=nodplc(iorder+nexnod)
      jr=nodplc(jmnode+load)
      nodplc(iorder+jr)=lc
      nodplc(jmnode+lc)=jr
      nodplc(iorder+nexnod)=load
      nodplc(jmnode+load)=nexnod
c
c  set pointers for upper triangle
c
      loc=isr+ir
  330 if (nodplc(loc).eq.0) go to 340
      loc=nodplc(loc)
      ic=nodplc(loc+1)
      jc=nodplc(jmnode+ic)
      if (jc.le.nexnod) go to 330
      nodplc(nmoffc+ic)=nodplc(nmoffc+ic)-1
      call extmem(iuc,1)
      nodplc(iuc+nut+1)=ic
      nut=nut+1
      go to 330
c
c  set pointers for lower triangle
c
  340 do 390 jr=nstart,nstop
      lc=nodplc(iorder+jr)
      ir=nodplc(iswap+lc)
      loc=isr+ir
  350 if (nodplc(loc).eq.0) go to 390
      loc=nodplc(loc)
      if (nodplc(loc+1).ne.load) go to 350
      nodplc(numoff+ir)=nodplc(numoff+ir)-1
      call extmem(ilr,1)
      nodplc(ilr+nlt+1)=lc
      nlt=nlt+1
c
c  check for fill-in terms
c
      nct=nodplc(iur+nexnod)
  360 if (nct.ge.(nut+1)) go to 390
      ic=nodplc(iuc+nct)
      call reserv(ir,ic)
      nct=nct+1
      go to 360
  390 continue
c
c
      nexnod=nexnod+1
      go to 160
c
c  reordering finished
c
  500 nodplc(iur+nstop+1)=nut+1
      nodplc(ilc+nstop+1)=nlt+1
      if (nut.eq.0) go to 515
      do 510 i=1,nut
      j=nodplc(iuc+i)
      nodplc(iuc+i)=nodplc(jmnode+j)
  510 continue
  515 if (nlt.eq.0) go to 600
      do 520 i=1,nlt
      j=nodplc(ilr+i)
      nodplc(ilr+i)=nodplc(jmnode+j)
  520 continue
      go to 600
c
c  error - voltage-source/inductor/transmission-line loop detected ...
c
  590 nogo=1
      write (6,591)
c...  loop should have been detected in topchk
  591 format('0*abort*:  spice internal error in reordr'/)
c
c  finished
c
  600 return
      end
      subroutine matloc
      implicit double precision (a-h,o-z)
c
c     this routine stores the locations of the various matrix terms to
c which the different circuit elements contribute.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c  resistors
c
      loc=locate(1)
  690 if (loc.eq.0) go to 700
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      nodplc(loc+4)=indxx(node1,node2)
      nodplc(loc+5)=indxx(node2,node1)
      nodplc(loc+6)=indxx(node1,node1)
      nodplc(loc+7)=indxx(node2,node2)
      loc=nodplc(loc)
      go to 690
c
c  capacitors
c
  700 loc=locate(2)
  710 if (loc.eq.0) go to 720
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      nodplc(loc+5)=indxx(node1,node2)
      nodplc(loc+6)=indxx(node2,node1)
      nodplc(loc+10)=indxx(node1,node1)
      nodplc(loc+11)=indxx(node2,node2)
      loc=nodplc(loc)
      go to 710
c
c  inductors
c
  720 loc=locate(3)
  730 if (loc.eq.0) go to 740
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ibr=nodplc(loc+5)
      nodplc(loc+6)=indxx(node1,ibr)
      nodplc(loc+7)=indxx(node2,ibr)
      nodplc(loc+8)=indxx(ibr,node1)
      nodplc(loc+9)=indxx(ibr,node2)
      nodplc(loc+13)=indxx(ibr,ibr)
      loc=nodplc(loc)
      go to 730
c
c  mutual inductances
c
  740 loc=locate(4)
  750 if (loc.eq.0) go to 760
      nl1=nodplc(loc+2)
      nl2=nodplc(loc+3)
      ibr1=nodplc(nl1+5)
      ibr2=nodplc(nl2+5)
      nodplc(loc+4)=indxx(ibr1,ibr2)
      nodplc(loc+5)=indxx(ibr2,ibr1)
      loc=nodplc(loc)
      go to 750
c
c  nonlinear voltage controlled current sources
c
  760 loc=locate(5)
  762 if (loc.eq.0) go to 764
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ndim=nodplc(loc+4)
      lnod=nodplc(loc+6)
      lmat=nodplc(loc+7)
      do 763 i=1,ndim
      node3=nodplc(lnod+1)
      node4=nodplc(lnod+2)
      lnod=lnod+2
      nodplc(lmat+1)=indxx(node1,node3)
      nodplc(lmat+2)=indxx(node1,node4)
      nodplc(lmat+3)=indxx(node2,node3)
      nodplc(lmat+4)=indxx(node2,node4)
      lmat=lmat+4
  763 continue
      loc=nodplc(loc)
      go to 762
c
c  nonlinear voltage controlled voltage sources
c
  764 loc=locate(6)
  766 if (loc.eq.0) go to 768
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ndim=nodplc(loc+4)
      ibr=nodplc(loc+6)
      lnod=nodplc(loc+7)
      lmat=nodplc(loc+8)
      nodplc(lmat+1)=indxx(node1,ibr)
      nodplc(lmat+2)=indxx(node2,ibr)
      nodplc(lmat+3)=indxx(ibr,node1)
      nodplc(lmat+4)=indxx(ibr,node2)
      lmat=lmat+4
      do 767 i=1,ndim
      node3=nodplc(lnod+1)
      node4=nodplc(lnod+2)
      lnod=lnod+2
      nodplc(lmat+1)=indxx(ibr,node3)
      nodplc(lmat+2)=indxx(ibr,node4)
      lmat=lmat+2
  767 continue
      loc=nodplc(loc)
      go to 766
c
c  nonlinear current controlled current sources
c
  768 loc=locate(7)
  770 if (loc.eq.0) go to 772
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ndim=nodplc(loc+4)
      locvs=nodplc(loc+6)
      lmat=nodplc(loc+7)
      do 771 i=1,ndim
      locvst=nodplc(locvs+i)
      ibr=nodplc(locvst+6)
      nodplc(lmat+1)=indxx(node1,ibr)
      nodplc(lmat+2)=indxx(node2,ibr)
      lmat=lmat+2
  771 continue
      loc=nodplc(loc)
      go to 770
c
c  nonlinear current controlled voltage sources
c
  772 loc=locate(8)
  774 if (loc.eq.0) go to 780
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ndim=nodplc(loc+4)
      ibr=nodplc(loc+6)
      locvs=nodplc(loc+7)
      lmat=nodplc(loc+8)
      nodplc(lmat+1)=indxx(node1,ibr)
      nodplc(lmat+2)=indxx(node2,ibr)
      nodplc(lmat+3)=indxx(ibr,node1)
      nodplc(lmat+4)=indxx(ibr,node2)
      lmat=lmat+4
      do 775 i=1,ndim
      locvst=nodplc(locvs+i)
      kbr=nodplc(locvst+6)
      nodplc(lmat+i)=indxx(ibr,kbr)
  775 continue
      loc=nodplc(loc)
      go to 774
c
c  voltage sources
c
  780 loc=locate(9)
  790 if (loc.eq.0) go to 800
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      iptr=nodplc(loc+6)
      nodplc(loc+7)=indxx(node1,iptr)
      nodplc(loc+8)=indxx(node2,iptr)
      nodplc(loc+9)=indxx(iptr,node1)
      nodplc(loc+10)=indxx(iptr,node2)
      loc=nodplc(loc)
      go to 790
c
c  diodes
c
  800 loc=locate(11)
  810 if (loc.eq.0) go to 820
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      nodplc(loc+7)=indxx(node1,node3)
      nodplc(loc+8)=indxx(node2,node3)
      nodplc(loc+9)=indxx(node3,node1)
      nodplc(loc+10)=indxx(node3,node2)
      nodplc(loc+13)=indxx(node1,node1)
      nodplc(loc+14)=indxx(node2,node2)
      nodplc(loc+15)=indxx(node3,node3)
      loc=nodplc(loc)
      go to 810
c
c  transistors
c
  820 loc=locate(12)
  830 if (loc.eq.0) go to 840
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      node6=nodplc(loc+7)
      node7=nodplc(loc+30)
      nodplc(loc+10)=indxx(node1,node4)
      nodplc(loc+11)=indxx(node2,node5)
      nodplc(loc+12)=indxx(node3,node6)
      nodplc(loc+13)=indxx(node4,node1)
      nodplc(loc+14)=indxx(node4,node5)
      nodplc(loc+15)=indxx(node4,node6)
      nodplc(loc+16)=indxx(node5,node2)
      nodplc(loc+17)=indxx(node5,node4)
      nodplc(loc+18)=indxx(node5,node6)
      nodplc(loc+19)=indxx(node6,node3)
      nodplc(loc+20)=indxx(node6,node4)
      nodplc(loc+21)=indxx(node6,node5)
      nodplc(loc+24)=indxx(node1,node1)
      nodplc(loc+25)=indxx(node2,node2)
      nodplc(loc+26)=indxx(node3,node3)
      nodplc(loc+27)=indxx(node4,node4)
      nodplc(loc+28)=indxx(node5,node5)
      nodplc(loc+29)=indxx(node6,node6)
      nodplc(loc+31)=indxx(node7,node7)
      nodplc(loc+32)=indxx(node4,node7)
      nodplc(loc+33)=indxx(node7,node4)
      nodplc(loc+34)=indxx(node2,node4)
      nodplc(loc+35)=indxx(node4,node2)
      loc=nodplc(loc)
      go to 830
c
c  jfets
c
  840 loc=locate(13)
  850 if (loc.eq.0) go to 860
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      nodplc(loc+9)=indxx(node1,node4)
      nodplc(loc+10)=indxx(node2,node4)
      nodplc(loc+11)=indxx(node2,node5)
      nodplc(loc+12)=indxx(node3,node5)
      nodplc(loc+13)=indxx(node4,node1)
      nodplc(loc+14)=indxx(node4,node2)
      nodplc(loc+15)=indxx(node4,node5)
      nodplc(loc+16)=indxx(node5,node2)
      nodplc(loc+17)=indxx(node5,node3)
      nodplc(loc+18)=indxx(node5,node4)
      nodplc(loc+20)=indxx(node1,node1)
      nodplc(loc+21)=indxx(node2,node2)
      nodplc(loc+22)=indxx(node3,node3)
      nodplc(loc+23)=indxx(node4,node4)
      nodplc(loc+24)=indxx(node5,node5)
      loc=nodplc(loc)
      go to 850
c
c  mosfets
c
  860 loc=locate(14)
  870 if (loc.eq.0) go to 900
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      node6=nodplc(loc+7)
      nodplc(loc+10)=indxx(node1,node5)
      nodplc(loc+11)=indxx(node2,node4)
      nodplc(loc+12)=indxx(node2,node5)
      nodplc(loc+13)=indxx(node2,node6)
      nodplc(loc+14)=indxx(node3,node6)
      nodplc(loc+15)=indxx(node4,node2)
      nodplc(loc+16)=indxx(node4,node5)
      nodplc(loc+17)=indxx(node4,node6)
      nodplc(loc+18)=indxx(node5,node1)
      nodplc(loc+19)=indxx(node5,node2)
      nodplc(loc+20)=indxx(node5,node4)
      nodplc(loc+21)=indxx(node5,node6)
      nodplc(loc+22)=indxx(node6,node2)
      nodplc(loc+23)=indxx(node6,node3)
      nodplc(loc+24)=indxx(node6,node4)
      nodplc(loc+25)=indxx(node6,node5)
      nodplc(loc+27)=indxx(node1,node1)
      nodplc(loc+28)=indxx(node2,node2)
      nodplc(loc+29)=indxx(node3,node3)
      nodplc(loc+30)=indxx(node4,node4)
      nodplc(loc+31)=indxx(node5,node5)
      nodplc(loc+32)=indxx(node6,node6)
      loc=nodplc(loc)
      go to 870
c
c  transmission lines
c
  900 loc=locate(17)
  910 if (loc.eq.0) go to 1000
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      ni1=nodplc(loc+6)
      ni2=nodplc(loc+7)
      ibr1=nodplc(loc+8)
      ibr2=nodplc(loc+9)
      nodplc(loc+10)=indxx(node1,node1)
      nodplc(loc+11)=indxx(node1,ni1)
      nodplc(loc+12)=indxx(node2,ibr1)
      nodplc(loc+13)=indxx(node3,node3)
      nodplc(loc+14)=indxx(node4,ibr2)
      nodplc(loc+15)=indxx(ni1,node1)
      nodplc(loc+16)=indxx(ni1,ni1)
      nodplc(loc+17)=indxx(ni1,ibr1)
      nodplc(loc+18)=indxx(ni2,ni2)
      nodplc(loc+19)=indxx(ni2,ibr2)
      nodplc(loc+20)=indxx(ibr1,node2)
      nodplc(loc+21)=indxx(ibr1,node3)
      nodplc(loc+22)=indxx(ibr1,node4)
      nodplc(loc+23)=indxx(ibr1,ni1)
      nodplc(loc+24)=indxx(ibr1,ibr2)
      nodplc(loc+25)=indxx(ibr2,node1)
      nodplc(loc+26)=indxx(ibr2,node2)
      nodplc(loc+27)=indxx(ibr2,node4)
      nodplc(loc+28)=indxx(ibr2,ni2)
      nodplc(loc+29)=indxx(ibr2,ibr1)
      nodplc(loc+31)=indxx(node3,ni2)
      nodplc(loc+32)=indxx(ni2,node3)
      loc=nodplc(loc)
      go to 910
c
c  .nodeset
c
 1000 call sizmem(nsnod,nic)
      if(nic.eq.0) go to 1020
      call getm4(nsmat,nic)
      do 1010 i=1,nic
      node=nodplc(nsnod+i)
      nodplc(nsmat+i)=indxx(node,node)
 1010 continue
c
c  transient initial conditions
c
 1020 call sizmem(icnod,nic)
      if(nic.eq.0) go to 1100
      call getm4(icmat,nic)
      do 1030 i=1,nic
      node=nodplc(icnod+i)
      nodplc(icmat+i)=indxx(node,node)
 1030 continue
c
c  finished
c
 1100 return
      end
      integer function indxx(node1,node2)
      implicit double precision (a-h,o-z)
c
c     this routine maps a (row, column) matrix term specification into
c the offset from the origin of the matrix storage at which the term is
c actually located.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c  check for ground
c
      if (node1.eq.1) go to 400
      if (node2.eq.1) go to 400
c
      n1=nodplc(iequa+node1)
      n1=nodplc(jmnode+n1)
      n2=nodplc(jmnode+node2)
c
c
      if (n1-n2) 100,200,300
c
c  upper triangle
c
  100 ns=nodplc(iur+n1)
      ne=nodplc(iur+n1+1)
  110 if (ns.ge.ne) go to 400
      if (nodplc(iuc+ns).eq.n2) go to 120
      ns=ns+1
      go to 110
  120 indxx=nstop+ns
      go to 500
c
c  diagonal
c
  200 indxx=node2
      go to 500
c
c  lower triangle
c
  300 ns=nodplc(ilc+n2)
      ne=nodplc(ilc+n2+1)
  310 if (ns.ge.ne) go to 400
      if (nodplc(ilr+ns).eq.n1) go to 320
      ns=ns+1
      go to 310
  320 indxx=nstop+nut+ns
      go to 500
c
c  unused location
c
  400 indxx=1
c
c  finished
c
  500 return
      end
      subroutine codgen
      implicit double precision (a-h,o-z)
c
c     this routine generates machine instructions (for the cdc 6400) to
c lu-factor and solve the set of circuit equations.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
      return
      end
