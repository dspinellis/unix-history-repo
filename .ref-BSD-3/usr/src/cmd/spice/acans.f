      subroutine acan
      implicit double precision (a-h,o-z)
c
c
c     this routine drives the small-signal analyses.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /miscel/ atime,aprog(3),adate,atitle(10),defl,defw,defad,
     1  defas,rstats(50),iwidth,lwidth,nopage
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /ac/ fstart,fstop,fincr,skw2,refprl,spw2,jacflg,idfreq,
     1   inoise,nosprt,nosout,nosin,idist,idprt
      common /cje/ maxtim,itime,icost
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
       data aendor /9.87654321d0/
      call second(t1)
c.. post-processor initialization
      if(ipostp.eq.0) go to 1
      numcur=jelcnt(9)
      numpos=nunods+numcur
      call getm16(ibuff,numpos)
      numpos=numpos*4
      if(numcur.eq.0) go to 1
      loc=locate(9)
      loccur=nodplc(loc+6)-1
c
c  allocate storage
c
    1 call getm8(ndiag,2*nstop)
      call getm8(lvn,nstop)
      call getm8(imvn,nstop)
      call getm16(lcvn,nstop)
      call getm8(lynl,nstop+nut+nlt)
      call getm8(imynl,nstop+nut+nlt)
      if (idist.ne.0) call dinit
      nandd=0
      if (inoise.eq.0) go to 10
      if (idist.eq.0) go to 10
      nandd=1
      call getm16(lvnctc,nstop)
   10 call getm16(loutpt,0)
      call crunch
      lyu=lynl+nstop
      lyl=lyu+nut
      numout=jelcnt(43)+jelcnt(44)+jelcnt(45)+1
c      if(ipostp.ne.0) call pheadr(atitle)
      icalc=0
      freq=fstart
c
c  load y matrix and c vector, solve for v vector
c
  100 call getcje
      if ((maxtim-itime).le.limtim) go to 900
      omega=twopi*freq
      call acload
      call acdcmp
      call acsol
      if (igoof.eq.0) go to 200
      write (6,121) igoof,freq
  121 format('0warning:  underflow ',i4,' time(s) in ac analysis at freq
     1 = ',1pd9.3,' hz')
      igoof=0
c
c  store outputs
c
  200 call extmem(loutpt,numout)
      loco=loutpt+icalc*numout
      icalc=icalc+1
      cvalue(loco+1)=dcmplx(freq,omega)
      loc=locate(43)
  310 if (loc.eq.0) go to 350
      if (nodplc(loc+5).ne.0) go to 320
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      iseq=nodplc(loc+4)
      cvalue(loco+iseq)=cvalue(lcvn+node1)-cvalue(lcvn+node2)
      loc=nodplc(loc)
      go to 310
  320 iptr=nodplc(loc+2)
      iptr=nodplc(iptr+6)
      iseq=nodplc(loc+4)
      cvalue(loco+iseq)=cvalue(lcvn+iptr)
      loc=nodplc(loc)
      go to 310
  350 if(ipostp.eq.0) go to 400
      cvalue(ibuff+1)=dcmplx(freq,0.0d0)
      call copy16(cvalue(lcvn+2),cvalue(ibuff+2),nunods-1)
      if(numcur.ne.0) call copy16(cvalue(lcvn+loccur+1),
     1  cvalue(ibuff+nunods+1),numcur)
      call dblsgl(cvalue(ibuff+1),numpos)
c      call fwrite(cvalue(ibuff+1),numpos)
c
c  noise and distortion analyses
c
  400 if (nandd.eq.0) go to 410
      call copy16(cvalue(lcvn+1),cvalue(lvnctc+1),nstop)
  410 if (inoise.ne.0) call noise(loco)
      if (nandd.eq.0) go to 420
      call copy16(cvalue(lvnctc+1),cvalue(lcvn+1),nstop)
  420 if (idist.ne.0) call disto(loco)
c
c  increment frequency
c
      if (icalc.ge.jacflg) go to 1000
      if (idfreq.ge.3) go to 510
      freq=freq*fincr
      go to 100
  510 freq=freq+fincr
      go to 100
c
c  finished
c
  900 write (6,901)
  901 format('0*error*:  cpu time limit exceeded ... analysis stopped'/)
      nogo=1
 1000 if(ipostp.eq.0) go to 1010
      cvalue(ibuff+1)=aendor
c      call fwrite(cvalue(ibuff+1),numpos)
      if(ipostp.ne.0) call clrmem(ibuff)
 1010 call clrmem(lvnim1)
      call clrmem(lx0)
      call clrmem(lvn)
      call clrmem(imvn)
      call clrmem(lcvn)
      call clrmem(imynl)
      call clrmem(lynl)
      call clrmem(ndiag)
      if (idist.eq.0) go to 1020
      call clrmem(ld0)
      call clrmem(ld1)
 1020 if (nandd.eq.0) go to 1040
      call clrmem(lvnctc)
 1040 call second(t2)
      rstats(7)=rstats(7)+t2-t1
      rstats(8)=rstats(8)+icalc
      return
      end
      subroutine cdiv(xr,xi,yr,yi,cr,ci)
c.. ok if cr and ci are really xr and xi or yr and yi
      implicit double precision (a-h,o-z)
      xrtemp=xr
      xitemp=xi
      yrtemp=yr
      yitemp=yi
      amag2=yrtemp*yrtemp+yitemp*yitemp
      cr=(xrtemp*yrtemp+xitemp*yitemp)/amag2
      ci=(xitemp*yrtemp-xrtemp*yitemp)/amag2
      return
      end
      subroutine cmult(xr,xi,yr,yi,cr,ci)
c.. ok if cr and ci are really xr and xi or yr and yi
      implicit double precision (a-h,o-z)
      xrtemp=xr
      xitemp=xi
      yrtemp=yr
      yitemp=yi
      cr=xrtemp*yrtemp-xitemp*yitemp
      ci=xitemp*yrtemp+xrtemp*yitemp
      return
      end
      subroutine dblsgl(carray,nwds)
c.. note that carray really contains complex*16
      complex*8 carray(1)
      complex*8 ctemp8(2),ctemp3
      complex*16 ctemp
      equivalence (ctemp,ctemp8(1))
c
c.. gather up numpos 16-bit words from dbl-complex to
c.. make up sgl-complex
c
      num=nwds/4
      do 10 i=1,num
      ndex=2*i-1
      ctemp8(1)=carray(ndex)
      ctemp8(2)=carray(ndex+1)
      ctemp3=ctemp
      carray(i)=ctemp3
   10 continue
      return
      end
      subroutine acdcmp
      implicit double precision (a-h,o-z)
c
c     this routine performs an lu factorization of the circuit equation
c coefficient matrix.
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
c
c
      do 100 i=2,nstop
      io=nodplc(iorder+i)
      gdiag=dabs(value(lynl+io))+dabs(value(imynl+io))
      if (gdiag.ge.gmin) go to 10
      value(lynl+io)=gmin
      value(imynl+io)=0.0d0
      igoof=igoof+1
   10 jstart=nodplc(ilc+i)
      jstop=nodplc(ilc+i+1)-1
      if (jstart.gt.jstop) go to 100
      do 90 j=jstart,jstop
      call cdiv(value(lyl+j),value(imynl+nstop+nut+j),value(lynl+io),
     1  value(imynl+io),value(lyl+j),value(imynl+nstop+nut+j))
      icol=nodplc(ilr+j)
      kstart=nodplc(iur+i)
      kstop=nodplc(iur+i+1)-1
      if (kstart.gt.kstop) go to 90
      do 80 k=kstart,kstop
      irow=nodplc(iuc+k)
      if (icol-irow) 20,60,40
c
c  find (icol,irow) matrix term (upper triangle)
c
   20 l=nodplc(iur+icol+1)
   30 l=l-1
      if (nodplc(iuc+l).ne.irow) go to 30
      ispot=lyu+l
      ispot2=imynl+nstop+l
      go to 70
c
c  find (icol,irow) matrix term (lower triangle)
c
   40 l=nodplc(ilc+irow+1)
   50 l=l-1
      if (nodplc(ilr+l).ne.icol) go to 50
      ispot=lyl+l
      ispot2=imynl+nstop+nut+l
      go to 70
c
c  find (icol,irow) matrix term (diagonal)
c
   60 ispot=lynl+nodplc(iorder+irow)
      ispot2=imynl+nodplc(iorder+irow)
c
   70 call cmult(value(lyl+j),value(imynl+nstop+nut+j),
     1  value(lyu+k),value(imynl+nstop+k),xreal,ximag)
      value(ispot)=value(ispot)-xreal
      value(ispot2)=value(ispot2)-ximag
   80 continue
   90 continue
  100 continue
      return
      end
      subroutine acsol
      implicit double precision (a-h,o-z)
c
c     this routine solves the circuit equations by performing a forward
c and backward substitution using the previously-computed lu factors.
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
c  forward substitution
c
      do 20 i=2,nstop
      jstart=nodplc(ilc+i)
      jstop=nodplc(ilc+i+1)-1
      if (jstart.gt.jstop) go to 20
      io=nodplc(iorder+i)
      if (value(lvn+io).ne.0.0d0) go to 5
      if (value(imvn+io).eq.0.0d0) go to 20
    5 do 10 j=jstart,jstop
      jo=nodplc(ilr+j)
      jo=nodplc(iorder+jo)
      call cmult(value(lyl+j),value(imynl+nstop+nut+j),value(lvn+io),
     1  value(imvn+io),xreal,ximag)
      value(lvn+jo)=value(lvn+jo)-xreal
      value(imvn+jo)=value(imvn+jo)-ximag
   10 continue
   20 continue
c
c  back substitution
c
      k=nstop+1
      do 50 i=2,nstop
      k=k-1
      io=nodplc(iorder+k)
      jstart=nodplc(iur+k)
      jstop=nodplc(iur+k+1)-1
      if (jstart.gt.jstop) go to 40
      do 30 j=jstart,jstop
      jo=nodplc(iuc+j)
      jo=nodplc(iorder+jo)
      call cmult(value(lyu+j),value(imynl+nstop+j),value(lvn+jo),
     1  value(imvn+jo),xreal,ximag)
      value(lvn+io)=value(lvn+io)-xreal
      value(imvn+io)=value(imvn+io)-ximag
   30 continue
   40 call cdiv(value(lvn+io),value(imvn+io),value(lynl+io),
     1  value(imynl+io),value(lvn+io),value(imvn+io))
   50 continue
      do 100 i=2,nstop
      cvalue(lcvn+i)=dcmplx(value(lvn+i),value(imvn+i))
  100 continue
      cvalue(lcvn+1)=dcmplx(0.0d0,0.0d0)
      return
      end
      subroutine acload
      implicit double precision (a-h,o-z)
c
c     this routine zeroes-out and then loads the complex coefficient
c matrix.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      complex*16 cval
c
c  zero y matrix and current vector
c
      call zero8(value(lvn+1),nstop)
      call zero8(value(imvn+1),nstop)
      call zero8(value(lynl+1),nstop+nut+nlt)
      call zero8(value(imynl+1),nstop+nut+nlt)
c
c  resistors
c
      loc=locate(1)
   20 if (loc.eq.0) go to 30
      locv=nodplc(loc+1)
      val=value(locv+1)
      locy=lynl+nodplc(loc+6)
      value(locy)=value(locy)+val
      locy=lynl+nodplc(loc+7)
      value(locy)=value(locy)+val
      locy=lynl+nodplc(loc+4)
      value(locy)=value(locy)-val
      locy=lynl+nodplc(loc+5)
      value(locy)=value(locy)-val
      loc=nodplc(loc)
      go to 20
c
c  capacitors
c
   30 loc=locate(2)
   40 if (loc.eq.0) go to 50
      locv=nodplc(loc+1)
      val=omega*value(locv+1)
      locyi=imynl+nodplc(loc+10)
      value(locyi)=value(locyi)+val
      locyi=imynl+nodplc(loc+11)
      value(locyi)=value(locyi)+val
      locyi=imynl+nodplc(loc+5)
      value(locyi)=value(locyi)-val
      locyi=imynl+nodplc(loc+6)
      value(locyi)=value(locyi)-val
      loc=nodplc(loc)
      go to 40
c
c  inductors
c
   50 loc=locate(3)
   60 if (loc.eq.0) go to 70
      locv=nodplc(loc+1)
      val=omega*value(locv+1)
      locyi=imynl+nodplc(loc+13)
      locy=lynl+nodplc(loc+13)
      value(locy)=0.0d0
      value(locyi)=-val
      locy=lynl+nodplc(loc+6)
      locyi=imynl+nodplc(loc+6)
      value(locy)=1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+7)
      locyi=imynl+nodplc(loc+7)
      value(locy)=-1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+8)
      locyi=imynl+nodplc(loc+8)
      value(locy)=1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+9)
      locyi=imynl+nodplc(loc+9)
      value(locy)=-1.0d0
      value(locyi)=0.0d0
      loc=nodplc(loc)
      go to 60
c
c  mutual inductors
c
   70 loc=locate(4)
   80 if (loc.eq.0) go to 90
      locv=nodplc(loc+1)
      val=omega*value(locv+1)
      locy=lynl+nodplc(loc+4)
      locyi=imynl+nodplc(loc+4)
      value(locy)=0.0d0
      value(locyi)=-val
      locy=lynl+nodplc(loc+5)
      locyi=imynl+nodplc(loc+5)
      value(locy)=0.0d0
      value(locyi)=-val
      loc=nodplc(loc)
      go to 80
c
c  nonlinear voltage controlled current sources
c
   90 loc=locate(5)
   95 if (loc.eq.0) go to 100
      ndim=nodplc(loc+4)
      lmat=nodplc(loc+7)
      loct=lx0+nodplc(loc+12)+2
      do 97 i=1,ndim
      val=value(loct)
      loct=loct+2
      locy=lynl+nodplc(lmat+1)
      value(locy)=value(locy)+val
      locy=lynl+nodplc(lmat+2)
      value(locy)=value(locy)-val
      locy=lynl+nodplc(lmat+3)
      value(locy)=value(locy)-val
      locy=lynl+nodplc(lmat+4)
      value(locy)=value(locy)+val
      lmat=lmat+4
   97 continue
      loc=nodplc(loc)
      go to 95
c
c  nonlinear voltage controlled voltage sources
c
  100 loc=locate(6)
  105 if (loc.eq.0) go to 110
      ndim=nodplc(loc+4)
      lmat=nodplc(loc+8)
      loct=lx0+nodplc(loc+13)+3
      locy=lynl+nodplc(lmat+1)
      locyi=imynl+nodplc(lmat+1)
      value(locy)=+1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(lmat+2)
      locyi=imynl+nodplc(lmat+2)
      value(locy)=-1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(lmat+3)
      locyi=imynl+nodplc(lmat+3)
      value(locy)=+1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(lmat+4)
      locyi=imynl+nodplc(lmat+4)
      value(locy)=-1.0d0
      value(locyi)=0.0d0
      lmat=lmat+4
      do 107 i=1,ndim
      val=value(loct)
      loct=loct+2
      locy=lynl+nodplc(lmat+1)
      value(locy)=value(locy)-val
      locy=lynl+nodplc(lmat+2)
      value(locy)=value(locy)+val
      lmat=lmat+2
  107 continue
      loc=nodplc(loc)
      go to 105
c
c  nonlinear current controlled current sources
c
  110 loc=locate(7)
  115 if (loc.eq.0) go to 120
      ndim=nodplc(loc+4)
      lmat=nodplc(loc+7)
      loct=lx0+nodplc(loc+12)+2
      do 117 i=1,ndim
      val=value(loct)
      loct=loct+2
      locy=lynl+nodplc(lmat+1)
      locyi=imynl+nodplc(lmat+1)
      value(locy)=+val
      value(locyi)=0.0d0
      locy=lynl+nodplc(lmat+2)
      locyi=imynl+nodplc(lmat+2)
      value(locy)=-val
      value(locyi)=0.0d0
      lmat=lmat+2
  117 continue
      loc=nodplc(loc)
      go to 115
c
c  nonlinear current controlled voltage sources
c
  120 loc=locate(8)
  125 if (loc.eq.0) go to 140
      ndim=nodplc(loc+4)
      lmat=nodplc(loc+8)
      loct=lx0+nodplc(loc+13)+3
      locy=lynl+nodplc(lmat+1)
      locyi=imynl+nodplc(lmat+1)
      value(locy)=+1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(lmat+2)
      locyi=imynl+nodplc(lmat+2)
      value(locy)=-1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(lmat+3)
      locyi=imynl+nodplc(lmat+3)
      value(locy)=+1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(lmat+4)
      locyi=imynl+nodplc(lmat+4)
      value(locy)=-1.0d0
      value(locyi)=0.0d0
      lmat=lmat+4
      do 127 i=1,ndim
      val=value(loct)
      loct=loct+2
      locy=lynl+nodplc(lmat+i)
      value(locy)=value(locy)-val
  127 continue
      loc=nodplc(loc)
      go to 125
c
c  voltage sources
c
  140 loc=locate(9)
  150 if (loc.eq.0) go to 160
      locv=nodplc(loc+1)
      iptr=nodplc(loc+6)
      value(lvn+iptr)=value(locv+2)
      value(imvn+iptr)=value(locv+3)
      locy=lynl+nodplc(loc+7)
      value(locy)=value(locy)+1.0d0
      locy=lynl+nodplc(loc+8)
      value(locy)=value(locy)-1.0d0
      locy=lynl+nodplc(loc+9)
      value(locy)=value(locy)+1.0d0
      locy=lynl+nodplc(loc+10)
      value(locy)=value(locy)-1.0d0
      loc=nodplc(loc)
      go to 150
c
c  current sources
c
  160 loc=locate(10)
  170 if (loc.eq.0) go to 200
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      value(lvn+node1)=value(lvn+node1)-value(locv+2)
      value(imvn+node1)=value(imvn+node1)-value(locv+3)
      value(lvn+node2)=value(lvn+node2)+value(locv+2)
      value(imvn+node2)=value(imvn+node2)+value(locv+3)
      loc=nodplc(loc)
      go to 170
c
c  diodes
c
  200 loc=locate(11)
  210 if (loc.eq.0) go to 250
      locv=nodplc(loc+1)
      area=value(locv+1)
      locm=nodplc(loc+5)
      locm=nodplc(locm+1)
      loct=lx0+nodplc(loc+11)
      gspr=value(locm+2)*area
      geq=value(loct+2)
      xceq=value(loct+4)*omega
      locy=lynl+nodplc(loc+13)
      value(locy)=value(locy)+gspr
      locy=lynl+nodplc(loc+14)
      locyi=imynl+nodplc(loc+14)
      value(locy)=value(locy)+geq
      value(locyi)=value(locyi)+xceq
      locy=lynl+nodplc(loc+15)
      locyi=imynl+nodplc(loc+15)
      value(locy)=value(locy)+geq+gspr
      value(locyi)=value(locyi)+xceq
      locy=lynl+nodplc(loc+7)
      value(locy)=value(locy)-gspr
      locy=lynl+nodplc(loc+8)
      locyi=imynl+nodplc(loc+8)
      value(locy)=value(locy)-geq
      value(locyi)=value(locyi)-xceq
      locy=lynl+nodplc(loc+9)
      value(locy)=value(locy)-gspr
      locy=lynl+nodplc(loc+10)
      locyi=imynl+nodplc(loc+10)
      value(locy)=value(locy)-geq
      value(locyi)=value(locyi)-xceq
      loc=nodplc(loc)
      go to 210
c
c  bjts
c
  250 loc=locate(12)
  260 if (loc.eq.0) go to 300
      locv=nodplc(loc+1)
      area=value(locv+1)
      locm=nodplc(loc+8)
      locm=nodplc(locm+1)
      loct=lx0+nodplc(loc+22)
      gcpr=value(locm+20)*area
      gepr=value(locm+19)*area
      gpi=value(loct+4)
      gmu=value(loct+5)
      gm=value(loct+6)
      go=value(loct+7)
      xgm=0.0d0
      td=value(locm+28)
      if(td.eq.0.0d0) go to 270
      arg=td*omega
      gm=gm+go
      xgm=-gm*dsin(arg)
      gm=gm*dcos(arg)-go
  270 gx=value(loct+16)
      xcpi=value(loct+9)*omega
      xcmu=value(loct+11)*omega
      xcbx=value(loct+15)*omega
      xccs=value(loct+13)*omega
      xcmcb=value(loct+17)*omega
      locy=lynl+nodplc(loc+24)
      value(locy)=value(locy)+gcpr
      locy=lynl+nodplc(loc+25)
      locyi=imynl+nodplc(loc+25)
      value(locy)=value(locy)+gx
      value(locyi)=value(locyi)+xcbx
      locy=lynl+nodplc(loc+26)
      value(locy)=value(locy)+gepr
      locy=lynl+nodplc(loc+27)
      locyi=imynl+nodplc(loc+27)
      value(locy)=value(locy)+gmu+go+gcpr
      value(locyi)=value(locyi)+xcmu+xccs+xcbx
      locy=lynl+nodplc(loc+28)
      locyi=imynl+nodplc(loc+28)
      value(locy)=value(locy)+gx+gpi+gmu
      value(locyi)=value(locyi)+xcpi+xcmu+xcmcb
      locy=lynl+nodplc(loc+29)
      locyi=imynl+nodplc(loc+29)
      value(locy)=value(locy)+gpi+gepr+gm+go
      value(locyi)=value(locyi)+xcpi+xgm
      locy=lynl+nodplc(loc+10)
      value(locy)=value(locy)-gcpr
      locy=lynl+nodplc(loc+11)
      value(locy)=value(locy)-gx
      locy=lynl+nodplc(loc+12)
      value(locy)=value(locy)-gepr
      locy=lynl+nodplc(loc+13)
      value(locy)=value(locy)-gcpr
      locy=lynl+nodplc(loc+14)
      locyi=imynl+nodplc(loc+14)
      value(locy)=value(locy)-gmu+gm
      value(locyi)=value(locyi)-xcmu+xgm
      locy=lynl+nodplc(loc+15)
      locyi=imynl+nodplc(loc+15)
      value(locy)=value(locy)-gm-go
      value(locyi)=value(locyi)-xgm
      locy=lynl+nodplc(loc+16)
      value(locy)=value(locy)-gx
      locy=lynl+nodplc(loc+17)
      locyi=imynl+nodplc(loc+17)
      value(locy)=value(locy)-gmu
      value(locyi)=value(locyi)-xcmu-xcmcb
      locy=lynl+nodplc(loc+18)
      locyi=imynl+nodplc(loc+18)
      value(locy)=value(locy)-gpi
      value(locyi)=value(locyi)-xcpi
      locy=lynl+nodplc(loc+19)
      value(locy)=value(locy)-gepr
      locy=lynl+nodplc(loc+20)
      locyi=imynl+nodplc(loc+20)
      value(locy)=value(locy)-go
      value(locyi)=value(locyi)+xcmcb
      locy=lynl+nodplc(loc+21)
      locyi=imynl+nodplc(loc+21)
      value(locy)=value(locy)-gpi-gm
      value(locyi)=value(locyi)-xcpi-xgm-xcmcb
      locyi=imynl+nodplc(loc+31)
      value(locyi)=value(locyi)+xccs
      locyi=imynl+nodplc(loc+32)
      value(locyi)=value(locyi)-xccs
      locyi=imynl+nodplc(loc+33)
      value(locyi)=value(locyi)-xccs
      locyi=imynl+nodplc(loc+34)
      value(locyi)=value(locyi)-xcbx
      locyi=imynl+nodplc(loc+35)
      value(locyi)=value(locyi)-xcbx
      loc=nodplc(loc)
      go to 260
c
c  jfets
c
  300 loc=locate(13)
  310 if (loc.eq.0) go to 350
      locv=nodplc(loc+1)
      area=value(locv+1)
      locm=nodplc(loc+7)
      locm=nodplc(locm+1)
      loct=lx0+nodplc(loc+19)
      gdpr=value(locm+4)*area
      gspr=value(locm+5)*area
      gm=value(loct+5)
      gds=value(loct+6)
      ggs=value(loct+7)
      xgs=value(loct+9)*omega
      ggd=value(loct+8)
      xgd=value(loct+11)*omega
      locy=lynl+nodplc(loc+20)
      value(locy)=value(locy)+gdpr
      locy=lynl+nodplc(loc+21)
      locyi=imynl+nodplc(loc+21)
      value(locy)=value(locy)+ggd+ggs
      value(locyi)=value(locyi)+xgd+xgs
      locy=lynl+nodplc(loc+22)
      value(locy)=value(locy)+gspr
      locy=lynl+nodplc(loc+23)
      locyi=imynl+nodplc(loc+23)
      value(locy)=value(locy)+gdpr+gds+ggd
      value(locyi)=value(locyi)+xgd
      locy=lynl+nodplc(loc+24)
      locyi=imynl+nodplc(loc+24)
      value(locy)=value(locy)+gspr+gds+gm+ggs
      value(locyi)=value(locyi)+xgs
      locy=lynl+nodplc(loc+9)
      value(locy)=value(locy)-gdpr
      locy=lynl+nodplc(loc+10)
      locyi=imynl+nodplc(loc+10)
      value(locy)=value(locy)-ggd
      value(locyi)=value(locyi)-xgd
      locy=lynl+nodplc(loc+11)
      locyi=imynl+nodplc(loc+11)
      value(locy)=value(locy)-ggs
      value(locyi)=value(locyi)-xgs
      locy=lynl+nodplc(loc+12)
      value(locy)=value(locy)-gspr
      locy=lynl+nodplc(loc+13)
      value(locy)=value(locy)-gdpr
      locy=lynl+nodplc(loc+14)
      locyi=imynl+nodplc(loc+14)
      value(locy)=value(locy)-ggd+gm
      value(locyi)=value(locyi)-xgd
      locy=lynl+nodplc(loc+15)
      value(locy)=value(locy)-gds-gm
      locy=lynl+nodplc(loc+16)
      locyi=imynl+nodplc(loc+16)
      value(locy)=value(locy)-ggs-gm
      value(locyi)=value(locyi)-xgs
      locy=lynl+nodplc(loc+17)
      value(locy)=value(locy)-gspr
      locy=lynl+nodplc(loc+18)
      value(locy)=value(locy)-gds
      loc=nodplc(loc)
      go to 310
c
c  mosfets
c
  350 loc=locate(14)
  360 if (loc.eq.0) go to 400
      locv=nodplc(loc+1)
      locm=nodplc(loc+8)
      itype=nodplc(locm+2)
      locm=nodplc(locm+1)
      loct=lx0+nodplc(loc+26)
      gdpr=value(locv+11)
      gspr=value(locv+12)
      if(itype.eq.0) go to 380
      xl=value(locv+1)-2.0d0*value(locm+20)
      xw=value(locv+2)-2.0d0*value(locm+36)
      covlgs=value(locm+8)*xw
      covlgd=value(locm+9)*xw
      covlgb=value(locm+10)*xl
      didvg=value(loct)
      didvd=value(loct+1)
      didvs=value(loct+2)
      geqbd=value(loct+3)
      geqbs=value(loct+5)
      ccgg=value(loct+8)
      ccgd=value(loct+9)
      ccgs=value(loct+10)
      ccbg=value(loct+11)
      ccbd=value(loct+12)
      ccbs=value(loct+13)
      capbd=value(loct+14)
      capbs=value(loct+15)
      xccg2=-0.5d0*(ccgg+ccbg)*omega
      xccd2=-0.5d0*(ccgd+ccbd)*omega
      xccs2=-0.5d0*(ccgs+ccbs)*omega
      xccdg=xccg2-covlgd*omega
      xccdd=xccd2+(capbd+covlgd)*omega
      xccds=xccs2
      xccsg=xccg2-covlgs*omega
      xccsd=xccd2
      xccss=xccs2+(capbs+covlgs)*omega
      xccgg=(ccgg+covlgd+covlgs+covlgb)*omega
      xccgd=(ccgd-covlgd)*omega
      xccgs=(ccgs-covlgs)*omega
      xccbg=(ccbg-covlgb)*omega
      xccbd=xccbd+(ccbd-capbd)*omega
      xccbs=xccbs+(ccbs-capbs)*omega
      gccdg=didvg
      gccdd=geqbd+didvd
      gccds=didvs
      gccsg=-didvg
      gccsd=-didvd
      gccss=geqbs-didvs
      gccbd=-geqbd
      gccbs=-geqbs
      locyi=imynl+nodplc(loc+28)
      value(locyi)=value(locyi)+xccgg
      locyi=imynl+nodplc(loc+30)
      value(locyi)=value(locyi)-xccbg-xccbd-xccbs
      locyi=imynl+nodplc(loc+31)
      value(locyi)=value(locyi)+xccdd
      locyi=imynl+nodplc(loc+32)
      value(locyi)=value(locyi)+xccss
      locyi=imynl+nodplc(loc+11)
      value(locyi)=value(locyi)-xccgg-xccgd-xccgs
      locyi=imynl+nodplc(loc+12)
      value(locyi)=value(locyi)+xccgd
      locyi=imynl+nodplc(loc+13)
      value(locyi)=value(locyi)+xccgs
      locyi=imynl+nodplc(loc+15)
      value(locyi)=value(locyi)+xccbg
      locyi=imynl+nodplc(loc+16)
      value(locyi)=value(locyi)+xccbd
      locyi=imynl+nodplc(loc+17)
      value(locyi)=value(locyi)+xccbs
      locyi=imynl+nodplc(loc+19)
      value(locyi)=value(locyi)+xccdg
      locyi=imynl+nodplc(loc+20)
      value(locyi)=value(locyi)-xccdg-xccdd-xccds
      locyi=imynl+nodplc(loc+21)
      value(locyi)=value(locyi)+xccds
      locyi=imynl+nodplc(loc+22)
      value(locyi)=value(locyi)+xccsg
      locyi=imynl+nodplc(loc+24)
      value(locyi)=value(locyi)-xccsg-xccsd-xccss
      locyi=imynl+nodplc(loc+25)
      value(locyi)=value(locyi)+xccsd
      locy=lynl+nodplc(loc+27)
      value(locy)=value(locy)+gdpr
      locy=lynl+nodplc(loc+29)
      value(locy)=value(locy)+gspr
      locy=lynl+nodplc(loc+30)
      value(locy)=value(locy)-gccbd-gccbs
      locy=lynl+nodplc(loc+31)
      value(locy)=value(locy)+gdpr+gccdd
      locy=lynl+nodplc(loc+32)
      value(locy)=value(locy)+gspr+gccss
      locy=lynl+nodplc(loc+10)
      value(locy)=value(locy)-gdpr
      locy=lynl+nodplc(loc+14)
      value(locy)=value(locy)-gspr
      locy=lynl+nodplc(loc+16)
      value(locy)=value(locy)+gccbd
      locy=lynl+nodplc(loc+17)
      value(locy)=value(locy)+gccbs
      locy=lynl+nodplc(loc+18)
      value(locy)=value(locy)-gdpr
      locy=lynl+nodplc(loc+19)
      value(locy)=value(locy)+gccdg
      locy=lynl+nodplc(loc+20)
      value(locy)=value(locy)-gccdg-gccdd-gccds
      locy=lynl+nodplc(loc+21)
      value(locy)=value(locy)+gccds
      locy=lynl+nodplc(loc+22)
      value(locy)=value(locy)+gccsg
      locy=lynl+nodplc(loc+23)
      value(locy)=value(locy)-gspr
      locy=lynl+nodplc(loc+24)
      value(locy)=value(locy)-gccsg-gccsd-gccss
      locy=lynl+nodplc(loc+25)
      value(locy)=value(locy)+gccsd
      loc=nodplc(loc)
      go to 360
c... ga-as fets
  380 continue
      devmod=value(locv+8)
      ggd=value(loct+6)
      gm=0.0d0
      gds=0.0d0
      gmj1=value(loct+7)
      gdb=value(loct+8)
      ggs=value(loct+9)
      xcds=value(loct+10)*omega
      gsb=value(loct+11)
      xcgs=value(loct+12)*omega
      gmj2=value(loct+13)
      xcgd=value(loct+14)*omega
      xcgb=value(loct+16)*omega
c     write(6,1001) ggd,gm,gds,gmj1,gdb,ggs,gsb,gmj2
 1001 format(' ggd gm gds gmj1 gdb ggs gsb gmj2'/,1x,1p8e9.1)
      if(devmod.gt.0.0d0) go to 385
      gmrev=gm
      gmnrm=0.0d0
      gm=0.0d0
      gmj1r=gmj1
      gmj1=0.0d0
      gmj1n=0.0d0
      gmj2n=0.0d0
      gmj2r=0.0d0
      go to 390
  385 gmnrm=gm
      gmrev=0.0d0
      gm=0.0d0
      gmj2n=gmj2
      gmj2r=0.0d0
      gmj2=0.0d0
      gmj1r=0.0d0
      gmj1n=0.0d0
  390 locy=lynl+nodplc(loc+27)
      value(locy)=value(locy)+gdpr
      locy=lynl+nodplc(loc+28)
      locyi=imynl+nodplc(loc+28)
      value(locy)=value(locy)+ggd+ggs
      value(locyi)=value(locyi)+xcgd+xcgs+xcgb
      locy=lynl+nodplc(loc+29)
      value(locy)=value(locy)+gspr
      locy=lynl+nodplc(loc+30)
      locyi=imynl+nodplc(loc+30)
      value(locy)=value(locy)+gdb+gsb+gmj1+gmj2
      value(locyi)=value(locyi)+xcgb
      locy=lynl+nodplc(loc+31)
      locyi=imynl+nodplc(loc+31)
      value(locy)=value(locy)+gdpr+gds+gdb+ggd-gmrev-gmj1r
      value(locyi)=value(locyi)+xcds+xcgd
      locy=lynl+nodplc(loc+32)
      locyi=imynl+nodplc(loc+32)
      value(locy)=value(locy)+gspr+gds+gsb+ggs+gmnrm-gmj2n
      value(locyi)=value(locyi)+xcds+xcgs
      locy=lynl+nodplc(loc+10)
      value(locy)=value(locy)-gdpr
      locyi=imynl+nodplc(loc+11)
      value(locyi)=value(locyi)-xcgb
      locy=lynl+nodplc(loc+12)
      locyi=imynl+nodplc(loc+12)
      value(locy)=value(locy)-ggd
      value(locyi)=value(locyi)-xcgd
      locy=lynl+nodplc(loc+13)
      locyi=imynl+nodplc(loc+13)
      value(locy)=value(locy)-ggs
      value(locyi)=value(locyi)-xcgs
      locy=lynl+nodplc(loc+14)
      value(locy)=value(locy)-gspr
      locy=lynl+nodplc(loc+15)
      locyi=imynl+nodplc(loc+15)
      value(locy)=value(locy)-gmj2n-gmj1n-gmj1r-gmj2r-gmj1-gmj2
      value(locyi)=value(locyi)-xcgb
      locy=lynl+nodplc(loc+16)
      value(locy)=value(locy)-gdb+gmj2r+gmj1r
      locy=lynl+nodplc(loc+17)
      value(locy)=value(locy)-gsb+gmj1n+gmj2n
      locy=lynl+nodplc(loc+18)
      value(locy)=value(locy)-gdpr
      locy=lynl+nodplc(loc+19)
      locyi=imynl+nodplc(loc+19)
      value(locy)=value(locy)-ggd+gmnrm+gmrev+gmj1r+gmj1n+gmj1+gm
      value(locyi)=value(locyi)-xcgd
      locy=lynl+nodplc(loc+20)
      value(locy)=value(locy)-gdb-gmj1-gm
      locy=lynl+nodplc(loc+21)
      locyi=imynl+nodplc(loc+21)
      value(locy)=value(locy)-gds-gmnrm-gmj1n
      value(locyi)=value(locyi)-xcds
      locy=lynl+nodplc(loc+22)
      locyi=imynl+nodplc(loc+22)
      value(locy)=value(locy)-ggs-gmnrm-gmrev+gmj2r+gmj2n+gmj2-gm
      value(locyi)=value(locyi)-xcgs
      locy=lynl+nodplc(loc+23)
      value(locy)=value(locy)-gspr
      locy=lynl+nodplc(loc+24)
      value(locy)=value(locy)-gsb-gmj2+gm
      locy=lynl+nodplc(loc+25)
      locyi=imynl+nodplc(loc+25)
      value(locy)=value(locy)-gds+gmrev-gmj2r
      value(locyi)=value(locyi)-xcds
      loc=nodplc(loc)
      go to 360
c
c  transmission lines
c
  400 loc=locate(17)
  410 if (loc.eq.0) go to 1000
      locv=nodplc(loc+1)
      z0=value(locv+1)
      y0=1.0d0/z0
      td=value(locv+2)
      arg=-omega*td
      rval=dcos(arg)
      xval=dsin(arg)
      locy=lynl+nodplc(loc+10)
      value(locy)=value(locy)+y0
      locy=lynl+nodplc(loc+11)
      locyi=imynl+nodplc(loc+11)
      value(locy)=-y0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+12)
      locyi=imynl+nodplc(loc+12)
      value(locy)=-1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+13)
      value(locy)=value(locy)+y0
      locy=lynl+nodplc(loc+14)
      locyi=imynl+nodplc(loc+14)
      value(locy)=-1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+15)
      locyi=imynl+nodplc(loc+15)
      value(locy)=-y0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+16)
      locyi=imynl+nodplc(loc+16)
      value(locy)=+y0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+17)
      locyi=imynl+nodplc(loc+17)
      value(locy)=+1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+18)
      locyi=imynl+nodplc(loc+18)
      value(locy)=+y0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+19)
      locyi=imynl+nodplc(loc+19)
      value(locy)=+1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+20)
      locyi=imynl+nodplc(loc+20)
      value(locy)=-1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+21)
      locyi=imynl+nodplc(loc+21)
      value(locy)=-rval
      value(locyi)=-xval
      locy=lynl+nodplc(loc+22)
      locyi=imynl+nodplc(loc+22)
      value(locy)=+rval
      value(locyi)=+xval
      locy=lynl+nodplc(loc+23)
      locyi=imynl+nodplc(loc+23)
      value(locy)=+1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+24)
      locyi=imynl+nodplc(loc+24)
      value(locy)=-rval*z0
      value(locyi)=-xval*z0
      locy=lynl+nodplc(loc+25)
      locyi=imynl+nodplc(loc+25)
      value(locy)=-rval
      value(locyi)=-xval
      locy=lynl+nodplc(loc+26)
      locyi=imynl+nodplc(loc+26)
      value(locy)=+rval
      value(locyi)=+xval
      locy=lynl+nodplc(loc+27)
      locyi=imynl+nodplc(loc+27)
      value(locy)=-1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+28)
      locyi=imynl+nodplc(loc+28)
      value(locy)=+1.0d0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+29)
      locyi=imynl+nodplc(loc+29)
      value(locy)=-rval*z0
      value(locyi)=-xval*z0
      locy=lynl+nodplc(loc+31)
      locyi=imynl+nodplc(loc+31)
      value(locy)=-y0
      value(locyi)=0.0d0
      locy=lynl+nodplc(loc+32)
      locyi=imynl+nodplc(loc+32)
      value(locy)=-y0
      value(locyi)=0.0d0
      loc=nodplc(loc)
      go to 410
c
c  reorder right-hand side
c
 1000 do 1010 i=2,nstop
      j=nodplc(iswap+i)
      value(ndiag+i)=value(lvn+j)
      value(ndiag+i+nstop)=value(imvn+j)
 1010 continue
      call copy8(value(ndiag+1),value(lvn+1),nstop)
      call copy8(value(ndiag+nstop+1),value(imvn+1),nstop)
c
c  finished
c
      return
      end
      subroutine noise(loco)
      implicit double precision (a-h,o-z)
c
c     this routine computes the noise due to various circuit elements.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /miscel/ atime,aprog(3),adate,atitle(10),defl,defw,defad,
     1  defas,rstats(50),iwidth,lwidth,nopage
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /ac/ fstart,fstop,fincr,skw2,refprl,spw2,jacflg,idfreq,
     1   inoise,nosprt,nosout,nosin,idist,idprt
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension vno1(12),vno2(12),vno3(12),vno4(12),vno5(12),vno6(12)
      dimension vntot(12),anam(12),string(5)
      dimension titln(4),v(2)
      dimension afmt1(3),afmt2(3)
      complex*16 cval,c(1)
      equivalence (c(1),v(1),cval)
      equivalence (v(1),vreal),(v(2),vimag)
      data titln / 8hnoise an, 8halysis  , 8h        , 8h         /
      data alsrb,alsrc,alsre,alsrs,alsrd / 2hrb,2hrc,2hre,2hrs,2hrd /
      data alsib,alsic,alsid,alsfn / 2hib,2hic,2hid,2hfn /
      data alstot / 5htotal /
      data aslash,ablnk / 1h/, 1h  /
      data afmt1 /8h(////,11,8hx,  (2x,,8ha8))    /
      data afmt2 /8h(1h0,a8,,8h1p  d10.,8h3)      /
      kntr=12
      if(lwidth.le.80) kntr=7
      ipos=11
      call move(afmt1,ipos,ablnk,1,2)
      call alfnum(kntr,afmt1,ipos)
      ipos=11
      call move(afmt2,ipos,ablnk,1,2)
      call alfnum(kntr,afmt2,ipos)
      nprnt=0
      freq=omega/twopi
      if (icalc.ge.2) go to 10
      fourkt=4.0d0*charge*vt
      twoq=2.0d0*charge
      noposo=nodplc(nosout+2)
      nonego=nodplc(nosout+3)
      kntlim=lwidth/11
      nkntr=1
   10 if (nosprt.eq.0) go to 30
      if (nkntr.gt.icalc) go to 30
      nprnt=1
      nkntr=nkntr+nosprt
      call title(0,lwidth,1,titln)
      write (6,16) freq
   16 format('0    frequency = ',1pd10.3,' hz'/)
c
c  obtain adjoint circuit solution
c
   30 vnrms=0.0d0
      cval=cvalue(lcvn+noposo)-cvalue(lcvn+nonego)
      vout=dsqrt(vreal*vreal+vimag*vimag)
      vout=dmax1(vout,1.0d-20)
      call zero8(value(lvn+1),nstop)
      call zero8(value(imvn+1),nstop)
      value(lvn+noposo)=-1.0d0
      value(lvn+nonego)=+1.0d0
      call acasol
c
c  resistors
c
      if (jelcnt(1).eq.0) go to 200
      ititle=0
   91 format(//'0**** resistor squared noise voltages (sq v/hz)')
  100 loc=locate(1)
      kntr=0
  110 if (loc.eq.0) go to 130
      kntr=kntr+1
      locv=nodplc(loc+1)
      anam(kntr)=value(locv)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      cval=cvalue(lcvn+node1)-cvalue(lcvn+node2)
      vntot(kntr)=(vreal*vreal+vimag*vimag)*fourkt*value(locv+1)
      vnrms=vnrms+vntot(kntr)
      if (kntr.ge.kntlim) go to 140
  120 loc=nodplc(loc)
      go to 110
  130 if (kntr.eq.0) go to 200
  140 if (nprnt.eq.0) go to 160
      if (ititle.eq.0) write (6,91)
      ititle=1
      write (6,afmt1) (anam(i),i=1,kntr)
      write (6,afmt2) alstot,(vntot(i),i=1,kntr)
  160 kntr=0
      if (loc.ne.0) go to 120
c
c  diodes
c
  200 if (jelcnt(11).eq.0) go to 300
      ititle=0
  201 format(//'0**** diode squared noise voltages (sq v/hz)')
  210 loc=locate(11)
      kntr=0
  220 if (loc.eq.0) go to 240
      kntr=kntr+1
      locv=nodplc(loc+1)
      anam(kntr)=value(locv)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      locm=nodplc(loc+5)
      locm=nodplc(locm+1)
      loct=nodplc(loc+11)
      area=value(locv+1)
      fnk=value(locm+10)
      fna=value(locm+11)
c
c  ohmic resistance
c
      cval=cvalue(lcvn+node1)-cvalue(lcvn+node3)
      vno1(kntr)=(vreal*vreal+vimag*vimag)*fourkt*value(locm+2)*area
c
c  junction shot noise and flicker noise
c
      cval=cvalue(lcvn+node3)-cvalue(lcvn+node2)
      vtemp=vreal*vreal+vimag*vimag
      arg=dmax1(dabs(value(lx0+loct+1)),1.0d-20)
      vno2(kntr)=vtemp*twoq*arg
      vno3(kntr)=vtemp*fnk*dexp(fna*dlog(arg))/freq
      vntot(kntr)=vno1(kntr)+vno2(kntr)+vno3(kntr)
      vnrms=vnrms+vntot(kntr)
      if (kntr.ge.kntlim) go to 250
  230 loc=nodplc(loc)
      go to 220
  240 if (kntr.eq.0) go to 300
  250 if (nprnt.eq.0) go to 260
      if (ititle.eq.0) write (6,201)
      ititle=1
      write (6,afmt1) (anam(i),i=1,kntr)
      write (6,afmt2) alsrs,(vno1(i),i=1,kntr)
      write (6,afmt2) alsid,(vno2(i),i=1,kntr)
      write (6,afmt2) alsfn,(vno3(i),i=1,kntr)
      write (6,afmt2) alstot,(vntot(i),i=1,kntr)
  260 kntr=0
      if (loc.ne.0) go to 230
c
c  bipolar junction transistors
c
  300 if (jelcnt(12).eq.0) go to 400
      ititle=0
  301 format(//'0**** transistor squared noise voltages (sq v/hz)')
  310 loc=locate(12)
      kntr=0
  320 if (loc.eq.0) go to 340
      kntr=kntr+1
      locv=nodplc(loc+1)
      anam(kntr)=value(locv)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      node6=nodplc(loc+7)
      locm=nodplc(loc+8)
      locm=nodplc(locm+1)
      loct=nodplc(loc+22)
      area=value(locv+1)
      fnk=value(locm+44)
      fna=value(locm+45)
c
c  extrinsic resistances
c
c...  base resistance
      cval=cvalue(lcvn+node2)-cvalue(lcvn+node5)
      vno1(kntr)=(vreal*vreal+vimag*vimag)*fourkt*value(lx0+loct+16)
     1  *area
c...  collector resistance
      cval=cvalue(lcvn+node1)-cvalue(lcvn+node4)
      vno2(kntr)=(vreal*vreal+vimag*vimag)*fourkt*value(locm+20)*area
c...  emitter resistance
      cval=cvalue(lcvn+node3)-cvalue(lcvn+node6)
      vno3(kntr)=(vreal*vreal+vimag*vimag)*fourkt*value(locm+19)*area
c
c  base current shot noise and flicker noise
c
      cval=cvalue(lcvn+node5)-cvalue(lcvn+node6)
      vtemp=vreal*vreal+vimag*vimag
      arg=dmax1(dabs(value(lx0+loct+3)),1.0d-20)
      vno4(kntr)=vtemp*twoq*arg
      vno5(kntr)=vtemp*fnk*dexp(fna*dlog(arg))/freq
c
c  collector current shot noise
c
      cval=cvalue(lcvn+node4)-cvalue(lcvn+node6)
      vno6(kntr)=(vreal*vreal+vimag*vimag)*twoq*dabs(value(lx0+loct+2))
      vntot(kntr)=vno1(kntr)+vno2(kntr)+vno3(kntr)+vno4(kntr)+vno5(kntr)
     1   +vno6(kntr)
      vnrms=vnrms+vntot(kntr)
      if (kntr.ge.kntlim) go to 350
  330 loc=nodplc(loc)
      go to 320
  340 if (kntr.eq.0) go to 400
  350 if (nprnt.eq.0) go to 360
      if (ititle.eq.0) write (6,301)
      ititle=1
      write (6,afmt1) (anam(i),i=1,kntr)
      write (6,afmt2) alsrb,(vno1(i),i=1,kntr)
      write (6,afmt2) alsrc,(vno2(i),i=1,kntr)
      write (6,afmt2) alsre,(vno3(i),i=1,kntr)
      write (6,afmt2) alsib,(vno4(i),i=1,kntr)
      write (6,afmt2) alsic,(vno6(i),i=1,kntr)
      write (6,afmt2) alsfn,(vno5(i),i=1,kntr)
      write (6,afmt2) alstot,(vntot(i),i=1,kntr)
  360 kntr=0
      if (loc.ne.0) go to 330
c
c  jfets
c
  400 if (jelcnt(13).eq.0) go to 500
      ititle=0
  401 format(//'0**** jfet squared noise voltages (sq v/hz)')
  410 loc=locate(13)
      kntr=0
  420 if (loc.eq.0) go to 440
      kntr=kntr+1
      locv=nodplc(loc+1)
      anam(kntr)=value(locv)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      locm=nodplc(loc+7)
      locm=nodplc(locm+1)
      loct=nodplc(loc+19)
      area=value(locv+1)
      fnk=value(locm+10)
      fna=value(locm+11)
c
c  extrinsic resistances
c
c...  drain resistance
      cval=cvalue(lcvn+node1)-cvalue(lcvn+node4)
      vno1(kntr)=(vreal*vreal+vimag*vimag)*fourkt*value(locm+4)*area
c...  source resistance
      cval=cvalue(lcvn+node3)-cvalue(lcvn+node5)
      vno2(kntr)=(vreal*vreal+vimag*vimag)*fourkt*value(locm+5)*area
c
c  drain current shot noise and flicker noise
c
      cval=cvalue(lcvn+node4)-cvalue(lcvn+node5)
      vtemp=vreal*vreal+vimag*vimag
      vno3(kntr)=vtemp*fourkt*2.0d0*dabs(value(lx0+loct+5))/3.0d0
      arg=dmax1(dabs(value(lx0+loct+3)),1.0d-20)
      vno4(kntr)=vtemp*fnk*dexp(fna*dlog(arg))/freq
      vntot(kntr)=vno1(kntr)+vno2(kntr)+vno3(kntr)+vno4(kntr)
      vnrms=vnrms+vntot(kntr)
      if (kntr.ge.kntlim) go to 450
  430 loc=nodplc(loc)
      go to 420
  440 if (kntr.eq.0) go to 500
  450 if (nprnt.eq.0) go to 460
      if (ititle.eq.0) write (6,401)
      ititle=1
      write (6,afmt1) (anam(i),i=1,kntr)
      write (6,afmt2) alsrd,(vno1(i),i=1,kntr)
      write (6,afmt2) alsrs,(vno2(i),i=1,kntr)
      write (6,afmt2) alsid,(vno3(i),i=1,kntr)
      write (6,afmt2) alsfn,(vno4(i),i=1,kntr)
      write (6,afmt2) alstot,(vntot(i),i=1,kntr)
  460 kntr=0
      if (loc.ne.0) go to 430
c
c  mosfets
c
  500 if (jelcnt(14).eq.0) go to 600
      ititle=0
  501 format(//'0**** mosfet squared noise voltages (sq v/hz)')
  510 loc=locate(14)
      kntr=0
  520 if (loc.eq.0) go to 540
      kntr=kntr+1
      locv=nodplc(loc+1)
      anam(kntr)=value(locv)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      node6=nodplc(loc+7)
      locm=nodplc(loc+8)
      itype=nodplc(locm+2)
      loct=nodplc(loc+26)
      locm=nodplc(locm+1)
      if(itype.eq.0) go to 522
      xl=value(locv+1)-2.0d0*value(locm+20)
      xw=value(locm+2)-2.0d0*value(locm+36)
      cox=value(locm+13)*xl*xw
      fnk=value(locm+27)
      fna=value(locm+28)
c
c  extrinsic resistances
c
c...  drain resistance
  522 cval=cvalue(lcvn+node1)-cvalue(lcvn+node5)
      vno1(kntr)=(vreal*vreal+vimag*vimag)*fourkt*value(locv+11)
c...  source resistance
      cval=cvalue(lcvn+node3)-cvalue(lcvn+node6)
      vno2(kntr)=(vreal*vreal+vimag*vimag)*fourkt*value(locv+12)
c
c  drain current shot noise and flicker noise
c
      cval=cvalue(lcvn+node5)-cvalue(lcvn+node6)
      vtemp=vreal*vreal+vimag*vimag
      gm=value(lx0+loct+7)
      arg=dmax1(dabs(value(lx0+loct+4)),1.0d-20)
      if(itype.ne.0) go to 524
      modeop=value(locv+8)
      if(modeop.le.0) gm=value(lx0+loct+13)
      if(value(locm+10).ne.0.0d0) gm=value(locm+10)
      xnexp=value(locm+11)
      fnk=value(locm+12)
      fna=value(locm+13)
      vno4(kntr)=vtemp*fnk*dexp(fna*dlog(arg))/(freq**xnexp)
  524 vno3(kntr)=vtemp*fourkt*dabs(gm)/1.5d0
      if(itype.eq.0) go to 525
      vno4(kntr)=vtemp*fnk*dexp(fna*dlog(arg))/(freq*cox)
  525 vntot(kntr)=vno1(kntr)+vno2(kntr)+vno3(kntr)+vno4(kntr)
      vnrms=vnrms+vntot(kntr)
      if (kntr.ge.kntlim) go to 550
  530 loc=nodplc(loc)
      go to 520
  540 if (kntr.eq.0) go to 600
  550 if (nprnt.eq.0) go to 560
      if (ititle.eq.0) write (6,501)
      ititle=1
      write (6,afmt1) (anam(i),i=1,kntr)
      write (6,afmt2) alsrd,(vno1(i),i=1,kntr)
      write (6,afmt2) alsrs,(vno2(i),i=1,kntr)
      write (6,afmt2) alsid,(vno3(i),i=1,kntr)
      write (6,afmt2) alsfn,(vno4(i),i=1,kntr)
      write (6,afmt2) alstot,(vntot(i),i=1,kntr)
  560 kntr=0
      if (loc.ne.0) go to 530
c
c  compute equivalent input noise voltage
c
  600 vnout=dsqrt(vnrms)
      vnin=vnout/vout
      if (nprnt.eq.0) go to 620
      do 610 i=1,5
      string(i)=ablnk
  610 continue
      ioutyp=1
      ipos=1
      call outnam(nosout,ioutyp,string,ipos)
      call move(string,ipos,aslash,1,1)
      ipos=ipos+1
      locv=nodplc(nosin+1)
      anam1=value(locv)
      call move(string,ipos,anam1,1,8)
      write (6,611) vnrms,vnout,string,vout,anam1,vnin
  611 format(////,
     1   '0**** total output noise voltage',9x,'= ',1pd10.3,' sq v/hz'/,
     2   1h0,40x,'= ',d10.3,' v/rt hz'/,
     3   '0     transfer function value:',
     4   1h0,7x,4a8,a1,'= ',d10.3,/,
     5   '0     equivalent input noise at ',a8,' = ',d10.3,' /rt hz')
c
c  save noise outputs
c
  620 loc=locate(44)
  630 if (loc.eq.0) go to 1000
      iseq=nodplc(loc+4)
      if (nodplc(loc+5).ne.2) go to 640
      cvalue(loco+iseq)=vnout
      go to 650
  640 cvalue(loco+iseq)=vnin
  650 loc=nodplc(loc)
      go to 630
c
c  finished
c
 1000 return
      end
      subroutine acasol
      implicit double precision (a-h,o-z)
c
c     this routine evaluates the response of the adjoint circuit by
c doing a forward/backward substitution step using the transpose of the
c circuit equation coefficient matrix.
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
c  evaluates adjoint response by doing forward/backward substitution on
c  the transpose of the y matrix
c
c
c  forward substitution
c
      do 20 i=2,nstop
      io=nodplc(iorder+i)
      call cdiv(value(lvn+io),value(imvn+io),value(lynl+io),
     1  value(imynl+io),value(lvn+io),value(imvn+io))
      jstart=nodplc(iur+i)
      jstop=nodplc(iur+i+1)-1
      if (jstart.gt.jstop) go to 20
      if (value(lvn+io).ne.0.0d0) go to 5
      if (value(imvn+io).eq.0.0d0) go to 20
    5 do 10 j=jstart,jstop
      jo=nodplc(iuc+j)
      jo=nodplc(iorder+jo)
      call cmult(value(lyu+j),value(imynl+nstop+j),value(lvn+io),
     1  value(imvn+io),xreal,ximag)
      value(lvn+jo)=value(lvn+jo)-xreal
      value(imvn+jo)=value(imvn+jo)-ximag
   10 continue
   20 continue
c
c  backward substitution
c
      k=nstop+1
      do 40 i=2,nstop
      k=k-1
      io=nodplc(iorder+k)
      jstart=nodplc(ilc+k)
      jstop=nodplc(ilc+k+1)-1
      if (jstart.gt.jstop) go to 40
      do 30 j=jstart,jstop
      jo=nodplc(ilr+j)
      jo=nodplc(iorder+jo)
      call cmult(value(lyl+j),value(imynl+nstop+nut+j),value(lvn+jo),
     1  value(imvn+jo),xreal,ximag)
      value(lvn+io)=value(lvn+io)-xreal
      value(imvn+io)=value(imvn+io)-ximag
   30 continue
   40 continue
c
c  reorder right-hand side
c
      do 50 i=2,nstop
      j=nodplc(iswap+i)
      value(ndiag+i)=value(lvn+j)
      value(ndiag+i+nstop)=value(imvn+j)
   50 continue
      call copy8(value(ndiag+1),value(lvn+1),nstop)
      call copy8(value(ndiag+nstop+1),value(imvn+1),nstop)
      do 120 i=2,nstop
      cvalue(lcvn+i)=dcmplx(value(lvn+i),value(imvn+i))
  120 continue
      cvalue(lcvn+1)=dcmplx(0.0d0,0.0d0)
c
c  finished
c
      return
      end
      subroutine dinit
      implicit double precision (a-h,o-z)
c
c     this routine performs storage-allocation and one-time computation
c needed to do the small-signal distortion analysis.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      call getm8(ld0,ndist)
      call getm16(ld1,5*nstop)
c
c  bipolar junction transistors
c
      loc=locate(12)
  100 if (loc.eq.0) go to 200
      locv=nodplc(loc+1)
      area=value(locv+1)
      locm=nodplc(loc+8)
      locm=nodplc(locm+1)
      loct=lx0+nodplc(loc+22)
      locd=ld0+nodplc(loc+23)
      csat=value(locm+1)*area
      ova=value(locm+4)
      tf=value(locm+24)
      tr=value(locm+33)
      czbe=value(locm+21)*area
      czbc=value(locm+29)*area
      pe=value(locm+22)
      xme=value(locm+23)
      pc=value(locm+30)
      xmc=value(locm+31)
      fcpe=value(locm+46)
      fcpc=value(locm+50)
      vbe=value(loct)
      vbc=value(loct+1)
      gpi=value(loct+4)
      go=value(loct+7)
      gm=value(loct+6)
      gmu=value(loct+5)
      if (vbe.gt.0.0d0) go to 110
      evbe=1.0d0
      cbe=csat*vbe/vt
      go to 120
  110 evbe=dexp(vbe/vt)
      cbe=csat*(evbe-1.0d0)
  120 if (vbc.gt.0.0d0) go to 130
      evbc=1.0d0
      cbc=csat*vbc/vt
      arg=1.0d0-vbc/pc
      go to 140
  130 evbc=dexp(vbc/vt)
      cbc=csat*(evbc-1.0d0)
  140 if (vbe.ge.fcpe) go to 150
      arg=1.0d0-vbe/pe
      sarg=dexp(xme*dlog(arg))
      cjeo=czbe/sarg
      argbe=pe-vbe
      cje1=xme*cjeo/argbe
      cje2=xme*(1.0d0+xme)*cje1/argbe
      go to 160
  150 denom=dexp((1.0d0+xme)*dlog(1.0d0-fcpe))
      cjeo=czbe*(1.0d0-fcpe*(1.0d0+xme)+xme*vbe/pe)/denom
      cje1=czbe*xme/(denom*pe)
      cje2=0.0d0
  160 if (vbc.ge.fcpc) go to 170
      arg=1.0d0-vbc/pc
      sarg=dexp(xmc*dlog(arg))
      cjco=czbc/sarg
      argbc=pc-vbc
      cjc1=xmc*cjco/argbc
      cjc2=xmc*(1.0d0+xmc)*cjc1/argbc
      go to 180
  170 denom=dexp((1.0d0+xmc)*dlog(1.0d0-fcpc))
      cjco=czbc*(1.0d0-fcpc*(1.0d0+xmc)+xmc*vbc/pc)/denom
      cjc1=czbc*xmc/(denom*pc)
      cjc2=0.0d0
  180 twovt=vt+vt
      go2=(-go+csat*(evbe+evbc)*ova)/twovt
      gmo2=(cbe+csat)*ova/vt-2.0d0*go2
      gm2=(gm+go)/twovt-gmo2-go2
      gmu2=gmu/twovt
      if (vbc.le.0.0d0) gmu2=0.0d0
      gpi2=gpi/twovt
      if (vbe.le.0.0d0) gpi2=0.0d0
      cbo=tf*csat*evbe/vt
      cbor=tr*csat*evbc/vt
      cb1=cbo/vt
      cb1r=cbor/vt
      trivt=3.0d0*vt
      go3=-(go2+(cbc+csat)*ova/twovt)/trivt
      gmo23=-3.0d0*go3
      gm2o3=-gmo23+(cbe+csat)*ova/(vt*twovt)
      gm3=(gm2-(cbe-cbc)*ova/twovt)/trivt
      gmu3=gmu2/trivt
      gpi3=gpi2/trivt
      cb2=cb1/twovt
      cb2r=cb1r/twovt
      value(locd)=cje1
      value(locd+1)=cje2
      value(locd+2)=cjc1
      value(locd+3)=cjc2
      value(locd+4)=go2
      value(locd+5)=gmo2
      value(locd+6)=gm2
      value(locd+7)=gmu2
      value(locd+8)=gpi2
      value(locd+9)=cbo
      value(locd+10)=cbor
      value(locd+11)=cb1
      value(locd+12)=cb1r
      value(locd+13)=go3
      value(locd+14)=gmo23
      value(locd+15)=gm2o3
      value(locd+16)=gm3
      value(locd+17)=gmu3
      value(locd+18)=gpi3
      value(locd+19)=cb2
      value(locd+20)=cb2r
      loc=nodplc(loc)
      go to 100
c
c  diodes
c
  200 loc=locate(11)
  210 if (loc.eq.0) go to 300
      locv=nodplc(loc+1)
      area=value(locv+1)
      locm=nodplc(loc+5)
      locm=nodplc(locm+1)
      loct=lx0+nodplc(loc+11)
      locd=ld0+nodplc(loc+12)
      csat=value(locm+1)*area
      vte=value(locm+3)*vt
      tau=value(locm+4)
      czero=value(locm+5)*area
      phib=value(locm+6)
      xm=value(locm+7)
      fcpb=value(locm+12)
      vd=value(loct)
      geq=value(loct+2)
      evd=1.0d0
      if (vd.ge.0.0d0) evd=dexp(vd/vte)
      if (vd.ge.fcpb) go to 220
      arg=1.0d0-vd/phib
      sarg=dexp(xm*dlog(arg))
      cdjo=czero/sarg
      argd=phib-vd
      cdj1=xm*czero/argd
      cdj2=xm*(1.0d0+xm)*cdj1/argd
      go to 230
  220 denom=dexp((1.0d0+xm)*dlog(1.0d0-fcpb))
      cdjo=czero*(1.0d0-fcpb*(1.0d0+xm)+xm*vd/phib)/denom
      cdj1=czero*xm/(denom*phib)
      cdj2=0.0d0
      cdj2=0.0d0
  230 cdbo=tau*csat*evd/vte
      cdb1=cdbo/vte
      twovte=2.0d0*vte
      geq2=geq/twovte
      if (vd.le.0.0d0) geq2=0.0d0
      trivte=3.0d0*vte
      geq3=geq2/trivte
      cdb2=cdb1/twovte
      value(locd)=cdj1
      value(locd+1)=cdj2
      value(locd+2)=cdbo
      value(locd+3)=cdb1
      value(locd+4)=geq2
      value(locd+5)=geq3
      value(locd+6)=cdb2
      loc=nodplc(loc)
      go to 210
c
c  finished
c
  300 return
      end
      subroutine disto(loco)
      implicit double precision (a-h,o-z)
c
c     this routine performs the small-signal distortion analysis.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /miscel/ atime,aprog(3),adate,atitle(10),defl,defw,defad,
     1  defas,rstats(50),iwidth,lwidth,nopage
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /ac/ fstart,fstop,fincr,skw2,refprl,spw2,jacflg,idfreq,
     1   inoise,nosprt,nosout,nosin,idist,idprt
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      complex*16 difvn1,difvn2,difvn3,difvi1,difvi2,difvi3,dsgo2,dsgm2,
     1   dsgmu2,dsgpi2,dscb1,dscb1r,dscje1,dscjc1,disto1,disto2,disto3,
     2   dsgmo2,dgm2o3,dgmo23,bew,cew,bcw,be2w,ce2w,bc2w,bew2,cew2,
     3   bcw2,bew12,cew12,bcw12,dscdb1,dscdj1,dsg2,cvabe,cvabc,cvace,
     4   cvout,cvdist
      dimension distit(4)
      dimension vdo(2,12)
      complex*16 cvdo(12)
      equivalence (cvdo(1),vdo(1,1))
      data distit / 8hdistorti, 8hon analy, 8hsis     , 8h         /
      icvw1=ld1
      icv2w1=icvw1+nstop
      icvw2=icv2w1+nstop
      icvw12=icvw2+nstop
      icvadj=icvw12+nstop
      iprnt=0
      if (icalc.ge.2) go to 10
      idnp=nodplc(idist+2)
      idnn=nodplc(idist+3)
      locv=nodplc(idist+1)
      rload=1.0d0/value(locv+1)
      kntr=1
   10 if (idprt.eq.0) go to 30
      if (kntr.gt.icalc) go to 30
      iprnt=1
      kntr=kntr+idprt
      call title(0,lwidth,1,distit)
   30 freq1=dreal(cvalue(loco+1))
      freq2=skw2*freq1
      call copy16(cvalue(lcvn+1),cvalue(icvw1+1),nstop)
      cvout=cvalue(icvw1+idnp)-cvalue(icvw1+idnn)
      call magphs(cvout,omag,ophase)
c
c  begin the distortion analysis
c
      do 1000 kdisto=1,7
      cvdist=dcmplx(0.0d0,0.0d0)
      go to (1000,110,120,130,140,160,170),kdisto
  110 freqd=2.0d0*freq1
      arg=dsqrt(2.0d0*rload*refprl)/(omag*omag)
      if (iprnt.eq.0) go to 200
      write (6,111) freq1,freqd,omag,ophase
  111 format (///5x,'2nd harmonic distortion',30x,'freq1 = ',1pd9.2,
     1   '  hz'//5x,'distortion frequency  ',d9.2,'  hz',16x,
     2   'mag ',d9.3,3x,'phs ',0pf7.2)
      go to 200
  120 freqd=3.0d0*freq1
      arg=2.0d0*rload*refprl/(omag*omag*omag)
      if (iprnt.eq.0) go to 200
      write (6,121) freq1,freqd,omag,ophase
  121 format (1h1,4x,'3rd harmonic distortion',30x,'freq1 = ',1pd9.2,
     1   '  hz'//5x,'distortion frequency  ',d9.2,'  hz',16x,
     2   'mag ',d9.3,3x,'phs ',0pf7.2)
      go to 200
  130 freqd=freq2
      go to 200
  140 freqd=freq1-freq2
      arg=dsqrt(2.0d0*rload*refprl)*spw2/(omag*omag)
      if (iprnt.eq.0) go to 200
      write (6,151) freq1,freq2,freqd,omag,ophase,ow2mag,ow2phs
  151 format (1h1,4x,'2nd order intermodulation difference component',
     1   7x,'freq1 = ',1pd9.2,'  hz',15x,'freq2 = ',d9.2,'  hz'//
     2   5x,'distortion frequency  ',d9.2,'  hz',16x,'mag ',
     3   d9.3,3x,'phs ',0pf7.2,9x,'mag ',1pd9.3,3x,'phs ',0pf7.2)
      go to 200
  160 freqd=freq1+freq2
      arg=dsqrt(2.0d0*rload*refprl)*spw2/(omag*omag)
      if (iprnt.eq.0) go to 200
      write (6,161) freq1,freq2,freqd,omag,ophase,ow2mag,ow2phs
  161 format (1h1,4x,'2nd order intermodulation sum component',
     1   14x,'freq1 = ',1pd9.2,'  hz',15x,'freq2 = ',d9.2,'  hz'//
     2   5x,'distortion frequency  ',d9.2,'  hz',16x,'mag ',
     3   d9.3,3x,'phs ',0pf7.2,9x,'mag ',1pd9.3,3x,'phs ',0pf7.2)
      go to 200
  170 freqd=2.0d0*freq1-freq2
      arg=2.0d0*rload*refprl*spw2/(omag*omag*omag)
      if (iprnt.eq.0) go to 200
      write (6,171) freq1,freq2,freqd,omag,ophase,ow2mag,ow2phs
  171 format (1h1,4x,'3rd order intermodulation difference component',
     1   7x,'freq1 = ',1pd9.2,'  hz',15x,'freq2 = ',d9.2,'  hz'//
     2   5x,'distortion frequency  ',d9.2,'  hz',16x,'mag ',
     3   d9.3,3x,'phs ',0pf7.2,9x,'mag ',1pd9.3,3x,'phs ',0pf7.2)
c
c  load and decompose y matrix
c
  200 omega=twopi*freqd
      igoof=0
      call acload
      call acdcmp
      if (igoof.eq.0) go to 220
      write (6,211) igoof,freqd
  211 format('0warning:  underflow ',i4,' time(s) in distortion analysis
     1 at freq = ',1pd9.3,' hz')
      igoof=0
  220 if (kdisto.eq.4) go to 710
c
c  obtain adjoint solution
c
      call zero8(value(lvn+1),nstop)
      call zero8(value(imvn+1),nstop)
      value(lvn+idnp)=-1.0d0
      value(lvn+idnn)=+1.0d0
      call acasol
      call copy16(cvalue(lcvn+1),cvalue(icvadj+1),nstop)
      call zero8(value(lvn+1),nstop)
      call zero8(value(imvn+1),nstop)
c
c  bjts
c
      if (jelcnt(12).eq.0) go to 500
      ititle=0
  301 format (////1x,'bjt distortion components'//1x,'name',11x,'gm',
     1   8x,'gpi',7x,'go',8x,'gmu',6x,'gmo2',7x,'cb',8x,'cbr',7x,'cje',
     2   7x,'cjc',6x,'total')
  311 format (////1x,'bjt distortion components'//1x,'name',11x,'gm',
     1   8x,'gpi',7x,'go',8x,'gmu',6x,'gmo2',7x,'cb',8x,'cbr',7x,'cje',
     2   7x,'cjc',6x,'gm203',5x,'gmo23',5x,'total')
  320 loc=locate(12)
  330 if (loc.eq.0) go to 500
      locv=nodplc(loc+1)
      loct=lx0+nodplc(loc+22)
      locd=ld0+nodplc(loc+23)
      node1=nodplc(loc+5)
      node2=nodplc(loc+6)
      node3=nodplc(loc+7)
      cje1=value(locd)
      cje2=value(locd+1)
      cjc1=value(locd+2)
      cjc2=value(locd+3)
      go2=value(locd+4)
      gmo2=value(locd+5)
      gm2=value(locd+6)
      gmu2=value(locd+7)
      gpi2=value(locd+8)
      cb1=value(locd+11)
      cb1r=value(locd+12)
      go3=value(locd+13)
      gmo23=value(locd+14)
      gm2o3=value(locd+15)
      gm3=value(locd+16)
      gmu3=value(locd+17)
      gpi3=value(locd+18)
      cb2=value(locd+19)
      cb2r=value(locd+20)
      bew=cvalue(icvw1+node2)-cvalue(icvw1+node3)
      cew=cvalue(icvw1+node1)-cvalue(icvw1+node3)
      bcw=cvalue(icvw1+node2)-cvalue(icvw1+node1)
      if (kdisto.eq.2) go to 370
      be2w=cvalue(icv2w1+node2)-cvalue(icv2w1+node3)
      ce2w=cvalue(icv2w1+node1)-cvalue(icv2w1+node3)
      bc2w=cvalue(icv2w1+node2)-cvalue(icv2w1+node1)
      if (kdisto.eq.3) go to 380
      bew2=cvalue(icvw2+node2)-cvalue(icvw2+node3)
      cew2=cvalue(icvw2+node1)-cvalue(icvw2+node3)
      bcw2=cvalue(icvw2+node2)-cvalue(icvw2+node1)
      if (kdisto.eq.5) go to 390
      if (kdisto.eq.6) go to 400
      bew12=cvalue(icvw12+node2)-cvalue(icvw12+node3)
      cew12=cvalue(icvw12+node1)-cvalue(icvw12+node3)
      bcw12=cvalue(icvw12+node2)-cvalue(icvw12+node1)
      go to 410
c
c  calculate hd2 current generators
c
  370 difvn1=0.5d0*cew*cew
      difvn2=0.5d0*bew*bew
      difvn3=0.5d0*bcw*bcw
      dsgmo2=gmo2*0.5d0*bew*cew
      go to 420
c
c  calculate hd3 current generators
c
  380 difvi1=0.50d0*cew*ce2w
      difvn1=0.25d0*cew*cew*cew
      difvi2=0.50d0*bew*be2w
      difvn2=0.25d0*bew*bew*bew
      difvi3=0.50d0*bcw*bc2w
      difvn3=0.25d0*bcw*bcw*bcw
      dsgmo2=gmo2*(bew*ce2w+be2w*cew)*0.5d0
      go to 430
c
c  calculate im2d current generators
c
  390 difvn1=cew*dconjg(cew2)
      difvn2=bew*dconjg(bew2)
      difvn3=bcw*dconjg(bcw2)
      dsgmo2=gmo2*0.5d0*(bew*dconjg(cew2)+cew*dconjg(bew2))
      go to 420
c
c  calculate im2s current generators
c
  400 difvn1=cew*cew2
      difvn2=bew*bew2
      difvn3=bcw*bcw2
      dsgmo2=gmo2*0.5d0*(bew*cew2+bew2*cew)
      go to 420
c
c  calculate im3 current generators
c
  410 difvi1=0.5d0*(ce2w*dconjg(cew2)+cew*cew12)
      difvi2=0.5d0*(be2w*dconjg(bew2)+bew*bew12)
      difvi3=0.5d0*(bc2w*dconjg(bcw2)+bcw*bcw12)
      difvn1=cew*cew*dconjg(cew2)*0.75d0
      difvn2=bew*bew*dconjg(bew2)*0.75d0
      difvn3=bcw*bcw*dconjg(bcw2)*0.75d0
      dsgmo2=gmo2*0.5d0*(dconjg(bew2)*ce2w+bew*cew12+dconjg(cew2)*be2w+
     1   cew*bew12)
      go to 430
c
  420 dsgo2=go2*difvn1
      dsgm2=gm2*difvn2
      dsgmu2=gmu2*difvn3
      dsgpi2=gpi2*difvn2
      dscb1=0.5d0*cb1*omega*dcmplx(-dimag(difvn2),dreal(difvn2))
      dscb1r=0.5d0*cb1r*omega*dcmplx(-dimag(difvn3),dreal(difvn3))
      dscje1=0.5d0*cje1*omega*dcmplx(-dimag(difvn2),dreal(difvn2))
      dscjc1=0.5d0*cjc1*omega*dcmplx(-dimag(difvn3),dreal(difvn3))
      go to 440
c
  430 dsgo2=2.0d0*go2*difvi1+go3*difvn1
      dsgm2=2.0d0*gm2*difvi2+gm3*difvn2
      dsgmu2=2.0d0*gmu2*difvi3+gmu3*difvn3
      dsgpi2=2.0d0*gpi2*difvi2+gpi3*difvn2
      dscb1=omega*(cb1*difvi2+cb2*difvn2/3.0d0)
      dscb1=dcmplx(-dimag(dscb1),dreal(dscb1))
      dscb1r=omega*(cb1r*difvi3+cb2r*difvn3/3.0d0)
      dscb1r=dcmplx(-dimag(dscb1r),dreal(dscb1r))
      dscje1=omega*(cje1*difvi2+cje2*difvn2/3.0d0)
      dscje1=dcmplx(-dimag(dscje1),dreal(dscje1))
      dscjc1=omega*(cjc1*difvi3+cjc2*difvn3/3.0d0)
      dscjc1=dcmplx(-dimag(dscjc1),dreal(dscjc1))
c
c  determine contribution of each distortion source
c
  440 cvabe=cvalue(icvadj+node2)-cvalue(icvadj+node3)
      cvabc=cvalue(icvadj+node2)-cvalue(icvadj+node1)
      cvace=cvalue(icvadj+node1)-cvalue(icvadj+node3)
      disto1=dsgm2+dsgo2+dsgmo2
      disto2=dsgpi2+dscb1+dscje1
      disto3=dsgmu2+dscb1r+dscjc1
      cvdo(1)=dsgm2*cvace*arg
      cvdo(2)=dsgpi2*cvabe*arg
      cvdo(3)=dsgo2*cvace*arg
      cvdo(4)=dsgmu2*cvabc*arg
      cvdo(5)=dsgmo2*cvace*arg
      cvdo(6)=dscb1*cvabe*arg
      cvdo(7)=dscb1r*cvabc*arg
      cvdo(8)=dscje1*cvabe*arg
      cvdo(9)=dscjc1*cvabc*arg
      if (kdisto.eq.3) go to 450
      if (kdisto.eq.7) go to 460
      cvdo(10)=cvdo(1)+cvdo(2)+cvdo(3)+cvdo(4)+cvdo(5)+cvdo(6)+cvdo(7)+
     1   cvdo(8)+cvdo(9)
      cvdist=cvdist+cvdo(10)
      if (iprnt.eq.0) go to 480
      do 445 j=1,10
      call magphs(cvdo(j),xmag,xphs)
      cvdo(j)=dcmplx(xmag,xphs)
  445 continue
      if (ititle.eq.0) write (6,301)
      ititle=1
      write (6,446) value(locv),(vdo(1,j),j=1,10)
  446 format(1h0,a8,'mag',1p12d10.3)
      write (6,447) (vdo(2,j),j=1,10)
  447 format(9x,'phs',12(1x,f7.2,2x))
      go to 480
  450 dgm2o3=gm2o3*cew*bew*bew*0.25d0
      dgmo23=gmo23*bew*cew*cew*0.25d0
      go to 470
  460 dgm2o3=gm2o3*(0.5d0*bew*dconjg(bew2)*cew+0.25d0*bew*bew*
     1  dconjg(cew2))
      dgmo23=gmo23*(0.5d0*cew*dconjg(cew2)*bew+0.25d0*cew*cew*
     1  dconjg(bew2))
  470 disto1=disto1+dgm2o3+dgmo23
      cvdo(10)=dgm2o3*cvace*arg
      cvdo(11)=dgmo23*cvace*arg
      cvdo(12)=cvdo(1)+cvdo(2)+cvdo(3)+cvdo(4)+cvdo(5)+cvdo(6)+cvdo(7)+
     1   cvdo(8)+cvdo(9)+cvdo(10)+cvdo(11)
      cvdist=cvdist+cvdo(12)
      if (iprnt.eq.0) go to 480
      do 475 j=1,12
      call magphs(cvdo(j),xmag,xphs)
      cvdo(j)=dcmplx(xmag,xphs)
  475 continue
      if (ititle.eq.0) write (6,311)
      ititle=1
      write (6,446) value(locv),(vdo(1,j),j=1,12)
      write (6,447) (vdo(2,j),j=1,12)
  480 value(lvn+node1)=value(lvn+node1)
     1  -dreal(disto1-disto3)
      value(lvn+node2)=value(lvn+node2)
     1  -dreal(disto2+disto3)
      value(lvn+node3)=value(lvn+node3)
     1  +dreal(disto1+disto2)
      value(imvn+node1)=value(imvn+node1)
     1  -dimag(disto1-disto3)
      value(imvn+node2)=value(imvn+node2)
     1  -dimag(disto2+disto3)
      value(imvn+node3)=value(imvn+node3)
     1  +dimag(disto1+disto2)
      loc=nodplc(loc)
      go to 330
c
c   junction diodes
c
  500 if (jelcnt(11).eq.0) go to 700
      ititle=0
  501 format (////1x,'diode distortion components'//1x,'name',
     1   11x,'geq',7x,'cb',8x,'cj',7x,'total')
  510 loc=locate(11)
  520 if (loc.eq.0) go to 700
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      locm=nodplc(loc+5)
      locm=nodplc(locm+1)
      loct=lx0+nodplc(loc+11)
      locd=ld0+nodplc(loc+12)
      cdj1=value(locd)
      cdj2=value(locd+1)
      cdb1=value(locd+3)
      geq2=value(locd+4)
      geq3=value(locd+5)
      cdb2=value(locd+6)
      bew=cvalue(icvw1+node3)-cvalue(icvw1+node2)
      if (kdisto.eq.2) go to 540
      be2w=cvalue(icv2w1+node3)-cvalue(icv2w1+node2)
      if (kdisto.eq.3) go to 550
      bew2=cvalue(icvw2+node3)-cvalue(icvw2+node2)
      if (kdisto.eq.5) go to 560
      if (kdisto.eq.6) go to 570
      bew12=cvalue(icvw12+node3)-cvalue(icvw12+node2)
      go to 580
c
c    calculate hd2 current generators
c
  540 difvn1=0.5d0*bew*bew
      go to 590
c
c    calculate hd3 current generators
c
  550 difvi1=0.5d0*bew*be2w
      difvn1=0.25d0*bew*bew*bew
      go to 600
c
c    calculate im2d current generators
c
  560 difvn1=bew*dconjg(bew2)
      go to 590
c
c    calculate im2s current generators
c
  570 difvn1=bew*bew2
      go to 590
c
c    calculate im3 current generators
c
  580 difvi1=0.5d0*(be2w*dconjg(bew2)+bew*bew12)
      difvn1=bew*bew*dconjg(bew2)*0.75d0
      go to 600
  590 dsg2=geq2*difvn1
      dscdb1=0.5d0*cdb1*omega*dcmplx(-dimag(difvn1),dreal(difvn1))
      dscdj1=0.5d0*cdj1*omega*dcmplx(-dimag(difvn1),dreal(difvn1))
      go to 610
c
  600 dsg2=2.0d0*geq2*difvi1+geq3*difvn1
      dscdb1=omega*(cdb1*difvi1+cdb2*difvn1/3.0d0)
      dscdb1=dcmplx(-dimag(dscdb1),dreal(dscdb1))
      dscdj1=omega*(cdj1*difvi1+cdj2*difvn1/3.0d0)
      dscdj1=dcmplx(-dimag(dscdj1),dreal(dscdj1))
c
c  determine contribution of each distortion source
c
  610 cvabe=cvalue(icvadj+node3)-cvalue(icvadj+node2)
      disto1=dsg2+dscdb1+dscdj1
      cvdo(1)=dsg2*cvabe*arg
      cvdo(2)=dscdb1*cvabe*arg
      cvdo(3)=dscdj1*cvabe*arg
      cvdo(4)=cvdo(1)+cvdo(2)+cvdo(3)
      cvdist=cvdist+cvdo(4)
      if (iprnt.eq.0) go to 680
      do 670 j=1,4
      call magphs(cvdo(j),xmag,xphs)
      cvdo(j)=dcmplx(xmag,xphs)
  670 continue
      if (ititle.eq.0) write (6,501)
      ititle=1
      write (6,446) value(locv),(vdo(1,j),j=1,4)
      write (6,447) (vdo(2,j),j=1,4)
  680 value(lvn+node2)=value(lvn+node2)+dreal(disto1)
      value(lvn+node3)=value(lvn+node3)-dreal(disto1)
      value(imvn+node2)=value(imvn+node2)+dimag(disto1)
      value(imvn+node3)=value(imvn+node3)-dimag(disto1)
      loc=nodplc(loc)
      go to 520
c
c  obtain total distortion solution if necessary
c
  700 go to (1000,710,790,710,710,840,860),kdisto
  710 call acsol
c
c  store solution, print and store answers
c
  760 go to (1000,770,790,800,820,840,860),kdisto
  770 call copy16(cvalue(lcvn+1),cvalue(icv2w1+1),nstop)
      call magphs(cvdist,o2mag,o2phs)
      if (iprnt.eq.0) go to 900
      o2log=20.0d0*dlog10(o2mag)
      write (6,781) o2mag,o2phs,o2log
  781 format (///5x,'hd2     magnitude  ',1pd10.3,5x,'phase  ',0pf7.2,
     1   5x,'=  ',f7.2,'  db')
      go to 900
  790 call magphs(cvdist,o3mag,o3phs)
      if (iprnt.eq.0) go to 900
      o3log=20.0d0*dlog10(o3mag)
      write (6,791) o3mag,o3phs,o3log
  791 format (///5x,'hd3     magnitude  ',1pd10.3,5x,'phase  ',0pf7.2,
     1   5x,'=  ',f7.2,'  db')
      go to 900
  800 call copy16(cvalue(lcvn+1),cvalue(icvw2+1),nstop)
      cvout=cvalue(icvw2+idnp)-cvalue(icvw2+idnn)
      call magphs(cvout,ow2mag,ow2phs)
      go to 1000
  820 call copy16(cvalue(lcvn+1),cvalue(icvw12+1),nstop)
  840 call magphs(cvdist,o12mag,o12phs)
      if (iprnt.eq.0) go to 900
      o12log=20.0d0*dlog10(o12mag)
      if (kdisto.eq.6) go to 850
      write (6,841) o12mag,o12phs,o12log
  841 format (///5x,'im2d    magnitude  ',1pd10.3,5x,'phase  ',0pf7.2,
     1   5x,'=  ',f7.2,'  db')
      go to 900
  850 write (6,851) o12mag,o12phs,o12log
  851 format (///5x,'im2s    magnitude  ',1pd10.3,5x,'phase  ',0pf7.2,
     1   5x,'=  ',f7.2,'  db')
      go to 900
  860 call magphs(cvdist,o21mag,o21phs)
      if (iprnt.eq.0) go to 900
      o21log=20.0d0*dlog10(o21mag)
      write (6,861) o21mag,o21phs,o21log
  861 format (///5x,'im3     magnitude  ',1pd10.3,5x,'phase  ',0pf7.2,
     1   5x,'=  ',f7.2,'  db')
      cma=dabs(4.0d0*o21mag*dcos((o21phs-ophase)/rad))
      cma=dmax1(cma,1.0d-20)
      cmp=dabs(4.0d0*o21mag*dsin((o21phs-ophase)/rad))
      cmp=dmax1(cmp,1.0d-20)
      cmalog=20.0d0*dlog10(cma)
      cmplog=20.0d0*dlog10(cmp)
      write (6,866)
  866 format (////5x,'approximate cross modulation components')
      write (6,871) cma,cmalog
  871 format (/5x,'cma     magnitude  ',1pd10.3,24x,'=  ',0pf7.2,'  db')
      write (6,881) cmp,cmplog
  881 format (/5x,'cmp     magnitude  ',1pd10.3,24x,'=  ',0pf7.2,'  db')
c
c  save distortion outputs
c
  900 iflag=kdisto+2
      if (iflag.ge.7) iflag=iflag-1
      loc=locate(45)
  910 if (loc.eq.0) go to 1000
      if (nodplc(loc+5).ne.iflag) go to 920
      iseq=nodplc(loc+4)
      cvalue(loco+iseq)=cvdist
  920 loc=nodplc(loc)
      go to 910
 1000 continue
c
c  finished
c
 2000 return
      end
