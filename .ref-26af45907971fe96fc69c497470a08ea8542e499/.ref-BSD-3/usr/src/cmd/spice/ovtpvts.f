      subroutine ovtpvt
      implicit double precision (a-h,o-z)
c
c
c     this routine generates the requested tabular listings of analysis
c results.  it calls plot to generate line-printer plots.
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
      common /miscel/ atime,aprog(3),adate,atitle(10),defl,defw,defad,
     1  defas,rstats(50),iwidth,lwidth,nopage
      common /dc/ tcstar(2),tcstop(2),tcincr(2),icvflg,itcelm(2),kssop,
     1   kinel,kidin,kovar,kidout
      common /ac/ fstart,fstop,fincr,skw2,refprl,spw2,jacflg,idfreq,
     1   inoise,nosprt,nosout,nosin,idist,idprt
      common /tran/ tstep,tstop,tstart,delmax,tdmax,forfre,jtrflg
      common /outinf/ xincr,string(15),xstart,yvar(8),itab(8),itype(8),
     1   ilogy(8),npoint,numout,kntr,numdgt
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
      complex*16 cval
      dimension prform(3)
      dimension subtit(4,3)
      data subtit / 8hdc trans, 8hfer curv, 8hes      , 8h        ,
     1              8htransien, 8ht analys, 8his      , 8h        ,
     2              8hac analy, 8hsis     , 8h        , 8h         /
      data prform / 8h(1pe11.3, 8h,2x,8e00, 8h.00)     /
      data aper,rprn / 1h., 1h) /
c
      call second(t1)
      if (icalc.le.0) go to 1000
      call crunch
      if (nogo.lt.0) go to 1000
c
c  construct format statement to be used for printing the outputs
c
      ifract=max0(numdgt-1,0)
      ifwdth=ifract+9
      ipos=15
      call alfnum(ifwdth,prform,ipos)
      call move(prform,ipos,aper,1,1)
      ipos=ipos+1
      call alfnum(ifract,prform,ipos)
      call move(prform,ipos,rprn,1,1)
c
      noprln=min0(8,(lwidth-12)/ifwdth)
      if (mode-2) 50,60,300
   50 numout=jelcnt(41)+1
      go to 70
   60 numout=jelcnt(42)+1
c
c  dc and transient analysis printing
c
   70 loc=locate(30+mode)
   80 if (loc.eq.0) go to 200
      kntr=min0(noprln,nodplc(loc+3))
      if (kntr.le.0) go to 120
      call title(1,lwidth,1,subtit(1,mode))
      call setprn(loc)
c
c  get buffer space
c
      call getm8(locx,npoint)
      call getm8(locy,kntr*npoint)
c
c  interpolate outputs
c
      call ntrpl8(locx,locy,numpnt)
c
c  print outputs
c
      do 100 i=1,numpnt
      xvar=value(locx+i)
      locyt=locy
      do 90 k=1,kntr
      yvar(k)=value(locyt+i)
      locyt=locyt+npoint
   90 continue
      write (6,prform) xvar,(yvar(k),k=1,kntr)
  100 continue
      write (6,111)
  111 format(1hy)
      call clrmem(locx)
      call clrmem(locy)
  120 loc=nodplc(loc)
      go to 80
c
c  dc and transient analysis plotting
c
  200 loc=locate(35+mode)
  210 if (loc.eq.0) go to 250
      kntr=nodplc(loc+3)
      if (kntr.le.0) go to 220
      locv=nodplc(loc+1)
      call title(1,lwidth,1,subtit(1,mode))
      call setplt(loc)
c
c     get buffer space
c
      call getm8(locx,npoint)
      call getm8(locy,kntr*npoint)
c
c  interpolate outputs and load plot buffers
c
      call ntrpl8(locx,locy,numpnt)
      call plot(numpnt,locx,locy,locv)
      call clrmem(locx)
      call clrmem(locy)
  220 loc=nodplc(loc)
      go to 210
c
c  fourier analysis
c
  250 if (mode.eq.1) go to 1000
      if (nfour.eq.0) go to 1000
      if (nogo.ne.0) go to 1000
      call fouran
      go to 1000
c
c  ac analysis printing
c
  300 numout=jelcnt(43)+jelcnt(44)+jelcnt(45)+1
      do 599 id=33,35
      loc=locate(id)
  320 if (loc.eq.0) go to 599
      kntr=min0(noprln,nodplc(loc+3))
      if (kntr.le.0) go to 595
      call title(1,lwidth,1,subtit(1,mode))
      call setprn(loc)
c
c  print ac outputs
c
      lout=loutpt
      do 590 i=1,icalc
      xvar=dreal(cvalue(lout+1))
      do 500 k=1,kntr
      iseq=itab(k)
      iseq=nodplc(iseq+4)
      cval=cvalue(lout+iseq)
      ktype=itype(k)
      go to (450,450,430,440,450,450), ktype
  430 yvar(k)=dreal(cval)
      go to 500
  440 yvar(k)=dimag(cval)
      go to 500
  450 call magphs(cval,xmag,xphs)
      go to (460,460,430,440,470,465), ktype
  460 yvar(k)=xmag
      go to 500
  465 yvar(k)=20.0d0*dlog10(xmag)
      go to 500
  470 yvar(k)=xphs
  500 continue
      lout=lout+numout
  580 write (6,prform) xvar,(yvar(k),k=1,kntr)
  590 continue
      write (6,111)
  595 loc=nodplc(loc)
      go to 320
  599 continue
c
c  ac analysis plotting
c
      do 760 id=38,40
      loc=locate(id)
  610 if (loc.eq.0) go to 760
      kntr=nodplc(loc+3)
      if (kntr.le.0) go to 750
      locv=nodplc(loc+1)
      call title(1,lwidth,1,subtit(1,mode))
      call setplt(loc)
c
      call getm8(locx,icalc)
      call getm8(locy,kntr*icalc)
c
c     load plot buffers
c
      lout=loutpt
      do 710 i=1,icalc
      xvar=dreal(cvalue(lout+1))
      locyt=locy
      do 700 k=1,kntr
      iseq=itab(k)
      iseq=nodplc(iseq+4)
      cval=cvalue(lout+iseq)
      ktype=itype(k)
      go to (670,670,650,660,670,670), ktype
  650 yvr=dreal(cval)
      go to 695
  660 yvr=dimag(cval)
      go to 695
  670 call magphs(cval,xmag,xphs)
      go to (680,680,650,660,690,685), ktype
  680 yvr=dlog10(xmag)
      go to 695
  685 yvr=20.0d0*dlog10(xmag)
      go to 695
  690 yvr=xphs
  695 value(locyt+i)=yvr
      locyt=locyt+icalc
  700 continue
      value(locx+i)=xvar
      lout=lout+numout
  710 continue
      call plot(icalc,locx,locy,locv)
      call clrmem(locx)
      call clrmem(locy)
  750 loc=nodplc(loc)
      go to 610
  760 continue
c
c  finished
c
 1000 call clrmem(loutpt)
      call second(t2)
      rstats(11)=rstats(11)+t2-t1
      return
      end
      subroutine ntrpl8(locx,locy,numpnt)
      implicit double precision (a-h,o-z)
c
c     this routine interpolates the analysis data to obtain the values
c printed and/or plotted, using linear interpolation.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /outinf/ xincr,string(15),xstart,yvar(8),itab(8),itype(8),
     1   ilogy(8),npoint,numout,kntr,numdgt
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
      if(mode.ne.1) go to 4
      numpnt=icalc
      loco=loutpt
      do 3 i=1,numpnt
      locyt=locy
      value(locx+i)=value(loco+1)
      do 2 k=1,kntr
      iseq=itab(k)
      iseq=nodplc(iseq+4)
      value(locyt+i)=value(loco+iseq)
      locyt=locyt+npoint
    2 continue
      loco=loco+numout
    3 continue
      return
    4 continue
      xvar=xstart
      xvtol=xincr*1.0d-5
      ippnt=0
      icpnt=2
      loco1=loutpt
      loco2=loco1+numout
      if (icalc.lt.2) go to 50
   10 x1=value(loco1+1)
      x2=value(loco2+1)
      dx1x2=x1-x2
   20 if (xincr.lt.0.0d0) go to 24
      if (xvar.le.(x2+xvtol)) go to 30
      go to 28
   24 if (xvar.ge.(x2+xvtol)) go to 30
   28 if (icpnt.ge.icalc) go to 100
      icpnt=icpnt+1
      loco1=loco2
      loco2=loco1+numout
      go to 10
   30 ippnt=ippnt+1
      value(locx+ippnt)=xvar
      dxx1=xvar-x1
      locyt=locy
      do 40 i=1,kntr
      iseq=itab(i)
      iseq=nodplc(iseq+4)
      v1=value(loco1+iseq)
      v2=value(loco2+iseq)
      yvr=v1+(v1-v2)*dxx1/dx1x2
      tol=dmin1(dabs(v1),dabs(v2))*1.0d-10
      if (dabs(yvr).le.tol) yvr=0.0d0
      value(locyt+ippnt)=yvr
      locyt=locyt+npoint
   40 continue
      if (ippnt.ge.npoint) go to 100
      xvar=xstart+dfloat(ippnt)*xincr
      if (dabs(xvar).ge.dabs(xvtol)) go to 20
      xvar=0.0d0
      go to 20
c
c  special handling if icalc = 1
c
c...  icalc=1;  just copy over the single point and return
   50 ippnt=1
      value(locx+ippnt)=xvar
      locyt=locy
      do 60 i=1,kntr
      iseq=itab(i)
      iseq=nodplc(iseq+4)
      value(locyt+ippnt)=value(loco1+iseq)
      locyt=locyt+npoint
   60 continue
      go to 100
c
c  return
c
  100 numpnt=ippnt
      return
      end
      subroutine setprn(loc)
      implicit double precision (a-h,o-z)
c
c     this routine formats the column headers for tabular listings of
c output variables.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /miscel/ atime,aprog(3),adate,atitle(10),defl,defw,defad,
     1  defas,rstats(50),iwidth,lwidth,nopage
      common /dc/ tcstar(2),tcstop(2),tcincr(2),icvflg,itcelm(2),kssop,
     1   kinel,kidin,kovar,kidout
      common /ac/ fstart,fstop,fincr,skw2,refprl,spw2,jacflg,idfreq,
     1   inoise,nosprt,nosout,nosin,idist,idprt
      common /tran/ tstep,tstop,tstart,delmax,tdmax,forfre,jtrflg
      common /outinf/ xincr,string(15),xstart,yvar(8),itab(8),itype(8),
     1   ilogy(8),npoint,numout,kntr,numdgt
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
      data ablnk, atimex, afreq / 1h , 6h  time, 6h  freq /
c
c  set limits depending upon the analysis mode
c
      if (mode-2) 10,20,30
   10 xstart=tcstar(1)
      xincr=tcincr(1)
      npoint=icvflg
      itemp=itcelm(1)
      loce=nodplc(itemp+1)
      asweep=value(loce)
      go to 40
   20 xstart=tstart
      xincr=tstep
      npoint=jtrflg
      asweep=atimex
      go to 40
   30 xstart=fstart
      xincr=fincr
      npoint=icalc
      asweep=afreq
c
c  construct and print the output variable names
c
   40 loct=loc+2
      ipos=1
      npos=ipos+numdgt+8
      do 90 i=1,kntr
      loct=loct+2
      itab(i)=nodplc(loct)
      itype(i)=nodplc(loct+1)
      call outnam(itab(i),itype(i),string,ipos)
      if (ipos.ge.npos) go to 70
      do 60 j=ipos,npos
      call move(string,j,ablnk,1,1)
   60 continue
      ipos=npos
      go to 80
   70 call move(string,ipos,ablnk,1,1)
      ipos=ipos+1
   80 npos=npos+numdgt+8
   90 continue
      call move(string,ipos,ablnk,1,7)
      jstop=(ipos+6)/8
      write (6,91) asweep,(string(j),j=1,jstop)
   91 format(/3x,a8,5x,14a8,a4)
      write (6,101)
  101 format(1hx/1h )
      return
      end
      subroutine setplt(loc)
      implicit double precision (a-h,o-z)
c
c     this routine generates the 'legend' subheading used to identify
c individual traces on multi-trace line-printer plots.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /miscel/ atime,aprog(3),adate,atitle(10),defl,defw,defad,
     1  defas,rstats(50),iwidth,lwidth,nopage
      common /dc/ tcstar(2),tcstop(2),tcincr(2),icvflg,itcelm(2),kssop,
     1   kinel,kidin,kovar,kidout
      common /ac/ fstart,fstop,fincr,skw2,refprl,spw2,jacflg,idfreq,
     1   inoise,nosprt,nosout,nosin,idist,idprt
      common /tran/ tstep,tstop,tstart,delmax,tdmax,forfre,jtrflg
      common /outinf/ xincr,string(15),xstart,yvar(8),itab(8),itype(8),
     1   ilogy(8),npoint,numout,kntr,numdgt
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
      dimension logopt(6)
      data logopt / 2, 2, 1, 1, 1, 1 /
      data ablnk, atimex, afreq / 1h , 6h  time, 6h  freq /
      data pltsym / 8h*+=$0<>? /
c
c  set limits depending upon the analysis mode
c
      if (mode-2) 10,20,30
   10 xstart=tcstar(1)
      xincr=tcincr(1)
      npoint=icvflg
      itemp=itcelm(1)
      loce=nodplc(itemp+1)
      asweep=value(loce)
      go to 40
   20 xstart=tstart
      xincr=tstep
      npoint=jtrflg
      asweep=atimex
      go to 40
   30 xstart=fstart
      xincr=fincr
      npoint=jacflg
      asweep=afreq
c
c  construct and print the output variables with corresponding plot
c    symbols
c
   40 loct=loc+2
      if (kntr.eq.1) go to 80
      write (6,41)
   41 format('0legend:'/)
      do 70 i=1,kntr
      loct=loct+2
      itab(i)=nodplc(loct)
      ioutyp=nodplc(loct+1)
      itype(i)=ioutyp
      ilogy(i)=1
      if (mode.le.2) go to 50
      ilogy(i)=logopt(ioutyp)
   50 ipos=1
      call outnam(itab(i),itype(i),string,ipos)
      call move(string,ipos,ablnk,1,7)
      jstop=(ipos+6)/8
      call move(achar,1,pltsym,i,1)
      write (6,61) achar,(string(j),j=1,jstop)
   61 format(1x,a1,2h: ,5a8)
   70 continue
   80 if (kntr.ge.2) go to 90
      itab(1)=nodplc(loc+4)
      ioutyp=nodplc(loc+5)
      itype(1)=ioutyp
      ilogy(1)=1
      if (mode.le.2) go to 90
      ilogy(1)=logopt(ioutyp)
   90 ipos=1
      call outnam(itab(1),itype(1),string,ipos)
      call move(string,ipos,ablnk,1,7)
      jstop=(ipos+6)/8
      write (6,101) asweep,(string(j),j=1,jstop)
  101 format(1hx/3x,a8,4x,5a8)
      return
      end
      subroutine plot(numpnt,locx,locy,locv)
      implicit double precision (a-h,o-z)
c
c     this routine generates the line-printer plots.
c
      common /miscel/ atime,aprog(3),adate,atitle(10),defl,defw,defad,
     1  defas,rstats(50),iwidth,lwidth,nopage
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /outinf/ xincr,string(15),xstart,yvar(8),itab(8),itype(8),
     1   ilogy(8),npoint,numout,kntr,numdgt
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      integer xxor
      dimension ycoor(5,8),icoor(8),delplt(8)
      dimension agraph(13),aplot(13)
      dimension asym(2),pmin(8),jcoor(8)
      data ablnk, aletx, aper / 1h , 1hx, 1h. /
      data asym1, asym2, arprn / 8h(-------, 8h--------, 1h) /
      data pltsym / 8h*+=$0<>? /
c
c
      iwide=1
      nwide=101
      nwide4=25
      if(lwidth.gt.80) go to 3
      iwide=0
      nwide=57
      nwide4=14
    3 if (numpnt.le.0) go to 400
      do 5 i=1,13
      agraph(i)=ablnk
    5 continue
      do 7 i=1,5
      ispot=1+nwide4*(i-1)
      call move(agraph,ispot,aper,1,1)
    7 continue
      locyt=locy
      lspot=locv-1
      mltscl=0
      if (value(locv).eq.0.0d0) mltscl=1
      do 235 k=1,kntr
      lspot=lspot+2
      ymin=value(lspot)
      ymax=value(lspot+1)
      if (ymin.ne.0.0d0) go to 10
      if (ymax.ne.0.0d0) go to 10
      go to 100
   10 ymin1=dmin1(ymin,ymax)
      ymax1=dmax1(ymin,ymax)
   30 if (ilogy(k).eq.1) go to 40
      ymin1=dlog10(dmax1(ymin1,1.0d-20))
      ymax1=dlog10(dmax1(ymax1,1.0d-20))
      del=dmax1(ymax1-ymin1,0.0001d0)/4.0d0
      go to 50
   40 del=dmax1(ymax1-ymin1,1.0d-20)/4.0d0
   50 ymin=ymin1
      ymax=ymax1
      go to 200
c
c  determine max and min values
c
  100 ymax1=value(locyt+1)
      ymin1=ymax1
      if (numpnt.eq.1) go to 150
      do 110 i=2,numpnt
      ymin1=dmin1(ymin1,value(locyt+i))
      ymax1=dmax1(ymax1,value(locyt+i))
  110 continue
c
c  scaling
c
  150 call scale(ymin1,ymax1,4,ymin,ymax,del)
c
c  determine coordinates
c
  200 ycoor(1,k)=ymin
      pmin(k)=ymin
      small=del*1.0d-4
      if (dabs(ycoor(1,k)).le.small) ycoor(1,k)=0.0d0
      do 210 i=1,4
      ycoor(i+1,k)=ycoor(i,k)+del
      if (dabs(ycoor(i+1,k)).le.small) ycoor(i+1,k)=0.0d0
  210 continue
      if (ilogy(k).eq.1) go to 230
      do 220 i=1,5
  220 ycoor(i,k)=dexp(xlog10*ycoor(i,k))
  230 delplt(k)=del/dfloat(nwide4)
      locyt=locyt+npoint
  235 continue
c
c  count distinct coordinates
c
      icoor(1)=1
      jcoor(1)=1
      numcor=1
      if (kntr.eq.1) go to 290
      do 250 i=2,kntr
      do 245 j=1,numcor
      l=jcoor(j)
c...  coordinates are *equal* if the most significant 24 bits agree
      do 240 k=1,5
      y1=ycoor(k,i)
      y2=ycoor(k,l)
      if(y1.eq.0.0d0.and.y2.eq.0.0d0) go to 240
      if(dabs((y1-y2)/dmax1(dabs(y1),dabs(y2))).ge.1.0d-7) go to 245
  240 continue
      icoor(i)=l
      go to 250
  245 continue
      icoor(i)=i
      numcor=numcor+1
      jcoor(numcor)=i
  250 continue
c
c  print coordinates
c
  260 do 280 i=1,numcor
      asym(1)=asym1
      asym(2)=asym2
      ipos=2
      do 270 j=1,kntr
      if (icoor(j).ne.jcoor(i)) go to 270
      call move(asym,ipos,pltsym,j,1)
      ipos=ipos+1
  270 continue
      call move(asym,ipos,arprn,1,1)
      k=jcoor(i)
      if(iwide.ne.0) write(6,271) asym,(ycoor(j,k),j=1,5)
  271 format(/1hx,2a8,4h----,1pd12.3,4(15x,d10.3)/26x,51(2h -))
      if(iwide.eq.0) write(6,273) asym,(ycoor(j,k),j=1,5)
  273 format(/1hx,2a8,1pd10.3,3(4x,d10.3),1x,d10.3/22x,29(2h -))
  280 continue
      go to 300
  290 if(iwide.ne.0) write(6,291) (ycoor(j,1),j=1,5)
  291 format(/1hx,20x,1pd12.3,4(15x,d10.3)/26x,51(2h -))
      if(iwide.eq.0) write(6,293) (ycoor(j,1),j=1,5)
  293 format(/1hx,14x,1pd12.3,3(4x,d10.3),1x,d10.3/22x,29(2h -))
c
c  plotting
c
  300 aspot=ablnk
      do 320 i=1,numpnt
      xvar=value(locx+i)
      locyt=locy
      call copy8(agraph,aplot,13)
      do 310 k=1,kntr
      yvr=value(locyt+i)
      ktmp=icoor(k)
      ymin1=pmin(ktmp)
      jpoint=idint((yvr-ymin1)/delplt(k)+0.5d0)+1
      if (jpoint.le.0) go to 306
      if (jpoint.gt.nwide) go to 306
      call move(aspot,1,aplot,jpoint,1)
      if (aspot.eq.ablnk) go to 303
      if (aspot.eq.aper) go to 303
      call move(aplot,jpoint,aletx,1,1)
      go to 306
  303 call move(aplot,jpoint,pltsym,k,1)
  306 locyt=locyt+npoint
  310 continue
      yvr=value(locy+i)
      if (ilogy(1).eq.1) go to 315
      yvr=dexp(xlog10*yvr)
  315 if(iwide.ne.0) write(6,316) xvar,yvr,aplot
  316 format(1x,1pd10.3,2x,d10.3,2x,13a8)
      if(iwide.eq.0) write(6,317) xvar,yvr,(aplot(k),k=1,8)
  317 format(1x,1pd10.3,1x,d10.3,7a8,a1)
  320 continue
c
c  finished
c
      if(iwide.ne.0) write(6,331)
  331 format(26x,51(2h -)//)
      if(iwide.eq.0) write(6,332)
  332 format(21x,29(2h -)//)
      go to 500
c
c  too few points
c
  400 write (6,401)
  401 format('0warning:  too few points for plotting'/)
  500 write (6,501)
  501 format(1hy)
      return
      end
      subroutine scale(xmin,xmax,n,xminp,xmaxp,del)
      implicit double precision (a-h,o-z)
c
c     this routine determines the 'optimal' scale to use for the plot of
c some output variable.
c
c
c  adapted from algorithm 463 of 'collected algorithms of the cacm'
c
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      integer xxor
      dimension vint(5)
      data vint / 1.0d0,2.0d0,5.0d0,10.0d0,20.0d0 /
      data eps / 1.0d-12 /
c
c
c...  trap too-small data spread
      if(xmin.eq.0.0d0.and.xmax.eq.0.0d0) go to 4
      if(dabs((xmax-xmin)/dmax1(dabs(xmin),dabs(xmax))).ge.1.0d-4)
     1  go to 10
    4 continue
      if (xmin.ge.0.0d0) go to 5
      xmax=0.5d0*xmin+eps
      xmin=1.5d0*xmin-eps
      go to 10
    5 xmax=1.5d0*xmin+eps
      xmin=0.5d0*xmin-eps
c...  find approximate interval size, normalized to [1,10]
   10 a=(xmax-xmin)/dfloat(n)
      nal=idint(dlog10(a))
      if (a.lt.1.0d0) nal=nal-1
      xfact=dexp(xlog10*dfloat(nal))
      b=a/xfact
c...  find closest permissible interval size
      do 20 i=1,3
      if (b.lt.(vint(i)+eps)) go to 30
   20 continue
      i=4
c...  compute interval size
   30 del=vint(i)*xfact
      fm1=xmin/del
      m1=fm1
      if (fm1.lt.0.0d0) m1=m1-1
      if (dabs(dfloat(m1)+1.0d0-fm1).lt.eps) m1=m1+1
c...  compute new maximum and minimum limits
      xminp=del*dfloat(m1)
      fm2=xmax/del
      m2=fm2+1.0d0
      if (fm2.lt.(-1.0d0)) m2=m2-1
      if (dabs(fm2+1.0d0-dfloat(m2)).lt.eps) m2=m2-1
      xmaxp=del*dfloat(m2)
      np=m2-m1
c...  check whether another loop required
      if (np.le.n) go to 40
      i=i+1
      go to 30
c...  do final adjustments and correct for roundoff error(s)
   40 nx=(n-np)/2
      xminp=dmin1(xmin,xminp-dfloat(nx)*del)
      xmaxp=dmax1(xmax,xminp+dfloat(n)*del)
      return
      end
      subroutine fouran
      implicit double precision (a-h,o-z)
c
c     this routine determines the fourier coefficients of a transient
c analysis waveform.
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
      common /miscel/ atime,aprog(3),adate,atitle(10),defl,defw,defad,
     1  defas,rstats(50),iwidth,lwidth,nopage
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /tran/ tstep,tstop,tstart,delmax,tdmax,forfre,jtrflg
      common /outinf/ xincr,string(15),xstart,yvar(8),itab(8),itype(8),
     1   ilogy(8),npoint,numout,kntr,numdgt
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension sinco(9),cosco(9)
      dimension fortit(4)
      data fortit / 8hfourier , 8hanalysis, 8h        , 8h         /
      data ablnk / 1h  /
c
c
      forprd=1.0d0/forfre
      xstart=tstop-forprd
      kntr=1
      xn=101.0d0
      xincr=forprd/xn
      npoint=xn
      call getm8(locx,npoint)
      call getm8(locy,npoint)
      do 105 nknt=1,nfour
      itab(1)=nodplc(ifour+nknt)
      kfrout=itab(1)
      call ntrpl8(locx,locy,numpnt)
      dcco=0.0d0
      call zero8(sinco,9)
      call zero8(cosco,9)
      loct=locy+1
      ipnt=0
   10 yvr=value(loct+ipnt)
      dcco=dcco+yvr
      forfac=dfloat(ipnt)*twopi/xn
      arg=0.0d0
      do 20 k=1,9
      arg=arg+forfac
      sinco(k)=sinco(k)+yvr*dsin(arg)
      cosco(k)=cosco(k)+yvr*dcos(arg)
   20 continue
      ipnt=ipnt+1
      if (ipnt.ne.npoint) go to 10
      dcco=dcco/xn
      forfac=2.0d0/xn
      do 30 k=1,9
      sinco(k)=sinco(k)*forfac
      cosco(k)=cosco(k)*forfac
   30 continue
      call title(0,72,1,fortit)
      ipos=1
      call outnam(kfrout,1,string,ipos)
      call move(string,ipos,ablnk,1,7)
      jstop=(ipos+6)/8
      write (6,61) (string(j),j=1,jstop)
   61 format(' fourier components of transient response ',5a8///)
      write (6,71) dcco
   71 format('0dc component =',1pd12.3/,
     1   '0harmonic   frequency    fourier    normalized    phase     no
     2rmalized'/,
     3   '    no         (hz)     component    component    (deg)    pha
     4se (deg)'//)
      iknt=1
      freq1=forfre
      xnharm=1.0d0
      call magphs(dcmplx(sinco(1),cosco(1)),xnorm,pnorm)
      phasen=0.0d0
      write (6,81) iknt,freq1,xnorm,xnharm,pnorm,phasen
   81 format(i6,1pd15.3,d12.3,0pf13.6,f10.3,f12.3/)
      thd=0.0d0
      do 90 iknt=2,9
      freq1=dfloat(iknt)*forfre
      call magphs(dcmplx(sinco(iknt),cosco(iknt)),
     1   harm,phase)
      xnharm=harm/xnorm
      phasen=phase-pnorm
      thd=thd+xnharm*xnharm
      write (6,81) iknt,freq1,harm,xnharm,phase,phasen
   90 continue
      thd=100.0d0*dsqrt(thd)
      write (6,101) thd
  101 format (//5x,'total harmonic distortion =  ',f12.6,'  percent')
  105 continue
      call clrmem(locx)
      call clrmem(locy)
  110 return
      end
