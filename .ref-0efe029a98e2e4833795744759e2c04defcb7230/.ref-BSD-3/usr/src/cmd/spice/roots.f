       program spice
      implicit double precision (a-h,o-z)
c
c
c
c         *** version VAX UNIX 2X.x (19aug79)
c           developed from
c         *** version 2d.2 (26sep76) ***
c         *** version hp 2.0 (6dec77) ***
c
c   by       dick dowell  - hewlett packard company
c            richard newton - uc berkeley
c
c     spice is an electronic circuit simulation program that was deve-
c loped by the integrated circuits group of the electronics research
c laboratory and the department of electrical engineering and computer
c sciences at the university of california, berkeley, california.  the
c program spice is available free of charge to any interested party.
c the sale, resale, or use of this program for profit without the
c express written consent of the department of electrical engineering
c and computer sciences, university of california, berkeley, california,
c is forbidden.
c
c
c implementation notes:
c
c     subroutines mclock and mdate return the time (as hh:mm:ss) and
c the date (as dd mmm yy), respectively.  subroutine getcje returns in
c common block /cje/ various attributes of the current job environment.
c spice expects getcje to set /cje/ variables maxtim, itime, and icost.
c maxtim is the maximum cpu time in seconds, itime is the elapsed cpu
c time in seconds, and icost is the job cost in cents.
c subroutine memory is used to change the number of memory words
c allocated to spice.  if the amount of memory allocated to a jobstep
c is fixed, subroutine memory need not be changed.
c     ifamwa (set in a data statement below) should be set to the
c address of the first available word of memory (following overlays, if
c any).  the proper value should be easily obtainable from any load map.
c     with the exception of most flags, all data in spice is stored in
c the form of managed tables allocated in the /blank/ array value().
c     spice is particularly well-suited to being run using a one-level
c overlay structure beginning with routines spice (the overlay root),
c readin, errchk, setup, dctran, dcop, acan, and ovtpvt.  the order of
c the routines in this listing corresponds to that structure.  note
c that if cdc-style overlay is to be used, an overlay directive card
c must be inserted before the first line of each of the just-named
c routines.
c
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /miscel/ atime,aprog(3),adate,atitle(10),defl,defw,defad,
     1  defas,rstats(50),iwidth,lwidth,nopage
      common /line/ achar,afield(15),oldlin(15),kntrc,kntlim
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /mosarg/ gamma,beta,vto,phi,cox,vbi,xnfs,xnsub,xd,xj,xl,
     1   xlamda,utra,uexp,vbp,von,vdsat,theta,vcrit,vtra,gleff,cdrain
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      common /dc/ tcstar(2),tcstop(2),tcincr(2),icvflg,itcelm(2),kssop,
     1   kinel,kidin,kovar,kidout
      common /ac/ fstart,fstop,fincr,skw2,refprl,spw2,jacflg,idfreq,
     1   inoise,nosprt,nosout,nosin,idist,idprt
      common /tran/ tstep,tstop,tstart,delmax,tdmax,forfre,jtrflg
      common /outinf/ xincr,string(15),xstart,yvar(8),itab(8),itype(8),
     1   ilogy(8),npoint,numout,kntr,numdgt
      common /cje/ maxtim,itime,icost
c.. common for putwds
      common /blank/ value(50000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
      integer*2 istats(200)
      real*4 r4stat(100)
      equivalence (rstats(1),istats(1),r4stat(1))
      integer*2 inknt
c
      dimension acctit(4)
      dimension remain(4)
      data amrje /4hmrje/
      data ablnk /1h  /
      data acctit / 8hjob stat, 8histics s, 8hummary  , 8h         /
      data ahdr1, ahdr2, ahdr3 / 8h spice 2,8h.Xx (vax,8h11/780)    /
c
c
      ipostp=0
       maxtim=1e8
       maxlin=50000
       icost=0
       ilines=0
c
c  initialization
c
      aprog(1)=ahdr1
      aprog(2)=ahdr2
      aprog(3)=ahdr3
      achar=ablnk
      keof=0
      call mclock(atime)
      call mdate(adate)
      boltz=1.3806226d-23
      charge=1.6021918d-19
      ctok=273.15d0
      eps0=8.854214871d-14
      epssil=11.7d0*eps0
      epsox=3.9d0*eps0
      twopi=8.0d0*datan2(1.0d0,1.0d0)
      rad=360.0d0/twopi
      xlog2=dlog(2.0d0)
      xlog10=dlog(10.0d0)
      root2=dsqrt(2.0d0)
      nodata=1
c
c  begin job
c
   10 if (keof.eq.1) go to 1000
      call getcje
      call second(time1)
      icost1=icost
      igoof=0
      mode=0
      nogo=0
      maxmem=100000
      call setmem(nodplc(1),maxmem)
      if (nogo.ne.0) go to 1000
      call zero8(rstats,50)
c
c  read remainder of data deck and check for input errors
c
      call readin
      if (nogo.ne.0) go to 300
      if (keof.eq.1) go to 1000
      nodata=0
      call errchk
      if (nogo.ne.0) go to 300
      call setup
      if (nogo.ne.0) go to 300
c
c  cycle through temperatures
c
      itemno=1
      if (numtem.eq.1) go to 110
  100 if (itemno.eq.numtem) go to 320
      itemno=itemno+1
      call tmpupd
c
c  dc transfer curves
c
  110 if (icvflg.eq.0) go to 150
c...  see routine *dctran* for explanation of *mode*, etc.
      mode=1
      modedc=3
      call dctran
      call ovtpvt
      if (nogo.ne.0) go to 300
c
c  small signal operating point
c
  150 if (kssop.gt.0) go to 170
      if (jacflg.ne.0) go to 170
      if ((icvflg+jtrflg).gt.0) go to 250
  170 mode=1
      modedc=1
      call dctran
      if (nogo.ne.0) go to 300
      call dcop
      if (nogo.ne.0) go to 300
c
c  ac small signal analysis
c
  200 if (jacflg.eq.0) go to 250
      mode=3
      call acan
      call ovtpvt
      if (nogo.ne.0) go to 300
c
c  transient analysis
c
  250 if (jtrflg.eq.0) go to 100
      mode=1
      modedc=2
      call dctran
      if (nogo.ne.0) go to 300
      call dcop
      if (nogo.ne.0) go to 300
      mode=2
      call dctran
      call ovtpvt
      if (nogo.ne.0) go to 300
      go to 100
c
c  job concluded
c
  300 write (6,301)
  301 format(1h0,9x,'***** job aborted')
      nodata=0
c
c  job accounting
c
  320 continue
      numel=0
      do 360 i=1,18
  360 numel=numel+jelcnt(i)
      numtem=max0(numtem-1,1)
      idist=min0(idist,1)
      if (iprnta.eq.0) go to 800
      call title(-1,lwidth,1,acctit)
      write (6,361) nunods,ncnods,numnod,numel,(jelcnt(i),i=11,14)
  361 format('   nunods ncnods numnod numel  diodes  bjts  jfets  mfets'
     1   //,i9,2i7,i6,i8,i6,2i7)
      write (6,371) numtem,icvflg,jtrflg,jacflg,inoise,idist,nogo
  371 format(/'0  numtem icvflg jtrflg jacflg inoise  idist   nogo'/,
     1   2h0 ,7i7)
      write (6,381) rstats(20),rstats(21),rstats(22),rstats(23),
     1   rstats(26),rstats(27)
  381 format(/'0  nstop   nttbr   nttar   ifill    iops    perspa'//,
     1   1x,5f8.0,f9.3)
      write (6,391) rstats(30),rstats(31),rstats(32),maxmem,maxuse,
     1   cpyknt
  391 format(/'0  numttp  numrtp  numnit  maxmem  memuse  copyknt',//,
     1   2x,3f8.0,2x,i6,2x,i6,2x,f8.0)
      write (6,401) (rstats(i),i=1,11)
  401 format(/,
     1   1h0,9x,'readin  ',12x,f10.2/,
     2   1h0,9x,'setup   ',12x,f10.2/,
     3   1h0,9x,'trcurv  ',12x,f10.2,10x,f6.0/,
     4   1h0,9x,'dcan    ',12x,f10.2,10x,f6.0/,
     5   1h0,9x,'acan    ',12x,f10.2,10x,f6.0/,
     6   1h0,9x,'tranan  ',12x,f10.2,10x,f6.0/,
     7   1h0,9x,'output  ',12x,f10.2)
  800 call getcje
      call second(time2)
      et=time2-time1
      tcost=dfloat(icost-icost1)/100.0d0
      if (iprnta.eq.0) go to 810
      ohead=et-(rstats(1)+rstats(2)+rstats(3)+rstats(5)+rstats(7)
     1   +rstats(9)+rstats(11))
      write (6,801) ohead
  801 format(1h0,9x,'overhead',12x,f10.2)
  810 write (6,811) et
  811 format(1h0,9x,'total job time      ',f10.2)
      tcost=tcost*11.5d0/23.0d0
c      write(6,812) tcost
c  812 format(1h0,9x,'total job cost       $',f8.2,
c     1  ' @ $11.50 per cpu minute',
c     2  /'0this lower rate applies for remainder of fiscal 79')
      rstats(33)=cpyknt
      rstats(34)=et
      rstats(35)=tcost
      rstats(36)=ohead
c.. convert dble to sgl - 72/2 is how many to convert
c     call dblsgl(rstats(1),72)
      istats(73)=nunods
      istats(74)=ncnods
      istats(75)=numnod
      istats(76)=numel
      istats(77)=jelcnt(11)
      istats(78)=jelcnt(12)
      istats(79)=jelcnt(13)
      istats(80)=jelcnt(14)
      istats(81)=numtem
      istats(82)=icvflg
      istats(83)=jtrflg
      istats(84)=jacflg
      istats(85)=inoise
      istats(86)=idist
      istats(87)=nogo
      istats(88)=maxmem
      istats(89)=maxuse
c     do 820 i=1,36
c 820 r4stat(i)=rlconv(r4stat(i))
c     call cadend(istats,100)
  900 if ((maxtim-itime).ge.limtim) go to 10
      write (6,901)
  901 format('1warning:  further analysis stopped due to cpu time limit'
     1/)
 1000 if(nodata.ne.0) write(6,1001)
 1001 format(/1x,'input deck (file) contains no data.')
      stop
      end
      subroutine tmpupd
      implicit double precision (a-h,o-z)
c
c     this routine updates the temperature-dependent parameters in the
c device models.  it also updates the values of temperature-dependent
c resistors.  the updated values are printed.
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
      common /blank/ value(50000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension tmptit(4)
      data tmptit / 8htemperat, 8hure-adju, 8hsted val, 8hues      /
c
c
      tempd=value(itemps+itemno)+ctok
      xkt=boltz*tempd
      oldvt=vt
      vt=xkt/charge
      ratio=tempd/(value(itemps+itemno-1)+ctok)
      ratlog=dlog(ratio)
      ratio1=ratio-1.0d0
      delt=value(itemps+itemno)-value(itemps+1)
      deltsq=delt*delt
      reftmp=27.0d0+ctok
      oldeg=egfet
      egfet=1.16d0-(7.02d-4*tempd*tempd)/(tempd+1108.0d0)
      oldxni=xni
      arg=-egfet/(xkt+xkt)+1.1151d0/(boltz*(reftmp+reftmp))
      factor=tempd/reftmp
      factor=factor*dsqrt(factor)
      xni=1.45d10*factor*dexp(charge*arg)
      pbfact=(vt+vt)*dlog(oldxni/xni)
      call title(0,lwidth,1,tmptit)
c
c  resistors
c
      loc=locate(1)
      ititle=0
   10 if (loc.eq.0) go to 100
      locv=nodplc(loc+1)
      tc1=value(locv+3)
      tc2=value(locv+4)
      if (tc1.ne.0.0d0) go to 20
      if (tc2.eq.0.0d0) go to 40
   20 if (ititle.ne.0) go to 30
      write (6,21)
   21 format(//'0**** resistors',/,'0name',8x,'value',//)
      ititle=1
   30 rnew=value(locv+2)*(1.0d0+tc1*delt+tc2*deltsq)
      value(locv+1)=1.0d0/rnew
      write (6,31) value(locv),rnew
   31 format(1x,a8,1p6d11.2)
   40 loc=nodplc(loc)
      go to 10
c
c  diode model
c
  100 loc=locate(21)
      if (loc.eq.0) go to 200
      write (6,101)
  101 format(//'0**** diode model parameters',/,'0name',9x,'is',9x,'pb',
     1//)
  110 if (loc.eq.0) go to 200
      locv=nodplc(loc+1)
c...  is(t2)=is(t1)*dexp(eg/(n*vt)*(t2/t1-1))*(t2/t1)^(pt/n)
      xn=value(locv+3)
      factor=ratio1*value(locv+8)/(xn*vt)+value(locv+9)/xn*ratlog
      factor=dexp(factor)
      value(locv+1)=value(locv+1)*factor
      oldpb=value(locv+6)
      value(locv+6)=ratio*oldpb+pbfact
      value(locv+12)=value(locv+12)*value(locv+6)/oldpb
      value(locv+15)=value(locv+15)*value(locv+6)/oldpb
      vte=value(locv+3)*vt
      value(locv+18)=vte*dlog(vte/(root2*value(locv+1)))
      write (6,31) value(locv),value(locv+1),value(locv+6)
      loc=nodplc(loc)
      go to 110
c
c  bipolar transistor model
c
  200 loc=locate(22)
      if (loc.eq.0) go to 300
      write (6,201)
  201 format(//'0**** bjt model parameters',/,'0name',9x,'js',8x,'bf ',
     1   7x,'jle',7x,'br ',7x,'jlc',7x,'vje',7x,'vjc',//)
  210 if (loc.eq.0) go to 300
      locv=nodplc(loc+1)
c...  is(t2)=is(t1)*dexp(eg/vt*(t2/t1-1))*(t2/t1)^pt
      factln=ratio1*value(locv+42)/vt+value(locv+43)*ratlog
      factor=dexp(factln)
      value(locv+1)=value(locv+1)*factor
      tb=value(locv+41)
      bfactr=dexp(tb*ratlog)
      value(locv+2)=value(locv+2)*bfactr
      value(locv+8)=value(locv+8)*bfactr
      value(locv+6)=value(locv+6)*dexp(factln/value(locv+7))/bfactr
      value(locv+12)=value(locv+12)*dexp(factln/value(locv+13))
     1               /bfactr
      oldpb=value(locv+22)
      value(locv+22)=ratio*oldpb+pbfact
      value(locv+46)=value(locv+46)*value(locv+22)/oldpb
      value(locv+47)=value(locv+47)*value(locv+22)/oldpb
      oldpb=value(locv+30)
      value(locv+30)=ratio*oldpb+pbfact
      value(locv+50)=value(locv+50)*value(locv+30)/oldpb
      value(locv+51)=value(locv+51)*value(locv+30)/oldpb
      value(locv+54)=vt*dlog(vt/(root2*value(locv+1)))
      write (6,211) value(locv),value(locv+1),value(locv+2),
     1   value(locv+6),value(locv+8),value(locv+12),value(locv+22),
     2   value(locv+30)
  211 format(1x,a8,1p7d10.2)
      loc=nodplc(loc)
      go to 210
c
c  jfet model
c
  300 loc=locate(23)
      if (loc.eq.0) go to 400
      write (6,301)
  301 format(//'0**** jfet model parameters',/,'0name',9x,'is',9x,'pb',
     1//)
  310 if (loc.eq.0) go to 400
      locv=nodplc(loc+1)
      value(locv+9)=value(locv+9)*dexp(ratio1*1.11d0/vt)
      oldpb=value(locv+8)
      value(locv+8)=ratio*oldpb+pbfact
      value(locv+12)=value(locv+12)*value(locv+8)/oldpb
      value(locv+13)=value(locv+13)*value(locv+8)/oldpb
      value(locv+16)=vt*dlog(vt/(root2*value(locv+9)))
      write (6,31) value(locv),value(locv+9),value(locv+8)
      loc=nodplc(loc)
      go to 310
c
c  mosfet model
c
  400 loc=locate(24)
      if (loc.eq.0) go to 1000
      iprnt=1
  410 if (loc.eq.0) go to 1000
c.. no temperature effects have been coded for ga-as fets
      if(nodplc(loc+2).eq.0) go to 430
      locv=nodplc(loc+1)
      if(iprnt.ne.0) write (6,401)
  401 format(//'0**** mosfet model parameters',/,'0name',8x,'vto',8x,
     1   'phi',9x,'pb',9x,'js',7x,'kp',//)
      iprnt=0
      ratio4=ratio*dsqrt(ratio)
      value(locv+2)=value(locv+2)/ratio4
      value(locv+23)=value(locv+23)/ratio4
      oldphi=value(locv+4)
      value(locv+4)=ratio*oldphi+pbfact
      phi=value(locv+4)
      type=nodplc(loc+2)
      tps=value(locv+22)
      vfb=value(locv+34)-type*oldphi
      vstrip=vfb+0.5d0*type*oldphi
      if(value(locv+21).ne.0.0d0) go to 415
      vstrip=vstrip+0.5d0*(oldeg-egfet)
      go to 420
  415 oldgat=oldvt*dlog(value(locv+21)/oldxni)
      gatnew=vt*dlog(value(locv+21)/xni)
      vstrip=vstrip+type*tps*(oldgat-gatnew)
  420 vfb=vstrip-0.5d0*type*phi
      value(locv+34)=vfb+type*phi
      value(locv+1)=value(locv+34)+type*value(locv+3)*dsqrt(phi)
      value(locv+15)=value(locv+15)*dexp(-egfet/vt+oldeg/oldvt)
      oldpb=value(locv+14)
      value(locv+14)=ratio*oldpb+pbfact
      pb=value(locv+14)
      ratio2=oldpb/pb
      ratio3=dsqrt(ratio2)
      value(locv+11)=value(locv+11)*ratio3
      value(locv+12)=value(locv+12)*ratio3
      pbrat=1.0d0/ratio2
      value(locv+29)=value(locv+29)*pbrat
      value(locv+30)=value(locv+30)*pbrat
      write (6,31) value(locv),value(locv+1),value(locv+4),
     1   value(locv+14),value(locv+15),value(locv+2)
  430 loc=nodplc(loc)
      go to 410
c
c  finished
c
 1000 return
      end
      subroutine find(aname,id,loc,iforce)
      implicit double precision (a-h,o-z)
c
c     this routine searches the list with number 'id' for an element
c with name 'aname'.  loc is set to point to the element.  if iforce is
c nonzero, then find expects to have to add the element to the list, and
c reports a fatal error if the element is found.  if subcircuit defini-
c tion is in progress (nonzero value for nsbckt), then find searches the
c current subcircuit definition list rather than the nominal element
c list.
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
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      common /blank/ value(50000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c  index to the contents of the various lists:
c
c        list      contents
c        ----      --------
c
c          1       resistors
c          2       nonlinear capacitors
c          3       nonlinear inductors
c          4       mutual inductors
c          5       nonlinear voltage controlled current sources
c          6       nonlinear voltage controlled voltage sources
c          7       nonlinear current controlled current sources
c          8       nonlinear current controlled voltage sources
c          9       independent voltage sources
c         10       independent current sources
c         11       diodes
c         12       bipolar junction transistors
c         13       junction field-effect transistors (jfets)
c         14       metal-oxide-semiconductor junction fets (mosfets)
c         15       s-parameter 2-port network
c         16       y-parameter 2-port network
c         17       transmission lines
c         18       <unused>
c         19       subcircuit calls
c         20       subcircuit definitions
c         21       diode model
c         22       bjt model
c         23       jfet model
c         24       mosfet model
c      25-30       <unused>
c         31       .print dc
c         32       .print tran
c         33       .print ac
c         34       .print noise
c         35       .print distortion
c         36       .plot dc
c         37       .plot tr
c         38       .plot ac
c         39       .plot noise
c         40       .plot distortion
c         41       outputs for dc
c         42       outputs for transient
c         43       outputs for ac
c         44       outputs for noise
c         45       outputs for distortion
c      46-50       <unused>
c
      integer xxor
      dimension lnod(50),lval(50)
      data lnod / 9,13,15, 7,14,15,14,15,12, 7,
     1           17,37,26,34, 7, 7,34, 0, 5, 5,
     2            4, 4, 4, 4, 0, 0, 0, 0, 0, 0,
     3           21,21,21,21,21,21,21,21,21,21,
     4            8, 8, 8, 8, 8, 0, 0, 0, 0, 0 /
      data lval / 5, 4, 4, 2, 1, 1, 1, 1, 4, 4,
     1            3, 4, 4,13, 1, 1, 9, 0, 1, 1,
     2           19,55,17,41, 0, 0, 0, 0, 0, 0,
     3            1, 1, 1, 1, 1,17,17,17,17,17,
     4            1, 1, 1, 1, 1, 0, 0, 0, 0, 0 /
      data ndefin /2h.u/
c
c
      anam=aname
      call sizmem(ielmnt,isize)
      locn=ielmnt+isize+2
      if (nsbckt.eq.0) go to 10
      loct=nodplc(isbckt+nsbckt)
      loc=nodplc(loct+3)
      if (loc.ne.0) go to 20
      nodplc(loct+3)=locn
      go to 60
   10 loc=locate(id)
      if (loc.ne.0) go to 20
      locate(id)=locn
      go to 50
c
c  search list for a name match
c
   20 locv=nodplc(loc+1)
      if (xxor(anam,value(locv)).ne.0) go to 30
      if (nsbckt.eq.0) go to 25
      if (nodplc(loc-1).ne.id) go to 30
   25 if (nodplc(loc+2).eq.ndefin) go to 200
      if (iforce.eq.0) go to 200
      write (6,26) anam
   26 format('0*error*:  above line attempts to redefine ',a8/)
      nogo=1
   30 if (nodplc(loc).eq.0) go to 40
      loc=nodplc(loc)
      go to 20
c
c  reserve space for this element
c
   40 nodplc(loc)=locn
      if (nsbckt.ne.0) go to 60
   50 jelcnt(id)=jelcnt(id)+1
   60 loc=locn
      itemp=loc+lnod(id)*nwd4-1
      locv=nxtevn(itemp-1)+1
      itemp=locv-itemp
      ktmp=lnod(id)*nwd4+lval(id)*nwd8+itemp
      call extmem(ielmnt,ktmp)
      locv=(locv-1)/nwd8+1
      iptr=0
      if (nsbckt.eq.0) go to 80
      iptr=id
   80 nodplc(loc-1)=iptr
      nodplc(loc)=0
      nodplc(loc+1)=locv
      value(locv)=anam
c
c  background storage
c
  100 nodplc(loc+2)=ndefin
      nword=lnod(id)-4
      if (nword.lt.1) go to 120
      call zero4(nodplc(loc+3),nword)
  120 nword=lval(id)-1
      if (nword.lt.1) go to 200
      call zero8(value(locv+1),nword)
c
c  exit
c
  200 return
      end
      subroutine title(ifold,len,icom,coment)
      implicit double precision (a-h,o-z)
c
c     this routine writes a title on the output file.  ifold indicates
c whether the page eject should be to the next concave, convex, or any
c page fold depending on whether its value is <0, >0, or =0.  the page
c eject is suppressed (as is much of the heading) if the variable nopage
c is nonzero.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /miscel/ atime,aprog(3),adate,atitle(10),defl,defw,defad,
     1  defas,rstats(50),iwidth,lwidth,nopage
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /blank/ value(50000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension coment(4)
c
c
      if(nopage.eq.1) go to 150
c
   30 if (len.le.80) go to 100
      write (6,31) adate,aprog,atime,(atitle(i),i=1,10)
   31 format(1h1,9(2h* ),a10,1x,11(2h* ),3a8,11(2h* ),a10,9(2h *),//1h0,
     1   15a8/)
      if (icom.eq.0) go to 40
      write (6,36) coment,value(itemps+itemno)
   36 format(5h0****,17x,4a8,21x,'temperature =',f9.3,' deg c'/)
   40 write (6,41)
   41 format(1h0,63(2h* )//)
      go to 200
c
c
  100 write (6,101) adate,aprog,atime,(atitle(i),i=1,10)
  101 format(1h1,5(1h*),a10,1x,8(1h*),3a8,8(1h*),a10,5(1h*)//1h0,10a8/)
      if (icom.eq.0) go to 110
      write (6,106) coment,value(itemps+itemno)
  106 format(10h0****     ,4a8,' temperature =',f9.3,' deg c'/)
  110 write (6,111)
  111 format(1h0,71(1h*)//)
      go to 200
c
c
  150 if (icom.eq.0) go to 160
      write (6,106) coment,value(itemps+itemno)
      go to 200
  160 write (6,161) aprog
  161 format(1h0,3a8,/)
c
c  finished
c
  200 return
      end
      subroutine dcdcmp
      implicit double precision (a-h,o-z)
c
c     this routine performs an in-place lu factorization of the coef-
c ficient matrix.
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
      common /blank/ value(50000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
      data ikount /0/
c
c
      do 100 i=2,nstop
      io=nodplc(iorder+i)
      if (dabs(value(lynl+io)).ge.gmin) go to 10
      value(lynl+io)=gmin
      igoof=igoof+1
      if(ikount.gt.20) go to 10
      ikount=ikount+1
      if(io.le.nunods) write(6,9) nodplc(junode+io)
    9 format(' at node ',i5)
   10 jstart=nodplc(ilc+i)
      jstop=nodplc(ilc+i+1)-1
      if (jstart.gt.jstop) go to 100
      do 90 j=jstart,jstop
      value(lyl+j)=value(lyl+j)/value(lynl+io)
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
      go to 70
c
c  find (icol,irow) matrix term (lower triangle)
c
   40 l=nodplc(ilc+irow+1)
   50 l=l-1
      if (nodplc(ilr+l).ne.icol) go to 50
      ispot=lyl+l
      go to 70
c
c  find (icol,irow) matrix term (diagonal)
c
   60 ispot=lynl+nodplc(iorder+irow)
c
   70 value(ispot)=value(ispot)-value(lyl+j)*value(lyu+k)
   80 continue
   90 continue
  100 continue
      return
      end
      subroutine dcsol
      implicit double precision (a-h,o-z)
c
c     this routine solves the system of circuit equations by performing
c a forward and backward substitution step using the previously-computed
c lu factors.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /blank/ value(50000)
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
      if (value(lvn+io).eq.0.0d0) go to 20
      do 10 j=jstart,jstop
      jo=nodplc(ilr+j)
      jo=nodplc(iorder+jo)
      value(lvn+jo)=value(lvn+jo)-value(lyl+j)*value(lvn+io)
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
      value(lvn+io)=value(lvn+io)-value(lyu+j)*value(lvn+jo)
   30 continue
   40 value(lvn+io)=value(lvn+io)/value(lynl+io)
   50 continue
      return
      end
       subroutine setmem(ipntr,ksize)
      implicit double precision (a-h,o-z)
c
c     this routine performs dynamic memory management.  it is used in
c     spice2, and useable in any program.
c
c     memory is managed within an array selected by the calling program.
c     one may either dimension this array to the 'maxmem' size, or more
c     desirably, find the address of the first available word of memory
c     above your program, and dimension your array to '1'.  passing the
c     address of the first data word available permits the manager to
c     use 'illegal' indices into the data area.
c
c     this routine must have access to an integer function called 'locf'
c     which returns the address of its argument.  addresses as used by this
c     program refer to 'integer' addresses, not byte addresses.
c
c entry points:
c      setmem - set initial memory
c      getm4  - get block for table of integers
c      getm8  - get block for table of floating point variables
c      getm16 - get block for table of complex variables
c      relmem - release part of block
c      extmem - extend size of existing block
c      sizmem - determine size of existing block
c      clrmem - release block
c      ptrmem - reset memory pointer
c      crunch - force memory compaction
c      avlm4  - amount of space available (integers)
c      avlm8  - amount of space available (real)
c      avlm16 - amount of space available (complex)
c
c calling sequences:
c      call setmem(imem(1),maxmem)
c      call setmem(imem(1),maxmem,kfamwa)  non 3000 machines kfamwa is
c                                          address of first available word
c                                          of data
c      call getm4 (ipntr,blksiz)  where blksize is the number of entries
c      call getm8 (ipntr,blksiz)
c      call getm16(ipntr,blksiz)
c      call relmem(ipntr,relsiz)
c      call extmem(ipntr,extsiz)  extsiz is the number of entries to be added
c      call sizmem(ipntr,blksiz)
c      call clrmem(ipntr)
c      call ptrmem(ipntr1,ipntr2)
c      call avlm4(ispace)
c      call avlm8(ispace)
c      call avlm16(ispace)
c      call crunch
c
c
c general comments:
c      for each block which is allocated, a 5-word entry is maintained
c in a table kept in high memory, of the form
c
c        word      contents
c        ----      --------
c
c          1       index of imem(.) into origin of block
c                  i.e. contents of pointer (used for error check)
c          2       block size (in words)
c          3       number of words in use
c          4       address of variable containing block origin
c          5       number of words used per table entry
c
c      all allocated blocks are an 'even' (nxtevn) number of words in length,
c where a 'word' is the storage unit required for an 'integer' variable.
c      since block repositioning may be necessary, the convention that
c only one variable contain a block origin should be observed.
c      for *getmem*, *ipntr* is set such that *array(ipntr+1)* is the
c first word of the allocated block.  'ipntr' is set to address the first
c entry of the table when used with the appropriate variable type, i.e.,
c nodplc(ipntr+1), value(ipntr+1), or cvalue(ipntr+1).
c      for *clrmem*, *ipntr* is set to 'invalid' to enable rapid detection
c of an attempt to use a cleared block.
c      if any fatal errors are found, a message is printed and a flag
c set inhibiting further action until *setmem* is called.  (in this
c context, insufficient memory is considered a fatal error.)
c      throughout this routine, *ldval* always contains the subscript of
c the last addressable word of memory, *memavl* always contains the
c number of available words of memory, *numblk* always contains the
c number of allocated blocks, and istack(*loctab* +1) always contains
c the first word of the block table.
c
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
c
c.. arguments to memory manager are set up as arrays, even though
c.. the calling programs usually use simple variables for arguments.
c.. this is necessary if we are to guarantee that the parameters are
c.. passed by 'address' and not by 'value'.  we must insure that locf(arg)
c.. returns the address of the argument, and not the address of a local
c.. copy of the argument.  as currently configured, this subroutine should
c.. work on any ansi fortran compiler, provided the function 'locf' can
c.. be provided.
      dimension ipntr(1)
c
      logical memptr
c
c...  approximate time required to copy *nwords* integer values
      nwd4=1
      nwd8=2
      nwd16=4
      memerr=0
      nevn=nxtevn(1)
      icheck=mod(nevn,nwd4)+mod(nevn,nwd8)+mod(nevn,nwd16)+
     1  mod(nxtmem(1),nevn)
      if(icheck.eq.0) go to 2
      memerr=1
      call errmem(6,memerr,ipntr(1))
    2 cpyknt=0.0d0
       ifamwa=locf(ipntr(1))
      maxmem=ksize
      ntab=nxtevn(5)
c... add 'lorg' to an address and you get the 'istack' index to that word
      lorg=1-locf(istack(1))
      ifwa=ifamwa+lorg-1
      nwoff=locf(ipntr(1))+lorg-1
      icore=nxtmem(1)
c... don't take chances, back off from 'end of memory' by nxtevn(1)
      ldval=ifwa+nxtmem(1)-nxtevn(1)
      memavl=ldval-ntab-ifwa
      maxcor=0
      maxuse=0
      call memory
      if(memerr.ne.0) call errmem(6,memerr,ipntr(1))
      numblk=1
      loctab=ldval-ntab
       istack(loctab+1)=0
      istack(loctab+2)=memavl
      istack(loctab+3)=0
      istack(loctab+4)=-1
      istack(loctab+5)=1
      return
      end
      subroutine getm4(ipntr,ksize)
      implicit double precision (a-h,o-z)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      dimension ipntr(1)
      iwsize=nwd4
      call getmx(ipntr(1),ksize,iwsize)
      return
      end
      subroutine getm8(ipntr,ksize)
      implicit double precision (a-h,o-z)
      dimension ipntr(1)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      iwsize=nwd8
      call getmx(ipntr(1),ksize,iwsize)
      return
      end
      subroutine getm16(ipntr,ksize)
      implicit double precision (a-h,o-z)
      dimension ipntr(1)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      iwsize=nwd16
      call getmx(ipntr(1),ksize,iwsize)
      return
      end
      subroutine getmx(ipntr,ksize,iwsize)
      implicit double precision (a-h,o-z)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      logical memptr
      dimension ipntr(1)
c
c***  getmem - get block
c
c
      isize=ksize*iwsize
c...  check for valid size
      if (isize.ge.0) go to 5
      memerr=2
      call errmem(3,memerr,ipntr(1))
c...  check for attempt to reallocate existing block
    5 if (.not.memptr(ipntr(1))) go to 8
      memerr=3
      call errmem(3,memerr,ipntr(1))
    8 jsize=nxtevn(isize)
      call comprs(0,ldval)
c...  check if enough space already there
      need=jsize+ntab-memavl
      if (need.le.0) go to 10
c...  insufficient space -- bump memory size
      need=nxtmem(need)
      icore=icore+need
      call memory
      if(memerr.ne.0) call errmem(3,memerr,ipntr(1))
      ltab1=ldval-ntab
      istack(ltab1+2)=istack(ltab1+2)+need
c...  relocate block entry table
      nwords=numblk*ntab
      cpyknt=cpyknt+dfloat(nwords)
      call copy4(istack(loctab+1),istack(loctab+need+1),nwords)
      loctab=loctab+need
      ldval=ldval+need
      memavl=memavl+need
c...  a block large enough now exists -- allocate it
   10 ltab1=ldval-ntab
      morg=istack(ltab1+1)
      msiz=istack(ltab1+2)
      muse=istack(ltab1+3)
      muse=nxtevn(muse)
      madr=istack(ltab1+4)
c...  construct new table entry
   15 istack(ltab1+2)=muse
      loctab=loctab-ntab
      nwords=numblk*ntab
      cpyknt=cpyknt+dfloat(nwords)
      call copy4(istack(loctab+ntab+1),istack(loctab+1),nwords)
      numblk=numblk+1
      memavl=memavl-ntab
      istack(ltab1+1)=morg+muse
      istack(ltab1+2)=msiz-muse-ntab
c...  set user size into table entry for this block
   20 istack(ltab1+3)=isize
      istack(ltab1+4)=locf(ipntr(1))
      istack(ltab1+5)=iwsize
      memavl=memavl-jsize
      ipntr(1)=istack(ltab1+1)/iwsize
      call memadj
      return
      end
      subroutine avlm4(iavl)
      implicit double precision (a-h,o-z)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
c
c***  avlmem - how much space is available ?
c
      iavl=((maxmem-icore)/nxtmem(1))*nxtmem(1)-ntab+memavl
      iavl=iavl/nwd4
      return
      end
      subroutine avlm8(iavl)
      implicit double precision (a-h,o-z)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      iavl=((maxmem-icore)/nxtmem(1))*nxtmem(1)-ntab+memavl
      iavl=iavl/nwd8
      return
      end
      subroutine avlm16(iavl)
      implicit double precision (a-h,o-z)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      iavl=((maxmem-icore)/nxtmem(1))*nxtmem(1)-ntab+memavl
      iavl=iavl/nwd16
      return
      end
      subroutine relmem(ipntr,ksize)
      implicit double precision (a-h,o-z)
      dimension ipntr(1)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      logical memptr
c
c***  relmem - release part of block
c
c
c...  check for valid pointer
      if (memptr(ipntr(1))) go to 10
      memerr=5
      call errmem(5,memerr,ipntr(1))
   10 isize=ksize*istack(ltab+5)
c...  check for valid size
      if (isize.ge.0) go to 20
      memerr=2
      call errmem(5,memerr,ipntr(1))
   20 jsize=istack(ltab+3)
      if (isize.le.jsize) go to 30
      memerr=6
      call errmem(5,memerr,ipntr(1))
   30 istack(ltab+3)=istack(ltab+3)-isize
      memavl=memavl+(nxtevn(jsize)-nxtevn(istack(ltab+3)))
      call memadj
      return
      end
      subroutine extmem(ipntr,ksize)
      implicit double precision (a-h,o-z)
      dimension ipntr(1)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      logical memptr
c
c***  extmem - extend size of existing block
c
c
c...  check for valid pointer
      if (memptr(ipntr(1))) go to 10
      memerr=5
      call errmem(2,memerr,ipntr(1))
   10 isize=ksize*istack(ltab+5)
c...  check for valid size
      if (isize.ge.0) go to 20
      memerr=2
      call errmem(2,memerr,ipntr(1))
c...  check if enough space already there
   20 if ((istack(ltab+2)-istack(ltab+3)).ge.isize) go to 40
      need=nxtevn(isize)-memavl
      if (need.le.0) go to 30
c...  insufficient space -- bump memory size
      need=nxtmem(need)
      icore=icore+need
      call memory
      if(memerr.ne.0) call errmem(2,memerr,ipntr(1))
      ltab1=ldval-ntab
      istack(ltab1+2)=istack(ltab1+2)+need
c...  relocate block entry table
      nwords=numblk*ntab
      cpyknt=cpyknt+dfloat(nwords)
      call copy4(istack(loctab+1),istack(loctab+need+1),nwords)
      loctab=loctab+need
      ldval=ldval+need
      memavl=memavl+need
      ltab=ltab+need
c...  move blocks to make space
   30 continue
      call comprs(0,ltab)
      call comprs(1,ltab)
   40 jsize=istack(ltab+3)
      istack(ltab+3)=istack(ltab+3)+isize
      memavl=memavl-(nxtevn(istack(ltab+3))-nxtevn(jsize))
      call memadj
      return
      end
      subroutine sizmem(ipntr,ksize)
      implicit double precision (a-h,o-z)
      dimension ipntr(1)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      logical memptr
c
c***  sizmem - determine size of existing block
c
c
c...  check for valid pointer
      if (memptr(ipntr(1))) go to 10
      memerr=5
      call errmem(7,memerr,ipntr(1))
   10 ksize=istack(ltab+3)/istack(ltab+5)
      return
      end
      subroutine clrmem(ipntr)
      implicit double precision (a-h,o-z)
      dimension ipntr(1)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      logical memptr
c
c***  clrmem - release block
c
c
c...  check that pointer is valid
      if (memptr(ipntr(1))) go to 10
      memerr=5
      call errmem(1,memerr,ipntr(1))
   10 msiz=istack(ltab+2)
      muse=istack(ltab+3)
      memavl=memavl+nxtevn(muse)
c...  assumption:  first allocated block is never cleared.
      ltab1=ltab-ntab
      istack(ltab1+2)=istack(ltab1+2)+msiz
c...  reposition the block table
      nwords=ltab-loctab
      cpyknt=cpyknt+dfloat(nwords)
      call copy4(istack(loctab+1),istack(loctab+ntab+1),nwords)
      numblk=numblk-1
      loctab=loctab+ntab
      memavl=memavl+ntab
      ltab1=ldval-ntab
      istack(ltab1+2)=istack(ltab1+2)+ntab
      ipntr(1)=2**31-1
      call memadj
      return
      end
      subroutine ptrmem(ipntr,ipntr2)
      implicit double precision (a-h,o-z)
      dimension ipntr(1),ipntr2(1)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      logical memptr
c
c***  ptrmem - reset memory pointer
c
c...  verify that pointer is valid
      if (memptr(ipntr(1))) go to 10
      memerr=5
      call errmem(4,memerr,ipntr(1))
c...  reset block pointer to be *ipntr2*
   10 ipntr2(1)=ipntr(1)
      istack(ltab+4)=locf(ipntr2(1))
      call memadj
      return
      end
      subroutine crunch
      implicit double precision (a-h,o-z)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
c
c***  crunch - force memory compaction
c
      call comprs(0,ldval)
      call memadj
      return
      end
      subroutine errmem(inam,ierror,ipntr)
      implicit double precision (a-h,o-z)
      dimension ipntr(1)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      dimension errnam(7)
      data errnam /6hclrmem,6hextmem,6hgetmem,6hptrmem,6hrelmem,
     1   6hsetmem,6hsizmem/
c
      go to (200,410,420,300,510,530),ierror
c
c*** error(s) found ***
c
c.. nxtevn and/or nxtmem incompatible with nwd4, nwd8, and nwd16
c
  200 write(6,201)
  201 format('0memory manager variables nwd4-8-16 incompatible with nxte
     1vn and nxtmem')
      go to 900
c
c...  memory needs exceed maximum available space
  300 write (6,301) maxmem
  301 format('0*error*:  memory needs exceed',i6,/,
     1  '0probable remedy, replace your "// exec spice" card with',/
     2  '0// exec spice,region=2000k')
      go to 900
c...    *isize* < 0
  410 write(6,411)
  411 format('0size parameter negative')
      go to 900
c...  getmem:  attempt to reallocate existing block
  420 write(6,421)
  421 format('0attempt to reallocate existing table')
      go to 900
c...    *ipntr* invalid
  510 write(6,511)
  511 format('0table pointer invalid')
      go to 900
c...  relmem:  *isize* larger than indicated block
  530 write(6,531)
  531 format('0attempt to release more than total table')
c...  issue error message
  900 write (6,901) errnam(inam)
  901 format('0*abort*:  internal memory manager error at entry ',
     1  a7)
  950 call dmpmem(ipntr(1))
 1000 stop
      end
      subroutine memadj
      implicit double precision (a-h,o-z)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
c
c*** adjust memory downward ***
c
   50 maxuse=max0(maxuse,ldval-memavl-ifwa)
      memdec=2*nxtmem(1)
      if (memavl.lt.memdec) return
c...  compress current allocations of memory
      call comprs(0,ldval)
c...  adjust memory size
      memdel=0
   60 icore=icore-memdec
      memdel=memdel+memdec
      memavl=memavl-memdec
      if (memavl.ge.memdec) go to 60
      ltab1=ldval-ntab
      istack(ltab1+2)=istack(ltab1+2)-memdel
c...  relocate block entry table
      nwords=numblk*ntab
      cpyknt=cpyknt+dfloat(nwords)
      call copy4(istack(loctab+1),istack(loctab-memdel+1),nwords)
      loctab=loctab-memdel
      ldval=ldval-memdel
      call memory
      return
      end
      integer function nxtevn(n)
c
c.. function returns the smallest value nxtevn greater than or equal to
c.. n which is evenly divisible by 'nwd4, nwd8, and nwd16' as defined
c.. in setmem
c
      nxtevn=((n+3)/4)*4
      return
      end
      integer function nxtmem(memwds)
c
c.. function returns the in nxtmem the next available memory size
c.. (which must be evenly divisible by 'nwd4, nwd8, and nwd16' as
c.. defined in setmem
c
      nxtmem=((memwds+1999)/2000)*2000
      return
      end
      subroutine comprs(icode,limit)
      implicit double precision (a-h,o-z)
c
c      this routine compresses all available memory into a single block.
c if *icode* is zero, compression of memory from word 1 to *limit* is
c done;  otherwise, compression from *ldval* down to *limit* is done.
c
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
c
c...  approximate time required to copy *nwords* real values
      if (icode.ne.0) go to 100
      nblk=numblk
      ltab2=loctab
   10 ltab1=ltab2
      if (ltab1.ge.limit) go to 200
      if (nblk.eq.1) go to 200
      nblk=nblk-1
      ltab2=ltab1+ntab
      morg=istack(ltab1+1)
      msiz=istack(ltab1+2)
      muse=istack(ltab1+3)
      muse=nxtevn(muse)
      if (msiz.eq.muse) go to 10
c...  move succeeding block down
      morg2=istack(ltab2+1)
      muse2=istack(ltab2+3)
      madr2=istack(ltab2+4)
      iwsize=istack(ltab2+5)
      if (madr2.ne.0) go to 15
      if (muse2.eq.0) go to 20
   15 cpyknt=cpyknt+dfloat(muse2)
      call copy4(istack(nwoff+morg2+1),istack(nwoff+morg+muse+1),muse2)
      istack(lorg+madr2)=(morg+muse)/iwsize
   20 istack(ltab1+2)=muse
      istack(ltab2+1)=morg+muse
      istack(ltab2+2)=istack(ltab2+2)+(msiz-muse)
      go to 10
c
c
  100 nblk=numblk
      ltab2=ldval-ntab
  110 ltab1=ltab2
      if (ltab1.le.limit) go to 200
      if (nblk.eq.1) go to 200
      nblk=nblk-1
      ltab2=ltab1-ntab
      morg=istack(ltab1+1)
      msiz=istack(ltab1+2)
      muse=istack(ltab1+3)
      muse=nxtevn(muse)
      madr=istack(ltab1+4)
      iwsize=istack(ltab1+5)
      mspc=msiz-muse
      if (mspc.eq.0) go to 110
      cpyknt=cpyknt+dfloat(muse)
      call copy4(istack(nwoff+morg+1),istack(nwoff+morg+mspc+1),muse)
      istack(ltab1+1)=morg+mspc
      istack(ltab1+2)=muse
      istack(ltab2+2)=istack(ltab2+2)+mspc
      if (madr.eq.0) go to 110
      istack(lorg+madr)=(morg+mspc)/iwsize
      go to 110
c...  all done
  200 return
      end
      logical function memptr(ipntr)
      implicit double precision (a-h,o-z)
c
c      this routine checks whether *ipntr* is a valid block pointer.
c if it is valid, *ltab* is set to point to the corresponding entry in
c the block table.
c
c... ipntr is an array to avoid 'call by value' problems (see setmem)
      dimension ipntr(1)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
c
      memptr=.false.
      ltab=loctab
      locpnt=locf(ipntr(1))
      do 20 i=1,numblk
      if (locpnt.ne.istack(ltab+4)) go to 10
      if (ipntr(1)*istack(ltab+5).ne.istack(ltab+1)) go to 10
      memptr=.true.
      go to 30
   10 ltab=ltab+ntab
   20 continue
   30 return
      end
      subroutine dmpmem(ipntr)
      implicit double precision (a-h,o-z)
c
c      this routine prints out the current memory allocation map.
c *ipntr* is the table pointer of the current memory manager call
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
c... ipntr is an array to avoid 'call by value' problems
      dimension ipntr(1)
      dimension aptr(61)
      data aptr /6hielmnt,6hisbckt,6hnsbckt,6hiunsat,6hnunsat,6hitemps,
     1 6hnumtem,6hisens ,6hnsens ,6hifour ,6hnfour ,6hifield,
     2 6hicode ,6hidelim,6hicolum,6hinsize,
     3 6hjunode,6hlsbkpt,6hnumbkp,6hiorder,6hjmnode,
     4 6hiur   ,6hiuc   ,6hilc   ,6hilr   ,6hnumoff,6hisr   ,
     5 6hnmoffc,6hiseq  ,6hiseq1  ,6hneqn  ,6hnodevs,
     6 6hndiag ,6hiswap ,6hiequa ,6hmacins,6hlvnim1,
     7 6hlx0   ,6hlvn   ,6hlynl  ,6hlyu   ,6hlyl   ,
     8 6hlx1   ,6hlx2   ,6hlx3   ,6hlx4   ,6hlx5   ,6hlx6   ,
     9 6hlx7   ,6hld0   ,6hld1   ,6hltd   ,6himynl ,6himvn  ,6hloutpt,
     * 6hnsnod ,6hnsmat ,6hnsval ,6hicnod ,6hicmat ,6hicval /
      data ablnk /1h /
      iaddr=locf(ielmnt)-1
      itemp=locf(ipntr(1))-iaddr
      anam=ablnk
      if(itemp.gt.0.and.itemp.le.61) anam=aptr(itemp)
      iadr=locf(ipntr(1))
      write (6,5) anam,iadr,icore,maxmem,memavl,ldval
    5 format('0current pointer 'a6,'@ = z',z6,/' corsiz=',i7,
     1  /' maxmem=',i7,/' avlspc=',i7,/' ldval=',i7,
     2  /1h0,24x,'memory allocation map'/14x,'blknum memorg memsiz',
     3  '  memuse usrptr  addr    name')
      ltab1=loctab
      do 20 i=1,numblk
      morg=istack(ltab1+1)
      msiz=istack(ltab1+2)
      muse=istack(ltab1+3)
      madr=istack(ltab1+4)
      anam=ablnk
      ndex=madr-iaddr
      if(ndex.gt.0.and.ndex.le.61) anam=aptr(ndex)
      jptr=0
      if (madr.gt.0) jptr=istack(lorg+madr)
      write (6,11) i,morg,msiz,muse,jptr,madr,anam
   11 format(13x,5i7,3x,z7,'z',1x,a6)
      ltab1=ltab1+ntab
   20 continue
      write (6,21)
   21 format(1h0,24x,'end of allocation map'/)
      return
      end
      subroutine memory
      implicit double precision (a-h,o-z)
      common /memmgr/ cpyknt,istack(1),lorg,icore,maxcor,maxuse,memavl,
     1   ldval,numblk,loctab,ltab,ifwa,nwoff,ntab,maxmem,memerr,nwd4,
     2   nwd8,nwd16
      if(icore.le.maxmem) go to 10
      memerr=4
      return
   10 continue
      return
      end
      subroutine magphs(cvar,xmag,xphs)
      implicit double precision (a-h,o-z)
c
c     this routine computes the magnitude and phase of its complex arg-
c ument cvar, storing the results in xmag and xphs.
c
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      complex*16 cvar
c
c
      xreal=dreal(cvar)
      ximag=dimag(cvar)
      xmag=dsqrt(xreal*xreal+ximag*ximag)
      if (xmag.ge.1.0d-20) go to 10
      xmag=1.0d-20
      xphs=0.0d0
      return
   10 xphs=rad*datan2(ximag,xreal)
      return
      end
      integer function xxor(a,b)
      implicit double precision (a-h,o-z)
c
c     this routine computes a single-precision integer result which is
c the result of exclusive-or*ing the two real-valued arguments a and b
c together.
c
      xxor=1
      if(a.eq.b) xxor=0
      return
      end
      subroutine outnam(loc,ktype,string,ipos)
      implicit double precision (a-h,o-z)
c
c     this routine constructs the 'name' for the output variable indi-
c cated by loc, adding the characters to the character array 'string',
c beginning with the position marked by ipos.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /blank/ value(50000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
      dimension string(1)
      dimension aout(19),lenout(19),aopt(5),lenopt(5)
      data aout / 6hv     , 6hvm    , 6hvr    , 6hvi    , 6hvp    ,
     1            6hvdb   , 6hi     , 6him    , 6hir    , 6hii    ,
     2            6hip    , 6hidb   , 6honoise, 6hinoise, 6hhd2   ,
     1            6hhd3   , 6hdim2  , 6hsim2  , 6hdim3   /
      data lenout / 1,2,2,2,2,3,1,2,2,2,2,3,6,6,3,3,4,4,4 /
      data aopt / 5hmag  , 5hreal , 5himag , 5hphase, 5hdb    /
      data lenopt / 3,4,4,5,2 /
      data alprn, acomma, arprn, ablnk / 1h(, 1h,, 1h), 1h  /
c
c
      ioutyp=nodplc(loc+5)
      if (ioutyp.ge.2) go to 10
      lout=ktype+ioutyp*6
      go to 20
   10 lout=ioutyp+11
   20 call move(string,ipos,aout(lout),1,lenout(lout))
      ipos=ipos+lenout(lout)
      if (ioutyp.ge.2) go to 200
      call move(string,ipos,alprn,1,1)
      ipos=ipos+1
      if (ioutyp.ne.0) go to 100
      node1=nodplc(loc+2)
      call alfnum(nodplc(junode+node1),string,ipos)
      node2=nodplc(loc+3)
      if (node2.eq.1) go to 30
      call move(string,ipos,acomma,1,1)
      ipos=ipos+1
      call alfnum(nodplc(junode+node2),string,ipos)
   30 call move(string,ipos,arprn,1,1)
      ipos=ipos+1
      go to 1000
c
  100 locv=nodplc(loc+1)
      anam=value(locv)
      achar=ablnk
      do 110 i=1,8
      call move(achar,1,anam,i,1)
      if (achar.eq.ablnk) go to 120
      call move(string,ipos,achar,1,1)
      ipos=ipos+1
  110 continue
  120 call move(string,ipos,arprn,1,1)
      ipos=ipos+1
      go to 1000
c
  200 if (ktype.eq.1) go to 1000
      call move(string,ipos,alprn,1,1)
      ipos=ipos+1
      call move(string,ipos,aopt(ktype-1),1,lenopt(ktype-1))
      ipos=ipos+lenopt(ktype-1)
      call move(string,ipos,arprn,1,1)
      ipos=ipos+1
c
c  finished
c
 1000 return
      end
      subroutine alfnum(number,string,ipos)
      implicit double precision (a-h,o-z)
c
c     this routine converts number into character form, storing the
c characters in the character array string, beginning with the position
c indicated by ipos.
c
c **** note that the 'ipos' variable is changed to indicate the position
c      of the next unwritten character.  this could clobber constants if
c      ipos is not a variable in the calling program
c
      dimension string(1)
      dimension adigit(10)
      data adigit / 1h0,1h1,1h2,1h3,1h4,1h5,1h6,1h7,1h8,1h9 /
      data aminus / 1h- /
c
c
      num=number
c
c  check for number < 0
c
      if (num.ge.0) go to 10
      num=-num
c...  negative number:  insert minus sign
      call move(string,ipos,aminus,1,1)
      ipos=ipos+1
c
c  convert number one digit at a time, in reverse order
c
   10 istart=ipos
   20 numtmp=num/10
      idigit=num-numtmp*10
      call move(string,ipos,adigit(idigit+1),1,1)
      ipos=ipos+1
      num=numtmp
      if (num.ne.0) go to 20
      istop=ipos-1
c
c  now reverse the order of the digits
c
   30 if (istop.le.istart) go to 40
      call move(tmpdgt,1,string,istart,1)
      call move(string,istart,string,istop,1)
      call move(string,istop,tmpdgt,1,1)
      istart=istart+1
      istop=istop-1
      go to 30
c
c  conversion complete
c
   40 return
      end
      subroutine getcje
      implicit double precision (a-h,o-z)
      common /cje/ maxtim,itime,icost
      call second(xtime)
      itime=xtime
      icost=xtime*38.3333
      return
      end
      subroutine move(a,iposa,b,iposb,nchar)
      character a(1),b(1)
      do 10 i=1,nchar
      a(iposa+i-1)=b(iposb+i-1)
   10 continue
      return
      end
      subroutine copy4(ifrom,ito,nwords)
      implicit double precision (a-h,o-z)
c
      dimension ifrom(1),ito(1)
c     this routine copies a block of #nwords# words (of the appropriate
c type) from the array #from# to the array #to#.  it determines from
c which end of the block to transfer first, to prevent over-stores which
c might over-write the data.
c
      if (nwords.eq.0) return
      if (locf(ifrom(1)).lt.locf(ito(1))) go to 20
c...  locf() returns as its value the address of its argument
      do 10 i=1,nwords
      ito(i)=ifrom(i)
   10 continue
      return
c
   20 i=nwords
   30 ito(i)=ifrom(i)
      i=i-1
      if (i.ne.0) go to 30
      return
c
c
      end
      subroutine copy8(rfrom,rto,nwords)
      implicit double precision (a-h,o-z)
c
      dimension rfrom(1),rto(1)
      if (nwords.eq.0) return
      if (locf(rfrom(1)).lt.locf(rto(1))) go to 120
      do 110 i=1,nwords
      rto(i)=rfrom(i)
  110 continue
      return
c
  120 i=nwords
  130 rto(i)=rfrom(i)
      i=i-1
      if (i.ne.0) go to 130
      return
c
c
      end
      subroutine copy16(cfrom,cto,nwords)
      implicit double precision (a-h,o-z)
c
      complex*16 cfrom(1),cto(1)
      if (nwords.eq.0) return
      if (locf(cfrom(1)).lt.locf(cto(1))) go to 220
      do 210 i=1,nwords
      cto(i)=cfrom(i)
  210 continue
      return
c
  220 i=nwords
  230 cto(i)=cfrom(i)
      i=i-1
      if (i.ne.0) go to 230
      return
      end
      subroutine zero4(iarray,length)
      implicit double precision (a-h,o-z)
c
      dimension iarray(1)
c     this routine zeroes the memory locations indicated by array(1)
c through array(length).
c
      if (length.eq.0) return
      do 10 i=1,length
      iarray(i)=0
   10 continue
      return
      end
      subroutine zero8(array,length)
      implicit double precision (a-h,o-z)
c
      dimension array(1)
c     this routine zeroes the memory locations indicated by array(1)
c through array(length).
c
      if (length.eq.0) return
      do 10 i=1,length
      array(i)=0.0d0
   10 continue
      return
      end
      subroutine zero16(carray,length)
      implicit double precision (a-h,o-z)
      complex*16 carray(1)
c
c     this routine zeroes the memory locations indicated by array(1)
c through array(length).
c
      if (length.eq.0) return
      do 10 i=1,length
      carray(i)=dcmplx(0.0d0,0.0d0)
   10 continue
      return
c
c
c
      end
       integer function locf(ivar)
       iabsa=loc(ivar)
      locf=iabsa/4
       if(iabsa.eq.locf*4) return
      write(6,100) iabsa
  100 format('0*error*:  system 370 error..address ',t10,
     1  ' is not on a 4-byte boundary')
      stop
      end
      subroutine mdate(anam)
      implicit double precision (a-h,o-z)
      call date(anam)
      return
      end
      subroutine mclock(anam)
      implicit double precision (a-h,o-z)
      call todalf(anam)
  100 return
      end
      subroutine second(t1)
      implicit double precision (a-h,o-z)
      dimension ibuff(4)
      real*8 t1
      call times (ibuff)
      t1 = dfloat (ibuff(1)) / 60.d0
      return
      end
      subroutine todalf(anam)
      double precision anam
      anam=0.0d0
      return
      end
      double precision function cpusec(time)
      cpusec=0.0d0
      return
      end
      subroutine date(anam)
      double precision anam
      anam=1.0d0
      return
      end
      FUNCTION DREAL ( X )
      COMPLEX*16 X
      DREAL = REAL (X)
      RETURN
      END
cunix FUNCTION DIMAG ( X )
cunix COMPLEX*16 X
cunix DIMAG = AIMAG (X)
cunix RETURN
cunix END
cunix COMPLEX FUNCTION DCMPLX ( X , Y )
cunix DCMPLX = CMPLX ( X , Y )
cunix RETURN
cunix END
cunix COMPLEX FUNCTION DCONJG ( X )
cunix COMPLEX*16 X
cunix DCONJG = CONJG ( X )
cunix RETURN
cunix END
      FUNCTION IFIXD ( X )
      IFIXD = IFIX ( X )
      RETURN
      END
