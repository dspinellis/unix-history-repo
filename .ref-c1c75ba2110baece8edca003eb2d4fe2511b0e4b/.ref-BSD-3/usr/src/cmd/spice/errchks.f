      subroutine errchk
      implicit double precision (a-h,o-z)
c
c
c     this routine drives the pre-processing and general error-checking
c of input performed by spice.
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
      common /cje/ maxtim,itime,icost
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
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
c
      dimension titlop(4)
      dimension nnods(50),aname(2)
      data aname / 4htrap, 4hgear /
      data titlop / 8hoption s, 8hummary  , 8h        , 8h         /
      data ndefin / 2h.u /
      data nnods / 2, 2, 2, 0, 2, 2, 2, 2, 2, 2,
     1             2, 4, 3, 4, 0, 0, 4, 0, 1, 0,
     2             0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     3             0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     4             2, 2, 2, 0, 0, 0, 0, 0, 0, 0 /
      data aelmt,amodel,aoutpt /7helement,5hmodel,6houtput/
      data alsdc,alstr,alsac / 2hdc, 4htran, 2hac /
c
c
      call second(t1)
      do 60 id=1,50
      loc=locate(id)
   10 if (loc.eq.0) go to 60
      if (nodplc(loc+2).ne.ndefin) go to 50
      nogo=1
      locv=nodplc(loc+1)
      if (id.ge.21) go to 20
      anam=aelmt
      go to 40
   20 if (id.ge.31) go to 30
      anam=amodel
      go to 40
   30 anam=aoutpt
   40 write (6,41) anam,value(locv)
   41 format('0*error*:  ',2a8,' has been referenced but not defined'/)
   50 loc=nodplc(loc)
      go to 10
   60 continue
      if (nogo.ne.0) go to 2000
c
c  construct ordered list of user specified nodes
c
      call getm4(junode,1)
      nodplc(junode+1)=0
      nunods=1
      do 180 id=1,50
      if (nnods(id).eq.0) go to 180
      loc=locate(id)
  110 if (loc.eq.0) go to 180
      if (id.le.4) go to 120
      if (id.le.8) go to 150
      if (id.eq.19) go to 165
      if (id.le.40) go to 120
      if (id.le.43) go to 170
  120 jstop=loc+nnods(id)-1
      do 130 j=loc,jstop
      call putnod(nodplc(j+2))
  130 continue
      go to 170
  150 call putnod(nodplc(loc+2))
      call putnod(nodplc(loc+3))
      if (id.ge.7) go to 170
      locp=nodplc(loc+id+1)
      nssnod=2*nodplc(loc+4)
  155 do 160 j=1,nssnod
      call putnod(nodplc(locp+j))
  160 continue
      go to 170
  165 locp=nodplc(loc+2)
      call sizmem(nodplc(loc+2),nssnod)
      go to 155
  170 loc=nodplc(loc)
      go to 110
  180 continue
      if (nogo.ne.0) go to 2000
      ncnods=nunods
c
c  assign program nodes
c
  200 do 280 id=1,50
      if (nnods(id).eq.0) go to 280
      loc=locate(id)
  210 if (loc.eq.0) go to 280
      if (id.le.4) go to 220
      if (id.le.8) go to 250
      if (id.eq.19) go to 265
      if (id.le.40) go to 220
      if (id.le.43) go to 240
  220 jstop=loc+nnods(id)-1
      do 230 j=loc,jstop
      call getnod(nodplc(j+2))
  230 continue
      go to 270
  240 if (nodplc(loc+5).eq.0) go to 220
      go to 270
  250 call getnod(nodplc(loc+2))
      call getnod(nodplc(loc+3))
      if (id.ge.7) go to 270
      locp=nodplc(loc+id+1)
      nssnod=2*nodplc(loc+4)
  255 do 260 j=1,nssnod
      call getnod(nodplc(locp+j))
  260 continue
      go to 270
  265 locp=nodplc(loc+2)
      call sizmem(nodplc(loc+2),nssnod)
      go to 255
  270 loc=nodplc(loc)
      go to 210
  280 continue
c
c  check and set .nodeset nodes to their internal values
c
      call sizmem(nsnod,nic)
      if(nic.eq.0) go to 300
      do 290 i=1,nic
      call getnod(nodplc(nsnod+i))
  290 continue
c
c   check and set .ic nodes to their internal values
c
  300 call sizmem(icnod,nic)
      if(nic.eq.0) go to 320
      do 310 i=1,nic
      call getnod(nodplc(icnod+i))
  310 continue
  320 if (nogo.ne.0) go to 2000
c
c  expand subcircuit calls
c
      call subckt
      if (nogo.ne.0) go to 2000
      if (ncnods.ge.2) go to 400
      write (6,321)
  321 format('0*error*:  circuit has no nodes'/)
      nogo=1
      go to 2000
  400 numnod=ncnods
c
c  link unsatisfied references
c
      call lnkref
      if (nogo.ne.0) go to 2000
c
c  generate subcircuit element names
c
      if (jelcnt(19).eq.0) go to 530
      do 520 id=1,24
      loc=locate(id)
  510 if (loc.eq.0) go to 520
      call subnam(loc)
      loc=nodplc(loc)
      go to 510
  520 continue
c
c  translate node initial conditions to device initial conditions
c  (capacitance, diode, bjt, and mosfet only
c
  530 call sizmem(icnod,nic)
      if(nic.eq.0) go to 600
      call getm8(lvnim1,numnod)
      call zero8(value(lvnim1+1),numnod)
      do 535 i=1,nic
      node=nodplc(icnod+i)
  535 value(lvnim1+node)=value(icval+i)
      loc=locate(2)
  540 if(loc.eq.0) go to 550
      locv=nodplc(loc+1)
      if(value(locv+2).ne.0.0d0) go to 545
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      value(locv+2)=value(lvnim1+node1)-value(lvnim1+node2)
  545 loc=nodplc(loc)
      go to 540
  550 loc=locate(11)
  555 if(loc.eq.0) go to 565
      locv=nodplc(loc+1)
      if(value(locv+2).ne.0.0d0) go to 560
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      value(locv+2)=value(lvnim1+node1)-value(lvnim1+node2)
  560 loc=nodplc(loc)
      go to 555
  565 loc=locate(12)
  570 if(loc.eq.0) go to 580
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      if(value(locv+2).eq.0.0d0) value(locv+2)=value(lvnim1+node2)-
     1  value(lvnim1+node3)
      if(value(locv+3).eq.0.0d0) value(locv+3)=value(lvnim1+node1)-
     1  value(lvnim1+node3)
      loc=nodplc(loc)
      go to 570
  580 loc=locate(13)
  585 if(loc.eq.0) go to 590
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      if(value(locv+2).eq.0.0d0) value(locv+2)=value(lvnim1+node1)-
     1  value(lvnim1+node3)
      if(value(locv+3).eq.0.0d0) value(locv+3)=value(lvnim1+node2)-
     1  value(lvnim1+node3)
      loc=nodplc(loc)
      go to 585
  590 loc=locate(14)
  595 if(loc.eq.0) go to 598
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      if(value(locv+5).eq.0.0d0) value(locv+5)=value(lvnim1+node1)-
     1  value(lvnim1+node3)
      if(value(locv+6).eq.0.0d0) value(locv+6)=value(lvnim1+node2)-
     1  value(lvnim1+node3)
      if(value(locv+7).eq.0.0d0) value(locv+7)=value(lvnim1+node4)-
     1  value(lvnim1+node3)
      loc=nodplc(loc)
      go to 595
  598 call clrmem(lvnim1)
c
c  process sources
c
  600 if (jtrflg.eq.0) go to 700
      do 690 id=9,10
      loc=locate(id)
  610 if (loc.eq.0) go to 690
      locv=nodplc(loc+1)
      locp=nodplc(loc+5)
      jtype=nodplc(loc+4)+1
      go to (680,620,630,640,650,675), jtype
  620 value(locp+3)=dmax1(value(locp+3),0.0d0)
      if (value(locp+4).le.0.0d0) value(locp+4)=tstep
      if (value(locp+5).le.0.0d0) value(locp+5)=tstep
      if (value(locp+6).le.0.0d0) value(locp+6)=tstop
      if (value(locp+7).le.0.0d0) value(locp+7)=tstop
      temp=value(locp+4)+value(locp+5)+value(locp+6)
      value(locp+7)=dmax1(value(locp+7),temp)
      value(locv+1)=value(locp+1)
      go to 680
  630 if (value(locp+3).le.0.0d0) value(locp+3)=1.0d0/tstop
      value(locp+4)=dmax1(value(locp+4),0.0d0)
      value(locv+1)=value(locp+1)
      go to 680
  640 value(locp+3)=dmax1(value(locp+3),0.0d0)
      if (value(locp+4).le.0.0d0) value(locp+4)=tstep
      if (value(locp+5).le.value(locp+3))
     1   value(locp+5)=value(locp+3)+tstep
      if (value(locp+6).le.0.0d0) value(locp+6)=tstep
      value(locv+1)=value(locp+1)
      go to 680
  650 value(locp+1)=dmin1(dmax1(value(locp+1),0.0d0),tstop)
      iknt=1
      call sizmem(nodplc(loc+5),nump)
  660 temp=value(locp+iknt)
      if (value(locp+iknt+2).eq.0.0d0) go to 670
      if (value(locp+iknt+2).ge.tstop) go to 670
      value(locp+iknt+2)=dmax1(value(locp+iknt+2),temp)
      if(temp.ne.value(locp+iknt+2)) go to 665
      write(6,661) value(locv)
  661 format('0*error*:  element ',a8,' piecewise linear source table no
     1t increasing in time')
      nogo=1
  665 iknt=iknt+2
      if (iknt.lt.nump) go to 660
  670 value(locp+iknt+2)=tstop
      value(locv+1)=value(locp+2)
      call relmem(nodplc(loc+5),nump-iknt-3)
      go to 680
  675 if (value(locp+3).le.0.0d0) value(locp+3)=1.0d0/tstop
      if (value(locp+5).le.0.0d0) value(locp+5)=1.0d0/tstop
      value(locv+1)=value(locp+1)
  680 loc=nodplc(loc)
      go to 610
  690 continue
c
c  use default values for mos device geometries if not specified
c
  700 loc=locate(14)
  710 if(loc.eq.0) go to 720
      locv=nodplc(loc+1)
      if(value(locv+1).le.0.0d0) value(locv+1)=defl
      if(value(locv+2).le.0.0d0) value(locv+2)=defw
      if(value(locv+3).le.0.0d0) value(locv+3)=defad
      if(value(locv+4).le.0.0d0) value(locv+4)=defas
      loc=nodplc(loc)
      go to 710
c
c  print listing of elements, process device models,
c  and check topology
c
  720 if (iprntl.eq.0) go to 730
      call elprnt
  730 call topchk
      call modchk
      if (nogo.ne.0) go to 2000
c
c  invert resistance values
c
  800 loc=locate(1)
  810 if (loc.eq.0) go to 900
      locv=nodplc(loc+1)
      value(locv+1)=1.0d0/value(locv+2)
      loc=nodplc(loc)
      go to 810
c
c  process mutual inductors
c
  900 loc=locate(4)
  910 if (loc.eq.0) go to 940
      locv=nodplc(loc+1)
      nl1=nodplc(loc+2)
      call sizmem(nodplc(nl1+10),nparam)
      if (nparam.ne.1) go to 920
      ispot1=nodplc(nl1+1)
      jspot=nodplc(nl1+10)
      value(ispot1+1)=value(jspot+1)
      if (value(ispot1+1).lt.0.0d0) go to 920
      nl2=nodplc(loc+3)
      call sizmem(nodplc(nl2+10),nparam)
      if (nparam.ne.1) go to 920
      ispot2=nodplc(nl2+1)
      jspot=nodplc(nl2+10)
      value(ispot2+1)=value(jspot+1)
      if (value(ispot2+1).lt.0.0d0) go to 920
      value(locv+1)=value(locv+1)*dsqrt(value(ispot1+1)*value(ispot2+1))
      go to 930
  920 write (6,921) value(locv)
  921 format('0*error*:  inductors coupled by ',a8,' are negative or non
     1linear'/)
      nogo=1
  930 loc=nodplc(loc)
      go to 910
  940 if (nogo.ne.0) go to 2000
c
c  limit delmax to minimum delay over 2 if transmission lines in circuit
c
      if (jtrflg.eq.0) go to 1200
      tdmax=0.0d0
      loc=locate(17)
 1010 if (loc.eq.0) go to 1200
      locv=nodplc(loc+1)
      delmax=dmin1(delmax,value(locv+2)/2.0d0)
      tdmax=dmax1(tdmax,value(locv+2))
      loc=nodplc(loc)
      go to 1010
c
c  process source parameters
c
 1200 numbkp=0
      if (jtrflg.eq.0) go to 1205
      tol=1.0d-2*delmax
      numbkp=2
      call getm8(lsbkpt,numbkp)
      value(lsbkpt+1)=0.0d0
      value(lsbkpt+2)=tstop
 1205 do 1290 id=9,10
      loc=locate(id)
 1210 if (loc.eq.0) go to 1290
      locv=nodplc(loc+1)
      locp=nodplc(loc+5)
      temp=value(locv+3)/rad
      value(locv+3)=value(locv+2)*dsin(temp)
      value(locv+2)=value(locv+2)*dcos(temp)
      if (jtrflg.eq.0) go to 1280
      jtype=nodplc(loc+4)+1
      go to (1280,1220,1230,1235,1240,1260), jtype
 1220 value(locp+4)=value(locp+4)+value(locp+3)
      temp=value(locp+5)
      value(locp+5)=value(locp+4)+value(locp+6)
      value(locp+6)=value(locp+5)+temp
      time=0.0d0
 1225 call extmem(lsbkpt,4)
      value(lsbkpt+numbkp+1)=value(locp+3)+time
      value(lsbkpt+numbkp+2)=value(locp+4)+time
      value(lsbkpt+numbkp+3)=value(locp+5)+time
      value(lsbkpt+numbkp+4)=value(locp+6)+time
      numbkp=numbkp+4
      time=time+value(locp+7)
      if (time.ge.tstop) go to 1280
      go to 1225
 1230 value(locp+3)=value(locp+3)*twopi
      call extmem(lsbkpt,1)
 1231 value(lsbkpt+numbkp+1)=value(locp+4)
      numbkp=numbkp+1
      go to 1280
 1235 call extmem(lsbkpt,2)
      value(lsbkpt+numbkp+1)=value(locp+3)
      value(lsbkpt+numbkp+2)=value(locp+5)
      numbkp=numbkp+2
      go to 1280
 1240 iknt=1
      call sizmem(nodplc(loc+5),nump)
 1250 call extmem(lsbkpt,1)
      value(lsbkpt+numbkp+1)=value(locp+iknt)
      numbkp=numbkp+1
      iknt=iknt+2
      if (iknt.le.nump) go to 1250
      go to 1280
 1260 value(locp+3)=value(locp+3)*twopi
      value(locp+5)=value(locp+5)*twopi
 1280 loc=nodplc(loc)
      go to 1210
 1290 continue
 1300 if (jtrflg.eq.0) go to 1600
      call extmem(lsbkpt,1)
      value(lsbkpt+numbkp+1)=tstop
      numbkp=numbkp+1
      call shlsrt(value(lsbkpt+1),numbkp)
      nbkpt=1
      do 1310 i=2,numbkp
      if ((value(lsbkpt+i)-value(lsbkpt+nbkpt)).lt.tol) go to 1310
      nbkpt=nbkpt+1
      value(lsbkpt+nbkpt)=value(lsbkpt+i)
      if (value(lsbkpt+nbkpt).ge.tstop) go to 1320
 1310 continue
 1320 call relmem(lsbkpt,numbkp-nbkpt)
      numbkp=nbkpt
      value(lsbkpt+numbkp)=dmax1(value(lsbkpt+numbkp),tstop)
c
c  print option summary
c
 1600 if (iprnto.eq.0) go to 1700
      call title(0,lwidth,1,titlop)
      write (6,1601) gmin,reltol,abstol,vntol,lvlcod,itl1,itl2
 1601 format('0dc analysis -',/,
     1   '0    gmin   = ',1pd10.3,/,
     2   '     reltol = ',  d10.3,/,
     3   '     abstol = ',  d10.3,/,
     4   '     vntol  = ',  d10.3,/,
     5   '     lvlcod = ',     i6,/,
     6   '     itl1   = ',     i6,/,
     7   '     itl2   = ',     i6,/)
      write (6,1611) aname(method),maxord,chgtol,trtol,lvltim,xmu,
     1   itl3,itl4,itl5
 1611 format('0transient analysis -',/,
     1   '0    method =  ',a8,/,
     2   '     maxord = ',     i6,/,
     3   '     chgtol = ',1pd10.3,/,
     4   '     trtol  = ',  d10.3,/,
     5   '     lvltim = ',     i6,/,
     6   '     mu     = ',0pf10.3,/,
     7   '     itl3   = ',     i6,/,
     8   '     itl4   = ',     i6,/,
     9   '     itl5   = ',     i6,/)
      write (6,1621) limpts,limtim,maxtim,numdgt,value(itemps+1),
     1   defl,defw,defad,defas
 1621 format('0miscellaneous -',/,
     1   '0    limpts = ',     i6,/,
     2   '     limtim = ',     i6,/,
     3   '     cptime = ',     i6,/,
     4   '     numdgt = ',     i6,/,
     5   '     tnom   = ',0pf10.3,/,
     6   '     defl   = ',1pe10.3,/,
     7   '     defw   = ',e10.3,/,
     8   '     defad  = ',e10.3,/,
     9   '     defas  = ',e10.3)
c
c  miscellaneous error checking
c
 1700 if (icvflg.eq.0) go to 1720
      if (icvflg.le.limpts) go to 1710
      icvflg=0
      write (6,1701) limpts,alsdc
 1701 format('0warning:  more than ',i5,' points for ',a4,' analysis,',/
     11x,'analysis omitted.  this limit may be overridden using the ',/
     21x,'limpts parameter on the .option card'/)
      go to 1720
 1710 if ((jelcnt(31)+jelcnt(36)).gt.0) go to 1720
      if(ipostp.ne.0) go to 1720
      icvflg=0
      write (6,1711) alsdc
 1711 format('0warning:  no ',a4,' outputs specified .',
     1  '.. analysis omitted'/)
 1720 if (jtrflg.eq.0) go to 1740
      if (method.eq.1) maxord=2
      if ((method.eq.2).and.(maxord.ge.3)) lvltim=2
      if (jtrflg.le.limpts) go to 1730
      jtrflg=0
      write (6,1701) limpts,alstr
      go to 1740
 1730 if ((jelcnt(32)+jelcnt(37)+nfour).gt.0) go to 1735
      if(ipostp.ne.0) go to 1735
      jtrflg=0
      write (6,1711) alstr
      go to 1740
 1735 if (nfour.eq.0) go to 1740
      forprd=1.0d0/forfre
      if ((tstop-forprd).ge.(tstart-1.0d-12)) go to 1740
      nfour=0
      call clrmem(ifour)
      write (6,1736)
 1736 format('0warning:  fourier analysis fundamental frequency is incom
     1patible with'/11x'transient analysis print interval ... fourier an
     2alysis omitted'/)
 1740 if (jacflg.eq.0) go to 1800
      if (jacflg.le.limpts) go to 1750
      jacflg=0
      write (6,1701) limpts,alsac
      go to 1800
 1750 if ((jelcnt(33)+jelcnt(34)+jelcnt(35)+jelcnt(38)+jelcnt(39)
     1   +jelcnt(40)+idist+inoise).gt.0) go to 1800
      if(ipostp.ne.0) go to 1800
      jacflg=0
      write (6,1711) alsac
c
c  sequence through the output lists
c
 1800 do 1820 id=41,45
      if (id.le.43) numout=1
      loc=locate(id)
 1810 if (loc.eq.0) go to 1820
      numout=numout+1
      nodplc(loc+4)=numout
      loc=nodplc(loc)
      go to 1810
 1820 continue
c
c   increase number of .prints if too many outputs for output line-width
c
      ifwdth=max0(numdgt-1,0)+9
      noprln=min0(8,(lwidth-12)/ifwdth)
      do 1860 id=31,35
      loc=locate(id)
 1830 if(loc.eq.0) go to 1860
      noprex=nodplc(loc+3)-noprln
      if(noprex.le.0) go to 1850
      nodplc(loc+3)=noprln
      call find(dfloat(jelcnt(id)),id,locnew,1)
      nodplc(locnew+2)=nodplc(loc+2)
      nodplc(locnew+3)=noprex
      call copy4(nodplc(loc+2*noprln+4),nodplc(locnew+4),2*noprex)
 1850 loc=nodplc(loc)
      go to 1830
 1860 continue
c
c  exit
c
 2000 call second(t2)
      rstats(1)=rstats(1)+t2-t1
      return
      end
      subroutine shlsrt(a,n)
      implicit double precision (a-h,o-z)
c
c     this routine sorts the array a using a shell sort algorithm.
c
      dimension a(n)
      integer h
c
c
c...  compute best starting step size
      h=1
   10 h=3*h+1
      if (h.lt.n) go to 10
c...  back off two times
      h=(h-1)/3
      h=(h-1)/3
      h=max0(h,1)
c
c  shell sort
c
   20 j=h+1
      go to 60
   30 i=j-h
c...  ak = record key;  ar = record
      ak=a(j)
      ar=ak
   40 if (ak.ge.a(i)) go to 50
      a(i+h)=a(i)
      i=i-h
      if (i.ge.1) go to 40
   50 a(i+h)=ar
      j=j+1
   60 if (j.le.n) go to 30
      h=(h-1)/3
      if (h.ne.0) go to 20
      return
      end
      subroutine putnod(node)
      implicit double precision (a-h,o-z)
c
c     this routine adds 'node' to the list of user input nodes in table
c junode.
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
c
      jknt=0
   10 jknt=jknt+1
      if (jknt.gt.nunods) go to 20
      if (node-nodplc(junode+jknt)) 20,100,10
   20 k=nunods+1
      call extmem(junode,1)
      if (k.le.jknt) go to 30
      call copy4(nodplc(junode+jknt),nodplc(junode+jknt+1),k-jknt)
      k=jknt
   30 nodplc(junode+k)=node
      nunods=nunods+1
c
c  finished
c
  100 return
      end
      subroutine getnod(node)
      implicit double precision (a-h,o-z)
c
c     this routine converts from the user node number to the internal
c (compact) node number.
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
c
      if (nogo.ne.0) go to 100
      jknt=0
   10 jknt=jknt+1
      if (jknt.gt.nunods) go to 20
      if (nodplc(junode+jknt).ne.node) go to 10
      node=jknt
      go to 100
c
c  unknown node -- must be implied by .print and/or .plot
c
   20 if (node.eq.0) go to 30
      write (6,21) node
   21 format('0warning:  attempt to reference undefined node ',i5,
     1   ' -- node reset to 0'/)
   30 node=1
c
c  finished
c
  100 return
      end
      subroutine subckt
      implicit double precision (a-h,o-z)
c
c     this routine drives the expansion of subcircuit calls.
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
c
c... avoid 'call by value' problems, make inodi, inodx arrays
c... in routines which receive them as parameters !!!
      locx=locate(19)
   10 if (locx.eq.0) go to 300
      locs=nodplc(locx+3)
      asnam=value(iunsat+locs)
      call fndnam(asnam,locx-1,locx+3,20)
      if (nogo.ne.0) go to 300
      locs=nodplc(locx+3)
c
c  check for recursion
c
      isbptr=nodplc(locx-1)
   20 if (isbptr.eq.0) go to 30
      if (locs.eq.nodplc(isbptr+3)) go to 260
      isbptr=nodplc(isbptr-1)
      go to 20
c
c
   30 call sizmem(nodplc(locx+2),nxnod)
      call sizmem(nodplc(locs+2),nssnod)
      if (nxnod.ne.nssnod) go to 250
      call getm4(inodx,nssnod)
      call getm4(inodi,nssnod)
      itemp=nodplc(locs+2)
      call copy4(nodplc(itemp+1),nodplc(inodx+1),nssnod)
      itemp=nodplc(locx+2)
      call copy4(nodplc(itemp+1),nodplc(inodi+1),nxnod)
c
c  add elements of subcircuit to nominal circuit
c
      loc=nodplc(locs+3)
  100 if (loc.eq.0) go to 200
      id=nodplc(loc-1)
      if (id.eq.20) go to 110
      call find(dfloat(jelcnt(id)),id,loce,1)
      nodplc(loce-1)=locx
      call addelt(loce,loc,id,inodx,inodi,nxnod)
  110 loc=nodplc(loc)
      go to 100
c
c
  200 call clrmem(inodx)
      call clrmem(inodi)
      locx=nodplc(locx)
      go to 10
c
c  errors
c
  250 locv=nodplc(locx+1)
      axnam=value(locv)
      locv=nodplc(locs+1)
      asnam=value(locv)
      write (6,251) axnam,asnam
  251 format('0*error*:  ',a8,' has different number of nodes than ',a8/
     1)
      nogo=1
      go to 300
  260 locsv=nodplc(locs+1)
      asnam=value(locsv)
      write (6,261) asnam
  261 format('0*error*:  subcircuit ',a8,' is defined recursively'/)
      nogo=1
c
c  finished
c
  300 return
      end
      subroutine fndnam(anam,jsbptr,ispot,id)
      implicit double precision (a-h,o-z)
c
c     this routine searches for an element with id 'id' by tracing back
c up the subcircuit definition list.  if the element is not found, the
c nominal element list is searched.
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
      integer xxor
c
c
      isbptr=nodplc(jsbptr)
   10 if (isbptr.eq.0) go to 50
      isub=nodplc(isbptr+3)
      loc=nodplc(isub+3)
   20 if (loc.eq.0) go to 40
      if (id.ne.nodplc(loc-1)) go to 30
      locv=nodplc(loc+1)
      if (xxor(anam,value(locv)).ne.0) go to 30
      if (id.ne.20) go to 50
      go to 65
   30 loc=nodplc(loc)
      go to 20
   40 isbptr=nodplc(isbptr-1)
      go to 10
c
   50 loc=locate(id)
   60 if (loc.eq.0) go to 90
      if (nodplc(loc-1).ne.isbptr) go to 70
      locv=nodplc(loc+1)
      if (xxor(anam,value(locv)).ne.0) go to 70
   65 nodplc(ispot)=loc
      go to 100
   70 loc=nodplc(loc)
      go to 60
   90 write (6,91) anam
   91 format('0*error*:  unable to find ',a8/)
      nogo=1
  100 return
      end
      subroutine newnod(nodold,nodnew,inodx,inodi,nnodi)
      implicit double precision (a-h,o-z)
c
c     this routine makes a new node number for an element which is about
c to be added to the circuit as a result of a subcircuit call.
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
c... inodx, inodi are arrays (see subckt)
      dimension inodx(1),inodi(1)
c
      if (nodold.ne.0) go to 5
      nodnew=1
      go to 20
    5 do 10 i=1,nnodi
      jnodx=inodx(1)
      if (nodold.ne.nodplc(jnodx+i)) go to 10
      jnodi=inodi(1)
      nodnew=nodplc(jnodi+i)
      go to 20
   10 continue
c
      call extmem(inodx(1),1)
      call extmem(inodi(1),1)
      call extmem(junode,1)
      nnodi=nnodi+1
      ncnods=ncnods+1
      jnodx=inodx(1)
      nodplc(jnodx+nnodi)=nodold
      jnodi=inodi(1)
      nodplc(jnodi+nnodi)=ncnods
      nodplc(junode+ncnods)=nodplc(junode+ncnods-1)+1
      nodnew=ncnods
   20 return
      end
      subroutine addelt(loce,loc,id,inodx,inodi,nnodi)
      implicit double precision (a-h,o-z)
c
c     this routine adds an element to the nominal circuit definition
c lists.
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
c... inodx(1), inodi(1) are arrays (see subckt)
      dimension inodx(1),inodi(1)
c
      dimension lnod(50),lval(50),nnods(50)
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
      data nnods / 2, 2, 2, 0, 2, 2, 2, 2, 2, 2,
     1             2, 4, 3, 4, 4, 4, 4, 0, 1, 0,
     2             0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     3             0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     4             2, 2, 2, 0, 0, 0, 0, 0, 0, 0 /
c
c  copy integer part
c
      nword=lnod(id)-3
      if (nword.le.0) go to 10
      call copy4(nodplc(loc+2),nodplc(loce+2),nword)
c
c  set nodes
c
   10 if (id.ge.21) go to 100
      if (nnods(id).eq.0) go to 100
      if (id.le.4) go to 20
      if (id.le.8) go to 40
      if (id.eq.19) go to 70
   20 jstop=nnods(id)
      do 30 j=1,jstop
      call newnod(nodplc(loc+j+1),nodplc(loce+j+1),inodx(1),
     1  inodi(1),nnodi)
   30 continue
      go to 100
   40 call newnod(nodplc(loc+2),nodplc(loce+2),inodx(1),inodi(1),nnodi)
      call newnod(nodplc(loc+3),nodplc(loce+3),inodx(1),inodi(1),nnodi)
      if (id.ge.7) go to 100
      nlocp=loc+id+1
      nssnod=2*nodplc(loc+4)
      call getm4(nodplc(loce+id+1),nssnod)
      nlocpe=loce+id+1
   50 do 60 j=1,nssnod
      locp=nodplc(nlocp)
      nodold=nodplc(locp+j)
      call newnod(nodold,nodnew,inodx(1),inodi(1),nnodi)
      locpe=nodplc(nlocpe)
      nodplc(locpe+j)=nodnew
   60 continue
      go to 100
   70 nlocp=loc+2
      call sizmem(nodplc(loc+2),nssnod)
      call getm4(nodplc(loce+2),nssnod)
      nlocpe=loce+2
      go to 50
c
c  copy real part
c
  100 if (nogo.ne.0) go to 300
      locv=nodplc(loc+1)
      locve=nodplc(loce+1)
      call copy8(value(locv),value(locve),lval(id))
c
c  treat non-node tables specially
c
  200 if (id.ge.11) go to 300
      go to (300,210,220,300,230,240,230,240,260,260), id
  210 call cpytb8(loc+7,loce+7)
      go to 300
  220 call cpytb8(loc+10,loce+10)
      go to 300
  230 itab=5
      go to 250
  240 itab=6
  250 if (id.le.6) go to 255
      call cpytb4(loc+itab+1,loce+itab+1)
  255 call cpytb4(loc+itab+2,loce+itab+2)
      call cpytb8(loc+itab+3,loce+itab+3)
      call cpytb8(loc+itab+4,loce+itab+4)
      call cpytb4(loc+itab+5,loce+itab+5)
      call cpytb8(loc+itab+6,loce+itab+6)
      go to 300
  260 call cpytb8(loc+5,loce+5)
c
c
  300 return
      end
      subroutine cpytb4(itabo,itabn)
      implicit double precision (a-h,o-z)
c
c     this routine copies a table.  its use is made necessary by the
c fact that only one pointer is allowed per table.
c
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      call sizmem(nodplc(itabo),isize)
      call getm4(nodplc(itabn),isize)
      loco=nodplc(itabo)
      locn=nodplc(itabn)
      call copy4(nodplc(loco+1),nodplc(locn+1),isize)
      return
      end
      subroutine cpytb8(itabo,itabn)
      implicit double precision (a-h,o-z)
c
c     this routine copies a table.  its use is made necessary by the
c fact that only one pointer is allowed per table.
c
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      call sizmem(nodplc(itabo),isize)
      call getm8(nodplc(itabn),isize)
      loco=nodplc(itabo)
      locn=nodplc(itabn)
      call copy8(value(loco+1),value(locn+1),isize)
      return
      end
      subroutine lnkref
      implicit double precision (a-h,o-z)
c
c     this routine resolves all unsatisfied name references.
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
c  mutual inductors
c
      loc=locate(4)
  100 if (loc.eq.0) go to 200
      iref=nodplc(loc+2)
      call fndnam(value(iunsat+iref),loc-1,loc+2,3)
      iref=nodplc(loc+3)
      call fndnam(value(iunsat+iref),loc-1,loc+3,3)
      loc=nodplc(loc)
      go to 100
c
c  current-controlled current source
c
  200 loc=locate(7)
  210 if (loc.eq.0) go to 300
      nump=nodplc(loc+4)
      locp=nodplc(loc+6)
      do 220 i=1,nump
      iref=nodplc(locp+i)
      call fndnam(value(iunsat+iref),loc-1,locp+i,9)
  220 continue
      loc=nodplc(loc)
      go to 210
c
c  current-controlled voltage sources
c
  300 loc=locate(8)
  310 if (loc.eq.0) go to 400
      nump=nodplc(loc+4)
      locp=nodplc(loc+7)
      do 320 i=1,nump
      iref=nodplc(locp+i)
      call fndnam(value(iunsat+iref),loc-1,locp+i,9)
  320 continue
      loc=nodplc(loc)
      go to 310
c
c  diodes
c
  400 loc=locate(11)
  410 if (loc.eq.0) go to 500
      iref=nodplc(loc+5)
      call fndnam(value(iunsat+iref),loc-1,loc+5,21)
      loc=nodplc(loc)
      go to 410
c
c  bjts
c
  500 loc=locate(12)
  510 if (loc.eq.0) go to 600
      iref=nodplc(loc+8)
      call fndnam(value(iunsat+iref),loc-1,loc+8,22)
      loc=nodplc(loc)
      go to 510
c
c  jfets
c
  600 loc=locate(13)
  610 if (loc.eq.0) go to 700
      iref=nodplc(loc+7)
      call fndnam(value(iunsat+iref),loc-1,loc+7,23)
      loc=nodplc(loc)
      go to 610
c
c  mosfets
c
  700 loc=locate(14)
  710 if (loc.eq.0) go to 1000
      iref=nodplc(loc+8)
      call fndnam(value(iunsat+iref),loc-1,loc+8,24)
      loc=nodplc(loc)
      go to 710
c
c  finished
c
 1000 call clrmem(iunsat)
      return
      end
      subroutine subnam(loce)
      implicit double precision (a-h,o-z)
c
c     this routine constructs the names of elements added as a result of
c subcircuit expansion.  the full element names are of the form
c                  name.xn. --- xd.xc.xb.xa
c where 'name' is the nominal element name, and the 'x'*s denote the
c sequence of subcircuit calls (from top or circuit level down through
c nested subcircuit calls) which caused the particular element to be
c added.  at present, spice restricts all element names to be 8 charac-
c ters or less.  therefore, the name used consists of the leftmost 8
c characters of the full element name, with the rightmost character
c replaced by an asterisk ('*') if the full element name is longer than
c 8 characters.
c
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      data ablank, aper, astk / 1h , 1h., 1h* /
c
c  construct subcircuit element name
c
      if (nodplc(loce-1).eq.0) go to 100
      locve=nodplc(loce+1)
      loc=loce
      nchar=0
      sname=ablank
      achar=ablank
   10 locv=nodplc(loc+1)
      elname=value(locv)
      do 20 ichar=1,8
      call move(achar,1,elname,ichar,1)
      if (achar.eq.ablank) go to 30
      if (nchar.eq.8) go to 40
      nchar=nchar+1
      call move(sname,nchar,achar,1,1)
   20 continue
   30 loc=nodplc(loc-1)
      if (loc.eq.0) go to 60
      if (nchar.eq.8) go to 40
      nchar=nchar+1
      call move(sname,nchar,aper,1,1)
      go to 10
c
c  name is longer than 8 characters:  flag with asterisk
c
   40 call move(sname,8,astk,1,1)
   60 value(locve)=sname
c
c  finished
c
  100 return
      end
      subroutine elprnt
      implicit double precision (a-h,o-z)
c
c     this routine prints a circuit element summary.
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
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /tran/ tstep,tstop,tstart,delmax,tdmax,forfre,jtrflg
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension itab(25),astyp(6)
      dimension eltitl(4)
      data eltitl / 8hcircuit , 8helement , 8hsummary , 8h         /
      data astyp / 1h , 5hpulse, 3hsin, 3hexp, 3hpwl, 4hsffm /
      data ablnk,aoff /1h ,3hoff/
c
c  print listing of elements
c
      call title(0,lwidth,1,eltitl)
c
c  print resistors
c
      if (jelcnt(1).eq.0) go to 50
      ititle=0
   21 format(//'0**** resistors'/'0     name        nodes     value
     1  tc1        tc2'//)
      loc=locate(1)
   30 if (loc.eq.0) go to 50
      if (ititle.eq.0) write (6,21)
      ititle=1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      write (6,31) value(locv),nodplc(junode+node1),
     1   nodplc(junode+node2),value(locv+2),value(locv+3),value(locv+4)
   31 format(6x,a8,2i5,1p3d11.2)
   40 loc=nodplc(loc)
      go to 30
c
c  print capacitors and inductors
c
   50 if ((jelcnt(2)+jelcnt(3)).eq.0) go to 80
      ititle=0
   51 format(//'0**** capacitors and inductors'/'0     name        nodes
     1    in cond     value'//)
      do 70 id=2,3
      loc=locate(id)
   60 if (loc.eq.0) go to 70
      if (ititle.eq.0) write (6,51)
      ititle=1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ltab=7
      if (id.eq.3) ltab=10
      call sizmem(nodplc(loc+ltab),nparam)
      if (nparam.ge.2) go to 62
      ispot=nodplc(loc+ltab)+1
      write (6,31) value(locv),nodplc(junode+node1),
     1   nodplc(junode+node2),value(locv+2),value(ispot)
      go to 65
   62 write (6,63) value(locv),nodplc(junode+node1),
     1   nodplc(junode+node2),value(locv+2)
   63 format(6x,a8,2i5,1pd11.2,'   variable')
   65 loc=nodplc(loc)
      go to 60
   70 continue
c
c  print mutual inductors
c
   80 if (jelcnt(4).eq.0) go to 100
      ititle=0
   81 format(//'0**** mutual inductors'/'0     name        coupled induc
     1tors   value'//)
      loc=locate(4)
   90 if (loc.eq.0) go to 110
      if (ititle.eq.0) write (6,81)
      ititle=1
      locv=nodplc(loc+1)
      nl1=nodplc(loc+2)
      nl1=nodplc(nl1+1)
      nl2=nodplc(loc+3)
      nl2=nodplc(nl2+1)
      write (6,91) value(locv),value(nl1),value(nl2),value(locv+1)
   91 format(6x,a8,4x,a8,2x,a8,1pd10.2)
   95 loc=nodplc(loc)
      go to 90
c
c  print nonlinear voltage controlled sources
c
  100 if (jelcnt(5).eq.0) go to 120
      ititle=0
  101 format(//'0**** voltage-controlled current sources'/'0     name
     1     +    -   dimension   function')
      loc=locate(5)
  110 if (loc.eq.0) go to 120
      if (ititle.eq.0) write (6,101)
      ititle=1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      write (6,111) value(locv),nodplc(junode+node1),
     1   nodplc(junode+node2),nodplc(loc+4)
  111 format(6x,a8,2i5,i8,9x,'poly')
  115 loc=nodplc(loc)
      go to 110
c
c  nonlinear voltage controlled voltage sources
c
  120 if (jelcnt(6).eq.0) go to 140
      ititle=0
  121 format(//'0**** voltage-controlled voltage sources'/'0     name
     1     +    -   dimension   function')
      loc=locate(6)
  130 if (loc.eq.0) go to 140
      if (ititle.eq.0) write (6,121)
      ititle=1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      write (6,111) value(locv),nodplc(junode+node1),
     1   nodplc(junode+node2),nodplc(loc+4)
  135 loc=nodplc(loc)
      go to 130
c
c  nonlinear current controlled current sources
c
  140 if (jelcnt(7).eq.0) go to 160
      ititle=0
  141 format(//'0**** current-controlled current sources'/'0     name
     1     +    -   dimension   function')
      loc=locate(7)
  150 if (loc.eq.0) go to 160
      if (ititle.eq.0) write (6,141)
      ititle=1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      write (6,111) value(locv),nodplc(junode+node1),
     1   nodplc(junode+node2),nodplc(loc+4)
  155 loc=nodplc(loc)
      go to 150
c
c  nonlinear current controlled voltage sources
c
  160 if (jelcnt(8).eq.0) go to 170
      ititle=0
  161 format(//'0**** current-controlled voltage sources'/'0     name
     1     +    -   dimension   function')
      loc=locate(8)
  165 if (loc.eq.0) go to 170
      if (ititle.eq.0) write (6,161)
      ititle=1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      write (6,111) value(locv),nodplc(junode+node1),
     1   nodplc(junode+node2),nodplc(loc+4)
  167 loc=nodplc(loc)
      go to 165
c
c  print independent sources
c
  170 if ((jelcnt(9)+jelcnt(10)).eq.0) go to 250
      ititle=0
  171 format(//'0**** independent sources'/'0     name        nodes   dc
     1 value   ac value   ac phase   transient'//)
      do 245 id=9,10
      loc=locate(id)
  180 if (loc.eq.0) go to 245
      if (ititle.eq.0) write (6,171)
      ititle=1
      locv=nodplc(loc+1)
      locp=nodplc(loc+5)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      itype=nodplc(loc+4)+1
      anam=astyp(itype)
      write (6,181) value(locv),nodplc(junode+node1),
     1   nodplc(junode+node2),value(locv+1),value(locv+2),
     2   value(locv+3),anam
  181 format(6x,a8,2i5,1p3d11.2,2x,a8)
      if (jtrflg.eq.0) go to 240
      jstart=locp+1
      go to (240,190,200,210,220,230), itype
  190 jstop=locp+7
      write (6,191) (value(j),j=jstart,jstop)
  191 format(1h0,42x,'initial value',1pd11.2,/,
     1           43x,'pulsed value.',  d11.2,/,
     2           43x,'delay time...',  d11.2,/,
     3           43x,'risetime.....',  d11.2,/,
     4           43x,'falltime.....',  d11.2,/,
     5           43x,'width........',  d11.2,/,
     6           43x,'period.......',  d11.2,/)
      go to 240
  200 jstop=locp+5
      write (6,201) (value(j),j=jstart,jstop)
  201 format(1h0,42x,'offset.......',1pd11.2,/,
     1           43x,'amplitude....',  d11.2,/,
     2           43x,'frequency....',  d11.2,/,
     3           43x,'delay........',  d11.2,/,
     4           43x,'theta........',  d11.2,/)
      go to 240
  210 jstop=locp+6
      write (6,211) (value(j),j=jstart,jstop)
  211 format(1h0,42x,'initial value',1pd11.2,/,
     1           43x,'pulsed value.',  d11.2,/,
     2           43x,'rise delay...',  d11.2,/,
     3           43x,'rise tau.....',  d11.2,/,
     4           43x,'fall delay...',  d11.2,/,
     5           43x,'fall tau.....',  d11.2,/)
      go to 240
  220 call sizmem(nodplc(loc+5),jstop)
      jstop=locp+jstop
      write (6,221) (value(j),j=jstart,jstop)
  221 format(1h0,49x,'time       value'//,(46x,1p2d11.2))
      write (6,226)
  226 format(1x)
      go to 240
  230 jstop=locp+5
      write (6,231) (value(j),j=jstart,jstop)
  231 format(1h0,42x,'offset.......',1pd11.2,/,
     1           43x,'amplitude....',  d11.2,/,
     2           43x,'carrier freq.',  d11.2,/,
     3           43x,'modn index...',  d11.2,/,
     4           43x,'signal freq..',  d11.2,/)
  240 loc=nodplc(loc)
      go to 180
  245 continue
c
c  print transmission lines
c
  250 if (jelcnt(17).eq.0) go to 260
      ititle=0
  251 format(//'0**** transmission lines'/'0     name             nodes
     1           z0         td'//)
      loc=locate(17)
  253 if (loc.eq.0) go to 260
      if (ititle.eq.0) write (6,251)
      ititle=1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      write (6,256) value(locv),nodplc(junode+node1),
     1   nodplc(junode+node2),nodplc(junode+node3),
     2   nodplc(junode+node4),value(locv+1),value(locv+2)
  256 format(6x,a8,4i5,1p2d11.2)
  258 loc=nodplc(loc)
      go to 253
c
c  print diodes
c
  260 if (jelcnt(11).eq.0) go to 290
      ititle=0
  261 format(//'0**** diodes'/'0     name        +    -  model       are
     1a'//)
      loc=locate(11)
  270 if (loc.eq.0) go to 290
      if (ititle.eq.0) write (6,261)
      ititle=1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      locm=nodplc(loc+5)
      locm=nodplc(locm+1)
      aic=ablnk
      if (nodplc(loc+6).eq.1) aic=aoff
      write (6,271) value(locv),nodplc(junode+node1),
     1   nodplc(junode+node2),value(locm),value(locv+1),aic
  271 format(6x,a8,2i5,2x,a8,f8.3,2x,a8)
  280 loc=nodplc(loc)
      go to 270
c
c  print transistors
c
  290 if (jelcnt(12).eq.0) go to 320
      ititle=0
  291 format(//'0**** bipolar junction transistors'/'0     name        c
     1    b    e    s  model       area'//)
      loc=locate(12)
  300 if (loc.eq.0) go to 320
      if (ititle.eq.0) write (6,291)
      ititle=1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      locm=nodplc(loc+8)
      locm=nodplc(locm+1)
      aic=ablnk
      if (nodplc(loc+9).eq.1) aic=aoff
      write (6,301) value(locv),nodplc(junode+node1),
     1   nodplc(junode+node2),nodplc(junode+node3),nodplc(junode+node4),
     2   value(locm),value(locv+1),aic
  301 format(6x,a8,4i5,2x,a8,f8.3,2x,a8)
  310 loc=nodplc(loc)
      go to 300
c
c  print jfets
c
  320 if (jelcnt(13).eq.0) go to 350
      ititle=0
  321 format(//'0**** jfets'/'0     name        d    g    s  model
     1 area'//)
      loc=locate(13)
  330 if (loc.eq.0) go to 350
      if (ititle.eq.0) write (6,321)
      ititle=1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      locm=nodplc(loc+7)
      locm=nodplc(locm+1)
      aic=ablnk
      if (nodplc(loc+8).eq.1) aic=aoff
      write (6,331) value(locv),nodplc(junode+node1),
     1   nodplc(junode+node2),nodplc(junode+node3),
     2   value(locm),value(locv+1),aic
  331 format(6x,a8,3i5,2x,a8,f8.3,2x,a8)
  340 loc=nodplc(loc)
      go to 330
c
c  print mosfets
c
  350 if (jelcnt(14).eq.0) go to 400
      ititle=0
  351 format(//'0**** mosfets',/,'0name      d   g   s   b  model      l
     1       w       ad      as      rd      rs',//)
      loc=locate(14)
  360 if (loc.eq.0) go to 400
      if (ititle.eq.0) write (6,351)
      ititle=1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      locm=nodplc(loc+8)
      locm=nodplc(locm+1)
      rd=value(locv+11)
      if(rd.eq.0.0d0) rd=value(locm+6)
      rs=value(locv+12)
      if(rs.eq.0.0d0) rs=value(locm+7)
      aic=ablnk
      if (nodplc(loc+9).eq.1) aic=aoff
      write (6,361) value(locv),nodplc(junode+node1),
     1   nodplc(junode+node2),nodplc(junode+node3),
     2   nodplc(junode+node4),value(locm),value(locv+1),value(locv+2),
     3   value(locv+3),value(locv+4),rd,rs
  361 format(1x,a8,4i4,1x,a8,1pd7.1,5d8.1)
      if(aic.ne.ablnk) write(6,362)
  362 format(1x,'above device specified to be *off* to aid dc solution',
     1  /)
  370 loc=nodplc(loc)
      go to 360
c
c  subcircuit calls
c
  400 if (jelcnt(19).eq.0) go to 500
      ititle=0
  401 format(//'0**** subcircuit calls'/'0     name     subcircuit   ext
     1ernal nodes'//)
      loc=locate(19)
  410 if (loc.eq.0) go to 500
      if (ititle.eq.0) write (6,401)
      ititle=1
      locv=nodplc(loc+1)
      locn=nodplc(loc+2)
      call sizmem(nodplc(loc+2),nnodx)
      locs=nodplc(loc+3)
      locsv=nodplc(locs+1)
      jstart=1
      ndprln=(lwidth-28)/5
  412 jstop=min0(nnodx,jstart+ndprln-1)
      do 414 j=jstart,jstop
      node=nodplc(locn+j)
      itab(j-jstart+1)=nodplc(junode+node)
  414 continue
      if (jstart.eq.1)
     1   write (6,416) value(locv),value(locsv),(itab(j),j=1,jstop)
  416 format(6x,a8,2x,a8,4x,20i5)
      if (jstart.ne.1)
     1   write (6,418) (itab(j-jstart+1),j=jstart,jstop)
  418 format(28x,20i5)
      jstart=jstop+1
      if (jstart.le.nnodx) go to 412
      if (nnodx.le.ndprln) go to 420
      write (6,226)
  420 loc=nodplc(loc)
      go to 410
c
c  finished
c
  500 return
      end
      subroutine modchk
      implicit double precision (a-h,o-z)
c
c     this routine performs one-time processing of device model para-
c meters and prints out a device model summary.  it also reserves the
c additional nodes required by nonzero device extrinsic resistances.
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
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension itab(50),atable(10)
      dimension ifun(4)
      dimension antype(4),aptype(4)
      dimension ipar(6),ampar(120),defval(120),ifmt(120)
      dimension titled(4),titleb(4),titlej(4),titlem(4)
      data titled / 8hdiode mo, 8hdel para, 8hmeters  , 8h         /
      data titleb / 8hbjt mode, 8hl parame, 8hters    , 8h         /
      data titlej / 8hjfet mod, 8hel param, 8heters   , 8h         /
      data titlem / 8hmosfet m, 8hodel par, 8hameters , 8h         /
      data ifun / 0, 0, 1, 1 /
      data antype /1h ,3hnpn,3hnjf,4hnmos/
      data aptype /1h ,3hpnp,3hpjf,4hpmos/
      data agaas /5hga-as/
      data ipar / 0, 14, 60, 72, 106, 119 /
      data hndrd,hndrd2 /1.0d+02,1.0d+04/
      data ampar /
     1   6his    ,6hrs    ,6hn     ,6htt    ,6hcjo   ,6hpb    ,6hm     ,
     2   6heg    ,6hpt    ,6hkf    ,6haf    ,6hfc    ,6hbv    ,6hibv   ,
     1   6hjs    ,6hbf    ,6hnf    ,6hvbf   ,6hjbf   ,6hjle   ,6hnle   ,
     2   6hbr    ,6hnr    ,6hvbr   ,6hjbr   ,6hjlc   ,6hnlc   ,6h0     ,
     3   6h0     ,6hrb    ,6hjrb   ,6hrbm   ,6hre    ,6hrc    ,6hcje   ,
     4   6hvje   ,6hmje   ,6htf    ,6hxtf   ,6hvtf   ,6hjtf   ,6hptf   ,
     5   6hcjc   ,6hvjc   ,6hmjc   ,6hcdis  ,6htr    ,6h0     ,6h0     ,
     6   6h0     ,6h0     ,6hcjs   ,6hvjs   ,6hmjs   ,6htb    ,6heg    ,
     7   6hpt    ,6hkf    ,6haf    ,6hfc    ,
     1   6hvto   ,6hbeta  ,6hlambda,6hrd    ,6hrs    ,6hcgs   ,6hcgd   ,
     2   6hpb    ,6his    ,6hkf    ,6haf    ,6hfc    ,
     1   6hvto   ,6hkp    ,6hgamma ,6hphi   ,6hlambda,6hrd    ,6hrs    ,
     2   6hcgs   ,6hcgd   ,6hcgb   ,6hcbd   ,6hcbs   ,6htox   ,6hpb    ,
     3   6hjs    ,6hnsub  ,6hnss   ,6hnfs   ,6hxj    ,6hld    ,6hngate ,
     4   6htps   ,6huo    ,6hucrit ,6huexp  ,6hutra  ,6hkf    ,6haf    ,
     5   6hfc    ,6hwd    ,6hecrit ,6hetra  ,6hvnorm ,6hdesat ,
     1   6hvp    ,6hvbr   ,6hvbi   ,6hvfwd  ,6hnd    ,6hkdso  ,6hkdv   ,
     2   6hcdso  ,6hczg   ,6hgnoise,6hnexp  ,6hkf    ,6haf    ,0.0d0 /
      data defval /
     1   1.0d-14, 0.0d0, 1.0d0, 2*0.0d0, 1.0d0, 0.5d0, 1.11d0,
     2       3.0d0, 0.0d0, 1.0d0, 0.5d0, 0.0d0, 1.0d-3,
     1   1.0d-16, 100.0d0, 1.0d0, 3*0.0d0, 1.5d0, 2*1.0d0, 3*0.0d0,
     2       2.0d0, 0.0d0, 1.0d0, 0.0d0,  0.0d0, 4*0.0d0, 0.75d0,
     3    0.33d0, 2*0.0d0, 2*0.0d0, 2*0.0d0, 0.75d0, 0.33d0, 1.0d0,
     4     2*0.0d0, 2*0.0d0, 2*0.0d0, 0.75d0, 0.0d0, 0.0d0, 1.11d0,
     5       3.0d0, 0.0d0, 1.0d0, 0.5d0,
     1      -2.0d0, 1.0d-4, 5*0.0d0, 1.0d0,1.0d-14, 0.0d0, 1.0d0,
     2       0.5d0,
     1     3*0.0d0, 0.0d0, 8*0.0d0, 1.0d-7,  0.8d0, 1.0d-4,6*0.0d0,
     2       1.0d0,700.0d0, 1.0d+4, 3*0.0d0, 1.0d0, 0.5d0, 0.0d0,
     3     3*0.0d0,1.5d+9,
     1   -2.1d0,   0.0d0, 0.8d0, 0.6d0, 1.0d17, 5.8d0  ,0.01d0,
     2    2.0d-10, 0.0d0, 0.0d0, 1.0d0, 0.0d0, 1.0d0, 0.0d0 /
      data ifmt /
     1   2,1,1,2,2,1,1,1,1,2,1,1,2,2,
     1   2,1,1,2,2,2,1,1,1,2,2,2,1,0,0,1,2,1,1,1,2,1,1,2,2,2,2,1,2,1,
     a   1,1,2,0,0,0,0,2,1,1,2,1,1,2,2,2,
     3   1,2,1,1,1,2,2,1,2,2,1,1,
     4   1,2,1,1,2,1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,2,1,1,2,1,1,2,1,1,2,
     a   2,2,2,2,
     5   1,1,1,1,2,2,2,2,2,2,1,2,1,0/
c
c
      tnom=value(itemps+1)+ctok
      xkt=boltz*tnom
      vt=xkt/charge
      egfet=1.16d0-(7.02d-04*tnom**2/(tnom+1108.0d0))
      arg=-egfet/2.0d0/boltz/tnom+1.1151d0/boltz/2.0d0/(27.0d0+ctok)
      xni=1.45d10*(tnom/(27.0d0+ctok))**1.5d0*dexp(charge*arg)
      nummod=jelcnt(21)+jelcnt(22)+jelcnt(23)+jelcnt(24)
      if (nummod.eq.0) go to 1000
c
c  assign default values
c
      kntlim=lwidth/11
      do 390 id=1,4
      if (jelcnt(id+20).eq.0) go to 390
      iflag=ifun(id)
      loc=locate(id+20)
   10 if (loc.eq.0) go to 20
      locv=nodplc(loc+1)
      id1=id
c... special case of gaas
      if(id.eq.4.and.nodplc(loc+2).eq.0) id1=5
      locm=ipar(id1)
      nopar=ipar(id1+1)-locm
      do 18 i=1,nopar
      itab(i)=ifmt(locm+i)
      if (value(locv+i).eq.0.0d0) go to 16
      if (iflag.eq.0) go to 14
      if (i.eq.1) go to 18
      if(i.eq.2.and.id1.eq.5) go to 18
   14 if (value(locv+i).gt.0.0d0) go to 18
c.. let pt be negative for bjts (for now anyway!)
      if(i.eq.43.and.id.eq.2) go to 18
c... nss, ld, wd, utra and tps for mosfet can be negative
      if((i.eq.22.or.i.eq.17.or.i.eq.20.or.i.eq.30.or.i.eq.26)
     1  .and.id.eq.4) go to 18
c... vbr for ga-as fets must be negative
   16 value(locv+i)=defval(locm+i)
   18 continue
      loc=nodplc(loc)
      go to 10
c
c  limit model values
c
   20 go to (30,40,50,60), id
c...  diodes
   30 loc=locate(21)
   32 if (loc.eq.0) go to 100
      locv=nodplc(loc+1)
      value(locv+7)=dmin1(value(locv+7),0.9d0)
      value(locv+8)=dmax1(value(locv+8),0.1d0)
      value(locv+11)=dmax1(value(locv+11),0.1d0)
      value(locv+12)=dmin1(value(locv+12),0.95d0)
      loc=nodplc(loc)
      go to 32
c...  bipolar transistors
   40 loc=locate(22)
   42 if (loc.eq.0) go to 100
      locv=nodplc(loc+1)
      value(locv+23)=dmin1(value(locv+23),0.9d0)
      if(value(locv+24).eq.0.0d0) value(locv+28)=0.0d0
      value(locv+31)=dmin1(value(locv+31),0.9d0)
      value(locv+32)=dmin1(value(locv+32),1.0d0)
      value(locv+40)=dmin1(value(locv+40),0.9d0)
      value(locv+42)=dmax1(value(locv+42),0.1d0)
      value(locv+45)=dmax1(value(locv+45),0.1d0)
      value(locv+46)=dmin1(value(locv+46),0.9999d0)
      loc=nodplc(loc)
      if(value(locv+18).eq.0.0d0) value(locv+18)=value(locv+16)
      if(value(locv+16).ge.value(locv+18)) go to 42
      write(6,44) value(locv)
   44 format('0warning:  minimum base resistance (rbm) is less than '
     1       ,'total (rb) for model ',a8,/10x' rbm set equal to rb',/)
      value(locv+18)=value(locv+16)
      go to 42
c...  jfets
   50 loc=locate(23)
   52 if (loc.eq.0) go to 100
      locv=nodplc(loc+1)
      value(locv+11)=dmax1(value(locv+11),0.1d0)
      value(locv+12)=dmin1(value(locv+12),0.95d0)
      loc=nodplc(loc)
      go to 52
c...  mosfets
   60 loc=locate(24)
   64 if (loc.eq.0) go to 100
      locv=nodplc(loc+1)
      if(nodplc(loc+2).eq.0) go to 70
c
c  special preprocessing for mosfet models
c
      type=nodplc(loc+2)
      cox=epsox/value(locv+13)/hndrd
c... if kp not given, calculate it from cox and uo
      if(value(locv+2).eq.0.0d0)
     1 value(locv+2)=value(locv+23)*cox
      value(locv+35)=0.0d0
c... nsub nonzero => calculate gamma, vto, phi unless specified
      if (value(locv+16).le.0.0d0) go to 68
      xnsub=value(locv+16)
      if (xnsub.le.xni) go to 66
      if (value(locv+4).le.0.0d0) value(locv+4)=2.0d0*vt*dlog(xnsub/xni)
      if (value(locv+3).le.0.0d0)
     1   value(locv+3)=dsqrt(2.0d0*epssil*charge*xnsub)/cox
      fermis=type*0.5d0*value(locv+4)
      wkfng=3.2d0
c... polysilicon gate ... calculate appropriate work function
      if (value(locv+21).le.0.0d0) go to 65
      fermig=type*value(locv+22)*vt*dlog(value(locv+21)/xni)
      wkfng=3.25d0+0.5d0*egfet-fermig
   65 wkfngs=wkfng-(3.25d0+0.5d0*egfet+fermis)
      if(value(locv+1).eq.0.0d0)
     1  value(locv+1)= wkfngs-value(locv+17)*charge/cox+
     2  type*(value(locv+4)+value(locv+3)*dsqrt(value(locv+4)))
      value(locv+35)=dsqrt((epssil+epssil)/(charge*xnsub))
      go to 68
   66 value(locv+16)=0.0d0
      write (6,67) value(locv)
   67 format('0*error*:  nsub < ni in mosfet model ',a8,/)
      nogo=1
c... set phi to default if still zero
   68 if(value(locv+4).eq.0.0d0) value(locv+4)=0.6d0
      value(locv+4)=dmax1(value(locv+4),0.1d0)
      value(locv+28)=dmax1(value(locv+28),0.1d0)
      value(locv+29)=dmin1(value(locv+29),0.95d0)
      loc=nodplc(loc)
      go to 64
c...  ga-as fets
   70 value(locv+1)=-dabs(value(locv+1))
      if(value(locv+2).ne.0.0d0) value(locv+2)=-dabs(value(locv+2))
      value(locv+2)=dmax1(value(locv+2),-200.0d0)
      if(value(locv+9).eq.0.0d0)
     1 value(locv+9)=2.49d-12*dsqrt(value(locv+5)/value(locv+3))
      loc=nodplc(loc)
      go to 64
c
c  print model parameters
c
  100 if (iprntm.eq.0) go to 390
      locs=locate(id+20)
  110 kntr=0
      loc=locs
      go to (120,130,140,150),id
  120 call title(0,lwidth,1,titled)
      go to 200
  130 call title(0,lwidth,1,titleb)
      go to 200
  140 call title(0,lwidth,1,titlej)
      go to 200
  150 call title(0,lwidth,1,titlem)
  200 if (loc.eq.0) go to 210
      if (kntr.lt.kntlim) go to 220
  210 locn=loc
      go to 240
  220 kntr=kntr+1
      locv=nodplc(loc+1)
      atable(kntr)=value(locv)
  230 loc=nodplc(loc)
      go to 200
  240 write (6,241) (atable(k),k=1,kntr)
  241 format(//11x,12(2x,a8))
      if (id.eq.1) go to 300
      kntr=0
      loc=locs
  250 if (loc.eq.0) go to 260
      if (kntr.ge.kntlim) go to 260
      kntr=kntr+1
      atable(kntr)=antype(id)
      if (nodplc(loc+2).eq.-1) atable(kntr)=aptype(id)
c... special type for ga-as (do not mix ga-as and mos!)
      if(id.eq.4.and.nodplc(loc+2).eq.0) atable(kntr)=agaas
      loc=nodplc(loc)
      go to 250
  260 write (6,261) (atable(k),k=1,kntr)
  261 format('0type',4x,12(4x,a6))
  300 do 340 i=1,nopar
      if (itab(i).eq.0) go to 340
      kntr=0
      loc=locs
  310 if (loc.eq.0) go to 320
      if (kntr.ge.kntlim) go to 320
      locv=nodplc(loc+1)
      kntr=kntr+1
      atable(kntr)=value(locv+i)
      loc=nodplc(loc)
      go to 310
  320 if (itab(i).eq.2) go to 330
      write (6,321) ampar(locm+i),(atable(k),k=1,kntr)
  321 format(1h ,a8,12f10.3)
      go to 340
  330 write (6,331) ampar(locm+i),(atable(k),k=1,kntr)
  331 format(1h ,a8,1p12d10.2)
  340 continue
      if (locn.eq.0) go to 390
      locs=locn
      go to 110
  390 continue
c
c  process model parameters
c
c  diodes
c
  400 loc=locate(21)
  410 if (loc.eq.0) go to 420
      locv=nodplc(loc+1)
      if (value(locv+2).ne.0.0d0) value(locv+2)=1.0d0/value(locv+2)
      pb=value(locv+6)
      xm=value(locv+7)
      fc=value(locv+12)
      value(locv+12)=fc*pb
      xfc=dlog(1.0d0-fc)
      value(locv+15)=pb*(1.0d0-dexp((1.0d0-xm)*xfc))/(1.0d0-xm)
      value(locv+16)=dexp((1.0d0+xm)*xfc)
      value(locv+17)=1.0d0-fc*(1.0d0+xm)
      csat=value(locv+1)
      vte=value(locv+3)*vt
      value(locv+18)=vte*dlog(vte/(root2*csat))
      bv=value(locv+13)
      if(bv.eq.0.0d0) go to 418
      cbv=value(locv+14)
      if(cbv.ge.csat*bv/vt) go to 412
      cbv=csat*bv/vt
      write(6,411) value(locv),cbv
  411 format('0warning:  in diode model ',a8,' ibv increased to ',
     1 1pd10.3,/10x,'to resolve incompatibility with specified is',/)
      xbv=bv
      go to 416
  412 tol=reltol*cbv
      xbv=bv-vt*dlog(1.0d0+cbv/csat)
      iter=0
  413 xbv=bv-vt*dlog(cbv/csat+1.0d0-xbv/vt)
      xcbv=csat*(dexp((bv-xbv)/vt)-1.0d0+xbv/vt)
      if (dabs(xcbv-cbv).le.tol) go to 416
      iter=iter+1
      if (iter.lt.25) go to 413
      write (6,415) xbv,xcbv
  415 format('0warning:  unable to match forward and reverse diode regio
     1ns',/,11x,'bv = ',1pd10.3,' and ibv = ',d10.3,/)
  416 value(locv+13)=xbv
  418 loc=nodplc(loc)
      go to 410
c
c  bipolar transistor models
c
  420 loc=locate(22)
  430 if (loc.eq.0) go to 440
      locv=nodplc(loc+1)
      if(value(locv+4).ne.0.0d0) value(locv+4)=1.0d0/value(locv+4)
      if(value(locv+5).ne.0.0d0) value(locv+5)=1.0d0/value(locv+5)
      if(value(locv+10).ne.0.0d0) value(locv+10)=1.0d0/value(locv+10)
      if(value(locv+11).ne.0.0d0) value(locv+11)=1.0d0/value(locv+11)
      if(value(locv+19).ne.0.0d0) value(locv+19)=1.0d0/value(locv+19)
      if(value(locv+20).ne.0.0d0) value(locv+20)=1.0d0/value(locv+20)
      if(value(locv+26).ne.0.0d0) value(locv+26)=1.0d0/value(locv+26)
     1   /1.44d0
      value(locv+28)=value(locv+28)/rad*value(locv+24)
      if(value(locv+35).ne.0.0d0) value(locv+35)=1.0d0/value(locv+35)
     1  /1.44d0
      pe=value(locv+22)
      xme=value(locv+23)
      pc=value(locv+30)
      xmc=value(locv+31)
      fc=value(locv+46)
      value(locv+46)=fc*pe
      xfc=dlog(1.0d0-fc)
      value(locv+47)=pe*(1.0d0-dexp((1.0d0-xme)*xfc))/(1.0d0-xme)
      value(locv+48)=dexp((1.0d0+xme)*xfc)
      value(locv+49)=1.0d0-fc*(1.0d0+xme)
      value(locv+50)=fc*pc
      value(locv+51)=pc*(1.0d0-dexp((1.0d0-xmc)*xfc))/(1.0d0-xmc)
      value(locv+52)=dexp((1.0d0+xmc)*xfc)
      value(locv+53)=1.0d0-fc*(1.0d0+xmc)
      csat=value(locv+1)
      value(locv+54)=vt*dlog(vt/(root2*csat))
      loc=nodplc(loc)
      go to 430
c
c  jfet models
c
  440 loc=locate(23)
  450 if (loc.eq.0) go to 460
      locv=nodplc(loc+1)
      if (value(locv+4).ne.0.0d0) value(locv+4)=1.0d0/value(locv+4)
      if (value(locv+5).ne.0.0d0) value(locv+5)=1.0d0/value(locv+5)
      pb=value(locv+8)
      xm=0.5d0
      fc=value(locv+12)
      value(locv+12)=fc*pb
      xfc=dlog(1.0d0-fc)
      value(locv+13)=pb*(1.0d0-dexp((1.0d0-xm)*xfc))/(1.0d0-xm)
      value(locv+14)=dexp((1.0d0+xm)*xfc)
      value(locv+15)=1.0d0-fc*(1.0d0+xm)
      csat=value(locv+9)
      value(locv+16)=vt*dlog(vt/(root2*csat))
      loc=nodplc(loc)
      go to 450
c
c  mosfet models
c
  460 loc=locate(24)
  470 if (loc.eq.0) go to 600
      locv=nodplc(loc+1)
      if(nodplc(loc+2).eq.0) go to 490
      type=nodplc(loc+2)
c... check validiy of lambda
      if(value(locv+5).lt.5.0d-6) go to 472
      write(6,471) value(locv)
  471 format('0warning:  value for lambda unrealisticly large for model'
     1 ,1x,a8,/'0this parameter has been re-defined.  see latest users '
     2 ,'guide.')
  472 value(locv+5)=value(locv+5)*hndrd
      value(locv+8)=value(locv+8)/hndrd
      value(locv+9)=value(locv+9)/hndrd
      value(locv+10)=value(locv+10)/hndrd
      value(locv+11)=value(locv+11)/hndrd2
      value(locv+12)=value(locv+12)/hndrd2
      value(locv+13)=value(locv+13)*hndrd
      value(locv+15)=value(locv+15)/hndrd2
      value(locv+19)=value(locv+19)*hndrd
      value(locv+20)=value(locv+20)*hndrd
c.. move the params wd-gleff out to positions 36-40
      value(locv+36)=value(locv+30)*hndrd
      value(locv+37)=value(locv+31)
      value(locv+38)=value(locv+32)
      value(locv+39)=value(locv+33)
      value(locv+40)=value(locv+34)
      if(value(locv+39).ne.0.0d0) value(locv+39)=1.0d0/value(locv+39)
      if (value(locv+6).ne.0.0d0) value(locv+6)=1.0d0/value(locv+6)
      if (value(locv+7).ne.0.0d0) value(locv+7)=1.0d0/value(locv+7)
      if (value(locv+13).ne.0.0d0) value(locv+13)=epsox/value(locv+13)
      value(locv+34)=value(locv+1)-
     1  type*value(locv+3)*dsqrt(value(locv+4))
      if (value(locv+13).ne.0.0d0)
     1   value(locv+24)=value(locv+24)*epssil/value(locv+13)
      pb=value(locv+14)
c... enter here from ga-as processing also
  475 xm=0.5d0
      fc=value(locv+29)
      value(locv+29)=fc*pb
      xfc=dlog(1.0d0-fc)
      value(locv+30)=pb*(1.0d0-dexp((1.0d0-xm)*xfc))/(1.0d0-xm)
      value(locv+31)=dexp((1.0d0+xm)*xfc)
      value(locv+32)=1.0d0-fc*(1.0d0+xm)
      value(locv+33)=-1.0d0
  480 loc=nodplc(loc)
      go to 470
c... ga-as processing
  490 value(locv+24)=2.5d+05*dexp(value(locv+2)/1.3d0)
      value(locv+25)=5.0d+06*dexp(-value(locv+4)/vt)
      value(locv+26)=3.9d-12*dsqrt(value(locv+5)*(value(locv+3)-
     1  value(locv+1)))
      value(locv+28)=value(locv+26)*(1.0d0-dsqrt((value(locv+3)-
     1 0.99999d0*value(locv+1))/(value(locv+3)-value(locv+1))))
      value(locv+29)=0.5d0
      pb=value(locv+3)
      go to 475
c
c  reserve additional nodes
c  convert mosfet geometries to cm
c
c  diodes
c
  600 loc=locate(11)
  610 if (loc.eq.0) go to 700
      locm=nodplc(loc+5)
      locm=nodplc(locm+1)
      if (value(locm+2).eq.0.0d0) go to 620
      numnod=numnod+1
      nodplc(loc+4)=numnod
      go to 630
  620 nodplc(loc+4)=nodplc(loc+2)
  630 loc=nodplc(loc)
      go to 610
c
c  transistors
c
  700 loc=locate(12)
  710 if (loc.eq.0) go to 800
c
c  put substrate node into nodplc(loc+30)
c
      nodplc(loc+30)=nodplc(loc+5)
      locm=nodplc(loc+8)
      locm=nodplc(locm+1)
      if(value(locm+16).eq.0.0d0) go to 720
      numnod=numnod+1
      nodplc(loc+6)=numnod
      go to 730
  720 nodplc(loc+6)=nodplc(loc+3)
  730 if (value(locm+20).eq.0.0d0) go to 740
      numnod=numnod+1
      nodplc(loc+5)=numnod
      go to 750
  740 nodplc(loc+5)=nodplc(loc+2)
  750 if (value(locm+19).eq.0.0d0) go to 760
      numnod=numnod+1
      nodplc(loc+7)=numnod
      go to 770
  760 nodplc(loc+7)=nodplc(loc+4)
  770 loc=nodplc(loc)
      go to 710
c
c  jfets
c
  800 loc=locate(13)
  810 if (loc.eq.0) go to 900
      locm=nodplc(loc+7)
      locm=nodplc(locm+1)
      if (value(locm+4).eq.0.0d0) go to 820
      numnod=numnod+1
      nodplc(loc+5)=numnod
      go to 830
  820 nodplc(loc+5)=nodplc(loc+2)
  830 if (value(locm+5).eq.0.0d0) go to 840
      numnod=numnod+1
      nodplc(loc+6)=numnod
      go to 850
  840 nodplc(loc+6)=nodplc(loc+4)
  850 loc=nodplc(loc)
      go to 810
c
c  mosfets
c
  900 loc=locate(14)
  910 if (loc.eq.0) go to 1000
      locm=nodplc(loc+8)
      locv=nodplc(loc+1)
      if(nodplc(locm+2).eq.0) go to 960
      locm=nodplc(locm+1)
      value(locv+1)=value(locv+1)*hndrd
      value(locv+2)=value(locv+2)*hndrd
      value(locv+3)=value(locv+3)*hndrd2
      value(locv+4)=value(locv+4)*hndrd2
c... check that effective channel length is greater than zero
      if((value(locv+1)-2.0d0*value(locm+20)).gt.0.0d0)
     1  go to 914
      write(6,913) value(locv),value(locm)
  913 format('0*error*:  effective channel length of ',a8,' less than ',
     1  'zero.',/' check value of ld for model ',a8)
      nogo=1
  914 if((value(locv+2)-2.0d0*value(locm+36)).gt.0.0d0) go to 916
      write(6,915) value(locv),value(locm)
  915 format('0*error*:  effective channel width of ',a8,' less than ',
     1  'zero.',/' check value of wd for model ',a8)
      nogo=1
  916 if (value(locv+11).eq.0.0d0) go to 917
      value(locv+11)=1.0d0/value(locv+11)
      go to 918
  917 if(value(locm+6).eq.0.0d0) go to 920
      value(locv+11)=value(locm+6)
  918 numnod=numnod+1
      nodplc(loc+6)=numnod
      go to 930
  920 nodplc(loc+6)=nodplc(loc+2)
  930 if (value(locv+12).eq.0.0d0) go to 931
      value(locv+12)=1.0d0/value(locv+12)
      go to 932
  931 if(value(locm+7).eq.0.0d0) go to 940
      value(locv+12)=value(locm+7)
  932 numnod=numnod+1
      nodplc(loc+7)=numnod
      go to 950
  940 nodplc(loc+7)=nodplc(loc+4)
  950 loc=nodplc(loc)
      go to 910
c.. special case for ga-as devices
c.. compute rd and rs if not specified on device card
c.. rd and rs are always non-zero.
  960 locm=nodplc(locm+1)
      req=1.25d+14/(value(locm+5)*value(locv+2))
      if (value(locv+11).eq.0.0d0) value(locv+11)=req
      value(locv+11)=1.0d0/value(locv+11)
      numnod=numnod+1
      nodplc(loc+6)=numnod
      if (value(locv+12).eq.0.0d0) value(locv+12)=req
      value(locv+12)=1.0d0/value(locv+12)
      numnod=numnod+1
      nodplc(loc+7)=numnod
      loc=nodplc(loc)
      go to 910
c
c  transmission lines
c
 1000 loc=locate(17)
 1010 if (loc.eq.0) go to 2000
      numnod=numnod+1
      nodplc(loc+6)=numnod
      numnod=numnod+1
      nodplc(loc+7)=numnod
      loc=nodplc(loc)
      go to 1010
c
c  finished
c
 2000 return
      end
      subroutine topchk
      implicit double precision (a-h,o-z)
c
c     this routine constructs the element node table.  it also checks
c for voltage source/inductor loops, current source/capacitor cutsets,
c and that every node has a dc (conductive) path to ground.
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
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension atable(12),aide(20),nnods(20)
      dimension idlist(4)
      dimension toptit(4)
      data toptit / 8helement , 8hnode tab, 8hle      , 8h         /
      data idlist / 3, 6, 8, 9 /
      data aide / 1hr,0.0d0,1hl,2*0.0d0,1he,0.0d0,1hh,1hv,0.0d0,1hd,
     1   1hq,1hj,1hm,0.0d0,0.0d0,1ht,0.0d0,0.0d0,0.0d0 /
      data nnods / 2,2,2,0,2,2,2,2,2,2,2,4,3,4,4,4,4,0,1,0 /
      data ablnk /1h /
c
c  allocate storage
c
      call getm4(iorder,ncnods)
      call getm4(iur,ncnods+1)
c
c  construct node table
c
      kntlim=lwidth/11
 1300 call getm4(itable,0)
      call getm4(itabid,0)
      istop=ncnods+1
      do 1310 i=1,istop
 1310 nodplc(iur+i)=1
      do 1370 id=1,19
      if (nnods(id).eq.0) go to 1370
      loc=locate(id)
 1320 if (loc.eq.0) go to 1370
      nloc=loc+1
      jstop=nnods(id)
      if (id.ne.19) go to 1330
      nloc=nodplc(loc+2)
      call sizmem(nodplc(loc+2),jstop)
 1330 do 1360 j=1,jstop
      node=nodplc(nloc+j)
      ispot=nodplc(iur+node+1)
      k=nodplc(iur+ncnods+1)
      call extmem(itable,1)
      call extmem(itabid,1)
      if (k.le.ispot) go to 1340
      call copy4(nodplc(itable+ispot),nodplc(itable+ispot+1),k-ispot)
      call copy4(nodplc(itabid+ispot),nodplc(itabid+ispot+1),k-ispot)
 1340 nodplc(itable+ispot)=loc
      nodplc(itabid+ispot)=id
c...  treat the substrate node of a mosfet as if it were a transmission
c...  line node, i.e. let it dangle if desired
      if(id.eq.14.and.j.eq.4) nodplc(itabid+ispot)=17
      k=node
      kstop=ncnods+1
 1350 k=k+1
      if (k.gt.kstop) go to 1360
      nodplc(iur+k)=nodplc(iur+k)+1
      go to 1350
 1360 continue
      loc=nodplc(loc)
      go to 1320
 1370 continue
c
c  check that every node has a dc path to ground
c
      call zero4(nodplc(iorder+1),ncnods)
      nodplc(iorder+1)=1
 1420 iflag=0
      do 1470 i=2,ncnods
      if (nodplc(iorder+i).eq.1) go to 1470
      jstart=nodplc(iur+i)
      jstop=nodplc(iur+i+1)-1
      if (jstart.gt.jstop) go to 1470
      do 1450 j=jstart,jstop
      loc=nodplc(itable+j)
      id=nodplc(itabid+j)
      if (aide(id).eq.0.0d0) go to 1450
      if (id.eq.17) go to 1445
      kstop=loc+nnods(id)-1
      do 1440 k=loc,kstop
      node=nodplc(k+2)
      if (nodplc(iorder+node).eq.1) go to 1460
 1440 continue
      go to 1450
 1445 if (nodplc(loc+2).eq.i) node=nodplc(loc+3)
      if (nodplc(loc+3).eq.i) node=nodplc(loc+2)
      if (nodplc(loc+4).eq.i) node=nodplc(loc+5)
      if (nodplc(loc+5).eq.i) node=nodplc(loc+4)
      if (nodplc(iorder+node).eq.1) go to 1460
 1450 continue
      go to 1470
 1460 nodplc(iorder+i)=1
      iflag=1
 1470 continue
      if (iflag.eq.1) go to 1420
c
c  print node table and topology error messages
c
      if (iprntn.eq.0) go to 1510
      call title(0,lwidth,1,toptit)
 1510 do 1590 i=1,ncnods
      jstart=nodplc(iur+i)
      jstop=nodplc(iur+i+1)-1
      if (iprntn.eq.0) go to 1550
      if (jstart.le.jstop) go to 1520
      write (6,1511) nodplc(junode+i)
 1511 format(1h0,i7)
      go to 1550
 1520 kntr=0
      jflag=1
      do 1540 j=jstart,jstop
      loc=nodplc(itable+j)
      locv=nodplc(loc+1)
      kntr=kntr+1
      atable(kntr)=value(locv)
      if (kntr.lt.kntlim) go to 1540
      if (jflag.eq.0) go to 1525
      jflag=0
      write (6,1521) nodplc(junode+i),(atable(k),k=1,kntr)
 1521 format(1h0,i7,3x,12(1x,a8))
      go to 1530
 1525 write (6,1526) (atable(k),k=1,kntr)
 1526 format(11x,12(1x,a8))
 1530 kntr=0
 1540 continue
      if (kntr.eq.0) go to 1550
      if (jflag.eq.0) go to 1545
      write (6,1521) nodplc(junode+i),(atable(k),k=1,kntr)
      go to 1550
 1545 write (6,1526) (atable(k),k=1,kntr)
 1550 if (jstart-jstop) 1560,1552,1556
c
c  allow node with only one connection iff element is a t-line
c
 1552 if (nodplc(itabid+jstart).eq.17) go to 1560
 1556 nogo=1
      write (6,1557) nodplc(junode+i)
 1557 format('0*error*:  less than 2 connections at node ',i6/)
      go to 1590
 1560 if (nodplc(iorder+i).eq.1) go to 1590
      nogo=1
      write (6,1561) nodplc(junode+i)
 1561 format('0*error*:  no dc path to ground from node ',i6/)
 1590 continue
c
c  check for inductor/voltage source loops
c
      do 1700 i=1,ncnods
      call zero4(nodplc(iorder+1),ncnods)
      nodplc(iorder+i)=-1
      do 1690 idcntr=1,4
      id=idlist(idcntr)
      loc=locate(id)
 1610 if (loc.eq.0) go to 1690
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      if (nodplc(iorder+node1)) 1620,1640,1630
 1620 nodplc(iorder+node1)=loc
 1630 node=node2
      go to 1670
 1640 if (nodplc(iorder+node2)) 1650,1680,1660
 1650 nodplc(iorder+node2)=loc
 1660 node=node1
 1670 if (nodplc(iorder+node).ne.0) go to 1710
      nodplc(iorder+node)=loc
 1680 loc=nodplc(loc)
      go to 1610
 1690 continue
 1700 continue
      go to 1900
c ... loop found
 1710 locv=nodplc(loc+1)
      write (6,1711) value(locv)
 1711 format('0*error*:  inductor/voltage source loop found, containing
     1',a8/)
      nogo=1
c
c
 1900 call clrmem(iorder)
      call clrmem(iur)
      call clrmem(itable)
      call clrmem(itabid)
 2000 return
      end
