      subroutine dctran
      implicit double precision (a-h,o-z)
c
c
c     this routine controls the dc transfer curve, dc operating point,
c and transient analyses.  the variables mode and modedc (defined below)
c determine exactly which analysis is performed.
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
      common /dc/ tcstar(2),tcstop(2),tcincr(2),icvflg,itcelm(2),kssop,
     1   kinel,kidin,kovar,kidout
      common /tran/ tstep,tstop,tstart,delmax,tdmax,forfre,jtrflg
      common /cje/ maxtim,itime,icost
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension subtit(4,2)
      dimension avhdr(3),avfrm(4)
       data aendor /9.87654321d+27/
      data avhdr / 8h( (2x,a4, 8h,3x,a7,3, 5hx)//) /
      data avfrm / 8h( (1h ,a, 8h1,i3,1h), 8h,f10.4,3, 4hx)/) /
      data anode, avltg / 4hnode, 7hvoltage /
      data subtit / 8hsmall si, 8hgnal bia, 8hs soluti, 8hon      ,
     1              8hinitial , 8htransien, 8ht soluti, 8hon      /
      data lprn /1h(/
c
c      the variables *mode*, *modedc*, and *initf* are used by spice to
c keep track of the state of the analysis.  the values of these flags
c (and the corresponding meanings) are as follows:
c
c        flag    value    meaning
c        ----    -----    -------
c
c        mode      1      dc analysis (subtype defined by *modedc*)
c                  2      transient analysis
c                  3      ac analysis (small signal)
c
c        modedc    1      dc operating point
c                  2      initial operating point for transient analysis
c                  3      dc transfer curve computation
c
c        initf     1      converge with 'off' devices allowed to float
c                  2      initialize junction voltages
c                  3      converge with 'off' devices held 'off'
c                  4      store small-signal parameters away
c                  5      first timepoint in transient analysis
c                  6      prediction step
c
c note:  *modedc* is only significant if *mode* = 1.
c
c
c  initialize
c
      call second(t1)
c.. don't take any chances with lx3, set to large number
      lx3=20000000
      lx2=20000000
      nolx2=0
      nolx3=0
      loctim=5
c.. post-processing initialization
      if(ipostp.eq.0) go to 1
      numcur=jelcnt(9)
      numpos=nunods+numcur
      call getm8(ibuff,numpos)
      numpos=numpos*4
      if(numcur.eq.0) go to 1
      loc=locate(9)
      loccur=nodplc(loc+6)-1
c...  set up format
    1 nvprln=4+(lwidth-72)/19
      nvprln=min0(nvprln,ncnods-1)
      ipos=2
      call alfnum(nvprln,avfrm,ipos)
      ipos=2
      call alfnum(nvprln,avhdr,ipos)
c...  allocate storage
      if (mode.eq.2) go to 5
      need=4*nstop+nut+nlt+nxtrm
      call avlm8(navl)
      if(need.le.navl) go to 4
c...  not enough memory for dc operating point analysis
      write(6,3) need,navl
    3 format('0insufficient memory available for dc analysis.',/
     1' memory required ',i6,', memory available ',i6,'.')
      nogo=1
      go to 1100
    4 call getm8(lvnim1,nstop)
      call getm8(ndiag,nstop)
      call getm8(lvn,nstop+nstop+nut+nlt)
      call getm8(lx0,nxtrm)
      if (modedc.ne.3) go to 15
    5 call getm8(lx1,nxtrm)
      if(nolx2.eq.0) call getm8(lx2,nxtrm)
      if (mode.ne.2) go to 12
      if(nolx3.eq.0) call getm8(lx3,nxtrm)
      call getm8(ltd,0)
   12 call getm8(loutpt,0)
   15 call crunch
      lynl=lvn+nstop
      lyu=lynl+nstop
      lyl=lyu+nut
   20 if (mode.eq.2) go to 500
      time=0.0d0
      ag(1)=0.0d0
      call sorupd
      if (modedc.eq.3) go to 300
c
c  ....  single point dc analysis
c
c
c  compute dc operating point
c
  100 initf=2
      call iter8(itl1)
      rstats(6)=rstats(6)+iterno
      if (igoof.ne.0) go to 150
      if (modedc.ne.1) go to 120
      initf=4
      call diode
      call bjt
      call jfet
      call mosfet
c
c  print operating point
c
  120 if ((mode.eq.1).and.(modedc.eq.2).and.(nosolv.ne.0)) go to 1000
      call title(-1,lwidth,1,subtit(1,modedc))
      write (6,avhdr) (anode,avltg,i=1,nvprln)
      write (6,avfrm) (lprn,nodplc(junode+i),value(lvnim1+i),i=2,ncnods)
      go to 1000
c
c  no convergence
c
  150 nogo=1
      write (6,151)
  151 format('1*error*:  no convergence in dc analysis'/'0last node vol'
     1   ,'tages:'/)
      write (6,avhdr) (anode,avltg,i=1,nvprln)
      write (6,avfrm) (lprn,nodplc(junode+i),value(lvnim1+i),i=2,ncnods)
      go to 1000
c
c  ....  dc transfer curves
c
  300 numout=jelcnt(41)+1
      if(ipostp.ne.0) call pheadr(atitle)
      itemp=itcelm(1)
      locs=nodplc(itemp+1)
      temval=value(locs+1)
      icvfl2=1
      if(itcelm(2).eq.0) go to 310
      itemp=itcelm(2)
      locs2=nodplc(itemp+1)
      temv2=value(locs2+1)
      value(locs2+1)=tcstar(2)
      temp=dabs((tcstop(2)-tcstar(2))/tcincr(2))+0.5d0
      icvfl2=idint(temp)+1
      icvfl2=max0(icvfl2,1)
  310 delta=tcincr(1)
      do 320 i=1,7
      delold(i)=delta
  320 continue
      icvfl1=icvflg/icvfl2
      value(locs+1)=tcstar(1)
      icalc=0
      ical2=0
      loctim=3
  340 initf=2
      call iter8(itl1)
      rstats(4)=rstats(4)+iterno
      call copy8(value(lx0+1),value(lx1+1),nxtrm)
      if(nolx2.eq.0) call copy8(value(lx0+1),value(lx2+1),nxtrm)
      if (igoof.ne.0) go to 450
      go to 360
  350 call getcje
      if ((maxtim-itime).le.limtim) go to 460
      initf=6
      call iter8(itl2)
      rstats(4)=rstats(4)+iterno
      if (igoof.ne.0) go to 340
c
c  store outputs
c
  360 call extmem(loutpt,numout)
      loco=loutpt+icalc*numout
      icalc=icalc+1
      ical2=ical2+1
      value(loco+1)=value(locs+1)
      loc=locate(41)
  370 if (loc.eq.0) go to 400
      if (nodplc(loc+5).ne.0) go to 380
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      iseq=nodplc(loc+4)
      value(loco+iseq)=value(lvnim1+node1)-value(lvnim1+node2)
      loc=nodplc(loc)
      go to 370
  380 iptr=nodplc(loc+2)
      iptr=nodplc(iptr+6)
      iseq=nodplc(loc+4)
      value(loco+iseq)=value(lvnim1+iptr)
      loc=nodplc(loc)
      go to 370
c
c  increment source value
c
  400 if(ipostp.eq.0) go to 410
      value(ibuff+1)=value(locs+1)
      call copy8(value(lvnim1+2),value(ibuff+2),nunods-1)
      if(numcur.ne.0) call copy8(value(lvnim1+loccur+1),
     1  value(ibuff+nunods+1),numcur)
      call fwrite(value(ibuff+1),numpos)
  410 if (icalc.ge.icvflg) go to 490
      if(ical2.ge.icvfl1) go to 480
      if(nolx2.ne.0) go to 420
      call ptrmem(lx2,itemp)
      call ptrmem(lx1,lx2)
      go to 430
  420 call ptrmem(lx1,itemp)
  430 call ptrmem(lx0,lx1)
      call ptrmem(itemp,lx0)
      value(locs+1)=tcstar(1)+dfloat(ical2)*delta
      go to 350
c
c  no convergence
c
  450 itemp=itcelm(1)
      loce=nodplc(itemp+1)
      write (6,451) value(loce),value(locs+1)
  451 format('1*error*:  no convergence in dc transfer curves at ',a8,
     1   ' = ',1pd10.3/'0last node voltages:'/)
      write (6,avhdr) (anode,avltg,i=1,nvprln)
      write (6,avfrm) (lprn,nodplc(junode+i),value(lvnim1+i),i=2,ncnods)
      go to 470
  460 write (6,461)
  461 format('0*error*:  cpu time limit exceeded ... analysis stopped'/)
  470 nogo=1
      go to 490
c... reset first sweep variable ... step second
  480 ical2=0
      value(locs+1)=tcstar(1)
      value(locs2+1)=value(locs2+1)+tcincr(2)
      go to 340
c
c  finished with dc transfer curves
c
  490 value(locs+1)=temval
      if(itcelm(2).ne.0) value(locs2+1)=temv2
      if(ipostp.eq.0) go to 1000
      value(ibuff+1)=aendor
      call fwrite(value(ibuff+1),numpos)
      go to 1000
c
c  ....  transient analysis
c
  500 numout=jelcnt(42)+1
      if(ipostp.ne.0) call pheadr(atitle)
      numese=jelcnt(2)+jelcnt(3)+jelcnt(11)+jelcnt(12)+jelcnt(13)
     1   +jelcnt(14)
      if (numese.eq.0) delmax=dmin1(delmax,tstep)
      initf=5
      iord=1
      loctim=9
      icalc=0
      numtp=0
      numrtp=0
      numnit=0
      time=0.0d0
      ibkflg=1
      delbkp=delmax
      nbkpt=1
      delta=delmax
      do 510 i=1,7
      delold(i)=delta
  510 continue
      delmin=1.0d-9*delmax
      go to 650
c
c  increment time, update sources, and solve next timepoint
c
  600 time=time+delta
      call sorupd
      if (nogo.ne.0) go to 950
      call getcje
      if ((maxtim-itime).le.limtim) go to 920
      if ((itl5.ne.0).and.(numnit.ge.itl5)) go to 905
      call comcof
      if (initf.ne.5) initf=6
      itrlim=itl4
      if ((numtp.eq.0).and.(nosolv.ne.0)) itrlim=itl1
      call iter8(itrlim)
      if(itl5.ne.0) numnit=numnit+iterno
      numtp=numtp+1
      if (numtp.ne.1) go to 605
      if(nolx2.eq.0) call copy8(value(lx1+1),value(lx2+1),nxtrm)
      if(nolx3.eq.0) call copy8(value(lx1+1),value(lx3+1),nxtrm)
c.. note that time-point is cut when itrlim exceeded regardless
c.. of which time-step contol is specified thru 'lvltim'.
  605 if (igoof.eq.0) go to 610
      jord=iord
      iord=1
      if (jord.ge.5) call clrmem(lx7)
      if (jord.ge.4) call clrmem(lx6)
      if (jord.ge.3) call clrmem(lx5)
      if ((jord.ge.2).and.(method.ne.1)) call clrmem(lx4)
      igoof=0
      time=time-delta
      delta=delta/8.0d0
      go to 620
  610 delnew=delta
      if (numtp.eq.1) go to 630
      call trunc(delnew)
      if (delnew.ge.(0.9d0*delta)) go to 630
      time=time-delta
      delta=delnew
  620 numrtp=numrtp+1
      ibkflg=0
      delold(1)=delta
      if (delta.ge.delmin) go to 600
      time=time+delta
      go to 900
c.. time-point accepted
  630 call copy8(delold(1),delold(2),6)
      delta=delnew
      delold(1)=delta
c
c  determine order of integration method
c
c...  skip if trapezoidal algorithm used
      if ((method.eq.1).and.(iord.eq.2)) go to 650
      if (numtp.eq.1) go to 650
      ordrat=1.05d0
      if (iord.gt.1) go to 635
      iord=2
      call trunc(delnew)
      iord=1
      if ((delnew/delta).le.ordrat) go to 650
      if (maxord.le.1) go to 650
      iord=2
      if (method.eq.1) go to 650
      call getm8(lx4,nxtrm)
      go to 650
  635 if (iord.lt.maxord) go to 640
      iord=iord-1
      call trunc(delnew)
      iord=iord+1
      if ((delnew/delta).le.ordrat) go to 650
      go to 642
  640 iord=iord-1
      call trunc(delnew)
      iord=iord+1
      if ((delnew/delta).le.ordrat) go to 645
  642 iord=iord-1
      if (iord.eq.1) call clrmem(lx4)
      if (iord.eq.2) call clrmem(lx5)
      if (iord.eq.3) call clrmem(lx6)
      if (iord.eq.4) call clrmem(lx7)
      go to 650
  645 iord=iord+1
      call trunc(delnew)
      iord=iord-1
      if ((delnew/delta).le.ordrat) go to 650
      iord=iord+1
      if (iord.eq.2) call getm8(lx4,nxtrm)
      if (iord.eq.3) call getm8(lx5,nxtrm)
      if (iord.eq.4) call getm8(lx6,nxtrm)
      if (iord.eq.5) call getm8(lx7,nxtrm)
c
c  store outputs
c
  650 if ((time+delta).le.tstart) go to 685
      if ((numtp.eq.0).and.(nosolv.ne.0)) go to 685
      call extmem(loutpt,numout)
      loco=loutpt+icalc*numout
      icalc=icalc+1
      value(loco+1)=time
      loc=locate(42)
  670 if (loc.eq.0) go to 682
      if (nodplc(loc+5).ne.0) go to 680
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      iseq=nodplc(loc+4)
      value(loco+iseq)=value(lvnim1+node1)-value(lvnim1+node2)
      loc=nodplc(loc)
      go to 670
  680 iptr=nodplc(loc+2)
      iptr=nodplc(iptr+6)
      iseq=nodplc(loc+4)
      value(loco+iseq)=value(lvnim1+iptr)
      loc=nodplc(loc)
      go to 670
  682 if(ipostp.eq.0) go to 684
      value(ibuff+1)=time
      call copy8(value(lvnim1+2),value(ibuff+2),nunods-1)
      if(numcur.ne.0) call copy8(value(lvnim1+loccur+1),
     1  value(ibuff+nunods+1),numcur)
      call fwrite(value(ibuff+1),numpos)
  684 continue
  685 if (jelcnt(17).eq.0) go to 694
      call sizmem(ltd,ltdsiz)
      numtd=ltdsiz/ntlin
      if (numtd.le.3) go to 689
      baktim=time-tdmax
      if (baktim.lt.0.0d0) go to 689
      lcntr=0
      ltemp=ltd
      do 686 i=1,numtd
      if (value(ltemp+1).ge.baktim) go to 687
      ltemp=ltemp+ntlin
      lcntr=lcntr+1
  686 continue
      go to 689
  687 if (lcntr.le.2) go to 689
      lcntr=lcntr-2
      nwords=lcntr*ntlin
      ltemp=ltemp-ntlin-ntlin
      call copy8(value(ltemp+1),value(ltd+1),ltdsiz-nwords)
      call relmem(ltd,nwords)
      call sizmem(ltd,ltdsiz)
  689 call extmem(ltd,ntlin)
      ltdptr=ltd+ltdsiz
      value(ltdptr+1)=time
      loc=locate(17)
  690 if (loc.eq.0) go to 693
      locv=nodplc(loc+1)
      z0=value(locv+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      ibr1=nodplc(loc+8)
      ibr2=nodplc(loc+9)
      lspot=nodplc(loc+30)+ltdptr
      if ((initf.eq.5).and.(nosolv.ne.0)) go to 691
      value(lspot)=value(lvnim1+node3)-value(lvnim1+node4)
     1   +value(lvnim1+ibr2)*z0
      value(lspot+1)=value(lvnim1+node1)-value(lvnim1+node2)
     1   +value(lvnim1+ibr1)*z0
      go to 692
  691 value(lspot)=value(locv+7)+value(locv+8)*z0
      value(lspot+1)=value(locv+5)+value(locv+6)*z0
  692 loc=nodplc(loc)
      go to 690
c
c  add two *fake* backpoints to ltd for interpolation near time=0.0d0
c
  693 if (numtd.ne.0) go to 694
      call extmem(ltd,ntlin+ntlin)
      call copy8(value(ltd+1),value(ltd+ntlin+1),ntlin)
      call copy8(value(ltd+1),value(ltd+2*ntlin+1),ntlin)
      value(ltd+2*ntlin+1)=time
      value(ltd+ntlin+1)=time-delta
      value(ltd+1)=time-delta-delta
c
c  rotate state vector storage
c
  694 go to (710,706,702,698,696,696), iord
  696 call ptrmem(lx7,itemp)
      call ptrmem(lx6,lx7)
      go to 700
  698 call ptrmem(lx6,itemp)
  700 call ptrmem(lx5,lx6)
      go to 704
  702 call ptrmem(lx5,itemp)
  704 call ptrmem(lx4,lx5)
      go to 708
  706 if (method.eq.1) go to 710
      call ptrmem(lx4,itemp)
  708 call ptrmem(lx3,lx4)
      go to 713
  710 if(nolx3.eq.0) go to 712
      if(nolx2.eq.0) go to 711
      call ptrmem(lx1,itemp)
      go to 714
  711 call ptrmem(lx2,itemp)
      call ptrmem(lx1,lx2)
      go to 714
  712 call ptrmem(lx3,itemp)
  713 call ptrmem(lx2,lx3)
      call ptrmem(lx1,lx2)
  714 call ptrmem(lx0,lx1)
      call ptrmem(itemp,lx0)
c
c  check breakpoints
c
  750 if (ibkflg.eq.0) go to 760
c.. just accepted analysis at breakpoint
      jord=iord
      iord=1
      if (jord.ge.5) call clrmem(lx7)
      if (jord.ge.4) call clrmem(lx6)
      if (jord.ge.3) call clrmem(lx5)
      if ((jord.ge.2).and.(method.ne.1)) call clrmem(lx4)
      ibkflg=0
      nbkpt=nbkpt+1
      if (nbkpt.gt.numbkp) go to 950
      temp=dmin1(delbkp,value(lsbkpt+nbkpt)-time)
      delta=dmin1(delta,0.1d0*temp,delmax)
      if (numtp.eq.0) delta=delta/10.0d0
      delold(1)=delta
      go to 600
  760 del1=value(lsbkpt+nbkpt)-time
      if ((1.01d0*delta).le.del1) go to 600
      ibkflg=1
      delbkp=delta
      delta=del1
      delold(1)=delta
      go to 600
c
c  transient analysis failed
c
  900 write (6,901)
  901 format('1*error*:  internal timestep too small in transient analys
     1is'/)
      go to 910
  905 write (6,906) itl5
  906 format('1*error*:  transient analysis iterations exceed limit of '
     1,i5,/'0this limit may be overridden using the itl5 parameter on th
     2e .option card')
  910 write (6,911) time,delta,numnit
  911 format(1h0,10x,'time = ',1pd12.5,';  delta = ',d12.5,';  numnit =
     1',i6/)
      write (6,916)
  916 format(1h0/'0last node voltages:'/)
      write (6,avhdr) (anode,avltg,i=1,nvprln)
      write (6,avfrm) (lprn,nodplc(junode+i),value(lvnim1+i),i=2,ncnods)
      go to 930
  920 write (6,921) time
  921 format('0*error*:  cpu time limit exceeded in transient analysis '
     1   ,'at time = ',1pd13.6/)
  930 nogo=1
c
c  finished with transient analysis
c
  950 rstats(10)=rstats(10)+numnit
      rstats(30)=rstats(30)+numtp
      rstats(31)=rstats(31)+numrtp
      rstats(32)=rstats(32)+numnit
      if(ipostp.eq.0) go to 1000
      value(ibuff+1)=aendor
      call fwrite(value(ibuff+1),numpos)
c
c  return unneeded memory
c
 1000 if (mode.eq.2) go to 1010
      if (modedc.ne.3) go to 1100
 1010 call clrmem(lvnim1)
      call clrmem(lx0)
      call clrmem(ndiag)
      call clrmem(lvn)
      call clrmem(lx1)
      if(nolx2.eq.0) call clrmem(lx2)
      if ((mode.eq.1).and.(modedc.eq.3)) go to 1020
      if(nolx3.eq.0) call clrmem(lx3)
      if (mode.eq.1) go to 1020
      call clrmem(ltd)
      if (iord.eq.1) go to 1020
      if (method.eq.1) go to 1020
      call clrmem(lx4)
      if (iord.eq.2) go to 1020
      call clrmem(lx5)
      if (iord.eq.3) go to 1020
      call clrmem(lx6)
      if (iord.eq.4) go to 1020
      call clrmem(lx7)
 1020 call extmem(loutpt,2*numout)
 1100 if(ipostp.ne.0) call clrmem(ibuff)
      call second(t2)
      rstats(loctim)=rstats(loctim)+t2-t1
      return
      end
      subroutine fwrite(iarray,numwds)
c
c   write onto 'punch' file numwds 16-bit words starting
c   with location iarray(/1/)
c
      integer iarray(1)
c
c... data must be written out in 40 word (80 byte) chunks
c
      integer idata(20)
      numwd4=(numwds+1)/2
      numblk=(numwd4-1)/20+1
      kntr=1
      numlft=numwd4
      do 10 i=1,numblk
      kstop=min0(numlft,20)
      call copy4(iarray(kntr),idata(1),kstop)
      write(7) idata
      kntr=kntr+20
      numlft=numlft-20
   10 continue
      return
      end
      subroutine pheadr(aheadr)
      implicit double precision (a-h,o-z)
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
      common /dc/ tcstar(2),tcstop(2),tcincr(2),icvflg,itcelm(2),kssop,
     1   kinel,kidin,kovar,kidout
      common /miscel/ atime,aprog(3),adate,atitle(10),defl,defw,defad,
     1  defas,rstats(50),iwidth,lwidth,nopage
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      integer*2 int2,nodpl2(128)
      equivalence (value(1),nodpl2(1))
      equivalence (value(1),nodplc(1),cvalue(1))
      dimension aheadr(10)
c
c  put out the header records onto the post-processing file
c  routine is used for all analysis modes (mode=1,2,3)
c
      dimension xtype(2)
      data xtype /4htime,4hfreq/
      data ablnk,aletv,aleti /1h ,1hv,1hi/
      call getm8(ibuff,12)
      call copy8(aheadr(1),value(ibuff+1),10)
      value(ibuff+11)=adate
      value(ibuff+12)=atime
      call fwrite(value(ibuff+1),48)
      numout=nunods+jelcnt(9)
      info=3
      call getm8(inames,numout)
      call getm4(itypes,numout)
      call getm4(iseqs,numout)
      itype2=itypes*2
      iseq2=iseqs*2
      iknt=1
      nodpl2(iseq2+1)=1
      if(mode.ne.1) go to 10
      loc=itcelm(1)
      locv=nodplc(loc+1)
      value(inames+1)=value(locv)
      anam=ablnk
      call move(anam,1,value(locv),1,1)
      ityp=0
      if(anam.eq.aletv) ityp=3
      if(anam.eq.aleti) ityp=4
      nodpl2(itype2+1)=ityp
      go to 20
   10 value(inames+1)=xtype(mode-1)
      nodpl2(itype2+1)=mode-1
   20 do 30 i=2,nunods
      nodpl2(itype2+i)=3
      nodpl2(iseq2+i)=i
      value(inames+i)=ablnk
      ipos=1
      call alfnum(nodplc(junode+i),value(inames+i),ipos)
   30 continue
      loc=locate(9)
      iknt=nunods
   40 if(loc.eq.0) go to 50
      iknt=iknt+1
      nodpl2(itype2+iknt)=4
      nodpl2(iseq2+iknt)=iknt
      locv=nodplc(loc+1)
      value(inames+iknt)=value(locv)
      loc=nodplc(loc)
      go to 40
   50 int2=numout
      call fwrite(int2,1)
      int2=info
      call fwrite(int2,1)
      nwds=numout*4
      call fwrite(value(inames+1),nwds)
      call fwrite(nodpl2(itype2+1),numout)
      call fwrite(nodpl2(iseq2+1),numout)
      call clrmem(ibuff)
      call clrmem(inames)
      call clrmem(itypes)
      call clrmem(iseqs)
      return
      end
      subroutine comcof
      implicit double precision (a-h,o-z)
c
c     this routine calculates the timestep-dependent terms used in the
c numerical integration.
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
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
      dimension gmat(7,7)
c
c  compute coefficients for particular integration method
c
      if (method.ne.1) go to 5
      if (iord.eq.1) go to 5
c...  trapezoidal method
      ag(1)=1.0d0/delta/(1.0d0-xmu)
      ag(2)=xmu/(1.0d0-xmu)
      go to 200
c
c  construct gear coefficient matrix
c
    5 istop=iord+1
      call zero8(ag,istop)
      ag(2)=-1.0d0
      do 10 i=1,istop
      gmat(1,i)=1.0d0
   10 continue
      do 20 i=2,istop
      gmat(i,1)=0.0d0
   20 continue
      arg=0.0d0
      do 40 i=2,istop
      arg=arg+delold(i-1)
      arg1=1.0d0
      do 30 j=2,istop
      arg1=arg1*arg
      gmat(j,i)=arg1
   30 continue
   40 continue
c
c  solve for gear coefficients ag(*)
c
c
c  lu decomposition
c
      do 70 i=2,istop
      jstart=i+1
      if (jstart.gt.istop) go to 70
      do 60 j=jstart,istop
      gmat(j,i)=gmat(j,i)/gmat(i,i)
      do 50 k=jstart,istop
      gmat(j,k)=gmat(j,k)-gmat(j,i)*gmat(i,k)
   50 continue
   60 continue
   70 continue
c
c  forward substitution
c
      do 90 i=2,istop
      jstart=i+1
      if (jstart.gt.istop) go to 90
      do 80 j=jstart,istop
      ag(j)=ag(j)-gmat(j,i)*ag(i)
   80 continue
   90 continue
c
c  backward substitution
c
      ag(istop)=ag(istop)/gmat(istop,istop)
      ir=istop
      do 110 i=2,istop
      jstart=ir
      ir=ir-1
      do 100 j=jstart,istop
      ag(ir)=ag(ir)-gmat(ir,j)*ag(j)
  100 continue
      ag(ir)=ag(ir)/gmat(ir,ir)
  110 continue
c
c  finished
c
  200 return
      end
      subroutine trunc(delnew)
      implicit double precision (a-h,o-z)
c
c     this routine determines the new transient stepsize by either
c calling terr to estimate the local truncation error, or by checking
c on the number of iterations needed to converge at the last timepoint.
c
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /tran/ tstep,tstop,tstart,delmax,tdmax,forfre,jtrflg
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      if (lvltim.ne.0) go to 5
      delnew=dmin1(tstep,delmax)
      return
    5 if (lvltim.ne.1) go to 10
      delnew=delta
      if (iterno.gt.itl3) return
      delnew=dmin1(2.0d0*delta,tstep,delmax)
      return
c
c  capacitors
c
   10 delnew=1.0d20
      loc=locate(2)
   20 if (loc.eq.0) go to 30
      loct=nodplc(loc+8)
      call terr(loct,delnew)
      loc=nodplc(loc)
      go to 20
c
c  inductors
c
   30 loc=locate(3)
   40 if (loc.eq.0) go to 50
      loct=nodplc(loc+11)
      call terr(loct,delnew)
      loc=nodplc(loc)
      go to 40
c
c  diodes
c
   50 loc=locate(11)
   60 if (loc.eq.0) go to 70
      loct=nodplc(loc+11)
      call terr(loct+3,delnew)
      loc=nodplc(loc)
      go to 60
c
c  bjts
c
   70 loc=locate(12)
   80 if (loc.eq.0) go to 90
      loct=nodplc(loc+22)
      call terr(loct+8,delnew)
      call terr(loct+10,delnew)
      call terr(loct+12,delnew)
      loc=nodplc(loc)
      go to 80
c
c  jfets
c
   90 loc=locate(13)
  100 if (loc.eq.0) go to 110
      loct=nodplc(loc+19)
      call terr(loct+9,delnew)
      call terr(loct+11,delnew)
      loc=nodplc(loc)
      go to 100
c
c  mosfets
c
  110 loc=locate(14)
  120 if (loc.eq.0) go to 200
      loct=nodplc(loc+26)
      call terr(loct+12,delnew)
      call terr(loct+14,delnew)
      call terr(loct+16,delnew)
      call terr(loct+18,delnew)
      call terr(loct+20,delnew)
      loc=nodplc(loc)
      go to 120
c
c  delta is allowed only to double at each timepoint
c
  200 delnew=dmin1(2.0d0*delta,delnew,delmax)
      return
      end
      subroutine terr(loct,delnew)
      implicit double precision (a-h,o-z)
c
c     this routine estimates the local truncation error for a particular
c circuit element.  it then computes the appropriate stepsize which
c should be used.
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
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension qcap(1),ccap(1),diff(8),deltmp(7),coef(6)
      equivalence (qcap(1),value(1)),(ccap(1),value(2))
      data coef / 5.000000000d-1, 2.222222222d-1, 1.363636364d-1,
     1            9.600000000d-2, 7.299270073d-2, 5.830903790d-2 /
      data xtwelv / 8.333333333d-2 /
c
c
      tol=reltol*dmax1(dabs(ccap(lx0+loct)),dabs(ccap(lx1+loct)))+abstol
      ctol=reltol*dmax1(dabs(qcap(lx0+loct)),dabs(qcap(lx1+loct)),
     1   chgtol)/delta
      tol=dmax1(tol,ctol)
c
c  determine divided differences
c
      go to (6,5,4,3,2,1), iord
    1 diff(8)=qcap(lx7+loct)
    2 diff(7)=qcap(lx6+loct)
    3 diff(6)=qcap(lx5+loct)
    4 diff(5)=qcap(lx4+loct)
    5 diff(4)=qcap(lx3+loct)
    6 diff(3)=qcap(lx2+loct)
      diff(2)=qcap(lx1+loct)
      diff(1)=qcap(lx0+loct)
      istop=iord+1
      do 10 i=1,istop
      deltmp(i)=delold(i)
   10 continue
   20 do 30 i=1,istop
      diff(i)=(diff(i)-diff(i+1))/deltmp(i)
   30 continue
      istop=istop-1
      if (istop.eq.0) go to 100
      do 40 i=1,istop
      deltmp(i)=deltmp(i+1)+delold(i)
   40 continue
      go to 20
c
c  diff(1) contains divided difference
c
  100 const=coef(iord)
      if ((method.eq.1).and.(iord.eq.2)) const=xtwelv
      del=trtol*tol/dmax1(abstol,const*dabs(diff(1)))
      if (iord.eq.1) go to 200
      if (iord.ge.3) go to 150
      del=dsqrt(del)
      go to 200
  150 del=dexp(dlog(del)/dfloat(iord))
  200 delnew=dmin1(delnew,del)
      return
      end
      subroutine sorupd
      implicit double precision (a-h,o-z)
c
c     this routine updates the independent voltage and current sources
c used in the circuit.  it also updates the ltd table (which contains
c previous (delayed) values of the sources used to model transmission
c lines).
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
      do 500 id=9,10
      loc=locate(id)
   10 if (loc.eq.0) go to 500
      locv=nodplc(loc+1)
      locp=nodplc(loc+5)
      itype=nodplc(loc+4)+1
      go to (490,100,200,300,400,450), itype
c
c  pulse source
c
  100 v1=value(locp+1)
      v2=value(locp+2)
      t1=value(locp+3)
      t2=value(locp+4)
      t3=value(locp+5)
      t4=value(locp+6)
      period=value(locp+7)
      time1=time
      if (time1.le.0.0d0) go to 160
  110 if (time1.lt.t1+period) go to 120
      time1=time1-period
      go to 110
  120 if (time1.lt.t4) go to 130
      value(locv+1)=v1
      go to 490
  130 if (time1.lt.t3) go to 140
      value(locv+1)=v2+(time1-t3)*(v1-v2)/(t4-t3)
      go to 490
  140 if (time1.lt.t2) go to 150
      value(locv+1)=v2
      go to 490
  150 if (time1.lt.t1) go to 160
      value(locv+1)=v1+(time1-t1)*(v2-v1)/(t2-t1)
      go to 490
  160 value(locv+1)=v1
      go to 490
c
c  sinusoidal source
c
  200 v1=value(locp+1)
      v2=value(locp+2)
      omeg=value(locp+3)
      t1=value(locp+4)
      theta=value(locp+5)
      time1=time-t1
      if (time1.gt.0.0d0) go to 210
      value(locv+1)=v1
      go to 490
  210 if (theta.ne.0.0d0) go to 220
      value(locv+1)=v1+v2*dsin(omeg*time1)
      go to 490
  220 value(locv+1)=v1+v2*dsin(omeg*time1)*dexp(-time1*theta)
      go to 490
c
c  exponential source
c
  300 v1=value(locp+1)
      v2=value(locp+2)
      t1=value(locp+3)
      tau1=value(locp+4)
      t2=value(locp+5)
      tau2=value(locp+6)
      time1=time
      if (time1.gt.t1) go to 310
      value(locv+1)=v1
      go to 490
  310 if (time1.gt.t2) go to 320
      value(locv+1)=v1+(v2-v1)*(1.0d0-dexp((t1-time1)/tau1))
      go to 490
  320 value(locv+1)=v1+(v2-v1)*(1.0d0-dexp((t1-time1)/tau1))
     1   +(v1-v2)*(1.0d0-dexp((t2-time1)/tau2))
      go to 490
c
c  piecewise-linear source
c
  400 t1=value(locp+1)
      v1=value(locp+2)
      t2=value(locp+3)
      v2=value(locp+4)
      iknt=4
  410 if (time.le.t2) go to 420
      t1=t2
      v1=v2
      t2=value(locp+iknt+1)
      v2=value(locp+iknt+2)
      iknt=iknt+2
      go to 410
  420 value(locv+1)=v1+((time-t1)/(t2-t1))*(v2-v1)
      go to 490
c
c  single-frequency fm
c
  450 v1=value(locp+1)
      v2=value(locp+2)
      omegc=value(locp+3)
      xmod=value(locp+4)
      omegs=value(locp+5)
      value(locv+1)=v1+v2*dsin(omegc*time+xmod*dsin(omegs*time))
  490 loc=nodplc(loc)
      go to 10
  500 continue
c
c  update transmission line sources
c
      if (jelcnt(17).eq.0) go to 1000
      if (mode.ne.2) go to 1000
      call sizmem(ltd,ltdsiz)
      numtd=ltdsiz/ntlin
      if (numtd.lt.3) go to 900
      loc=locate(17)
  610 if (loc.eq.0) go to 1000
      locv=nodplc(loc+1)
      td=value(locv+2)
      baktim=time-td
      if (baktim.lt.0.0d0) go to 640
      ltdptr=nodplc(loc+30)
      icntr=2
      l1=ltd
      l2=l1+ntlin
      l3=l2+ntlin
      t1=value(l1+1)
      t2=value(l2+1)
  620 t3=value(l3+1)
      icntr=icntr+1
      if (baktim.le.t3) go to 630
      if (icntr.eq.numtd) go to 900
      l1=l2
      l2=l3
      l3=l2+ntlin
      t1=t2
      t2=t3
      go to 620
  630 dt1t2=t1-t2
      dt1t3=t1-t3
      dt2t3=t2-t3
      tdnom1=1.0d0/(dt1t2*dt1t3)
      tdnom2=-1.0d0/(dt1t2*dt2t3)
      tdnom3=1.0d0/(dt2t3*dt1t3)
      dtt1=baktim-t1
      dtt2=baktim-t2
      dtt3=baktim-t3
      tfact1=dtt2*dtt3*tdnom1
      tfact2=dtt1*dtt3*tdnom2
      tfact3=dtt1*dtt2*tdnom3
      value(locv+3)=value(l1+ltdptr+0)*tfact1+value(l2+ltdptr+0)*tfact2
     1   +value(l3+ltdptr+0)*tfact3
      value(locv+4)=value(l1+ltdptr+1)*tfact1+value(l2+ltdptr+1)*tfact2
     1   +value(l3+ltdptr+1)*tfact3
  640 loc=nodplc(loc)
      go to 610
c
c  internal logic error:  less than 3 entries in ltd
c
  900 nogo=1
      write (6,901) numtd,icntr
  901 format('0*abort*:  internal spice error:  sorupd:  ',2i5/)
c
c  finished
c
 1000 return
      end
      subroutine iter8(itlim)
      implicit double precision (a-h,o-z)
c
c     this routine drives the newton-raphson iteration technique used to
c solve the set of nonlinear circuit equations.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
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
      igoof=0
      iterno=0
      ndrflo=0
      noncon=0
      ipass=0
c
c  construct linear equations and check convergence
c
   10 call load
      if ((mode.eq.1).and.(modedc.eq.2).and.(nosolv.ne.0)) go to 300
      iterno=iterno+1
      go to (20,30,40,50,50,50),initf
   20 if(mode.ne.1) go to 22
      call sizmem(nsnod,nic)
      if(nic.eq.0) go to 22
      if(ipass.ne.0) noncon=ipass
      ipass=0
   22 if(noncon.eq.0) go to 300
      go to 100
   30 initf=3
   40 if (noncon.eq.0) initf=1
      ipass=1
      go to 100
   50 initf=1
c
c  solve equations for next iteration
c
  100 if (iterno.ge.itlim) go to 200
  110 call dcdcmp
      call dcsol
  120 if (igoof.eq.0) go to 130
      ndrflo=ndrflo+1
      igoof=0
  130 value(lvn+1)=0.0d0
      ntemp=noncon
      noncon=0
      if (ntemp.gt.0) go to 150
      if (iterno.eq.1) go to 150
      do 140 i=2,numnod
      vold=value(lvnim1+i)
      vnew=value(lvn+i)
      tol=reltol*dmax1(dabs(vold),dabs(vnew))+vntol
      if (dabs(vold-vnew).le.tol) go to 140
      noncon=noncon+1
  140 continue
  150 call copy8(value(lvn+1),value(lvnim1+1),nstop)
      go to 10
c
c  no convergence
c
  200 igoof=1
  300 if (ndrflo.eq.0) go to 400
      write (6,301) ndrflo
  301 format('0warning:  underflow occurred ',i4,' time(s)')
c
c  finished
c
  400 return
      end
      subroutine load
      implicit double precision (a-h,o-z)
c
c     this routine zeroes-out and then loads the coefficient matrix.
c the active devices and the controlled sources are loaded by separate
c subroutines.
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
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension qcap(1),ccap(1)
      equivalence (qcap(1),value(1)),(ccap(1),value(2))
      dimension find(1),vind(1)
      equivalence (find(1),value(1)),(vind(1),value(2))
c
c  zero y matrix and current vector
c
      call zero8(value(lvn+1),nstop+nstop+nut+nlt)
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
   40 if (loc.eq.0) go to 100
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      lcoef=nodplc(loc+7)
      call sizmem(nodplc(loc+7),ncoef)
      loct=nodplc(loc+8)
      vcap=value(locv+2)
      if ((mode.eq.1).and.(initf.eq.2)) go to 45
      if ((nosolv.ne.0).and.(initf.eq.5)) go to 45
      vcap=value(lvnim1+node1)-value(lvnim1+node2)
   45 value(locv+3)=vcap
      if (mode.eq.1) go to 60
      if (initf.ne.6) go to 50
      qcap(lx0+loct)=qcap(lx1+loct)
      go to 60
   50 call evpoly(qcap(lx0+loct),-1,lcoef,ncoef,locv+2,1,loc+8)
      if (initf.ne.5) go to 60
      if (nosolv.eq.0) go to 55
      vcap=value(locv+2)
      value(locv+3)=vcap
      call evpoly(qcap(lx0+loct),-1,lcoef,ncoef,locv+2,1,loc+8)
   55 qcap(lx1+loct)=qcap(lx0+loct)
   60 call evpoly(value(locv+1),0,lcoef,ncoef,locv+2,1,loc+8)
      if (mode.eq.1) go to 90
      call intgr8(geq,ceq,value(locv+1),loct)
      ceq=ceq-geq*vcap+ag(1)*qcap(lx0+loct)
      if(initf.ne.5) go to 70
      ccap(lx1+loct)=ccap(lx0+loct)
   70 locy=lynl+nodplc(loc+10)
      value(locy)=value(locy)+geq
      locy=lynl+nodplc(loc+11)
      value(locy)=value(locy)+geq
      locy=lynl+nodplc(loc+5)
      value(locy)=value(locy)-geq
      locy=lynl+nodplc(loc+6)
      value(locy)=value(locy)-geq
      value(lvn+node1)=value(lvn+node1)-ceq
      value(lvn+node2)=value(lvn+node2)+ceq
   90 loc=nodplc(loc)
      go to 40
c
c  inductors
c
  100 if (jelcnt(3).eq.0) go to 400
      if (mode.eq.1) go to 150
      if (initf.eq.6) go to 150
      loc=locate(3)
  110 if (loc.eq.0) go to 120
      locv=nodplc(loc+1)
      iptr=nodplc(loc+5)
      lcoef=nodplc(loc+10)
      call sizmem(nodplc(loc+10),ncoef)
      loct=nodplc(loc+11)
      cind=value(lvnim1+iptr)
      if ((mode.eq.1).and.(initf.eq.2)) cind=value(locv+2)
      if ((initf.eq.5).and.(nosolv.ne.0)) cind=value(locv+2)
      value(locv+3)=cind
      call evpoly(find(lx0+loct),-1,lcoef,ncoef,locv+2,1,loc+11)
      loc=nodplc(loc)
      go to 110
  120 loc=locate(4)
  130 if (loc.eq.0) go to 150
      locv=nodplc(loc+1)
      nl1=nodplc(loc+2)
      nl2=nodplc(loc+3)
      iptr1=nodplc(nl1+5)
      iptr2=nodplc(nl2+5)
      loct1=nodplc(nl1+11)
      loct2=nodplc(nl2+11)
      find(lx0+loct1)=find(lx0+loct1)+value(locv+1)*value(lvnim1+iptr2)
      find(lx0+loct2)=find(lx0+loct2)+value(locv+1)*value(lvnim1+iptr1)
      loc=nodplc(loc)
      go to 130
  150 loc=locate(3)
  160 if (loc.eq.0) go to 300
      locv=nodplc(loc+1)
      iptr=nodplc(loc+5)
      lcoef=nodplc(loc+10)
      call sizmem(nodplc(loc+10),ncoef)
      loct=nodplc(loc+11)
      cind=value(lvnim1+iptr)
      if ((mode.eq.1).and.(initf.eq.2)) cind=value(locv+2)
      if ((nosolv.ne.0).and.(initf.eq.5)) cind=value(locv+2)
      value(locv+3)=cind
      if (mode.ne.1) go to 200
      veq=0.0d0
      req=0.0d0
      go to 210
  200 if (initf.ne.6) go to 205
      find(lx0+loct)=find(lx1+loct)
      go to 210
  205 if (initf.ne.5) go to 210
      find(lx1+loct)=find(lx0+loct)
  210 call evpoly(value(locv+1),0,lcoef,ncoef,locv+2,1,loc+11)
      if (mode.eq.1) go to 250
      call intgr8(req,veq,value(locv+1),loct)
      if (ncoef.eq.1) go to 250
      veq=veq-req*cind+ag(1)*find(lx0+loct)
  250 value(lvn+iptr)=veq
      if(initf.ne.5) go to 260
      vind(lx1+loct)=vind(lx0+loct)
  260 locy=lynl+nodplc(loc+13)
      value(locy)=-req
      locy=lynl+nodplc(loc+6)
      value(locy)=1.0d0
      locy=lynl+nodplc(loc+7)
      value(locy)=-1.0d0
      locy=lynl+nodplc(loc+8)
      value(locy)=1.0d0
      locy=lynl+nodplc(loc+9)
      value(locy)=-1.0d0
      loc=nodplc(loc)
      go to 160
c
c  mutual inductances
c
  300 loc=locate(4)
  310 if (loc.eq.0) go to 400
      locv=nodplc(loc+1)
      req=ag(1)*value(locv+1)
      locy=lynl+nodplc(loc+4)
      value(locy)=-req
      locy=lynl+nodplc(loc+5)
      value(locy)=-req
      loc=nodplc(loc)
      go to 310
c
c  nonlinear controlled sources
c
  400 call nlcsrc
c
c  voltage sources
c
      loc=locate(9)
  610 if (loc.eq.0) go to 700
      locv=nodplc(loc+1)
      iptr=nodplc(loc+6)
      value(lvn+iptr)=value(locv+1)
      locy=lynl+nodplc(loc+7)
      value(locy)=value(locy)+1.0d0
      locy=lynl+nodplc(loc+8)
      value(locy)=value(locy)-1.0d0
      locy=lynl+nodplc(loc+9)
      value(locy)=value(locy)+1.0d0
      locy=lynl+nodplc(loc+10)
      value(locy)=value(locy)-1.0d0
      loc=nodplc(loc)
      go to 610
c
c  current sources
c
  700 loc=locate(10)
  710 if (loc.eq.0) go to 800
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      val=value(locv+1)
      value(lvn+node1)=value(lvn+node1)-val
      value(lvn+node2)=value(lvn+node2)+val
      loc=nodplc(loc)
      go to 710
c
c  call device model routines
c
  800 call diode
      call bjt
      call jfet
      call mosfet
c
c  transmission lines
c
      loc=locate(17)
  910 if (loc.eq.0) go to 980
      locv=nodplc(loc+1)
      z0=value(locv+1)
      y0=1.0d0/z0
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      ibr1=nodplc(loc+8)
      ibr2=nodplc(loc+9)
      locy=lynl+nodplc(loc+10)
      value(locy)=value(locy)+y0
      locy=lynl+nodplc(loc+11)
      value(locy)=-y0
      locy=lynl+nodplc(loc+12)
      value(locy)=-1.0d0
      locy=lynl+nodplc(loc+13)
      value(locy)=value(locy)+y0
      locy=lynl+nodplc(loc+14)
      value(locy)=-1.0d0
      locy=lynl+nodplc(loc+15)
      value(locy)=-y0
      locy=lynl+nodplc(loc+16)
      value(locy)=+y0
      locy=lynl+nodplc(loc+17)
      value(locy)=+1.0d0
      locy=lynl+nodplc(loc+18)
      value(locy)=+y0
      locy=lynl+nodplc(loc+19)
      value(locy)=+1.0d0
      locy=lynl+nodplc(loc+20)
      value(locy)=-1.0d0
      locy=lynl+nodplc(loc+23)
      value(locy)=+1.0d0
      locy=lynl+nodplc(loc+27)
      value(locy)=-1.0d0
      locy=lynl+nodplc(loc+28)
      value(locy)=+1.0d0
      locy=lynl+nodplc(loc+31)
      value(locy)=-y0
      locy=lynl+nodplc(loc+32)
      value(locy)=-y0
      if (mode.ne.1) go to 920
      locy=lynl+nodplc(loc+21)
      value(locy)=-1.0d0
      locy=lynl+nodplc(loc+22)
      value(locy)=+1.0d0
      locy=lynl+nodplc(loc+24)
      value(locy)=-(1.0d0-gmin)*z0
      locy=lynl+nodplc(loc+25)
      value(locy)=-1.0d0
      locy=lynl+nodplc(loc+26)
      value(locy)=+1.0d0
      locy=lynl+nodplc(loc+29)
      value(locy)=-(1.0d0-gmin)*z0
      go to 950
  920 if (initf.ne.5) go to 930
      if (nosolv.ne.0) go to 925
      value(locv+3)=value(lvnim1+node3)-value(lvnim1+node4)
     1   +value(lvnim1+ibr2)*z0
      value(locv+4)=value(lvnim1+node1)-value(lvnim1+node2)
     1   +value(lvnim1+ibr1)*z0
      go to 930
  925 value(locv+3)=value(locv+7)+value(locv+8)*z0
      value(locv+4)=value(locv+5)+value(locv+6)*z0
  930 value(lvn+ibr1)=value(locv+3)
      value(lvn+ibr2)=value(locv+4)
  950 loc=nodplc(loc)
      go to 910
c
c  initialize nodes
c
  980 if(mode.ne.1) go to 995
      if(initf.ne.3.and.initf.ne.2) go to 1000
      call sizmem(nsnod,nic)
      if(nic.eq.0) go to 995
      call sizmem(icnod,ntest)
      if(modedc.eq.2.and.ntest.ne.0) go to 995
      g=1.0d0
      do 990 i=1,nic
      locy=lynl+nodplc(nsmat+i)
      value(locy)=value(locy)+g
      node=nodplc(nsnod+i)
      value(lvn+node)=value(lvn+node)+value(nsval+i)*g
  990 continue
c
c  transient initial conditions (uic not specified)
c
  995 if(mode.ne.1) go to 1000
      if(modedc.ne.2) go to 1000
      if(nosolv.ne.0) go to 1000
      call sizmem(icnod,nic)
      if(nic.eq.0) go to 1000
      g=1.0d0
      do 996 i=1,nic
      locy=lynl+nodplc(icmat+i)
      value(locy)=value(locy)+g
      node=nodplc(icnod+i)
      value(lvn+node)=value(lvn+node)+value(icval+i)*g
  996 continue
c
c  reorder right-hand side
c
 1000 do 1010 i=2,nstop
      j=nodplc(iswap+i)
      value(ndiag+i)=value(lvn+j)
 1010 continue
      call copy8(value(ndiag+1),value(lvn+1),nstop)
c
c  finished
c
      return
      end
      subroutine nlcsrc
      implicit double precision (a-h,o-z)
c
c     this routine loads the nonlinear controlled sources into the
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
c  nonlinear voltage-controlled current sources
c
      loc=locate(5)
   10 if (loc.eq.0) go to 100
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ndim=nodplc(loc+4)
      lnod=nodplc(loc+6)
      lmat=nodplc(loc+7)
      lcoef=nodplc(loc+8)
      call sizmem(nodplc(loc+8),ncoef)
      larg=nodplc(loc+9)
      lexp=nodplc(loc+10)
      lic=nodplc(loc+11)
      loct=nodplc(loc+12)+1
      icheck=0
      do 20 i=1,ndim
      call update(value(lic+i),loct,nodplc(lnod+1),nodplc(lnod+2),2,
     1   icheck)
      value(larg+i)=value(lx0+loct)
      loct=loct+2
      lnod=lnod+2
   20 continue
      call evpoly(cold,0,lcoef,ncoef,larg,ndim,lexp)
      loct=nodplc(loc+12)
      if (icheck.eq.1) go to 30
      if (initf.eq.6) go to 30
      tol=reltol*dmax1(dabs(cold),dabs(value(lx0+loct)))+abstol
      if (dabs(cold-value(lx0+loct)).lt.tol) go to 40
   30 noncon=noncon+1
   40 value(lx0+loct)=cold
      ceq=cold
      do 50 i=1,ndim
      call evpoly(geq,i,lcoef,ncoef,larg,ndim,lexp)
      loct=loct+2
      value(lx0+loct)=geq
      ceq=ceq-geq*value(larg+i)
      locy=lynl+nodplc(lmat+1)
      value(locy)=value(locy)+geq
      locy=lynl+nodplc(lmat+2)
      value(locy)=value(locy)-geq
      locy=lynl+nodplc(lmat+3)
      value(locy)=value(locy)-geq
      locy=lynl+nodplc(lmat+4)
      value(locy)=value(locy)+geq
      lmat=lmat+4
   50 continue
      value(lvn+node1)=value(lvn+node1)-ceq
      value(lvn+node2)=value(lvn+node2)+ceq
      loc=nodplc(loc)
      go to 10
c
c  nonlinear voltage controlled voltage sources
c
  100 loc=locate(6)
  110 if (loc.eq.0) go to 200
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ndim=nodplc(loc+4)
      iptr=nodplc(loc+6)
      lnod=nodplc(loc+7)
      lmat=nodplc(loc+8)
      lcoef=nodplc(loc+9)
      call sizmem(nodplc(loc+9),ncoef)
      larg=nodplc(loc+10)
      lexp=nodplc(loc+11)
      lic=nodplc(loc+12)
      loct=nodplc(loc+13)+2
      icheck=0
      do 120 i=1,ndim
      call update(value(lic+i),loct,nodplc(lnod+1),nodplc(lnod+2),2,
     1   icheck)
      value(larg+i)=value(lx0+loct)
      loct=loct+2
      lnod=lnod+2
  120 continue
      call evpoly(volt,0,lcoef,ncoef,larg,ndim,lexp)
      loct=nodplc(loc+13)
      if (icheck.eq.1) go to 130
      if (initf.eq.6) go to 130
      tol=reltol*dmax1(dabs(volt),dabs(value(lx0+loct)))+vntol
      if (dabs(volt-value(lx0+loct)).lt.tol) go to 140
  130 noncon=noncon+1
  140 value(lx0+loct)=volt
      value(lx0+loct+1)=value(lvnim1+iptr)
      veq=volt
      locy=lynl+nodplc(lmat+1)
      value(locy)=+1.0d0
      locy=lynl+nodplc(lmat+2)
      value(locy)=-1.0d0
      locy=lynl+nodplc(lmat+3)
      value(locy)=+1.0d0
      locy=lynl+nodplc(lmat+4)
      value(locy)=-1.0d0
      lmat=lmat+4
      loct=loct+1
      do 150 i=1,ndim
      call evpoly(vgain,i,lcoef,ncoef,larg,ndim,lexp)
      loct=loct+2
      value(lx0+loct)=vgain
      veq=veq-vgain*value(larg+i)
      locy=lynl+nodplc(lmat+1)
      value(locy)=value(locy)-vgain
      locy=lynl+nodplc(lmat+2)
      value(locy)=value(locy)+vgain
      lmat=lmat+2
  150 continue
      value(lvn+iptr)=veq
      loc=nodplc(loc)
      go to 110
c
c  nonlinear current-controlled current sources
c
  200 loc=locate(7)
  210 if (loc.eq.0) go to 300
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ndim=nodplc(loc+4)
      lvs=nodplc(loc+6)
      lmat=nodplc(loc+7)
      lcoef=nodplc(loc+8)
      call sizmem(nodplc(loc+8),ncoef)
      larg=nodplc(loc+9)
      lexp=nodplc(loc+10)
      lic=nodplc(loc+11)
      loct=nodplc(loc+12)+1
      icheck=0
      do 220 i=1,ndim
      iptr=nodplc(lvs+i)
      iptr=nodplc(iptr+6)
      call update(value(lic+i),loct,iptr,1,2,icheck)
      value(larg+i)=value(lx0+loct)
      loct=loct+2
  220 continue
      call evpoly(csrc,0,lcoef,ncoef,larg,ndim,lexp)
      loct=nodplc(loc+12)
      if (icheck.eq.1) go to 230
      if (initf.eq.6) go to 230
      tol=reltol*dmax1(dabs(csrc),dabs(value(lx0+loct)))+abstol
      if (dabs(csrc-value(lx0+loct)).lt.tol) go to 240
  230 noncon=noncon+1
  240 value(lx0+loct)=csrc
      ceq=csrc
      do 250 i=1,ndim
      call evpoly(cgain,i,lcoef,ncoef,larg,ndim,lexp)
      loct=loct+2
      value(lx0+loct)=cgain
      ceq=ceq-cgain*value(larg+i)
      locy=lynl+nodplc(lmat+1)
      value(locy)=value(locy)+cgain
      locy=lynl+nodplc(lmat+2)
      value(locy)=value(locy)-cgain
      lmat=lmat+2
  250 continue
      value(lvn+node1)=value(lvn+node1)-ceq
      value(lvn+node2)=value(lvn+node2)+ceq
      loc=nodplc(loc)
      go to 210
c
c  nonlinear current controlled voltage sources
c
  300 loc=locate(8)
  310 if (loc.eq.0) go to 1000
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      ndim=nodplc(loc+4)
      ibr=nodplc(loc+6)
      lvs=nodplc(loc+7)
      lmat=nodplc(loc+8)
      lcoef=nodplc(loc+9)
      call sizmem(nodplc(loc+9),ncoef)
      larg=nodplc(loc+10)
      lexp=nodplc(loc+11)
      lic=nodplc(loc+12)
      loct=nodplc(loc+13)+2
      icheck=0
      do 320 i=1,ndim
      iptr=nodplc(lvs+i)
      iptr=nodplc(iptr+6)
      call update(value(lic+i),loct,iptr,1,2,icheck)
      value(larg+i)=value(lx0+loct)
      loct=loct+2
  320 continue
      call evpoly(volt,0,lcoef,ncoef,larg,ndim,lexp)
      loct=nodplc(loc+13)
      if (icheck.eq.1) go to 330
      if (initf.eq.6) go to 330
      tol=reltol*dmax1(dabs(volt),dabs(value(lx0+loct)))+vntol
      if (dabs(volt-value(lx0+loct)).lt.tol) go to 340
  330 noncon=noncon+1
  340 value(lx0+loct)=volt
      value(lx0+loct+1)=value(lvnim1+ibr)
      veq=volt
      locy=lynl+nodplc(lmat+1)
      value(locy)=+1.0d0
      locy=lynl+nodplc(lmat+2)
      value(locy)=-1.0d0
      locy=lynl+nodplc(lmat+3)
      value(locy)=+1.0d0
      locy=lynl+nodplc(lmat+4)
      value(locy)=-1.0d0
      lmat=lmat+4
      loct=loct+1
      do 350 i=1,ndim
      call evpoly(transr,i,lcoef,ncoef,larg,ndim,lexp)
      loct=loct+2
      value(lx0+loct)=transr
      veq=veq-transr*value(larg+i)
      locy=lynl+nodplc(lmat+i)
      value(locy)=value(locy)-transr
  350 continue
      value(lvn+ibr)=veq
      loc=nodplc(loc)
      go to 310
c
c  finished
c
 1000 return
      end
      subroutine update(vinit,loct,node1,node2,nupda,icheck)
      implicit double precision (a-h,o-z)
c
c     this routine updates and limits the controlling variables for the
c nonlinear controlled sources.
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
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      go to (40,10,40,20,30,50), initf
   10 vnew=vinit
      go to 70
   20 vnew=value(lx0+loct)
      go to 70
   30 vnew=value(lx1+loct)
      go to 70
   40 vnew=value(lvnim1+node1)-value(lvnim1+node2)
      go to 60
   50 call copy8(value(lx1+loct),value(lx0+loct),nupda)
      xfact=delta/delold(2)
      vnew=(1.0d0+xfact)*value(lx1+loct)-xfact*value(lx2+loct)
   60 if (dabs(vnew).le.1.0d0) go to 80
      delv=vnew-value(lx0+loct)
      if (dabs(delv).le.0.1d0) go to 80
      vlim=dmax1(dabs(0.1d0*value(lx0+loct)),0.1d0)
      vnew=value(lx0+loct)+dsign(dmin1(dabs(delv),vlim),delv)
      go to 70
   70 icheck=1
   80 value(lx0+loct)=vnew
      return
      end
      subroutine evpoly(result,itype,lcoef,ncoef,larg,
     1  narg,lexp)
      implicit double precision (a-h,o-z)
c
c     this routine evaluates a polynomial.  lcoef points to the coef-
c ficients, and larg points to the values of the polynomial argument(s).
c
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      if (itype) 100,200,300
c
c  integration (polynomial *must* be one-dimensional)
c
  100 result=0.0d0
      arg=1.0d0
      arg1=value(larg+1)
      do 110 i=1,ncoef
      arg=arg*arg1
      result=result+value(lcoef+i)*arg/dfloat(i)
  110 continue
      go to 1000
c
c  evaluation of the polynomial
c
  200 result=value(lcoef+1)
      if (ncoef.eq.1) go to 1000
      call zero4(nodplc(lexp+1),narg)
      do 220 i=2,ncoef
      call nxtpwr(nodplc(lexp+1),narg)
      if (value(lcoef+i).eq.0.0d0) go to 220
      arg=1.0d0
      do 210 j=1,narg
      call evterm(val,value(larg+j),nodplc(lexp+j))
      arg=arg*val
  210 continue
      result=result+value(lcoef+i)*arg
  220 continue
      go to 1000
c
c  partial derivative with respect to the itype*th variable
c
  300 result=0.0d0
      if (ncoef.eq.1) go to 1000
      call zero4(nodplc(lexp+1),narg)
      do 330 i=2,ncoef
      call nxtpwr(nodplc(lexp+1),narg)
      if (nodplc(lexp+itype).eq.0) go to 330
      if (value(lcoef+i).eq.0.0d0) go to 330
      arg=1.0d0
      do 320 j=1,narg
      if (j.eq.itype) go to 310
      call evterm(val,value(larg+j),nodplc(lexp+j))
      arg=arg*val
      go to 320
  310 call evterm(val,value(larg+j),nodplc(lexp+j)-1)
      arg=arg*dfloat(nodplc(lexp+j))*val
  320 continue
      result=result+value(lcoef+i)*arg
  330 continue
c
c  finished
c
 1000 return
      end
      subroutine evterm(val,arg,iexp)
      implicit double precision (a-h,o-z)
c
c     this routine evaluates one term of a polynomial.
c
      jexp=iexp+1
      if (jexp.ge.6) go to 60
      go to (10,20,30,40,50), jexp
   10 val=1.0d0
      go to 100
   20 val=arg
      go to 100
   30 val=arg*arg
      go to 100
   40 val=arg*arg*arg
      go to 100
   50 val=arg*arg
      val=val*val
      go to 100
   60 if (arg.eq.0.0d0) go to 70
      argexp=dfloat(iexp)*dlog(dabs(arg))
      if (argexp.lt.-200.0d0) go to 70
      val=dexp(argexp)
      if((iexp/2)*2.eq.iexp) go to 100
      val=dsign(val,arg)
      go to 100
   70 val=0.0d0
c
c  finished
c
  100 return
      end
      subroutine nxtpwr(pwrseq,pdim)
      implicit double precision (a-h,o-z)
c
c     this routine determines the 'next' set of exponents for the
c different dimensions of a polynomial.
c
      integer pwrseq(1),pdim,psum
c
c
      if (pdim.eq.1) go to 80
      k=pdim
   10 if (pwrseq(k).ne.0) go to 20
      k=k-1
      if (k.ne.0) go to 10
      go to 80
   20 if (k.eq.pdim) go to 30
      pwrseq(k)=pwrseq(k)-1
      pwrseq(k+1)=pwrseq(k+1)+1
      go to 100
   30 km1=k-1
      do 40 i=1,km1
      if (pwrseq(i).ne.0) go to 50
   40 continue
      pwrseq(1)=pwrseq(pdim)+1
      pwrseq(pdim)=0
      go to 100
   50 psum=1
      k=pdim
   60 if (pwrseq(k-1).ge.1) go to 70
      psum=psum+pwrseq(k)
      pwrseq(k)=0
      k=k-1
      go to 60
   70 pwrseq(k)=pwrseq(k)+psum
      pwrseq(k-1)=pwrseq(k-1)-1
      go to 100
   80 pwrseq(1)=pwrseq(1)+1
c
c  finished
c
  100 return
      end
      subroutine intgr8(geq,ceq,capval,loct)
      implicit double precision (a-h,o-z)
c
c     this routine performs the actual numerical integration for each
c circuit element.
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
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension qcap(1),ccap(1)
      equivalence (qcap(1),value(1)),(ccap(1),value(2))
c
c
      if (method.eq.2) go to 100
c
c  trapezoidal algorithm
c
      if (iord.eq.1) go to 100
      ccap(lx0+loct)=-ccap(lx1+loct)*ag(2)
     1   +ag(1)*(qcap(lx0+loct)-qcap(lx1+loct))
      go to 190
c
c  gears algorithm
c
  100 go to (110,120,130,140,150,160), iord
  110 ccap(lx0+loct)=ag(1)*qcap(lx0+loct)+ag(2)*qcap(lx1+loct)
      go to 190
  120 ccap(lx0+loct)=ag(1)*qcap(lx0+loct)+ag(2)*qcap(lx1+loct)
     1              +ag(3)*qcap(lx2+loct)
      go to 190
  130 ccap(lx0+loct)=ag(1)*qcap(lx0+loct)+ag(2)*qcap(lx1+loct)
     1              +ag(3)*qcap(lx2+loct)+ag(4)*qcap(lx3+loct)
      go to 190
  140 ccap(lx0+loct)=ag(1)*qcap(lx0+loct)+ag(2)*qcap(lx1+loct)
     1              +ag(3)*qcap(lx2+loct)+ag(4)*qcap(lx3+loct)
     2              +ag(5)*qcap(lx4+loct)
      go to 190
  150 ccap(lx0+loct)=ag(1)*qcap(lx0+loct)+ag(2)*qcap(lx1+loct)
     1              +ag(3)*qcap(lx2+loct)+ag(4)*qcap(lx3+loct)
     2              +ag(5)*qcap(lx4+loct)+ag(6)*qcap(lx5+loct)
      go to 190
  160 ccap(lx0+loct)=ag(1)*qcap(lx0+loct)+ag(2)*qcap(lx1+loct)
     1              +ag(3)*qcap(lx2+loct)+ag(4)*qcap(lx3+loct)
     2              +ag(5)*qcap(lx4+loct)+ag(6)*qcap(lx5+loct)
     3              +ag(7)*qcap(lx6+loct)
c... ceq is the equivalent current applicable to linear capacitance
c    (inductance) only, i.e. q=c*v
  190 ceq=ccap(lx0+loct)-ag(1)*qcap(lx0+loct)
      geq=ag(1)*capval
      return
      end
      subroutine pnjlim(vnew,vold,vt,vcrit,icheck)
      implicit double precision (a-h,o-z)
c
c     this routine limits the change-per-iteration of device pn-junction
c voltages.
c
      if (vnew.le.vcrit) go to 30
      vlim=vt+vt
      delv=vnew-vold
      if (dabs(delv).le.vlim) go to 30
      if (vold.le.0.0d0) go to 20
      arg=1.0d0+delv/vt
      if (arg.le.0.0d0) go to 10
      vnew=vold+vt*dlog(arg)
      icheck=1
      go to 100
   10 vnew=vcrit
      icheck=1
      go to 100
   20 vnew=vt*dlog(vnew/vt)
      icheck=1
      go to 100
   30 continue
c
c  finished
c
  100 return
      end
      subroutine diode
      implicit double precision (a-h,o-z)
c
c     this routine processes diodes for dc and transient analyses.
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
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension vdo(1),cdo(1),gdo(1),qd(1),cqd(1)
      equivalence (vdo(1),value(1)),(cdo(1),value(2)),
     1   (gdo(1),value(3)),(qd(1),value(4)),(cqd(1),value(5))
c
c
      loc=locate(11)
   10 if (loc.eq.0) return
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      locm=nodplc(loc+5)
      ioff=nodplc(loc+6)
      locm=nodplc(locm+1)
      loct=nodplc(loc+11)
c
c  dc model parameters
c
      area=value(locv+1)
      csat=value(locm+1)*area
      gspr=value(locm+2)*area
      vte=value(locm+3)*vt
      bv=value(locm+13)
      vcrit=value(locm+18)
c
c  initialization
c
      icheck=1
      go to (100,20,30,50,60,70),initf
   20 if(mode.ne.1.or.modedc.ne.2.or.nosolv.eq.0) go to 25
      vd=value(locv+2)
      go to 300
   25 if(ioff.ne.0) go to 40
      vd=vcrit
      go to 300
   30 if (ioff.eq.0) go to 100
   40 vd=0.0d0
      go to 300
   50 vd=vdo(lx0+loct)
      go to 300
   60 vd=vdo(lx1+loct)
      go to 300
   70 xfact=delta/delold(2)
      vdo(lx0+loct)=vdo(lx1+loct)
      vd=(1.0d0+xfact)*vdo(lx1+loct)-xfact*vdo(lx2+loct)
      cdo(lx0+loct)=cdo(lx1+loct)
      gdo(lx0+loct)=gdo(lx1+loct)
      go to 110
c
c  compute new nonlinear branch voltage
c
  100 vd=value(lvnim1+node3)-value(lvnim1+node2)
  110 delvd=vd-vdo(lx0+loct)
      cdhat=cdo(lx0+loct)+gdo(lx0+loct)*delvd
c
c  bypass if solution has not changed
c
      if (6    .eq.6) go to 200
      tol=reltol*dmax1(dabs(vd),dabs(vdo(lx0+loct)))+vntol
      if (dabs(delvd).ge.tol) go to 200
      tol=reltol*dmax1(dabs(cdhat),dabs(cdo(lx0+loct)))+abstol
      if (dabs(cdhat-cdo(lx0+loct)).ge.tol) go to 200
      vd=vdo(lx0+loct)
      cd=cdo(lx0+loct)
      gd=gdo(lx0+loct)
      go to 800
c
c  limit new junction voltage
c
  200 vlim=vte+vte
      if(bv.eq.0.0d0) go to 205
      if (vd.lt.dmin1(0.0d0,-bv+10.0d0*vte)) go to 210
  205 icheck=0
      call pnjlim(vd,vdo(lx0+loct),vte,vcrit,icheck)
      go to 300
  210 vdtemp=-(vd+bv)
      call pnjlim(vdtemp,-(vdo(lx0+loct)+bv),vte,vcrit,icheck)
      vd=-(vdtemp+bv)
c
c  compute dc current and derivitives
c
  300 if (vd.lt.-5.0d0*vte) go to 310
      evd=dexp(vd/vte)
      cd=csat*(evd-1.0d0)+gmin*vd
      gd=csat*evd/vte+gmin
      go to 330
  310 if(bv.eq.0.0d0) go to 315
      if(vd.lt.-bv) go to 320
  315 gd=-csat/vd+gmin
      cd=gd*vd
      go to 330
  320 evrev=dexp(-(bv+vd)/vt)
      cd=-csat*(evrev-1.0d0+bv/vt)
      gd=csat*evrev/vt
  330 if (mode.ne.1) go to 500
      if ((modedc.eq.2).and.(nosolv.ne.0)) go to 500
      if (initf.eq.4) go to 500
      go to 700
c
c  charge storage elements
c
  500 tau=value(locm+4)
      czero=value(locm+5)*area
      pb=value(locm+6)
      xm=value(locm+7)
      fcpb=value(locm+12)
      if (vd.ge.fcpb) go to 510
      arg=1.0d0-vd/pb
      sarg=dexp(-xm*dlog(arg))
      qd(lx0+loct)=tau*cd+pb*czero*(1.0d0-arg*sarg)/(1.0d0-xm)
      capd=tau*gd+czero*sarg
      go to 520
  510 f1=value(locm+15)
      f2=value(locm+16)
      f3=value(locm+17)
      czof2=czero/f2
      qd(lx0+loct)=tau*cd+czero*f1+czof2*(f3*(vd-fcpb)
     1   +(xm/(pb+pb))*(vd*vd-fcpb*fcpb))
      capd=tau*gd+czof2*(f3+xm*vd/pb)
c
c  store small-signal parameters
c
  520 if ((mode.eq.1).and.(modedc.eq.2).and.(nosolv.ne.0)) go to 700
      if (initf.ne.4) go to 600
      value(lx0+loct+4)=capd
      go to 1000
c
c  transient analysis
c
  600 if (initf.ne.5) go to 610
      qd(lx1+loct)=qd(lx0+loct)
  610 call intgr8(geq,ceq,capd,loct+3)
      gd=gd+geq
      cd=cd+cqd(lx0+loct)
      if (initf.ne.5) go to 700
      cqd(lx1+loct)=cqd(lx0+loct)
c
c  check convergence
c
  700 if (initf.ne.3) go to 710
      if (ioff.eq.0) go to 710
      go to 750
  710 if (icheck.eq.1) go to 720
      tol=reltol*dmax1(dabs(cdhat),dabs(cd))+abstol
      if (dabs(cdhat-cd).le.tol) go to 750
  720 noncon=noncon+1
  750 vdo(lx0+loct)=vd
      cdo(lx0+loct)=cd
      gdo(lx0+loct)=gd
c
c  load current vector
c
  800 cdeq=cd-gd*vd
      value(lvn+node2)=value(lvn+node2)+cdeq
      value(lvn+node3)=value(lvn+node3)-cdeq
c
c  load matrix
c
      locy=lynl+nodplc(loc+13)
      value(locy)=value(locy)+gspr
      locy=lynl+nodplc(loc+14)
      value(locy)=value(locy)+gd
      locy=lynl+nodplc(loc+15)
      value(locy)=value(locy)+gd+gspr
      locy=lynl+nodplc(loc+7)
      value(locy)=value(locy)-gspr
      locy=lynl+nodplc(loc+8)
      value(locy)=value(locy)-gd
      locy=lynl+nodplc(loc+9)
      value(locy)=value(locy)-gspr
      locy=lynl+nodplc(loc+10)
      value(locy)=value(locy)-gd
 1000 loc=nodplc(loc)
      go to 10
      end
      subroutine bjt
      implicit double precision (a-h,o-z)
c
c     this routine processes bjts for dc and transient analyses.
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
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension vbeo(1),vbco(1),cco(1),cbo(1),gpio(1),gmuo(1),gmo(1),
     1   goo(1),qbe(1),cqbe(1),qbc(1),cqbc(1),qcs(1),cqcs(1),qbx(1),
     2   cqbx(1),gxo(1),cexbc(1)
      equivalence (vbeo(1),value(1)),(vbco(1),value(2)),
     1   (cco(1),value(3)),(cbo(1),value(4)),(gpio(1),value(5)),
     2   (gmuo(1),value(6)),(gmo(1),value(7)),(goo(1),value(8)),
     3   (qbe(1),value(9)),(cqbe(1),value(10)),(qbc(1),value(11)),
     4   (cqbc(1),value(12)),(qcs(1),value(13)),(cqcs(1),value(14)),
     5   (qbx(1),value(15)),(cqbx(1),value(16)),(gxo(1),value(17)),
     6   (cexbc(1),value(18))
c
c
      loc=locate(12)
   10 if (loc.eq.0) return
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      node6=nodplc(loc+7)
      node7=nodplc(loc+30)
      locm=nodplc(loc+8)
      ioff=nodplc(loc+9)
      type=nodplc(locm+2)
      locm=nodplc(locm+1)
      loct=nodplc(loc+22)
      gccs=0.0d0
      ceqcs=0.0d0
      geqbx=0.0d0
      ceqbx=0.0d0
      geqcb=0.0d0
c
c  dc model paramters
c
      area=value(locv+1)
      bfm=value(locm+2)
      brm=value(locm+8)
      csat=value(locm+1)*area
      rbpr=value(locm+18)/area
      rbpi=value(locm+16)/area-rbpr
      gcpr=value(locm+20)*area
      gepr=value(locm+19)*area
      ova=value(locm+4)
      ovb=value(locm+10)
      oik=value(locm+5)/area
      c2=value(locm+6)*area
      vte=value(locm+7)*vt
      oikr=value(locm+11)/area
      c4=value(locm+12)*area
      vtc=value(locm+13)*vt
      vcrit=value(locm+54)
      td=value(locm+28)
      xjrb=value(locm+17)*area
c
c  initialization
c
      icheck=1
      go to (100,20,30,50,60,70),initf
   20 if(mode.ne.1.or.modedc.ne.2.or.nosolv.eq.0) go to 25
      vbe=type*value(locv+2)
      vce=type*value(locv+3)
      vbc=vbe-vce
      vbx=vbc
      vcs=0.0d0
      go to 300
   25 if(ioff.ne.0) go to 40
      vbe=vcrit
      vbc=0.0d0
      go to 300
   30 if (ioff.eq.0) go to 100
   40 vbe=0.0d0
      vbc=0.0d0
      go to 300
   50 vbe=vbeo(lx0+loct)
      vbc=vbco(lx0+loct)
      vbx=type*(value(lvnim1+node2)-value(lvnim1+node4))
      vcs=type*(value(lvnim1+node7)-value(lvnim1+node4))
      go to 300
   60 vbe=vbeo(lx1+loct)
      vbc=vbco(lx1+loct)
      vbx=type*(value(lvnim1+node2)-value(lvnim1+node4))
      vcs=type*(value(lvnim1+node7)-value(lvnim1+node4))
      if(mode.ne.2.or.nosolv.eq.0) go to 300
      vbx=type*(value(locv+2)-value(locv+3))
      vcs=0.0d0
      go to 300
   70 xfact=delta/delold(2)
      vbeo(lx0+loct)=vbeo(lx1+loct)
      vbe=(1.0d0+xfact)*vbeo(lx1+loct)-xfact*vbeo(lx2+loct)
      vbco(lx0+loct)=vbco(lx1+loct)
      vbc=(1.0d0+xfact)*vbco(lx1+loct)-xfact*vbco(lx2+loct)
      cco(lx0+loct)=cco(lx1+loct)
      cbo(lx0+loct)=cbo(lx1+loct)
      gpio(lx0+loct)=gpio(lx1+loct)
      gmuo(lx0+loct)=gmuo(lx1+loct)
      gmo(lx0+loct)=gmo(lx1+loct)
      goo(lx0+loct)=goo(lx1+loct)
      go to 110
c
c  compute new nonlinear branch voltages
c
  100 vbe=type*(value(lvnim1+node5)-value(lvnim1+node6))
      vbc=type*(value(lvnim1+node5)-value(lvnim1+node4))
  110 delvbe=vbe-vbeo(lx0+loct)
      delvbc=vbc-vbco(lx0+loct)
      vbx=type*(value(lvnim1+node2)-value(lvnim1+node4))
      vcs=type*(value(lvnim1+node7)-value(lvnim1+node4))
      cchat=cco(lx0+loct)+(gmo(lx0+loct)+goo(lx0+loct))*delvbe
     1   -(goo(lx0+loct)+gmuo(lx0+loct))*delvbc
      cbhat=cbo(lx0+loct)+gpio(lx0+loct)*delvbe+gmuo(lx0+loct)*delvbc
c
c  limit nonlinear branch voltages
c
  200 icheck=0
      call pnjlim(vbe,vbeo(lx0+loct),vt,vcrit,icheck)
      call pnjlim(vbc,vbco(lx0+loct),vt,vcrit,icheck)
c
c  determine dc current and derivitives
c
  300 vtn=vt*value(locm+3)
      if(vbe.le.-5.0d0*vtn) go to 320
      evbe=dexp(vbe/vtn)
      cbe=csat*(evbe-1.0d0)+gmin*vbe
      gbe=csat*evbe/vtn+gmin
      if (c2.ne.0.0d0) go to 310
      cben=0.0d0
      gben=0.0d0
      go to 350
  310 evben=dexp(vbe/vte)
      cben=c2*(evben-1.0d0)
      gben=c2*evben/vte
      go to 350
  320 gbe=-csat/vbe+gmin
      cbe=gbe*vbe
      gben=-c2/vbe
      cben=gben*vbe
  350 vtn=vt*value(locm+9)
      if(vbc.le.-5.0d0*vtn) go to 370
      evbc=dexp(vbc/vtn)
      cbc=csat*(evbc-1.0d0)+gmin*vbc
      gbc=csat*evbc/vtn+gmin
      if (c4.ne.0.0d0) go to 360
      cbcn=0.0d0
      gbcn=0.0d0
      go to 400
  360 evbcn=dexp(vbc/vtc)
      cbcn=c4*(evbcn-1.0d0)
      gbcn=c4*evbcn/vtc
      go to 400
  370 gbc=-csat/vbc+gmin
      cbc=gbc*vbc
      gbcn=-c4/vbc
      cbcn=gbcn*vbc
c
c  determine base charge terms
c
  400 q1=1.0d0/(1.0d0-ova*vbc-ovb*vbe)
      if (oik.ne.0.0d0) go to 405
      if (oikr.ne.0.0d0) go to 405
      qb=q1
      dqbdve=q1*qb*ovb
      dqbdvc=q1*qb*ova
      go to 410
  405 q2=oik*cbe+oikr*cbc
      arg=dmax1(0.0d0,1.0d0+4.0d0*q2)
      sqarg=1.0d0
      if(arg.ne.0.0d0) sqarg=dsqrt(arg)
      qb=q1*(1.0d0+sqarg)/2.0d0
      dqbdve=q1*(qb*ovb+oik*gbe/sqarg)
      dqbdvc=q1*(qb*ova+oikr*gbc/sqarg)
c
c  weil's approx. for excess phase applied with backward-
c  euler integration
c
  410 cc=0.0d0
      cex=cbe
      gex=gbe
      if(mode.eq.1) go to 420
      if(td.eq.0.0d0) go to 420
      arg1=delta/td
      arg2=3.0d0*arg1
      arg1=arg2*arg1
      denom=1.0d0+arg1+arg2
      arg3=arg1/denom
      if(initf.ne.5) go to 411
      cexbc(lx1+loct)=cbe/qb
      cexbc(lx2+loct)=cexbc(lx1+loct)
  411 cc=(cexbc(lx1+loct)*(1.0d0+delta/delold(2)+arg2)
     1  -cexbc(lx2+loct)*delta/delold(2))/denom
      cex=cbe*arg3
      gex=gbe*arg3
      cexbc(lx0+loct)=cc+cex/qb
c
c  determine dc incremental conductances
c
  420 cc=cc+(cex-cbc)/qb-cbc/brm-cbcn
      cb=cbe/bfm+cben+cbc/brm+cbcn
      gx=rbpr+rbpi/qb
      if(xjrb.eq.0.0d0) go to 430
      arg1=dmax1(cb/xjrb,1.0d-9)
      arg2=(-1.0d0+dsqrt(1.0d0+14.59025d0*arg1))/2.4317d0/dsqrt(arg1)
      arg1=dtan(arg2)
      gx=rbpr+3.0d0*rbpi*(arg1-arg2)/arg2/arg1/arg1
  430 if(gx.ne.0.0d0) gx=1.0d0/gx
      gpi=gbe/bfm+gben
      gmu=gbc/brm+gbcn
      go=(gbc+(cex-cbc)*dqbdvc/qb)/qb
      gm=(gex-(cex-cbc)*dqbdve/qb)/qb-go
      if (mode.ne.1) go to 500
      if ((modedc.eq.2).and.(nosolv.ne.0)) go to 500
      if (initf.eq.4) go to 500
      go to 700
c
c  charge storage elements
c
  500 tf=value(locm+24)
      tr=value(locm+33)
      czbe=value(locm+21)*area
      pe=value(locm+22)
      xme=value(locm+23)
      cdis=value(locm+32)
      ctot=value(locm+29)*area
      czbc=ctot*cdis
      czbx=ctot-czbc
      pc=value(locm+30)
      xmc=value(locm+31)
      fcpe=value(locm+46)
      czcs=value(locm+38)*area
      ps=value(locm+39)
      xms=value(locm+40)
      xtf=value(locm+25)
      ovtf=value(locm+26)
      xjtf=value(locm+27)*area
      if(tf.eq.0.0d0) go to 505
      if(vbe.le.0.0d0) go to 505
      argtf=0.0d0
      arg2=0.0d0
      arg3=0.0d0
      if(xtf.eq.0.0d0) go to 504
      argtf=xtf
      if(ovtf.ne.0.0d0) argtf=argtf*dexp(vbc*ovtf)
      arg2=argtf
      if(xjtf.eq.0.0d0) go to 503
      temp=cbe/(cbe+xjtf)
      argtf=argtf*temp*temp
      arg2=argtf*(3.0d0-temp-temp)
  503 arg3=cbe*argtf*ovtf
  504 cbe=cbe*(1.0d0+argtf)/qb
      gbe=(gbe*(1.0d0+arg2)-cbe*dqbdve)/qb
      geqcb=tf*(arg3-cbe*dqbdvc)/qb
  505 if (vbe.ge.fcpe) go to 510
      arg=1.0d0-vbe/pe
      sarg=dexp(-xme*dlog(arg))
      qbe(lx0+loct)=tf*cbe+pe*czbe*(1.0d0-arg*sarg)/(1.0d0-xme)
      capbe=tf*gbe+czbe*sarg
      go to 520
  510 f1=value(locm+47)
      f2=value(locm+48)
      f3=value(locm+49)
      czbef2=czbe/f2
      qbe(lx0+loct)=tf*cbe+czbe*f1+czbef2*(f3*(vbe-fcpe)
     1   +(xme/(pe+pe))*(vbe*vbe-fcpe*fcpe))
      capbe=tf*gbe+czbef2*(f3+xme*vbe/pe)
  520 fcpc=value(locm+50)
      f1=value(locm+51)
      f2=value(locm+52)
      f3=value(locm+53)
      if (vbc.ge.fcpc) go to 530
      arg=1.0d0-vbc/pc
      sarg=dexp(-xmc*dlog(arg))
      qbc(lx0+loct)=tr*cbc+pc*czbc*(1.0d0-arg*sarg)/(1.0d0-xmc)
      capbc=tr*gbc+czbc*sarg
      go to 540
  530 czbcf2=czbc/f2
      qbc(lx0+loct)=tr*cbc+czbc*f1+czbcf2*(f3*(vbc-fcpc)
     1   +(xmc/(pc+pc))*(vbc*vbc-fcpc*fcpc))
      capbc=tr*gbc+czbcf2*(f3+xmc*vbc/pc)
  540 if(vbx.ge.fcpc) go to 550
      arg=1.0d0-vbx/pc
      sarg=dexp(-xmc*dlog(arg))
      qbx(lx0+loct)=pc*czbx*(1.0d0-arg*sarg)/(1.0d0-xmc)
      capbx=czbx*sarg
      go to 560
  550 czbxf2=czbx/f2
      qbx(lx0+loct)=czbx*f1+czbxf2*(f3*(vbx-fcpc)+(xmc/(pc+pc))*
     1   (vbx*vbx-fcpc*fcpc))
      capbx=czbxf2*(f3+xmc*vbx/pc)
  560 if(vcs.ge.0.0d0) go to 570
      arg=1.0d0-vcs/ps
      sarg=dexp(-xms*dlog(arg))
      qcs(lx0+loct)=ps*czcs*(1.0d0-arg*sarg)/(1.0d0-xms)
      capcs=czcs*sarg
      go to 580
  570 qcs(lx0+loct)=vcs*czcs*(1.0d0+xms*vcs/(2.0d0*ps))
      capcs=czcs*(1.0d0+xms*vcs/ps)
c
c  store small-signal parameters
c
  580 if ((mode.eq.1).and.(modedc.eq.2).and.(nosolv.ne.0)) go to 700
      if (initf.ne.4) go to 600
      value(lx0+loct+9)=capbe
      value(lx0+loct+11)=capbc
      value(lx0+loct+13)=capcs
      value(lx0+loct+15)=capbx
      value(lx0+loct+17)=geqcb
      go to 1000
c
c  transient analysis
c
  600 if (initf.ne.5) go to 610
      qbe(lx1+loct)=qbe(lx0+loct)
      qbc(lx1+loct)=qbc(lx0+loct)
      qbx(lx1+loct)=qbx(lx0+loct)
      qcs(lx1+loct)=qcs(lx0+loct)
  610 call intgr8(geq,ceq,capbe,loct+8)
      geqcb=geqcb*ag(1)
      gpi=gpi+geq
      cb=cb+cqbe(lx0+loct)
      call intgr8(geq,ceq,capbc,loct+10)
      gmu=gmu+geq
      cb=cb+cqbc(lx0+loct)
      cc=cc-cqbc(lx0+loct)
      call intgr8(gccs,ceq,capcs,loct+12)
      ceqcs=type*(cqcs(lx0+loct)-vcs*gccs)
      call intgr8(geqbx,ceq,capbx,loct+14)
      ceqbx=type*(cqbx(lx0+loct)-vbx*geqbx)
      if (initf.ne.5) go to 700
      cqbe(lx1+loct)=cqbe(lx0+loct)
      cqbc(lx1+loct)=cqbc(lx0+loct)
      cqbx(lx1+loct)=cqbx(lx0+loct)
      cqcs(lx1+loct)=cqcs(lx0+loct)
c
c  check convergence
c
  700 if (initf.ne.3) go to 710
      if (ioff.eq.0) go to 710
      go to 750
  710 if (icheck.eq.1) go to 720
      tol=reltol*dmax1(dabs(cchat),dabs(cc))+abstol
      if (dabs(cchat-cc).gt.tol) go to 720
      tol=reltol*dmax1(dabs(cbhat),dabs(cb))+abstol
      if (dabs(cbhat-cb).le.tol) go to 750
  720 noncon=noncon+1
  750 vbeo(lx0+loct)=vbe
      vbco(lx0+loct)=vbc
      cco(lx0+loct)=cc
      cbo(lx0+loct)=cb
      gpio(lx0+loct)=gpi
      gmuo(lx0+loct)=gmu
      gmo(lx0+loct)=gm
      goo(lx0+loct)=go
      gxo(lx0+loct)=gx
c
c  load current excitation vector
c
  900 ceqbe=type*(cc+cb-vbe*(gm+go+gpi)+vbc*(go-geqcb))
      ceqbc=type*(-cc+vbe*(gm+go)-vbc*(gmu+go))
      value(lvn+node2)=value(lvn+node2)-ceqbx
      value(lvn+node4)=value(lvn+node4)+ceqcs+ceqbx+ceqbc
      value(lvn+node5)=value(lvn+node5)-ceqbe-ceqbc
      value(lvn+node6)=value(lvn+node6)+ceqbe
      value(lvn+node7)=value(lvn+node7)-ceqcs
c
c  load y matrix
c
      locy=lynl+nodplc(loc+24)
      value(locy)=value(locy)+gcpr
      locy=lynl+nodplc(loc+25)
      value(locy)=value(locy)+gx+geqbx
      locy=lynl+nodplc(loc+26)
      value(locy)=value(locy)+gepr
      locy=lynl+nodplc(loc+27)
      value(locy)=value(locy)+gmu+go+gcpr+gccs+geqbx
      locy=lynl+nodplc(loc+28)
      value(locy)=value(locy)+gx  +gpi+gmu+geqcb
      locy=lynl+nodplc(loc+29)
      value(locy)=value(locy)+gpi+gepr+gm+go
      locy=lynl+nodplc(loc+10)
      value(locy)=value(locy)-gcpr
      locy=lynl+nodplc(loc+11)
      value(locy)=value(locy)-gx
      locy=lynl+nodplc(loc+12)
      value(locy)=value(locy)-gepr
      locy=lynl+nodplc(loc+13)
      value(locy)=value(locy)-gcpr
      locy=lynl+nodplc(loc+14)
      value(locy)=value(locy)-gmu+gm
      locy=lynl+nodplc(loc+15)
      value(locy)=value(locy)-gm-go
      locy=lynl+nodplc(loc+16)
      value(locy)=value(locy)-gx
      locy=lynl+nodplc(loc+17)
      value(locy)=value(locy)-gmu-geqcb
      locy=lynl+nodplc(loc+18)
      value(locy)=value(locy)-gpi
      locy=lynl+nodplc(loc+19)
      value(locy)=value(locy)-gepr
      locy=lynl+nodplc(loc+20)
      value(locy)=value(locy)-go+geqcb
      locy=lynl+nodplc(loc+21)
      value(locy)=value(locy)-gpi-gm-geqcb
      locy=lynl+nodplc(loc+31)
      value(locy)=value(locy)+gccs
      locy=lynl+nodplc(loc+32)
      value(locy)=value(locy)-gccs
      locy=lynl+nodplc(loc+33)
      value(locy)=value(locy)-gccs
      locy=lynl+nodplc(loc+34)
      value(locy)=value(locy)-geqbx
      locy=lynl+nodplc(loc+35)
      value(locy)=value(locy)-geqbx
 1000 loc=nodplc(loc)
      go to 10
      end
      subroutine fetlim(vnew,vold,vto,icheck)
c     
c     *** fetlim is not used in this version ***
c     *   if problems arrise with the conver-  *
c     *   gence of MOSFET circuit it should be *
c     *   re-installed.   R.Newton.            *
c     ***                                    ***
c
      return
      end
      subroutine jfet
      implicit double precision (a-h,o-z)
c
c     this routine processes jfets for dc and transient analyses.
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
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension vgso(1),vgdo(1),cgo(1),cdo(1),cgdo(1),gmo(1),gdso(1),
     1   ggso(1),ggdo(1),qgs(1),cqgs(1),qgd(1),cqgd(1)
      equivalence (vgso(1),value( 1)),(vgdo(1),value( 2)),
     1            (cgo (1),value( 3)),(cdo (1),value( 4)),
     2            (cgdo(1),value( 5)),(gmo (1),value( 6)),
     3            (gdso(1),value( 7)),(ggso(1),value( 8)),
     4            (ggdo(1),value( 9)),(qgs (1),value(10)),
     5            (cqgs(1),value(11)),(qgd (1),value(12)),
     6            (cqgd(1),value(13))
c
c
      loc=locate(13)
   10 if (loc.eq.0) return
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      locm=nodplc(loc+7)
      ioff=nodplc(loc+8)
      type=nodplc(locm+2)
      locm=nodplc(locm+1)
      loct=nodplc(loc+19)
c
c  dc model parameters
c
      area=value(locv+1)
      vto=value(locm+1)
      beta=value(locm+2)*area
      xlamb=value(locm+3)
      gdpr=value(locm+4)*area
      gspr=value(locm+5)*area
      csat=value(locm+9)*area
      vcrit=value(locm+16)
c
c  initialization
c
      icheck=1
      go to (100,20,30,50,60,70), initf
   20 if(mode.ne.1.or.modedc.ne.2.or.nosolv.eq.0) go to 25
      vds=type*value(locv+2)
      vgs=type*value(locv+3)
      vgd=vgs-vds
      go to 300
   25 if(ioff.ne.0) go to 40
      vgs=-1.0d0
      vgd=-1.0d0
      go to 300
   30 if (ioff.eq.0) go to 100
   40 vgs=0.0d0
      vgd=0.0d0
      go to 300
   50 vgs=vgso(lx0+loct)
      vgd=vgdo(lx0+loct)
      go to 300
   60 vgs=vgso(lx1+loct)
      vgd=vgdo(lx1+loct)
      go to 300
   70 xfact=delta/delold(2)
      vgso(lx0+loct)=vgso(lx1+loct)
      vgs=(1.0d0+xfact)*vgso(lx1+loct)-xfact*vgso(lx2+loct)
      vgdo(lx0+loct)=vgdo(lx1+loct)
      vgd=(1.0d0+xfact)*vgdo(lx1+loct)-xfact*vgdo(lx2+loct)
      cgo(lx0+loct)=cgo(lx1+loct)
      cdo(lx0+loct)=cdo(lx1+loct)
      cgdo(lx0+loct)=cgdo(lx1+loct)
      gmo(lx0+loct)=gmo(lx1+loct)
      gdso(lx0+loct)=gdso(lx1+loct)
      ggso(lx0+loct)=ggso(lx1+loct)
      ggdo(lx0+loct)=ggdo(lx1+loct)
      go to 110
c
c  compute new nonlinear branch voltages
c
  100 vgs=type*(value(lvnim1+node2)-value(lvnim1+node5))
      vgd=type*(value(lvnim1+node2)-value(lvnim1+node4))
  110 delvgs=vgs-vgso(lx0+loct)
      delvgd=vgd-vgdo(lx0+loct)
      delvds=delvgs-delvgd
      cghat=cgo(lx0+loct)+ggdo(lx0+loct)*delvgd+ggso(lx0+loct)*delvgs
      cdhat=cdo(lx0+loct)+gmo(lx0+loct)*delvgs+gdso(lx0+loct)*delvds
     1   -ggdo(lx0+loct)*delvgd
c
c  bypass if solution has not changed
c
      if (initf.eq.6) go to 200
      tol=reltol*dmax1(dabs(vgs),dabs(vgso(lx0+loct)))+vntol
      if (dabs(delvgs).ge.tol) go to 200
      tol=reltol*dmax1(dabs(vgd),dabs(vgdo(lx0+loct)))+vntol
      if (dabs(delvgd).ge.tol) go to 200
      tol=reltol*dmax1(dabs(cghat),dabs(cgo(lx0+loct)))+abstol
      if (dabs(cghat-cgo(lx0+loct)).ge.tol) go to 200
      tol=reltol*dmax1(dabs(cdhat),dabs(cdo(lx0+loct)))+abstol
      if (dabs(cdhat-cdo(lx0+loct)).ge.tol) go to 200
      vgs=vgso(lx0+loct)
      vgd=vgdo(lx0+loct)
      vds=vgs-vgd
      cg=cgo(lx0+loct)
      cd=cdo(lx0+loct)
      cgd=cgdo(lx0+loct)
      gm=gmo(lx0+loct)
      gds=gdso(lx0+loct)
      ggs=ggso(lx0+loct)
      ggd=ggdo(lx0+loct)
      go to 900
c
c  limit nonlinear branch voltages
c
  200 icheck=0
      call pnjlim(vgs,vgso(lx0+loct),vt,vcrit,icheck)
      call pnjlim(vgd,vgdo(lx0+loct),vt,vcrit,icheck)
      call fetlim(vgs,vgso(lx0+loct),vto,icheck)
      call fetlim(vgd,vgdo(lx0+loct),vto,icheck)
c
c  determine dc current and derivatives
c
  300 vds=vgs-vgd
      if (vgs.gt.-5.0d0*vt) go to 310
      ggs=-csat/vgs+gmin
      cg=ggs*vgs
      go to 320
  310 evgs=dexp(vgs/vt)
      ggs=csat*evgs/vt+gmin
      cg=csat*(evgs-1.0d0)+gmin*vgs
  320 if (vgd.gt.-5.0d0*vt) go to 330
      ggd=-csat/vgd+gmin
      cgd=ggd*vgd
      go to 340
  330 evgd=dexp(vgd/vt)
      ggd=csat*evgd/vt+gmin
      cgd=csat*(evgd-1.0d0)+gmin*vgd
  340 cg=cg+cgd
c
c  compute drain current and derivitives for normal mode
c
  400 if (vds.lt.0.0d0) go to 450
      vgst=vgs-vto
c
c  normal mode, cutoff region
c
      if (vgst.gt.0.0d0) go to 410
      cdrain=0.0d0
      gm=0.0d0
      gds=0.0d0
      go to 490
c
c  normal mode, saturation region
c
  410 betap=beta*(1.0d0+xlamb*vds)
      twob=betap+betap
      if (vgst.gt.vds) go to 420
      cdrain=betap*vgst*vgst
      gm=twob*vgst
      gds=xlamb*beta*vgst*vgst
      go to 490
c
c  normal mode, linear region
c
  420 cdrain=betap*vds*(vgst+vgst-vds)
      gm=twob*vds
      gds=twob*(vgst-vds)+xlamb*beta*vds*(vgst+vgst-vds)
      go to 490
c
c  compute drain current and derivitives for inverse mode
c
  450 vgdt=vgd-vto
c
c  inverse mode, cutoff region
c
      if (vgdt.gt.0.0d0) go to 460
      cdrain=0.0d0
      gm=0.0d0
      gds=0.0d0
      go to 490
c
c  inverse mode, saturation region
c
  460 betap=beta*(1.0d0-xlamb*vds)
      twob=betap+betap
      if (vgdt.gt.-vds) go to 470
      cdrain=-betap*vgdt*vgdt
      gm=-twob*vgdt
      gds=xlamb*beta*vgdt*vgdt-gm
      go to 490
c
c  inverse mode, linear region
c
  470 cdrain=betap*vds*(vgdt+vgdt+vds)
      gm=twob*vds
      gds=twob*vgdt-xlamb*beta*vds*(vgdt+vgdt+vds)
c
c  compute equivalent drain current source
c
  490 cd=cdrain-cgd
      if (mode.ne.1) go to 500
      if ((modedc.eq.2).and.(nosolv.ne.0)) go to 500
      if (initf.eq.4) go to 500
      go to 700
c
c  charge storage elements
c
  500 czgs=value(locm+6)*area
      czgd=value(locm+7)*area
      phib=value(locm+8)
      twop=phib+phib
      fcpb=value(locm+12)
      fcpb2=fcpb*fcpb
      f1=value(locm+13)
      f2=value(locm+14)
      f3=value(locm+15)
      czgsf2=czgs/f2
      czgdf2=czgd/f2
      if (vgs.ge.fcpb) go to 510
      sarg=dsqrt(1.0d0-vgs/phib)
      qgs(lx0+loct)=twop*czgs*(1.0d0-sarg)
      capgs=czgs/sarg
      go to 520
  510 qgs(lx0+loct)=czgs*f1+czgsf2*(f3*(vgs-fcpb)
     1   +(vgs*vgs-fcpb2)/(twop+twop))
      capgs=czgsf2*(f3+vgs/twop)
  520 if (vgd.ge.fcpb) go to 530
      sarg=dsqrt(1.0d0-vgd/phib)
      qgd(lx0+loct)=twop*czgd*(1.0d0-sarg)
      capgd=czgd/sarg
      go to 560
  530 qgd(lx0+loct)=czgd*f1+czgdf2*(f3*(vgd-fcpb)
     1   +(vgd*vgd-fcpb2)/(twop+twop))
      capgd=czgdf2*(f3+vgd/twop)
c
c  store small-signal parameters
c
  560 if ((mode.eq.1).and.(modedc.eq.2).and.(nosolv.ne.0)) go to 700
      if (initf.ne.4) go to 600
      value(lx0+loct+9)=capgs
      value(lx0+loct+11)=capgd
      go to 1000
c
c  transient analysis
c
  600 if (initf.ne.5) go to 610
      qgs(lx1+loct)=qgs(lx0+loct)
      qgd(lx1+loct)=qgd(lx0+loct)
  610 call intgr8(geq,ceq,capgs,loct+9)
      ggs=ggs+geq
      cg=cg+cqgs(lx0+loct)
      call intgr8(geq,ceq,capgd,loct+11)
      ggd=ggd+geq
      cg=cg+cqgd(lx0+loct)
      cd=cd-cqgd(lx0+loct)
      cgd=cgd+cqgd(lx0+loct)
      if (initf.ne.5) go to 700
      cqgs(lx1+loct)=cqgs(lx0+loct)
      cqgd(lx1+loct)=cqgd(lx0+loct)
c
c  check convergence
c
  700 if (initf.ne.3) go to 710
      if (ioff.eq.0) go to 710
      go to 750
  710 if (icheck.eq.1) go to 720
      tol=reltol*dmax1(dabs(cghat),dabs(cg))+abstol
      if (dabs(cghat-cg).ge.tol) go to 720
      tol=reltol*dmax1(dabs(cdhat),dabs(cd))+abstol
      if (dabs(cdhat-cd).le.tol) go to 750
  720 noncon=noncon+1
  750 vgso(lx0+loct)=vgs
      vgdo(lx0+loct)=vgd
      cgo(lx0+loct)=cg
      cdo(lx0+loct)=cd
      cgdo(lx0+loct)=cgd
      gmo(lx0+loct)=gm
      gdso(lx0+loct)=gds
      ggso(lx0+loct)=ggs
      ggdo(lx0+loct)=ggd
c
c  load current vector
c
  900 ceqgd=type*(cgd-ggd*vgd)
      ceqgs=type*((cg-cgd)-ggs*vgs)
      cdreq=type*((cd+cgd)-gds*vds-gm*vgs)
      value(lvn+node2)=value(lvn+node2)-ceqgs-ceqgd
      value(lvn+node4)=value(lvn+node4)-cdreq+ceqgd
      value(lvn+node5)=value(lvn+node5)+cdreq+ceqgs
c
c  load y matrix
c
      locy=lynl+nodplc(loc+20)
      value(locy)=value(locy)+gdpr
      locy=lynl+nodplc(loc+21)
      value(locy)=value(locy)+ggd+ggs
      locy=lynl+nodplc(loc+22)
      value(locy)=value(locy)+gspr
      locy=lynl+nodplc(loc+23)
      value(locy)=value(locy)+gdpr+gds+ggd
      locy=lynl+nodplc(loc+24)
      value(locy)=value(locy)+gspr+gds+gm+ggs
      locy=lynl+nodplc(loc+9)
      value(locy)=value(locy)-gdpr
      locy=lynl+nodplc(loc+10)
      value(locy)=value(locy)-ggd
      locy=lynl+nodplc(loc+11)
      value(locy)=value(locy)-ggs
      locy=lynl+nodplc(loc+12)
      value(locy)=value(locy)-gspr
      locy=lynl+nodplc(loc+13)
      value(locy)=value(locy)-gdpr
      locy=lynl+nodplc(loc+14)
      value(locy)=value(locy)+gm-ggd
      locy=lynl+nodplc(loc+15)
      value(locy)=value(locy)-gds-gm
      locy=lynl+nodplc(loc+16)
      value(locy)=value(locy)-ggs-gm
      locy=lynl+nodplc(loc+17)
      value(locy)=value(locy)-gspr
      locy=lynl+nodplc(loc+18)
      value(locy)=value(locy)-gds
 1000 loc=nodplc(loc)
      go to 10
      end
      subroutine mosfet
      implicit double precision (a-h,o-z)
c
c     this routine processes mosfets for dc and transient analyses.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /cirdat/ locate(50),jelcnt(50),nunods,ncnods,numnod,nstop,
     1   nut,nlt,nxtrm,ndist,ntlin,ibr,numvs
      common /mosarg/ gamma,beta,vto,phi,cox,vbi,xnfs,xnsub,xd,xj,xl,
     1   xlamda,utra,uexp,vbp,von,vdsat,theta,vcrit,vtra,gleff,cdrain
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension vbdo(1),vbso(1),vgbo(1),gdgo(1),cdo(1),cbo(1),
     1   gddo(1),gdso(1),gbgo(1),gbdo(1),gbso(1),qb(1),cqb(1),qg(1),
     2   cqg(1),qd(1),cqd(1)
c.. note: for direct indexing with 'value', use, e.g. loct+2 to find vgbo
      equivalence (vbdo (1),value( 1)),(vbso (1),value( 2)),
     1            (vgbo (1),value( 3)),
     2            (cdo  (1),value( 5)),(cbo  (1),value( 6)),
     3            (gddo (1),value( 7)),(gdgo (1),value(8)),
     4            (gdso (1),value( 9)),(gbgo (1),value(10)),
     5            (gbdo (1),value(11)),(gbso (1),value(12)),
     6            (qb   (1),value(13)),(cqb  (1),value(14)),
     7            (qg   (1),value(15)),(cqg  (1),value(16)),
     8            (qd   (1),value(17)),(cqd  (1),value(18))
c
c
      loc=locate(14)
   10 if (loc.eq.0) return
      locm=nodplc(loc+8)
      if(nodplc(locm+2).ne.0) go to 15
      call gasfet(loc)
      go to 1000
   15 locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      node6=nodplc(loc+7)
      ioff=nodplc(loc+9)
      type=nodplc(locm+2)
      locm=nodplc(locm+1)
      loct=nodplc(loc+26)
c
c  dc model parameters
c
      xj=value(locm+19)
      xl=value(locv+1)-2.0d0*value(locm+20)
      xw=value(locv+2)-2.0d0*value(locm+36)
      devmod=value(locv+8)
      vto=type*value(locm+1)
      beta=value(locm+2)*xw/xl
      gamma=value(locm+3)
      phi=value(locm+4)
      xlamda=value(locm+5)
      csat=value(locm+15)
      ad=value(locv+3)
      as=value(locv+4)
      cdsat=csat*ad
      cssat=csat*as
      gdpr=value(locv+11)
      gspr=value(locv+12)
      covlgs=value(locm+8)*xw
      covlgd=value(locm+9)*xw
      covlgb=value(locm+10)*xl
      cox=value(locm+13)
      xnsub=value(locm+16)
      xnfs=value(locm+18)
      vbp=value(locm+24)
      uexp=value(locm+25)
      utra=value(locm+26)
      vbi=type*value(locm+34)
      xd=value(locm+35)
      vcrit=value(locm+37)*xl
      vtra=value(locm+38)*xl
      theta=value(locm+39)
      gleff=value(locm+40)
c
c  initialization
c
      icheck=1
      go to (100,20,30,50,60,70), initf
   20 if(mode.ne.1.or.modedc.ne.2.or.nosolv.eq.0) go to 25
      vbs=type*value(locv+7)
      vbd=type*value(locv+5)-vbs
      vgb=type*value(locv+6)-vbs
      go to 300
   25 if(ioff.ne.0) go to 40
      vbs=0.0d0
      vbd=0.0d0
      vgb=vto
      go to 300
   30 if (ioff.eq.0) go to 100
   40 vbs=0.0d0
      vbd=0.0d0
      vgb=0.0d0
      go to 300
   50 vbs=vbso(lx0+loct)
      vbd=vbdo(lx0+loct)
      vgb=vgbo(lx0+loct)
      go to 300
   60 vbs=vbso(lx1+loct)
      vbd=vbdo(lx1+loct)
      vgb=vgbo(lx1+loct)
      go to 300
   70 xfact=delta/delold(2)
      vbso(lx0+loct)=vbso(lx1+loct)
      vbs=(1.0d0+xfact)*vbso(lx1+loct)-xfact*vbso(lx2+loct)
      vbdo(lx0+loct)=vbdo(lx1+loct)
      vbd=(1.0d0+xfact)*vbdo(lx1+loct)-xfact*vbdo(lx2+loct)
      vgbo(lx0+loct)=vgbo(lx1+loct)
      vgb=(1.0d0+xfact)*vgbo(lx1+loct)-xfact*vgbo(lx2+loct)
      cdo(lx0+loct)=cdo(lx1+loct)
      cbo(lx0+loct)=cbo(lx1+loct)
      gdgo(lx0+loct)=gdgo(lx1+loct)
      gddo(lx0+loct)=gddo(lx1+loct)
      gdso(lx0+loct)=gdso(lx1+loct)
      gbgo(lx0+loct)=gbgo(lx1+loct)
      gbdo(lx0+loct)=gbdo(lx1+loct)
      gbso(lx0+loct)=gbso(lx1+loct)
      go to 110
c
c  compute new nonlinear branch voltages
c
  100 vbs=type*(value(lvnim1+node4)-value(lvnim1+node6))
      vbd=type*(value(lvnim1+node4)-value(lvnim1+node5))
      vgb=type*(value(lvnim1+node2)-value(lvnim1+node4))
  110 delvbs=vbs-vbso(lx0+loct)
      delvbd=vbd-vbdo(lx0+loct)
      delvgb=vgb-vgbo(lx0+loct)
      cdhat=cdo(lx0+loct)+gdgo(lx0+loct)*delvgb-gddo(lx0+loct)*delvbd
     1  -gdso(lx0+loct)*delvbs
      cbhat=cbo(lx0+loct)+gbgo(lx0+loct)*delvgb-gbdo(lx0+loct)*delvbd
     1   -gbso(lx0+loct)*delvbs
c
c  bypass if solution has not changed
c
c********** kill bypass for now!!!!!
      if (6    .eq.6) go to 200
      tol=reltol*dmax1(dabs(vbs),dabs(vbso(lx0+loct)))+vntol
      if (dabs(delvbs).ge.tol) go to 200
      tol=reltol*dmax1(dabs(vbd),dabs(vbdo(lx0+loct)))+vntol
      if (dabs(delvbd).ge.tol) go to 200
      tol=reltol*dmax1(dabs(vgb),dabs(vgbo(lx0+loct)))+vntol
      if (dabs(delvgb).ge.tol) go to 200
      tol=reltol*dmax1(dabs(cdhat),dabs(cdo(lx0+loct)))+abstol
      if (dabs(cdhat-cdo(lx0+loct)).ge.tol) go to 200
      tol=reltol*dmax1(dabs(cbhat),dabs(cbo(lx0+loct)))+abstol
      if (dabs(cbhat-(cbo(lx0+loct))).ge.tol) go to 200
      vbd=vbdo(lx0+loct)
      vbs=vbso(lx0+loct)
      vgb=vgbo(lx0+loct)
      cdrain=cdo(lx0+loct)
      cbulk=cbo(lx0+loct)
      gccdg=gdgo(lx0+loct)
      gccdd=gddo(lx0+loct)
      gccds=gdso(lx0+loct)
      gccbg=gbgo(lx0+loct)
      gccbd=gbdo(lx0+loct)
      gccbs=gbso(lx0+loct)
      go to 900
c
c  limit nonlinear branch voltages
c
  200 von=type*value(locv+9)
      icheck=0
      call fetlim(vgb,vgbo(lx0+loct),von,icheck)
      vcornr=0.0d0
c     if(vbs.gt.0.0d0) vcornr=vt*dlog(vt/(root2*cssat))
      call pnjlim(vbs,vbso(lx0+loct),vt,vcornr,icheck)
c     vbs=dmax1(vbso(lx0+loct)-10.0d0,vbs)
      vcornr=0.0d0
c     if(vbd.gt.0.0d0) vcornr=vt*dlog(vt/(root2*cdsat))
      call pnjlim(vbd,vbdo(lx0+loct),vt,vcornr,icheck)
c     vbd=dmax1(vbdo(lx0+loct)-10.0d0,vbd)
c
c  determine bulk-drain and bulk-source diode terms
c
  300 fivevt=-5.0d0*vt
      if (vbs.gt.fivevt) go to 310
      geqbs=-cssat/vbs+gmin
      cbulk=geqbs*vbs
      go to 320
  310 evbs=dexp(vbs/vt)
      geqbs=cssat*evbs/vt+gmin
      cbulk=cssat*(evbs-1.0d0)+gmin*vbs
  320 if (vbd.gt.fivevt) go to 330
      geqbd=-cdsat/vbd+gmin
      cbd=geqbd*vbd
      cbulk=cbulk+cbd
      go to 340
  330 evbd=dexp(vbd/vt)
      geqbd=cdsat*evbd/vt+gmin
      cbd=cdsat*(evbd-1.0d0)+gmin*vbd
      cbulk=cbulk+cbd
c.. cbd must also be subtracted from drain current
  340 continue
      gccdd=geqbd
      gccbd=-geqbd
      gccss=geqbs
      gccbs=-geqbs
      if (mode.ne.1) go to 350
c.. zero out some conductances and cgate
      cgate=0.0d0
      gccgg=0.0d0
      gccgd=0.0d0
      gccgs=0.0d0
      gccbg=0.0d0
c
c  compute drain current and derivatives
c
  350 cox=cox*xl*xw
      if(vbd.gt.vbs) go to 360
c.. normal operation
      devmod=1.0d0
      call calcq(vgb,vbd,vbs,qgate,qchan,qbulk,
     1 ccgg,ccgd,ccgs,ccbg,ccbd,ccbs,didvg,didvd,didvs)
      didvg=beta*didvg
      didvd=beta*didvd
      didvs=beta*didvs
      go to 370
c.. inverted operation
  360 devmod=-1.0d0
      call calcq(vgb,vbs,vbd,qgate,qchan,qbulk,
     1 ccgg,ccgs,ccgd,ccbg,ccbs,ccbd,didvg,didvs,didvd)
      didvg=-beta*didvg
      didvd=-beta*didvd
      didvs=-beta*didvs
      cdrain=-cdrain
  370 cdrain=beta*cdrain-cbd
c     if(mode.ne.1) write(6,6666) qgate,qchan,qbulk,time,delta
 6666 format(' qg qc qb',1p3e11.3,' time delta ',2e11.3)
      value(locv+8)=devmod
      value(locv+9)=type*von
      value(locv+10)=type*vdsat
      if(mode.ne.1) go to 500
      if ((modedc.eq.2).and.(nosolv.ne.0)) go to 500
      if (initf.eq.4) go to 500
      gccdg=didvg
      gccdd=gccdd+didvd
      gccds=didvs
      gccsg=-didvg
      gccsd=-didvd
      gccss=gccss-didvs
      go to 700
c
c  charge storage elements
c
c.. bulk-drain and bulk-source depletion capacitances
  500 czbd=value(locm+11)*ad
      czbs=value(locm+12)*as
      phib=value(locm+14)
      twop=phib+phib
      fcpb=value(locm+29)
      if(vbs.lt.fcpb.and.vbd.lt.fcpb) go to 505
      fcpb2=fcpb*fcpb
      f1=value(locm+30)
      f2=value(locm+31)
      f3=value(locm+32)
      czbsf2=czbs/f2
      czbdf2=czbd/f2
      if (vbs.ge.fcpb) go to 510
  505 sarg=dsqrt(1.0d0-vbs/phib)
      qbs=twop*czbs*(1.0d0-sarg)
      capbs=czbs/sarg
      go to 520
  510 qbs=czbs*f1+czbsf2*(f3*(vbs-fcpb)
     1   +(vbs*vbs-fcpb2)/(twop+twop))
      capbs=czbsf2*(f3+vbs/twop)
  520 if (vbd.ge.fcpb) go to 530
      sarg=dsqrt(1.0d0-vbd/phib)
      qbd=twop*czbd*(1.0d0-sarg)
      capbd=czbd/sarg
      go to 540
  530 qbd=czbd*f1+czbdf2*(f3*(vbd-fcpb)
     1   +(vbd*vbd-fcpb2)/(twop+twop))
      capbd=czbdf2*(f3+vbd/twop)
c.. bulk and channel charge (plus overlaps)
  540 qgd=covlgd*(vgb+vbd)
      qgs=covlgs*(vgb+vbs)
      qgb=covlgb*vgb
      qg(lx0+loct)=qgate+qgb+qgd+qgs
      qd(lx0+loct)=qchan*0.5d0-qgd-qbd
      qb(lx0+loct)=qbulk+qbd+qbs-qgb
c
c  store small-signal parameters
c
  590 if ((mode.eq.1).and.(modedc.eq.2).and.(nosolv.ne.0)) go to 900
      if (initf.ne.4) go to 600
      value(lx0+loct)=didvg
      value(lx0+loct+1)=didvd
      value(lx0+loct+2)=didvs
      value(lx0+loct+3)=geqbd
c.. cdrain is used in printing as well as noise calculation
      value(lx0+loct+4)=cdrain
      value(lx0+loct+5)=geqbs
c..   (loct+6 not used)
c.. this is the 'gm' term used in the noise calculation
      value(lx0+loct+7)=didvg
      value(lx0+loct+8)=ccgg
      value(lx0+loct+9)=ccgd
      value(lx0+loct+10)=ccgs
      value(lx0+loct+11)=ccbg
      value(lx0+loct+12)=ccbd
      value(lx0+loct+13)=ccbs
      value(lx0+loct+14)=capbd
      value(lx0+loct+15)=capbs
      go to 1000
c
c  transient analysis
c
  600 if (initf.ne.5) go to 610
      qb(lx1+loct)=qb(lx0+loct)
      qg(lx1+loct)=qg(lx0+loct)
      qd(lx1+loct)=qd(lx0+loct)
c.. integrate qb
  610 call intgr8(geq,ceq,0.0d0,loct+12)
c.. integrate qg
      call intgr8(geq,ceq,0.0d0,loct+14)
c.. integrate qd
      call intgr8(geq,ceq,0.0d0,loct+16)
c.. divvey up the channel charge 50/50 to source and drain
c.. note that symmetry also precludes need for 'devmod' decisions
      gccg2=-0.5d0*(ccgg+ccbg)*ag(1)
      gccd2=-0.5d0*(ccgd+ccbd)*ag(1)
      gccs2=-0.5d0*(ccgs+ccbs)*ag(1)
      gccdg=gccg2+didvg-covlgd*ag(1)
      gccdd=gccdd+gccd2+didvd+(capbd+covlgd)*ag(1)
      gccds=gccs2+didvs
      gccsg=gccg2-didvg-covlgs*ag(1)
      gccsd=gccd2-didvd
      gccss=gccss+gccs2-didvs+(capbs+covlgs)*ag(1)
      gccgg=(ccgg+covlgd+covlgs+covlgb)*ag(1)
      gccgd=(ccgd-covlgd)*ag(1)
      gccgs=(ccgs-covlgs)*ag(1)
      gccbg=(ccbg-covlgb)*ag(1)
      gccbd=gccbd+(ccbd-capbd)*ag(1)
      gccbs=gccbs+(ccbs-capbs)*ag(1)
      cgate=cqg(lx0+loct)
      cbulk=cbulk+cqb(lx0+loct)
      cdrain=cdrain+cqd(lx0+loct)
      if (initf.ne.5) go to 700
      cqb(lx1+loct)=cqb(lx0+loct)
      cqg(lx1+loct)=cqg(lx0+loct)
      cqd(lx1+loct)=cqd(lx0+loct)
c
c  check convergence
c
  700 if (initf.ne.3) go to 710
      if (ioff.ne.0) go to 750
  710 if (icheck.eq.1) go to 720
c     tol=reltol*dmax1(dabs(cdhat),dabs(cdrain))+abstol
c     if (dabs(cdhat-cdrain).ge.tol) go to 720
c     tol=reltol*dmax1(dabs(cbhat),dabs(cbulk))+abstol
c     if (dabs(cbhat-cbulk).le.tol) go to 750
      tol=reltol*dabs(vgb)+vntol
      if(dabs(delvgb).ge.tol) go to 720
      tol=reltol*dabs(vbd)+vntol
      if(dabs(delvbd).ge.tol) go to 720
      tol=reltol*dabs(vbs)+vntol
      if(dabs(delvbs).lt.tol) go to 750
  720 noncon=noncon+1
  750 vbdo(lx0+loct)=vbd
      vbso(lx0+loct)=vbs
      vgbo(lx0+loct)=vgb
      cdo(lx0+loct)=cdrain
      cbo(lx0+loct)=cbulk
      gdgo(lx0+loct)=gccdg
      gddo(lx0+loct)=gccdd
      gdso(lx0+loct)=gccds
      gbgo(lx0+loct)=gccbg
      gbdo(lx0+loct)=gccbd
      gbso(lx0+loct)=gccbs
c
c  load current vector
c
  900 ceqg=type*(cgate-gccgg*vgb+gccgd*vbd+gccgs*vbs)
      ceqb=type*(cbulk-gccbg*vgb+gccbd*vbd+gccbs*vbs)
      ceqd=type*(cdrain-gccdg*vgb+gccdd*vbd+gccds*vbs)
      value(lvn+node2)=value(lvn+node2)-ceqg
      value(lvn+node4)=value(lvn+node4)-ceqb
      value(lvn+node5)=value(lvn+node5)-ceqd
      value(lvn+node6)=value(lvn+node6)+ceqd+ceqg+ceqb
c
c  load y matrix
c
      locy=lynl+nodplc(loc+27)
      value(locy)=value(locy)+gdpr
      locy=lynl+nodplc(loc+28)
      value(locy)=value(locy)+gccgg
      locy=lynl+nodplc(loc+29)
      value(locy)=value(locy)+gspr
      locy=lynl+nodplc(loc+30)
      value(locy)=value(locy)-gccbg-gccbd-gccbs
      locy=lynl+nodplc(loc+31)
      value(locy)=value(locy)+gdpr+gccdd
      locy=lynl+nodplc(loc+32)
      value(locy)=value(locy)+gspr+gccss
      locy=lynl+nodplc(loc+10)
      value(locy)=value(locy)-gdpr
      locy=lynl+nodplc(loc+11)
      value(locy)=value(locy)-gccgg-gccgd-gccgs
      locy=lynl+nodplc(loc+12)
      value(locy)=value(locy)+gccgd
      locy=lynl+nodplc(loc+13)
      value(locy)=value(locy)+gccgs
      locy=lynl+nodplc(loc+14)
      value(locy)=value(locy)-gspr
      locy=lynl+nodplc(loc+15)
      value(locy)=value(locy)+gccbg
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
 1000 loc=nodplc(loc)
      go to 10
      end
      subroutine calcq(vgb,vbd,vbs,qg,qc,qb,
     1  ccgg,ccgd,ccgs,ccbg,ccbd,ccbs,
     2  didvg,didvd,didvs)
      implicit double precision (a-h,o-z)
      common /status/ omega,time,delta,delold(7),ag(7),vt,xni,egfet,
     1   xmu,mode,modedc,icalc,initf,method,iord,maxord,noncon,iterno,
     2   itemno,nosolv,ipostp,iscrch
      common /mosarg/ gamma,beta,vto,phi,cox,vbi,xnfs,xnsub,xd,xj,xl,
     1   xlamda,utra,uexp,vbp,von,vdsat,theta,vcrit,vtra,gleff,cdrain
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      iflag=1
      if(mode.ne.1) go to 5
      iflag=0
      if(modedc.eq.2.and.nosolv.ne.0) iflag=1
      if(initf.eq.4) iflag=1
    5 vd=dmax1(phi-vbd,1.0d-8)
      vg=vgb-vbi+phi
      vs=dmax1(phi-vbs,1.0d-8)
      vsp5=dsqrt(vs)
      gammad=gamma
      if(gamma.eq.0.0d0) go to 7
      if(xj.eq.0.0d0) go to 7
      arg=dsqrt(1.0d0+xd*2.0d0*vsp5/xj)
      gfact=1.0d0-xj/xl*(arg-1.0d0)
      gfact=dmax1(0.5d0,gfact)
      gammad=gamma*gfact
    7 vth=gammad*vsp5+vs
c.. von is referenced to vgb for 'fetlim'
c.. change mosfet to reference to vgs (von=vth+vbi-vs) for
c.. printing
      von=vth+vbi-phi
      vdsat=0.0d0
      if(vg.lt.vth) go to 100
c
c  'on' region (linear and saturated)
c
      gamma2=gammad*0.5d0
      sqarg=dsqrt(gamma2*gamma2+vg)
      vsat=(sqarg-gamma2)**2
      vsatcl=vsat
      vs2=vs*vs
      vs3=vs2*vs
      vs5=vs3*vs2
      vs1p5=vs*vsp5
      vs2p5=vs1p5*vs
      if(vcrit.eq.0.0d0) go to 9
c...... iterate to new vsat
      iter=1
    8 ve2=vsat*vsat
c     write(6,8878) iter,vsat
 8878 format(' iter vsat ',i4,1pd11.2)
      vep5=dsqrt(vsat)
      ve1p5=vsat*vep5
      arg2=0.5d0*(ve2-vs2)
      arg1p5=gammad*(ve1p5-vs1p5)/1.5d0
      cdrain=vg*(vsat-vs)-arg1p5-arg2
      didve=vg-gammad*vep5-vsat
      d2idve=-0.5d0*gammad/dmax1(vep5,1.0d-5)-1.0d0
      if(vtra.eq.0.0d0) go to 88
      trafac=1.0d0/(1.0d0+(vsat-vs)/vtra)
      dtrdve=-trafac*trafac/vtra
      d2idve=d2idve*trafac+(dtrdve+dtrdve)*(didve-cdrain*trafac/vtra)
      didve=didve*trafac+dtrdve*cdrain
      cdrain=cdrain*trafac
   88 delv=(didve*vcrit-cdrain)/dabs(didve-vcrit*d2idve)
c.. limit voltage excursion to 1/2 old vsat
      if(dabs(delv).gt.0.5d0*vsat) delv=vsat*dsign(0.5d0,delv)
      vsat=vsat+delv
c     vsat=dmax1(vsat,1.0d-5)
c     vsat=dmin1(vsat,vsatcl)
      if(dabs(delv).lt.1.0d-6) go to 9
      iter=iter+1
      if(iter.gt.20) write(6,7777) vg,vs,vsat,delv
 7777 format(' iteration count for vsat hit limit of 20'/,
     1  ' vg vs vsat delv ',1p4d11.3)
      if(iter.gt.20) go to 9
      go to 8
c.. end of iteration loop
c.. vdsat is referenced to vds for printing only
    9 vdsat=vsat-vs
      if(vsat.gt.vsatcl) write(6,9989) vsat,vsatcl
 9989 format(' ********error****** vsat is larger than classical vsat',/
     1' vsat ',1pd11.3,' classical vsat ',d11.3)
 9999 format(' vsat ',1pd11.3,' classical vsat ',d11.3)
      if(vd.ge.vsat) go to 10
c.. linear region
      ve=vd
      dvedvd=1.0d0
      dvedvg=0.0d0
      go to 15
c.. saturated region
   10 ve=vsat
      dvedvd=0.0d0
c**************** zero dvedvg!!!
c     dvedvg=1.0d0-gamma2/sqarg
      dvedvg=0.0d0
c
   15 ve2=ve*ve
      ve3=ve2*ve
      ve5=ve3*ve2
      vep5=dsqrt(ve)
      ve1p5=ve*vep5
      ve2p5=ve1p5*ve
      arg2=0.5d0*(ve2-vs2)
      arg1p5=gammad*(ve1p5-vs1p5)/1.5d0
      cdrain=vg*(ve-vs)-arg1p5-arg2
      didve=vg-gammad*vep5-ve
      didvg=ve-vs+didve*dvedvg
      didvs=-vg+gammad*vsp5+vs
      if(iflag.eq.0) go to 30
      if(dabs(cdrain).gt.1.0d-5) go to 20
c .. special case when ve almost equals vs and regular formulas don't work
c     write(6,5475) time,cdrain
 5475 format(' time = ',1pd15.6,' cdrain = ',e11.3)
      qg=cox*(vg-vs)
      ccgg=cox
      ccgd=-0.5d0*cox
      ccgs=ccgd
      qb=-cox*gammad*vsp5
      ccbg=0.0d0
      ccbd=-cox*0.25d0*gammad/dmax1(vsp5,1.0d-2)
      ccbs=ccbd
      go to 30
c
   20 arg2p5=gammad*0.4d0*(ve2p5-vs2p5)
      varg=(vg*arg2-arg2p5-(ve3-vs3)/3.0d0)/cdrain
      qg=cox*(vg-varg)
      dqgdve=cox/cdrain*(varg-ve)*didve
      ccgg=cox*(1.0d0-(arg2-varg*(ve-vs))/cdrain)
     1  +dqgdve*dvedvg
      ccgd=dqgdve*dvedvd
      ccgs=cox/cdrain*(varg-vs)*didvs
      qb=-cox/cdrain*(vg*arg1p5-gammad*gammad*arg2-arg2p5)
      dqbdve=-cox/cdrain*(gammad*vep5+qb/cox)*didve
      ccbd=dqbdve*dvedvd
      ccbs=-cox/cdrain*(gammad*vsp5+qb/cox)*didvs
      ccbg=-cox/cdrain*(arg1p5+qb/cox*(ve-vs))
     1  +dqbdve*dvedvg
c.. mobility factor (a-la bdm)
   30 if(uexp.eq.0.0d0) go to 35
      vdenom=vg-vth-utra*(ve-vs)
      if(vdenom.le.vbp) go to 45
      arg=vbp/vdenom
      ufact=dexp(uexp*dlog(arg))
      dcoef=-uexp*ufact*arg/vbp
      didvg=ufact*didvg+cdrain*dcoef
      didvs=ufact*didvs-cdrain*dcoef*(0.5d0*gammad/vsp5+1.0d0-utra)
      didve=ufact*didve-cdrain*dcoef*utra
      cdrain=cdrain*ufact
      go to 45
c.. lateral field effects 
   35 if(vtra.eq.0.0d0) go to 40
      trafac=1.0d0/(1.0d0+(ve-vs)/vtra)
      dtrdve=-trafac*trafac/vtra
      didve=didve*trafac+dtrdve*cdrain
c.. note that dtrdvs=-dtrdve
      didvs=didvs*trafac-dtrdve*cdrain
      didvg=didvg*trafac
      cdrain=cdrain*trafac
c.. mobility variation a-la sun-daseking
   40 if(theta.eq.0.0d0) go to 45
      ufact=1.0d0/(1.0d0+theta*(vg-vth))
      dufact=-theta*ufact*ufact
      didve=ufact*didve
      didvg=ufact*didvg+cdrain*dufact
      didvs=ufact*didvs-cdrain*dufact*(0.5d0*gammad/vsp5+1.0d0)
      cdrain=cdrain*ufact
c .. done with 've', use it
   45 didvd=didve*dvedvd
c.. channel length modulation
      if(vcrit.ne.0.0d0) go to 80
      if (xlamda.gt.0.0d0) go to 50
      if (xnsub.eq.0.0d0) go to 50
c.. frohman-grove (lousy) formulation modified a-la newton
      arg1=(vd-vsat)/4.0d0
      arg2=dsqrt(1.0d0+arg1*arg1)
      arg3=dsqrt(arg1+arg2)
      clfact=1.0d0/(1.0d0-xd/xl*arg3)
      if(clfact.le.0.0d0) go to 60
      dclfct=0.125d0*clfact*clfact*xd/xl*(1.0d0+arg1/arg2)/arg3
      didvd=clfact*didvd+cdrain*dclfct
      didvg=clfact*didvg-cdrain*dclfct*(1.0d0-gamma2/sqarg)
      didvs=clfact*didvs
      cdrain=cdrain*clfact
      go to 200
c.. simple (1+vds*lambda/l) formulation
   50 xlfact=xlamda/xl
      clfact=1.0d0+xlfact*(vd-vs)
      didvd=clfact*didvd+cdrain*xlfact
      didvs=clfact*didvs-cdrain*xlfact
      didvg=clfact*didvg
      cdrain=cdrain*clfact
      go to 200
c.. device punched thru
   60 clfact=1000.0d0
      if(ipunch.gt.50) go to 200
      ipunch=ipunch+1
      write(6,61)
   61 format('0warning:  channel length reduced to zero in mosfet')
      go to 200
c.. into saturation with vcrit ne 0
   80 if(vd.le.vsat) go to 200
      xk1=vcrit/2.0d0/xl/gleff
      temp=dsqrt(xk1*xk1+(vd-vsat)/gleff)
      clfact=1.0d0+(temp-xk1)/xl
      dclfct=0.5d0/xl/temp/gleff
      didvd=didvd*clfact+cdrain*dclfct
      didvs=didvs*clfact
      didvg=didvg*clfact
      cdrain=cdrain*clfact
c
      go to 200
c
c.. cut-off region (vg<vth)
c
  100 continue
      cdrain=0.0d0
      didvg=0.0d0
      didvd=0.0d0
      didvs=0.0d0
      if(iflag.eq.0) return
      if(vg.gt.0.0d0) go to 120
      qg=cox*vg
      ccgg=cox
      go to 130
  120 gamma2=gammad*0.5d0
      sqarg=dsqrt(gamma2*gamma2+vg)
      qg=gammad*cox*(sqarg-gamma2)
      ccgg=0.5d0*cox*gammad/sqarg
  130 qb=-qg
      ccbg=-ccgg
      ccgd=0.0d0
      ccgs=0.0d0
      ccbd=0.0d0
      ccbs=0.0d0
  200 qc=-(qg+qb)
c     write(6,7657) time,vg,vs,ve,qg,ccgg,ccgd,ccgs,qb,ccbg,ccbd,ccbs
 7657 format(' time = ',1pd12.5,' vg vs ve ',3d11.3,
     1 /,' qg ',4d11.3,' qb ',4d11.3)
      return
      end
      subroutine gasfet(loc)
c     *** a gasfet (or any other) model may ***
c     *   be inserted here... if you happen   *
c     *   to have a good one!                 *
c     ***                                   ***
c
      return
      end
