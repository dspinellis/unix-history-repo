      subroutine dcop
      implicit double precision (a-h,o-z)
c
c
c     this routine prints out the operating points of the nonlinear
c circuit elements.
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
      common /miscel/ atime,aprog(3),adate,atitle(10),defl,defw,defad,
     1  defas,rstats(50),iwidth,lwidth,nopage
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
      dimension optitl(4)
      dimension anam(12),av1(12),ai1(12),req(12)
      dimension amod(12),vd(12),cap(12)
      dimension cb(12),cc(12),vbe(12),vbc(12),vce(12),rpi(12),
     1   ro(12),cpi(12),cmu(12),betadc(12),betaac(12),ft(12),
     2   ccs(12),cbx(12),rx(12)
      dimension cg(12),vgs(12),vds(12),gds(12),vbs(12),cbd(12),cbs(12),
     2  cgsov(12),cgdov(12),cgbov(12),vth(12),vdsat(12),cd(12),gm(12),
     3  ccgg(12),ccgd(12),ccgs(12),ccbg(12),ccbd(12),ccbs(12),
     4  gmb(12)
      dimension cgs(12),cgd(12),cgb(12),cds(12)
      equivalence(cb(1),cg(1)),(cc(1),vgs(1)),(vbe(1),vds(1)),
     1(vbc(1),gds(1)),(vce(1),vbs(1)),(rpi(1),cbd(1)),
     2(ro(1),cbs(1)),(cpi(1),cgsov(1)),(cmu(1),cgdov(1)),
     3(betadc(1),cgbov(1)),(betaac(1),vth(1)),(ft(1),vdsat(1)),
     4(ccs(1),cd(1)),(cbx(1),ccgg(1)),(rx(1),ccgd(1))
      equivalence(vd(1),cg(1)),(cap(1),vgs(1)),(av1(1),vds(1)),
     1  (ai1(1),gds(1)),(req(1),vbs(1))
      equivalence (cgs(1),ccgg(1)),(cgd(1),ccgd(1)),(cgb(1),ccgs(1)),
     1  (cds(1),ccbg(1))
      dimension afmt1(3),afmt2(2),afmt3(3),afmt4(3)
      data optitl / 8hoperatin, 8hg point , 8hinformat, 8hion      /
      data av,avd,avbe,avbc,avce,avgs,avds,avbs / 1hv,2hvd,3hvbe,3hvbc,
     1   3hvce,3hvgs,3hvds,3hvbs /
      data acntrv,acntri,asrcv,asrci,atrang,atranr,avgain,aigain /
     1   8hv-contrl, 8hi-contrl, 8hv-source, 8hi-source,
     2   8htrans-g , 8htrans-r , 8hv gain  , 8hi gain   /
      data ai,aid,aib,aic,aig / 1hi,2hid,2hib,2hic,2hig /
      data areq,arpi,aro / 3hreq,3hrpi,2hro /
      data acap,acpi,acmu,acgs,acgd,acbd,acbs / 3hcap,3hcpi,3hcmu,3hcgs,
     1   3hcgd,3hcbd,3hcbs /
      data acgsov,acgdov,acgbov /6hcgsovl,6hcgdovl,6hcgbovl/
      data accgg,accgd,accgs,accbg,accbd,accbs /7hdqgdvgb,7hdqgdvdb,
     1  7hdqgdvsb,7hdqbdvgb,7hdqbdvdb,7hdqbdvsb/
      data acgb,acds / 3hcgb,3hcds /
      data avth, avdsat / 3hvth, 5hvdsat /
      data agm,agds / 2hgm,3hgds /
      data agmb / 4hgmb /
      data accs,acbx,arx /3hccs,3hcbx,2hrx/
      data abetad,abetaa / 6hbetadc,6hbetaac /
      data aft / 2hft /
c
      data ablnk /1h /
      data afmt1 /8h(//1h0,1,8h0x,  (2x,8h,a8))   /
      data afmt2 /8h(1h ,a8,,8h  f10.3)/
      data afmt3 /8h(1h ,a8,,8h1p  d10.,8h2)      /
      data afmt4 /8h('0model,8h   ',  (,8h2x,a8)) /
c
c.. fix-up the format statements
c
      kntr=12
      if(lwidth.le.80) kntr=7
      ipos=12
      call move(afmt1,ipos,ablnk,1,2)
      call alfnum(kntr,afmt1,ipos)
      ipos=9
      call move(afmt2,ipos,ablnk,1,2)
      call alfnum(kntr,afmt2,ipos)
      ipos=11
      call move(afmt3,ipos,ablnk,1,2)
      call alfnum(kntr,afmt3,ipos)
      ipos=14
      call move(afmt4,ipos,ablnk,1,2)
      call alfnum(kntr,afmt4,ipos)
c
c  compute voltage source currents and power dissipation
c
      call second(t1)
      if ((mode.eq.1).and.(modedc.eq.2).and.(nosolv.ne.0)) go to 700
      power=0.0d0
      if (jelcnt(9).eq.0) go to 50
      ititle=0
   11 format (////5x,'voltage source currents'//5x,'name',
     1   7x,'current'/)
      loc=locate(9)
   20 if (loc.eq.0) go to 50
      locv=nodplc(loc+1)
      iptr=nodplc(loc+6)
      creal=value(lvnim1+iptr)
      power=power-creal*value(locv+1)
      if (ititle.eq.0) write (6,11)
      ititle=1
      write (6,21) value(locv),creal
   21 format (/5x,a8,1x,1pd10.3)
   30 loc=nodplc(loc)
      go to 20
   50 loc=locate(10)
   60 if (loc.eq.0) go to 90
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      power=power-value(locv+1)
     1   *(value(lvnim1+node1)-value(lvnim1+node2))
      loc=nodplc(loc)
      go to 60
   90 write (6,91) power
   91 format (//5x,'total power dissipation  ',1pd9.2,'  watts')
c
c  small signal device parameters
c
      numdev=jelcnt(5)+jelcnt(6)+jelcnt(7)+jelcnt(8)+jelcnt(11)
     1   +jelcnt(12)+jelcnt(13)+jelcnt(14)
      if (numdev.eq.0) go to 600
      call title(0,lwidth,1,optitl)
      kntlim=lwidth/11
c
c  nonlinear voltage controlled current sources
c
      if (jelcnt(5).eq.0) go to 175
      ititle=0
  111 format(1h0,/,'0**** voltage-controlled current sources')
      loc=locate(5)
      kntr=0
  120 if (loc.eq.0) go to 140
      kntr=kntr+1
      locv=nodplc(loc+1)
      loct=lx0+nodplc(loc+12)
      anam(kntr)=value(locv)
      ai1(kntr)=value(loct)
      if (kntr.ge.kntlim) go to 150
  130 loc=nodplc(loc)
      go to 120
  140 if (kntr.eq.0) go to 175
  150 if (ititle.eq.0) write (6,111)
      ititle=1
      write (6,afmt1) (anam(i),i=1,kntr)
      write (6,afmt3) asrci,(ai1(i),i=1,kntr)
      kntr=0
      if (loc.ne.0) go to 130
c
c  nonlinear voltage controlled voltage sources
c
  175 if (jelcnt(6).eq.0) go to 186
      ititle=0
  176 format(1h0,/,'0**** voltage-controlled voltage sources')
      loc=locate(6)
      kntr=0
  178 if (loc.eq.0) go to 182
      kntr=kntr+1
      locv=nodplc(loc+1)
      loct=lx0+nodplc(loc+13)
      anam(kntr)=value(locv)
      av1(kntr)=value(loct)
      ai1(kntr)=value(loct+1)
      if (kntr.ge.kntlim) go to 184
  180 loc=nodplc(loc)
      go to 178
  182 if (kntr.eq.0) go to 186
  184 if (ititle.eq.0) write (6,176)
      ititle=1
      write (6,afmt1) (anam(i),i=1,kntr)
      write (6,afmt2) asrcv,(av1(i),i=1,kntr)
      write (6,afmt3) asrci,(ai1(i),i=1,kntr)
      kntr=0
      if (loc.ne.0) go to 180
c
c  nonlinear current controlled current sources
c
  186 if (jelcnt(7).eq.0) go to 196
      ititle=0
  187 format(1h0,/,'0**** current-controlled current sources')
      loc=locate(7)
      kntr=0
  188 if (loc.eq.0) go to 192
      kntr=kntr+1
      locv=nodplc(loc+1)
      loct=lx0+nodplc(loc+12)
      anam(kntr)=value(locv)
      ai1(kntr)=value(loct)
      if (kntr.ge.kntlim) go to 194
  190 loc=nodplc(loc)
      go to 188
  192 if (kntr.eq.0) go to 196
  194 if (ititle.eq.0) write (6,187)
      ititle=1
      write (6,afmt1) (anam(i),i=1,kntr)
      write (6,afmt3) asrci,(ai1(i),i=1,kntr)
      kntr=0
      if (loc.ne.0) go to 190
c
c  nonlinear current controlled voltage sources
c
  196 if (jelcnt(8).eq.0) go to 210
      ititle=0
  197 format(1h0,/,'0**** current-controlled voltage sources')
      loc=locate(8)
      kntr=0
  198 if (loc.eq.0) go to 202
      kntr=kntr+1
      locv=nodplc(loc+1)
      loct=lx0+nodplc(loc+13)
      anam(kntr)=value(locv)
      av1(kntr)=value(loct)
      ai1(kntr)=value(loct+1)
      if (kntr.ge.kntlim) go to 204
  200 loc=nodplc(loc)
      go to 198
  202 if (kntr.eq.0) go to 210
  204 if (ititle.eq.0) write (6,197)
      ititle=1
      write (6,afmt1) (anam(i),i=1,kntr)
      write (6,afmt2) asrcv,(av1(i),i=1,kntr)
      write (6,afmt3) asrci,(ai1(i),i=1,kntr)
      kntr=0
      if (loc.ne.0) go to 200
c
c  diodes
c
  210 if (jelcnt(11).eq.0) go to 300
      ititle=0
  211 format(1h0,/,'0**** diodes')
      loc=locate(11)
      kntr=0
  220 if (loc.eq.0) go to 240
      kntr=kntr+1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      locm=nodplc(loc+5)
      locm=nodplc(locm+1)
      loct=lx0+nodplc(loc+11)
      anam(kntr)=value(locv)
      amod(kntr)=value(locm)
      cd(kntr)=value(loct+1)
      vd(kntr)=value(lvnim1+node1)-value(lvnim1+node2)
      if (modedc.ne.1) go to 225
      req(kntr)=1.0d0/value(loct+2)
      cap(kntr)=value(loct+4)
  225 if (kntr.ge.kntlim) go to 250
  230 loc=nodplc(loc)
      go to 220
  240 if (kntr.eq.0) go to 300
  250 if (ititle.eq.0) write (6,211)
      ititle=1
      write (6,afmt1) (anam(i),i=1,kntr)
      write (6,afmt4) (amod(i),i=1,kntr)
      write (6,afmt3) aid,(cd(i),i=1,kntr)
      write (6,afmt2) avd,(vd(i),i=1,kntr)
      if (modedc.ne.1) go to 260
      write (6,afmt3) areq,(req(i),i=1,kntr)
      write (6,afmt3) acap,(cap(i),i=1,kntr)
  260 kntr=0
      if (loc.ne.0) go to 230
c
c  bipolar junction transistors
c
  300 if (jelcnt(12).eq.0) go to 400
      ititle=0
  301 format(1h0,/,'0**** bipolar junction transistors')
      loc=locate(12)
      kntr=0
  320 if (loc.eq.0) go to 340
      kntr=kntr+1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      locm=nodplc(loc+8)
      type=nodplc(locm+2)
      locm=nodplc(locm+1)
      loct=lx0+nodplc(loc+22)
      anam(kntr)=value(locv)
      amod(kntr)=value(locm)
      cb(kntr)=type*value(loct+3)
      cc(kntr)=type*value(loct+2)
      vbe(kntr)=value(lvnim1+node2)-value(lvnim1+node3)
      vbc(kntr)=value(lvnim1+node2)-value(lvnim1+node1)
      vce(kntr)=vbe(kntr)-vbc(kntr)
      betadc(kntr)=cc(kntr)/dsign(dmax1(dabs(cb(kntr)),1.0d-20),
     1  cb(kntr))
      if (modedc.ne.1) go to 325
      rx(kntr)=0.0d0
      if(value(loct+16).ne.0.0d0) rx(kntr)=1.0d0/value(loct+16)
      ccs(kntr)=value(loct+13)
      cbx(kntr)=value(loct+15)
      rpi(kntr)=1.0d0/value(loct+4)
      gm(kntr)=value(loct+6)
      ro(kntr)=1.0d0/value(loct+7)
      cpi(kntr)=value(loct+9)
      cmu(kntr)=value(loct+11)
      betaac(kntr)=gm(kntr)*rpi(kntr)
      ft(kntr)=gm(kntr)/(twopi*dmax1(cpi(kntr)+cmu(kntr)+cbx(kntr),
     1  1.0d-20))
  325 if (kntr.ge.kntlim) go to 350
  330 loc=nodplc(loc)
      go to 320
  340 if (kntr.eq.0) go to 400
  350 if (ititle.eq.0) write (6,301)
      ititle=1
      write (6,afmt1) (anam(i),i=1,kntr)
      write (6,afmt4) (amod(i),i=1,kntr)
      write (6,afmt3) aib,(cb(i),i=1,kntr)
      write (6,afmt3) aic,(cc(i),i=1,kntr)
      write (6,afmt2) avbe,(vbe(i),i=1,kntr)
      write (6,afmt2) avbc,(vbc(i),i=1,kntr)
      write (6,afmt2) avce,(vce(i),i=1,kntr)
      write (6,afmt2) abetad,(betadc(i),i=1,kntr)
      if (modedc.ne.1) go to 360
      write (6,afmt3) agm,(gm(i),i=1,kntr)
      write (6,afmt3) arpi,(rpi(i),i=1,kntr)
      write(6,afmt3) arx,(rx(i),i=1,kntr)
      write (6,afmt3) aro,(ro(i),i=1,kntr)
      write (6,afmt3) acpi,(cpi(i),i=1,kntr)
      write (6,afmt3) acmu,(cmu(i),i=1,kntr)
      write(6,afmt3) acbx,(cbx(i),i=1,kntr)
      write(6,afmt3) accs,(ccs(i),i=1,kntr)
      write (6,afmt2) abetaa,(betaac(i),i=1,kntr)
      write (6,afmt3) aft,(ft(i),i=1,kntr)
  360 kntr=0
      if (loc.ne.0) go to 330
c
c  jfets
c
  400 if (jelcnt(13).eq.0) go to 500
      ititle=0
  401 format(1h0,/,'0**** jfets')
      loc=locate(13)
      kntr=0
  420 if (loc.eq.0) go to 440
      kntr=kntr+1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      locm=nodplc(loc+7)
      type=nodplc(locm+2)
      locm=nodplc(locm+1)
      loct=lx0+nodplc(loc+19)
      anam(kntr)=value(locv)
      amod(kntr)=value(locm)
      cd(kntr)=type*(value(loct+3)-value(loct+4))
      vgs(kntr)=value(lvnim1+node2)-value(lvnim1+node3)
      vds(kntr)=value(lvnim1+node1)-value(lvnim1+node3)
      if (modedc.ne.1) go to 425
      gm(kntr)=value(loct+5)
      gds(kntr)=value(loct+6)
      cgs(kntr)=value(loct+9)
      cgd(kntr)=value(loct+11)
  425 if (kntr.ge.kntlim) go to 450
  430 loc=nodplc(loc)
      go to 420
  440 if (kntr.eq.0) go to 500
  450 if (ititle.eq.0) write (6,401)
      ititle=1
      write (6,afmt1) (anam(i),i=1,kntr)
      write (6,afmt4) (amod(i),i=1,kntr)
      write (6,afmt3) aid,(cd(i),i=1,kntr)
      write (6,afmt2) avgs,(vgs(i),i=1,kntr)
      write (6,afmt2) avds,(vds(i),i=1,kntr)
      if (modedc.ne.1) go to 460
      write (6,afmt3) agm,(gm(i),i=1,kntr)
      write (6,afmt3) agds,(gds(i),i=1,kntr)
      write (6,afmt3) acgs,(cgs(i),i=1,kntr)
      write (6,afmt3) acgd,(cgd(i),i=1,kntr)
  460 kntr=0
      if (loc.ne.0) go to 430
c
c  mosfets
c
  500 if (jelcnt(14).eq.0) go to 600
      ititle=0
  501 format(1h0,/,'0**** mosfets')
      loc=locate(14)
      kntr=0
  520 if (loc.eq.0) go to 540
      kntr=kntr+1
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      node6=nodplc(loc+7)
      locm=nodplc(loc+8)
      type=nodplc(locm+2)
      locm=nodplc(locm+1)
      loct=lx0+nodplc(loc+26)
      anam(kntr)=value(locv)
      amod(kntr)=value(locm)
      if(type.eq.0.0d0) go to 522
      cd(kntr)=type*value(loct+4)
      vgs(kntr)=value(lvnim1+node2)-value(lvnim1+node3)
      vds(kntr)=value(lvnim1+node1)-value(lvnim1+node3)
      vbs(kntr)=value(lvnim1+node4)-value(lvnim1+node3)
      if (modedc.ne.1) go to 525
      xl=value(locv+1)-2.0d0*value(locm+20)
      xw=value(locv+2)-2.0d0*value(locm+36)
      covlgs=value(locm+8)*xw
      covlgd=value(locm+9)*xw
      covlgb=value(locm+10)*xl
      devmod=value(locv+8)
      vdsat(kntr)=value(locv+10)
      vth(kntr)=value(locv+9)+type*(value(lvnim1+node4)-
     1  value(lvnim1+node6))
      gds(kntr)=value(loct+1)
      gm(kntr)=value(loct)
      gmb(kntr)=-(value(loct)+value(loct+1)+value(loct+2))
      if(devmod.gt.0.0d0) go to 521
      gds(kntr)=value(loct+2)
      vth(kntr)=value(locv+9)+type*(value(lvnim1+node4)-
     1  value(lvnim1+node5))
  521 cbd(kntr)=value(loct+14)
      cbs(kntr)=value(loct+15)
      cgsov(kntr)=covlgs
      cgdov(kntr)=covlgd
      cgbov(kntr)=covlgb
      ccgg(kntr)=value(loct+8)
      ccgd(kntr)=value(loct+9)
      ccgs(kntr)=value(loct+10)
      ccbg(kntr)=value(loct+11)
      ccbd(kntr)=value(loct+12)
      ccbs(kntr)=value(loct+13)
      go to 525
c... special case for ga-as
  522 cd(kntr)=value(loct+4)
      cg(kntr)=value(loct+5)
      vgs(kntr)=value(lvnim1+node2)-value(lvnim1+node3)
      vds(kntr)=value(lvnim1+node1)-value(lvnim1+node3)
      vbs(kntr)=value(lvnim1+node4)-value(lvnim1+node3)
      if(modedc.ne.1) go to 525
      modeop=value(locv+8)
      gm(kntr)=value(loct+7)
      gds(kntr)=(value(loct+8)*value(loct+11))
     1  /(value(loct+8)+value(loct+11))
      if(modeop.le.0) gm(kntr)=value(loct+13)
      cds(kntr)=value(loct+10)
      cgs(kntr)=value(loct+12)
      cgd(kntr)=value(loct+14)
      cgb(kntr)=value(loct+16)
  525 if (kntr.ge.kntlim) go to 550
  530 loc=nodplc(loc)
      go to 520
  540 if (kntr.eq.0) go to 600
  550 if (ititle.eq.0) write (6,501)
      ititle=1
      write (6,afmt1) (anam(i),i=1,kntr)
      write (6,afmt4) (amod(i),i=1,kntr)
      if(type.eq.0.0d0) go to 555
      write (6,afmt3) aid,(cd(i),i=1,kntr)
      write (6,afmt2) avgs,(vgs(i),i=1,kntr)
      write (6,afmt2) avds,(vds(i),i=1,kntr)
      write (6,afmt2) avbs,(vbs(i),i=1,kntr)
      if (modedc.ne.1) go to 560
      write (6,afmt2) avth,(vth(i),i=1,kntr)
      write (6,afmt2) avdsat,(vdsat(i),i=1,kntr)
      write (6,afmt3) agm,(gm(i),i=1,kntr)
      write (6,afmt3) agds,(gds(i),i=1,kntr)
      write (6,afmt3) agmb,(gmb(i),i=1,kntr)
      write (6,afmt3) acbd,(cbd(i),i=1,kntr)
      write (6,afmt3) acbs,(cbs(i),i=1,kntr)
      write (6,afmt3) acgsov,(cgsov(i),i=1,kntr)
      write (6,afmt3) acgdov,(cgdov(i),i=1,kntr)
      write (6,afmt3) acgbov,(cgbov(i),i=1,kntr)
      write(6,551)
  551 format(' derivatives of gate (dqgdvx) and bulk (dqbdvx) charges')
      write (6,afmt3) accgg,(ccgg(i),i=1,kntr)
      write (6,afmt3) accgd,(ccgd(i),i=1,kntr)
      write (6,afmt3) accgs,(ccgs(i),i=1,kntr)
      write (6,afmt3) accbg,(ccbg(i),i=1,kntr)
      write (6,afmt3) accbd,(ccbd(i),i=1,kntr)
      write (6,afmt3) accbs,(ccbs(i),i=1,kntr)
      go to 560
  555 write(6,afmt3) aid,(cd(i),i=1,kntr)
      write(6,afmt3) aig,(cg(i),i=1,kntr)
      write (6,afmt2) avgs,(vgs(i),i=1,kntr)
      write (6,afmt2) avds,(vds(i),i=1,kntr)
      write (6,afmt2) avbs,(vbs(i),i=1,kntr)
      if (modedc.ne.1) go to 560
      write (6,afmt3) agm,(gm(i),i=1,kntr)
      write (6,afmt3) agds,(gds(i),i=1,kntr)
      write (6,afmt3) acgs,(cgs(i),i=1,kntr)
      write (6,afmt3) acgd,(cgd(i),i=1,kntr)
      write (6,afmt3) acgb,(cgb(i),i=1,kntr)
      write (6,afmt3) acds,(cds(i),i=1,kntr)
  560 kntr=0
      if (loc.ne.0) go to 530
  600 if (modedc.ne.1) go to 700
      if (kinel.eq.0) go to 610
      call sstf
  610 if (nsens.eq.0) go to 700
      call sencal
c
c  finished
c
  700 if (modedc.eq.2) go to 710
      if (jacflg.ne.0) go to 705
      call clrmem(lvnim1)
      call clrmem(lx0)
  705 call clrmem(lvn)
      call clrmem(ndiag)
  710 call second(t2)
      rstats(5)=rstats(5)+t2-t1
      return
      end
      subroutine sstf
      implicit double precision (a-h,o-z)
c
c     this routine computes the value of the small-signal transfer
c function specified by the user.
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
      common /dc/ tcstar(2),tcstop(2),tcincr(2),icvflg,itcelm(2),kssop,
     1   kinel,kidin,kovar,kidout
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension string(5),save(3)
      data aslash, ablnk / 1h/, 1h  /
c
c  setup current vector for input resistance and transfer function
c
      call zero8(value(lvn+1),nstop)
      if (kidin.eq.10) go to 5
c...  voltage source input
      iptri=nodplc(kinel+6)
      value(lvn+iptri)=+1.0d0
      go to 10
c...  current source input
    5 noposi=nodplc(kinel+2)
      nonegi=nodplc(kinel+3)
      value(lvn+noposi)=-1.0d0
      value(lvn+nonegi)=+1.0d0
c
c  lu decompose and solve the system of circuit equations
c
c...  reorder the right-hand side
   10 do 15 i=2,nstop
      j=nodplc(iswap+i)
      value(ndiag+i)=value(lvn+j)
   15 continue
      call copy8(value(ndiag+1),value(lvn+1),nstop)
   20 call dcdcmp
      call dcsol
      value(lvn+1)=0.0d0
c
c  evaluate transfer function
c
      if (nodplc(kovar+5).ne.0) go to 30
c...  voltage output
      noposo=nodplc(kovar+2)
      nonego=nodplc(kovar+3)
      trfn=value(lvn+noposo)-value(lvn+nonego)
      go to 40
c...  current output (through voltage source)
   30 iptro=nodplc(kovar+2)
      iptro=nodplc(iptro+6)
      trfn=value(lvn+iptro)
c
c  evaluate input resistance
c
   40 if (kidin.eq.9) go to 50
c...  current source input
      zin=value(lvn+nonegi)-value(lvn+noposi)
      go to 70
c...  voltage source input
   50 creal=value(lvn+iptri)
      if (dabs(creal).ge.1.0d-20) go to 60
      zin=1.0d20
      go to 70
   60 zin=-1.0d0/creal
c
c  setup current vector for output resistance
c
   70 call zero8(value(lvn+1),nstop)
      if (nodplc(kovar+5).ne.0) go to 80
c...  voltage output
      value(lvn+noposo)=-1.0d0
      value(lvn+nonego)=+1.0d0
      go to 90
   80 if (nodplc(kovar+2).ne.kinel) go to 85
      zout=zin
      go to 200
c...  current output (through voltage source)
   85 value(lvn+iptro)=+1.0d0
c
c  perform new forward and backward substitution
c
c...  reorder the right-hand side
   90 do 95 i=2,nstop
      j=nodplc(iswap+i)
      value(ndiag+i)=value(lvn+j)
   95 continue
      call copy8(value(ndiag+1),value(lvn+1),nstop)
      call dcsol
      value(lvn+1)=0.0d0
c
c  evaluate output resistance
c
  100 if (nodplc(kovar+5).ne.0) go to 110
c...  voltage output
      zout=value(lvn+nonego)-value(lvn+noposo)
      go to 200
c...  current output (through voltage source)
  110 creal=value(lvn+iptro)
      if (dabs(creal).ge.1.0d-20) go to 120
      zout=1.0d20
      go to 200
  120 zout=-1.0d0/creal
c
c  print results
c
  200 do 210 i=1,5
      string(i)=ablnk
  210 continue
      ipos=1
      call outnam(kovar,1,string,ipos)
      call copy8(string,save,3)
      call move(string,ipos,aslash,1,1)
      ipos=ipos+1
      locv=nodplc(kinel+1)
      anam=value(locv)
      call move(string,ipos,anam,1,8)
      write (6,231) string,trfn,anam,zin,save,zout
  231 format(////,'0****     small-signal characteristics'//,
     1   1h0,5x,5a8,3h = ,1pd10.3,/,
     2   1h0,5x,'input resistance at ',a8,12x,3h = ,d10.3,/,
     3   1h0,5x,'output resistance at ',2a8,a3,3h = ,d10.3)
      return
      end
      subroutine sencal
      implicit double precision (a-h,o-z)
c
c     this routine computes the dc sensitivities of circuit elements
c with respect to user specified outputs.
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
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /dc/ tcstar(2),tcstop(2),tcincr(2),icvflg,itcelm(2),kssop,
     1   kinel,kidin,kovar,kidout
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c
      dimension string(5),sentit(4)
      data alsrs,alsis,alsn,alsrb,alsrc,alsre / 2hrs,2his,1hn,2hrb,2hrc,
     1   2hre /
      data alsbf,alsc2,alsbr,alsc4,alsne,alsnc,alsik,alsikr,alsva,alsvb
     1   / 2hbf,3hjle,2hbr,3hjlc,3hnle,3hnlc,3hjbf,3hjbr,3hvbf,3hvbr/
      data alsjs /2hjs/
      data sentit / 8hdc sensi, 8htivity a, 8hnalysis , 8h         /
      data ablnk / 1h  /
c
c
      if (kinel.ne.0) go to 8
    4 call dcdcmp
c
c
    8 do 1000 n=1,nsens
c
c  prepare adjoint excitation vector
c
      call zero8(value(lvn+1),nstop)
      locs=nodplc(isens+n)
      ioutyp=nodplc(locs+5)
      if (ioutyp.ne.0) go to 10
c...  voltage output
      ivolts=1
      noposo=nodplc(locs+2)
      nonego=nodplc(locs+3)
      value(lvn+noposo)=-1.0d0
      value(lvn+nonego)=+1.0d0
      go to 20
c...  current output (through voltage source)
   10 iptro=nodplc(locs+2)
      ivolts=0
      iptro=nodplc(iptro+6)
      value(lvn+iptro)=-1.0d0
c
c  obtain adjoint solution by doing forward/backward substitution on
c  the transpose of the y matrix
c
   20 call asol
      value(lvn+1)=0.0d0
c
c  real solution in lvnim1;  adjoint solution in lvn ...
c
      call title(0,lwidth,1,sentit)
      ipos=1
      call outnam(locs,1,string,ipos)
      call move(string,ipos,ablnk,1,7)
      jstop=(ipos+6)/8
      write (6,36) (string(j),j=1,jstop)
   36 format('0dc sensitivities of output ',5a8)
      if(ivolts.ne.0) write (6,41)
      if(ivolts.eq.0) write(6,42)
   41 format(1h0,8x,'element',9x,'element',7x,'element',7x,'normalized'/
     1   10x,'name',12x,'value',6x,'sensitivity    sensitivity'/35x,
     2   ' (volts/unit) (volts/percent)'/)
   42 format(1h0,8x,'element',9x,'element',7x,'element',7x,'normalized'/
     1   10x,'name',12x,'value',6x,'sensitivity    sensitivity'/35x,
     2   '  (amps/unit)  (amps/percent)'/)
c
c  resistors
c
      loc=locate(1)
  100 if (loc.eq.0) go to 110
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      val=1.0d0/value(locv+1)
      sens=-(value(lvnim1+node1)-value(lvnim1+node2))*
     1      (value(lvn   +node1)-value(lvn   +node2))/(val*val)
      sensn=val*sens/100.0d0
      write (6,101) value(locv),val,sens,sensn
  101 format(10x,a8,4x,1pd10.3,5x,d10.3,5x,d10.3)
  105 loc=nodplc(loc)
      go to 100
c
c  voltage sources
c
  110 loc=locate(9)
  140 if (loc.eq.0) go to 150
      locv=nodplc(loc+1)
      val=value(locv+1)
      iptrv=nodplc(loc+6)
      sens=-value(lvn+iptrv)
      sensn=val*sens/100.0d0
      write (6,101) value(locv),val,sens,sensn
  145 loc=nodplc(loc)
      go to 140
c
c  current sources
c
  150 loc=locate(10)
  160 if (loc.eq.0) go to 170
      locv=nodplc(loc+1)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      val=value(locv+1)
      sens=value(lvn+node1)-value(lvn+node2)
      sensn=val*sens/100.0d0
      write (6,101) value(locv),val,sens,sensn
  165 loc=nodplc(loc)
      go to 160
c
c  diodes
c
  170 loc=locate(11)
  180 if (loc.eq.0) go to 210
      locv=nodplc(loc+1)
      write (6,181) value(locv)
  181 format(1x,a8)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      locm=nodplc(loc+5)
      locm=nodplc(locm+1)
      area=value(locv+1)
c
c  series resistance (rs)
c
      val=value(locm+2)*area
      if (val.ne.0.0d0) go to 190
      write (6,186) alsrs
  186 format(10x,a8,5x,2h0.,13x,2h0.,13x,2h0.)
      go to 200
  190 val=1.0d0/val
      sens=-(value(lvnim1+node1)-value(lvnim1+node3))*
     1      (value(lvn   +node1)-value(lvn   +node3))/(val*val)
      sensn=val*sens/100.0d0
      write (6,101) alsrs,val,sens,sensn
c
c  intrinsic parameters
c
  200 csat=value(locm+1)*area
      xn=value(locm+3)
      vbe=value(lvnim1+node3)-value(lvnim1+node2)
      vte=xn*vt
      evbe=dexp(vbe/vte)
      vabe=value(lvn+node3)-value(lvn+node2)
c
c  saturation current (is)
c
      sens=vabe*(evbe-1.0d0)
      sensn=csat*sens/100.0d0
      write (6,101) alsis,csat,sens,sensn
c
c  ideality factor (n)
c
      sens=-vabe*(csat/xn)*(vbe/vte)*evbe
      if (dabs(sens).lt.1.0d-50) sens=0.0d0
      sensn=xn*sens/100.0d0
      write (6,101) alsn,xn,sens,sensn
  205 loc=nodplc(loc)
      go to 180
c
c  bipolar junction transistors
c
  210 loc=locate(12)
  220 if (loc.eq.0) go to 1000
      locv=nodplc(loc+1)
      write (6,181) value(locv)
      node1=nodplc(loc+2)
      node2=nodplc(loc+3)
      node3=nodplc(loc+4)
      node4=nodplc(loc+5)
      node5=nodplc(loc+6)
      node6=nodplc(loc+7)
      locm=nodplc(loc+8)
      type=nodplc(locm+2)
      locm=nodplc(locm+1)
      loct=lx0+nodplc(loc+22)
      area=value(locv+1)
c
c  base resistance (rb)
c
      val=value(loct+16)
      if (val.ne.0.0d0) go to 230
      write (6,186) alsrb
      go to 240
  230 val=1.0d0/val
      sens=-(value(lvnim1+node2)-value(lvnim1+node5))*
     1      (value(lvn   +node2)-value(lvn   +node5))/(val*val)
      sensn=val*sens/100.0d0
      write (6,101) alsrb,val,sens,sensn
c
c  collector resistance (rc)
c
  240 val=value(locm+20)*area
      if (val.ne.0.0d0) go to 250
      write (6,186) alsrc
      go to 260
  250 val=1.0d0/val
      sens=-(value(lvnim1+node1)-value(lvnim1+node4))*
     1      (value(lvn   +node1)-value(lvn   +node4))/(val*val)
      sensn=val*sens/100.0d0
      write (6,101) alsrc,val,sens,sensn
c
c  emitter resistance (re)
c
  260 val=value(locm+19)*area
      if (val.ne.0.0d0) go to 270
      write (6,186) alsre
      go to 280
  270 val=1.0d0/val
      sens=-(value(lvnim1+node3)-value(lvnim1+node6))*
     1      (value(lvn   +node3)-value(lvn   +node6))/(val*val)
      sensn=val*sens/100.0d0
      write (6,101) alsre,val,sens,sensn
c
c  intrinsic parameters
c
  280 bf=value(locm+2)
      br=value(locm+8)
      csat=value(locm+1)*area
      ova=value(locm+4)
      ovb=value(locm+19)
      oik=value(locm+5)/area
      c2=value(locm+6)*area
      xne=value(locm+7)
      vte=xne*vt
      oikr=value(locm+11)/area
      c4=value(locm+12)*area
      xnc=value(locm+13)
      vtc=xnc*vt
      vbe=type*(value(lvnim1+node5)-value(lvnim1+node6))
      vbc=type*(value(lvnim1+node5)-value(lvnim1+node4))
      vabe=type*(value(lvn+node5)-value(lvn+node6))
      vabc=type*(value(lvn+node5)-value(lvn+node4))
      vace=vabe-vabc
      if (vbe.le.-vt) go to 320
      evbe=dexp(vbe/vt/value(locm+3))
      cbe=csat*(evbe-1.0d0)
      gbe=csat*evbe/vt/value(locm+3)
      if (c2.ne.0.0d0) go to 310
      cben=0.0d0
      gben=0.0d0
      go to 350
  310 evben=dexp(vbe/vte)
      cben=c2     *(evben-1.0d0)
      gben=c2     *evben/vte
      go to 350
  320 gbe=-csat/vbe
      cbe=gbe*vbe
      gben=-c2/vbe
      cben=gben*vbe
  350 if (vbc.le.-vt) go to 370
      evbc=dexp(vbc/vt/value(locm+9))
      cbc=csat*(evbc-1.0d0)
      gbc=csat*evbc/vt/value(locm+9)
      if (c4.ne.0.0d0) go to 360
      cbcn=0.0d0
      gbcn=0.0d0
      go to 400
  360 evbcn=dexp(vbc/vtc)
      cbcn=c4     *(evbcn-1.0d0)
      gbcn=c4     *evbcn/vtc
      go to 400
  370 gbc=-csat/vbc
      cbc=gbc*vbc
      gbcn=-c4/vbc
      cbcn=gbcn*vbc
  400 q1=1.0d0/(1.0d0-ova*vbc-ovb*vbe)
      q2=oik*cbe+oikr*cbc
      sqarg=dsqrt(1.0d0+4.0d0*q2)
      qb=q1*(1.0d0+sqarg)/2.0d0
      dqb=(cbe-cbc)/(qb*qb)
      sqarg=dsqrt(1.0d0+4.0d0*q2)
      dq1=dqb*(1.0d0+sqarg)+(q1*q1)/2.0d0
      dq2=q1*dqb/sqarg
c
c  compute sensitivities
c
c...  bf
      sens=-vabe*cbe/bf/bf
      sensn=bf*sens/100.0d0
      write (6,101) alsbf,bf,sens,sensn
c...  jle
      if (c2.ne.0.0d0) go to 430
      write (6,186) alsc2
      go to 440
  430 sens=vabe*cben/c2
      sensn=c2*sens/100.0d0
      write (6,101) alsc2,c2,sens,sensn
c...  br
  440 sens=-vabc*cbc/br/br
      sensn=br*sens/100.0d0
      write (6,101) alsbr,br,sens,sensn
c...  jlc
      if (c4.ne.0.0d0) go to 450
      write (6,186) alsc4
      go to 460
  450 sens=vabc*cbcn/c4
      sensn=c4*sens/100.0d0
      write (6,101) alsc4,c4,sens,sensn
c...  is
  460 sens=(vabe*(cbe/bf)+vabc*(cbc/br)
     1   +vace*(dqb*qb-dq2*q2))/csat
      sensn=csat*sens/100.0d0
      write (6,101) alsjs,csat,sens,sensn
c...  ne
      sens=-vabe*gben*vbe/xne
      sensn=xne*sens/100.0d0
      write (6,101) alsne,xne,sens,sensn
c...  nc
      sens=-vabc*gbcn*vbc/xnc
      sensn=xnc*sens/100.0d0
      write (6,101) alsnc,xnc,sens,sensn
c...  ik
      if (oik.ne.0.0d0) go to 470
      write (6,186) alsik
      go to 480
  470 val=1.0d0/oik
      sens=vace*dq2*cbe/(val*val)
      sensn=val*sens/100.0d0
      write (6,101) alsik,val,sens,sensn
c...  ikr
  480 if (oikr.ne.0.0d0) go to 490
      write (6,186) alsikr
      go to 500
  490 val=1.0d0/oikr
      sens=vace*dq2*cbc/(val*val)
      sensn=val*sens/100.0d0
      write (6,101) alsikr,val,sens,sensn
c...  va
  500 if (ova.ne.0.0d0) go to 510
      write (6,186) alsva
      go to 520
  510 va=1.0d0/ova
      sens=vace*dq1*vbc/(va*va)
      sensn=va*sens/100.0d0
      write (6,101) alsva,va,sens,sensn
c...  vb
  520 if (ovb.ne.0.0d0) go to 530
      write (6,186) alsvb
      go to 540
  530 vb=1.0d0/ovb
      sens=vace*dq1*vbe/(vb*vb)
      sensn=vb*sens/100.0d0
      write (6,101) alsvb,vb,sens,sensn
c
c
  540 loc=nodplc(loc)
      go to 220
c
c  finished
c
 1000 continue
      return
      end
      subroutine asol
      implicit double precision (a-h,o-z)
c
c     this routine evaluates the adjoint circuit response by doing a
c forward/backward substitution on the transpose of the coefficient
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
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c  forward substitution
c
      do 20 i=2,nstop
      io=nodplc(iorder+i)
      value(lvn+io)=value(lvn+io)/value(lynl+io)
      jstart=nodplc(iur+i)
      jstop=nodplc(iur+i+1)-1
      if (jstart.gt.jstop) go to 20
      if (value(lvn+io).eq.0.0d0) go to 20
      do 10 j=jstart,jstop
      jo=nodplc(iuc+j)
      jo=nodplc(iorder+jo)
      value(lvn+jo)=value(lvn+jo)-value(lyu+j)*value(lvn+io)
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
      value(lvn+io)=value(lvn+io)-value(lyl+j)*value(lvn+jo)
   30 continue
   40 continue
c
c  reorder right-hand side
c
      do 50 i=2,nstop
      j=nodplc(iswap+i)
      value(ndiag+i)=value(lvn+j)
   50 continue
      call copy8(value(ndiag+1),value(lvn+1),nstop)
c
c  finished
c
      return
      end
