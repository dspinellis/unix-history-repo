      subroutine readin
      implicit double precision (a-h,o-z)
c
c
c     this routine drives the input processing of spice.  element cards
c and device models are handled by this routine.
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
      common /cje/ maxtim,itime,icost
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
c  control card identifiers
c
      dimension aide(20),nnods(20),ntnods(20)
      dimension numic(4)
      dimension aidm(8),ipolar(8),modid(8),ipar(6),ampar(120)
      dimension titinp(4)
      dimension aidc(20)
      dimension alibm(6),alpar(46,6),dummy1(92),dummy2(92),dummy3(92)
      equivalence (alpar(1,1),dummy1(1)),(alpar(1,3),dummy2(1))
      equivalence (alpar(1,5),dummy3(1))
      data titinp / 8hinput li, 8hsting   , 8h        , 8h         /
      data naidc / 20 /
      data aidc / 8hac      , 8hdc      , 8hdistorti, 8hend     ,
     1            8hends    , 8hfourier , 8hmodel   , 8hnoise   ,
     2            8hop      , 8hoptions , 8hplot    , 8hprint   ,
     3            8hsubckt  , 8hsensitiv, 8htransien, 8htf      ,
     4            8htemperat, 8hwidth   , 8hnodeset , 8hic      /
c
c  element card identifiers, keywords, and information
c
      data aide / 1hr,1hc,1hl,1hk,1hg,1he,1hf,1hh,1hv,1hi,1hd,1hq,1hj,
     1   1hm,1hs,1hy,1ht,0.0d0,1hx,0.0d0 /
      data alsac,alspu,alsex,alssi /2hac,2hpu,2hex,2hsi/
      data alsoff,alsdc,alspw / 3hoff,2hdc,3hpw  /
      data alsz0,alszo,alsnl,alsf,alstd / 2hz0,2hzo,2hnl,1hf,2htd /
      data alsl,alsw,alsas,alsad,alsrd,alsrs / 1hl,1hw,2has,2had,2hrd,
     1  2hrs /
      data alszx /2hzx/
      data alssf / 4hsf   /
      data apoly, aic, area / 4hpoly, 2hic, 4harea /
      data alstc / 2htc /
      data numic / 1, 2, 2, 3 /
      data ablnk, aper / 1h , 1h. /
      data nnods / 2,2,2,0,2,2,2,2,2,2,2,3,3,4,4,4,4,0,0,0 /
      data ntnods / 2,2,2,0,2,2,2,2,2,2,3,6,5,6,4,4,4,0,0,0 /
c
c  model card keywords
c
      data aidm /1hd,3hnpn,3hpnp,3hnjf,3hpjf,4hnmos,4hpmos,4hgaas/
      data ipolar /0,1,-1,1,-1,1,-1,0/
      data modid /1,2,2,3,3,4,4,4/
      data ipar / 0, 14, 60, 72, 106, 119 /
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
      data alibm /7hlib001n,7hlib002n,7hlib239n,7hlib250n,7hlib255n,
     1  7hlib620n/
      data dummy1/
     * 1.60d-16, 1.08d+02, 1.04d+00, 1.34d+02, 1.54d-02, 1.20d-14,
     * 1.53d+00, 5.86d-01, 1.03d+00, 1.00d+10, 9.82d-03,    0.0d0,
     *    0.0d0,    0.0d0, 1.04d+00, 2.40d+02, 6.47d-04, 1.80d+02,
     * 3.90d+00, 9.20d+01, 1.90d-13, 6.70d-01, 3.33d-01, 1.51d-10,
     *    0.0d0,    0.0d0,    0.0d0,   9.14d0, 1.60d-13, 4.20d-01,
     * 3.33d-01, 9.70d-01, 1.51d-09,    0.0d0,    0.0d0,    0.0d0,
     *    0.0d0, 1.60d-13, 4.20d-01, 3.33d-01, 1.10d+00, 1.11d+00,
     *    0.0d0,    0.0d0,    0.0d0,    0.0d0,
     * 6.17d-16, 1.64d+02, 1.05d+00, 7.60d+01, 3.34d-02, 2.50d-14,
     * 1.49d+00, 1.36d+00, 1.05d+00, 1.00d+10, 2.15d-04,    0.0d0,
     *    0.0d0,    0.0d0, 1.05d+00, 7.33d+01, 2.14d-03, 3.00d+01,
     * 2.70d+00, 1.00d+01, 5.70d-13, 6.70d-01, 3.33d-01, 1.25d-10,
     *    0.0d0,    0.0d0,    0.0d0,  10.22d0, 5.20d-13, 4.80d-01,
     * 3.33d-01, 7.50d-01, 1.25d-09,    0.0d0,    0.0d0,    0.0d0,
     *    0.0d0, 5.20d-13, 4.80d-01, 3.33d-01, 1.10d+00, 1.11d+00,
     *    0.0d0,    0.0d0,    0.0d0,    0.0d0/
      data dummy2 /
     * 2.89d-16, 6.88d+01, 1.01d+00, 4.43d+01, 1.67d-01, 1.60d-13,
     * 1.85d+00, 3.32d-01, 1.10d+00, 1.00d+10, 3.42d-03,    0.0d0,
     *    0.0d0,    0.0d0, 1.01d+00, 2.00d+01, 8.18d-03, 1.00d+01,
     * 1.90d+00, 5.50d+00, 5.70d-13, 1.20d+00, 5.00d-01, 1.80d-11,
     *    0.0d0,    0.0d0,    0.0d0,   38.2d0, 5.60d-13, 8.20d-01,
     * 5.00d-01, 3.50d-01, 1.80d-10,    0.0d0,    0.0d0,    0.0d0,
     *    0.0d0, 5.60d-13, 8.20d-01, 5.00d-01, 1.10d+00, 1.11d+00,
     *    0.0d0,    0.0d0,    0.0d0,    0.0d0,
     * 1.90d-15, 1.02d+02, 1.08d+00, 5.50d+01, 7.63d-02, 8.80d-14,
     * 1.98d+00, 5.54d+00, 1.05d+00, 1.00d+10, 1.36d-02,    0.0d0,
     *    0.0d0,    0.0d0, 1.08d+00, 1.27d+02, 1.23d-03, 1.50d+01,
     * 2.50d+00, 6.00d+00, 4.80d-13, 1.20d+00, 5.00d-01, 4.40d-11,
     *    0.0d0,    0.0d0,    0.0d0,  16.15d0, 2.90d-13, 4.20d-01,
     * 3.33d-01, 6.10d-01, 4.40d-10,    0.0d0,    0.0d0,    0.0d0,
     *    0.0d0, 2.90d-13, 4.20d-01, 3.33d-01, 1.10d+00, 1.11d+00,
     *    0.0d0,    0.0d0,    0.0d0,    0.0d0/
      data dummy3 /
     * 1.90d-16, 1.00d+02, 1.00d+00, 5.70d+01, 4.17d-02, 4.80d-16,
     * 1.38d+00, 6.19d-01, 1.04d+00, 1.00d+10, 5.52d-03,    0.0d0,
     *    0.0d0,    0.0d0, 1.00d+00, 1.33d+02, 1.17d-03, 1.60d+01,
     * 2.40d+00, 7.00d+00, 3.20d-13, 1.20d+00, 5.00d-01, 4.70d-11,
     *    0.0d0,    0.0d0,    0.0d0,  14.51d0, 2.40d-13, 3.60d-01,
     * 3.33d-01, 4.50d-01, 4.70d-10,    0.0d0,    0.0d0,    0.0d0,
     *    0.0d0, 2.40d-13, 3.60d-01, 3.33d-01, 1.10d+00, 1.11d+00,
     *    0.0d0,    0.0d0,    0.0d0,    0.0d0,
     * 1.59d-15, 9.14d+01, 1.03d+00, 6.00d+01, 2.06d-01, 1.33d-16,
     * 1.23d+00, 1.27d+01, 1.00d+00, 1.00d+10, 5.39d-02,    0.0d0,
     *    0.0d0,    0.0d0, 1.03d+00, 8.33d+01, 1.87d-03, 8.00d+00,
     * 1.20d+00, 3.50d+00, 1.30d-12, 1.20d+00, 5.00d-01, 3.26d-11,
     *    0.0d0,    0.0d0,    0.0d0,  45.87d0, 7.50d-13, 5.50d-01,
     * 3.33d-01, 9.70d-01, 3.26d-10,    0.0d0,    0.0d0,    0.0d0,
     *    0.0d0, 7.50d-13, 5.50d-01, 3.33d-01, 1.10d+00, 1.11d+00,
     *    0.0d0,    0.0d0,    0.0d0,    0.0d0/
c
c  initialize variables
c
      call second(t1)
      call getlin
      if (keof.ne.0) go to 6000
      call copy8(afield,atitle,15)
      call getm4(ielmnt,0)
      call getm8(itemps,1)
      value(itemps+1)=25.0d0
      itemno=1
      nopage=0
      call title(-1,72,1,titinp)
      iwidth=80
      do 5 i=1,8
      achar=ablnk
      call move(achar,1,atitle(10),i,1)
      if(achar.eq.ablnk) go to 8
    5 continue
      write(6,6)
    6 format('0warning:  input line-width set to 72 columns because',/
     11x,'possible sequencing appears in cols 73-80')
      iwidth=72
    8 do 10 i=1,15
      afield(i)=ablnk
   10 continue
      call copy8(afield,oldlin,15)
      call getm4(isbckt,0)
      nsbckt=0
      call getm8(iunsat,0)
      nunsat=0
      lwidth=80
      iprnta=1
      iprntl=1
      iprntm=1
      iprntn=1
      iprnto=0
      gmin=1.0d-12
      reltol=0.001d0
      abstol=1.0d-12
      vntol=50.0d-6
      trtol=7.0d0
      chgtol=1.0d-14
      defl=1.0d0
      defw=1.0d0
      defad=1.0d-12
      defas=1.0d-12
      numdgt=4
      numtem=1
      itl1=100
      itl2=50
      itl3=4
      itl4=10
      itl5=5000
      limtim=2
      limpts=201
      lvlcod=2
      lvltim=1
      method=1
      xmu=0.5d0
      maxord=2
      nosolv=0
      icvflg=0
      itcelm(2)=0
      idist=0
      idprt=0
      inoise=0
      jacflg=0
      jtrflg=0
      call getm4(ifour,0)
      nfour=0
      call getm4(nsnod,0)
      call getm8(nsval,0)
      call getm4(icnod,0)
      call getm8(icval,0)
      kinel=0
      kovar=0
      kssop=0
      nosprt=0
      nsens=0
      call getm4(isens,0)
      numnod=0
      ncnods=0
      nunods=0
      call zero4(locate,50)
      call zero4(jelcnt,50)
      insize=50
      call getm8(ifield,insize)
      call getm4(icode,insize)
      call getm8(idelim,insize)
      call getm4(icolum,insize)
      go to 50
c
c  error entry
c
   40 nogo=1
c
c  read and decode next card in input deck
c
   50 igoof=0
      call card
      if (keof.ne.0) go to 5000
      if (igoof.ne.0) go to 40
      if (nodplc(icode+1).eq.0) go to 95
      anam=value(ifield+1)
      call move(anam,2,ablnk,1,7)
      if (anam.ne.aper) go to 70
      call move(anam,1,value(ifield+1),2,7)
      call keysrc(aidc,naidc,anam,id)
      if (id.le.0) go to 90
      if (id.eq.4) go to 5000
      if (id.eq.5) go to 800
      if (id.eq.7) go to 500
      if (id.eq.13) go to 700
      if (nsbckt.ge.1) go to 85
      call runcon(id)
      if (igoof.ne.0) go to 40
      go to 50
   70 id=0
   80 id=id+1
      if (id.gt.20) go to 90
      if (anam.eq.aide(id)) go to 100
      go to 80
   85 write (6,86)
   86 format('0warning:  above line not allowed within subcircuit -- ',
     1   'ignored'/)
      go to 50
   90 write (6,91) value(ifield+1)
   91 format('0*error*:  unknown data card:  ',a8/)
      go to 40
   95 write (6,96)
   96 format('0*error*:  unrecognizable data card'/)
      go to 40
c
c  element and device cards
c
  100 call find(value(ifield+1),id,loc,1)
      locv=nodplc(loc+1)
      if (id.eq.4) go to 140
      if (id.eq.19) go to 900
      istop=nnods(id)+1
      do 110 i=2,istop
      if (nodplc(icode+i).ne.0) go to 410
      if (value(ifield+i).lt.0.0d0) go to 400
  110 nodplc(loc+i)=value(ifield+i)
      go to (120,130,130,140,150,150,180,180,200,200,300,300,300,300,
     1   390,390,350,390,390,390), id
c
c  resistor
c
  120 if (nodplc(icode+4).ne.0) go to 420
      if (value(ifield+4).eq.0.0d0) go to 480
      value(locv+2)=value(ifield+4)
      ifld=4
  122 ifld=ifld+1
      if (nodplc(icode+ifld)) 50,122,124
  124 anam=value(ifield+ifld)
      if (anam.ne.alstc) go to 460
      ifld=ifld+1
      if (nodplc(icode+ifld)) 50,126,124
  126 value(locv+3)=value(ifield+ifld)
      ifld=ifld+1
      if (nodplc(icode+ifld)) 50,128,124
  128 value(locv+4)=value(ifield+ifld)
      go to 50
c
c  capacitor or inductor
c
  130 ifld=3
      iknt=0
      ltab=7
      if (id.eq.3) ltab=10
      call getm8(nodplc(loc+ltab),0)
      if (nodplc(icode+4).ne.1) go to 132
      anam=value(ifield+4)
      if (anam.ne.apoly) go to 450
      ifld=4
  132 ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 136
      call extmem(nodplc(loc+ltab),1)
      iknt=iknt+1
      ispot=nodplc(loc+ltab)+iknt
      value(ispot)=value(ifield+ifld)
      go to 132
  136 if (iknt.eq.0) go to 420
      if (nodplc(icode+ifld).ne.1) go to 50
      anam=value(ifield+ifld)
      if (anam.ne.aic) go to 460
      ifld=ifld+1
      if (nodplc(icode+ifld)) 50,138,136
  138 value(locv+2)=value(ifield+ifld)
      go to 50
c
c  mutual inductance
c
  140 if (nodplc(icode+2).ne.1) go to 430
      anam=value(ifield+2)
      call move(anam,2,ablnk,1,7)
      if (anam.ne.aide(3)) go to 430
      call extnam(value(ifield+2),nodplc(loc+2))
      if (nodplc(icode+3).ne.1) go to 430
      anam=value(ifield+3)
      call move(anam,2,ablnk,1,7)
      if (anam.ne.aide(3)) go to 430
      call extnam(value(ifield+3),nodplc(loc+3))
      if (nodplc(icode+4).ne.0) go to 420
      xk=value(ifield+4)
      if (xk.le.0.0d0) go to 420
      if (xk.le.1.0d0) go to 145
      xk=1.0d0
      write (6,141)
  141 format('0warning:  coefficient of coupling reset to 1.0d0'/)
  145 value(locv+1)=xk
      go to 50
c
c  voltage controlled (nonlinear) sources
c
  150 ndim=1
      ifld=3
      if (nodplc(icode+4)) 410,156,152
  152 anam=value(ifield+4)
      if (anam.ne.apoly) go to 450
      if (nodplc(icode+5).ne.0) go to 420
      ndim=value(ifield+5)
      if (ndim.le.0) go to 420
      ifld=5
  156 nodplc(loc+4)=ndim
      ltab=id+1
      nssnod=2*ndim
      nmat=4*ndim
      if (id.eq.6) nmat=4+2*ndim
      call getm4(nodplc(loc+ltab),nssnod)
      call getm4(nodplc(loc+ltab+1),nmat)
      call getm8(nodplc(loc+ltab+2),0)
      call getm8(nodplc(loc+ltab+3),ndim)
      call getm4(nodplc(loc+ltab+4),ndim)
      call getm8(nodplc(loc+ltab+5),ndim)
      ispot=nodplc(loc+ltab+5)
      call zero8(value(ispot+1),ndim)
      lnod=nodplc(loc+ltab)
      do 158 i=1,nssnod
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 410
      if (value(ifield+ifld).lt.0.0d0) go to 400
      nodplc(lnod+i)=value(ifield+ifld)
  158 continue
  160 iknt=0
  162 ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 164
      call extmem(nodplc(loc+ltab+2),1)
      iknt=iknt+1
      ispot=nodplc(loc+ltab+2)+iknt
      value(ispot)=value(ifield+ifld)
      go to 162
  164 if (iknt.eq.0) go to 420
      if (nodplc(icode+ifld).ne.1) go to 170
      anam=value(ifield+ifld)
      if (anam.ne.aic) go to 460
      do 168 i=1,ndim
      ifld=ifld+1
      if (nodplc(icode+ifld)) 170,166,420
  166 ispot=nodplc(loc+ltab+5)+i
      value(ispot)=value(ifield+ifld)
  168 continue
  170 if (ndim.ne.1) go to 50
      if (iknt.ne.1) go to 50
      call extmem(nodplc(loc+ltab+2),1)
      ispot=nodplc(loc+ltab+2)
      value(ispot+2)=value(ispot+1)
      value(ispot+1)=0.0d0
      go to 50
c
c  current controlled (nonlinear) sources
c
  180 ndim=1
      ifld=3
      if (nodplc(icode+4).ne.1) go to 470
      anam=value(ifield+4)
      if (anam.ne.apoly) go to 182
      ifld=5
      if (nodplc(icode+5).ne.0) go to 420
      ndim=value(ifield+5)
      if (ndim.le.0) go to 420
  182 nodplc(loc+4)=ndim
      ltab=id-1
      nmat=2*ndim
      if (id.eq.8) nmat=4+ndim
      call getm4(nodplc(loc+ltab),ndim)
      call getm4(nodplc(loc+ltab+1),nmat)
      call getm8(nodplc(loc+ltab+2),0)
      call getm8(nodplc(loc+ltab+3),ndim)
      call getm4(nodplc(loc+ltab+4),ndim)
      call getm8(nodplc(loc+ltab+5),ndim)
      ispot=nodplc(loc+ltab+5)
      call zero8(value(ispot+1),ndim)
      do 184 i=1,ndim
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.1) go to 470
      anam=value(ifield+ifld)
      call move(anam,2,ablnk,1,7)
      if (anam.ne.aide(9)) go to 470
      call extnam(value(ifield+ifld),loct)
      ispot=nodplc(loc+ltab)+i
      nodplc(ispot)=loct
  184 continue
      go to 160
c
c  independent sources
c
  200 ifld=3
      call getm8(nodplc(loc+5),0)
  210 ifld=ifld+1
  215 if (nodplc(icode+ifld)) 50,220,230
  220 if (ifld.gt.4) go to 210
  225 value(locv+1)=value(ifield+ifld)
      go to 210
  230 anam=value(ifield+ifld)
      if (anam.ne.alsdc) go to 235
      ifld=ifld+1
      if (nodplc(icode+ifld)) 50,225,230
  235 if (anam.ne.alsac) go to 260
      value(locv+2)=1.0d0
      ifld=ifld+1
      if (nodplc(icode+ifld)) 50,240,230
  240 value(locv+2)=value(ifield+ifld)
      ifld=ifld+1
      if (nodplc(icode+ifld)) 50,250,230
  250 value(locv+3)=value(ifield+ifld)
      go to 210
  260 id=0
      call move(anam,3,ablnk,1,6)
      if (anam.eq.alspu) id=1
      if (anam.eq.alssi) id=2
      if (anam.eq.alsex) id=3
      if (anam.eq.alspw) id=4
      if (anam.eq.alssf) id=5
      if (id.eq.0) go to 450
      nodplc(loc+4)=id
      iknt=0
  270 ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 280
      call extmem(nodplc(loc+5),1)
      iknt=iknt+1
      ispot=nodplc(loc+5)+iknt
      value(ispot)=value(ifield+ifld)
      go to 270
  280 aval=0.0d0
      if (id.ne.4) go to 285
c...  for pwl source function, force even number of input values
      ibit=0
      if(iknt.ne.(iknt/2)*2) ibit=1
      aval=value(ispot)
      if (ibit.eq.0) go to 290
      call extmem(nodplc(loc+5),1)
      aval=value(ispot-1)
      iknt=iknt+1
      ispot=nodplc(loc+5)+iknt
      value(ispot)=aval
      go to 290
  285 if (iknt.ge.7) go to 215
  290 call extmem(nodplc(loc+5),2)
      ispot=nodplc(loc+5)+iknt
      value(ispot+1)=0.0d0
      value(ispot+2)=aval
      iknt=iknt+2
      go to 285
c
c  device cards
c
  300 if(id.ne.14) value(locv+1)=1.0d0
      locm=loc+ntnods(id)+2
      ifld=nnods(id)+2
c
c  temporarily (until modchk) put substrate node into nodplc(loc+5)
c
      if(id.ne.12) go to 308
      if(nodplc(icode+5).ne.0) go to 308
      ifld=6
      nodplc(loc+5)=value(ifield+5)
  308 continue
      if (nodplc(icode+ifld).ne.1) go to 440
      call extnam(value(ifield+ifld),nodplc(locm))
  310 ifld=ifld+1
      if (nodplc(icode+ifld)) 50,325,315
  315 anam=value(ifield+ifld)
      if (anam.ne.alsoff) go to 320
      nodplc(locm+1)=1
      go to 310
  320 if (anam.ne.area) go to 330
      ifld=ifld+1
      if (nodplc(icode+ifld)) 50,325,315
  325 if (value(ifield+ifld).le.0.0d0) go to 420
      if (id.eq.14) go to 343
      value(locv+1)=value(ifield+ifld)
      go to 310
  330 if (anam.ne.aic) go to 341
      iknt=0
      icloc=0
      if (id.eq.14) icloc=3
      maxknt=numic(id-10)
  335 ifld=ifld+1
      if (nodplc(icode+ifld)) 50,340,315
  340 iknt=iknt+1
      if (iknt.gt.maxknt) go to 335
      value(locv+icloc+iknt+1)=value(ifield+ifld)
      go to 335
  341 if (id.ne.14) go to 460
      ispot=0
      if (anam.eq.alsl) ispot=1
      if (anam.eq.alsw) ispot=2
      if (anam.eq.alsad) ispot=3
      if(anam.eq.alszx) ispot=3
      if (anam.eq.alsas) ispot=4
      if(anam.eq.alsrd) ispot=11
      if(anam.eq.alsrs) ispot=12
      if (ispot.eq.0) go to 460
      ifld=ifld+1
      if (nodplc(icode+ifld)) 50,342,315
  342 if (value(ifield+ifld).le.0.0d0) go to 420
      value(locv+ispot)=value(ifield+ifld)
      go to 310
  343 iknt=0
  344 iknt=iknt+1
      if(value(ifield+ifld).le.0.0d0) go to 420
      if(iknt.gt.12) go to 490
      if(iknt.eq.5) iknt=11
      value(locv+iknt)=value(ifield+ifld)
      ifld=ifld+1
      if(nodplc(icode+ifld)) 345,344,345
  345 if(nodplc(icode+ifld)) 50,50,315
c
c  transmission lines
c
  350 ifld=5
      xnl=0.25d0
      tfreq=0.0d0
  355 ifld=ifld+1
      if (nodplc(icode+ifld)) 378,355,360
  360 anam=value(ifield+ifld)
      if (anam.eq.aic) go to 364
      if (anam.eq.alsnl) go to 370
      if (anam.eq.alsf) go to 374
      id=0
      if (anam.eq.alsz0) id=1
      if (anam.eq.alszo) id=1
      if (anam.eq.alstd) id=2
      if (id.eq.0) go to 460
      ifld=ifld+1
      if (nodplc(icode+ifld)) 378,362,360
  362 if (value(ifield+ifld).le.0.0d0) go to 420
      value(locv+id)=value(ifield+ifld)
      go to 355
  364 iknt=0
  366 ifld=ifld+1
      if (nodplc(icode+ifld)) 378,368,360
  368 iknt=iknt+1
      if (iknt.gt.4) go to 366
      value(locv+iknt+4)=value(ifield+ifld)
      go to 366
  370 ifld=ifld+1
      if (nodplc(icode+ifld)) 378,372,360
  372 if (value(ifield+ifld).le.0.0d0) go to 420
      xnl=value(ifield+ifld)
      go to 355
  374 ifld=ifld+1
      if (nodplc(icode+ifld)) 378,376,360
  376 if (value(ifield+ifld).le.0.0d0) go to 420
      tfreq=value(ifield+ifld)
      go to 355
  378 if (value(locv+1).ne.0.0d0) go to 380
      write (6,379)
  379 format('0*error*:  z0 must be specified'/)
      go to 40
  380 if (value(locv+2).ne.0.0d0) go to 50
      if (tfreq.ne.0.0d0) go to 382
      write (6,381)
  381 format('0*error*:  either td or f must be specified'/)
      go to 40
  382 value(locv+2)=xnl/tfreq
      go to 50
c
c  elements not yet implemented
c
  390 write (6,391)
  391 format('0*error*:  element type not yet implemented'/)
      go to 40
c
c  element card errors
c
  400 write (6,401)
  401 format('0*error*:  negative node number found'/)
      go to 40
  410 write (6,411)
  411 format('0*error*:  node numbers are missing'/)
      go to 40
  420 write (6,421)
  421 format('0*error*:  value is missing or is nonpositive'/)
      go to 40
  430 write (6,431)
  431 format('0*error*:  mutual inductance references are missing'/)
      go to 40
  440 write (6,441)
  441 format('0*error*:  model name is missing'/)
      go to 40
  450 write (6,451) anam
  451 format('0*error*:  unknown source function:  ',a8)
      go to 40
  460 write (6,461) anam
  461 format('0*error*:  unknown parameter:  ',a8/)
      go to 40
  470 write (6,471)
  471 format('0*error*:  voltage source not found on above line'/)
      go to 40
  480 write (6,481)
  481 format('0*error*:  value is zero'/)
      go to 40
  490 write(6,491)
  491 format('0*error*:  extra numerical data on mosfet card'/)
      go to 40
c
c  model card
c
  500 if (nodplc(icode+2).ne.1) go to 650
      if (nodplc(icode+3).ne.1) go to 650
c
c  process for library models
c
      iknt=0
  502 iknt=iknt+1
      if(iknt.gt.6) go to 506
      if(alibm(iknt).ne.value(ifield+3)) go to 502
      ipol=ipolar(2)
      jtype=modid(2)
      id=jtype+20
      call find(value(ifield+2),id,loc,1)
      nodplc(loc+2)=ipol
      locv=nodplc(loc+1)
      do 504 i=1,46
  504 value(locv+i)=alpar(i,iknt)
      go to 520
  506 id=0
  510 id=id+1
      if (id.gt.8) go to 660
      if (value(ifield+3).ne.aidm(id)) go to 510
      ipol=ipolar(id)
      jtype=modid(id)
      id=jtype+20
      call find(value(ifield+2),id,loc,1)
c... adjust jtype for gaas
      if(jtype.eq.4.and.ipol.eq.0) jtype=5
      nodplc(loc+2)=ipol
      locv=nodplc(loc+1)
  520 locm=ipar(jtype)
      nopar=ipar(jtype+1)-locm
      ifld=3
  530 ifld=ifld+1
      if (nodplc(icode+ifld)) 50,530,560
  560 anam=value(ifield+ifld)
      if(jtype.eq.2) anam=alias(anam)
      iknt=0
  570 iknt=iknt+1
      if (iknt.gt.nopar) go to 670
      if (anam.ne.ampar(locm+iknt)) go to 570
      ifld=ifld+1
      if (nodplc(icode+ifld)) 50,580,560
  580 value(locv+iknt)=value(ifield+ifld)
      ifld=ifld+1
      if (nodplc(icode+ifld)) 50,590,560
  590 iknt=iknt+1
      if (iknt.gt.nopar) go to 530
      if (ablnk.ne.ampar(locm+iknt)) go to 530
      go to 580
c
c  model card errors
c
  650 write (6,651)
  651 format('0*error*:  model type is missing'/)
      go to 40
  660 write (6,661) value(ifield+3)
  661 format('0*error*:  unknown model type:  ',a8/)
      go to 40
  670 write (6,671) anam
  671 format('0*error*:  unknown model parameter:  ',a8,/)
      nogo=1
      go to 530
c
c  subcircuit definition
c
  700 if (nodplc(icode+2).ne.1) go to 780
      call find(value(ifield+2),20,loc,1)
      call extmem(isbckt,1)
      nsbckt=nsbckt+1
      nodplc(isbckt+nsbckt)=loc
      ifld=2
      if (nodplc(icode+3).ne.0) go to 790
      call getm4(nodplc(loc+2),0)
      iknt=0
  710 ifld=ifld+1
      if (nodplc(icode+ifld)) 50,720,710
  720 call extmem(nodplc(loc+2),1)
      iknt=iknt+1
      ispot=nodplc(loc+2)+iknt
      if (value(ifield+ifld).le.0.0d0) go to 770
      nodplc(ispot)=value(ifield+ifld)
      node=nodplc(ispot)
      i=iknt-1
  730 if (i.eq.0) go to 710
      ispot=ispot-1
      if (nodplc(ispot).eq.node) go to 760
      i=i-1
      go to 730
  760 write (6,761) node
  761 format('0*error*:  subcircuit definition duplicates node ',i5,/)
      go to 40
  770 write (6,771)
  771 format('0*error*:  nonpositive node number found in subcircuit ',
     1   'definition'/)
      go to 40
  780 write (6,781)
  781 format('0*error*:  subcircuit name missing'/)
      go to 40
  790 write (6,791)
  791 format('0*error*:  subcircuit nodes missing'/)
      go to 40
c
c  .ends processing
c
  800 if (nsbckt.eq.0) go to 890
      iknt=1
      if (nodplc(icode+2).le.0) go to 820
      anam=value(ifield+2)
      iknt=nsbckt
  810 loc=nodplc(isbckt+iknt)
      locv=nodplc(loc+1)
      anams=value(locv)
      if (anam.eq.anams) go to 820
      iknt=iknt-1
      if (iknt.ne.0) go to 810
      go to 880
  820 irel=nsbckt-iknt+1
      call relmem(isbckt,irel)
      nsbckt=nsbckt-irel
      go to 50
  880 write (6,881) anam
  881 format('0*error*:  unknown subcircuit name:  ',a8/)
      go to 40
  890 write (6,891)
  891 format('0warning:  no subcircuit definition known -- line ignored'
     1/)
      go to 50
c
c  subcircuit call
c
  900 call getm4(nodplc(loc+2),0)
      ifld=1
      iknt=0
  910 ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 920
      call extmem(nodplc(loc+2),1)
      iknt=iknt+1
      ispot=nodplc(loc+2)+iknt
      if (value(ifield+ifld).lt.0.0d0) go to 400
      nodplc(ispot)=value(ifield+ifld)
      go to 910
  920 if (iknt.eq.0) go to 410
      if (nodplc(icode+ifld).ne.1) go to 990
      call extnam(value(ifield+ifld),nodplc(loc+3))
      go to 50
  990 write (6,991)
  991 format('0*error*:  subcircuit name missing'/)
      go to 40
c
c  end
c
 5000 if (nsbckt.eq.0) go to 5010
      nsbckt=0
      write (6,5001)
 5001 format('0*error*:  .ends  card missing'/)
      nogo=1
 5010 call clrmem(ifield)
      call clrmem(icode)
      call clrmem(idelim)
      call clrmem(icolum)
      call clrmem(isbckt)
      if (nfour.eq.0) call clrmem(ifour)
      if (nsens.eq.0) call clrmem(isens)
 6000 call second(t2)
      rstats(1)=t2-t1
      return
      end
      double precision function alias(anam)
      implicit double precision (a-h,o-z)
      dimension anam1(15),anam2(15)
      data anam1 /3his ,3hva ,3hne ,3hvb ,3hnc ,3hccs,3hns ,
     1            3hpe ,3hme ,3hpc ,3hmc ,3hps ,3hms ,3hik ,3hikr/
      data anam2 /3hjs ,3hvbf,3hnle,3hvbr,3hnlc,3hcjs,3hnss,
     1            3hvje,3hmje,3hvjc,3hmjc,3hvjs,3hmjs,3hjbf,3hjbr/
c
c  this function returns the mgp equivalent of the gp parameters
c  (those which apply)
c
      iknt=0
      alias=anam
   10 iknt=iknt+1
      if(iknt.gt.15) return
      if(anam1(iknt).ne.anam) go to 10
      alias=anam2(iknt)
      return
      end
      subroutine keysrc(keytab,lentab,tstwrd,index)
      implicit double precision (a-h,o-z)
      double precision keytab
c
c     this routine searches the keyword table 'keytab' for the possible
c entry 'tstwrd'.  abbreviations are considered as matches.
c
      dimension keytab(lentab)
      integer xxor
      data ablnk / 1h  /
c
c
      index=0
      lenwrd=0
      achar=ablnk
      do 10 i=1,8
      call move(achar,8,tstwrd,i,1)
      if (achar.eq.ablnk) go to 20
      lenwrd=lenwrd+1
   10 continue
c
   20 if (lenwrd.eq.0) go to 40
      tstchr=ablnk
      call move(tstchr,8,tstwrd,1,1)
   30 index=index+1
      if (index.gt.lentab) go to 40
      akey=ablnk
      call move(akey,1,keytab(index),1,lenwrd)
      if (xxor(akey,tstwrd).eq.0) go to 50
      go to 30
c
   40 index=-1
   50 return
      end
      subroutine extnam(aname,index)
      implicit double precision (a-h,o-z)
c
c     this routine adds 'aname' to the list of 'unsatisfied' names (that
c is, names which can only be resolved after subcircuit expansion).
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
      integer xxor
c
c
      anam=aname
      if (nunsat.eq.0) go to 20
      do 10 index=1,nunsat
      if (xxor(anam,value(iunsat+index)).eq.0) go to 30
   10 continue
c
   20 call extmem(iunsat,1)
      nunsat=nunsat+1
      index=nunsat
      value(iunsat+index)=anam
   30 return
      end
      subroutine runcon(id)
      implicit double precision (a-h,o-z)
c
c     this routine processes run control cards.
c
      common /tabinf/ ielmnt,isbckt,nsbckt,iunsat,nunsat,itemps,numtem,
     1   isens,nsens,ifour,nfour,ifield,icode,idelim,icolum,insize,
     2   junode,lsbkpt,numbkp,iorder,jmnode,iur,iuc,ilc,ilr,numoff,isr,
     3   nmoffc,iseq,iseq1,neqn,nodevs,ndiag,iswap,iequa,macins,lvnim1,
     4   lx0,lvn,lynl,lyu,lyl,lx1,lx2,lx3,lx4,lx5,lx6,lx7,ld0,ld1,ltd,
     5   imynl,imvn,lcvn,loutpt,nsnod,nsmat,nsval,icnod,icmat,icval
      common /cje/ maxtim,itime,icost
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
      dimension iprnt(5),limits(4),itrlim(5),contol(6),dflts(4)
      equivalence (iprnt(1),iprnta),(limits(1),limtim),(itrlim(1),itl1),
     1   (contol(1),gmin),(dflts(1),defl)
c
c
      integer xxor
c
c  print/plot keywords
c
      dimension aopt(5)
      dimension aopts(31),lsetop(5)
      dimension aide(20)
      data aopt / 2hdc, 2htr, 2hac, 2hno, 2hdi /
c
c  options card keywords
c
      data aopts / 6hnoacct, 6hnolist, 6hnomod , 6hnonode, 6hopts  ,
     1             6hitl1  , 6hitl2  , 6hitl3  , 6hitl4  , 6hitl5  ,
     2             6hlimtim, 6hlimpts, 6hlvlcod, 6hlvltim, 6hgmin  ,
     3             6hreltol, 6habstol, 6hvntol , 6htrtol , 6hchgtol,
     4             6htnom  , 6hnumdgt, 6hmaxord, 6hmethod, 6hnopage,
     5             6hmu    , 6hcptime, 6hdefl  , 6hdefw  , 6hdefad ,
     6             6hdefas /
      data lsetop / 0 ,0, 0, 0, 1 /
c
c
      data aide / 1hr,1hc,1hl,1hk,1hg,1he,1hf,1hh,1hv,1hi,1hd,1hq,1hj,
     1   1hm,1hs,1hy,1ht,0.0d0,1hx,0.0d0 /
      data alsde,alsoc,alsli / 3hdec, 3hoct, 3hlin /
      data atrap, agear, auic / 4htrap, 4hgear, 3huic /
      data ablnk, ain, aout / 1h , 2hin, 3hout /
      data amiss / 8h*missing /
      data ams / 2hms /
      data minpts / 1 /
c
c
      go to (1200,1100,1650,6000,6000,1700,6000,1600,1550,2000,3600,
     1   3500,6000,1750,1300,1500,1800,4000,4100,4200), id
c
c  dc transfer curves
c
 1100 ifld=2
      icvflg=0
      inum=1
 1105 anam=value(ifield+ifld)
      if(inum.gt.2) go to 6000
      id=0
      call move(anam,2,ablnk,1,7)
      if (anam.eq.aide(9)) id=9
      if (anam.eq.aide(10)) id=10
      if (id.eq.0) go to 1130
      call find(value(ifield+ifld),id,itcelm(inum),0)
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 1130
      tcstar(inum)=value(ifield+ifld)
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 1130
      tcstop(inum)=value(ifield+ifld)
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 1130
      tcincr(inum)=value(ifield+ifld)
      if (tcincr(inum).eq.0.0d0) go to 1130
      temp=(tcstop(inum)-tcstar(inum))/tcincr(inum)
      if (temp.gt.0.0d0) go to 1110
      tcincr(inum)=-tcincr(inum)
      temp=-temp
 1110 itemp=idint(temp+0.5d0)+1
      itemp=max0(itemp,minpts)
      if(inum.eq.1) icvflg=itemp
      if(inum.eq.2) icvflg=itemp*icvflg
      ifld=ifld+1
      inum=2
      if(nodplc(icode+ifld)) 6000,1130,1105
 1130 write (6,1131)
      icvflg=0
 1131 format('0warning:  missing parameter(s) ... analysis omitted'/)
      go to 6000
c
c  frequency specification
c
 1200 ifld=2
      if (nodplc(icode+2)) 1250,1250,1210
 1210 id=0
      if (value(ifield+ifld).eq.alsde) id=1
      if (value(ifield+ifld).eq.alsoc) id=2
      if (value(ifield+ifld).eq.alsli) id=3
      if (id.eq.0) go to 1240
      idfreq=id
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 1250
      if (value(ifield+ifld).le.0.0d0) go to 1250
      fincr=value(ifield+ifld)
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 1250
      if (value(ifield+ifld).le.0.0d0) go to 1250
      fstart=value(ifield+ifld)
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 1250
      if (value(ifield+ifld).le.0.0d0) go to 1250
      fstop=value(ifield+ifld)
      if (fstart.gt.fstop) go to 1260
      jacflg=fincr
      if (idfreq-2) 1215,1220,1235
 1215 fincr=dexp(xlog10/fincr)
      go to 1230
 1220 fincr=dexp(xlog2/fincr)
 1230 temp=dlog(fstop/fstart)/dlog(fincr)
      jacflg=idint(temp+0.999d0)+1
 1235 jacflg=max0(jacflg,minpts)
      if (idfreq.ne.3) go to 6000
      fincr=(fstop-fstart)/dfloat(max0(jacflg-1,1))
      go to 6000
 1240 write (6,1241) value(ifield+ifld)
 1241 format('0warning:  unknown frequency function:  ',a8,' ... analys'
     1   ,'is omitted'/)
      go to 6000
 1250 write (6,1251)
 1251 format('0warning:  frequency parameters incorrect ... analysis om'
     1   ,'itted'/)
      go to 6000
 1260 write (6,1261)
 1261 format('0warning:  start freq > stop freq ... analysis omitted'/)
      go to 6000
c
c  time specification
c
 1300 ifld=2
      if (nodplc(icode+ifld).ne.0) go to 1430
      if (value(ifield+ifld).le.0.0d0) go to 1430
      tstep=value(ifield+ifld)
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 1430
      if (value(ifield+ifld).le.0.0d0) go to 1430
      tstop=value(ifield+ifld)
      tstart=0.0d0
      delmax=tstop/50.0d0
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 1310
      if (value(ifield+ifld).lt.0.0d0) go to 1430
      tstart=value(ifield+ifld)
      delmax=(tstop-tstart)/50.0d0
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 1310
      if (value(ifield+ifld).le.0.0d0) go to 1430
      delmax=value(ifield+ifld)
      ifld=ifld+1
 1310 if (nodplc(icode+ifld).ne.1) go to 1320
      if (value(ifield+ifld).ne.auic) go to 1320
      nosolv=1
 1320 if (tstart.gt.tstop) go to 1440
      if (tstep.gt.tstop) go to 1430
      jtrflg=idint((tstop-tstart)/tstep+0.5d0)+1
      jtrflg=max0(jtrflg,minpts)
      go to 6000
 1430 write (6,1431)
 1431 format('0warning:  time parameters incorrect ... analysis omitted'
     1   /)
      go to 6000
 1440 write (6,1441)
 1441 format('0warning:  start time > stop time ... analysis omitted'/)
      go to 6000
c
c  transfer function
c
 1500 kssop=1
      ifld=2
      if (nodplc(icode+ifld).ne.1) go to 1530
      call outdef(ifld,1,kovar,ktype)
      if (igoof.ne.0) go to 1530
      if (ktype.ne.1) go to 1540
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.1) go to 1530
      anam=value(ifield+ifld)
      call move(anam,2,ablnk,1,7)
      id=0
      if (anam.eq.aide(9)) id=9
      if (anam.eq.aide(10)) id=10
      if (id.eq.0) go to 1530
      call find(value(ifield+ifld),id,kinel,0)
      kidin=id
      go to 6000
 1530 kovar=0
      kinel=0
      write (6,1131)
      igoof=0
      go to 6000
 1540 kovar=0
      kinel=0
      write (6,1541)
 1541 format('0warning:  illegal output variable ... analysis omitted'/)
      igoof=0
      go to 6000
c
c  operating point
c
 1550 kssop=1
      go to 6000
c
c  noise analysis
c
 1600 ifld=2
      if (nodplc(icode+ifld).ne.1) go to 1610
      call outdef(ifld,2,nosout,ntype)
      if (igoof.ne.0) go to 1610
      if (ntype.ne.1) go to 1610
      if (nodplc(nosout+5).ne.0) go to 1610
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.1) go to 1620
      anam=value(ifield+ifld)
      call move(anam,2,ablnk,1,7)
      id=0
      if (anam.eq.aide(9)) id=9
      if (anam.eq.aide(10)) id=10
      if (id.eq.0) go to 1620
      call find(value(ifield+ifld),id,nosin,0)
      nosprt=0
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 1605
      nosprt=dmax1(0.0d0,value(ifield+ifld))
 1605 inoise=1
      go to 6000
 1610 write (6,1611)
 1611 format('0warning:  voltage output unrecognizable ... analysis omit
     1ted'/)
      igoof=0
      go to 6000
 1620 write (6,1621)
 1621 format('0warning:  invalid input source ... analysis omitted'/)
      igoof=0
      go to 6000
c
c  distortion analysis
c
 1650 ifld=2
      if (nodplc(icode+ifld).ne.1) go to 1660
      anam=value(ifield+ifld)
      call move(anam,2,ablnk,1,7)
      if (anam.ne.aide(1)) go to 1660
      call find(value(ifield+ifld),1,idist,0)
      idprt=0
      skw2=0.9d0
      refprl=1.0d-3
      spw2=1.0d0
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 6000
      idprt=value(ifield+ifld)
      idprt=max0(idprt,0)
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 6000
      if (value(ifield+ifld).le.0.001d0) go to 1670
      if (value(ifield+ifld).gt.0.999d0) go to 1670
      skw2=value(ifield+ifld)
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 6000
      if (value(ifield+ifld).lt.1.0d-10) go to 1670
      refprl=value(ifield+ifld)
      ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 6000
      if (value(ifield+ifld).lt.0.001d0) go to 1670
      spw2=value(ifield+ifld)
      go to 6000
 1660 write (6,1661)
 1661 format('0warning:  distortion load resistor missing ... analysis '
     1   ,'omitted'/)
      go to 6000
 1670 idist=0
      write (6,1671)
 1671 format('0warning:  distortion parameters incorrect ... analysis o'
     1   ,'mitted'/)
      go to 6000
c
c  fourier analysis
c
 1700 ifld=2
      if (nodplc(icode+ifld).ne.0) go to 1720
      if (value(ifield+ifld).le.0.0d0) go to 1720
      forfre=value(ifield+ifld)
 1705 ifld=ifld+1
      if (nodplc(icode+ifld).ne.1) go to 1710
      call outdef(ifld,2,loct,ltype)
      if (igoof.ne.0) go to 1720
      if (ltype.ne.1) go to 1720
      call extmem(ifour,1)
      nfour=nfour+1
      nodplc(ifour+nfour)=loct
      go to 1705
 1710 if (nfour.ge.1) go to 6000
 1720 write (6,1721)
 1721 format('0warning:  fourier parameters incorrect ... analysis omit'
     1   ,'ted'/)
      igoof=0
      nfour=0
      call clrmem(ifour)
      call getm4(ifour,0)
      go to 6000
c
c  sensitivity analysis
c
 1750 kssop=1
      ifld=1
 1760 ifld=ifld+1
      if (nodplc(icode+ifld).ne.1) go to 6000
      call outdef(ifld,1,loct,ltype)
      if (igoof.ne.0) go to 1780
      if (ltype.ne.1) go to 1780
      call extmem(isens,1)
      nsens=nsens+1
      nodplc(isens+nsens)=loct
      go to 1760
 1780 write (6,1781)
 1781 format('0warning:  output variable unrecognizable ... analysis om'
     1   ,'mitted'/)
      igoof=0
      nsens=0
      call clrmem(isens)
      call getm4(isens,0)
      go to 6000
c
c  temperature variation
c
 1800 ifld=1
 1810 ifld=ifld+1
      if (nodplc(icode+ifld).ne.0) go to 6000
      if (value(ifield+ifld).le.-223.0d0) go to 1810
      call extmem(itemps,1)
      numtem=numtem+1
      value(itemps+numtem)=value(ifield+ifld)
      go to 1810
c
c  options card
c
 2000 ifld=1
 2010 ifld=ifld+1
 2020 if (nodplc(icode+ifld)) 6000,2010,2030
 2030 anam=value(ifield+ifld)
      do 2040 i=1,5
      if (anam.ne.aopts(i)) go to 2040
      iprnt(i)=lsetop(i)
      ifld=ifld+1
      if(nodplc(icode+ifld).ne.0) go to 2020
      iprnt(i)=value(ifield+ifld)
      go to 2010
 2040 continue
      if (anam.eq.aopts(24)) go to 2110
      if (anam.eq.aopts(25)) go to 2120
      if(anam.eq.aopts(26)) go to 2130
      if(anam.eq.aopts(27)) go to 2150
      if (nodplc(icode+ifld+1).ne.0) go to 2510
      ifld=ifld+1
      aval=value(ifield+ifld)
      do 2050 i=6,10
      if (anam.ne.aopts(i)) go to 2050
      if(aval.le.0.0d0.and.i.ne.10) go to 2510
      itrlim(i-5)=aval
      go to 2010
 2050 continue
      if (aval.le.0.0d0) go to 2510
      do 2060 i=11,14
      if (anam.ne.aopts(i)) go to 2060
      limits(i-10)=aval
      go to 2010
 2060 continue
      do 2070 i=15,20
      if (anam.ne.aopts(i)) go to 2070
      contol(i-14)=aval
      go to 2010
 2070 continue
      do 2075 i=28,31
      if(anam.ne.aopts(i)) go to 2075
      dflts(i-27)=aval
      go to 2010
 2075 continue
      if (anam.ne.aopts(21)) go to 2080
      if (aval.lt.-223.0d0) go to 2510
      value(itemps+1)=aval
      go to 2010
 2080 if (anam.ne.aopts(22)) go to 2100
      ndigit=aval
      if (ndigit.le.7) go to 2090
      ndigit=7
      write (6,2081) ndigit
 2081 format('0warning:  numdgt may not exceed',i2,
     1 ';  maximum value assumed'/)
 2090 numdgt=ndigit
      go to 2010
 2100 if (anam.ne.aopts(23)) go to 2500
      n=aval
      if ((n.le.1).or.(n.ge.7)) go to 2510
      maxord=n
      go to 2010
 2110 if (nodplc(icode+ifld+1).ne.1) go to 2510
      ifld=ifld+1
      anam=value(ifield+ifld)
      call move(anam,5,ablnk,1,4)
      jtype=0
      if (anam.eq.atrap) jtype=1
      if (anam.eq.agear) jtype=2
      if (jtype.eq.0) go to 2510
      method=jtype
      go to 2010
 2120 nopage=1
      go to 2010
 2130 ifld=ifld+1
      if(nodplc(icode+ifld)) 6000,2140,2010
 2140 aval=value(ifield+ifld)
      if(aval.lt.0.0d0.or.aval.gt.0.500001d0) go to 2510
      xmu=aval
      go to 2010
 2150 ifld=ifld+1
      if(nodplc(icode+ifld)) 6000,2160,2010
 2160 aval=value(ifield+ifld)
      maxtim=aval
      go to 2010
 2500 write (6,2501) anam
 2501 format('0warning:  unknown option:  ',a8,' ... ignored'/)
      go to 2010
 2510 write (6,2511) anam
 2511 format('0warning:  illegal value specified for option:  ',a8,' ...
     1 ignored'/)
      go to 2010
c
c  print card
c
 3500 iprpl=0
      go to 3610
c
c  plot (and print) card
c
 3600 iprpl=1
 3610 ifld=2
 3613 anam=amiss
      if (nodplc(icode+ifld).ne.1) go to 3950
      anam=value(ifield+ifld)
      ms=0
      if (xxor(anam,ams).ne.0) go to 3615
      ms=1
      ifld=3
      if (nodplc(icode+ifld).ne.1) go to 3970
      anam=value(ifield+ifld)
 3615 call move(anam,3,ablnk,1,6)
      do 3620 i=1,5
      if (anam.ne.aopt(i)) go to 3620
      ktype=i
      go to 3630
 3620 continue
      go to 3950
 3630 id=30+5*iprpl+ktype
      call find(dfloat(jelcnt(id)),id,loc,1)
      nodplc(loc+2)=ktype
      if (ms.eq.0) go to 3635
      locv=nodplc(loc+1)
      value(locv)=0.0d0
 3635 numout=0
 3640 ifld=ifld+1
      if (nodplc(icode+ifld)) 3900,3640,3650
 3650 call outdef(ifld,ktype,loct,ltype)
      if (igoof.ne.0) go to 3970
      if (iprpl.eq.0) go to 3660
      plimlo=0.0d0
      plimhi=0.0d0
      if (nodplc(icode+ifld+1).ne.0) go to 3660
      if (nodplc(icode+ifld+2).ne.0) go to 3660
      plimlo=value(ifield+ifld+1)
      plimhi=value(ifield+ifld+2)
      ifld=ifld+2
 3660 numout=numout+1
      lspot=loc+2*numout+2
      nodplc(lspot)=loct
      nodplc(lspot+1)=ltype
      if (iprpl.eq.0) go to 3670
      locv=nodplc(loc+1)
      lspot=locv+2*numout-1
      value(lspot)=plimlo
      value(lspot+1)=plimhi
 3670 if (numout.eq.8) go to 3900
      go to 3640
 3900 nodplc(loc+3)=numout
      if (iprpl.eq.0) go to 6000
c...  propogate plot limits downward
      if (numout.le.1) go to 6000
      locv=nodplc(loc+1)
      lspot=locv+2*numout-1
      plimlo=value(lspot)
      plimhi=value(lspot+1)
      i=numout-1
 3905 lspot=lspot-2
      if (value(lspot).ne.0.0d0) go to 3910
      if (value(lspot+1).ne.0.0d0) go to 3910
      value(lspot)=plimlo
      value(lspot+1)=plimhi
      go to 3920
 3910 plimlo=value(lspot)
      plimhi=value(lspot+1)
 3920 i=i-1
      if (i.ge.1) go to 3905
      go to 6000
c
c     errors
c
 3950 write (6,3951) anam
 3951 format('0warning:  unknown analysis mode:  ',a8,
     1  ' ... line ignored'/)
      go to 6000
 3970 write (6,3971)
 3971 format('0warning:  unrecognizable output variable on above line'/)
      igoof=0
      go to 3640
c
c  width card
c
 4000 ifld=1
 4010 ifld=ifld+1
      if (nodplc(icode+ifld).ne.1) go to 6000
 4020 anam=value(ifield+ifld)
      if (anam.ne.ain) go to 4040
      ifld=ifld+1
      if (nodplc(icode+ifld)) 6000,4030,4020
 4030 iwidth=value(ifield+ifld)
      iwidth=min0(max0(iwidth,10),120)
      go to 4010
 4040 if (anam.ne.aout) go to 6000
      ifld=ifld+1
      if (nodplc(icode+ifld)) 6000,4050,4020
 4050 lwidth=dmin1(dmax1(value(ifield+ifld),72.0d0),132.0d0)
      go to 4010
c
c  nodeset statement
c
 4100 ifld=1
 4110 ifld=ifld+1
      if(nodplc(icode+ifld)) 6000,4120,4110
 4120 nodnum=value(ifield+ifld)
      if(nodnum.le.0) go to 4190
      ifld=ifld+1
      if(nodplc(icode+ifld)) 4180,4130,4170
 4130 call sizmem(nsnod,nic)
      call extmem(nsnod,1)
      call extmem(nsval,1)
      nodplc(nsnod+nic+1)=nodnum
      value(nsval+nic+1)=value(ifield+ifld)
      go to 4110
c
c  errors on .nodeset statement
c
 4170 write(6,4171) value(ifield+ifld)
 4171 format('0warning: out-of-place non-numeric field ',a8,
     1 ' skipped'/)
      go to 4110
 4180 write(6,4181) nodnum
 4181 format('0warning: initial value missing for node ',i5,/)
      go to 6000
 4190 write(6,4191)
 4191 format('0warning: attempt to specify initial condition for ',
     1 'ground ingnored',/)
      ifld=ifld+1
      if(nodplc(icode+ifld)) 6000,4110,4170
c
c  initial conditions statement
c
 4200 ifld=1
 4210 ifld=ifld+1
      if(nodplc(icode+ifld)) 6000,4220,4210
 4220 nodnum=value(ifield+ifld)
      if(nodnum.le.0) go to 4290
      ifld=ifld+1
      if(nodplc(icode+ifld)) 4280,4230,4270
 4230 call sizmem(icnod,nic)
      call extmem(icnod,1)
      call extmem(icval,1)
      nodplc(icnod+nic+1)=nodnum
      value(icval+nic+1)=value(ifield+ifld)
      go to 4210
c
c  errors on .ic statement
c
 4270 write(6,4271) value(ifield+ifld)
 4271 format('0warning: out-of-place non-numeric field ',a8,
     1 ' skipped'/)
      go to 4210
 4280 write(6,4281) nodnum
 4281 format('0warning: initial value missing for node ',i5,/)
      go to 6000
 4290 write(6,4291)
 4291 format('0warning: attempt to specify initial condition for ',
     1 'ground ingnored',/)
      ifld=ifld+1
      if(nodplc(icode+ifld)) 6000,4210,4270
c
c  finished
c
 6000 return
      end
      subroutine outdef(ifld,mode,loct,ltype)
      implicit double precision (a-h,o-z)
c
c     this routine constructs the internal list element for an output
c variable defined on some input card.
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
      integer xxor
      dimension aout(19),aopts(5)
      data aout / 4hv   , 4hvm  , 4hvr  , 4hvi  , 4hvp  , 4hvdb ,
     1            4hi   , 4him  , 4hir  , 4hii  , 4hip  , 4hidb ,
     2            4honoi, 4hinoi, 4hhd2 , 4hhd3 , 4hdim2, 4hsim2,
     3            4hdim3 /
      data aopts / 1hm, 1hr, 1hi, 1hp, 1hd /
      data alprn, acomma, ablnk, aletv / 1h(, 1h,, 1h , 1hv /
c
      if (nodplc(icode+ifld).ne.1) go to 300
      anam=value(ifield+ifld)
      call move(anam,5,ablnk,1,4)
      do 10 i=1,19
      if (xxor(anam,aout(i)).ne.0) go to 10
      idout=i
      go to 20
   10 continue
      go to 300
c
c  further error checking
c
   20 if (mode.ge.3) go to 25
c...  dc or tran
      if ((idout.ne.1).and.(idout.ne.7)) go to 300
      go to 38
   25 if (mode.ge.4) go to 30
c...  ac
      if (idout.ge.13) go to 300
      go to 38
   30 if (mode.eq.5) go to 35
c...  noise
      if ((idout.ne.13).and.(idout.ne.14)) go to 300
      go to 38
c...  distortion
   35 if (idout.lt.15) go to 300
   38 ktype=0
      ltype=idout
      if (idout.lt.7) go to 40
      ktype=1
      ltype=ltype-6
      if (idout.lt.13) go to 40
      ktype=idout-11
      ltype=1
c
c  voltage output
c
   40 id=40+mode
      if (ktype.ne.0) go to 100
      if (nodplc(icode+ifld+1).ne.0) go to 300
      ifld=ifld+1
      n1=value(ifield+ifld)
      if (n1.lt.0) go to 300
      if(n1.gt.9999) go to 300
      n2=0
      adelim=value(idelim+ifld)
      if (adelim.eq.acomma) go to 45
      if (adelim.ne.ablnk) go to 50
   45 if (nodplc(icode+ifld+1).ne.0) go to 300
      ifld=ifld+1
      n2=value(ifield+ifld)
      if (n2.lt.0) go to 300
      if(n2.gt.9999) go to 300
   50 outnam=ablnk
      ipos=1
      call alfnum(n1,outnam,ipos)
      ipos=5
      call alfnum(n2,outnam,ipos)
      call find(outnam,id,loct,0)
      nodplc(loct+2)=n1
      nodplc(loct+3)=n2
      go to 400
c
c  current output
c
  100 if (ktype.ne.1) go to 200
      if (nodplc(icode+ifld+1).ne.1) go to 300
      ifld=ifld+1
      avsrc=value(ifield+ifld)
      achek=avsrc
      call move(achek,2,ablnk,1,7)
      if (achek.ne.aletv) go to 300
      call find(avsrc,id,loct,0)
      call find(avsrc,9,nodplc(loct+2),0)
      nodplc(loct+5)=1
      go to 400
c
c  noise or distortion outputs
c
  200 id=44
      if (ktype.ge.4) id=id+1
      if (value(idelim+ifld).ne.alprn) go to 220
      if (nodplc(icode+ifld+1).ne.1) go to 300
      ifld=ifld+1
      atype=value(ifield+ifld)
      call move(atype,2,ablnk,1,7)
      do 210 i=1,5
      if (atype.ne.aopts(i)) go to 210
      ltype=i+1
      go to 220
  210 continue
      go to 300
  220 call find(anam,id,loct,0)
      nodplc(loct+2)=0
      nodplc(loct+5)=ktype
      go to 400
c
c  errors
c
  300 igoof=1
c
c  finished
c
  400 return
      end
      subroutine card
      implicit double precision (a-h,o-z)
c
c     this routine scans the input lines, storing each field into the
c tables ifield, idelim, icolum, and icode.  with the exception of the
c '.end' line, card always reads the next line to check for a possible
c continuation before it exits.
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
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      common /knstnt/ twopi,xlog2,xlog10,root2,rad,boltz,charge,ctok,
     1   gmin,reltol,abstol,vntol,trtol,chgtol,eps0,epssil,epsox
      common /blank/ value(1000)
      integer nodplc(64)
      complex*16 cvalue(32)
      equivalence (value(1),nodplc(1),cvalue(1))
c
      dimension adigit(10)
      data adigit / 1h0,1h1,1h2,1h3,1h4,1h5,1h6,1h7,1h8,1h9 /
      data ablnk,aper,aplus,aminus,astk / 1h , 1h., 1h+, 1h-, 1h* /
      data ag,ak,au,an,ap,ae,am,af,at /1hg,1hk,1hu,1hn,1hp,1he,1hm,
     1  1hf,1ht/
      data ai / 1hi /
      data alprn, arprn, aequal / 1h(, 1h), 1h= /
      data aend / 4h.end /
c
c      note:  the value of the function *nxtchr* (used extensively in
c this routine) is as follows:
c
c                    <0:  end-of-line
c                    =0:  delimiter found
c                    >0:  non-delimiter found
c
      numfld=0
      nofld=10
      go to 20
c
c  read next card
c
   10 nofld=10
      call getlin
      if (keof.eq.0) go to 20
c...  error:  unexpected end-of-file condition on input
   15 keof=1
      nofld=1
      numfld=0
      igoof=1
      write (6,16)
   16 format('0*error*:  .end card missing'/)
      go to 1000
c
c  eliminate trailing blanks rapidly
c
   20 if (afield(nofld).ne.ablnk) go to 40
      if (nofld.eq.1) go to 30
      nofld=nofld-1
      go to 20
c...  write blank card
   30 write (6,31)
   31 format(1x)
      go to 10
c...  copy the card to output listing
   40 write (6,41) (afield(i),i=1,nofld)
   41 format(1x,10a8)
c
c  initialization for new card
c
   45 kntrc=0
      kntlim=min0(8*nofld,iwidth)
c
c  fetch first non-delimiter (see routine *nxtchr* for list)
c
   50 if (nxtchr(0)) 600,50,60
c...  check for comment (leading asterisk)
   60 if (achar.eq.astk) go to 10
      go to 100
c
c  fetch next character
c
   70 if (nxtchr(0)) 600,80,100
c
c  two consecutive delimiters imply numeric zero unless the delimiter
c  is a blank or parenthesis.
c
   80 if (achar.eq.ablnk) go to 70
      if (achar.eq.alprn) go to 70
      if (achar.eq.arprn) go to 70
      if (achar.eq.aequal) go to 70
c...  check for sufficient space in storage arrays
      if (numfld.lt.insize-1) go to 90
      call extmem(ifield,50)
      call extmem(icode,50)
      call extmem(idelim,50)
      call extmem(icolum,50)
      insize=insize+50
   90 numfld=numfld+1
      value(ifield+numfld)=0.0d0
      nodplc(icode+numfld)=0
      value(idelim+numfld)=achar
      nodplc(icolum+numfld)=kntrc
      go to 70
c
c  check for sufficient space in storage arrays
c
  100 if (numfld.lt.insize-1) go to 110
      call extmem(ifield,50)
      call extmem(icode,50)
      call extmem(idelim,50)
      call extmem(icolum,50)
      insize=insize+50
c
c  begin scan of next field
c
c...  initialization
  110 jdelim=0
      xsign=1.0d0
      xmant=0.0d0
      idec=0
      iexp=0
c...  check for leading plus or minus sign
      if (achar.eq.aplus) go to 210
      if (achar.eq.aminus) go to 200
c...  finish initialization
      anam=ablnk
      kchr=1
c...  an isolated period indicates that a continuation card follows
      if (achar.ne.aper) go to 120
c...  alter initialization slightly if leading period found
      idec=1
      iexp=-1
      anam=aper
      kchr=2
c...  now take a look at the next character
      if (nxtchr(0)) 10,10,120
c
c  test for number (any digit)
c
  120 do 130 i=1,10
      if (achar.ne.adigit(i)) go to 130
      xmant=dfloat(i-1)
      go to 210
  130 continue
c
c  assemble name
c
      numfld=numfld+1
      call move(anam,kchr,achar,1,1)
      kchr=kchr+1
      do 150 i=kchr,8
      if (nxtchr(0)) 160,160,140
  140 call move(anam,i,achar,1,1)
  150 continue
      go to 170
  160 jdelim=1
  170 value(ifield+numfld)=anam
      nodplc(icode+numfld)=1
      nodplc(icolum+numfld)=kntrc
c...  no '+' format continuation possible for .end card
      if (numfld.ge.2) go to 400
      if (anam.ne.aend) go to 400
      nodplc(icode+numfld+1)=-1
      go to 1000
c
c  process number
c
c...  take note of leading minus sign
  200 xsign=-1.0d0
c...  take a look at the next character
  210 if (nxtchr(0)) 335,335,220
c...  test for digit
  220 do 230 i=1,10
      if (achar.ne.adigit(i)) go to 230
      xmant=xmant*10.0d0+dfloat(i-1)
      if (idec.eq.0) go to 210
      iexp=iexp-1
      go to 210
  230 continue
c
c  check for decimal point
c
      if (achar.ne.aper) go to 240
c...  make certain that this is the first one found
      if (idec.ne.0) go to 500
      idec=1
      go to 210
c
c  test for exponent
c
  240 if (achar.ne.ae) go to 300
      if (nxtchr(0)) 335,335,250
  250 itemp=0
      isign=1
c...  check for possible leading sign on exponent
      if (achar.eq.aplus) go to 260
      if (achar.ne.aminus) go to 270
      isign=-1
  260 if (nxtchr(0)) 285,285,270
c...  test for digit
  270 do 280 i=1,10
      if (achar.ne.adigit(i)) go to 280
      itemp=itemp*10+i-1
      go to 260
  280 continue
      go to 290
  285 jdelim=1
c...  correct internal exponent
  290 iexp=iexp+isign*itemp
      go to 340
c
c  test for scale factor
c
  300 if (achar.ne.am) go to 330
c...  special check for *me* (as distinguished from *m*)
      if (nxtchr(0)) 320,320,310
  310 if (achar.ne.ae) go to 315
      iexp=iexp+6
      go to 340
  315 if (achar.ne.ai) go to 325
      xmant=xmant*25.4d-6
      go to 340
  320 jdelim=1
  325 iexp=iexp-3
      go to 340
  330 if (achar.eq.at) iexp=iexp+12
      if (achar.eq.ag) iexp=iexp+9
      if (achar.eq.ak) iexp=iexp+3
      if (achar.eq.au) iexp=iexp-6
      if (achar.eq.an) iexp=iexp-9
      if (achar.eq.ap) iexp=iexp-12
      if (achar.eq.af) iexp=iexp-15
      go to 340
  335 jdelim=1
c
c  assemble the final number
c
  340 if (xmant.eq.0.0d0) go to 350
      if (iexp.eq.0) go to 350
      if (iabs(iexp).ge.201) go to 500
      xmant=xmant*dexp(dfloat(iexp)*xlog10)
      if (xmant.gt.1.0d+35) go to 500
      if (xmant.lt.1.0d-35) go to 500
  350 numfld=numfld+1
      value(ifield+numfld)=dsign(xmant,xsign)
      nodplc(icode+numfld)=0
      nodplc(icolum+numfld)=kntrc
c
c  skip to non-blank delimiter (if necessary)
c
  400 if (jdelim.eq.0) go to 440
  410 value(idelim+numfld)=achar
c...  the characters  )  and  .  form a single delimiter if adjacent
      if (achar.ne.arprn) go to 70
      if (nxtchr(0)) 430,430,420
  420 if (achar.ne.aper) go to 430
      call move(value(idelim+numfld),2,aper,1,1)
      go to 70
  430 kntrc=kntrc-1
      go to 70
  440 if (nxtchr(0)) 450,410,440
  450 value(idelim+numfld)=achar
      go to 600
c
c  errors
c
  500 write (6,501) kntrc
  501 format('0*error*:  illegal number -- scan stopped at column ',i3/)
      igoof=1
      numfld=numfld+1
      value(ifield+numfld)=0.0d0
      nodplc(icode+numfld)=0
      value(idelim+numfld)=achar
      nodplc(icolum+numfld)=kntrc
c
c  finished
c
  600 nodplc(icode+numfld+1)=-1
c
c  check next line for possible continuation
c
  610 call getlin
      if (keof.eq.1) go to 15
      nofld=10
  620 if (afield(nofld).ne.ablnk) go to 630
      if (nofld.eq.1) go to 650
      nofld=nofld-1
      go to 620
  630 kntrc=0
      kntlim=min0(8*nofld,iwidth)
c...  continuation line has a '+' as first non-delimiter on card
  632 if(nxtchr(0)) 650,632,634
  634 if(achar.ne.aplus) go to 640
      write(6,41) (afield(i),i=1,nofld)
      go to 70
  640 if (achar.ne.astk) go to 1000
  650 write (6,41) (afield(i),i=1,nofld)
      go to 610
 1000 return
      end
      subroutine getlin
      implicit double precision (a-h,o-z)
c
c     this routine reads the next line of input into the array afield.
c if end-of-file is found, the variable keof is set to 1.
c
      common /line/ achar,afield(15),oldlin(15),kntrc,kntlim
      common /flags/ iprnta,iprntl,iprntm,iprntn,iprnto,limtim,limpts,
     1   lvlcod,lvltim,itl1,itl2,itl3,itl4,itl5,igoof,nogo,keof
      call copy8(afield,oldlin,15)
      read(5,6,end=10) (afield(i),i=1,10)
      go to 100
    6 format(10a8)
   10 keof=1
  100 return
      end
      integer function nxtchr(int)
      implicit double precision (a-h,o-z)
c
c     this routine advances the current line scan pointer one column
c     and checks whether or not the next character is a delimiter
c
      common /line/ achar,afield(15),oldlin(15),kntrc,kntlim
c
      dimension adelim(5)
      data adelim / 1h , 1h,, 1h=, 1h(, 1h) /
      data ablnk / 1h  /
      data ichar /0/
c
c  advance scan pointer (kntrc)
      kntrc=kntrc+1
      if (kntrc.gt.kntlim) go to 30
      call move(achar,1,afield,kntrc,1)
      call move(ichar,2,achar,1,1)
      if(ichar.gt.31.and.ichar.lt.91) go to 5
c.. delete ascii control codes
      if(ichar.lt.32) ichar=32
      if(ichar.gt.96.and.ichar.lt.123) ichar=ichar-32
      if(ichar.eq.127) ichar=32
      call move(achar,1,ichar,2,1)
    5 do 10 i=1,5
      if (achar.eq.adelim(i)) go to 20
   10 continue
c
c  non-delimiter
c
      nxtchr=1
      return
c
c  delimiter
c
   20 nxtchr=0
      return
c
c  end-of-line
c
   30 nxtchr=-1
      achar=ablnk
      return
      end
