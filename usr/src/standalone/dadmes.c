# include "RP.h"
dadmes(dptr)
register int *dptr ;
{
register char *mesp ;
register int i ;
 
mesp = " cyl     trk     sec    " ;
i = l2x(*(dptr+RP_cyl) & 01777,&mesp[5]) ;
blnkit(&mesp[5+i],4-i) ;
 
i = l2x((*(dptr+RP_stk)>>8) & 037 , &mesp[13]) ;
blnkit(&mesp[13+i],3-i) ;
 
i = l2x(*(dptr+RP_stk)&037,&mesp[21]) ;
blnkit(&mesp[21+i],3-i) ;
 
putlin(mesp) ;
}
