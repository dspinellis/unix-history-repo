#ifndef lint
static char *rcsid =
   "$Header: fixmask.c,v 1.2 83/04/10 21:34:40 sklower Exp $";
#endif

/*					-[Sat Jan 29 12:42:54 1983 by jkf]-
 * 	fixmask.c				$Locker:  $
 * complete program to change register save masks on the vax
 *
 * (c) copyright 1982, Regents of the University of California
 */


#include <stdio.h>
char mybuf[BUFSIZ];
extern unsigned short mask[];
main(){
	register savesize = 0; char *cp;
	while(fgets(mybuf,BUFSIZ,stdin)!=NULL) {
		if(*mybuf=='#') {
			if(strcmpn(mybuf,"#save	",6)==0){
				savesize = mybuf[6]-'0';
			} else if (strcmpn(mybuf,"#protect	",9)==0){
				savesize = '0'-1-mybuf[9];
			}
		}
		if(savesize && strcmpn(mybuf,"	.set	L",7)==0) {
			for(cp=mybuf;*cp++!=',';);
			sprintf(cp,"0x%X\n",mask[savesize + 10]);
			savesize = 0;
		}
		fputs(mybuf,stdout);
	}
}
unsigned short mask[] = {
	0,0,0,0xfc0,0xfc0,0xfc0,0xec0,0xcc0,0x8c0,0x0c0,0,
	0x800,0xc00,0xe00,0xf00,0xf80,0xfc0,0,0,0,0};
