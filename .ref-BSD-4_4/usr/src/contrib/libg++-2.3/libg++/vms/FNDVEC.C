#include <stdio.h>
#include <string.h>

char * strchr(const char *, char);

struct vector{
	char	flag;	/* indicates if this has been commented out */
	char*	name;
	struct	vector*	next;};

FILE	*marfile;
FILE	*libfile;
char	line[140];
struct	vector*	vlist = (struct vector*) NULL;
struct  vector* vpoint;
struct  vector* vend = (struct vector*) NULL;
int	vcount=0;
int	vmax=0;	/* maximum number of transfer vectors */
		/* this is used to reserve space for this many vectors*/

#ifndef GSMAJOR
#define GSMAJOR 0
#endif

#ifndef GSMINOR
#define GSMINOR 0
#endif

int	gmajor = GSMAJOR;
int	gminor = GSMINOR;

readmar(){
	char	*status;
	char	*pnt;
	int	cmpr;
	int flag;
	do{
	status=fgets(line,sizeof(line),marfile);	
	if(status==(char*) NULL) break;
	cmpr=strncmp(line,"VMAX=",5);
	if(cmpr==0){
		pnt=line+5;
		sscanf(pnt,"%d",&vmax);};
	cmpr=strncmp(line,"	.IDENT	",8);
	if(cmpr==0){
		pnt=line+8;
		sscanf(pnt,"/%d-%d/",&gmajor,&gminor);};
	flag = 0;
	if(line[0] == ';') flag = 1; 
	cmpr=strncmp(line+flag,"UNUSED",6);
	if(cmpr == 0){
		flag += 2;
		add_unused();
		continue;
		};
	cmpr=strncmp(line+flag,"ROUTINE ",8);
	if(cmpr!=0) continue;	/* test for anything but the routine name */
	pnt=strchr(line,'\n');
	if(pnt!=(char*) NULL) *pnt='\0';
	pnt=line+8+flag;
	checkvec(pnt,0,flag);	/* add this vector to the list*/
	}while(1==1);
}

readlib(){
	char	*status;
	char	*pnt;
	char	*epnt;
	char	module[32];
	int	i;
	int	cmpr;
	for(i=0;i<8;i++) {
		status=fgets(line,sizeof(line),libfile);
		if(status==(char*)NULL) return 0;};
	do{
	  status=fgets(line,sizeof(line),libfile);
	  if(status==(char*) NULL) break;
	  pnt=strchr(line,'\n');
	  if(pnt!=(char*) NULL) *pnt='\0';
	  if(strlen(line)==0) continue;	/* and delete blank lines*/
	  cmpr=strncmp(line,"Module ",7);
	  if(cmpr==0){
		printf("%s ",line);
		continue;};
	  pnt=line;
	  do{
	    epnt=module;
	    while((*pnt!=' ')&&(*pnt!='\0')) *epnt++=*pnt++;
	    *epnt='\0';
	    checkvec(module,1,0);	/*see if this vector belongs on the list*/
	    while(*pnt==' ') pnt++;	/* trim the spaces inbetween the names*/
	    if(strlen(pnt)==0) break;
	  }while(1==1);
	}while(1==1);
	return 1;
}

add_unused(){
	vpoint=(struct vector*) calloc(sizeof(struct vector),1);
	if(vlist==(struct vector*) NULL)
		vlist=vpoint;
	else
		vend->next=vpoint;
	vend=vpoint;
	vend->name="\0";
	vend->flag = 2;
}

checkvec(char* pnt,int cflag,int commflag){
	int i;
	vpoint=vlist;
/* We do not need external visibility for the CTOR and DTOR lists. */
	if(strncmp(pnt,"_GLOBAL_.",9) == 0) return;
/* These two entry points are not used in the sharable image library.  */
	if(strncmp(pnt,"_VT.",4) == 0) commflag |= 4;
	if(strncmp(pnt,"__LIBGXX_DEFINED_",17) == 0) commflag |= 4;
	if(strcmp(pnt,"__MAIN") == 0) return;
	if(strcmp(pnt,"_GXX_VMS_STARTUP_2") == 0) return;
	if((cflag==1)&&(vpoint!=(struct vector*) NULL)){
	  do{
	    if(strcmp(pnt,vpoint->name)==0) return;
 	    vpoint=vpoint->next;
 	  }while(vpoint!=(struct vector*) NULL);};
/* 	if(cflag==1) printf("\n Adding %s",pnt);*/
	vpoint=(struct vector*) calloc(sizeof(struct vector),1);
	if(vlist==(struct vector*) NULL)
		vlist=vpoint;
	else
		vend->next=vpoint;
	vend=vpoint;
	vend->name=(char*) malloc(strlen(pnt)+1);
	vend->flag = commflag;
	strcpy(vend->name,pnt);
	vcount++;
}

writemar(char* pnt){
	char	title[30];
	char*	i;
	strncpy(title,pnt,30);
	i=strchr(title,'.');
	if(i!=(char*) NULL) *i='\0';	/*fix up the title*/
	fprintf(marfile,";\n");
	fprintf(marfile,"	.TITLE  %s\n",title);
	fprintf(marfile,"	.IDENT	/%d-%4.4d/\n",gmajor,gminor);
	if(vmax==0){
		vmax=1;
		while(vmax<vcount) vmax <<= 1;
		vmax <<=1;		/* one more for good measure */
		};
	fprintf(marfile,"VMAX=%d\n",vmax);
	fprintf(marfile,"; This program was generated automatically by the sharable library building program\n");
	fprintf(marfile,"; This MACRO program was lifted verbatim from pages 5-9 and 5-10 of:\n");
	fprintf(marfile,";       Guide to Creating Modular Procedures on VAX/VMS\n");
	fprintf(marfile,";\n");
	fprintf(marfile,"        .MACRO  ROUTINE NAME\n");
	fprintf(marfile,"        .EXTRN          NAME\n");
	fprintf(marfile,"        .ALIGN  QUAD\n");
	fprintf(marfile,"        .TRANSFER       NAME\n");
	fprintf(marfile,"        .MASK           NAME\n");
	fprintf(marfile,"        JMP             NAME+2\n");
	fprintf(marfile,"        .ENDM\n");
	fprintf(marfile,";\n");
	fprintf(marfile,"        .MACRO  UNUSED	NAME\n");
	fprintf(marfile,"	.long	0\n");
	fprintf(marfile,"	.long	0\n");
	fprintf(marfile,"        .ENDM\n");
	fprintf(marfile,";\n");
	fprintf(marfile,"        .PSECT  $$$$%s PIC, USR, CON, REL, LCL, SHR, -\n",title);
	fprintf(marfile,"                            EXE, RD, NOWRT, QUAD\n");
	fprintf(marfile,";\n");
	fprintf(marfile,"; Add new routines only to the bottom of the list!!!\n");
	fprintf(marfile,";\n");
	fprintf(marfile,"lstart:\n");
	vpoint=vlist;
	  do{
	    if((vpoint->flag & 4) == 4) {
	        fprintf(marfile,";UNIVERSAL = %s\n",vpoint->name);
 	        vpoint=vpoint->next;
		continue;
	    };
	    if((vpoint->flag & 1) == 1) fprintf(marfile,";");
	    if((vpoint->flag & 2) == 2) fprintf(marfile,"UNUSED\n");
	    else
	      fprintf(marfile,"ROUTINE %s\n",vpoint->name);
 	    vpoint=vpoint->next;
 	  }while(vpoint!=(struct vector*) NULL);
	fprintf(marfile,"lend:	.REPEAT VMAX - <<lend - lstart>/8>\n");
	fprintf(marfile,"	.long	0\n");
	fprintf(marfile,"	.long	0\n");
	fprintf(marfile,"	.ENDR\n");
	fprintf(marfile,"	.END\n");
}

main(int argc,char *argv[]){
	int nvec1;
	if(argc<2){
		printf(" Usage: fndvec marfile liblistfile");
		exit(1);};
	marfile=fopen(argv[1],"r");
	libfile=fopen(argv[2],"r");
	readmar();
	fclose(marfile);
	printf(" There were %d transfer vectors in the old file.\n",vcount);
	nvec1=vcount;
	readlib();
	fclose(libfile);
	printf(" There were %d transfer vectors after reading the library list.\n",vcount);
	if(nvec1==vcount){
	 	printf("There were no new vectors  - file not written.\n");
		return 1;};
	if(vcount>vmax && vmax != 0){
	printf("The number of transfer vectors has exceeded the number of positions allocated.\n");
	printf("This version of the library will NOT be compatible with previous versions.\n");
	gmajor++;
	gminor=0;
	vmax=0;};
	if (vmax != 0) gminor++;
	marfile=fopen(argv[1],"w");
	writemar(argv[1]);
	fclose(marfile);
	return 1;
}
