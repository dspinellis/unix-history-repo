#include	"sys/types.h"
#include	"sys/stat.h"
#include "fio.h"
f_open(a) olist *a;
{	unit *b;
	int n;
	char buf[256];
	cllist x;
	if(a->ounit>=MXUNIT || a->ounit<0)
		err(a->oerr,101,"open")
	b= &units[a->ounit];
	if(b->ufd!=0) goto connected;
unconnected:
	b->url=a->orl;
	if(a->oblnk && *a->oblnk=='b') b->ublnk=1;
	else b->ublnk=0;
	if(a->ofm==0)
	{	if(b->url>0) b->ufmt=0;
		else b->ufmt=1;
	}
	else if(*a->ofm=='f') b->ufmt=1;
	else b->ufmt=0;
	if(a->osta==0) goto unknown;
	switch(*a->osta)
	{
	unknown:
	default:
	case 'o':
		if(a->ofnm==0) err(a->oerr,107,"open")
		g_char(a->ofnm,a->ofnmlen,buf);
		b->uscrtch=0;
		if(*a->osta=='o' && access(buf,0))
			err(a->oerr,errno,"open")
	done:
		b->ufnm=(char *) calloc(strlen(buf)+1,sizeof(char));
		if(b->ufnm==NULL) err(a->oerr,113,"no space");
		strcpy(b->ufnm,buf);
		b->uend=0;
		if(isdev(buf))
		{	b->ufd = fopen(buf,"r");
			if(b->ufd==NULL) err(a->oerr,errno,buf)
			else	b->uwrt = 0;
		}
		else
		{	b->ufd = fopen(buf, "a");
			if(b->ufd != NULL) b->uwrt = 1;
			else if((b->ufd = fopen(buf, "r")) != NULL)
			{	fseek(b->ufd, 0L, 2);
				b->uwrt = 0;
			}
			else	err(a->oerr, errno, buf)
		}
		b->useek=canseek(b->ufd);
		if((b->uinode=inode(buf))==-1)
			err(a->oerr,108,"open")
		if(a->orl && b->useek) rewind(b->ufd);
		return(0);
	 case 's':
		b->uscrtch=1;
		strcpy(buf,"tmp.FXXXXXX");
		mktemp(buf);
		goto done;
	case 'n':
		b->uscrtch=0;
		if(a->ofnm==0) err(a->oerr,107,"open")
		g_char(a->ofnm,a->ofnmlen,buf);
		/*SYSDEP access*/
		if(access(buf, 0) == -1) close(creat(buf, 0666));
		goto done;
	}
connected:
	if(a->ofnm==0)
	{
	same:	if(a->oblnk!= 0) b->ublnk= *a->oblnk== 'b'?0:1;
		return(0);
	}
	g_char(a->ofnm,a->ofnmlen,buf);
	if(inode(buf)==b->uinode) goto same;
	x.cunit=a->ounit;
	x.csta=0;
	x.cerr=a->oerr;
	if((n=f_clos(&x))!=0) return(n);
	goto unconnected;
}
fk_open(rd,seq,fmt,n) ftnint n;
{	char nbuf[10];
	olist a;
	sprintf(nbuf,"fort.%D",n);
	a.oerr=1;
	a.ounit=n;
	a.ofnm=nbuf;
	a.ofnmlen=strlen(nbuf);
	a.osta=NULL;
	a.oacc= seq==SEQ?"s":"d";
	a.ofm = fmt==FMT?"f":"u";
	a.orl = seq==DIR?1:0;
	a.oblnk=NULL;
	return(f_open(&a));
}
isdev(s) char *s;
{	struct stat x;
	int j;
	if(stat(s, &x) == -1) return(0);
	if((j = (x.st_mode&S_IFMT)) == S_IFREG || j == S_IFDIR) return(0);
	else	return(1);
}
