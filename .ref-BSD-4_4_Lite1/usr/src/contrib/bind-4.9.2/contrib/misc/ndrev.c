#if 0
Date: 	Mon, 26 Apr 1993 18:03:06 -0400
From: Andy Poling <andy@jhunix.hcf.jhu.edu>
Subject: Re: send me your tools 
To: Paul A Vixie <vixie>
In-Reply-To: <9304261923.AA15488@cognition.pa.dec.com>
Message-Id: <Pine.3.05.9304261804.B1586-d100000@jhunix2.hcf.jhu.edu>
Mime-Version: 1.0
Content-Type: TEXT/PLAIN; charset=US-ASCII

On Mon, 26 Apr 1993, Paul A Vixie wrote:
> more is better.  send it along.

Here is the source (the date will give you an idea of its age), which
luckily contains the original author's name (though I apparently lost his
address long ago). 

-Andy

Andy Poling                              Internet: andy@jhunix.hcf.jhu.edu
UNIX Systems Programmer                  Bitnet: ANDY@JHUNIX
Homewood Academic Computing              Voice: (410)516-8096    
Johns Hopkins University                 UUCP: uunet!mimsy!aplcen!jhunix!andy

===============================================================================
#endif /*0*/

/*
 *	Ndrev	Takes a standard "NAMED" file and creates "reverse" (PTR)
 *		entries from each host (IN A) entry.
 *
 *	Calling sequence:
 *
 *		ndrev NETWORK -d DOMAIN [FILE]
 *
 *	where:	NETWORK	is the class a, b, or c network or subnetwork
 *			number to reverse.  All entries not belonging
 *			to this network will be ignored.
 *
 *		DOMAIN	is the root domain of the NAMED file being processed.
 *
 *		FILE	is the source file name.  Default is standard input.
 *
 *	Written by Douglas E. Nelson, Michigan State University
 *
 *	Copyright, 1989, Michigan State University Board of Trustees
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

gotoeol(f) 
register FILE *f; 
{ 
  register char ch; 
  do
	ch=getc(f);
  while(ch!='\n' && ch!=EOF); 
  if (ch==EOF) 
	return(EOF);
  else 
	return(0); 
} 

/* subroutine:  no case string compare(str1,str2) 
* compares two strings ignoring alphabetic case 
* returns NULL if they are equal 
*/ 
int 
ncstrcmp(str1,str2) 
register unsigned char *str1,*str2; 
{ 
  register unsigned char ch1,ch2; 
  if (str1==str2) return(0); 
  if (str1==NULL || str2==NULL) return(1); 
  while (*str1!=NULL && *str2!=NULL) 
  {
    ch1= *str1>='A'&&*str1<='Z'?*str1++|32:*str1++; 
    ch2= *str2>='A'&&*str2<='Z'?*str2++|32:*str2++; 
    if (ch1!=ch2) return(1); 
  } 
  if (*str1!=NULL || *str2!=NULL) 
	return(-1); 
  return(0); 
} 


main(argc,argv) 
int argc; 
char *argv[];
{ 
  FILE *f; 
  char ch; 
  int i,hwmany; 
  char name[80],instr[80],instr2[80],adrs[80],deptname[80]; 
  int a1, a2, a3, a4;
  int nadr, adr1, adr2, adr3, adr4;

  deptname[0]='\0';

  f = stdin;
  while (--argc)  
  {
    ++argv;
    if (**argv >= '0' && **argv <= '9')
    {
      nadr = sscanf (*argv,"%d.%d.%d.%d",&adr1,&adr2,&adr3,&adr4);
      if (!nadr)
      {
	fprintf (stderr, "Unknown argument: %s\n",*argv);
	exit (-1);
      }
      if (nadr > 3)
      {
	fprintf (stderr, "Cannot reverse a single host\n");
	exit (-1);
      }
    }
    else if (strcmp(*argv,"-d") == 0)
    {
      if (--argc < 1)
      {
	fprintf (stderr, "Domain name argument missing for -d\n");
	exit (-1);
      }
      strcpy (deptname, *++argv);
      strcat (deptname, ".");
    }
    else
    {
      f=fopen(*argv,"r"); 
      if (f==0)
      {
	fprintf (stderr, "Unable to open file: %s\n",*argv);
	exit (-2);
      }
    }
  }

  if (!nadr)
  {
    fprintf (stderr, "Must specify the (sub)network to reverse, e.g. 35.8\n");
    exit (-1);
  }

  fprintf (stdout, "$ORIGIN\t");
  if (nadr > 2)  
	fprintf (stdout, "%d.",adr3);
  if (nadr > 1)  
	fprintf (stdout, "%d.",adr2);
  fprintf (stdout, "%d.IN-ADDR.ARPA.\n",adr1);

  do
  { 
    do 
	ch=getc(f); 
    while (ch=='\n' && ch!=EOF); 
    if (ch<'A' || ch>'z' || ch>'Z' && ch<'a')
    {
      switch (ch)
      {
	case '$':
	{
	  fscanf(f,"%s %s",name,instr);
	  ch=gotoeol(f);
	  if (ncstrcmp("ORIGIN",name)==NULL)
	  {
	    strcpy(deptname,instr);
	  }
	  break;
	}
/*
	case ';':
	{
	  putc(ch,stdout);
	  while (ch!='\n' && ch!=EOF)
	  {
	    ch=getc(f);
	    putc(ch,stdout);
	  }
	  break;
	}
*/
	default:
	{
	  ch=gotoeol(f);
	  break;
	}
      }
    }
    else
    { 
      ungetc(ch,f); 
      hwmany=fscanf(f,"%s %s %s %s",name,instr,instr2,adrs); 
      if (hwmany>=4 && ((ncstrcmp("IN",instr)==NULL) && (ncstrcmp("A",instr2)==NULL)))
      {
	sscanf (adrs,"%d.%d.%d.%d",&a1,&a2,&a3,&a4);
	if (a1 == adr1 && (nadr < 2 || a2 == adr2) &&
	    (nadr < 3 || a3 == adr3))
	    {
	  fprintf(stdout,"%d",a4);
	  if (nadr < 3)
		fprintf(stdout,".%d",a3);
	  if (nadr < 2)
		fprintf(stdout,".%d",a2);
	  fprintf(stdout,"\tIN\tPTR\t");
	  fprintf(stdout,"%s",name);
	  if (strlen(deptname)>0 && name[strlen(name)-1] != '.')
	    fprintf(stdout,".%s",deptname);
	  putc('\n',stdout);
	}
      }
    }
  }
  while (ch!=EOF); 
  fclose(f);
  return(0); 
} 

