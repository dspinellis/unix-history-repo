#ifndef lint
static char *rcsid = "$Header: xnsremote.c,v 1.3 87/04/01 08:23:53 ed Exp $";
#endif lint

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* $Log:	xnsremote.c,v $
 * Revision 1.3  87/04/01  08:23:53  ed
 * Changed for new MakeSecondaryCreds call.
 * 
 * Revision 1.2  87/03/25  10:55:53  ed
 * Don't reset usefiling in login.
 * 
 * Revision 1.1  87/03/17  16:29:19  ed
 * Initial revision
 * 
 * 
 */

#include <stdio.h>
#include <sys/time.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <netns/ns.h>
#include <netns/sp.h>
#include <xnscourier/FilingSubset1.h>
#include <xnscourier/Filing4.h>
#include <xnscourier/except.h>
#undef __Clearinghouse2			/* Filing4.h defs this */
#include <xnscourier/CH.h>
#include <xnscourier/filetypes.h>

#ifdef PRINTOPTION
#ifndef XNSPRINT
#define XNSPRINT	"/usr/new/xnsprint"
#endif XNSPRINT

#ifndef MAHA
#define MAHA		"/usr/new/maha"
#endif MAHA
#endif PRINTOPTION

#define ROOT_DIRECTORY "/"

CourierConnection *connected;
Clearinghouse3_ObjectName hostobjname;
Authentication3_Verifier verifier;

/* the following 3 items make up the current session */
FilingSubset1_Session session;	/* the current session */
Clearinghouse3_ObjectName username;
FilingSubset1_Handle rootHandle;
char cur_dir[512]= 0;
char cur_pathname[512]= 0;
char cur_name[512]= 0;

static FilingSubset1_ControlSequence nullControls = {0,0};
static FilingSubset1_ScopeSequence nullScope = {0,0};

/* global data used to communicate with BDT procedures
 */
extern GetAttributeSequences(), 
	listproc(), retrieveproc();

char *AttrToString();
Boolean AttrToBoolean();
LongCardinal AttrToLongCardinal();
Cardinal AttrToCardinal();

static (*ProcEachSeq)();
static FILE *fout= stdout;

Boolean files_found= FALSE;
Boolean filing_subset= TRUE;
Boolean usefiling= TRUE;
Boolean verbose= FALSE;

LongCardinal typevalue= 0;
char *service, *pager;
extern int errno;
char thru_options[512];

struct name_entry {
	char *pathname;
	LongCardinal type;
} ;

static struct name_entry *name_list= 0;
static int name_count= 0;
static int name_size= 0;
#define MAX_NAMES	10

main(argc, argv)
int argc;
char *argv[];
{
	char *file, *rightbrkt;
	char oldservice[100];
	char *index(), *getenv();
	int i;
	CourierConnection *hookup();
	int opt;
	extern int optind;
	extern char *optarg;

#ifdef PRINTOPTION
	static char *options= "fvP:";
	static char *usage= "Usage: %s [-f] [-v] [-P printer] file1 ... filen\n";
#else PRINTOPTION
	static char *options= "fvs";
	static char *usage= "Usage: %s [-f] [-v] [-s] file1 ... filen\n";
#endif PRINTOPTION

	if ( argc < 1 ) {
		fprintf(stderr, usage, argv[0]);
		exit(1);
	}

#ifndef PRINTOPTION
	pager= getenv("PAGER");
#endif PRINTOPTION

	strcpy(thru_options, "");

	while ((opt= getopt(argc, argv, options)) != EOF) 
		switch (opt) {
			case 'v':
				verbose++;
				break;

			case 'f':
				usefiling= 0;
				break;

#ifdef PRINTOPTION
			case 'P' :
				strcat(thru_options, " -P ");
				strcat(thru_options, optarg);
				break;
#else PRINTOPTION
			case 's' :
				pager= '\0';	/* override pager */
				break;
#endif PRINTOPTION
			default:
				fprintf(stderr, "Invalid command option -%c\n", opt);
				exit(1);
		}

	for ( ; optind < argc ; optind++ ) {
		if ( getserviceandfile(argv[optind], &service, &file) == 0 ) {
			fprintf(stderr, "Invalid name %s\n", argv[optind]);
			exit(1);
		}
		DURING {
			if ( strcmp(oldservice, service) != 0 ) {
				if ( (connected= hookup(service)) == (CourierConnection *)0 ) {
					fprintf(stderr, "\nCan't connect to %s\n", service);
					continue;
				}
				login(0,0);
				strcpy(oldservice, service);
			}

			getfile(file);

		} HANDLER {
			FilingErrMsg(Exception.Code, Exception.Message);
#ifndef PRINTOPTION
			if ( fout != stdout )
				pclose(fout);
#endif PRINTOPTION
		} END_HANDLER;

	}

	return(0);
}

getserviceandfile(name, srvcptr, fileptr)
char *name;
char **srvcptr, **fileptr;
{
	char *sptr, *fptr;
	char *index(), *rindex();

	/*
	 * look for Xerox forms first:
	 *	[host]filename
	 */

	if ( (sptr= index(name, '[')) != 0 ) {
		if ( (fptr= index(sptr, ']')) != 0 ) {
			*fptr= '\0';
			*srvcptr= sptr + 1;
			*fileptr= fptr + 1;
			return(1);
		} else
			return(0);
	}

	/*
	 *	(host)filename
	 */

	if ( (sptr= index(name, '(')) != 0 ) {
		if ( (fptr= index(sptr, ')')) != 0 ) {
			*fptr= '\0';
			*srvcptr= sptr + 1;
			*fileptr= fptr + 1;
			return(1);
		} else
			return(0);
	}

	/*
	 * look for XNS style with trailing : delimiter
	 * (assumes no : in file name, use alternate spec instead)
	 *	object:domain:organization:filename
	 *		domain & organization are optional
	 */

	if ( (fptr= rindex(name, ':')) != 0 ) {
		*fptr= '\0';
		*srvcptr= name;
		*fileptr= fptr + 1;
		return(1);
	} else
		return(0);
}

copyhandle(dest,src)
	FilingSubset1_Handle dest,src;
{
	if (dest == (Unspecified *) 0) {
		fprintf(stderr,"Oops.  dest is null in copyhandle\n");
		exit(1);
	}
	dest[0] = src[0];
	dest[1] = src[1];
}

getfilehandle(filename, handle)
	char *filename;
	FilingSubset1_Handle handle;
{
	FilingSubset1_Attribute pathattr[1];
	FilingSubset1_AttributeSequence attrseq;
	FilingSubset1_OpenResults openresult;
	Filing4_OpenResults openresult2;

	if (filename == (char *)0 || *filename == '\000' || (strcmp(filename, "/") == 0) ) {
		if ( filing_subset )
			copyhandle(handle,FilingSubset1_nullHandle);
		else
			copyhandle(handle,rootHandle);
		return;
	}

	attrseq.length = 1;
	attrseq.sequence = pathattr;
	pathattr[0].type = FilingSubset1_pathname;
	copyhandle(handle, FilingSubset1_nullHandle);
#ifdef XEROXFSCOMPATIBILITY
	if ( filename[0] == '/')
		StringToAttr(filename+1, &pathattr[0]);
	else
		StringToAttr(filename, &pathattr[0]);
#else XEROXFSCOMPATIBILITY
	StringToAttr(filename, &pathattr[0]);
#endif XEROXFSCOMPATIBILITY
	alarm(0);
	if ( filing_subset ) {
		openresult = FilingSubset1_Open(connected, NULL, attrseq,
					  handle, nullControls,
					  session);
		copyhandle(handle, openresult.file);
	} else {
		openresult2 = Filing4_Open(connected, NULL, attrseq,
					  handle, nullControls,
					  session);
		copyhandle(handle, openresult2.file);
	} 
}

getdirhandle(filename, handle)
	char *filename;
	FilingSubset1_Handle handle;
{
	FilingSubset1_Attribute pathattr[1];
	FilingSubset1_AttributeSequence attrseq;
	FilingSubset1_OpenResults openresult;
	Filing4_OpenResults openresult2;
	char *rindex();
	char *slash;

	if (filename == (char *)0 || *filename == '\000' || (strcmp(filename, "/") == 0) ) {
		strcpy(cur_pathname, "/");
		strcpy(cur_name, "/");
		if ( filing_subset )
			copyhandle(handle,FilingSubset1_nullHandle);
		else
			copyhandle(handle,rootHandle);
		return;
	} else if ( filename[0] == '/' ) {
		strcpy(cur_pathname, filename);
	} else {
		strcpy(cur_pathname, cur_dir);
		if ( strcmp(cur_pathname, "/") != 0 )
			strcat(cur_pathname, "/");
		strcat(cur_pathname, filename); 
	}

	if ( (slash= rindex(cur_pathname,'/')) == NULL )
		strcpy(cur_name, cur_pathname);
	else
		strcpy(cur_name, slash+1);

	if ( filing_subset ) {
		copyhandle(handle, FilingSubset1_nullHandle);
	} else {
		if ( slash == cur_pathname) {
			copyhandle(handle, rootHandle);
			return;
		}

		attrseq.length = 1;
		attrseq.sequence = pathattr;
		pathattr[0].type = FilingSubset1_pathname;
		copyhandle(handle, FilingSubset1_nullHandle);
		*slash= '\0';		/* separate pathname from name */
#ifdef XEROXFSCOMPATIBILITY
		if ( cur_pathname[0] == '/' )
			StringToAttr(cur_pathname+1, &pathattr[0]);
		else
			StringToAttr(cur_pathname, &pathattr[0]);
#else XEROXFSCOMPATIBILITY
		StringToAttr(cur_pathname, &pathattr[0]);
#endif XEROXFSCOMPATIBILITY
		*slash= '/';		/* and put back */
		alarm(0);
		if ( filing_subset ) {
			openresult = FilingSubset1_Open(connected, NULL, attrseq,
						  handle, nullControls,
						  session);
			copyhandle(handle, openresult.file);
		} else {
			openresult2 = Filing4_Open(connected, NULL, attrseq,
						  handle, nullControls,
						  session);
			copyhandle(handle, openresult2.file);
		}
	}
}

freefilehandle(handle)
	FilingSubset1_Handle handle;
{
	if (handle[0] == FilingSubset1_nullHandle[0] &&
	    handle[1] == FilingSubset1_nullHandle[1])
		return;		/* don't free nullHandle */
	if (handle[0] == rootHandle[0] &&
	    handle[1] == rootHandle[1])
		return;		/* don't free root directory */
	alarm(0);
	if ( filing_subset )
		FilingSubset1_Close(connected, NULL, handle, session);
	else
		Filing4_Close(connected, NULL, handle, session);
}

CourierConnection *
hookup(name)
	char *name;
{
	register struct ns_addr *hostaddr;
	extern struct ns_addr *getXNSaddr();
	Clearinghouse3_ObjectName defaultobjname;
	static char hnamebuf[128];
	CourierConnection *cconn;

	CH_NameDefault(&defaultobjname);
	hostobjname = CH_StringToName(name, &defaultobjname);
	if ((hostaddr = CH_LookupAddrDN( hostobjname, 0, hnamebuf, 128))) {
		/* should check here to be sure host is a file service */
		hostaddr->x_port = htons(5); /* ?? */
		cconn = CourierOpen(hostaddr);
		/* reset objname to flush wildcards */
		/* clear_Clearinghouse3_ThreePartName(&hostobjname); */
		hostobjname = CH_StringToName(hnamebuf, &defaultobjname);
	}
	return(cconn);
}


login(name,pwd)
	char *pwd;
	char *name;
{
	FilingSubset1_Credentials credentials;
	FilingSubset1_LogonResults logonresult;
	FilingSubset1_LogonResults *resultptr= &logonresult;
	Filing4_LogonResults logonresult2;
	FilingSubset1_AttributeSequence attrseq;
	FilingSubset1_OpenResults openresult;
	Filing4_OpenResults openresult2;


	if ( name != 0 )
		username = CH_StringToName(name,&hostobjname);

	if ( usefiling ) {
		filing_subset= FALSE;
		if ( name == 0 && pwd == 0 ) {
			GetSimpleCredsAndVerifier(&username, 0, 
							&credentials.primary, &verifier);
		} else {
			MakeSimpleCredsAndVerifier(&username,pwd,
						&credentials.primary, &verifier);
		}
		logonresult2= Filing4_Logon(connected, NULL, hostobjname,
						credentials.primary, verifier);
		resultptr= (FilingSubset1_LogonResults *) &logonresult2;
		
	} else {
		if ( name == 0 && pwd == 0 ) {
			GetSimpleCredsAndVerifier(&username, 0, &credentials.primary, &verifier);
			MakeSecondaryCreds(hostobjname.object, 0, 0, &credentials.secondary);
		} else {
			MakeSimpleCredsAndVerifier(0, pwd, &credentials.primary, &verifier);
			MakeSecondaryCreds(hostobjname.object, name, pwd, &credentials.secondary);
		}
		filing_subset= TRUE;
		DURING 
			logonresult = FilingSubset1_Logon(connected, NULL, hostobjname,
					    credentials, verifier);
		HANDLER {
			switch (Exception.Code) {
			case REJECT_ERROR:
				filing_subset= FALSE;
				logonresult2= Filing4_Logon(connected, NULL, hostobjname,
							credentials.primary, verifier);
				resultptr= (FilingSubset1_LogonResults *) &logonresult2;
				break;
			default:
				RERAISE;
			}
		} END_HANDLER;
	}

	if ( filing_subset )
		session = resultptr->session;
	else
		session = resultptr->session;

	attrseq.length= 0;
	attrseq.sequence= 0;
	if ( filing_subset ) {
		openresult= FilingSubset1_Open(connected, NULL, attrseq,
					 FilingSubset1_nullHandle, nullControls,
					 session);
		copyhandle(rootHandle, openresult.file);
	} else {
		openresult2= Filing4_Open(connected, NULL, attrseq,
					 FilingSubset1_nullHandle, nullControls,
					 session);
		copyhandle(rootHandle, openresult2.file);
	}
	strcpy(cur_dir, ROOT_DIRECTORY);
}

logout()
{
	if ( filing_subset )
		FilingSubset1_Logoff(connected, NULL, session);
	else
		Filing4_Logoff(connected, NULL, session);
	clear_FilingSubset1_Session(&session);
}


getfile(remote)
	char *remote;
{
	FILE *popen();
	FilingSubset1_Handle remotehandle; /* note: an array */
	FilingSubset1_Handle dirhandle; /* note: an array */
	FilingSubset1_AttributeTypeSequence typeseq;
	FilingSubset1_AttributeType tsvals[10];
	FilingSubset1_ScopeSequence scopeseq;
	FilingSubset1_Scope scope;
	char pcmd[500];
	int i;
	register struct name_entry *entry;

	name_count= 0;
	name_size= MAX_NAMES;
	if ( (name_list= (struct name_entry *) malloc(sizeof(struct name_entry) * name_size)) == 0 ) {
		perror("getfile: ");
		return;
	}

	typeseq.length = 0;  typeseq.sequence = tsvals;
	scopeseq.length= 1; scopeseq.sequence= &scope;
	scope.designator= FilingSubset1_filter;
	scope.FilingSubset1_filter_case.designator= FilingSubset1_matches;
	if ( filing_subset )
		scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute.type= FilingSubset1_pathname;
	else
		scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute.type= FilingSubset1_name;

	copyhandle(remotehandle, FilingSubset1_nullHandle);
	getdirhandle(remote, dirhandle);

	if ( filing_subset )
		StringToAttr(cur_pathname,&scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute);
	else
		StringToAttr(cur_name,&scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute);

	typeseq.length = 2;
	typeseq.sequence[0] = FilingSubset1_pathname;
	typeseq.sequence[1] = FilingSubset1_type;

	ProcEachSeq = listproc;
	if ( filing_subset )
		FilingSubset1_List(connected, GetAttributeSequences, dirhandle,
			     typeseq, scopeseq,
			     BulkData1_immediateSink, session);
	else
		Filing4_List(connected, GetAttributeSequences, dirhandle,
			     typeseq, scopeseq,
			     BulkData1_immediateSink, session);


	if ( !files_found ) {
		fprintf(stderr, "\n(%s)%s not found\n", service, remote);
	} else {
		for ( i= 0 ; i < name_count ; i++ ) {
			entry= &name_list[i];
#ifdef PRINTOPTION
			if ( (entry->type != FilingSubset1_tText) && (entry->type != TYPE_VPMailNote) &&
					(entry->type != TYPE_Interpress) ) {
				fprintf(stderr, "\n\n\n\nInvalid file type (%d)\n\n\n\n", entry->type);
				clear_String(&entry->pathname);
				continue;
			}

			if ( entry->type == TYPE_Interpress ) {
				sprintf(pcmd, "%s -q -b \"(%s)%s\"%s -", XNSPRINT, service, entry->pathname, thru_options);
			} else {
				sprintf(pcmd, "%s -b \"(%s)%s\"%s", MAHA, service, entry->pathname, thru_options);
			}

			if ( (fout= popen(pcmd, "w")) == NULL ) {
				perror("popen: ");
				exit(1);
			}

			if (verbose) {
			 	fprintf(stderr, "\n\printing (%s)%s...\n\n",service,entry->pathname);
			}
#else PRINTOPTION
			if ( entry->type != FilingSubset1_tText ) {
				fprintf(stderr, "\n\n\nCan only view text files.\n\n\n");
				clear_String(&entry->pathname);
				continue;
			}

			if ( pager == NULL || *pager == '\0' ) {
				fout= stdout;
			} else {
				sprintf(pcmd, "%s%s", pager, thru_options);
				if ( (fout= popen(pcmd, "w")) == NULL )
					fout= stdout;
			}

			if (verbose) {
			 	fprintf(stderr, "\n\n\n\nretrieving (%s)%s...\n\n\n\n",service,entry->pathname);
			}
#endif PRINTOPTION

			typevalue= entry->type;

			getfilehandle(entry->pathname, remotehandle);	/* get file handle */
			if ( filing_subset ) {
				FilingSubset1_Retrieve(connected, retrieveproc, remotehandle,
						 BulkData1_immediateSink, session);
			} else {
				Filing4_Retrieve(connected, retrieveproc, remotehandle,
						 BulkData1_immediateSink, session);
			}

			freefilehandle(remotehandle);
#ifdef PRINTOPTION
			pclose(fout);
#else PRINTOPTION
			if ( fout != stdout )
				pclose(fout);
#endif PRINTOPTION

			clear_String(&entry->pathname);
		}
	} 

	free(name_list);
	freefilehandle(dirhandle);

}

listproc(attr)
	FilingSubset1_AttributeSequence attr;
{
	int i;
	FilingSubset1_AttributeType t;
	struct name_entry *entry;

	files_found= TRUE;

/*
 *	Xerox file servers will return all versions of the requested file in
 *	ascending version order. We assume that the last version will be the
 *	highest and remember that name so that the retrieve will pull the
 *	highest version of the file. If we request just the file with no
 *	version, the server will return the oldest version (not what I would
 *	expect...)
 */

	if ( name_count > name_size ) {
		name_size += MAX_NAMES;
		name_list= (struct name_entry *) realloc(name_list,
				sizeof(struct name_entry) * name_size);
	}

	entry= &name_list[name_count];
	for (i = 0; i < attr.length; i++) {
		t = attr.sequence[i].type;
		if (t == FilingSubset1_name || t == FilingSubset1_pathname) {
			entry->pathname = AttrToString(&attr.sequence[i]);
		} else if (t == FilingSubset1_type) {
			entry->type = AttrToLongCardinal(&attr.sequence[i]);
		}
	}
	name_count++;
}


#define MAXPACKS 20
static
GetAttributeSequences(conn)
	CourierConnection *conn;
{
	int count, i;
	Unspecified buffer[MAXWORDS*MAXPACKS], *bp, *bufend;
	FilingSubset1_StreamOfAttributeSequence attrs;
	
	files_found= FALSE;

	bufend = buffer;
	bp = buffer+((MAXWORDS-1)*MAXPACKS);    /* end of available space */
	while ((count = BDTread(conn, (char*)bufend, 
				MAXWORDS*sizeof(Unspecified))) > 0) {
		bufend += count/sizeof(Unspecified);
		if (bufend > bp) {
			fprintf(stderr,"BDT read too big to fit\n");
			BDTabort(conn);
			/* should clear out stuff here if we knew how much */
		}
	}
	bp = buffer;
	while (bp < bufend) {
		bp += internalize_FilingSubset1_StreamOfAttributeSequence(&attrs,bp);
		if (0 == (int) attrs.designator) {
		   for (i=0; i < attrs.nextSegment_case.segment.length; i++) {
			(*ProcEachSeq)(
				attrs.nextSegment_case.segment.sequence[i]);
		   }
		   free(attrs.nextSegment_case.segment.sequence);
		} else {
		   for (i = 0; i < attrs.lastSegment_case.length; i++) {
			(*ProcEachSeq)(
				attrs.lastSegment_case.sequence[i]);
		   }
		   free(attrs.lastSegment_case.sequence);
		   return;
		}
	}
}

int
getBDTch(conn,bpp)
	CourierConnection *conn;
	u_char **bpp;
{
	static u_char buffer[SPPMAXDATA];
	static int count;

	if (*bpp == NULL) {*bpp = buffer; count = 0;}
	if (*bpp >= buffer+count) {
		count=BDTread(conn,buffer,sizeof(buffer));
		*bpp = buffer;
	}
	if (count <= 0) return(EOF);
	else return(*((*bpp)++));
		
}

retrieveproc(conn)
	CourierConnection *conn;
{
	int count, ocount, ch;
	char buffer[SPPMAXDATA];
	int charset, charset16;
	char *bp;

	errno = ocount = 0;
	fflush(fout);

	switch (typevalue) {
	default :
		fprintf(stderr, "Unsupported file type\n");
		BDTabort(conn);
		break;

	case TYPE_Interpress :
		while ((count = BDTread(conn, buffer, sizeof(buffer))) > 0) {
			if ((ocount = write (fileno(fout), buffer, count)) < 0) {
				perror("write");
				BDTabort(conn);
				break;
			}
		}
		if (count < 0) perror("netin");
		break;

	case TYPE_VPMailNote :
	case TYPE_A :
		charset = 0; charset16 = 0; bp = NULL;
		while ((ch = getBDTch(conn,&bp)) != EOF) {
			if (ch == '\377') {
				ch = getBDTch(conn,&bp);
				if (ch == '\377') charset16 = 1;
				else charset = ch;
				continue;
			}
			if (charset16) {
				charset = ch;
				ch = getBDTch(conn,&bp);
			}
			switch (charset) {
			case 0:	/* normal character set -- minimal xlation */
				if (ch == '\r') {
				    	int nextch;

				    	putc('\n',fout);
				    	if ( (nextch = getBDTch(conn,&bp)) != '\n'){
					    if (nextch == '\r')
						putc('\n',fout);
					    else if ( nextch != EOF )
				    		putc(nextch,fout);
					    else
						continue;
					}
					break;
				}
				else if (ch == ','+0200) ch = '_';
				/* more mapping here */
				putc(ch,fout);
				break;
			default:
				break; /* ignore */
			}
		}
		/* if (count < 0) perror("netin"); */
		break;
	}
}

