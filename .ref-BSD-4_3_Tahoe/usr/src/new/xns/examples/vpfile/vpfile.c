#ifndef lint
static char *rcsid = "$Header: vpfile.c,v 1.2 87/04/01 08:23:06 ed Exp $";
#endif lint

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* $Log:	vpfile.c,v $
 * Revision 1.2  87/04/01  08:23:06  ed
 * Changed for new MakeSecondaryCreds call.
 * 
 * Revision 1.1  87/03/17  16:28:30  ed
 * Initial revision
 * 
 * 
 */

#include <stdio.h>
#include <sys/file.h>
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
char *myhostname, mydir[512], olddir[512];

static FilingSubset1_ControlSequence nullControls = {0,0};
static FilingSubset1_ScopeSequence nullScope = {0,0};

/* global data used to communicate with BDT procedures
 */
extern GetAttributeSequences(), 
	listproc();

char *rindex(), *typetostring();
char *AttrToString();
Boolean AttrToBoolean();
LongCardinal AttrToLongCardinal();
Cardinal AttrToCardinal();

static (*ProcEachSeq)();

Boolean files_found= FALSE;
Boolean filing_subset= TRUE;
Boolean usefiling= TRUE;

char *service, oldservice[100];

main(argc, argv)
int argc;
char *argv[];
{
	char *file, *rightbrkt;
	char *malloc();
	int i;
	CourierConnection *hookup();
	int opt;
	extern int optind;
	extern char *optarg;

	static char *options= "f";
	static char *usage= "Usage: %s [-f] file1 ... filen\n";

	if ( argc < 1 ) {
		fprintf(stderr, usage, argv[0]);
		exit(1);
	}

	gethostname(myhostname=malloc(100),100);
	getwd(mydir);

	while ((opt= getopt(argc, argv, options)) != EOF) 
		switch (opt) {
			case 'f':
				usefiling= 0;
				break;

			default:
				fprintf(stderr, "Invalid command option -%c\n", opt);
				exit(1);
		}

	for ( ; optind < argc ; optind++ ) {
		if ( getserviceandfile(argv[optind], &service, &file) == 0 ) {
			fprintf(stderr, "Invalid name %s\n", argv[optind]);
			exit(1);
		}

		if ( service == 0 ) {
			locallist(argv[optind]);
			continue;
		}
		DURING {
			if ( strcmp(oldservice, service) != 0 ) {
				if ( (connected= hookup(service)) == (CourierConnection *)0 ) {
					fprintf(stderr, "\nCan't connect to %s\n", service);
					continue;
				}
				login(0,0);
				strcpy(oldservice, service);

				printf("\n\n(%s):\n", service);
			}

			remotelist(file);

		} HANDLER {
			FilingErrMsg(Exception.Code, Exception.Message);
		} END_HANDLER;

	}

	return(0);
}

getserviceandfile(name, srvcptr, fileptr)
char *name;
char **srvcptr, **fileptr;
{
	char *sptr, *fptr;
	char *index();

	*srvcptr= 0;

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
	} 
	return(1);
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

getdirhandle(filename, handle)
	char *filename;
	FilingSubset1_Handle handle;
{
	FilingSubset1_Attribute pathattr[1];
	FilingSubset1_AttributeSequence attrseq;
	FilingSubset1_OpenResults openresult;
	Filing4_OpenResults openresult2;
	char *slash;

	if (filename == (char *)0 || *filename == '\000' || (strcmp(filename, "/") == 0) ) {
		strcpy(cur_pathname, "/");
		strcpy(cur_name, "/");
		if ( filing_subset )
			copyhandle(handle,FilingSubset1_nullHandle);
		else
			copyhandle(handle,rootHandle);
		printf("\n/ :\n\n");
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
			printf("\n/ :\n\n");
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
		printf("\n%s :\n\n",cur_pathname);
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
		usefiling= 0;
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
		usefiling= 0;
		if ( name == 0 && pwd == 0 ) {
			GetSimpleCredsAndVerifier(&username, 0, &credentials.primary, &verifier);
			MakeSecondaryCreds(hostobjname.object, username.object, 0, &credentials.secondary);
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

remotelist(remote)
	char *remote;
{
	FilingSubset1_Handle dirhandle; /* note: an array */
	FilingSubset1_AttributeTypeSequence typeseq;
	FilingSubset1_AttributeType tsvals[10];
	FilingSubset1_ScopeSequence scopeseq;
	FilingSubset1_Scope scope;

	typeseq.length = 0;  typeseq.sequence = tsvals;
	scopeseq.length= 1; scopeseq.sequence= &scope;
	scope.designator= FilingSubset1_filter;
	scope.FilingSubset1_filter_case.designator= FilingSubset1_matches;
	if ( filing_subset )
		scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute.type= FilingSubset1_pathname;
	else
		scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute.type= FilingSubset1_name;

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


	if ( !files_found )
		fprintf(stderr, "\n(%s)%s not found\n", service, remote);

	freefilehandle(dirhandle);

}

listproc(attr)
	FilingSubset1_AttributeSequence attr;
{
	int i;
	FilingSubset1_AttributeType t;
	char *thisname, *name;
	LongCardinal thistype;

	files_found= TRUE;

	for (i = 0; i < attr.length; i++) {
		t = attr.sequence[i].type;
		if (t == FilingSubset1_name || t == FilingSubset1_pathname) {
			thisname = AttrToString(&attr.sequence[i]);
		} else if (t == FilingSubset1_type) {
			thistype = AttrToLongCardinal(&attr.sequence[i]);
		}
	}

	if ( (name= rindex(thisname, '/')) == 0 )
		name= thisname;
	else
		name++;

	printf("  %s:\t%s\n", name, typetostring(thistype));
	clear_String(&thisname);
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


locallist(file)
	char *file;
{
	FILE *fin, *fopen();
	int type;
	char *dir, *slash;

	if ( strcmp(oldservice, myhostname) != 0 ) {
		printf("\n\n(%s):\n", myhostname);
		strcpy(oldservice, myhostname);
	}

	if ( (slash= rindex(file, '/')) == 0 ) {
		dir= mydir;
	} else {
		*slash= '\0';
		dir= (slash == file ? "/" : file);
	}

	if ( strcmp(dir, olddir) != 0 ) {
		printf("\n%s:\n\n", dir);
		strcpy(olddir, dir);
	}

	if ( slash ) *slash= '/';

	if ( access(file, R_OK | F_OK ) == -1 ) {
		fprintf(stderr, "  %s:\tError ", (slash ? slash+1 : file));
		perror("");
		return;
	}

	type= get_type(file);

	printf("  %s:\t%s\n", (slash ? slash+1 : file), typetostring(type));

}
