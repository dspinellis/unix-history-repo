#ifndef lint
static char *rcsid = "$Header: xnsrestore.c,v 1.1 87/03/17 16:27:26 ed Exp $";
#endif lint

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* $Log:	xnsrestore.c,v $
 * Revision 1.1  87/03/17  16:27:26  ed
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
#include <xnscourier/Filing4.h>
#include <xnscourier/except.h>
#include <xnscourier/CH.h>
#include <xnscourier/filetypes.h>
#define XNS_TIME_DIFFERENCE	2177452800	/* [(1970-1901) years * 365 days/year + 17 leap days */
						/* * 24 hours/day * 60 minutes/hour * 60 seconds/minute */

#define ROOT_DIRECTORY "/"

CourierConnection *connected;
Clearinghouse2_ObjectName hostobjname;
Authentication2_Verifier verifier;

/* the following 3 items make up the current session */
Filing4_Session session;	/* the current session */
Clearinghouse2_ObjectName username;
Filing4_Handle rootHandle;
char cur_dir[512]= 0;
char cur_pathname[512]= 0;
char cur_name[512]= 0;

static Filing4_ControlSequence nullControls = {0,0};

/* global data used to communicate with BDT procedures
 */
extern GetAttributeSequences(), 
	listproc(), isdirproc(), storeproc();

char *AttrToString();
Boolean AttrToBoolean();
LongCardinal AttrToLongCardinal();
Cardinal AttrToCardinal();

static (*ProcEachSeq)();
static FILE *fin= NULL;
FILE *lfile= NULL;

Boolean files_found= FALSE;
Boolean verbose= FALSE;
Boolean is_a_directory= FALSE;
char *logfile= 0;

char *ctime();
long time();
char *service;
extern int errno;

main(argc, argv)
int argc;
char *argv[];
{
	char *remotefile;
	char *localfile;
	int i;
	CourierConnection *hookup();
	int opt;
	extern int optind;
	extern char *optarg;

	static char *options= "vl:";
	static char *usage= "Usage: %s [-v] [-l log-file] local-file remote-file\n";

	if ( argc < 3 ) {
		fprintf(stderr, usage, argv[0]);
		exit(1);
	}

	while ((opt= getopt(argc, argv, options)) != EOF) 
		switch (opt) {
			case 'l' :
				logfile= optarg;
				break;

			case 'v' :
				verbose++;
				break;

			default:
				fprintf(stderr, "Invalid command option -%c\n", opt);
				exit(1);
		}

	localfile= argv[optind];
	optind++;
	if ( getserviceandfile(argv[optind], &service, &remotefile) == 0 ) {
		fprintf(stderr, "Invalid name %s\n", argv[optind]);
		exit(1);
	}
	DURING {
		if ( (connected= hookup(service)) == (CourierConnection *)0 ) {
				fprintf(stderr, "\nCan't connect to %s\n", service);
				exit(1);
		}
		login(0,0);

		deserializefile(localfile, remotefile);

	} HANDLER {
		FilingErrMsg(Exception.Code, Exception.Message);
	} END_HANDLER;

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
	Filing4_Handle dest,src;
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
	Filing4_Handle handle;
{
	Filing4_Attribute pathattr[1];
	Filing4_AttributeSequence attrseq;
	Filing4_OpenResults openresult;
	Filing4_OpenResults openresult2;

	if (filename == (char *)0 || *filename == '\000' || (strcmp(filename, "/") == 0) ) {
		copyhandle(handle,rootHandle);
		return;
	}

	attrseq.length = 1;
	attrseq.sequence = pathattr;
	pathattr[0].type = Filing4_pathname;
	copyhandle(handle, Filing4_nullHandle);
#ifdef XEROXFSCOMPATIBILITY
	if ( filename[0] == '/')
		StringToAttr(filename+1, &pathattr[0]);
	else
		StringToAttr(filename, &pathattr[0]);
#else XEROXFSCOMPATIBILITY
	StringToAttr(filename, &pathattr[0]);
#endif XEROXFSCOMPATIBILITY
	alarm(0);
	openresult2 = Filing4_Open(connected, NULL, attrseq,
				  handle, nullControls,
				  session);
	copyhandle(handle, openresult2.file);
}

getdirhandle(filename, handle)
	char *filename;
	Filing4_Handle handle;
{
	Filing4_Attribute pathattr[1];
	Filing4_AttributeSequence attrseq;
	Filing4_OpenResults openresult;
	Filing4_OpenResults openresult2;
	char *rindex();
	char *slash;

	if (filename == (char *)0 || *filename == '\000' || (strcmp(filename, "/") == 0) ) {
		strcpy(cur_pathname, "/");
		strcpy(cur_name, "/");
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

	if ( slash == cur_pathname) {
		copyhandle(handle, rootHandle);
		return;
	}

	attrseq.length = 1;
	attrseq.sequence = pathattr;
	pathattr[0].type = Filing4_pathname;
	copyhandle(handle, Filing4_nullHandle);
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
	openresult2 = Filing4_Open(connected, NULL, attrseq,
				  handle, nullControls, session);
	copyhandle(handle, openresult2.file);
}

freefilehandle(handle)
	Filing4_Handle handle;
{
	if (handle[0] == Filing4_nullHandle[0] &&
	    handle[1] == Filing4_nullHandle[1])
		return;		/* don't free nullHandle */
	if (handle[0] == rootHandle[0] &&
	    handle[1] == rootHandle[1])
		return;		/* don't free root directory */
	alarm(0);
	Filing4_Close(connected, NULL, handle, session);
}

CourierConnection *
hookup(name)
	char *name;
{
	register struct ns_addr *hostaddr;
	extern struct ns_addr *getXNSaddr();
	Clearinghouse2_ObjectName defaultobjname;
	static char hnamebuf[128];
	CourierConnection *cconn;

	CH_NameDefault(&defaultobjname);
	hostobjname = CH_StringToName(name, &defaultobjname);
	if ((hostaddr = CH_LookupAddrDN( hostobjname, 0, hnamebuf, 128))) {
		/* should check here to be sure host is a file service */
		hostaddr->x_port = htons(5); /* ?? */
		cconn = CourierOpen(hostaddr);
		/* reset objname to flush wildcards */
		/* clear_Clearinghouse2_ThreePartName(&hostobjname); */
		hostobjname = CH_StringToName(hnamebuf, &defaultobjname);
	}
	return(cconn);
}


login(name,pwd)
	char *pwd;
	char *name;
{
	Filing4_Credentials credentials;
	Filing4_LogonResults logonresult;
	Filing4_AttributeSequence attrseq;
	Filing4_OpenResults openresult;


	if ( name != 0 )
		username = CH_StringToName(name,&hostobjname);

	if ( name == 0 && pwd == 0 ) {
		GetSimpleCredsAndVerifier(&username, 0, 
					&credentials, &verifier);
	} else {
		MakeSimpleCredsAndVerifier(&username,pwd,
					&credentials, &verifier);
	}
	logonresult= Filing4_Logon(connected, NULL, hostobjname,
					credentials, verifier);
	session = logonresult.session;

	attrseq.length= 0;
	attrseq.sequence= 0;
	openresult= Filing4_Open(connected, NULL, attrseq,
				 Filing4_nullHandle, nullControls,
				 session);
	copyhandle(rootHandle, openresult.file);
	strcpy(cur_dir, ROOT_DIRECTORY);
}

logout()
{
	Filing4_Logoff(connected, NULL, session);
	clear_Filing4_Session(&session);
}


deserializefile(local, remote)
	char *local;
	char *remote;
{
	FILE *fopen();
	Filing4_Handle remotehandle; /* note: an array */
	Filing4_Handle dirhandle; /* note: an array */
	Filing4_Handle listhandle; /* note: an array */
	Filing4_DeserializeResults results;
	Filing4_AttributeSequence attrseq;
	Filing4_Attribute attrvals[50];
	Filing4_AttributeTypeSequence typeseq;
	Filing4_AttributeType tsvals[10];
	Filing4_ScopeSequence scopeseq;
	Filing4_Scope scope;
	Filing4_ScopeSequence lscopeseq;
	Filing4_Scope lscope;
	int i;
	char *name, *rindex();
	long date;
	Boolean piping= FALSE;

	name= '\0';

	scopeseq.sequence= &scope; lscopeseq.sequence= &lscope;
	attrseq.sequence= attrvals;

	if ( strcmp(local, "-") == 0 ) {
		fin= stdin;
		piping= TRUE;
	} else {
		if ( (fin= fopen(local, "r")) == NULL ) {
			perror("Cannot open local file ");
			return(1);
		}
		if ( (name= rindex(local, '/')) == 0 ) {
			name= local;
		} else {
			name++;
		}
	}

	getdirhandle(remote, dirhandle);

	scopeseq.length= 1;
	scope.designator= Filing4_filter;
	scope.Filing4_filter_case.designator= Filing4_matches;
	scope.Filing4_filter_case.Filing4_matches_case.attribute.type= Filing4_name;
	StringToAttr(cur_name,&scope.Filing4_filter_case.Filing4_matches_case.attribute);

	typeseq.length = 2;  typeseq.sequence = tsvals;
	typeseq.sequence[0] = Filing4_name;
	typeseq.sequence[1] = Filing4_isDirectory;

	is_a_directory= FALSE;

	ProcEachSeq = isdirproc;
	Filing4_List(connected, GetAttributeSequences, dirhandle,
				     typeseq, scopeseq,
				     BulkData1_immediateSink, session);

	if ( files_found ) {
		if ( is_a_directory ) {
			if ( piping ) {
				fprintf(stderr, "Must specify file name when pinput is from stdin\n");
				return(1);
			}
			freefilehandle(dirhandle);
			getfilehandle(remote, dirhandle);
			strcat(cur_pathname, "/");
			strcat(cur_pathname, name);
		} else {
			name= cur_name;
		}
	} else {
		name= cur_name;
	}

	attrseq.length= 1;
	attrvals[0].type= Filing4_name;
	StringToAttr(name, &attrvals[0]);

	if ( AddExtendedDeserializeAttributes(fin, &attrseq) != -1 ) {
		fprintf(stderr, "Cannot determine extended attributes\n");
		fclose(fin);
		return(1);
	}

	if ( verbose ) {
		fprintf(stdout, "   Restoring %s\n", cur_pathname);
	}

	results= Filing4_Deserialize(connected, storeproc, dirhandle,
				 attrseq, nullControls,
				 BulkData1_immediateSink, session);

	if ( logfile ) {
		if ( (lfile= fopen(logfile, "w")) != NULL ) {
			scopeseq.length= 1;
			scope.designator= Filing4_filter;
			scope.Filing4_filter_case.designator= Filing4_matches;
			scope.Filing4_filter_case.Filing4_matches_case.attribute.type= Filing4_name;
			StringToAttr(name,&scope.Filing4_filter_case.Filing4_matches_case.attribute);
	
			typeseq.length = 2;  typeseq.sequence = tsvals;
			typeseq.sequence[0] = Filing4_pathname;
			typeseq.sequence[1] = Filing4_isDirectory;
	
			is_a_directory= FALSE;
	
			ProcEachSeq = isdirproc;
			Filing4_List(connected, GetAttributeSequences, dirhandle,
					     typeseq, scopeseq,
					     BulkData1_immediateSink, session);

			/*
			 * for a directory, we list all files...
			 * for non-directory, just list it...
			 */
			if ( is_a_directory ) {
				copyhandle(listhandle, results.file);
				lscopeseq.length= 1;
				lscope.designator= Filing4_depth;
				lscope.Filing4_depth_case= Filing4_allDescendants;
			} else {
				copyhandle(listhandle, dirhandle);
				lscopeseq.length= 1;
				lscope.designator= Filing4_filter;
				lscope.Filing4_filter_case.designator= Filing4_matches;
				lscope.Filing4_filter_case.Filing4_matches_case.attribute.type= Filing4_name;
				StringToAttr(name,&lscope.Filing4_filter_case.Filing4_matches_case.attribute);
	
			}

			typeseq.length = 4;
			typeseq.sequence[0] = Filing4_pathname;
			typeseq.sequence[1] = Filing4_type;
			typeseq.sequence[2] = Filing4_createdOn;
			typeseq.sequence[3] = Filing4_modifiedOn;

			date= time(0);
			fprintf(lfile, "\n\n\tRestore of %s\n\tPerformed on %s\n\n\n", cur_pathname, ctime(&date));
			fprintf(lfile, "\tFiles restored as follows:\n\n");
			fprintf(lfile, "    Create Date\t\t Modification Date\t    Type\t\t\t\t\t\tName\n\n");
	
			ProcEachSeq = listproc;
			Filing4_List(connected, GetAttributeSequences, listhandle,
					     typeseq, lscopeseq,
					     BulkData1_immediateSink, session);
			fclose(lfile);
		}
	}

	freefilehandle(results.file);
	freefilehandle(dirhandle);
}

listproc(attr)
	Filing4_AttributeSequence attr;
{
	int i;
	char *thisname, *typetostring();
	char createstr[30], modstr[30];
	LongCardinal thistype, createdate, moddate;
	Filing4_AttributeType t;

	files_found= TRUE;
	createdate= moddate= time(0);

	for (i = 0; i < attr.length; i++) {
		t = attr.sequence[i].type;
		if (t == Filing4_pathname) {
			thisname = AttrToString(&attr.sequence[i]);
		} else if (t == Filing4_type) {
			thistype = AttrToLongCardinal(&attr.sequence[i]);
		} else if (t == Filing4_createdOn) {
			createdate= AttrToLongCardinal(&attr.sequence[i]);
			createdate= createdate - XNS_TIME_DIFFERENCE;
			strcpy(createstr, ctime(&createdate));
			createstr[24]= '\0';
		} else if (t == Filing4_modifiedOn) {
			moddate= AttrToLongCardinal(&attr.sequence[i]);
			moddate= moddate - XNS_TIME_DIFFERENCE;
			strcpy(modstr, ctime(&moddate));
			modstr[24]= '\0';
		}
	}

	fprintf(lfile, "%s\t%s\t%-16s\t%s\n", createstr+4, modstr+4, typetostring(thistype), thisname);
	clear_String(&thisname);
}

isdirproc(attr)
	Filing4_AttributeSequence attr;
{
	int i;
	Filing4_AttributeType t;
	char *thisname;

	files_found= TRUE;

	for (i = 0; i < attr.length; i++) {
		t = attr.sequence[i].type;
		if (t == Filing4_isDirectory) {
			is_a_directory = AttrToBoolean(&attr.sequence[i]);
		} else if (t == Filing4_name) {
			thisname= AttrToString(&attr.sequence[i]);
			strcpy(cur_name, thisname);
			clear_String(&thisname);
		}
	}

}

#define MAXPACKS 20
static
GetAttributeSequences(conn)
	CourierConnection *conn;
{
	int count, i;
	Unspecified buffer[MAXWORDS*MAXPACKS], *bp, *bufend;
	Filing4_StreamOfAttributeSequence attrs;
	
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
		bp += internalize_Filing4_StreamOfAttributeSequence(&attrs,bp);
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

storeproc(conn)
	CourierConnection *conn;
{
	int count, ocount, ch;
	char buffer[SPPMAXDATA];
	char *bp;

	errno = ocount = 0;
	clearerr(fin);

	while ( ((count= fread(buffer, sizeof(char), SPPMAXDATA, fin)) != 0)
		 && (ocount= BDTwrite (conn, buffer, count)) > 0) {
			;
	}
	if ( count < 0 ) {
		BDTabort(conn);
		perror("netout");
		exit(1);
	} else if ( ferror(fin) ) {
		BDTabort(conn);
		perror("fread");
		exit(1);
	} else {
		BDTclosewrite(conn);
	}
}

