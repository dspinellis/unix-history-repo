#ifndef lint
static char *rcsid = "$Header: xnftp.c,v 2.13 87/05/28 14:17:14 ed Exp $";
#endif lint

/* $Log:	xnftp.c,v $
 * Revision 2.13  87/05/28  14:17:14  ed
 * I botched the compiler bug fix...
 * 
 * Revision 2.12  87/05/14  11:37:34  ed
 * Raise error from hookup if connection not established.
 * Better handling of truncated bulk data stream when it overflows buffer.
 * 
 * Revision 2.11  87/05/11  14:40:12  ed
 * Incorporated changes from JQ's 4.3e version.
 * 
 * Revision 2.11  87/03/08  07:09:53  jqj
 * work around "schain botch" 4.3BSD VAX compiler bug (from Scooter Morris).
 * 
 * Revision 2.10  87/05/05  14:49:00  ed
 * Move alarm setting/resetting closer to actual procedure calls.
 * 
 * Revision 2.9  87/04/16  15:23:47  ed
 * Fixed lingering bugs in Subset pathname usage.
 * 
 * Revision 2.8  87/04/01  09:33:36  ed
 * Changed for new MakeSecondaryCreds call.
 * Reset connection on login failure.
 * 
 * Revision 2.7  87/03/27  15:19:19  ed
 * Don't assume secondary username is primary name.
 * Additional check for underscore translation on text files.
 * 
 * Revision 2.6  87/03/23  12:32:12  ed
 * allow round-trip transfer of Viewpoint files, retain/specify uninterpreted
 * 	attributes.
 * Serialization/Deserialization of directories from server.
 * Wildcard deletion.
 * Compatibility with XDE MFileServer.
 * New commands: Archive, Restore, Unify.
 * 
 * Revision 2.5  87/01/14  15:59:29  ed
 * Use FilingSubset, if rejected attempt Filing
 * Allows user override with -F switch
 * Maintain FilingSubset mandatory attributes
 * User niceties:  echo file name/type on transfer commands
 * 		prompt on delete
 * guess type which will determine file type implied by content
 * New commands: (type related) Guess, Whatis
 * 	      (file transfer) Copy, Move, Rename
 * 
 * Revision 2.4  86/12/15  11:41:16  jqj
 * Added support for more ViewPoint file types (no other attributes, though)
 * 
 * Revision 2.3  86/12/11  06:12:22  jqj
 * Eliminated form, mode, and struct commands.  Started adding support for
 * more file types.
 * 
 * Revision 2.2  86/09/07  07:43:40  jqj
 * Cope with failure return from CourierOpen.
 * 
 * Revision 2.1  86/06/30  12:19:39  jqj
 * convert to Authentication v. 2 for compatibility with official spec.
 * 
 * Revision 2.0  85/11/21  07:22:51  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.5  85/09/24  14:45:10  jqj
 * fix bug in alarm() handling that caused aborts during large file transfers.
 * 
 * Revision 1.4  85/09/17  07:49:47  jqj
 * 4.3 changes.  Use more routines from CHlookup
 *
 * Revision 1.1  85/05/27  06:31:07  jqj
 * Initial revision
 * 
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/time.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <netns/ns.h>
#include <netns/sp.h>
#include "ftp_var.h"
#include <xnscourier/Filing4.h>
#include <xnscourier/except.h>
#undef __Clearinghouse2			/* Filing4.h defs this */
#include <xnscourier/CH.h>

#define	XNS_TIME_DIFFERENCE	2177452800		/* [(1970-1901) years * 365 days/year + 17 leap days */
							/* * 24 hours/day * 60 minutes/hour * 60 seconds/minute */
#define ROOT_DIRECTORY	"/"
#define MAXNAMES	10

CourierConnection *connected;
Clearinghouse3_ObjectName hostobjname;
Authentication3_Verifier verifier;

/* the following 3 items make up the current session */
FilingSubset1_Session session;	/* the current session */
Clearinghouse3_ObjectName username;
int continuetime;
int remoteprocpending;
FilingSubset1_Handle rootHandle;
char cur_dir[512]= 0;
char cur_pathname[512]= 0;
char cur_name[512]= 0;

struct name_entry {
	char *pathname;
	LongCardinal type;
} ;

static struct name_entry *name_list= 0;
static int name_count= 0;
static int name_size= 0;

static FilingSubset1_ControlSequence nullControls = {0,0};
static FilingSubset1_ScopeSequence nullScope = {0,0};

/* global data used to communicate with BDT procedures
 */
extern GetAttributeSequences(), 
	GetAllAttributes(),
	listproc(), nlistproc(),
	storeproc(), retrieveproc(),
	rlistproc(), mkdirproc(),
	cdproc(), isdirproc(), deleteproc();

char *malloc();
char *AttrToString();
Boolean AttrToBoolean();
LongCardinal AttrToLongCardinal();
Cardinal AttrToCardinal();
char *typetostring();

static (*ProcEachSeq)();
static long bytessent;
static FILE *fout, *fin;

LongCardinal filetypevalue;		/* real transfer type */

struct timeval timbuf[2];
Boolean is_a_directory= FALSE;
Boolean files_found= FALSE;
Boolean filing_subset= TRUE;
Boolean isdir= FALSE;

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
	alarm(continuetime);
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
	char *slash, *bang;

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

	if ( (bang= rindex(cur_name, '!')) != NULL )
		*bang= '\0';

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
		alarm(continuetime);
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
	alarm(continuetime);
}

/*
 * do a continue to make sure that the session doesn't time out.
 * Note that this is usually called by an ALARM interrupt
 */
probe()
{
	FilingSubset1_ContinueResults cresult;
	Filing4_ContinueResults cresult2;
	alarm(0);		/* cancel previous alarms */
	if ( filing_subset ) {
		cresult = FilingSubset1_Continue(connected, NULL, session);
		continuetime = cresult.continuance / 5; /* seconds */
	} else {
		cresult2 = Filing4_Continue(connected, NULL, session);
		continuetime = cresult2.continuance / 5; /* seconds */
	}
	alarm(continuetime);	/* reset for another 2 min. or so */
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
		if ( cconn == (CourierConnection *) 0 ) {
			Cardinal problem;
			problem= FilingSubset1_noResponse;
			raise(FilingSubset1_ConnectionError, &problem);
		}
		/* reset objname to flush wildcards */
		/* clear_Clearinghouse3_ThreePartName(&hostobjname); */
		hostobjname = CH_StringToName(hnamebuf, &defaultobjname);
		hostname = hnamebuf;
		if (verbose)
		  printf("Connected to %s\n", hnamebuf);
	} else {			
		printf("%s: unknown host\n", name);
		usefiling= 0;
		cconn = (CourierConnection*)0;
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
				connected= (CourierConnection *)0;	/* reset */
				RERAISE;
			}
		} END_HANDLER;
	}

	if ( filing_subset )
		session = resultptr->session;
	else
		session = resultptr->session;
	if (verbose)
	  printf("User %s:%s:%s logged on\n", username.object,
		 username.domain, username.organization);

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
	alarm(0);
	signal(SIGALRM, probe);
	probe();
}

logout()
{
	signal(SIGALRM, SIG_IGN);
	if ( filing_subset )
		FilingSubset1_Logoff(connected, NULL, session);
	else
		Filing4_Logoff(connected, NULL, session);
	clear_FilingSubset1_Session(&session);
}

domakedir(dest)
	char *dest;
{
	struct timeval start, stop, time;
	FilingSubset1_StoreResults storeresults;
	Filing4_StoreResults storeresults2;
	FilingSubset1_Handle dirhandle;
	FilingSubset1_AttributeSequence attrseq;
	FilingSubset1_Attribute	attrvals[5];

	gettimeofday(&time, (struct timezone *) 0);

	if (dest) {
		getdirhandle(dest, dirhandle);
	} else {
		printf("No remote name specified\n");
		return;
	}

	bytessent= 0;
	alarm(0);

	attrseq.length= 3;
	attrseq.sequence= attrvals;

	if ( filing_subset ) {
		attrvals[0].type= FilingSubset1_pathname;
		StringToAttr(cur_pathname, &attrvals[0]);
	} else {
		attrvals[0].type= FilingSubset1_name;
		StringToAttr(cur_name, &attrvals[0]);
	}
	attrvals[1].type = FilingSubset1_isDirectory;
	BooleanToAttr(TRUE, &attrvals[1]);
	attrvals[2].type = FilingSubset1_type;
	LongCardinalToAttr(FilingSubset1_tDirectory, &attrvals[2]);
	gettimeofday(&start, (struct timezone *)0);
	if ( filing_subset )
		storeresults = FilingSubset1_Store(connected, BDTclosewrite,
					       dirhandle, attrseq,
					       nullControls,
					       BulkData1_immediateSource, session);
	else
		storeresults2 = Filing4_Store(connected, BDTclosewrite,
					       dirhandle, attrseq,
					       nullControls,
					       BulkData1_immediateSource, session);
	alarm(continuetime);
	gettimeofday(&stop, (struct timezone *)0);
	if ( filing_subset )
		freefilehandle(storeresults.file);
	else
		freefilehandle(storeresults2.file);

	freefilehandle(dirhandle);
}

doremovedir(src)
	char *src;
{
	dodelete(src);
}

dostore(src, dest)
	char *src, *dest;
{
	sendrequest("STOR", src, dest);
}

doappend(src, dest)
	char *src, *dest;
{
	NYI();
}

dorename(src, dest)
	char *src, *dest;
{
	Filing4_Handle srchandle, dirhandle;
	Filing4_AttributeSequence attrseq;
	Filing4_Attribute attrvals[1];

	if ( filing_subset ) {
		NotAvailableUnderSubset("Rename function not available");
		return;
	}

	if ( index(dest, '/') != 0 ) {		/* rename across directory */
		docopy("MOVE", src, dest);	/* use move */
		return;
	}

	getdirhandle(src, dirhandle);
	getfilehandle(cur_pathname, srchandle);

	attrseq.length= 1;
	attrseq.sequence= attrvals;

	attrvals[0].type= Filing4_name;
	StringToAttr(dest, &attrvals[0]);

	if (verbose) {
		printf("renaming %s to %s...\n", src, dest);
	}

	alarm(0);
	Filing4_ChangeAttributes(connected, NULL, srchandle, attrseq, session);
	alarm(continuetime);

	freefilehandle(srchandle);
	freefilehandle(dirhandle);
}

docopy(cmd, src, dest)
	char *cmd, *src, *dest;
{
	Filing4_Handle srchandle, srcdirhandle, dirhandle, newhandle;
	Filing4_AttributeSequence attrseq;
	Filing4_Attribute attrvals[1];
	Filing4_AttributeTypeSequence typeseq;
	Filing4_AttributeType attrs[2];
	Filing4_ScopeSequence scopeseq;
	Filing4_Scope scope;
	Filing4_CopyResults copyresults;
	Boolean copy= FALSE;

	if ( filing_subset ) {
		NotAvailableUnderSubset("Copy/Move function not available");
		return;
	}

	if ( strcmp(cmd, "COPY") == 0 )
		copy= TRUE;

	getdirhandle(src, srcdirhandle);
	getfilehandle(cur_pathname, srchandle);
	freefilehandle(srcdirhandle);

	getdirhandle(dest, dirhandle);

	typeseq.length= 2; typeseq.sequence= attrs;
	attrs[0]= Filing4_isDirectory;
	attrs[1]= Filing4_pathname;

	scopeseq.length= 1; scopeseq.sequence= &scope;
	scope.designator= Filing4_filter;
	scope.Filing4_filter_case.designator= Filing4_matches;
	scope.Filing4_filter_case.Filing4_matches_case.attribute.type= Filing4_name;
	StringToAttr(cur_name, &scope.Filing4_filter_case.Filing4_matches_case.attribute);
	ProcEachSeq= isdirproc;
	isdir= FALSE;

	alarm(0);	
	Filing4_List(connected, GetAttributeSequences, dirhandle, typeseq,
		     scopeseq, BulkData1_immediateSink, session);
	alarm(continuetime);

	if ( isdir ) {
		getfilehandle(cur_pathname, dirhandle);	/* open directory as file */

		attrseq.length= 0;
		attrseq.sequence= attrvals;
	} else {
		attrseq.length= 1;
		attrseq.sequence= attrvals;

		attrvals[0].type= Filing4_name;
		StringToAttr(cur_name, &attrvals[0]);
	}

	if (verbose) {
		if ( copy )
			printf("copying ");
		else 
			printf("moving ");
		printf("%s to %s%s%s...\n", src, dest, (isdir ? "/" : ""), 
						(isdir ? src : ""));
	}

	alarm(0);
	if ( copy ) {
		copyresults= Filing4_Copy(connected, NULL, srchandle, dirhandle,
					  attrseq, nullControls, session);

		freefilehandle(copyresults.newFile);
	} else {
		Filing4_Move(connected, NULL, srchandle, dirhandle,
			     attrseq, session);
	}
	alarm(continuetime);

	freefilehandle(srchandle);
	freefilehandle(dirhandle);
}

dounify(remote)
	char *remote;
{
	Filing4_Handle dirhandle, remotehandle;

	if ( filing_subset ) {
		NotAvailableUnderSubset("Unify AccessLists");
		return;
	}

	getdirhandle(remote, dirhandle);
	getfilehandle(cur_pathname, remotehandle);
	freefilehandle(dirhandle);

	if ( verbose ) {
		printf("unify access lists for %s\n", remote);
	}

	alarm(0);
	Filing4_UnifyAccessLists(connected, NULL, remotehandle, session);
	alarm(continuetime);

	freefilehandle(remotehandle);

}
recvrequest(cmd, local, remote, mode)
	char *cmd, *local, *remote, *mode;
{
	FILE *popen();
	int (*closefunc)(), pclose(), fclose();
	int do_unlink= FALSE;
	int pos, i;
	struct timeval start, stop;
	FilingSubset1_Handle remotehandle; /* note: an array */
	FilingSubset1_Handle dirhandle; /* note: an array */
	FilingSubset1_AttributeTypeSequence typeseq;
	FilingSubset1_AttributeType tsvals[10];
	char *dir;
	FilingSubset1_ScopeSequence scopeseq;
	FilingSubset1_Scope scope;

	closefunc = NULL;

	fout = stdout;
	typeseq.length = 0;  typeseq.sequence = tsvals;
	scopeseq.length= 1; scopeseq.sequence= &scope;
	scope.designator= FilingSubset1_filter;
	scope.FilingSubset1_filter_case.designator= FilingSubset1_matches;
	if ( filing_subset )
		scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute.type= FilingSubset1_pathname;
	else
		scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute.type= FilingSubset1_name;
	timbuf[0].tv_sec= 0;

	copyhandle(remotehandle, FilingSubset1_nullHandle);

	if (strcmp(local, "-") && *local != '|')
		if (access(local, 2) < 0) {
			dir = rindex(local, '/');
			/* get a good error message */
			if (dir != NULL) *dir = '\0';
			if (access(dir ? local : ".", 2) < 0) {
				perror(local);
				goto bad;
			}
			if (dir != NULL) *dir = '/';
		}
	if (strcmp(local, "-") == 0)
		fout = stdout;
	else if (*local == '|') {
		char *ptr;
		ptr= local+1;
		while (isspace(*ptr)) ptr++;
		fout = popen(ptr, "w");
		if (fout == NULL) {
			perror(ptr);
			goto bad;
		}
		closefunc = pclose;
	} else {
		fout = fopen(local, mode);
		if (fout == NULL) {
			perror(local);
			goto bad;
		}
		closefunc = fclose;
	}

	if (remote) {
		getdirhandle(remote, dirhandle);
	}
	bytessent= 0;
	filetypevalue= typevalue;

	if ( filing_subset )
		StringToAttr(cur_pathname+1,&scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute);
	else
		StringToAttr(cur_name,&scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute);

	if (strcmp(cmd,"NLST") == 0) {
		typeseq.length = 1;
		typeseq.sequence[0] = FilingSubset1_pathname;
		ProcEachSeq = nlistproc;
		alarm(0);
		gettimeofday(&start, (struct timezone *)0);
		if ( filing_subset )
			FilingSubset1_List(connected, GetAttributeSequences, dirhandle,
				     typeseq, scopeseq,
				     BulkData1_immediateSink, session);
		else
			Filing4_List(connected, GetAttributeSequences, dirhandle,
				     typeseq, scopeseq,
				     BulkData1_immediateSink, session);
		alarm(continuetime);
	}
	else if (strcmp(cmd,"LIST") == 0) {
		typeseq.length = 7;
		if ( filing_subset )
			typeseq.sequence[0] = FilingSubset1_pathname;
		else
			typeseq.sequence[0] = FilingSubset1_name;
		typeseq.sequence[1] = FilingSubset1_dataSize;
		typeseq.sequence[2] = FilingSubset1_isDirectory;
		typeseq.sequence[3] = FilingSubset1_isTemporary;
		typeseq.sequence[4] = FilingSubset1_type;
		typeseq.sequence[5] = FilingSubset1_createdOn;
		typeseq.sequence[6] = FilingSubset1_version;
		ProcEachSeq = listproc;
		alarm(0);
		gettimeofday(&start, (struct timezone *)0);
		if ( filing_subset )
			FilingSubset1_List(connected, GetAttributeSequences, dirhandle,
				     typeseq, scopeseq,
				     BulkData1_immediateSink, session);
		else
			Filing4_List(connected, GetAttributeSequences, dirhandle,
				     typeseq, scopeseq,
				     BulkData1_immediateSink, session);
		alarm(continuetime);
	}
	else if (strcmp(cmd,"RETR") == 0) {
		typeseq.length= 4;
		typeseq.sequence[0]= FilingSubset1_createdOn;
		typeseq.sequence[1]= FilingSubset1_pathname;
		typeseq.sequence[2]= FilingSubset1_type;
		typeseq.sequence[3]= FilingSubset1_isDirectory;

		is_a_directory= FALSE;
		ProcEachSeq= rlistproc;

		alarm(0);
		if ( filing_subset )
			FilingSubset1_List(connected, GetAttributeSequences, dirhandle,
				     typeseq, scopeseq,
				     BulkData1_immediateSink, session);
		else 
			Filing4_List(connected, GetAttributeSequences, dirhandle,
				     typeseq, scopeseq,
				     BulkData1_immediateSink, session);

		alarm(continuetime);

		if ( files_found ) {
			if ( is_a_directory && (filetypevalue == FilingSubset1_tDirectory) ) {
				if ( filing_subset ) {
					NotAvailableUnderSubset("Cannot retrieve directory files");
					do_unlink= TRUE;
					goto error;
				}
			}

			if (verbose) {
			 	printf("%s...(%s)...", local, typetostring(filetypevalue));
				fflush(stdout);
			}

			if ( (filetypevalue == FilingSubset1_tDirectory) ||
					((filetypevalue > LAST_FILING_TYPE) &&
					(filetypevalue != TYPE_Interpress) &&
					(filetypevalue != TYPE_VPCanvas))  ) {
				if ( filing_subset ) {
					NotAvailableUnderSubset("Cannot retrieve Viewpoint files");
					do_unlink= TRUE;
					goto error;
				}

				alarm(0);
				ProcEachSeq= GetAllAttributes;
				Filing4_List(connected, GetAttributeSequences, dirhandle,
						Filing4_allAttributeTypes, scopeseq,
						BulkData1_immediateSink, session);
				alarm(continuetime);
			}

			bytessent= 0;
			getfilehandle(cur_pathname, remotehandle);	/* get file handle */
			alarm(0);
			gettimeofday(&start, (struct timezone *)0);
			if ( filing_subset ) {
				FilingSubset1_Retrieve(connected, retrieveproc, remotehandle,
						 BulkData1_immediateSink, session);
			} else {
				if ( is_a_directory )
					Filing4_Serialize(connected, retrieveproc, remotehandle,
						BulkData1_immediateSink, session);
				else
					Filing4_Retrieve(connected, retrieveproc, remotehandle,
						 BulkData1_immediateSink, session);
			}
			alarm(continuetime);
		}
	} else if (strcmp(cmd,"SER") == 0) {
		ProcEachSeq= GetAllAttributes;

		alarm(0);
		if ( filing_subset ) {
			NotAvailableUnderSubset("Cannot serialize files");
			do_unlink= TRUE;
			goto error;
		} else {
			Filing4_List(connected, GetAttributeSequences, dirhandle,
					Filing4_allAttributeTypes, scopeseq,
					BulkData1_immediateSink, session);
		}
		alarm(continuetime);

		if ( files_found ) {
			if (verbose) {
			 	printf("%s to %s...(%s)...",cur_pathname, local, typetostring(filetypevalue));
				fflush(stdout);
			}

			bytessent= 0;
			getfilehandle(cur_pathname, remotehandle);	/* get file handle */
			alarm(0);
			gettimeofday(&start, (struct timezone *)0);
			Filing4_Serialize(connected, retrieveproc, remotehandle,
					BulkData1_immediateSink, session);
			alarm(continuetime);
		}
	}
	else printf("unrecognized command %s\n",cmd);
	gettimeofday(&stop, (struct timezone *)0);
	freefilehandle(remotehandle);

	if ( files_found ) {
		if (bytessent > 0 && verbose)
			ptransfer("received", bytessent, &start, &stop);
	} else {
		printf("%s not found\n",cur_pathname);
		do_unlink= TRUE;
	}

error:
	freefilehandle(dirhandle);

bad:
	if (closefunc != NULL && fout != NULL) {
		(*closefunc)(fout);
		if ( closefunc == fclose ) {
	    		if (timbuf[0].tv_sec != 0 )
				utimes(local,&timbuf[0]);
		}
	}
	if ( do_unlink )
		unlink(local);

	fout = NULL;
}


sendrequest(cmd, local, remote)
	char *cmd, *local, *remote;
{
	FILE *popen();
	int (*closefunc)(), pclose(), fclose();
	struct stat st;
	struct timeval start, stop;
	FilingSubset1_StoreResults storeresults;
	Filing4_DeserializeResults deserializeresults;
	Filing4_StoreResults storeresults2;
	FilingSubset1_Handle dirhandle;
	FilingSubset1_AttributeSequence attrseq;
	FilingSubset1_Attribute attrvals[50];
	Boolean GetDirectoryAttribute();
	struct timeval time;
	long createdate;
	long datasize;

	gettimeofday(&time,(struct timezone *) 0);
	createdate= time.tv_sec + XNS_TIME_DIFFERENCE;
	filetypevalue= typevalue;

	closefunc = NULL;
	if (strcmp(local, "-") == 0) {
		fin = stdin;
		closefunc = NULL;
	} else if (*local == '|') {
		char *ptr;
		ptr= local+1;
		while (isspace(*ptr)) ptr++;
		fin = popen(ptr, "r");
		if (fin == NULL) {
			perror(ptr);
			return;
		}
		closefunc = pclose;
	} else {
		if (typevalue == TYPE_Guess) {
		    	filetypevalue= get_type(local);	/* guess file type */
		}
		fin = fopen(local, "r");
		if (fin == NULL) {
			perror(local);
			return;
		}
		closefunc = fclose;
		if (fstat(fileno(fin), &st) < 0 ||
		    (st.st_mode&S_IFMT) != S_IFREG) {
			fprintf(stderr, "%s: not a plain file.", local);
			fclose(fin);
			fin= NULL;
			return;
		}
		createdate= st.st_mtime + XNS_TIME_DIFFERENCE;
		datasize= st.st_size;
	}

	if (filetypevalue == TYPE_Guess)	/* if input from file, TYPE_G should already be replaced */
		filetypevalue= TYPE_A;	/* assume ascii for pipes/stdin... */

	if (remote) {
		getdirhandle(remote, dirhandle);
	} else {
		printf("No remote name specified\n");
		return;
	}
	bytessent = 0;
	if (strcmp(cmd,"STOR") == 0) {
		if (verbose) {
		 	printf("%s to %s...",local,remote);
			fflush(stdout);
		}
		attrseq.length = 1;
		attrseq.sequence = attrvals;
		if ( filing_subset ) {
			attrvals[0].type = FilingSubset1_pathname;
			StringToAttr(cur_pathname, &attrvals[0]);
		} else {
			attrvals[0].type = FilingSubset1_name;
			StringToAttr(cur_name, &attrvals[0]);
		}

		if ( (filetypevalue == TYPE_Directory) ||
				((filetypevalue > LAST_FILING_TYPE) && 
				(filetypevalue != TYPE_Interpress) &&
				(filetypevalue != TYPE_VPCanvas)) ) {
			isdir= GetDirectoryAttribute(fin);
		} else {
			isdir= FALSE;
		}

		if ( !isdir ) {
			attrseq.length += 4;
			attrvals[1].type = FilingSubset1_type;
			LongCardinalToAttr(filetypevalue, &attrvals[1]);
			attrvals[2].type = FilingSubset1_createdOn;
			LongCardinalToAttr(createdate,&attrvals[2]);
			attrvals[3].type= FilingSubset1_isDirectory;
			BooleanToAttr(FALSE, &attrvals[3]);
			attrvals[4].type= FilingSubset1_dataSize;
			LongCardinalToAttr(datasize, &attrvals[4]);
		}

		if (verbose) {
			printf("(%s)...", typetostring(filetypevalue));
			fflush(stdout);
		}

		if ( (filetypevalue == TYPE_Directory) ||
				((filetypevalue > LAST_FILING_TYPE) && 
				(filetypevalue != TYPE_Interpress) &&
				(filetypevalue != TYPE_VPCanvas)) ) {
			if ( filing_subset ) {
				NotAvailableUnderSubset("Cannot store Viewpoint files");
				goto error;
			}
			if ( isdir ) {
				if ( AddExtendedDeserializeAttributes(fin, &attrseq) == 0 ) {
					goto error;
				}
			} else {
				if ( AddExtendedStoreAttributes(fin, &attrseq) == 0 ) {
					goto error;
				}
			}
		}

		alarm(0);
		gettimeofday(&start, (struct timezone *)0);
		if ( filing_subset ) {
			storeresults = FilingSubset1_Store(connected, storeproc,
						     dirhandle, attrseq,
						     nullControls,
						     BulkData1_immediateSource,
						     session);
		} else {
			if ( isdir )
				deserializeresults = Filing4_Deserialize(connected, storeproc,
						dirhandle, attrseq,
						nullControls,
						BulkData1_immediateSource,
						session);
			else
				storeresults2 = Filing4_Store(connected, storeproc,
						dirhandle, attrseq,
						nullControls,
						BulkData1_immediateSource,
						session);
		}
		alarm(continuetime);
		gettimeofday(&stop, (struct timezone *)0);
		if ( filing_subset ) {
			freefilehandle(storeresults.file);
		} else {
			if ( isdir )
				freefilehandle(deserializeresults.file);
			else
				freefilehandle(storeresults2.file);
		}
	} else if (strcmp(cmd,"DSER") == 0) {
		if ( filing_subset ) {
			NotAvailableUnderSubset("Cannot Deserialize files");
			goto error;
		}
		if (verbose) {
		 	printf("%s to %s...",local,remote);
			fflush(stdout);
		}
		attrseq.length = 1;
		attrseq.sequence = attrvals;
		attrvals[0].type = FilingSubset1_name;
		StringToAttr(cur_name, &attrvals[0]);

		if (verbose) {
			printf("(%s)...", typetostring(filetypevalue));
			fflush(stdout);
		}

		if ( AddExtendedDeserializeAttributes(fin, &attrseq) == 0 ) {
					goto error;
		}

		alarm(0);
		gettimeofday(&start, (struct timezone *)0);
		deserializeresults = Filing4_Deserialize(connected, storeproc,
				dirhandle, attrseq, nullControls,
				BulkData1_immediateSource, session);
		alarm(continuetime);
		gettimeofday(&stop, (struct timezone *)0);
		freefilehandle(deserializeresults.file);
	} 
	else {
		printf("unrecognized command %s\n",cmd);
		alarm(continuetime);
	}
	if (bytessent > 0 && verbose)
		ptransfer("sent", bytessent, &start, &stop);
error:
	freefilehandle(dirhandle);
	if (closefunc != NULL && fin != NULL)
		(*closefunc)(fin);
	fin = NULL;
}



docd(dest)
	char *dest;
{
	FilingSubset1_AttributeSequence attrseq;
	FilingSubset1_AttributeTypeSequence typeseq;
	Boolean current= FALSE;
	FilingSubset1_AttributeType cdattrs[2];
	FilingSubset1_ScopeSequence scopeseq;
	FilingSubset1_Scope scope;
	FilingSubset1_Handle remotehandle;

	if (dest == (char*)NULL || *dest == '\0' || (strcmp(dest, "/") == 0) ) {
		getdirhandle("/", remotehandle);
		strcpy(cur_dir, "/");
		dopwd();
		return;
	} else {
		getdirhandle(dest, remotehandle);
	}

	isdir= FALSE;

	if ( filing_subset ) {
		StringToAttr(cur_pathname+1,&scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute);
		scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute.type= FilingSubset1_pathname;
	} else {
		StringToAttr(cur_name,&scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute);
		scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute.type= FilingSubset1_name;
	}
	typeseq.length = 2; typeseq.sequence = cdattrs;
	cdattrs[0] = FilingSubset1_isDirectory;
	cdattrs[1] = FilingSubset1_pathname;
	scopeseq.length= 1; scopeseq.sequence= &scope;
	scope.designator= FilingSubset1_filter;
	scope.FilingSubset1_filter_case.designator= FilingSubset1_matches;
	ProcEachSeq= cdproc;
	alarm(0);
	if ( filing_subset )
		FilingSubset1_List(connected, GetAttributeSequences, remotehandle, typeseq,
			     scopeseq, BulkData1_immediateSink, session);
	else
		Filing4_List(connected, GetAttributeSequences, remotehandle, typeseq,
			     scopeseq, BulkData1_immediateSink, session);

	alarm(continuetime);
	freefilehandle(remotehandle);

	if ( files_found == FALSE ) {
		printf("%s not found\n", dest);
	} else if ( !isdir ) {
		printf("%s not a directory\n", dest);
	} else {
		if ( dest[0] != '/' ) {
			if ( strcmp(cur_dir, "/") != 0 )
				strcat(cur_dir, "/");
			strcat(cur_dir, dest);
		} else {
		    	strcpy(cur_dir, dest);
		}
		if (verbose) dopwd();
	}

}

dopwd()
{
	printf("Remote working directory:  %s\n",cur_dir);
}
	
dodelete(src)
	char *src;
{
	int i;
	FilingSubset1_Handle remotehandle;
	FilingSubset1_Handle dirhandle;
	FilingSubset1_AttributeSequence attrseq;
	FilingSubset1_AttributeTypeSequence typeseq;
	FilingSubset1_AttributeType delattrs[2];
	FilingSubset1_ScopeSequence scopeseq;
	FilingSubset1_Scope scope;

	typeseq.length = 2; typeseq.sequence= delattrs;
	delattrs[0] = FilingSubset1_type;
	delattrs[1]= FilingSubset1_pathname;

	scopeseq.length= 1; scopeseq.sequence= &scope;
	scope.designator= FilingSubset1_filter;
	scope.FilingSubset1_filter_case.designator= FilingSubset1_matches;

	name_count= 0;
	name_size= MAXNAMES;
	if ( (name_list= (struct name_entry *)malloc(sizeof(struct name_entry) * name_size)) == 0 ) {
		perror("dodelete");
		return;
	}

	getdirhandle(src, dirhandle);

	if ( filing_subset ) {
		scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute.type= FilingSubset1_pathname;
		StringToAttr(cur_pathname+1,&scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute);
	} else {
		scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute.type= FilingSubset1_name;
		StringToAttr(cur_name,&scope.FilingSubset1_filter_case.FilingSubset1_matches_case.attribute);
	}

	ProcEachSeq= deleteproc;
	alarm(0);
	if ( filing_subset )
		FilingSubset1_List(connected, GetAttributeSequences, dirhandle, typeseq,
			     scopeseq, BulkData1_immediateSink, session);
	else
		Filing4_List(connected, GetAttributeSequences, dirhandle, typeseq,
			     scopeseq, BulkData1_immediateSink, session);

	alarm(continuetime);
	freefilehandle(dirhandle);

	for ( i= 0 ; i < name_count ; i++ ) {
		struct name_entry *entry;

		entry= &name_list[i];
		if ( verbose ) {
			if ( entry->type == TYPE_Directory ) {
				if (!confirm("Delete directory", entry->pathname) ) {
					clear_String(&entry->pathname);
					continue;
				}
			} else if ( entry->type == TYPE_VPDrawer ) {
				if (!confirm("Delete file drawer", entry->pathname) ) {
					clear_String(&entry->pathname);
					continue;
				}
			} else {
				if (!confirm("Delete file",entry->pathname) ) {
					clear_String(&entry->pathname);
					continue;
				}
			}
		}

		getfilehandle(entry->pathname,remotehandle);
		alarm(0);
		if ( filing_subset )
			FilingSubset1_Delete(connected, NULL, remotehandle, session);
		else
			Filing4_Delete(connected, NULL, remotehandle, session);
		alarm(continuetime);

		clear_String(&entry->pathname);
	}
}

NYI()
{
	printf("Not yet implemented\n");
}

NotAvailableUnderSubset(message)
char *message;
{
	printf("%s under Subset,\n  Reopen connection with -F switch and retry\n", message);
}

ptransfer(direction, bytes, t0, t1)
	char *direction;
	long bytes;
	struct timeval *t0, *t1;
{
	struct timeval td;
	long ms;
	float bs;

	tvsub(&td, t1, t0);
	ms = (td.tv_sec * 1000) + (td.tv_usec / 1000);
#define	nz(x)	((x) == 0 ? 1 : (x))
	bs = ((1000. * (float) bytes) / (float) nz(ms));
	printf("\n%ld bytes %s in %d.%02d seconds (%.2g Kbytes/s)\n",
		bytes, direction, td.tv_sec, td.tv_usec / 10000, bs / 1024.);
}

tvadd(tsum, t0)
	struct timeval *tsum, *t0;
{

	tsum->tv_sec += t0->tv_sec;
	tsum->tv_usec += t0->tv_usec;
	if (tsum->tv_usec > 1000000)
		tsum->tv_sec++, tsum->tv_usec -= 1000000;
}

tvsub(tdiff, t1, t0)
	struct timeval *tdiff, *t1, *t0;
{

	tdiff->tv_sec = t1->tv_sec - t0->tv_sec;
	tdiff->tv_usec = t1->tv_usec - t0->tv_usec;
	if (tdiff->tv_usec < 0)
		tdiff->tv_sec--, tdiff->tv_usec += 1000000;
}

nlistproc(attr)
	FilingSubset1_AttributeSequence attr;
{
	int i;
	char *thisname;
	FilingSubset1_AttributeType t;
	
	files_found= TRUE;

	for (i = 0; i < attr.length; i++) {
		t = attr.sequence[i].type;
		if (t == FilingSubset1_pathname) {
			thisname = AttrToString(&attr.sequence[i]);
#ifdef XEROXFSCOMPATIBILITY
			/*
			 * Xerox File servers don't include beginning /
			 */
			if ( *thisname != '/' )
				fputc('/', fout);
#endif XEROXFSCOMPATIIBLITY
			fputs(thisname, fout);
			fputc('\n', fout);
			clear_String(&thisname);
			return;
		}
	}
}


listproc(attr)
	FilingSubset1_AttributeSequence attr;
{
	int i;
	char *thisname;
	char *slash;
	Boolean istemp = 0;
	Boolean isdir = 0;
	LongCardinal thistype = 0;
	LongCardinal thissize = 0;
	LongCardinal thisdate = 0;
	FilingSubset1_AttributeType t;
	char filetypestr[25];
	char filetypebuf[20];
	Cardinal thisversion = 0;
	char *filedatestr;
	char *ctime();
	char *rindex();

	files_found= TRUE;

	for (i = 0; i < attr.length; i++) {
		t = attr.sequence[i].type;
		if (t == FilingSubset1_name || t == FilingSubset1_pathname)
			thisname = AttrToString(&attr.sequence[i]);
		else if (t == FilingSubset1_isDirectory)
			isdir = AttrToBoolean(&attr.sequence[i]);
		else if (t == FilingSubset1_isTemporary)
			istemp = AttrToBoolean(&attr.sequence[i]);
		else if (t == FilingSubset1_type)
			thistype = AttrToLongCardinal(&attr.sequence[i]);
		else if (t == FilingSubset1_dataSize)
			thissize = AttrToLongCardinal(&attr.sequence[i]);
		else if (t == FilingSubset1_version)
			thisversion = AttrToCardinal(&attr.sequence[i]);
		else if (t == FilingSubset1_createdOn) {
			thisdate = AttrToLongCardinal(&attr.sequence[i]);
			thisdate = thisdate - XNS_TIME_DIFFERENCE;
			filedatestr= ctime(&thisdate);
			filedatestr[24]= '\0';
			filedatestr += 4;
		}
	}

	strcpy(filetypestr, "(");
	strcat(filetypestr, typetostring(thistype));
	strcat(filetypestr, ")");

	if ( (slash= rindex(thisname, '/')) == NULL )
		slash= thisname;
	else
		slash++;

	fprintf(fout, "%c%c%-16s%7ld %s %s",
		isdir?'D':' ', istemp?'T':' ',
		filetypestr, thissize, filedatestr, slash);
	if ( thisversion != 0)
		fprintf(fout,"!%d",thisversion);
	fprintf(fout,"\n");
	clear_String(&thisname);
}

/*
 *	process used by retrieve to get file type, createdOn and pathname
 */
rlistproc(attr)
	FilingSubset1_AttributeSequence attr;
{
	int i;
	char *thisname;
	FilingSubset1_AttributeType t;
	char *AttrToString();

	files_found= TRUE;

/*
 *	Xerox file servers will return all versions of the requested file in
 *	ascending version order. We assume that the last version will be the
 *	highest and remember that name so that the retrieve will pull the
 *	highest version of the file. If we request just the file with no
 *	version, the server will return the oldest version (not what I would
 *	expect...)
 */

	for (i= 0; i < attr.length; i++) {
		t= attr.sequence[i].type;
		if (t == FilingSubset1_createdOn) {
		    	gettimeofday(&timbuf[0],(struct timezone *)0);
			timbuf[1].tv_sec= AttrToLongCardinal(&attr.sequence[i]) - XNS_TIME_DIFFERENCE;
			timbuf[1].tv_usec= 0;
		} else if (t == FilingSubset1_type) {
		    	if (typevalue == TYPE_Guess) {
				filetypevalue= AttrToLongCardinal(&attr.sequence[i]);
			} 
		} else if (t == FilingSubset1_pathname) {
			thisname= AttrToString(&attr.sequence[i]);
			if (verbose) {
				printf("%s to ", thisname);
				fflush(stdout);
			}
			clear_String(&thisname);
		} else if (t == FilingSubset1_isDirectory) {
			is_a_directory= AttrToBoolean(&attr.sequence[i]);
		}
	}

}

cdproc(attr)
	FilingSubset1_AttributeSequence attr;
{
    	char *AttrtoString();
	char *dest;
	int i;

	files_found= TRUE;

	dest= 0;
	for (i= 0; i < attr.length; i++) {
	    	if (attr.sequence[i].type == FilingSubset1_isDirectory
		    && AttrToBoolean(&attr.sequence[i])) {
				isdir= TRUE;			/* if directory, change handles */
		}
		if (attr.sequence[i].type == FilingSubset1_pathname)
			dest= AttrToString(&attr.sequence[i]);
	}

	if (!isdir || dest == 0) {	/* if no directory or pathname */
		isdir= FALSE;		/* assume failure */
	}
}

#define MAXPACKS 20
static
GetAttributeSequences(conn)
	CourierConnection *conn;
{
	int count, i;
	Unspecified buffer[MAXWORDS*MAXPACKS], *bp, *bufend;
	FilingSubset1_StreamOfAttributeSequence attrs;
	Boolean overflow= FALSE;
	
	files_found= FALSE;

	bufend = buffer;
	bp = buffer+((MAXWORDS-1)*MAXPACKS);    /* end of available space */
	while ((count = BDTread(conn, (char*)bufend, 
				MAXWORDS*sizeof(Unspecified))) > 0) {
		bufend += count/sizeof(Unspecified);
		bytessent += count;
		if (bufend > bp) {
			fprintf(stderr,"BDT read too big to fit\n");
			BDTabort(conn);
			/* should clear out stuff here if we knew how much
			 * fall back to previous block on the assumption
			 * we can give a truncated list
			 */
			bufend -= count/sizeof(Unspecified);
			overflow=TRUE;
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
	if ( overflow ) {
		fprintf(stderr, "\nListing was truncated due to internal bulk data buffer size\n");
		overflow= FALSE;
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
	int count, ocount, ch, hashbytes;
	char buffer[SPPMAXDATA];
	int charset, charset16;
	char *bp;

	switch (filetypevalue) {
	default :
		errno = ocount = 0;
		fflush(fout);
		while ((count = BDTread(conn, buffer, sizeof(buffer))) > 0) {
			if ((ocount = write(fileno(fout),buffer,count)) < 0) {
				perror("write");
				BDTabort(conn);
				break;
			}
			bytessent += count;
			if (hash) {
				putchar('#');
				fflush(stdout);
			}
		}
		if (count < 0) perror("netin");
		break;

	case TYPE_VPMailNote :
	case TYPE_A :
		charset = 0; charset16 = 0; bp = NULL;
		hashbytes = 0;
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
					bytessent++;
				    	if ( (nextch = getBDTch(conn,&bp)) != '\n'){
					    if (nextch == '\r')
						putc('\n',fout);
					    else if ( nextch == ','+0200 ) 
						putc('_',fout);
					    else if ( nextch != EOF )
				    		putc(nextch,fout);
					    else
						continue;
					}
				    	bytessent= bytessent++;

					while (hash && bytessent >= hashbytes){
						putchar('#');
						fflush(stdout);
						hashbytes += sizeof(buffer);
					}
					break;
				}
				else if (ch == ','+0200) ch = '_';
				/* more mapping here */
				putc(ch,fout);
				bytessent++;
				break;
			default:
				break; /* ignore */
			}
		}
		if (hash) {
			while (bytessent >= hashbytes) {
				putchar('#');
				hashbytes += sizeof(buffer);
			}
			putchar('\n');
			fflush(stdout);
		}
		/* if (count < 0) perror("netin"); */
		break;
	}
}

storeproc(conn)
	CourierConnection *conn;
{
	int count, ocount;
	u_char buffer[SPPMAXDATA];
	u_char *bp;

	errno = ocount = 0;
	clearerr(fin);
	switch (filetypevalue) {

	default :
		while ((count = fread(buffer, sizeof(char), SPPMAXDATA, fin)) > 0
		       && (ocount = BDTwrite(conn, buffer, count)) > 0) {
			bytessent += count;
			if (hash) {
				putchar('#');
				fflush(stdout);
			}
		}
		break;
	case TYPE_VPMailNote :
	case TYPE_A :
		while ((count = fread(buffer, sizeof(char), SPPMAXDATA, fin))
		       > 0) {
			ocount = count;
			for (bp = buffer; count > 0; count--, bp++) {
				if (*bp == '\n') *bp = '\r';
				else if (*bp == '_') *bp = ','+0200;
				/* more translations here */
			}
			if ((ocount = BDTwrite(conn, buffer, ocount)) <= 0)
				break;
			bytessent += ocount;
			if (hash) {
				putchar('#');
				fflush(stdout);
			}
		}
		break;
	}
	if (ocount < 0) {
		BDTabort(conn);
		perror("netout");
	}
	else if (ferror(fin)) {
		BDTabort(conn);
		perror("fread");
	}
	else
		BDTclosewrite(conn);
}

isdirproc(attr)
	FilingSubset1_AttributeSequence attr;
{
	int i;
	char *name;
	FilingSubset1_AttributeType t;

	for ( i= 0; i < attr.length; i++ ) {
		t= attr.sequence[i].type;
		if ( t == FilingSubset1_isDirectory ) {
			isdir= AttrToBoolean(&attr.sequence[i]);
		} else if ( t == FilingSubset1_pathname ) {
			name= AttrToString(&attr.sequence[1]);
			strcpy(cur_pathname, name);
			clear_String(&name);
		}
	}
}

deleteproc(attr)
	FilingSubset1_AttributeSequence attr;
{
	int i;
	char *name;
	struct name_entry *entry;
	FilingSubset1_AttributeType t;

	if ( name_count > name_size ) {
		name_size += MAXNAMES;
		name_list= (struct name_entry *) realloc(name_list, 
				sizeof(struct name_entry) * name_size);
	}

	entry= &name_list[name_count];
	for ( i= 0; i < attr.length; i++ ) {
		t= attr.sequence[i].type;
		if ( t == FilingSubset1_type ) {
			entry->type= AttrToLongCardinal(&attr.sequence[i]);
		} else if ( t == FilingSubset1_pathname ) {
			entry->pathname= AttrToString(&attr.sequence[1]);
		}
	}
	name_count++;
}

GetAllAttributes(attr)
	FilingSubset1_AttributeSequence attr;
{
	int i;
	char *thisname;
	FilingSubset1_AttributeType t;
	int got_createdon, got_type, got_pathname;

	files_found= TRUE;
	got_createdon= got_pathname= got_type= 0;

/*
 *	Xerox file servers will return all versions of the requested file in
 *	ascending version order. We assume that the last version will be the
 *	highest and remember that name so that the retrieve will pull the
 *	highest version of the file. If we request just the file with no
 *	version, the server will return the oldest version (not what I would
 *	expect...)
 */
	for (i= 0; i < attr.length; i++) {
	    	t= attr.sequence[i].type;
		if (t == FilingSubset1_createdOn) {
		    	gettimeofday(&timbuf[0],(struct timezone *)0);
			timbuf[1].tv_sec= AttrToLongCardinal(&attr.sequence[i]) - XNS_TIME_DIFFERENCE;
			timbuf[1].tv_usec= 0;
			got_createdon++;
		} else if (t == FilingSubset1_type) {
		    	if (typevalue == TYPE_Guess) {
				filetypevalue= AttrToLongCardinal(&attr.sequence[i]);
			} 
			got_type++;
		} else if (t == FilingSubset1_pathname) {
			thisname= AttrToString(&attr.sequence[i]);
			strcpy(cur_pathname, thisname);
			clear_String(&thisname);
			got_pathname++;
		} 

		if ( got_createdon && got_type && got_pathname )
			break;
	}

	SaveExtendedAttributes(fout, attr);

	return;
}
