#ifndef lint
static char *rcsid = "$Header: xnftp.c,v 2.5 87/03/08 07:09:53 jqj Exp $";
#endif lint

/* $Log:	xnftp.c,v $
 * Revision 2.5  87/03/08  07:09:53  jqj
 * work around "schain botch" 4.3BSD VAX compiler bug (from Scooter Morris).
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
#include <sys/time.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <netns/ns.h>
#include <netns/sp.h>
#include "ftp_var.h"
#include <xnscourier/CH.h>

CourierConnection *connected;
Clearinghouse2_ObjectName hostobjname;
Authentication2_Verifier verifier;

/* the following 3 items make up the current session */
Filing4_Session session;	/* the current session */
Clearinghouse2_ObjectName username;
int continuetime;
int remoteprocpending;
Filing4_Handle wdHandle;	/* the current remote working dir */

static Filing4_ControlSequence nullControls = {0,0};
static Filing4_ScopeSequence nullScope = {0,0};

/* global data used to communicate with BDT procedures
 */
extern GetAttributeSequences(), 
	listproc(), nlistproc(),
	storeproc(), retrieveproc();
static (*ProcEachSeq)();
static long bytessent;
static FILE *fout, *fin;

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

StringToAttr(str, attr)
	char *str;
	Filing4_Attribute *attr;
{
	Unspecified buf[2049], *bp;
	Cardinal len;

	bp = buf + sizeof_Cardinal(len);
	len = externalize_String(&str, bp);
	(void) externalize_Cardinal(&len, buf);
	internalize_Clearinghouse2_Item(&(attr->value), buf);
	return;
}

char *
AttrToString(attr)
	Filing4_Attribute *attr;
{
	Unspecified buf[2049], *bp;
	Cardinal len;
	char *strval;

	externalize_Clearinghouse2_Item(&(attr->value), buf);
	bp = buf;
	bp += internalize_Cardinal(&len, bp);
	bp += internalize_String(&strval, bp);	
	return(strval);
}

UserToAttr(id, attr)
	Clearinghouse2_Name id;
	Filing4_Attribute *attr;
{
	Unspecified buf[2049], *bp;
	Cardinal len;

	bp = buf + sizeof_Cardinal(len);
	len = externalize_Clearinghouse2_Name(&id, bp);
	(void) externalize_Cardinal(&len, buf);
	internalize_Clearinghouse2_Item(&(attr->value), buf);
	return;
}

LongCardinalToAttr(val, attr)
	LongCardinal val;
	Filing4_Attribute *attr;
{
	Unspecified buf[3], *bp;
	Cardinal len;

	bp = buf + sizeof_Cardinal(len);
	len = externalize_LongCardinal(&val, bp);
	(void) externalize_Cardinal(&len, buf);
	internalize_Clearinghouse2_Item(&(attr->value), buf);
	return;
}

BooleanToAttr(val, attr)
	int val;
	Filing4_Attribute *attr;
{
	Boolean boolval;
	Unspecified buf[3], *bp;
	Cardinal len;

	boolval = (Boolean) val;
	bp = buf + sizeof_Cardinal(len);
	len = externalize_Boolean(&boolval, bp);
	(void) externalize_Cardinal(&len, buf);
	internalize_Clearinghouse2_Item(&(attr->value), buf);
	return;
}

int
AttrToBoolean(attr)
	Filing4_Attribute *attr;
{
	Unspecified buf[1];
	Boolean result;

	(void) externalize_Unspecified(attr->value.sequence, buf);
	(void) internalize_Boolean(&result, buf);
	return(result);
}

LongCardinal
AttrToLongCardinal(attr)
	Filing4_Attribute *attr;
{
	Unspecified buf[2];
	LongCardinal result;

	(void) externalize_Unspecified(attr->value.sequence, buf);
	(void) externalize_Unspecified((attr->value.sequence)+1, buf+1);
	(void) internalize_LongCardinal(&result, buf);
	return(result);
}


getfilehandle(filename, handle)
	char *filename;
	Filing4_Handle handle;
{
	Filing4_Attribute pathattr[1];
	Filing4_AttributeSequence attrseq;
	Filing4_OpenResults openresult;

	if (filename == (char *)0 || *filename == '\000') {
		copyhandle(handle,wdHandle);
		return;
	}
	attrseq.length = 1;
	attrseq.sequence = pathattr;
	pathattr[0].type = Filing4_pathname;
	copyhandle(handle, Filing4_nullHandle);
	if (*filename != '/') {	/* relative pathname specified */
		StringToAttr(filename, &pathattr[0]);
		copyhandle(handle, wdHandle);
	} else if (filename[1] == '\000') {
		/* root specified */
		attrseq.length = 0;
	} else {	/* absolute pathname specified */
		StringToAttr(filename+1, &pathattr[0]);
	}
	alarm(0);
	openresult = Filing4_Open(connected, NULL, attrseq,
				  handle, nullControls,
				  session);
	alarm(continuetime);
	copyhandle(handle, openresult.file);
}

freefilehandle(handle)
	Filing4_Handle handle;
{
	if (handle[0] == Filing4_nullHandle[0] &&
	    handle[1] == Filing4_nullHandle[1])
		return;		/* don't free nullHandle */
	if (handle[0] == wdHandle[0] &&
	    handle[1] == wdHandle[1])
		return;		/* don't free working directory */
	alarm(0);
	Filing4_Close(connected, NULL, handle, session);
	alarm(continuetime);
}

/*
 * do a continue to make sure that the session doesn't time out.
 * Note that this is usually called by an ALARM interrupt
 */
probe()
{
	Filing4_ContinueResults cresult;

	alarm(0);		/* cancel previous alarms */
	cresult = Filing4_Continue(connected, NULL, session);
	continuetime = cresult.continuance / 5; /* seconds */
	alarm(continuetime);	/* reset for another 2 min. or so */
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
		hostaddr->x_port = htons(IDPPORT_COURIER); /* ?? */
		cconn = CourierOpen(hostaddr);
		/* reset objname to flush wildcards */
		/* clear_Clearinghouse2_ThreePartName(&hostobjname); */
		hostobjname = CH_StringToName(hnamebuf, &defaultobjname);
		hostname = hnamebuf;
		if (cconn == (CourierConnection*) 0)
			printf("%s: connection failed\n", hnamebuf);
		else if (verbose)
			printf("Connected to %s\n", hnamebuf);
	} else {			
		printf("%s: unknown host\n", name);
		cconn = (CourierConnection*)0;
	}
	return(cconn);
}


login(name,pwd)
	char *pwd;
	char *name;
{
	Authentication2_Credentials credentials;
	Filing4_LogonResults logonresult;

	username = CH_StringToName(name,&hostobjname);
	MakeSimpleCredsAndVerifier(&username,pwd,
			&credentials, &verifier);
	logonresult = Filing4_Logon(connected, NULL, hostobjname,
				    credentials, verifier);
	session = logonresult.session;
	copyhandle(wdHandle, Filing4_nullHandle);
	if (verbose)
	  printf("User %s:%s:%s logged on\n", username.object,
		 username.domain, username.organization);
	alarm(0);
	signal(SIGALRM, probe);
	probe();
}

logout()
{
	signal(SIGALRM, SIG_IGN);
	Filing4_Logoff(connected, NULL, session);
	clear_Filing4_Session(&session);
	copyhandle(wdHandle, Filing4_nullHandle);
}

domakedir(dest)
	char *dest;
{
	sendrequest("MKDIR", "-", dest);
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
	NYI();
}

recvrequest(cmd, local, remote, mode)
	char *cmd, *local, *remote, *mode;
{
	FILE *popen();
	int (*closefunc)(), pclose(), fclose();
	struct timeval start, stop;
	Filing4_Handle remotehandle; /* note: an array */
	Filing4_AttributeTypeSequence typeseq;
	Filing4_AttributeType tsvals[10];
	char *dir;

	closefunc = NULL;
	fout = stdout;
	typeseq.length = 0;  typeseq.sequence = tsvals;
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
		fout = popen(local + 1, "w");
		if (fout == NULL) {
			perror(local + 1);
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
	bytessent = 0;
	gettimeofday(&start, (struct timezone *)0);
	getfilehandle(remote, remotehandle);
	alarm(0);
	if (strcmp(cmd,"NLST") == 0) {
		typeseq.length = 1;
		typeseq.sequence[0] = Filing4_pathname;
		ProcEachSeq = nlistproc;
		Filing4_List(connected, GetAttributeSequences, remotehandle,
			     typeseq, nullScope,
			     BulkData1_immediateSink, session);
	}
	else if (strcmp(cmd,"LIST") == 0) {
		typeseq.length = 5;
		typeseq.sequence[0] = Filing4_name;
		typeseq.sequence[1] = Filing4_dataSize;
		typeseq.sequence[2] = Filing4_isDirectory;
		typeseq.sequence[3] = Filing4_isTemporary;
		typeseq.sequence[4] = Filing4_type;
		ProcEachSeq = listproc;
		Filing4_List(connected, GetAttributeSequences, remotehandle,
			     typeseq, nullScope,
			     BulkData1_immediateSink, session);
	}
	else if (strcmp(cmd,"RETR") == 0) {
		Filing4_Retrieve(connected, retrieveproc, remotehandle,
				 BulkData1_immediateSink, session);
	}
	else printf("unrecognized command %s\n",cmd);
	alarm(continuetime);
	gettimeofday(&stop, (struct timezone *)0);
	freefilehandle(remotehandle);
	if (bytessent > 0 && verbose)
		ptransfer("received", bytessent, &start, &stop);
 bad:
	if (closefunc != NULL && fout != NULL)
		(*closefunc)(fout);
	fout = NULL;
}


sendrequest(cmd, local, remote)
	char *cmd, *local, *remote;
{
	FILE *popen();
	int (*closefunc)(), pclose(), fclose();
	struct stat st;
	struct timeval start, stop;
	Filing4_StoreResults storeresults;
	Filing4_CreateResults createresults;
	Filing4_Handle remotehandle;
	Filing4_AttributeSequence attrseq;
	Filing4_Attribute attrvals[5];

	closefunc = NULL;
	if (strcmp(local, "-") == 0) {
		fin = stdin;
		closefunc = NULL;
	} else if (*local == '|') {
		fin = popen(local + 1, "r");
		if (fin == NULL) {
			perror(local + 1);
			goto bad;
		}
		closefunc = pclose;
	} else {
		fin = fopen(local, "r");
		if (fin == NULL) {
			perror(local);
			goto bad;
		}
		closefunc = fclose;
		if (fstat(fileno(fin), &st) < 0 ||
		    (st.st_mode&S_IFMT) != S_IFREG) {
			fprintf(stderr, "%s: not a plain file.", local);
			goto bad;
		}
	}
	if (remote) {
		char *dir = rindex(remote,'/');
		if (dir != NULL) {
			*dir = '\000';
			getfilehandle(remote, remotehandle);
			*dir = '/';
			remote = dir+1;
		} else {
			getfilehandle("", remotehandle);
		}
	} else {
		printf("No remote name specified\n");
		return;
	}
	bytessent = 0;
	gettimeofday(&start, (struct timezone *)0);
	alarm(0);
	if (strcmp(cmd,"STOR") == 0) {
		attrseq.length = 2;
		attrseq.sequence = attrvals;
		attrvals[0].type = Filing4_name;
		StringToAttr(remote, &attrvals[0]);
		attrvals[1].type = Filing4_type;
		LongCardinalToAttr(typevalue, &attrvals[1]);
		storeresults = Filing4_Store(connected, storeproc,
					     remotehandle, attrseq,
					     nullControls,
					     BulkData1_immediateSource,
					     session);
		alarm(continuetime);
		freefilehandle(storeresults.file);
	}
	else if (strcmp(cmd,"MKDIR") == 0) {
		attrseq.length = 3;
		attrseq.sequence = attrvals;
		attrvals[0].type = Filing4_name;
		StringToAttr(remote, &attrvals[0]);
		attrvals[1].type = Filing4_isDirectory;
		BooleanToAttr(1, &attrvals[1]);
		attrvals[2].type = Filing4_type;
		LongCardinalToAttr(Filing4_tDirectory, &attrvals[2]);
		createresults = Filing4_Create(connected, NULL,
					       remotehandle, attrseq,
					       nullControls, session);
		alarm(continuetime);
		freefilehandle(createresults.file);
	}
	else {
		printf("unrecognized command %s\n",cmd);
		alarm(continuetime);
	}
	gettimeofday(&stop, (struct timezone *)0);
	freefilehandle(remotehandle);
	if (bytessent > 0 && verbose)
		ptransfer("sent", bytessent, &start, &stop);
bad:
	if (closefunc != NULL && fin != NULL)
		(*closefunc)(fin);
	fin = NULL;
}



docd(dest)
	char *dest;
{
	Filing4_AttributeSequence attrseq;
	Filing4_AttributeTypeSequence typeseq;
	Filing4_AttributeType cdattrs[1];
	Filing4_GetAttributesResults garesult;
	Filing4_Handle remotehandle, temphandle;
	char trydest[100];
	int i;

	if (dest == (char*)NULL || *dest == '\0') {
		trydest[0] = '/'; /* assume absolute pathname */
		strcpy(trydest+1,username.object);
		getfilehandle(trydest, remotehandle);
	}
	else
		getfilehandle(dest, remotehandle);
	typeseq.length = 1; typeseq.sequence = cdattrs;
	cdattrs[0] = Filing4_isDirectory;
	alarm(0);
	garesult = Filing4_GetAttributes(connected, NULL, remotehandle,
					 typeseq, session);
	alarm(continuetime);
	for (i = 0; i < garesult.attributes.length; i++) {
		if (garesult.attributes.sequence[i].type == Filing4_isDirectory
		    && AttrToBoolean(&(garesult.attributes.sequence[i]))) {
			copyhandle(temphandle, wdHandle);
			copyhandle(wdHandle, remotehandle); /* change dir */
			if (verbose) dopwd();
			freefilehandle(temphandle); /* free old wdHandle */
			return;
		}
	}
	printf("%s is not a directory\n", dest);
	freefilehandle(remotehandle);
}

dopwd()
{
	Filing4_AttributeSequence attrseq;
	Filing4_AttributeTypeSequence typeseq;
	Filing4_AttributeType pwdattrs[1];
	Filing4_GetAttributesResults garesult;

	if (wdHandle[0] == 0 && wdHandle[1] == 0) {
		printf("Remote working directory:  /\n");
		return;
	}
	typeseq.length = 1; typeseq.sequence = pwdattrs;
	pwdattrs[0] = Filing4_pathname;
	alarm(0);
	garesult = Filing4_GetAttributes(connected, NULL, wdHandle, typeseq,
					 session);
	alarm(continuetime);
	if (garesult.attributes.length > 0 &&
	    garesult.attributes.sequence[0].type == Filing4_pathname)
		printf("Remote working directory:  /%s\n",
		       AttrToString(&(garesult.attributes.sequence[0])));
	else printf("Remote working directory not set\n");
	clear_Filing4_GetAttributesResults(&garesult);
}
	
dodelete(src)
	char *src;
{
	Filing4_Handle remotehandle;
	Filing4_AttributeSequence attrseq;
	Filing4_AttributeTypeSequence typeseq;
	Filing4_AttributeType delattrs[1];
	Filing4_GetAttributesResults garesult;

	typeseq.length = 1; typeseq.sequence = delattrs;
	delattrs[0] = Filing4_isDirectory;
	getfilehandle(src, remotehandle);
	garesult = Filing4_GetAttributes(connected, NULL, remotehandle,
					 typeseq, session);
	if (garesult.attributes.length > 0 &&
	    garesult.attributes.sequence[0].type == Filing4_isDirectory &&
	    AttrToBoolean(&(garesult.attributes.sequence[0]))) {
		    if (!confirm("Delete directory", src)) return;
	}

	clear_Filing4_GetAttributesResults(&garesult);
	alarm(0);
	Filing4_Delete(connected, NULL, remotehandle, session);
	alarm(continuetime);
}

NYI()
{
	printf("Not yet implemented\n");
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
	Filing4_AttributeSequence attr;
{
	int i;
	char *thisname;
	Filing4_AttributeType t;
	
	for (i = 0; i < attr.length; i++) {
		t = attr.sequence[i].type;
		if (t == Filing4_pathname) fputc('/', fout);
		if (t == Filing4_name ||
		    t == Filing4_pathname) {
			thisname = AttrToString(&attr.sequence[i]);
			fputs(thisname, fout);
			fputc('\n', fout);
			clear_String(&thisname);
			return;
		}
	}
}


listproc(attr)
	Filing4_AttributeSequence attr;
{
	int i;
	char *thisname = "";
	Boolean istemp = 0;
	Boolean isdir = 0;
	LongCardinal thistype = 0;
	LongCardinal thissize = 0;
	Filing4_AttributeType t;
	char *filetypestr;
	char filetypebuf[20];
	
	for (i = 0; i < attr.length; i++) {
		t = attr.sequence[i].type;
		if (t == Filing4_name ||
		    t == Filing4_pathname)
			thisname = AttrToString(&attr.sequence[i]);
		else if (t == Filing4_isDirectory)
			isdir = AttrToBoolean(&attr.sequence[i]);
		else if (t == Filing4_isTemporary)
			istemp = AttrToBoolean(&attr.sequence[i]);
		else if (t == Filing4_type)
			thistype = AttrToLongCardinal(&attr.sequence[i]);
		else if (t == Filing4_dataSize)
			thissize = AttrToLongCardinal(&attr.sequence[i]);
	}
	
	if (thistype == Filing4_tUnspecified)
		filetypestr = "";
	else if (thistype == Filing4_tDirectory)
		filetypestr = "(dir.)";
	else if (thistype == Filing4_tText)
		filetypestr = "(text)";
	else if (thistype == Filing4_tSerialized)
		filetypestr = "(serial)";
	else if (thistype == TYPE_VP)
		filetypestr = "(VP doc)";
	else if (thistype == TYPE_Interpress)
		filetypestr = "(IPress)";
	else if (thistype == TYPE_VPCanvas)
		filetypestr = "(VP can)";
	else if (thistype == TYPE_VPDictionary)
		filetypestr = "(VP dic)";
	else if (thistype == TYPE_VPMailNote)
		filetypestr = "(VPnote)";
	else if (thistype == TYPE_VPReference)
		filetypestr = "(VP ref)";
	else {
		sprintf(filetypebuf, "(%ld)", thistype);
		filetypestr = filetypebuf;
	}
	fprintf(fout, "%c%c%-8s%7ld %s\n",
		isdir?'D':' ', istemp?'T':' ',
		filetypestr, thissize, thisname);
	clear_String(&thisname);
}

#define MAXPACKS 20
static
GetAttributeSequences(conn)
	CourierConnection *conn;
{
	int count, i;
	Unspecified buffer[MAXWORDS*MAXPACKS], *bp, *bufend;
	Filing4_StreamOfAttributeSequence attrs;
	
	bufend = buffer;
	bp = buffer+((MAXWORDS-1)*MAXPACKS);    /* end of available space */
	while ((count = BDTread(conn, (char*)bufend, 
				MAXWORDS*sizeof(Unspecified))) > 0) {
		bufend += count/sizeof(Unspecified);
		bytessent += count;
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

	switch (typevalue) {
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
					ch = '\n';
					while (hash && bytessent >= hashbytes){
						putchar('#');
						fflush(stdout);
						hashbytes += sizeof(buffer);
					}
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
			while (bytessent < hashbytes) {
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
	switch (typevalue) {
	default :
		while ((count = fread(buffer,sizeof(char),SPPMAXDATA,fin)) > 0
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
		while ((count = fread(buffer,sizeof(char),SPPMAXDATA,fin))
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
