#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/uio.h>
#include <stdio.h>
main () {
	int sock, msgsock, rval, rv ;
	struct sockaddr_un rqsts;
	char buf[1024] ;

	sock = socket (AF_UNIX, SOCK_STREAM, 0) ;
	if (sock < 0) {
		perror("stream socket") ;
		exit(1) ;
	}
	rqsts.sun_family = AF_UNIX ;
	strcpy (rqsts.sun_path,"/dev/connect") ;
	if (bind (sock, &rqsts, sizeof (rqsts))) {
		perror ("bind /dev/connect") ;
		exit (1) ;
	}
	listen (sock, 5) ;
	for (;;) {
		msgsock = accept(sock, 0, 0) ;
		if (msgsock == -1) {
			perror ("accept") ;
			continue ;
		}
		loop:
		rv = read (msgsock, buf, 1024) ;
		if (rv < 0) perror("read") ;
		else if (rv > 0) {
		struct msghdr msg ; int fd ;
	struct iovec iov[1];
	char filename[32] ;
			write (1, "{", 1) ; 
			write (1, buf, rv) ; 
			write (1, "}\n", 2) ; 
			/*strcat(buf, "-connectd responds") ;
			write(msgsock, buf, strlen(buf)) ;
			if (strncmp(buf,"byebye", 6) == 0) break;
			*/
			
	strcpy(filename,"a filename") ;
	msg.msg_name = "";		/* optional address */
	msg.msg_namelen = 0 ;		/* size of address */
	/*
	iov->iov_base = filename;
	iov->iov_len = sizeof (filename);
	*/
	iov->iov_base = 0;
	iov->iov_len = 0 ;
	msg.msg_iov = iov;
	msg.msg_iovlen = 1;
	msg.msg_accrights = (caddr_t) &fd ;		/* access rights sent/received */
	msg.msg_accrightslen = 4;
	fd = open (buf, 2) ;
printf("fd %d \n", fd) ;
	printf("$rv %d\n", sendmsg (msgsock, &msg, 0) );
	goto exitx ;

		}
		if (rv) goto loop ;
		close (msgsock) ;
	}
	exitx:
	close (sock) ;
	unlink ("/dev/connect") ;
}
