*** /tmp/uux.c.old	Sat Apr  2 20:16:45 1983
--- /tmp/uux.c.new	Sat Apr  2 20:16:54 1983
***************
*** 11,18
  *cmdp++ = ' ';\
  *cmdp = '\0';}
  
! #define GENSEND(f, a, b, c, d) {\
! fprintf(f, "S %s %s %s - %s 0666\n", a, b, c, d);\
  }
  #define GENRCV(f, a, b, c) {\
  fprintf(f, "R %s %s %s - \n", a, b, c);\

--- 11,18 -----
  *cmdp++ = ' ';\
  *cmdp = '\0';}
  
! #define GENSEND(f, a, b, c, d, e) {\
! fprintf(f, "S %s %s %s -%s %s 0666\n", a, b, c, d, e);\
  }
  #define GENRCV(f, a, b, c) {\
  fprintf(f, "R %s %s %s - \n", a, b, c);\
***************
*** 36,41
  	char inargs[BUFSIZ];
  	int pipein = 0;
  	int startjob = 1;
  	char path[MAXFULLNAME];
  	char cmd[BUFSIZ];
  	char *ap, *cmdp;

--- 36,42 -----
  	char inargs[BUFSIZ];
  	int pipein = 0;
  	int startjob = 1;
+ 	int Copy = 1;
  	char path[MAXFULLNAME];
  	char cmd[BUFSIZ];
  	char *ap, *cmdp;
***************
*** 65,70
  		case 'r':
  			startjob = 0;
  			break;
  		case 'x':
  			Debug = atoi(&argv[1][2]);
  			if (Debug <= 0)

--- 66,74 -----
  		case 'r':
  			startjob = 0;
  			break;
+ 		case 'c':
+ 			Copy = 0;
+ 			break;
  		case 'x':
  			Debug = atoi(&argv[1][2]);
  			if (Debug <= 0)
***************
*** 148,154
  		}
  		fclose(fpd);
  		if (strcmp(local, xsys) != SAME) {
! 			GENSEND(fpc, dfile, dfile, User, dfile);
  			cflag++;
  		}
  		fprintf(fprx, "%c %s\n", X_RQDFILE, dfile);

--- 152,158 -----
  		}
  		fclose(fpd);
  		if (strcmp(local, xsys) != SAME) {
! 			GENSEND(fpc, dfile, dfile, User, "", dfile);
  			cflag++;
  		}
  		fprintf(fprx, "%c %s\n", X_RQDFILE, dfile);
***************
*** 230,240
  				fprintf(stderr, "permission denied %s\n", rest);
  				cleanup(1);
  			}
! 			if (xcp(rest, dfile) != 0) {
! 				fprintf(stderr, "can't copy %s to %s\n", rest, dfile);
! 				cleanup(1);
! 			}
! 			GENSEND(fpc, rest, dfile, User, dfile);
  			cflag++;
  			if (redir == '<') {
  				fprintf(fprx, "%c %s\n", X_STDIN, dfile);

--- 234,247 -----
  				fprintf(stderr, "permission denied %s\n", rest);
  				cleanup(1);
  			}
! 			if (Copy) {
! 				if (xcp(rest, dfile) != 0) {
! 					fprintf(stderr, "can't copy %s to %s\n", rest, dfile);
! 					cleanup(1);
! 				}
! 				GENSEND(fpc, rest, dfile, User, "", dfile);
! 			} else
! 				GENSEND(fpc, rest, dfile, User, "c", dfile);
  			cflag++;
  			if (redir == '<') {
  				fprintf(fprx, "%c %s\n", X_STDIN, dfile);
***************
*** 288,294
  			gename(DATAPRE, xsys, 'T', t2file);
  			GENRCV(fpd, rest, t2file, User);
  			fclose(fpd);
! 			GENSEND(fpc, dfile, tfile, User, dfile);
  			cflag++;
  			if (redir == '<') {
  				fprintf(fprx, "%c %s\n", X_RQDFILE, t2file);

--- 295,301 -----
  			gename(DATAPRE, xsys, 'T', t2file);
  			GENRCV(fpd, rest, t2file, User);
  			fclose(fpd);
! 			GENSEND(fpc, dfile, tfile, User, "", dfile);
  			cflag++;
  			if (redir == '<') {
  				fprintf(fprx, "%c %s\n", X_RQDFILE, t2file);
***************
*** 332,338
  				xuuxqt();
  	}
  	else {
! 		GENSEND(fpc, rxfile, tfile, User, rxfile);
  		cflag++;
  	}
  

--- 339,345 -----
  				xuuxqt();
  	}
  	else {
! 		GENSEND(fpc, rxfile, tfile, User, "", rxfile);
  		cflag++;
  	}
  
