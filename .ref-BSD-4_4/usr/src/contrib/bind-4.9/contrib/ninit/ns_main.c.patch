RCS file: RCS/ns_main.c,v
retrieving revision 1.1
retrieving revision 1.2
diff -c -r1.1 -r1.2
*** /tmp/,RCSt1014490	Mon May 13 16:35:41 1991
--- /tmp/,RCSt2014490	Mon May 13 16:35:42 1991
***************
*** 182,187 ****
--- 182,188 ----
  	extern struct qstream *sqadd();
  	extern struct qinfo *qhead; 
  	extern char Version[];
+ 	int	nofork = 0;
  
  	ns_port = htons(NAMESERVER_PORT);
  
***************
*** 224,229 ****
--- 225,234 ----
  					setdebug(1);
  					break;
  
+ 				case 'n':
+ 					nofork++;
+ 					break;
+ 
  				case 'p':
  					if (--argc <= 0)
  						usage();
***************
*** 352,358 ****
  	 * we've done any slow initialization
  	 * and are ready to answer queries.
  	 */
! 	if (!debug) {
  #if defined(BSD) && BSD >= 199006
  		daemon(1, 0);
  #else
--- 357,363 ----
  	 * we've done any slow initialization
  	 * and are ready to answer queries.
  	 */
! 	if (!debug && !nofork) {
  #if defined(BSD) && BSD >= 199006
  		daemon(1, 0);
  #else
