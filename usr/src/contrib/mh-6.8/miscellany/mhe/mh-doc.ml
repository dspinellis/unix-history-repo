; This is "mhe", the Emacs-based front end to "mh", which is the Rand Mail
; Handler. MH is a set of programs designed to be called as commands from the
; shell. This system uses single-keystroke commands and maintains a visual
; display of the contents of the message file. I initially wrote it because I
; was drowning in mail and I needed some way to pare out the junk, and it has
; just sort of mushroomed into a real system. 
; 
; Brian K. Reid, Stanford University
; Version 1: April 1982
; Version 2: May 1982: new commands added
; Version 3: August 1982: rewrote send-to-shell for increased speed
; Version 4: Added extensive header caching mechanism for increased speed
; Version 5: documentation updated slightly. November 1982
; 
; ------------------------------------------------------------------------
; GETTING IT INSTALLED AT YOUR SITE:
; 
; Mhe consists of about a dozen mlisp files. The primary file is mh-e.ml,
; which in turn loads the others as needed. All of them must be in the
; directory where your Emacs will look for its library files. 
; 
; The file mh-e.ml must be edited to reflect the filename paths on your
; system:
; 	mh-progs	must be set to the name of the directory
; 			in which the MH programs are stored, i.e.
; 			"/usr/local/lib/mh" if the "scan" command
; 			is /usr/local/lib/mh/scan.
; 	bboard-path	must be set to the name of the directory
; 			that is the root of your "readnews" tree.
; 			If your "fa.human-nets" newsgroup is stored
; 			in /usr/spool/news/fa.human-nets/*, then
; 			you should set this variable to
; 			"/usr/spool/news". If you don't use
; 			readnews, then set it to "/dev/null".
; 
; The MH programs "repl", and "forw" have to be modified to include
; the option "-build", which causes them not to ask the "What now?"
; question at the end, but instead just exit (having built the file). Mhe will
; also be a lot more tolerable if you remove a lot of the warning messages
; from adrparse.c; there's no point making them fatal errors. If you aren't up
; to hacking directly on the MH programs, contact me as Reid@SU-SCORE or
; ucbvax!Shasta!reid, and I will provide you with my version of the code.
; If I weren't so lazy I would propagate these changes back to Rand, but I've
; forgotten the name of the contact there and I can't find our licensing
; agreement to look his name up. Besides, they have probably changed their
; sources out from under me anyhow. I have included a summary of the important
; changes at the end of this documentation.
; 
; Mhe requires Emacs #45 of Fri May 21 1982 or later, because it uses
; buffer-local variables.
; ----------------------------------------------------------------------------
; SETTING UP A NEW MHE USER.
; 
; If you are an mh user, then you can just run mhe with no further ado.
; However, you can speed things up substantially by putting an alias into your
; .cshrc file so that you won't need to spawn a new subshell when you run it:
; 
; alias mhe /usr/local/bin/emacs -lmh-e.ml -estartup $*
; 
; The shell syntax for mhe is
; 	mhe
; or
; 	mhe +inbox		first argument is folder name
; or
; 	mhe +inbox 200:300	second argument is message range
; 
; The folder name defaults to current-folder, and the message range defaults
; to "all".
; ------------------------------------------------------------------------
; HOW MHE WORKS
; 
; Mhe uses the Emacs subprocess facility to run mh commands in a subshell.
; Normally when you use mh, it runs the editor in a subshell; this inverted
; scheme of the editor running mh in the subshell is actually much much
; faster, because editors are slow in starting up but the mh programs are
; pretty fast. When you start mhe, it builds a buffer whose name equals the
; name of the current folder (e.g. "+inbox"), and places a "scan" listing into
; that buffer. Then as you edit your mail, deleting and moving messages, mhe
; builds up a set of shell commands in a buffer called "cmd-buffer". When you
; exit from mhe, it passes the contents of cmd-buffer off to the shell, and
; the deletes and moves are actually processed. If you open another mail file,
; its header is given its own buffer ("+carbons", "+bugs", etc.), and you can
; switch back and forth to them as needed. The Emacs buffer-local context
; mechanism makes everything happen almost perfectly. 
; 
; To avoid the overhead of doing a "scan" everytime you run mhe or switch
; folders, mhe maintains a cache of header lines in a file with the same name
; as the buffer; e.g. a file named ~/Mail/inbox/+inbox will hold the header
; cache for folder +inbox. The extended command "scavenge" will regenerate
; this header listing.
; 
; To avoid the overhead of loading the entire 50000-character mhe system on
; startup, most of the command-driven functions are off in autoloaded files,
; so that the first time you use a command you will have to wait for its
; definition to be loaded. This scheme seems to be perfectly acceptable to
; users. However, most people use mhe by running it once in the morning and
; sitting in it all day, so this feature doesn't buy much in the grand scheme.
; ------------------------------------------------------------------------
; 
; MODIFICATIONS TO MH
; 
; Here is a summary of the relevant changes to MH that I have made. Some of
; them are just optimizations.
; 
; (from repl.c; nearly identical changes go into forw.c.)
; 
; short	buildflag = 0;		/* just building a reply file? */
; ...
; 	"build",	      0,      /*12 */
; ...
; 			case 12:buildflag++; continue;	     /* -build */
; 	if (buildflag) 
; 	    drft = m_maildir("reply");
; 	else 
; 	    drft = m_maildir(draft);
; ...
; 	if((!buildflag) & (stat(drft, &stbuf) != -1)) {
; 		cp = concat("\"", drft, "\" exists; delete (y,n,l) ? ", 0);
; ...
; 	if (!buildflag) {
; 		if(m_edit(&ed, drft, NOUSE, msg) < 0)
; 			return;
; 		    }
; ...
; 	    if(!buildflag) {
; 	       if(!(argp = getans("\nWhat now? ", aleqs))) {
; 		 VOID unlink("@");
; 		 return;
; 	     }
; ...
; 	switch(buildflag ? 4 : smatch(*argp, aleqs)) {
; 		case 0: VOID showfile(drft);                    /* list */
; ...
; 
; In inc.c: (this code makes it possible for you to use an "inc" command
; outside of mhe, like in your .login file, and still have mhe pick up
; the headers of the new messages the next time you run it)
; 
; FILE    *in, *aud, *mhe_aud;
; ...
; 	char ..., *mhe_audfile;
; ...
; 	mhe_audfile = m_find("mhe");
; 	if(!m_find("path")) free(path("./", TFOLDER));
; ...
; 	if(mhe_audfile) {
; 		cp = concat(maildir, "/++", NULLCP);
; 		i = stat(cp, &stbuf);
; 		if((mhe_aud = fopen(cp, "a")) == NULL) {
; 			fprintf(stderr, "Can't append to ");
; 			perror(cp);
; 		} else if(i < 0)
; 			VOID chmod(cp, 0600);
; 	}
; ...
; 		if(aud)
; 			fputs(scanl, aud);
; 		if(mhe_aud)
; 			fputs(scanl, mhe_aud);
; ...
; 	if(mhe_aud)
; 		VOID fclose(mhe_aud);
; 
; In adrparse.c:
; Remove all instances of "goto line", replacing it with "break" if it is in
; the "switch" statement, otherwise just taking it out. This makes it so syntax
; errors will be non-fatal.  Remove the
; 	if(isalnum(*cp))||*cp=="-" || etc.
; statement about 40% of the way through, so that all characters not given
; specific meanings in the switch statement above it will be legal in mail
; names.
; 
; 
; ------------------------------------------------------------------------
; these functions let me edit the above documentation without the semicolons.
(defun
    (add-semicolons
	(beginning-of-file)
	(while (! (| (eobp) (looking-at "^(defun")))
	       (insert-string "; ")
	       (next-line) (beginning-of-line)
	)
    )
    
    (remove-semicolons
	(beginning-of-file)
	(while (! (| (eobp) (looking-at "^(defun")))
	       (while (| (looking-at "^; ") (looking-at "^;$"))
		      (delete-next-character)
		      (if (! (eolp))
		          (delete-next-character))
	       )
	       (next-line)
	)
    )
)
