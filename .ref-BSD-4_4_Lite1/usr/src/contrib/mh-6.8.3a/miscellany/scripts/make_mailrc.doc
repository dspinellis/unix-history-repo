	Here is a script that will convert your .mhrc (if you have one
	in your home directory) into a legal .mailrc.  It will correctly
	handle lines of the form:

;	normal comment				#	normal comment
;;;;	also a legal comment			#	also a legal comment
single:	user1					alias single user1
mult:	user1, user2, user3			alias mult user1 user2 user3

semi;	user1					alias semi user1
semimul;	user2, user3			alias seminul user2 user3

	I put a target in my makefile which checks to see if .mhrc is
	more recent than .mailrc to regenerate it only when needed.

			      --      jad      --
			         John A Dilley

ARPA:			      terrapin@Purdue.EDU
UUCP:			      {ihnp4}! purdue!jad
PHONE:			      (317)494-6311 x2250
