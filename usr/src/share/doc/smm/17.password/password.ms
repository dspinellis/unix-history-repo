.\" %sccs.include.proprietary.roff%
.\"
.\"	@(#)password.ms	6.2 (Berkeley) %G%
.\"
.\" tbl mm ^ eqn ^ troff -ms
.EH 'SMM:18-%''Password Security: A Case History'
.OH 'Password Security: A Case History''SMM:18-%'
.EQ
delim $$
.EN
.\".RP
.\" TM 78-1271-5 39199 39199-11
.ND April 3, 1978
.TL
Password Security:
A Case History
.\" .OK
.\"Encryption
.\"Computing
.AU "MH 2C-524" 3878
Robert Morris
.AU "MH 2C-523" 2394
Ken Thompson
.AI
AT&T Bell Laboratories
Murray Hill, NJ
.AB
This paper describes the history of the design of the
password security scheme on a remotely accessed time-sharing
system.
The present design was the result of countering
observed attempts to penetrate the system.
The result is a compromise between extreme security and
ease of use.
.AE
.\" .CS 6 0 6 0 0 4
.SH
INTRODUCTION
.PP
Password security on the
.UX
time-sharing system [1] is provided by a
collection of programs
whose elaborate and strange design is the outgrowth of
many years of experience with earlier versions.
To help develop a secure system, we have had a continuing
competition to devise new ways to
attack the security of the system (the bad guy) and, at the same time, to
devise new techniques to resist the new attacks (the good guy).
This competition has been in the same vein as the
competition of long standing between manufacturers of armor
plate and those of armor-piercing shells.
For this reason, the description that follows will
trace the history of the password system rather than simply
presenting the program in its current state.
In this way, the reasons for the design will be made clearer,
as the design cannot be understood without also
understanding the potential attacks.
.PP
An underlying goal has been to provide password security
at minimal inconvenience to the users of the system.
For example, those who want to run a completely open
system without passwords, or to have passwords only at the
option of the individual users, are able to do so, while
those who require all of their users to have passwords
gain a high degree of security
against penetration of the system by unauthorized
users.
.PP
The password system must be able not only to prevent
any access to the system by unauthorized users
(i.e. prevent them from logging in at all),
but it must also
prevent users who are already logged in from doing
things that they are not authorized to do.
The so called ``super-user'' password, for example, is especially
critical because the super-user has all sorts of
permissions and has essentially unlimited access to
all system resources.
.PP
Password security is of course only one component of
overall system security, but it is an essential component.
Experience has shown that attempts to penetrate
remote-access systems have been astonishingly
sophisticated.
.PP
Remote-access systems are peculiarly vulnerable to
penetration by outsiders as there are threats at the
remote terminal, along the communications link, as well
as at the computer itself.
Although the security of a password encryption algorithm
is an interesting intellectual and mathematical problem,
it is only one tiny facet of a very large problem.
In practice, physical security of the computer, communications
security of the communications link, and physical control
of the computer itself loom as far more important issues.
Perhaps most important of all is control over the actions
of ex-employees, since they are not under any direct control
and they may have intimate
knowledge about the system, its resources, and
methods of access.
Good system security involves realistic
evaluation of the risks not only of deliberate
attacks but also of casual unauthorized access
and accidental disclosure.
.SH
PROLOGUE
.PP
The UNIX system was first implemented with a password file that contained
the actual passwords of all the users, and for that reason
the password file had to
be heavily protected against being either read or written.
Although historically, this had been the technique used
for remote-access systems,
it was completely unsatisfactory for several reasons.
.PP
The technique is excessively vulnerable to lapses in
security.
Temporary loss of protection can occur when
the password file is being edited or otherwise modified.
There is no way to prevent the making of copies by
privileged users.
Experience with several earlier remote-access systems
showed that such lapses occur with frightening frequency.
Perhaps the most memorable such occasion occurred
in the early 60's when
a system administrator on the CTSS system at MIT
was editing the
password file and another system administrator was editing
the daily message that is printed on everyone's terminal
on login.
Due to a software design error, the temporary editor files
of the two users were interchanged and thus, for a time, the password
file was printed on every terminal when it was logged in.
.PP
Once such a lapse in security has been discovered, everyone's
password must be changed, usually simultaneously, at a considerable
administrative cost.
This is not a great matter, but
far more serious is the high probability of such lapses
going unnoticed by the system administrators.
.PP
Security against unauthorized disclosure of the passwords was,
in the last analysis, impossible with this system because,
for example, if the
contents of the file system are put on to magnetic tape for
backup, as they must be, then anyone who has physical
access to the tape
can read anything on it with no restriction.
.PP
Many programs must get information of various kinds
about the users of the system, and these programs in general
should have no special permission to read the password file.
The information which should have been in the password file actually was
distributed (or replicated) into a number of files, all of
which had to be updated whenever a user was added to or
dropped from the system.
.SH
THE FIRST SCHEME
.PP
The obvious solution is to arrange that the passwords not
appear in the system at all, and it is not difficult to decide
that this can be done by encrypting each user's password,
putting only the encrypted form in the password file, and
throwing away his original password (the one that
he typed in).
When the user later tries to log in to the system, the password
that he types is encrypted and compared with the encrypted
version in the password file.
If the two match, his login attempt is accepted.
Such a scheme was first described
in [3, p.91ff.].
It also seemed advisable to devise
a system in which neither the password file nor the
password program itself needed to be
protected against being read by anyone.
.PP
All that was needed to implement these ideas
was to find a means of encryption that was very difficult
to invert, even when the encryption program
is available.
Most of the standard encryption methods used (in the past)
for encryption of messages are rather easy to invert.
A convenient and rather good encryption program happened
to exist on the system at the time; it simulated the
M-209 cipher machine [4]
used by the U.S. Army during World War II.
It turned out that the M-209 program was usable, but with
a given key, the ciphers produced by this program are
trivial to invert.
It is a much more difficult matter to find out the key
given the cleartext input and the enciphered output of the program.
Therefore,
the password was used not as the text to be encrypted but as the
key, and a constant was encrypted using this key.
The encrypted result was entered into the password file.
.SH
ATTACKS ON THE FIRST APPROACH
.PP
Suppose that the bad guy has available
the text of the password encryption program and
the complete password file.
Suppose also that he has substantial computing
capacity at his disposal.
.PP
One obvious approach to penetrating the password
mechanism is to attempt to find a general method of inverting
the encryption algorithm.
Very possibly this can be done, but few
successful results
have come to light, despite substantial efforts extending
over a period of more than five years.
The results have not proved to be very useful
in penetrating systems.
.PP
Another approach to penetration is simply to keep trying
potential
passwords until one succeeds; this is a general cryptanalytic
approach called
.I
key search.
.R
Human beings being what they are, there is a strong tendency
for people to choose relatively short and simple passwords that
they can remember.
Given free choice, most people will choose their passwords
from a restricted character set (e.g. all lower-case letters),
and will often choose words or names.
This human habit makes the key search job a great deal easier.
.PP
The critical factor involved in key search is the amount of
time needed to encrypt a potential password and to check the result
against an entry in the password file.
The running time to encrypt one trial password and check
the result turned out to be approximately 1.25 milliseconds on
a PDP-11/70 when the encryption algorithm was recoded for
maximum speed.
It is takes essentially no more time to test the encrypted
trial password against all the passwords in
an entire password file, or for that matter, against
any collection of encrypted passwords, perhaps collected
from many installations.
.PP
If we want to check all passwords of length
.I
n
.R
that consist entirely of lower-case letters, the number
of such passwords is $26 sup n$.
If we suppose that the password consists of
printable characters only, then the number of possible passwords
is somewhat less than $95 sup n$.
(The standard system ``character erase'' and ``line kill''
characters are, for example, not prime
candidates.)
We can immediately estimate the running time of a program that
will test every password of a given length with all of its
characters chosen from some set of characters.
The following table gives estimates of the running time
required on a PDP-11/70
to test all possible character strings of length $n$
chosen from various sets of characters: namely, all lower-case
letters, all lower-case letters plus digits,
all alphanumeric characters, all 95 printable
ASCII characters, and finally all 128 ASCII characters.
.TS
cccccc
cccccc
nnnnnn.
	26 lower-case	36 lower-case letters	62 alphanumeric	95 printable	all 128 ASCII
n	letters	and digits	characters	characters	characters
.sp .5
1	30 msec.	40 msec.	80 msec.	120 msec.	160 msec.
2	800 msec.	2 sec.	5 sec.	11 sec.	20 sec.
3	22 sec.	58 sec.	5 min.	17 min.	43 min.
4	10 min.	35 min.	5 hrs.	28 hrs.	93 hrs.
5	4 hrs.	21 hrs.	318 hrs.
6	107 hrs.
.TE
.LP
One has to conclude that it is no great matter for someone with
access to a PDP-11 to test all lower-case alphabetic strings up
to length five
and, given access to the machine for, say, several weekends, to test
all such strings up to six characters in length.
By using such a program against a collection of actual encrypted
passwords, a substantial fraction of all the passwords will be
found.
.PP
Another profitable approach for the bad guy is to use the word
list from a dictionary or to use a list of names.
For example, a large commercial dictionary contains typicallly about
250,000 words; these words can be checked in about five minutes.
Again, a noticeable fraction of any collection of passwords
will be found.
Improvements and extensions will be (and have been) found by
a determined bad guy.
Some ``good'' things to try are:
.IP -
The dictionary with the words spelled backwards.
.IP -
A list of first names (best obtained from some mailing list).
Last names, street names, and city names also work well.
.IP -
The above with initial upper-case letters.
.IP -
All valid license plate numbers in your state.
(This takes about five hours in New Jersey.)
.IP -
Room numbers, social security numbers, telephone numbers, and
the like.
.PP
The authors have conducted experiments to try to determine
typical users' habits in the choice of passwords when no
constraint is put on their choice.
The results were disappointing, except to the bad guy.
In a collection of 3,289 passwords
gathered from many users over a long period of time;
.IP
15 were a single ASCII character;
.IP
72 were strings of two ASCII characters;
.IP
464 were strings of three ASCII characters;
.IP
477 were string of four alphamerics;
.IP
706 were five letters, all upper-case or all lower-case;
.IP
605 were six letters, all lower-case.
.LP
An additional 492 passwords appeared in various available
dictionaries, name lists, and the like.
A total of 2,831, or 86% of this sample of passwords fell into one of
these classes.
.PP
There was, of course, considerable overlap between the
dictionary results and the character string searches.
The dictionary search alone, which required only five
minutes to run, produced about one third of the passwords.
.PP
Users could be urged (or forced) to use either longer passwords
or passwords chosen from a larger character set, or the system
could itself choose passwords for the users.
.SH
AN ANECDOTE
.PP
An entertaining and instructive example is
the attempt made at one installation to force users to use less predictable
passwords.
The users did not choose their own passwords; the system supplied
them.
The supplied passwords were eight characters long and 
were taken from the character set consisting of
lower-case letters and digits.
They were generated by a pseudo-random number generator
with only $2 sup 15$ starting values.
The time required to search (again on a PDP-11/70) through
all character strings of length 8 from a 36-character
alphabet is 112 years.
.PP
Unfortunately, only $2 sup 15$ of them need be looked at,
because that is the number of possible outputs of the random
number generator.
The bad guy did, in fact, generate and test each of these strings
and found every one of the system-generated passwords using
a total of only about one minute of machine time.
.SH
IMPROVEMENTS TO THE FIRST APPROACH
.NH
Slower Encryption
.PP
Obviously, the first algorithm used was far too fast.
The announcement of the DES encryption algorithm [2]
by the National Bureau of Standards
was timely and fortunate.
The DES is, by design, hard to invert, but equally valuable
is the fact that it is extremely slow when implemented in
software.
The DES was implemented and used in the following way:
The first eight characters of the user's password are
used as a key for the DES; then the algorithm
is used to encrypt a constant.
Although this constant is zero at the moment, it is easily
accessible and can be made installation-dependent.
Then the DES algorithm is iterated 25 times and the
resulting 64 bits are repacked to become a string of
11 printable characters.
.NH
Less Predictable Passwords
.PP
The password entry program was modified so as to urge
the user to use more obscure passwords.
If the user enters an alphabetic password (all upper-case or
all lower-case) shorter than six characters, or a
password from a larger character set shorter than five
characters, then the program asks him to enter a
longer password.
This further reduces the efficacy of key search.
.PP
These improvements make it exceedingly difficult to find
any individual password.
The user is warned of the risks and if he cooperates,
he is very safe indeed.
On the other hand, he is not prevented from using
his spouse's name if he wants to.
.NH
Salted Passwords
.PP
The key search technique is still
likely to turn up a few passwords when it is used
on a large collection of passwords, and it seemed wise to make this
task as difficult as possible.
To this end, when a password is first entered, the password program
obtains a 12-bit random number (by reading the real-time clock)
and appends this to the password typed in by the user.
The concatenated string is encrypted and both the
12-bit random quantity (called the $salt$) and the 64-bit
result of the encryption are entered into the password
file.
.PP
When the user later logs in to the system, the 12-bit
quantity is extracted from the password file and appended
to the typed password.
The encrypted result is required, as before, to be the same as the
remaining 64 bits in the password file.
This modification does not increase the task of finding
any individual
password,
starting from scratch,
but now the work of testing a given character string
against a large collection of encrypted passwords has
been multiplied by 4096 ($2 sup 12$).
The reason for this is that there are 4096 encrypted
versions of each password and one of them has been picked more
or less at random by the system.
.PP
With this modification,
it is likely that the bad guy can spend days of computer
time trying to find a password on a system with hundreds
of passwords, and find none at all.
More important is the fact that it becomes impractical
to prepare an encrypted dictionary in advance.
Such an encrypted dictionary could be used to crack
new passwords in milliseconds when they appear.
.PP
There is a (not inadvertent) side effect of this
modification.
It becomes nearly impossible to find out whether a
person with passwords on two or more systems has used
the same password on all of them,
unless you already know that.
.NH
The Threat of the DES Chip
.PP
Chips to perform the DES encryption are already commercially
available and they are very fast.
The use of such a chip speeds up the process of password
hunting by three orders of magnitude.
To avert this possibility, one of the internal tables
of the DES algorithm
(in particular, the so-called E-table)
is changed in a way that depends on the 12-bit random
number.
The E-table is inseparably wired into the DES chip,
so that the commercial chip cannot be used.
Obviously, the bad guy could have his own chip designed and
built, but the cost would be unthinkable.
.NH
A Subtle Point
.PP
To login successfully on the UNIX system, it is necessary
after dialing in to type a valid user name, and then the
correct password for that user name.
It is poor design to write the login command in such a way that it
tells an interloper when he has typed in a invalid user name.
The response to an invalid name should be identical to
that for a valid name.
.PP
When the slow encryption algorithm was first implemented,
the encryption was done only if the user name was valid,
because otherwise there was no encrypted password to
compare with the supplied password.
The result was that the response was delayed
by about one-half second if the name was valid, but was
immediate if invalid.
The bad guy could find out
whether a particular user name was valid.
The routine was modified to do the encryption in either
case.
.SH
CONCLUSIONS
.PP
On the issue of password security, UNIX is probably
better than most systems.
The use of encrypted passwords appears reasonably
secure in the absence of serious attention of experts
in the field.
.PP
It is also worth some effort to conceal even the encrypted
passwords.
Some UNIX systems have instituted what is called an
``external security code'' that must be typed when
dialing into the system, but before logging in.
If this code is changed periodically, then someone
with an old password will likely be prevented from
using it.
.PP
Whenever any security procedure is instituted that attempts
to deny access to unauthorized persons, it is wise to
keep a record of both successful and unsuccessful attempts
to get at the secured resource.
Just as an out-of-hours visitor to a computer center normally
must not only identify himself, but a record is usually also kept of
his entry.
Just so, it is a wise precaution to make and keep a record
of all attempts to log into a remote-access time-sharing
system, and certainly all unsuccessful attempts.
.PP
Bad guys fall on a spectrum whose one end is someone with
ordinary access to a system and whose goal is to find
out a particular password (usually that of the super-user)
and, at the other end, someone who wishes to collect as
much password information as possible from as many systems
as possible.
Most of the work reported here serves to frustrate the latter type;
our experience indicates that the former type of bad guy never
was very successful.
.PP
We recognize that a time-sharing system must operate in a
hostile environment.
We did not attempt to hide the security aspects of the operating
system, thereby playing the customary make-believe game in
which weaknesses of the system are not discussed no matter
how apparent.
Rather we advertised the password algorithm and invited attack
in the belief that this approach would minimize future trouble.
The approach has been successful.
.\" .SG MH-1271-RM/KT
.SH
References
.IP [1]
Ritchie, D.M. and Thompson, K.
The UNIX Time-Sharing System.
.I
Comm. ACM
.B
17
.R
(July 1974),
pp. 365-375.
.IP [2]
.I
Proposed Federal Information Processing Data Encryption Standard.
.R
Federal Register (40FR12134), March 17, 1975
.IP [3]
Wilkes, M. V.
.I
Time-Sharing Computer Systems.
.R
American Elsevier,
New York, (1968).
.IP [4]
U. S. Patent Number 2,089,603.
