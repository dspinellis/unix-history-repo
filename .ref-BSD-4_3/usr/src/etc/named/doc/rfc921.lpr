

Network Working Group                                         Jon Postel
Request for Comments: 921                                            ISI
                                                            October 1984
Updates:  RFC 897, RFC 881

          Domain Name System Implementation Schedule - Revised


Status of this Memo

   This memo is a policy statement on the implementation of the Domain
   Style Naming System in the Internet.  This memo is an update of
   RFC-881, and RFC-897.  This is an official policy statement of the
   IAB and the DARPA.  Distribution of this memo is unlimited.

   The intent of this memo is to detail the schedule for the
   implementation for the Domain Style Naming System.  The explanation
   of how this system works is to be found in the references.

The Current Situation

   There are three aspects to the domain style naming system, (1) the
   names themselves, (2) the method of translating names to addresses,
   and (3) the relationship between the Internet and the rest of the
   world.

   Names

      The names are being changed from simple names, or globally unique
      strings, to structured names, where each component name is unique
      only with respect to the superior component name.

      Simple Names

         Until recently, hosts in the DARPA research and DDN operational
         communities were assigned names in a flat or global name space
         of character strings.  There are some limits on these names.
         They must start with a letter, end with a letter or digit and
         have only letters or digits or hyphen as interior characters.
         Case is not significant.

            For example:  USC-ISIF

      Hierarchical Names

         Because of the growth of the Internet, structured names (or
         domain style names) have been introduced.  Each element of the
         structured name will be a character string (with the same
         constraints that previously applied to the simple names).  The




Postel                                                          [Page 1]



RFC 921                                                     October 1984
Domain Implementation Schedule - Revised


         elements (or components) of the structured names are separated
         with periods, and the elements are written from the most
         specific on the left to the most general on the right.

            For example:  USC-ISIF.ARPA

      The Initial and Temporary Domain

         The introduction of these hierarchical names has been very
         limited.  Every current name in this new system has the form
         "old-simple-name.ARPA".  That is, the all the hosts are in a
         domain called "ARPA".  This is a temporary situation.  The
         current intention is for the ARPA domain to cease to exist.
         This means that all hosts will change their names as the domain
         style names come into full use.

   Name to Address Lookup

      Every host in the Internet is expected to have a way of
      translating the name of any other host into its Internet address.

      By and large, the name to address translation is done by looking
      up the information in a table of all hosts.

      The maintenance of this table is centralized at the Network
      Information Center (NIC).  Each host is expected to obtain a
      current copy of the table on a timely basis.  This table is called
      "HOSTS.TXT" [8] and is normally accessed via the Hostnames
      Server [9].

   Interface to the World

      A great deal of mail moves between the Internet and other
      "systems" that somehow transport mail among computers.  This is
      currently done by hiding some sort of "other-system" addressing
      information in the local-part of the mail address and using a
      mail-relay host in the host-part of the mailbox.

      For example,

         OBERST%EDUCOM.MAILNET@MIT-MULTICS.ARPA
         EDMISTON.CIC@CSNET-RELAY.ARPA







Postel                                                          [Page 2]



RFC 921                                                     October 1984
Domain Implementation Schedule - Revised


The Future Situation

   Names

      Hierarchical Names

         The use of the hierarchical names will be greatly expanded
         according to the rules established in the "Domain Requirements"
         memo (RFC-920) [5].

            For example:  F.ISI.USC.EDU

      There are several levels of development for use of the domain
      style names.

      First, there is the current simple substitution of the domain
      style names for the old style host names.  At this stage all
      domain style names directly translate to host addresses (using the
      NIC tables) and all domain style names have two components.  The
      mail system uses addresses of the form "local-part@host", where
      host is a domain style host name.

         For example:  USC-ISIF.ARPA  and  Postel@USC-ISIF.ARPA

         Here we expect that "USC-ISIF.ARPA" is the name of an Internet
         host and that we can send mail for "Postel" to the SMTP port on
         that host.  It may be that some backward host can still fake it
         by ignoring the ".ARPA" and looking up an address for
         "USC-ISIF" in some old style file.

      Second, there is an extension to more name components and more top
      level domains.  The mail system still uses addresses of the form
      "local-part@host", where host is a domain style host name.

         For example:  F.ISI.USC.EDU  and  Postel@F.ISI.USC.EDU

         Here we expect that "F.ISI.USC.EDU" is the name of an Internet
         host and that we can send mail for "Postel" to the SMTP port on
         that host.  It is likely that the NIC will enter these new
         domain style names in the centrally maintained table (i.e.,
         HOSTS.TXT) during the transition period.  It is unlikely that a
         backward host can hack this at all.

      Third, there is an extension to domain style names that may
      represent only organizations or administrative entities.  Finding
      a host that acts for such entities may require a level of



Postel                                                          [Page 3]



RFC 921                                                     October 1984
Domain Implementation Schedule - Revised


      indirection in the search.  The mail system may use
      "local-part@domain-name", where the "domain-name" identifies a
      host (as before) or an organization.

         For example:  USC-ISI.EDU  and  Postel@USC-ISI.EDU

         Here we don't count on "USC-ISI. EDU" being the name of an
         Internet host.  When we want to send mail to "Postel" we ask
         the domain name server about sending mail to "USC-ISI.EDU".
         The server will tell us the name (and address) of a real
         Internet host that handles mail on this organizations behalf,
         for example, "F.ISI.USC.EDU = 10.2.0.52".  We then send mail
         for "Postel@USC-ISI.EDU" to the SMTP port on F.ISI.USC.EDU.

   Name to Address Lookup

      Every host in the Internet will be expected to have a way of
      translating the name of any other host into its Internet address.

      By and large, the name to address translation will be done by
      interacting with a lookup server.  There will be a number of
      servers that each hold a portion of the name to address
      information.

      The maintenance of the translation data base will be subdivided
      and distributed.

      The design and implementation details for this service are given
      in RFC-882 [2] and RFC-883 [3].

   Interface to the World

      Mail will continue to move between the Internet and other
      "systems".  This may be done by designating some sort of
      "other-system" representative organization in the domain server
      data bases that can indirect mail to a mail-relay host.

      For example,

         Oberst@EDUCOM.MAILNET

         When we want to send mail to "Oberst" we ask the domain name
         server about sending mail to "EDUCOM.MAILNET".  The server will
         tell us the name (and address) of a real Internet host that
         handles mail on this organizations behalf, for example,
         "MIT-MULTICS.ARPA = 10.0.0.6".  We then send mail for
         "Oberst@EDUCOM.MAILNET" to the SMTP port on MIT-MULTICS.ARPA.


Postel                                                          [Page 4]



RFC 921                                                     October 1984
Domain Implementation Schedule - Revised


      For example,

         Edmiston@CIC.CSNET

         When we want to send mail to "Edmiston" we ask the domain name
         server about sending mail to "CIC.CSNET".  The server will tell
         us the name (and address) of a real Internet host that handles
         mail on this organizations behalf, for example,
         "CSNET-RELAY.ARPA = 10.4.0.5".  We then send mail for
         "Edmiston@CIC.CSNET" to the SMTP port on CSNET-RELAY.ARPA.

The Transition Situation

   Actually, the situation is a bit more complicated, of course.  Hosts
   are already using domain style names under the constraint that their
   domain style name is exactly their old style name with the string
   ".ARPA" appended.  The first transition step is to ensure that all
   hosts do this, and then to eliminate the use of old style names
   altogether.

   Please note carefully that two types of changes are being made:

      One is a change in the support mechanism for translating a host
      name to an internet address,

         that is from using local copies of a full centrally maintained
         table to dynamically accessing a distributed set of servers
         each posesing a portion of a data base maintained in a
         distributed fashion.

      The other is a change in the host names themselves,

         from a flat global space of unstructured strings to a
         hierarchical structure of names.

   There are two steps to the transition plan.

      First, change from old names to domain style names.

      Second, change from using central tables to using name servers.

   There are two communities that are taking slightly different courses
   in this transition.  The DARPA research community is making the full
   transition.  The DDN operational community is making the change in
   naming on the same schedule, but is not requiring hosts in the DDN
   operational community make the change to using servers at the same



Postel                                                          [Page 5]



RFC 921                                                     October 1984
Domain Implementation Schedule - Revised


   time (they can if they want to).  The DDN PMO will establish a
   schedule for that change at a later time.  The NIC will maintain a
   central table of all DDN operational hosts.

   Interface to the World

      The interchange of mail with "other-systems" will have to continue
      pretty much as it has (except that RELAY-HOST is RELAY-HOST.ARPA)
      until organization names can be used.  Then representative
      organizations can be designated for each "other-system" in the
      domain server data bases that will then specify a mail-relay host.

All Hosts Change Names

   The impact of introducing the domain style names is that all hosts
   change their names at least once.  Hosts that move to new domains or
   subdomains may change their names several times.

   Hosts have an official (or primary) name and possibly several
   nicknames.  When mail is sent from a host, the official name is used
   in the mail header address fields.

   Suppose, that in the old days before domains were thought of, a host
   changed its name.  What is the impact on users of changing the name
   of a host?

      Mail that was sent before the name was changed can not be answered
      using mail program commands that automatically fill in the return
      address.  While it may be possible to use special tricks to fix up
      the "From" or the "To" users addresses, the "Cc" addresses are
      very difficult to correct.

         Suppose one host changed its name from FOO to BAR.  Mail that
         was sent from FRED@FOO to JOE@ABC can not be answered unless
         the change of name is known to the user or the mail program at
         ABC and the host name BAR substituted for FOO.  Mail that is
         sent to JOE@ABC from SAM@DEF with a cc to FRED@FOO can not be
         answered easily.

      Any mailing lists that have mailboxes with the host that changed
      names will now have incorrect entries.

   The point is that while the host that changed names may be able to
   use special tricks for a while to fix things up for the users, it is
   difficult for other hosts to do this.




Postel                                                          [Page 6]



RFC 921                                                     October 1984
Domain Implementation Schedule - Revised


   A general trick is to make the old name a nickname for the host for
   some period of time.

   The introduction of domain style names means that all hosts change
   their names essentially at the same time.

   To lessen the havoc, there will be a period of time when both the old
   and the new names are allowed.  That is, the old names will be
   nicknames for a while.

Primary Names

   Currently, host have an official or primary names and may have
   several nicknames.  For example,

      Primary Name             Nicknames

      USC-ISIF.ARPA            USC-ISIF ISIF

      ADA-VAX.ARPA             ADA-VAX ISI-VAXB  AJPO  VAXB

   The data base is such than given any of the names for a host one can
   find the address, and given the address one can find the primary
   name.

   In the new domain style name system this property must be maintained.
   That is, given the Internet address of a host one must be able to
   find the primary name of that host.  This calls for careful
   management of the distributed database by those in charge of the
   domains and zones.



















Postel                                                          [Page 7]



RFC 921                                                     October 1984
Domain Implementation Schedule - Revised


The Revised Time Table

   There are three major phases to the implementation of the domain
   names system: (1) putting the machinery in place (servers,
   resolvers), (2) getting the data base installed, (3) changing the
   user programs (mailers, etc.).

      The machinery is now (at last) well along, there is a server for
      TOPS-20, and two different servers for Unix.  The data base now
      contains the ARPA domain and is initialized for the other top
      level domains.  Little has been done to change user programs to
      use the new procedures.

   Done

      Service Design and Specification:  The design and specification
      for the protocol and data base were published (RFC-882, RFC-883).

      Domain Requirements Specification:  The requirements for
      establishing a new domain are published as an RFC (RFC-920).

      Domain Style Names in Table:  Hosts are using their domain style
      names as their official and primary names.  The standard table of
      host names contains domain style names as the official and primary
      name.

      Servers for ARPA Domain:  Several domain name servers are in
      operation to supply host name to internet address translations,
      one of these servers is at the NIC.

   15 Dec 84  Domain Table

      A master table of top level domain names and their associated
      servers is established at the NIC.  Probably this information will
      be added to the HOSTS.TXT file as a new entry type.

   15 Jan 85  Begin New Domain Registration

      New domains may register according to the procedures and
      restrictions described in RFC-920 [5].

   15 Feb 85  Major Machinery Completed

      The principal servers are up and running, there are resolvers
      programmed and tested for the most popular systems (Unix 4.2bsd,
      TOPS-20).



Postel                                                          [Page 8]



RFC 921                                                     October 1984
Domain Implementation Schedule - Revised


   15 May 85  Significant Use of Resolvers and Servers

      Programs (e.g., Mailers, Telnet, FTP) begin regular use of the new
      mechanisms (resolvers and servers).  This may be done by changing
      the programs to act as resolvers themselves and call on servers
      directly, or to provide system calls that include the resolver
      function to replace old system calls that accessed the host table.

   15 Jul 85  Implementation of the Domain Naming System Completed

      The goal is to complete the switch over to the domain style names
      and the use of the servers by this date.  All programs that
      translate host name to Internet addresses should now use
      procedures based on the use of the domain style names system of
      resolvers and servers and the distributed data base.

   15 Sep 85  Decommission Host Table

      At this point the master host table maintained by the NIC need no
      longer be complete for the DARPA research community.  A full table
      of the DDN operational hosts will be maintained by the NIC.

   15 Oct 85  DDN Plan for Domains Name Service

      The DDN PMO may establish a plan for the future support of name to
      address translations in the DDN community.























Postel                                                          [Page 9]



RFC 921                                                     October 1984
Domain Implementation Schedule - Revised


Appendix : The Old Time Table

   Here we present the time table from the previous schedule (RFC-897)
   with some comments on what was and was not accomplished.

   -- Nov 83  Plan and Schedule

      At this point the overall plan for the implementation of domain
      style names and name servers, and a schedule of events was
      published (RFC-881).  Also the design and specification for the
      protocol and data base were published (RFC-882, RFC-883).

         <This was done, but the schedule did not work.>

   -- Nov 83  Initial Domain Style Host Name Table

      At this point a version of the host table which includes the
      domain style names is made available (DHOSTS.TXT).

         <This was done, on schedule.>

   -- Feb 84  Domain Requirements Specification

      At this point the requirements for establishing a new domain are
      published as an RFC.

         <This topic was much discussed in the Namedroppers mailing
         list, but no RFC was published until Oct84 [5].>

   14 Mar 84  Begin using Domain Style Names

      At this point all hosts should start using their domain style
      names as their official and primary names.  The standard table of
      host names contains domain style names as the official and primary
      name (DHOSTS.TXT becomes HOSTS.TXT).

         <This was done, on schedule.>

   04 Apr 84  Server for ARPA Domain

      At this point several domain name servers are in operation to
      supply host name to internet address translations, one of these
      servers is at the NIC.

         <This was done, not on schedule, but by Sep84.>




Postel                                                         [Page 10]



RFC 921                                                     October 1984
Domain Implementation Schedule - Revised


   04 Apr 84  Domain Table

      At this point a master table of top level domain names and their
      associated servers is established at the NIC.

         <Not done yet.>

   02 May 84  Stop using old style Names

      At this point the use of old style names must be completely phased
      out.

         <I think this is done.  Except that some hosts still use the
         OHOSTS.TXT file.>

   02 May 84  Certain New Domains

      At this point a few new domains may be established, in particular
      the DDN domain.

         <Not done yet.  Well, "DDN" won't be a top level domain
         according to the new rules (see [5]).>

   06 Jun 84  General & Multilevel Domains

      At this point additional new domains may be established, if they
      meet the requirements.  Domain style names may have more than two
      segments.

         <Not done yet.>

   18 Jul 84  Organizational Domains

      Domain style names may identify organizations.  Finding an address
      for a host may involve a level of indirection.

         <Not done yet.>

   05 Sep 84  Decommission Host Table

      At this point the master host table maintained by the NIC need no
      longer be complete for the DARPA research community.  A full table
      of the DDN operational hosts will be maintained by the NIC.

         <Not done yet.>




Postel                                                         [Page 11]



RFC 921                                                     October 1984
Domain Implementation Schedule - Revised


   03 Oct 84  DDN Plan for Domains Name Service

      At this point the DDN PMO will establish a plan for the future
      support of name to address translations in the DDN community.

         <Not done yet.>











































Postel                                                         [Page 12]



RFC 921                                                     October 1984
Domain Implementation Schedule - Revised


References

   [1]  Postel, J., "The Domain Names Plan and Schedule", RFC-881, USC
        Information Sciences Institute, November 1983.

   [2]  Mockapetris, P., "Domain Names - Concepts and Facilities",
        RFC-882, USC Information Sciences Institute, November 1983.

   [3]  Mockapetris, P., "Domain Names - Implementation and
        Specification", RFC-883, USC Information Sciences Institute,
        November 1983.

   [4]  Postel, J., "Domain Name System Implementation Schedule",
        RFC-897, USC Information Sciences Institute, February 1984.

   [5]  Postel, J., and J. Reynolds, "Domain Requirements", RFC-920, USC
        Information Sciences Institute, October 1984.

   [6]  Mockapetris, P., "The Domain Name System", Proceedings of the
        IFIP 6.5 Working Conference on Computer Message Services,
        Nottingham, England, May 1984.  Also as ISI/RS-84-133,
        June 1984.

   [7]  Mockapetris, P., J. Postel, and P. Kirton, "Name Server Design
        for Distributed Systems", Proceedings of the Seventh
        International Conference on Computer Communication, Sidney,
        Australia, October 1984.  Also as ISI/RS-84-132, June 1984.

   [8]  Feinler, E., K. Harrenstien, Z. Su, and V. White, "DoD Internet
        Host Table Specification", RFC-810, Network Information Center,
        SRI International, March 1982.

   [9]  Harrenstien, K., V. White, and E. Feinler, "Hostnames Server",
        RFC-811, Network Information Center, SRI International,
        March 1982.














Postel                                                         [Page 13]

