From chuqui@nsc.UUCP (Cheshire Chuqui) Tue Nov 27 13:21:24 1984
Relay-Version: version B 2.10.2 11/2/84; site seismo.UUCP
Posting-Version: version B 2.10.2 9/17/84 chuqui version 1.7 9/23/84; site nsc.UUCP
Path: seismo!nsc!chuqui
From: chuqui@nsc.UUCP (Cheshire Chuqui)
Newsgroups: net.wanted.sources
Subject: Re: local netnews usage statistics
Message-ID: <1908@nsc.UUCP>
Date: 27 Nov 84 18:21:24 GMT
Date-Received: 27 Nov 84 18:32:17 GMT
References: <>
Reply-To: chuqui@nsc.UUCP (Cheshire Chuqui)
Distribution: net
Organization: Plaid Heaven
Lines: 41
Summary: 
In article <772@islenet.UUCP> richard@islenet.UUCP (Richard Foulk) writes:
>Does anyone have some kind of utility that pokes around in peoples .newsrc
>files and produces meaningful (?) statistics on general news readership
>for that site?

This is something Fred Blonder wrote a while back and passed around. It
does come in handy, especially if you are short on disk space and trying to
figure out what groups to shorten...


#! /bin/sh
#
# @(#)subscribers.sh	(University of Maryland) Fred Blonder 19-Aug-1983
#
# Find out how many people subscribe to each newsgroup

sub_tmp=/tmp/#s.$$

trap "rm -f $sub_tmp" 0 1 2 15

for dir in `awk -F: '{ print $6 }' /etc/passwd | sort -u`
do	# locate all login directories
	if	# if .newsrc exists
		[ -r $dir/.newsrc ]
	then	# find all newsgroups subscribed to, append to $sub_tmp
		awk -F: '/^net\..*: [0-9].*$/ { print $1 }' \
			$dir/.newsrc >> $sub_tmp
	fi
done

# Count all ocurrences of all newsgroups.
# Print result sorted by decreasing number of subscribers.
awk '{ x[$1] = x[$1] + 1 } \
	END { for (i in x) print i " " x[i] }' $sub_tmp | sort +1 -rn

-- 
>From the center of a Plaid pentagram:		Chuq Von Rospach
{cbosgd,decwrl,fortune,hplabs,ihnp4,seismo}!nsc!chuqui  nsc!chuqui@decwrl.ARPA

  ~But you know, monsieur, that as long as she wears the claw of the dragon
  upon her breast you can do nothing-- her soul belongs to me!~


