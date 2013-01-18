#!/usr/bin/perl
use CGI;

#print "Content-type: text/html\n\n";

$q=new CGI;
%params = $q->Vars;


#while (my ($k, $v) = each %params) { print "$k\t\t=\t$v\n" }

#print "\nENV IS\n\n";
#while (my ($k, $v) = each %ENV) { print "$k\t\t=\t$v\n" }

#print '/usr/local/bin/aspcc ' . %ENV->{'PATH_TRANSLATED'};
exec "/usr/local/bin/aspcc $ENV{PATH_TRANSLATED}"
