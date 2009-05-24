#!/usr/bin/perl
#

use strict;
use Switch;
use HTML::Parser;

my $keydir = $ENV{XDG_DATA_HOME} . "/uzbl/forms";
my ($config,$pid,$xid,$fifo,$socket,$url,$title,$cmd) = @ARGV;
if($fifo eq "") { die "No fifo"; };

sub domain {
	my ($url) = @_;
	$url =~ s#http(s)?://(www\.|)([A-Za-z0-9\.]+)(/.*)?#$3#;
	return $url;
}

my $terminal = "urxvt";
my $editor = "emacs";
my $downloader = "curl -s -A 'Uzbl'";
my @fields = ("type","name","value");

my %command;
$command{load} = sub {
	my ($domain) = @_;
	my $file = "$keydir/$domain";
	if( -e $file){
		open(FH,$file);
		my (@lines) = <FH>;
		close(FH);
		$|++;
		open(FIFO,">>$fifo");
		print "opened $fifo\n";
		foreach my $line (@lines) {
				if($line !~ m/^#/){
					my ($type,$name,$value) = ($line =~ /\s*(\w+)\s*\|\s*(.*?)\s*\|\s*(.*?)\s*$/);
					switch ($type) {
						case ""					{}
						case "checkbox"	{ printf FIFO 'act js document.getElementsByName("%s")[0].checked = %s;',	$name, $value}
						#case "submit"		{ printf FIFO 'act js function fs (n) {try{n.submit()} catch (e){fs(n.parentNode)}}; fs(document.getElementsByName("%s")[0]);', $name }
						else 						{ printf FIFO 'act js document.getElementsByName("%s")[0].value = "%s";',	$name, $value}
					}

					print FIFO "\n";
				}
		}
		$|--;
		close(FIFO);
	} else {
		$command{new}->($domain);
		$command{edit}->($domain);
	}
}	;
$command{edit} = sub {
	my ($domain) = @_;
	my $file = "$keydir/$domain";
	if(-e $file){
        system ($terminal, "-e", $editor, $file);
	}
};
$command{new} = sub {
	my ($domain) = @_;
	my $file = "$keydir/$domain";
	
	sub start {
		my ($tag,$attr) = @_;
		if($tag eq "input"){
			printf FILE " %-10s | %-10s | %-10s\n", (map {$attr->{"$_"}} @fields);
		}
	}
	my $p = HTML::Parser->new(api_version => 3, start_h => [\&start, "tagname,attr"]);
	open(FILE,">>$file");
	$p->parse(join(" ",`$downloader $url`));
	close(FILE);
};
$command{$cmd}->(domain($url));
