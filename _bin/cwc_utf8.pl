#!/usr/bin/env perl -w

my $s;
my $x;
my $y;
my $ncword = 0;
my $cword = 0;

while (<>) {
    $s = $_;
    $s =~ s/\s//g;
    $x = () = $s =~ /[\x00-\x7f]+?/g;
    $y = () = $s =~ /[^\x00-\x7f]+?/g;

    $ncword += $x;
    $cword += $y;
}

$cword /= 3;

$s = $ncword + $cword;
print "非中文(不含空格)\t$ncword\n中文\t\t\t$cword\n共计\t\t\t$s\n";
