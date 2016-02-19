#!/usr/bin/perl

sub toInt($) {
    my $param = shift();
    return ($param) ? 1 : 0;
}
    
# -dirname
# -filename
sub runTest($$) {
    my ($dir, $file) = @_;
    my $path = "$dir/$file";
    my $output = `scala -cp scala-parser-combinators.jar:. Checker $path 2>&1`;
    my $isErr = toInt($output =~ /ill\-typed/m);
    my $isGood = toInt($output =~ /well\-typed/m);
    my $expectedErr = toInt($file =~ /^bad/);
    print "$path: ";
    if (($isGood == 0 && $isErr == 1) ||
        ($isGood == 1 && $isErr == 0)) {
        if ($expectedErr == $isErr) {
            print "pass!\n";
			return 1;
        } else {
            print "----FAIL----\n";
			return 0;
        }
    } else {
        print "----UNKNOWN (Consider Failure)----\n";
		return 0;
    }
}

# Given the name of a directory, runs all tests in that directory
sub runTests($) {
    my $dirName = shift();
    my $fd;
	my $count = 0;
    opendir($fd, $dirName) or die "Could not open tests directory: '$dirName'";
    while (my $file = readdir($fd)) {
        if ($file =~/^(good|bad).*\.fun$/) {
            $count += runTest($dirName, $file);
        }
    }
    closedir($fd);
	return $count;
}

$x=0;
$x += runTests("basic_tests");
$x += runTests("big_tests");
$x += runTests("some-tests");
$x += runTests("more-tests-assign4");
print "You passed $x/77 tests!\n"
