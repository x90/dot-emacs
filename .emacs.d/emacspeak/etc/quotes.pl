#!/usr/bin/perl -w
#$Id: quotes.pl 4528 2007-05-03 17:23:55Z tv.raman.tv $
use Finance::YahooQuote;
my @symbols = <>;
chomp (@symbols);
my @labels=qw(
              Symbol
              Company
              Last-Price
              Last-Trade-Date
              Last-Trade-Time
              Change
              Percent-Change
              Volume
              Average-Daily-Vol
              Bid
              Ask
              Close
              Open
              Day-Range
              52-Week-Range
              EPS
              P/E
              Dividend-Pay-Date
              Dividend-per-Share
              Dividend-Yield
              Market-Capitalization
              Stock-Exchange
             );

@quotes = getquote @symbols;
print join (", ", @labels ),"\n";
my $sep = q(",");
foreach $q (@quotes ) {
  print "\"",join($sep, @$q),"\"\n";
}
