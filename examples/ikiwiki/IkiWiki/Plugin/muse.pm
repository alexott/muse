#!/usr/bin/perl
# Ikiwiki plugin for Emacs Muse.
# Author: Michael Olson
# License: GPLv2 or later
#
# In your ikiwiki.setup file, set the muse_init option to the location
# of the init file for Muse.  Some examples provided in the
# examples/ikiwiki directory are muse-init-simple.el and
# muse-init-project.el.

package IkiWiki::Plugin::muse;

use warnings;
use strict;
use IkiWiki 2.00;
use Encode;
use File::Temp;

sub import {
    hook(type => "getsetup", id => "muse", call => \&getsetup);
    hook(type => "htmlize", id => "muse", call => \&htmlize);
}

sub getsetup () {
    return (
        plugin => {
            safe => 1,
            rebuild => 1,         # format plugin
        },
        muse_init => {
            type => "string",
            example => "~/ikiwiki/muse-init.el",
            description => "the location of your Muse init file",
            safe => 1,
            rebuild => 1,
        },
    );
}

sub htmlize (@) {
    my %params=@_;
    my $content = encode_utf8($params{content});
    my $qname = $params{page};
    $qname =~ s/"/\\"/g;

    my ($fh, $filename) = File::Temp::tempfile();
    print $fh $content;
    close $fh;
    my $qfile = $filename;
    $qfile =~ s/"/\\"/g;
    eval {
        system qw( emacs -q --no-site-file -batch -l ),
          $config{muse_init}, '--eval',
            qq{(muse-ikiwiki-publish-file "$qfile" "$qname")};
        {
            open my $ifh, '<', $filename;
            local $/;
            $content = <$ifh>;
            close $ifh;
        }
        unlink $filename;
    };
    if ($@) {
        unlink $filename;
        die $@;
    }
    return decode_utf8($content);
}

1
