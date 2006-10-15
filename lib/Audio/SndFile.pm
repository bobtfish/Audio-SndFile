# Audio::SndFile - perl glue to libsndfile
#
# Copyright (C) 2006 by Joost Diepenmaat, Zeekat Softwareontwikkeling
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

package Audio::SndFile;

use 5.008006;
use strict;
use warnings;
use Carp qw(croak);
our $VERSION = '0.01';
use Fcntl;
require XSLoader;
XSLoader::load('Audio::SndFile', $VERSION);

use Audio::SndFile::Constants qw(:all);


my %constanttoname;
for (@Audio::SndFile::Constants::FORMAT_TYPES, @Audio::SndFile::Constants::FORMAT_SUBTYPES, @Audio::SndFile::Constants::ENDIANNESS) {
    no strict 'refs';
    my $c = eval { &{"Audio::SndFile::$_"} };
    if (!$@) {
        $constanttoname{$c} = $_;
    }
}

my %filehandles = ();

sub open {
    my ($class,$mode,$filename,@args) = @_;
    my $nmode;
    my $auto_close_fh = 0;
    if ($mode eq '<') {
        $nmode = SFM_READ;
    }
    elsif ($mode eq '>') {
        $nmode = SFM_WRITE;
    }
    elsif ($mode eq "+<") {
        $nmode = SFM_RDWR;
    }
    my $info;
    if (@args == 1) {
        $info = $args[0];
    }
    else {
        $info = Audio::SndFile::Info->new();
        while (@args) {
            my ($name,$value) = splice @args,0,2;
            $info->$name($value);
        }
    }
    if ($nmode == SFM_WRITE && ! $info->format_check()) {
        croak "Invalid format for writing";
    }
    my $fh;
    if (ref $filename) {
        $fh = $filename;
    }
    else {
        open $fh,$mode,$filename or croak "Can't open $filename with mode $mode: $!";
        $auto_close_fh = 0;
    }
    my $fn = fileno($fh);
    if (! defined($fn)) {
        croak "Cannot get a fileno from filehandle.";
    }
    my $self = $class->open_fd(fileno $fh,$nmode,$info,$auto_close_fh);
    $filehandles{$self} = $fh; # store $fh to prevent it from being closed at the end of scope.
    $self;
}

sub DESTROY {
    my $self = shift;
    $self->close();
    delete $filehandles{$self};
}


for my $s (qw(type subtype channels endianness samplerate seekable sections)) {
    no strict 'refs';
    *{$s} = sub {
        my $self = shift;
        $self->info->$s();
    }
}

for (qw(title copyright software artist comment date)) {
    no strict 'refs';
    *{$_} = sub {
        my $self = shift;
        my $method = @_ ? "set_$_" : "get_$_";
        $self->$method(@_);
    };
}

my %pack = (
   short => "s",
   'int' => "i",
   float => "f",
   double => "d",
);
 
for my $type (keys %pack) {
    no strict 'refs';
    *{"unpack_$type"} = sub {
        my ($self,$len) = @_;
        my $meth = "read_$type";
        my $buff = "";
        my $reallen = $self->$meth($buff,$len);
        return unpack "$pack{$type}$reallen",$buff;
    };
    *{"unpackf_${type}"} = sub {
        my ($self,$len) = @_;
        my $meth = "readf_${type}";
        my $buff = "";
        my $reallen = $self->$meth($buff,$len);
        my $packlen = $reallen * $self->channels;
        return unpack "$pack{$type}$packlen",$buff;
    };
    *{"pack_$type"} = sub {
        my $self = shift;
        my $len = @_;
        my $buff = pack "$pack{$type}$len",@_;
        my $meth = "write_${type}";
        $self->$meth($buff);
    },
    *{"packf_$type"} = sub {
        my $self = shift;
        my $len = @_;
        my $buff = pack "$pack{$type}$len",@_;
        my $meth = "writef_${type}";
        $self->$meth($buff);
    },
 
}


package Audio::SndFile::Info;
use Audio::SndFile::Constants ":all";
use Carp qw(croak);

for (qw(samplerate channels format)) {
    my $get = "get_$_";
    my $set = "set_$_";
    no strict 'refs';
    *{$_} = sub {
        my $self = shift;
        if (@_) {
            $self->$set(@_);
        }
        else {
            $self->$get();
        }
    };
}

sub type {
    my $self = shift;
    my $format = $self->get_format;
    if (@_) {
        my $type = shift;
        $format &= ~ SF_FORMAT_TYPEMASK;
        my $constname = "SF_FORMAT_\U$type";
        no strict 'refs';
        $format |= &$constname();
        $self->set_format($format);
    }
    else {
        my $type = $format & SF_FORMAT_TYPEMASK;
        my $constname = $constanttoname{$type} || croak "Uknown type $type";
        $constname =~ s/SF_FORMAT_//;
        lc($constname);
    }
}

sub subtype {
    my $self = shift;
    my $format = $self->get_format;
    if (@_) {
        my $type = shift;
        $format &= ~ SF_FORMAT_SUBMASK;
        my $constname = "SF_FORMAT_\U$type";
        no strict 'refs';
        $format |= &$constname();
        $self->set_format($format);
    }
    else {
        my $type = $format & SF_FORMAT_SUBMASK;
        my $constname = $constanttoname{$type} || croak "Uknown subtype $type";
        $constname =~ s/SF_FORMAT_//;
        lc($constname);
    }
}

sub endianness {
    my $self = shift;
    my $format = $self->get_format;
    if (@_) {
        my $type = shift;
        $format &= ~ SF_FORMAT_ENDMASK;
        my $constname = "SF_ENDIAN_\U$type";
        no strict 'refs';
        $format |= &$constname();
        $self->set_format($format);
    }
    else {
        my $type = $format & SF_FORMAT_ENDMASK;
        my $constname = $constanttoname{$type} || croak "Uknown endianness $type";
        $constname =~ s/SF_ENDIAN_//;
        lc($constname);
    }
}



1;

__END__

=head1 NAME

Audio::SndFile - Portable reading and writing of audio files

=head1 SYNOPSIS

  use Audio::SndFile;

  my $f = Audio::SndFile->open("<","audiofile.wav");
  my $g = Audio::SndFile->open(">","audiofile.au", type => 'au', 
          subtype => 'pcm_16', channels => 1, endianness => 'file');

  my $buffer = "";
  while ($f->read_int($buffer,1024)) {
     $g->write_int($buffer);
  }

=head1 DESCRIPTION

Audio::SndFile is a perl interface to the sndfile library and provides a portable
API for reading and writing audio data in different formats.

=head1 API

=head2 Constructor

 my $sndfile = Audio::SndFile->open($mode, $file, %options);

Creates an Audio::SndFile object from a file specification.

$mode can be "<" (read), ">" (write) or "+<" (read/write)

$file is a filehandle or filename.

%options: if you're opening a file write-only, you should specify
at least the options "type", "subtype" and "channels". The default
endianness is "file". See also L</FORMATS>

=head2 File info

 my $type       = $sndfile->type;
 my $subtype    = $sndfile->subtype;
 my $endianness = $sndfile->endianness;
 my $channels   = $sndfile->channels;
 my $samplerate = $sndfile->samplerate;
 my $sections   = $sndfile->sections;
 my $bool       = $sndfile->seekable;
 my $frames     = $sndfile->frames;

Read info from an Audio::SndFile object. See also L</FORMATS>

=head2 Read audio data

 my $numsamples = $sndfile->read_TYPE($buffer,$num);

 my $numframes  = $sndfile->readf_TYPE($buffer,$num);

Read max $num samples (single values) or frames (interleaved values; one value for
each channel) from $sndfile into $buffer as a packed
string of native endianness. TYPE may be one of "short", "int", "float" or
"double". Values will be converted if necessary.

Returns the number of samples / frames read. $buffer will be shrunk
or grown accordingly.

 my @values = $sndfile->unpack_TYPE($num);
 my @values = $sndfile->unpackf_TYPE($num);

Same as read_TYPE and readf_TYPE, but returns the values as a list of
scalars.

=head2 Write audio data

 my $num = $sndfile->write_TYPE($buffer);
 my $num = $sndfile->writef_TYPE($buffer);

Write $buffer with packed samples or frames to $sndfile. TYPE may be one of
"short", "int", "float" or "double". Returns the number of frames / samples
written.

 my $num = $sndfile->pack_TYPE(@values);
 my $num = $sndfile->packf_TYPE(@values);

Same as write_TYPE and writef_TYPE but these take a list of values instead of
a packed string.

=head2 Seek

 $sndfile->seek($offset, $whence);

L<perlfunc/seek|seek()> to frame $offset. See also L<Fcntl>.

=head2 Errors

Most methods throw an exception on error, but if you need to know:

 my $enum    = $sndfile->error;
 my $estring = $sndfile->strerror;

Return the last error as a number or string.

=head2 lib_version

 my $libsndfile_version = Audio::SndFile::lib_version;

Version of the libsndfile library linked by the module. Note that if you
update your libsndfile you should also recompile this module if you want
to take advantage of new formats provided by libsndfile.

=head1 FORMATS

The exact list of supported file types are dependend on your libsndfile version.
When building this module it tries to figure out which types are available.
File types that are not supported by your libsndfile at the time of building this
module will not be available. In other words: recompile this module after
upgrading your libsndfile.

Supported file types (when avaiable) in this version of Audio::SndFile are:

wav, aiff, au, raw, paf, svx, nist, voc, ircam, w64, mat4, mat5, pvf, xi, htk, 
sds, avr, wavex, sd2, flac, caf.

Supported subtypes are:

pcm_s8, pcm_16, pcm_24, pcm_32, pcm_u8, float, double, ulaw, alaw, ima_adpcm,
ms_adpcm, gsm610, vox_adpcm, g721_32, g723_24, g723_40, dwvw_12, dwvw_16, dwvw_24,
dwvw_n, dpcm_8, dpcm_16.

These map to SF_FORMAT_$type in the C API.

See L<http://www.mega-nerd.com/libsndfile/api.html#open> for the description of
each (sub)type.

The following endianness specifications are supported:

file, big, little, cpu.

These map to SF_ENDIAN_$endianness in the C API.

Note that not all combinations of type, subtype and endianness is supported.
See also L<http://www.mega-nerd.com/libsndfile/#Features>.

=head1 BUGS & ISSUES.

Currenly there are no I<known> bugs, but this code is new and not very well
tested.

This module does not implement the full libsndfile API. Notably missing is
a decent way of using the sf_command() calls. This will be implemented later.

There is currently no way to read seperate channels into seperate buffers.

=head1 SEE ALSO 

Erik de Castro Lopo's libsndfile page: L<http://www.mega-nerd.com/libsndfile/>

L<Audio::SoundFile> - an old(er) interface to libsndfile. Doesn't build on
my perl and looks incomplete.

=head1 AUTHOR

Joost Diepenmaat, E<lt>joost@zeekat.nlE<gt>. L<http://zeekat.nl>. 

=head1 COPYRIGHT AND LICENSE

B<Note:> The following copyright & license only apply to this perl package
(i.e. the "glue" to libsndfile). See L<http://www.mega-nerd.com/libsndfile/#Licensing>
for the license to libsndfile.

Copyright (C) 2006 by Joost Diepenmaat, Zeekat Softwareontwikkeling

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

