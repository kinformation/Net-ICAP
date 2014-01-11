# Net::ICAP::Message -- Message object for ICAP
#
# (c) 2012, Arthur Corliss <corliss@digitalmages.com>
#
# $Id$
#
#    This software is licensed under the same terms as Perl, itself.
#    Please see http://dev.perl.org/licenses/ for more information.
#
#####################################################################

#####################################################################
#
# Environment definitions
#
#####################################################################

package Net::ICAP::Message;

use 5.008003;

use strict;
use warnings;
use vars qw($VERSION @ISA @_properties @_methods);
use Class::EHierarchy qw(:all);
use Paranoid::Debug;
use Net::ICAP::Common qw(:std :debug);

($VERSION) = ( q$Revision: 0.01 $ =~ /(\d+(?:\.(\d+))+)/sm );

@ISA = qw(Class::EHierarchy);

use constant DEF_CHUNK => 1024;

@_properties = (
    [ CEH_RESTR | CEH_ARRAY, '_errors' ],
    [ CEH_RESTR | CEH_SCALAR, '_version', ICAP_VERSION ],
    [ CEH_RESTR | CEH_SCALAR, '_start' ],
    [ CEH_RESTR | CEH_HASH,   '_headers' ],
    [ CEH_RESTR | CEH_SCALAR, '_req-hdr' ],
    [ CEH_RESTR | CEH_SCALAR, '_res-hdr' ],
    [ CEH_RESTR | CEH_SCALAR, '_body' ],
    [ CEH_RESTR | CEH_SCALAR, '_body_type' ],
    [ CEH_RESTR | CEH_SCALAR, '_trailer' ],
    );

@_methods = (
    [ CEH_RESTR, '_getLine' ],
    [ CEH_RESTR, '_putLine' ],
    [ CEH_RESTR, '_parseHeaders' ],
    [ CEH_RESTR, '_genHeaders' ],
    [ CEH_RESTR, '_readChunked' ],
    [ CEH_RESTR, '_writeChunked' ],
    [ CEH_RESTR, '_parseEncap' ],
    [ CEH_RESTR, '_genEncap' ],
    [ CEH_RESTR, '_validHeaders' ],
    );

#####################################################################
#
# Module code follows
#
#####################################################################

sub _initialize ($;@) {

    # Purpose:  Does nothing, base class
    # Returns:  Boolean
    # Usage:    $rv = $obj->_initialization;

    my $obj  = shift;
    my %args = @_;
    my $rv   = 1;

    pdebug( "entering w/$obj and @{[ keys %args ]}", ICAPDEBUG1 );
    pIn();

    # Set internal state if args were passed
    $rv = $obj->version( $args{version} ) if exists $args{version};
    $rv = $obj->setHeaders( %{ $args{headers} } )
        if exists $args{headers} and $rv;
    $rv = $obj->reqhdr( $args{reqhdr} ) if exists $args{reqhdr} and $rv;
    $rv = $obj->reshdr( $args{reshdr} ) if exists $args{reshdr} and $rv;
    $rv = $obj->body( @args{qw(body_type body)} )
        if $rv
            and exists $args{body_type}
            and exists $args{body};
    $rv = $obj->trailer( $args{trailer} ) if exists $args{trailer} and $rv;

    pOut();
    pdebug( "leaving w/rv: $rv", ICAPDEBUG1 );

    return $rv;
}

sub _validHeaders ($) {

    # Purpose:  Returns a list of valid ICAP headers
    # Returns:  Array
    # Usage:    @val = $obj->_validHeaders;

    my $obj = shift;

    return qw(Cache-Control Connection Date Expires
        Pragma Trailer Upgrade Encapsulated);
}

sub error ($;$) {

    # Purpose:  Returns errors or logs a new one
    # Returns:  Array
    # Usage:    $obj->error($msg);
    # Usage:    @errors = $obj->error;

    my $obj = shift;
    my $msg = shift;
    my @rv;

    if ( defined $msg ) {
        $obj->push( '_errors', $msg );
        pdebug( $msg, ICAPDEBUG1 );
    }
    @rv = $obj->property('_errors');

    return @rv;
}

sub reqhdr ($;$) {

    # Purpose:  Gets/sets the request header
    # Returns:  Boolean/String
    # Usage:    $rv     = $obj->reqhdr($text);
    # Usage:    $header = $obj->reqhdr;

    my $obj = shift;
    my ($header) = @_;
    my $rv;

    $rv =
        scalar @_
        ? $obj->property( '_req-hdr', $header )
        : $obj->property('_req-hdr');

    return $rv;
}

sub reshdr ($;$) {

    # Purpose:  Gets/sets the response header
    # Returns:  Boolean/String
    # Usage:    $rv     = $obj->reshdr($text);
    # Usage:    $header = $obj->reshdr;

    my $obj = shift;
    my ($header) = @_;
    my $rv;

    $rv =
        scalar @_
        ? $obj->property( '_res-hdr', $header )
        : $obj->property('_res-hdr');

    return $rv;
}

sub trailer ($;$) {

    # Purpose:  Gets/sets the trailer
    # Returns:  Boolean/String
    # Usage:    $rv      = $obj->trailer($text);
    # Usage:    $trailer = $obj->trailer;

    my $obj = shift;
    my ($trailer) = @_;
    my $rv;

    $rv =
        scalar @_
        ? $obj->property( '_trailer', $trailer )
        : $obj->property('_trailer');

    return $rv;
}

sub body ($;$$) {

    # Purpose:  Gets/sets the body type and content
    # Returns:  Array
    # Usage:    ($type, $body) = $obj->body;
    # Usage:    $rv            = $obj->body($type, $body);

    my $obj = shift;
    my ( $type, $body ) = @_;
    my $rv;

    if (@_) {
        $rv = $obj->property( '_body_type', $type )
            && $obj->property( '_body', $body );
    } else {
        $rv = [ $obj->property('_body_type'), $obj->property('_body') ];
    }

    return ref $rv eq 'ARRAY' ? @$rv : $rv;
}

sub version ($;$) {

    # Purpose:  Gets/sets version
    # Returns:  Boolean/String
    # Usage:    $rv     = $obj->version($version);
    # Usage:    $method = $obj->version;

    my $obj     = shift;
    my $version = shift;
    my $v       = defined $version ? $version : 'undef';
    my ( $r, $rv );

    pdebug( "entering w/$v", ICAPDEBUG1 );
    pIn();

    if ( defined $version ) {

        # Write mode
        if ( $version eq ICAP_VERSION ) {
            $rv = $obj->property( '_version', $version );
        } else {
            pdebug( "invalid version passed: $version", ICAPDEBUG1 );
            $rv = 0;
        }

    } else {

        # Read mode
        $rv = $obj->property('_version');
    }

    $r = defined $rv ? $rv : 'undef';
    pOut();
    pdebug( "leaving w/rv: $r", ICAPDEBUG1 );

    return $rv;
}

sub _getLine ($$) {

    # Purpose:  Retrieves input from passed string ref/object/file handle
    # Returns:  String
    # Usage:    $line = _getLine($fh);
    # Usage:    $line = _getLine(\$text);
    # Usage:    $line = _getLine($io_handle);

    my $obj = shift;
    my $ref = shift;
    my ( $line, @lines, $rv );

    pdebug( "entering w/$ref", ICAPDEBUG4 );
    pIn();

    if ( defined $ref ) {
        if ( ref $ref eq 'SCALAR' ) {
            ( $line, @lines ) = split /\r\n/sm, $$ref;
            $$ref = join "\r\n", @lines;
            $line .= "\r\n";
        } elsif ( ref $ref eq 'GLOB' ) {
            $line = <$ref>;
        } elsif ( $ref->isa('IO::Handle') ) {
            $line = $ref->getline;
        } else {
            $obj->error("don't know what to do with ref $ref");
        }
    } else {
        $obj->error('undefined value passed for reference');
    }

    $rv = defined $line ? $line : 'undef';

    pOut();
    pdebug( "leaving w/rv: $rv", ICAPDEBUG4 );

    return $line;
}

sub _putLine ($$@) {

    # Purpose:  Writes strings to passed string ref/object/file handle
    # Returns:  Boolean
    # Usage:    $rv = _putLine($fh, @lines);
    # Usage:    $rv = _putLine(\$text, @lines);
    # Usage:    $rv = _putLine($io_handle, @lines);

    my $obj   = shift;
    my $ref   = shift;
    my @lines = splice @_;
    my $rv    = 0;

    pdebug( "entering w/$ref, @{[ scalar @lines ]} line(s)", ICAPDEBUG4 );
    pIn();

    if ( defined $ref ) {
        if ( ref $ref eq 'SCALAR' ) {
            $rv = 1;
            $$ref .= join '', @lines;
        } elsif ( ref $ref eq 'GLOB' ) {
            $rv = print $ref join '', @lines;
        } elsif ( $ref->isa('IO::Handle') ) {
            $rv = $ref->print( join '', @lines );
        } else {
            $obj->error("don't know what to do with ref $ref");
        }
    } else {
        $obj->error(
            pdebug( 'undefined value passed for reference', ICAPDEBUG1 ) );
    }

    pOut();
    pdebug( "leaving w/rv: $rv", ICAPDEBUG4 );

    return $rv;
}

sub _parseHeaders (@) {

    # Purpose:  Parses lines of text to extract headers
    # Returns:  Hash
    # Usage:    %headers = _parseHeaders(@lines);

    my @lines = splice @_;
    my ( $text, $line, $k, $v, %headers );

    pdebug( "entering w/@{[ scalar @lines ]} line(s) of text", ICAPDEBUG3 );
    pIn();

    if ( scalar @lines ) {
        $text = join "\r\n", @lines;

        # Fold header continuation lines
        $text =~ s/\r\n\s+/ /smg;

        # Get new set of lines, each one a different header
        @lines = split /\r\n/sm, $text;

        foreach $line (@lines) {
            ( $k, $v ) = ( $line =~ m/^(\S+):\s*(.*?)\s*$/sm );
            $headers{$k} = exists $headers{$k} ? "$headers{$k},$v" : $v;
        }
    }

    pOut();
    pdebug( "leaving w/@{[ scalar keys %headers ]} headers", ICAPDEBUG3 );

    return %headers;
}

sub _genHeaders ($) {

    # Purpose:  Returns header block
    # Returns:  String
    # Usage:    $headers = $obj->_genHeaders;

    my $obj     = shift;
    my %headers = $obj->getHeaders;
    my @valid   = $obj->_validHeaders;
    my $text    = '';
    my ( $h, $v );

    foreach $h (@valid) {
        if ( exists $headers{$h} ) {
            $v = $headers{$h};
            $text .= "$h: $v\r\n" if defined $v and length $v;
        }
    }

    return $text;
}

sub setHeaders ($@) {

    # Purpose:  Sets all valid headers
    # Returns:  Boolean
    # Usage:    $rv = $obj->setHeaders(%headers);

    my $obj     = shift;
    my %headers = splice @_;
    my @valid   = $obj->_validHeaders;
    my $rv      = 1;
    my $k;

    pdebug( 'entering', ICAPDEBUG2 );
    pIn();

    # Validate headers
    foreach $k ( keys %headers ) {
        if ( $k =~ /^X-\w+[\w-]*/sm or grep { $_ eq $k } @valid ) {

            # Chomp header value
            $headers{$k} =~ s/\r\n$//sm;

        } else {

            $rv = 0;
            $obj->error("ignoring invalid header: $k");
            delete $headers{$k};
        }
    }

    # Store anything left
    $obj->property( '_headers', %headers )
        if scalar keys %headers;

    pOut();
    pdebug( "leaving w/rv: $rv", ICAPDEBUG2 );

    return $rv;
}

sub getHeaders ($) {

    # Purpose:  Gets all headers
    # Returns:  Hash
    # Usage:    %headers = $obj->getHeaders;

    my $obj = shift;
    return $obj->property('_headers');
}

sub header ($$;$) {

    # Purpose:  Gets/sets the requested header
    # Returns:  Boolean/String
    # Usage:    $value = $obj->header($name);
    # Usage:    $rv    = $obj->header($name, $value);

    my $obj    = shift;
    my $header = shift;
    my $v      = @_ ? $_[0] : '(omitted)';
    my @valid  = $obj->_validHeaders;
    my ( $value, $rv, $r );

    pdebug( "entering w/$obj, $header, $v", ICAPDEBUG1 );
    pIn();

    if ( $header =~ /^X-\w+[\w-]*/sm or grep { $_ eq $header } @valid ) {

        # Valid header requested
        if (@_) {

            # Write mode
            $value = shift @_;
            if ( defined $value ) {

                # Set mode
                $obj->store( '_headers', $header, $value );
                $rv = 1;

            } else {

                # Delete mode
                $obj->remove( '_headers', $header );
                $rv = 1;
            }

        } else {

            # Read mode
            $rv = $obj->retrieve( '_headers', $header );
        }
    } else {
        $obj->error("invalid header requested: $header");
    }

    $r = defined $rv ? $rv : 'undef';
    pOut();
    pdebug( "leaving w/rv: $r", ICAPDEBUG1 );

    return $rv;
}

sub _readChunked ($$) {

    # Purpose:  Reads chunked-encoded text
    # Returns:  String
    # Usage:    $text = $obj->-readChunked($input);

    my $obj   = shift;
    my $input = shift;
    my $text  = '';
    my ( $line, $chunk, $c );

    pdebug( "entering w/$obj, $input", ICAPDEBUG2 );
    pIn();

    if ( defined( $line = $obj->_getLine($input) ) ) {

        # Get initial chunk size
        ($c) = ( $line =~ /^([0-9a-fA-F]+)\r\n$/sm );
        $c = hex $c;

        OUTER: while ($c) {

            # Read lines until chunk size is met
            $chunk = '';
            while ( length $chunk <= $c ) {
                $line = $obj->_getLine($input);

                unless ( defined $line ) {
                    $obj->error('ran out of text while reading chunks');
                    last OUTER;
                }

                $chunk .= $line;
            }

            # Trim line separator appended to chunk
            $chunk =~ s/\r\n$//sm;

            # Check for chunk size accuracy
            if ( length $chunk == $c ) {

                # Save chunk
                $text .= $chunk;

                # Get next chunk size
                $line = $obj->_getLine($input);
                if ( defined $line ) {
                    ($c) = ( $line =~ /^([0-9a-fA-F]+)\r\n$/sm );
                    $c = hex $c;
                } else {
                    $c = 0;
                    $obj->error('missing next chunk header');
                }

            } else {
                $obj->error( "chunk size mismatch: expected $c "
                        . "recieved @{[ length $chunk ]}" );
                last;
            }
        }
    }

    pOut();
    pdebug( "leaving w/@{[ length $text ]} characters of text", ICAPDEBUG2 );

    return $text;
}

sub _writeChunked ($) {

    # Purpose:  Writes the body in chunked encoding
    # Returns:  String
    # Usage:    $chunked = $obj->_writeChunked;

    my $obj  = shift;
    my $body = $obj->property('_body');
    my ( @segments, $r, $rv );

    pdebug( 'entering', ICAPDEBUG2 );
    pIn();

    if ( defined $body ) {
        while ( defined $body and length $body ) {
            push @segments, substr $body, 0, DEF_CHUNK, '';
        }
        $rv = '';
        foreach (@segments) {
            $rv .= sprintf "%x\r\n", length $_;

            # the following should probably be in the above sprintf,
            # but I'm a little leery of some of the sprintf bugs in the
            # past with binary data...
            $rv .= "$_\r\n";
        }
        $rv .= "0\r\n";
    }

    $r = defined $rv ? "@{[ length $rv ]} characters" : 'undef';
    pOut();
    pdebug( "leaving w/rv: $r", ICAPDEBUG2 );

    return $rv;
}

sub _parseEncap ($$) {

    # Purpose:  Parses message body as per rules in parseEncap
    # Returns:  Boolean
    # Usage:    $rv = $obj->parseEncap($input);

    my $obj        = shift;
    my $input      = shift;
    my $rv         = 1;
    my @ventitites = qw(rep-hdr req-hdr res-body req-body null-body opt-body);
    my ( $encap, @entities, $t, $l, $n, $offset, $line, $text );

    pdebug( 'entering', ICAPDEBUG2 );
    pIn();

    $encap = $obj->header('Encapsulated');
    if ( defined $encap ) {
        @entities = split /\s*,\s*/sm, $encap;

        # Sanity tests:
        #
        #   1) there must be one (and only one) *-body tag as last entity
        $n = scalar grep /^\w+-body=\d+$/sm, @entities;
        unless ( $n == 1 ) {
            $rv = 0;
            $obj->error(
                "invalid number of body entities in Encapsulated: $encap");
        }
        unless ( $entities[$#entities] =~ /^\w+-body=\d+$/sm ) {
            $rv = 0;
            $obj->error( 'last entity must be a body entities in '
                    . "Encapsulated: $encap" );
        }

        #   2) only one req-hdr and/or resp-hdr allowed, but are optional
        $n = scalar grep /^req-hdr=\d+$/sm, @entities;
        unless ( $n <= 1 ) {
            $rv = 0;
            $obj->error("too many req-hedr entities in Encapsulated: $encap");
        }
        $n = scalar grep /^res-hdr=\d+$/sm, @entities;
        unless ( $n <= 1 ) {
            $rv = 0;
            $obj->error("too many res-hedr entities in Encapsulated: $encap");
        }

        #   3) offsets are monotonically increasing
        $n = undef;
        foreach ( map {m/=(\d+)$/sm} @entities ) {
            unless ( !defined $n or $_ > $n ) {
                $rv = 0;
                $obj->error( 'Encapsulated offsets aren\'t monotonically '
                        . "ordered: $encap" );
                last;
            }
            unless ( defined $n ) {
                unless ( $_ == 0 ) {
                    $rv = 0;
                    $obj->error( 'Encapsulated offsets don\'t start at '
                            . "0: $encap" );
                    last;
                }
            }
            $n = $_;
        }

        #   4) no unknown entity types
        if ( scalar grep !m/^(?:re[qs]-hdr|(?:opt|null|re[qs])-body)=\d+$/sm,
            @entities ) {
            $rv = 0;
            $obj->error("invalid entities in Encapsulated: $encap");
        }

        # Read data
        if ($rv) {
            $offset = 0;
            while (@entities) {
                ( $t, $l ) = split /=/sm, shift @entities;
                ( $line, $text ) = ( '', '' );

                if ( $t =~ /-hdr$/sm ) {

                    # Read headers
                    while ( defined( $line = $obj->_getLine($input) ) ) {
                        last if $line eq "\r\n";
                        $text .= $line;
                    }

                    # Store the headers
                    $obj->property( "_$t", $text );

                } elsif ( $t =~ /-body$/sm ) {
                    unless ( $t eq 'null-body' ) {
                        $text = $obj->_readChunked($input);
                        $obj->property( '_body',      $text );
                        $obj->property( '_body_type', $t );
                    }
                }

                # Check the intermediate length
                if (@entities) {
                    ($offset) = ( $entities[0] =~ /=(\d+)$/sm );
                    $l = $l == 0 ? $offset - 2 : $offset - $l - 2;
                    unless ( length $text == $l ) {
                        $rv = 0;
                        $obj->error( "$t length mismatch: expected $l "
                                . 'characters, recieved '
                                . length $text );
                    }
                }
            }
        }

        # Check for trailers for all message bodies
        if ( grep /\b(?:res|req|opt)-body=/sm, $encap ) {
            $line = $obj->_getLine($input);
            if ( defined $line and $line ne "\r\n" ) {
                $text = $line;
                while ( defined( $line = $obj->_getLine($input) ) ) {
                    last if $line eq "\r\n";
                    $text .= $line;
                }
                $obj->property( '_trailer', $text );
            }
        }

    } else {
        $rv = 0;
        $obj->error('no Encapsulated header found');
    }

    pOut();
    pdebug( "leaving w/rv: $rv", ICAPDEBUG2 );

    return $rv;
}

sub _genEncap ($) {

    # Purpose:  Returns a string Encapsulated value based on
    #           stored data
    # Returns:  String
    # Usage:    $encap = $obj->_genEncap;

    my $obj    = shift;
    my $encap  = '';
    my $offset = 0;
    my ( $t, $tt );

    pdebug( 'entering', ICAPDEBUG2 );
    pIn();

    # Check for req-hdr
    $t = $obj->reqhdr;
    if ( defined $t and length $t ) {
        $encap  = "req-hdr=$offset";
        $offset = length($t) + 2;
    }

    # Check for res-hdr
    $t = $obj->reshdr;
    if ( defined $t and length $t ) {
        $encap .= ', ' if length $encap;
        $encap .= "res-hdr=$offset";
        $offset = length($t) + 2;
    }

    # Check for body
    ( $tt, $t ) = $obj->body;
    if ( defined $tt and length $tt and defined $t and length $t ) {
        $encap .= ', ' if length $encap;
        $encap .= "$tt=$offset";
    } else {
        $encap .= ', ' if length $encap;
        $encap .= "null-body=$offset";
    }

    pOut();
    pdebug( "leaving w/rv: $encap", ICAPDEBUG2 );

    return $encap;
}

sub parse ($$) {

    # Purpose:  Reads request/response from input
    # Returns:  Boolean
    # Usage:    $rv = $obj->parse($input);

    my $obj   = shift;
    my $input = shift;
    my ( $start, @headers, $line, $icap_msg );
    my $rv = 0;

    pdebug( "entering w/$obj, $input", ICAPDEBUG1 );
    pIn();

    # Purge internal state
    $obj->purge('_errors');
    $obj->purge('_headers');
    $obj->property( '_start',   undef );
    $obj->property( '_req-hdr', undef );
    $obj->property( '_res-hdr', undef );
    $obj->property( '_body',    undef );
    $obj->property( '_trailer', undef );

    # Read the transaction
    while ( defined( $line = $obj->_getLine($input) ) ) {
        last if $line eq "\r\n";
        $icap_msg .= $line;
    }

    # Process $icap_msg
    if ( length $icap_msg ) {

        # Strip any trailing line terminations
        $icap_msg =~ s/\r\n$//sm;

        # Separate start line from headers
        ( $start, @headers ) = split /\r\n/sm, $icap_msg;

        # Store the start line, headers, and parse Encap data
        $obj->property( '_start', $start );
        $rv = $obj->setHeaders( _parseHeaders(@headers) )
            && $obj->_parseEncap($input);
    }

    pOut();
    pdebug( "leaving w/rv: $rv", ICAPDEBUG1 );

    return $rv;
}

sub generate ($$) {

    # Purpose:  Writes output to the passed reference
    # Returns:  Boolean
    # Usage:    $rv = $obj->generate($ref);

    my $obj = shift;
    my $ref = shift;
    my $rv  = 1;
    my ( $t, $tt, $l );

    pdebug( "entering w/$ref", ICAPDEBUG1 );
    pIn();

    # Print Start/Status line
    $t = $obj->property('_start') . "\r\n";
    $rv = defined $t and length $t ? $obj->_putLine( $ref, $t ) : 0;

    # Print ICAP headers
    if ($rv) {
        $obj->store( qw(_headers Encapsulated), $obj->_genEncap );
        $l = $t = $obj->_genHeaders . "\r\n";
        $rv = defined $t and length $t ? $obj->_putLine( $ref, $t ) : 0;
    }

    # Print req-hdr
    if ($rv) {
        $t = $obj->property('_req-hdr');
        if ( defined $t and length $t ) {
            while ( $t !~ /\r\n\r\n$/sm ) { $t .= "\r\n" }
            $l = $t;
            $rv = $obj->_putLine( $ref, $t );
        }
    }

    # Print res-hdr
    if ($rv) {
        $t = $obj->property('_res-hdr');
        if ( defined $t and length $t ) {
            while ( $t !~ /\r\n\r\n$/sm ) { $t .= "\r\n" }
            $l = $t;
            $rv = $obj->_putLine( $ref, $t );
        }
    }

    # Print body
    if ($rv) {
        ( $tt, $t ) = $obj->body;
        if ( defined $t and length $t ) {
            $l = $t = $obj->_writeChunked;
            $rv = $obj->_putLine( $ref, $t );
        }
    }

    # Print end of message termination
    if ($rv) {
        while ( $l !~ /\r\n\r\n/sm ) {
            $l .= "\r\n";
            $rv = $obj->_putLine( $ref, "\r\n" );
        }
    }

    pOut();
    pdebug( "leaving w/rv: $rv", ICAPDEBUG1 );

    return $rv;
}

1;

__END__

=head1 NAME

Net::ICAP::Message - Base class for requests & responses

=head1 VERSION

$Id$

=head1 SYNOPSIS

    use Net::ICAP::Message;

    $msg    = new Net::ICAP::Message;
    $rv     = $msg->parse($fh);
    @errors = $obj->error;

    $value      = $obj->header($name);
    %headers    = $obj->getHeaders;
    $reqheader  = $obj->reqhdr;
    $resheader  = $obj->reshdr;
    ($type, $body) = $obj->body;
    $version    = $obj->version;

    $msg = Net::ICAP::Message->new(
        headers => {
            Date    => $date,
            Expires => $exp,
            },
        );
    $rv = $obj->body($type, $body);
    $rv = $obj->header($name, $value);
    $rv = $obj->setHeaders(%headers);
    $rv = $obj->reqhdr($text);
    $rv = $obj->reshdr($text);
    $rv = $obj->reshdr($text);
    $rv = $obj->version($version);

    $rv     = $msg->generate($fh);
    @errors = $obj->error;

=head1 DESCRIPTION

This is the base class from which L<Net::ICAP::Request> and
L<Net::ICAP::Response> are built on.  It provides the parsing engine for
extracting the various portions of an ICAP message, namely:

  o Start/Status line
  o ICAP headers
  o HTTP Request/Response headers
  o HTTP Body
  o HTTP trailer

To that end, this class automatically verifies valid ICAP header names (but
not values), as well as extracting and validating the message body as per 
the B<Encapsulated> header's values.  It, obviously, has to know how to
perform chunked encoding & decoding as well.

L<Net::ICAP::Message> is also a message generator ahd, as such, can construct
encoded messages based on the data handed to it.

As a base class this is not meant to be used directly.

=head1 SUBROUTINES/METHODS

=head2 PUBLIC METHODS

The following methods are provided as part of a public API to be used by
external code.

=head3 body

    ($type, $body) = $obj->body;
    $rv            = $obj->body($type, $body);

Sets or gets the body type and content.  Type must be a value consisting of
either B<ICAP_REQ_BODY>, B<ICAP_RES_BODY>, B<ICAP_OPT_BODY>, or
B<ICAP_NULL_BODY>.  The body is assumed to be in raw (B<NOT> chunked
encoded) format.

=head3 error

    $obj->error($msg);
    @errors = $obj->error;

Adds an error message to the internal array, or retrieves an array of string
error messages.  This internal array is reset with every call to the B<parse>
method.

=head3 header

    $value = $obj->header($name);
    $rv    = $obj->header($name, $value);

This method gets or sets a named header.  The header named must be recognized
as a valid ICAP header.  Setting any valid header to a either B<undef> or a
zero-length string will cause the header to be deleted.

=head3 getHeaders

    %headers = $obj->getHeaders;

This method retrieves all ICAP headers set to date.

=head3 setHeaders

    $rv = $obj->setHeaders(%headers);

This method sets the ICAP headers en masse.  All previous headers are purged,
and invalid headers ignored.  This will return false if invalid headers are
declared.  Passing it an empty hash will effectively be a no-op.

=head3 parse

    $rv = $obj->parse($ref);

This method reads from the passed reference and attempts to parse the content.
The passed reference can be either a scalar reference, a file handle, or an
L<IO::Handle> object.  In the case of a scalar reference the read is
destructive in that every line read (terminated by \\r\\n) is deleted from the
strings content.

In the event of a parsing error this method will simply stop reading and
return false.  This means you may have additional input that needs to be
drained before you can effectively issue a response.

=head3 generate

    $rv = $obj->generate($ref);

This method generates an ICAP message based on the internal state data.  It
accepts a reference to write to, which can be either a scalar reference, a
file handle, or an L<IO::Handle> object.  In the case of a scalar reference it
does not erase any previous contents, it appends to it.

=head3 reqhdr

    $rv     = $obj->reqhdr($text);
    $header = $obj->reqhdr;

This method gets or sets the HTTP request header.

=head3 reshdr

    $rv     = $obj->reshdr($text);
    $header = $obj->reshdr;

This method gets or sets the HTTP response header.

=head3 trailer

    $rv     = $obj->reshdr($text);
    $header = $obj->reshdr;

This method gets or sets the HTTP headers which were passed as part of the
HTTP trailer, rather than in the actual header block itself.

=head3 version

    $rv     = $obj->version($version);
    $method = $obj->version;

This method gets or sets the ICAP protocol version string.  It does perform
validation of the string and will ignore any unknown strings.

=head2 RESTRICTED METHODS

To do.

=head1 DEPENDENCIES

=over

=item L<Class::EHierarchy>

=item L<Paranoid>

=back

=head1 BUGS AND LIMITATIONS 

Positively.

=head1 AUTHOR 

Arthur Corliss (corliss@digitalmages.com)

=head1 LICENSE AND COPYRIGHT

This software is licensed under the same terms as Perl, itself. 
Please see http://dev.perl.org/licenses/ for more information.

(c) 2013, Arthur Corliss (corliss@digitalmages.com)

