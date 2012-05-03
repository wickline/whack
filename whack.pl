#!/usr/bin/env perl
use strict;
use warnings;

use File::Next;
use Getopt::Long;
use List::Util qw( max sum );
use PPI::Cache;
use PPI;
use Try::Tiny;

GetOptions(
    'details'       => \( my $Should_show_score_details      ),
    'G=s'           => \( my $Path_regex = '\.(?:pm|pl|t)\z' ),
    'group-by-path' => \( my $Group_by_path = 0              ),
    'ppi-cache=s'   => \( my $PPI_cache                      ),
    'raw'           => \( my $Should_show_raw_metrics        ),
    'sub=s'         => \( my $Subroutine_name                ),
    'top=i'         => \( my $Top = 1                        ),
    'verbose'       => \( my $Should_use_verbose_output = 0  ),
);

main();
exit; # or "way out" for those in the UK

=pod for you_dear_reader

    whack [options] [paths]

    identifies perl subroutines in need of refactoring

    Example:

        # the top 5 subs to fix in foo-related files:
        whack.pl --top=5 -G='[fF]oo' lib t

    When working to improve code in some of the dustier
    corners of your repo, it can be difficult to know where
    to start. It can also be difficult to know when to stop.
    This script helps you to know where to start by helping
    you to select one or more subs in need of attention. It
    helps you know when to stop because you can decide in
    advance that you'll make them "at least as good as that
    other subroutine". Why not assign one team member each
    week or month or whatever to improve the top five subs
    such that they fall out of the top ten?

    Note that this script uses PPI and works only on perl
    scripts, modules, tests, etc. PPI takes a bit of time
    too, so if running this on an entire large repo, you
    may want to multitask or get some coffee.

    By default, searches recursively for perl files in . and
    identifies the subroutine most likely to invoke confusion
    in the minds of maintainers using such heuristics as the
        length of the subroutine
        number of conditional/loop blocks in the subroutine
        depth of block nesting within the subroutine
    
    The idea is that you would decompose this subroutine into
    more maintainable units until it was no longer the worst
    offender (or continue further until you were satisfied).
    Then you would find the next subroutine.

    Note that even with the --top=N option, the output will
    only include subroutines which are worse than average.
    If you somehow manage to factor your code into a bunch
    of one line subroutines containing no blocks, then all
    of your subroutines will have the same score and none
    of them will be identified as the next whack target.

    If no paths are provided, searches in . recursively. The
    script skips similar files to what ack skips (swap files,
    VCS directories, etc) and identifies perl files by their
    file extensions currently (.pl, .pm, .t).

    -G=perl_regex
        skip files unless they match this regex
        (-G is an ack-compatible option name)
        This value defaults to this regex:
            \.(?:pm|pl|t)\z
        WARNING: it is possible to construct
        perl regular expressions that do very
        evil things. I make no attempt to look
        out for aperformant regexes or regexes
        which do Evil Things. Go ahead and shoot
        your foot if you like.

    --top=N
        defaults to show --top=1
        you can show top ten or whatever you like.
        For N>1, rankings are included in the output
        and #1 is the subroutine in greatest need of
        your attention.

    --group-by-path
        if showing top N>1, you may prefer to see
        all of the subs organized by path so you
        can target one file at a time. By default,
        subroutines will be listed in rank order.
        Grouping by path allows for more consice
        output as paths need not be repeated.

    --details
        include subroutine score details instead of
        just the rankings. The subroutine score is
        the average of its individual metric scores.
        The individual metric score is the number of
        standard deviations from the average raw
        score of inspected subroutines for a metric.
        The raw score is something like line count
        or block count or block nesting depth.

        It is possible for a bad score in one metric
        to be offset by good scores in other metrics
        when they are averaged to determine overall
        score for the subroutine.

        NOTE: because these are relative values, you
        may see when listing the top N that working
        on the top offender *raises* the scores of
        the other nine. This is because your work
        creates multiple small subroutines which
        lowers the average and raises the scores of
        the other subroutines you are *not* editing.
        For this reason, we default to showing just
        the rankings and not the scores.

    --raw
        implies --details, but just shows the raw
        metrics (not the score or the per-metric
        detail scores). This also supresses the
        display of the rank. --raw is most likely
        useful when you want to repeatedly run the
        script to see how much you've improved a
        specific subroutine. See also --sub below.

    --sub
        to specify the name of the sub for which you
        wish to see output. This can make sense in the
        case of --raw where you are working on some
        improvements for just one sub, or if you are
        comparing different versions of the same sub
        in different files.

    --ppi-cache=path_to_dir
        to use a cache to speed up PPI

    --verbose
        warn about issues such as
            failed to find path specified on cmd line
            files which contain no subs

    The script currently considers the following metrics

        line count
            roughly the number of non-comment/pod lines
            of code within the subroutine. Yes long strings
            will count against you here. Move them into
            config, database, templates, etc.

        block count
            roughly the count of blocks within the
            subroutine (whether nested or not)

        block nesting
            roughly the depth of block nesting within
            the subroutine (nested conditionals/loops)

    These metrics will tend to correlate and no attempt
    is made to sort that out. If you have a very very
    long sub with lots and lots of deeply nested loops
    and conditionals, you'll get a much worse score. This
    is considered a feature.

    The name whack is intended to evoke the following

        "Dude, that's whack!"  (crazy or ridulous subs)
        beat/strike/hit        (provides a target to hit)
        whack-a-mole           (fix one, another appears)

    Thanks to Grant Street Group for allowing this script
    to be open sourced. It was initially developed using
    GSG time and equipment.

    whack.pl is licensed under a
        Creative Commons Attribution 3.0 Unported License
        http://creativecommons.org/licenses/by/3.0/
    and is based on a work at
        https://github.com/wickline/whack

    TODO:
        --help
        real pod
        determine future directions. some ideas:
            use PPI::Find instead of cache-in-the-hash
            oo instead of passing around state
            break it into modules
            cpan distro like ack
            factor out metrics; pluggable like perl critic
            better re-use of file selection code from ack
=cut

sub main {
    validate_options();
    whack();
}

sub whack {
    my $metrics = {
        'line count'          => \&determine_length_of_element,
        'block count'         => \&determine_block_count_of_element,
        'block nesting depth' => \&determine_block_nesting_depth_of_element,
    };

    my $source_iter = get_sources_from_argv( \@ARGV );

    my ( $num_files_visited, $scores );
    while ( defined ( my $path = $source_iter->() ) ) {
        $num_files_visited++;
        clear_internal_cache();

        # skip files w/o subs or which are too large for PPI to handle
        next unless my $s = score( $metrics, $path );

        $scores->{$path} = $s;
    }
    warn "searched in zero files\n"
        unless $num_files_visited;

    unless ( $Should_show_raw_metrics ) {
        my $aggregates = calc_aggregates_from_scores( $metrics, $scores );
        fill_in_relative_scores( $metrics, $scores, $aggregates );
    }

    output_subar_report( $metrics, $scores );
}

sub validate_options
{
    $Should_show_score_details = 1
        if $Should_show_raw_metrics;

    if ( defined $Top ) {
        die "--top must be a positive integer if provided"
            unless $Top > 0;
    }
    if ( defined $PPI_cache ) {
        die "--ppi-cache must be a directory"
            unless -d $PPI_cache;
        PPI::Cache->new( path => $PPI_cache );
    }
    if ( defined $Path_regex ) {
        warn "an empty -G regex will not exclude any paths"
            unless length $Path_regex;
        try { qr/$Path_regex/ } catch {
            s/ at \S+ line \d+.*//s;
            die( "Invalid -G regex '$Path_regex':\n  $_" );
        }
    }
    if ( $Should_show_raw_metrics && $Top > 1) {
        die "--raw skips score calc. Incompatible w/ --top";
    }
}

sub get_sources_from_argv {
    my ( $argv ) = @_;

    my $real_sources =  [ map {
        my $source = File::Next::reslash( $_ );
        my $exists = -e $source;
        warn "source '$source' not found"
            if !$exists && $Should_use_verbose_output;
        $exists ? ( $source ) : ();
    } @$argv ? @$argv : '.' ];

    my $iterator = File::Next::files({
        file_filter => sub {
            is_searchable_file( $File::Next::name )
            &&
            $File::Next::name =~ qr/$Path_regex/
        },
        descend_filter  => sub { ## shamelessly stole from Ack
            my %ignore_dirs = (
                '.bzr'              => 'Bazaar',
                '.cdv'              => 'Codeville',
                '~.dep'             => 'Interface Builder',
                '~.dot'             => 'Interface Builder',
                '~.nib'             => 'Interface Builder',
                '~.plst'            => 'Interface Builder',
                '.git'              => 'Git',
                '.hg'               => 'Mercurial',
                '.pc'               => 'quilt',
                '.svn'              => 'Subversion',
                _MTN                => 'Monotone',
                blib                => 'Perl module building',
                CVS                 => 'CVS',
                RCS                 => 'RCS',
                SCCS                => 'SCCS',
                _darcs              => 'darcs',
                _sgbak              => 'Vault/Fortress',
                'autom4te.cache'    => 'autoconf',
                'cover_db'          => 'Devel::Cover',
                _build              => 'Module::Build',
            );
            return !exists $ignore_dirs{$_} && !exists $ignore_dirs{$File::Next::dir};
        },
    }, @$real_sources);

    return $iterator;
}

sub is_searchable_file { # shamelessly stole this from ack
    my $filename = shift;

    # If these are updated, update the --help message
    return if $filename =~ /[.]bak$/;
    return if $filename =~ /~$/;
    return if $filename =~ m{^#.*#$}o;
    return if $filename =~ m{^core\.\d+$}o;
    return if $filename =~ m{[._].*\.swp$}o;

    return 1;
}

sub should_show_rankings {
    return 0 if $Should_show_raw_metrics;
    return $Top > 1;
}

sub output_subar_report {
    my ( $metrics, $scores ) = @_;

    my @subpar = extract_details_for_subpar_subs( $scores );
    unless ( @subpar ) {
        print "your subs are all so similar that none stand out as whackable\n";
        return;
    }

    my $path_scores = get_path_scores_from_subpar_list( \@subpar );
    order_the_subpar_list( \@subpar, $path_scores );

    my $column_1_width = max( map {
        my $key = $_;
        map { length( $_->{$key} ) } @subpar
    } qw( path sub_name ));

    my $rank_format = should_show_rankings()
        ? '%' . ( length( $Top ) + 2) . 's'
        : '';

    my $legend = {};
    if ( $Should_show_score_details ) {
        print "\nMetrics:\n";

        my $label = 'a';
        for ( sort keys %$metrics ) {
            $legend->{$label} = $_;
            print "    $label - $_\n";
            $label++;
        }
    }

    my $prior_path = '';
    for my $info ( @subpar ) {
        my $path_to_print = $info->{'path'} eq $prior_path
            ? ''
            : "\n" . $info->{'path'} . "\n"
            ;
        my $score_details = $Should_show_score_details
            ? score_details_for_subpar_info( $metrics, $legend, $info )
            : ''
            ;
        $score_details &&= "  ( $score_details )";
        printf( "%s%${column_1_width}s${rank_format}%s\n",
            $path_to_print,
            $info->{'sub_name'},
            ( $rank_format ? $info->{'rank'} : () ),
            $score_details,
        );
        $prior_path = $info->{'path'};
    }
    print "\n";
}

sub get_path_scores_from_subpar_list {
    my ( $subpar ) = @_;
    my $path_scores;
    $path_scores->{ $_->{'path'} } += $_->{'score'} for @$subpar;
    return $path_scores;
}

sub score_details_for_subpar_info {
    my ( $metrics, $legend, $info ) = @_;
    my $formatted_score = $Should_show_raw_metrics
        ? ''
        : sprintf( 'Score: %.2f;  ', $info->{'score'} )
        ;
    my $format = $Should_show_raw_metrics ? '%s' : '%.2f';
    return $formatted_score . join( '  ', map {
        sprintf( "%s: $format", $_, $info->{'details'}{ $legend->{$_} } );
    } sort keys %$legend);
}

sub order_the_subpar_list {
    my ( $subpar, $path_scores ) = @_;

    return unless $Group_by_path;
        # already sorted in order to support finding top-n

    @$subpar = ( sort {
        $path_scores->{ $b->{'path'} } <=> $path_scores->{ $a->{'path'} }
        ||
        $a->{'path'} cmp $b->{'path'}
        ||
        $b->{'score'} <=> $a->{'score'}
        ||
        $a->{'sub_name'} cmp $b->{'sub_name'}
    } @$subpar );
}

sub extract_details_for_subpar_subs {
    my ( $scores ) = @_;

    my @subpar; # Trivia: this script was originally called subpar.pl
    for my $path ( keys %$scores ) {
        for my $sub_name ( keys %{ $scores->{$path} } ) {
            my $score = $scores->{$path}{$sub_name}{'score'};
            next unless $Should_show_raw_metrics || $score > 0;

            push @subpar, {
                path     => $path,
                sub_name => $sub_name,
                score    => $score,
                details  => ( $Should_show_raw_metrics
                    ? $scores->{$path}{$sub_name}
                    : $scores->{$path}{$sub_name}{'standard_deviations'},
                )
            };
        }
    }

    @subpar = sort {
        ( $Should_show_raw_metrics ? 0 : $b->{'score'} <=> $a->{'score'} )
        ||
        $a->{'path'} cmp $b->{'path'}
        ||
        $a->{'sub_name'} cmp $b->{'sub_name'}
    } @subpar;

    splice( @subpar, $Top )
        if $Top && @subpar > $Top;

    if ( should_show_rankings() ) {
        my $rank = 1;
        $_->{'rank'} = $rank++ for @subpar;
    }

    return @subpar;
}

sub fill_in_relative_scores {
    my ( $metrics, $scores, $aggs ) = @_;
    for my $path ( keys %$scores ) {
        for my $sub_name ( keys %{ $scores->{$path} } ) {
            my @standard_deviations;
            for my $metric ( keys %$metrics ) {
                my $metric_score = $scores->{$path}{$sub_name}{$metric};
                my $offset = $metric_score - $aggs->{'avg'}{$metric};
                my $sds = $aggs->{'stdev'}{$metric}
                    ? $offset / $aggs->{'stdev'}{$metric}
                    : 0
                    ;
                $scores->{$path}{$sub_name}{'standard_deviations'}{$metric} = $sds;
                push @standard_deviations, $sds;
            }
            $scores->{$path}{$sub_name}{'score'} = @standard_deviations
                ? sum( 0, @standard_deviations ) / @standard_deviations
                : 0
                ;
        }
    }
}

sub calc_aggregates_from_scores {
    my ( $metrics, $scores ) = @_;
    my %aggr;
    for my $metric ( keys %$metrics ) {
        my @indiv_scores = map { $_->{$metric} } map { values %$_ } values %$scores;
        next unless @indiv_scores;

        $aggr{'avg'}{$metric} = sum( 0, @indiv_scores ) / @indiv_scores;

        $aggr{'stdev'}{$metric} = (
            sum( 0, (
                map {
                    ( $aggr{'avg'}{$metric} - $_ ) ** 2
                } @indiv_scores
            )) / @indiv_scores
        ) ** 0.5;
    }
    return \%aggr;
}

sub score {
    my ( $metrics, $path ) = @_;
    -f $path or die "path '$path' is not a file";

    my $doc = cleaned_document_from_path( $path );
    return unless $doc;

    return score_dimensions_for_document_at_path( $metrics, $doc, $path );
}

sub dim_summary_by_metric {
    my ( $dim, $metric ) = @_;
    return (
        map {
            sprintf( "%s (%d)", $_, $dim->{$_}->{$metric} );
        } sort {
            $dim->{$b}->{$metric} <=> $dim->{$a}->{$metric}
            || $a cmp $b
        } keys %$dim
    );
}

sub score_dimensions_for_document_at_path {
    my ( $metrics, $doc, $path ) = @_;
    $doc->index_locations();

    my $sub_nodes = $doc->find( sub {
        my $is_match = $_[1]->isa( 'PPI::Statement::Sub' )
            && $_[1]->name()  # skip anon subs
            && $_[1]->block() # skip fwd declarations
            ;
        return $is_match ? 1 : 0;
    });
    warn "failed to ->find() sub nodes in $path " . PPI::Document->errstr()
        unless defined $sub_nodes;

    unless ( $sub_nodes ) {
        warn "zero matches trying to ->find() sub nodes in $path"
            if $Should_use_verbose_output;
        return;
    }

    return({
        map {
            my $sub_node = $_;
            my $sub_name = $sub_node->name();
            $sub_name => {
                map {
                    $_ => $metrics->{$_}->( $sub_node, $path, $sub_name );
                } keys %$metrics
            };
        }
        grep {
            !defined( $Subroutine_name ) || $_->name() eq $Subroutine_name;
        }
        @$sub_nodes
    });
}

sub determine_length_of_element {
    my ( $e, $path, $sub_name ) = @_;
    return(
        $e->last_token()->line_number()
        -
        $e->first_token()->line_number()
    );
}

sub determine_block_nesting_depth_of_element {
    my ( $e, $path, $sub_name ) = @_;
    my $blocks = find_blocks_of_interest( $e, $path, $sub_name );
    return 0 unless $blocks && @$blocks;

    my $max_depth_seen = 0;
    for my $block ( @$blocks ) {
        my $block_depth = 1;
        my $child = $block;
        while ( my $parent = $child->parent() ) {
            $block_depth++;
            $child = $parent;
        }
        $max_depth_seen = $block_depth
            if $block_depth > $max_depth_seen;
    }
    return $max_depth_seen;
}

{
    my %internal_cache;
    sub clear_internal_cache { %internal_cache = () }
    sub find_blocks_of_interest {
        my ( $e, $path, $sub_name ) = @_;
        return $internal_cache{'find_blocks_of_interest'}{"$path$;$sub_name"} ||= [
            grep {
                my $prev_sib = $_->sprevious_sibling();
                not( $prev_sib && $prev_sib->isa( 'PPI::Token::Cast' ) );
            } @{ $e->find( 'PPI::Structure::Block' ) || [] }
        ];
    }
}

sub determine_block_count_of_element {
    my ( $e, $path, $sub_name ) = @_;
    my $blocks = find_blocks_of_interest( $e, $path, $sub_name );
    return 0 unless $blocks && @$blocks;
    return scalar @$blocks;
}

sub cleaned_document_from_path {
    my ( $path ) = @_;
    my $document = PPI::Document->new( $path );
    unless ( $document ) {
        warn "failed to PPI::Document->new( '$path' ) "
            . PPI::Document->errstr();
        return;
    }

    $document->prune( "PPI::Token::$_" ) for qw/ Pod Comment /;

    return PPI::Document->new( \( $document->serialize() ) )
        or warn "failed to PPI::Document->new() the serialized pruned doc "
        . PPI::Document->errstr();
}

__END__
