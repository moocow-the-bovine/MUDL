##-*- Mode: CPerl -*-

## File: MUDL::Make::FieldData.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: make administration: config (pseudo-)fields: data
##======================================================================

package MUDL::Make::FieldData;
#use MUDL::Make::Fields;
use strict;
use Carp;
our @ISA = qw(MUDL::Object);

##======================================================================
## Globals

##======================================================================
## Constructor(s)

## $obj = $class_or_obj->new(%args)
##  + %args:
##      mfields => $mudl_make_fields,   ##-- later accessible in key '_mf' : NOT
##      config  => $base_config,        ##-- later accessible in key '_'
sub new {
  my ($that,%args) = @_;
  my $mf  = $args{mfields} ? $args{mfields} : undef;
  my $cfg = $args{config}  ? $args{config} : undef;
  delete(@args{qw(mfields config)});
  my $data = $that->SUPER::new(
			       #'_mf'=>undef,   ##-- the referencing MUDL::Make::Fields object: NOT
			       #'_'   =>undef,  ##-- the underlying MUDL::Make::Config object
			       %args,
			      );
  $data->populate($cfg,$mf) if (defined($cfg) && defined($mf));
  return $data;
}

## $obj = $obj->populate($cfg,$mf)
##  + $mf is a MUDL::Make::Fields object
##  + %args overrides default values from ($mf) fields of ($cfg)
sub populate {
  my ($data,$cfg,$mf) = @_;
  my ($field);
  foreach $field (@{$mf->xfields}) {
    $data->{$field->{title}}        = $mf->fieldValue($cfg,$field);
    $data->{$field->{title}.':str'} = $mf->fieldValueString($field,$data->{$field->{title}});
  }
  $data->{_} = $cfg;
  return $data;
}

## $obj = $obj->clear()
sub clear {
  my $data = shift;
  %$data = qw();
  return $data;
}

## $obj->DESTROY
##  + break circular references
sub DESTROY { $_[0]->clear; }



1;

__END__

##---------------------------------------------------------------
## Docs
=pod

=head1 NAME

MUDL - MUDL Unsupervised Dependency Learner

=head1 SYNOPSIS

 use MUDL;

=cut

##======================================================================
## Description
=pod

=head1 DESCRIPTION

...

=cut

##======================================================================
## Footer
=pod

=head1 ACKNOWLEDGEMENTS

perl by Larry Wall.

=head1 AUTHOR

Bryan Jurish E<lt>jurish@ling.uni-potsdam.deE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
