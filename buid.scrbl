#lang scribble/manual

@(require (for-label buid
                     racket/base
                     racket/contract)
          scribble/example)

@title{@tt{buid}: universally unique lexicographically sortable identifiers}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[buid]

This package provides an implementation of flake ids that are
lexicographically-sortable and highly unlikely to collide in a
distributed system.


@section{Spec}

A BUID is made up of a 40 bit time component and an 88 bit randomness
component.  The time component represents the current number of
centiseconds since the UNIX timestamp @tt{1586026830000}.

BUIDs are represented as 22 character strings where the first 7
characters represent the 0-padded time component encoded in base 62
and the remaining 15 characters represent the 0-padded randomness
component encoded in base 62.

The randomness component must have its most significant bit initially
set to 0 and all ids generated within the same centisecond must
increase monotonically.

In binary, BUIDs are represented as 16 bytes, encoded in network order.


@section{Reference}

@defthing[buid/c string?]{
  Represents a BUID string.
}

@defthing[buid-bytes/c bytes?]{
  Represents the binary representation of a BUID.
}

@defproc[(make-buid-factory) (-> (and/c buid/c immutable?))]{
  Returns a function that can be used to generate BUIDs.  Any BUIDs
  generated within the same centisecond by the resulting function will
  increase monotonically.

  The generator functions are thread-safe.
}

@defproc[(buid) (-> (and/c buid/c immutable?))]{
  Generates an BUID.

  @examples[
  #:label #f
    (require buid)

    (for ([_ (in-range 10)]) (displayln (buid)))
  ]
}

@defproc[(buid-time [s buid/c]) exact-nonnegative-integer?]{
  Returns the time component of the BUID @racket[s].
}

@defproc[(buid-posix-time [s buid/c]) exact-nonnegative-integer?]{
  Returns the time component of the BUID @racket[s], adjusted to a
  POSIX timestamp in milliseconds.
}

@defproc[(buid-randomness [s buid/c]) exact-nonnegative-integer?]{
  Returns the random component of the BUID @racket[s].
}

@defproc[(buid->bytes [u buid/c]) buid-bytes/c]{
  Returns the binary representation of the BUID @racket[u].
}

@defproc[(bytes->buid [bs buid-bytes/c]) buid/c]{
  Converts @racket[bs] to a BUID.  Raises an error if the result is
  not a valid BUID.
}
