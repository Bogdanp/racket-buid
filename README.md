# buid

Universally unique, lexicographically-sortable flake ids for Racket.

## Spec

A BUID is made up of a 40 bit time component and an 88 bit randomness
component.  The time component represents the current number of
centiseconds since the UNIX timestamp `1586026830000`.

BUIDs are represented as 22 character strings where the first 7
characters represent the 0-padded time component encoded in base 62
and the remaining 15 characters represent the 0-padded randomness
component encoded in base 62.

The randomness component must have its most significant bit initially
set to 0 and all ids generated within the same centisecond must
increase monotonically.

In binary, BUIDs are represented as 16 bytes, encoded in network order.

## License

    buid is licensed under the 3-Clause BSD license.
