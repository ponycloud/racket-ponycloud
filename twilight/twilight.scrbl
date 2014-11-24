#lang scribble/manual

@require[(for-label racket twilight libvirt)]

@title{Twilight: Host Management Agent}
@author+email["Jan Dvořák" "mordae@anilinux.org"]

The Twilight agent is responsible for realizing the configuration from
the Sparkle controller and sending state information back.


@defmodule[twilight]

This module is not intended to be integrated into anything.
It represents the bulk of a standalone application.


@defclass[twilight% object% ()]{
  @defconstructor[((connect-to string?))]{
    Create new @racket[twilight%] instance instructed to communicate
    with a @tt{Sparkle} daemon behind @tt{ZeroMQ} endpoint specified
    by the supplied @racket[connect-to] string.

    Please note that there must be an open
    @racket[current-libvirt-connection] to @racket["qemu:///system"]
    for agent to properly initialize and work.
  }

  @defmethod[(get-evt) evt?]{
    Produce an infinite event that can be supplied to @racket[sync] in
    order to communicate with @tt{Sparkle} and modify the host system.
  }
}

@; vim:set ft=scribble sw=2 ts=2 et:
