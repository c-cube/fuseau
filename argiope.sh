#!/bin/sh
OPTS="--profile=release --display=quiet"
dune exec $OPTS examples/lwt/argiope/argiope.exe -- $@
