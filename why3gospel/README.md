
This directory contains the OCaml sources of a Why3 plug-in to read GOSPEL
specifications.

More precisely, this plug-in installs a new Why3 format "gospel", mapped
to files with suffix ".mli". When reading a file "f.mli" with this plug-in,
a single Why3 module called "Sig" is built. This way, we can do

  use f.Sig

in a Why3 file and have access to the GOSPEL declarations in "f.mli".
