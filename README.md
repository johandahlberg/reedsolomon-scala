# Reed-Solomon encoding in Scala

[Reed-Solomon error correction](https://en.wikipedia.org/wiki/Reed%E2%80%93Solomon_error_correction) is a
error-correcting technique, that works by adding error correction bits to a data block of a fixed size. It is able to
both recover lost data, and correct errors, provided that the following in-equity holds:

```
2*nbr_erasures + nbr_of_errors <= size_of_dictonary - message_size
```

This library is a re-implementation of the Python library [unireedsolomon](https://github.com/lrq3000/unireedsolomon)
in Scala. Many thanks to Stephen Larroque and Andrew Brown for writing the original Python implementations.

This is still a work in process, and it is still lacking in documentation and performance, but all the basic concepts
should be in place.

## Try it out

To try out the Reed-Solomon encoder, make sure you have [sbt](https://www.scala-sbt.org/) installed. Then compile the
jar-files by running

```
sbt stage
```

You should now have now a set of run script under `./target/universal/stage/bin/`. To try it out, take a file,
encode it and de-decode it with:

```
cat <your file>| ./target/universal/stage/bin/rs-encode | ./target/universal/stage/bin/rs-decode > <your file again>
```

Replace `<your file>` with appropriate values.
