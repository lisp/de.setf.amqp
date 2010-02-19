
DE.SETF.AMQP: How to Build It
============

In principle, `de.setf.amqp` is built with [`asdf`](http://www.common-lisp.net/projects/asdf).


## Instructions

  1. Obtain the required libraries (see [amqp.asd](file://amqp.asd)). The sources are reflected in the respective
system names:
      * [net.common-lisp.usocket](http://common-lisp.net/project/usocket/) : `@ r520`; Take the svn source; 
        If you use `0.4.1`, you will also need `split-sequence`.
      * [net.common-lisp.closer-mop](http://common-lisp.net/project/closer/) : [`@ 0-61`](http://common-lisp.net/project/closer/ftp/closer-mop_0.61.tar.gz)
        (also [0-55](http://common-lisp.net/project/closer/ftp/))
      * [net.common-lisp.bordeaux-threads](http://common-lisp.net/project/bordeaux-threads/) : `@ 0-8-0` (or @ patch 165)
      * [net.common-lisp.alexandria](http://common-lisp.net/projects/alexandria/) : 
      * [de.weitz.cl-ppcre](http://weitz.de/cl-ppcre/) : `@ 2.0.1`
      * [com.b9.puri.ppcre](http://github.com/lisp/com.b9.puri.ppcre) @github/lisp :
         This version modifies the [original](http://puri.b9.com/) to replace the parser with
         a cl-ppcre implementation which supports userinfo and to add an argument to `merge-uri` for non-strict
         scheme merging.
  2. Obtain the `de.setf.amqp` source and that for the `de.setf.utility` library
      * [de.setf.amqp](http://github.com/lisp/de.setf.amqp)
      * [de.setf.utility](http://github.com/lisp/de.setf.utility) :
         This includes the `de.setf.utility.mime` module.
  3. Obtain and load [`asdf`](http://common-lisp.net/projects/asdf/), add the
     [`hierarchical names`](http://github.com/lisp/de.setf.utility/blob/master/asdf/hierarchical-names.lisp) utility.
  4. Place the libraries in a source tree to mirror their global identity as reflected in the required system 
     names, and add the root of this tree to the `asdf` registry. If the central registry entry specifies a logical
     host with binary [mapping](http://github.com/lisp/de.setf.utility/blob/master/pathnames.lisp),
     the hierarchical naming mechanism should suffice to map binaries the specified location.
     The `bordeaux-threads` -> `alexandria` reference is unqualified, and as such, requires additional registration. 
  5. Compile and load as: `(asdf:operate 'asdf:load-op :de.setf.amqp._version_)`

The example initialization [file](./build-init.lisp) was used for CCL and SBCL. Contact [me](mailto:james.anderson@setf.de) should any questions arise.

## Notes

### ASDF

ASDF requires version 1.622 in order to construct correct component pathnames where the system has a logical root pathname.

On OS X, I have observed symbolic registry _finder_ links to fail to work from ccl and sbcl. An `ln -s` in the terminal is required.

### Clozure Common Lisp

Further instructions are in [README-build-ccl.md](./README-build-ccl.md)


### hierarchical system names

 There are, to date, 498 `.asd` files in my combined production and development source trees.
Each system is identified by provenance and found in the file system at a location to be inferred from its identifier. Thus, given the
development source tree [root](./tree.txt)

    /Development/Source/dev/Library/

the pathname for the `de.setf.amqp` system definition is

    /Development/Source/dev/Library/de/setf/amqp/amqp.asd

This convention is supported by the `asdf`
[`hierarchical names`](http://github.com/lisp/de.setf.utility/blob/master/asdf/hierarchical-names.lisp)
extension.
It adds a system search method which parses the system identifier and uses its elements to
prospectively augment locations from the `*central-registry*` with directory paths.
Versions are supported by probing with a wild suffix added to the name of the final directory, and by
electing the most recently modified system file if several versions are found.
A nickname is added to each loaded system to augment an unqualified name with a qualified one.

Where the dependencies in a system definition use unqualified cross-references, this fails.
Thus `alexandria` requires a registry addition to permit the cross-reference from `usocket`.


### logical pathnames

The source and binary file names are expressed as logical pathnames in order that they can be
remapped to new locations for delivered images.

- LIBRARY : locates the root of the source tree
- AMPQ    : locates the root of the specific AMQP source

If asdf output location mapping is disabled, the binary files are stored in `LIBRARY:bin;` and `AMPQ:bin;`.
Once can change this, but changing the logical pathname transaltions.
In order to update the host translations for a relocaed image

    (setf (logical-pathname-translations "AMQP") nil
          (logical-pathname-translations "LIBRARY") nil)
    (asdf:find-system :de.setf.utility)
    (asdf:find-system :de.setf.amqp)


### Macintosh Common Lisp

If the `asdf` version includes mandatory output translations, either disable that or
otherwise ensure that it does not choke on the default binding for `user-homedir-pathname`.

Further instructions are in [README-build-mcl.md](./README-build-mcl.md)


### Steel Bank Common Lisp

Further instructions are in [README-build-sbcl.md](./README-build-sbcl.md)