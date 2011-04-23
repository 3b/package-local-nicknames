## Package Local Nicknames

still in the design stage, you probably don't want to use the code...

(it sort of works on sbcl, but is nowhere near complete, expect it to
break things unpredictably)

## rationale

`cl-opengl` wants the nickname "GL". `CLX/GLX` wants the nickname
"GL".  A hypothetical Parenscript/WebGL wants the nickname "GL". It is
reasonable to want to load more than one of these libraries at once, and
to want to use the nickname `GL:` for more than one of them from different
code, without having to remember to `RENAME-PACKAGE` a bunch of packages
before loading new code (particularly when doing interactive development
on more than one of them at once). Specifying nicknames with the creating
package leads to conflict like these, particularly since there may be
multiple 'obvious' nicknames to define, even if nobody ends up using more
than one of them for a given package.

See also JSON libraries and the `JSON:` nickname, particularly since
they are likely to be indirect dependencies of libraries which could
be useful together, for example a NoSQL lib and a json-rpc lib.

If users can easily add nicknames scoped to a specific package, library
authors might be more willing to drop the convenient-but-conflicting
global nicknames.

### differences from other options:

* [cl-package-aliases](http://www.cliki.net/cl-package-aliases)  
  in `package-local-nicknames`, local nicknames shadow the global
  namespace which makes conflicts much less likely, but adds some edge
  cases (print/read consistency for example) which need thought about
  and worked out

* allegro [hierarchical packages](http://www.franz.com/support/documentation/6.0/doc/packages.htm)  
  If I understand them correctly, hierarchical packages are targeted more
  at use within a group of related packages, while `package-local-nicknames`
  is targeted at making it easier to use third-party libraries, and to
  reducing nickname conflicts between those libraries.

* [conduits](http://www.tfeb.org/lisp/hax.html#CONDUITS)  
  seems more oriented towards fine-grained package manipulation, and
  packages intended to be `:use`d
  (and another example of the problem of specifying nicknames with the
   creating package, since it looks like it has a nickname conflict with
   common-lisp-controller)
