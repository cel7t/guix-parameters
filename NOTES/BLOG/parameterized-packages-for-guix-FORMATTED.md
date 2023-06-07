title: Parameterized Packages for GNU Guix
date: 2023-06-06 13:37
author: Sarthak Shah
tags: GSoC
---
Hello Guix!
I'm Sarthak and I'll be working on implementing Parameterized Packages 
for GNU Guix as a [Google Summer of Code](https://summerofcode.withgoogle.com/) 
intern under the guidance of Pjotr Prins and Gábor Boskovits.

#### What are Parameterized Packages?

One of the many advantages of [free software](https://www.gnu.org/philosophy/free-sw.html) is the
availability of compile-time options for almost all packages. 
Thanks to its dedication to building all packages from source, 
Guix is one of the few GNU/Linux distributions that can take advantage 
of these compile-time features; in fact, many advanced users such as 
those using Guix on [High-Performance Computing Systems](https://hpc.guix.info) 
and [new ISAs like RISC-V](https://10years.guix.gnu.org/video/gnu-guix-and-the-risc-v-future/) have already been doing this 
by utilizing a feature known as [Package Transformations](https://guix.gnu.org/manual/en/html_node/Package-Transformation-Options.html).

**Parameterized Packages** are a new type of package transformations
that will be able to tweak an even wider array of compile-time
options, such as removing unused dependencies or building a package
with support for just a specific locale.  These will have a wide
variety of applications, ranging from High-Performance Computing to
Embedded Systems and could also help tackle a few of Guix's issues
like large binary sizes and dense dependency graphs.

#### What would these look like?

The *syntax* for parameterized packages is still under heavy
deliberation, however the final syntax will have the following
features: 
- Maintainers will be able to specify what combinations of
parameters a package supports, along with a _default configuration_ 
of parameters for a given package.
- Users will be able to pass parameters they want enabled or disabled 
through --with-parameters which will then get validated against the 
valid combinations specified by maintainers before being run 
- For a given package and a given set of parameters, only those in 
the package's parameter specification will be used 
- Users will be able to specify a global parameter transform that 
will apply to all packages - Packages will be built with the 
_default_ configuration if the global transform creates an 
invalid configuration

#### Potential Problems with Parameterization

### Combinatorial Explosion of Variants 

One of the biggest and most obvious issues with parameters is the 
combinatorial explosion of package variants they will create. One way 
to address this is to use tools to calculate and regulate allowed 
complexity; one such tool could be a function that takes a 
parameter combination specification and returns the number of variants 
it could create.

### Increase in Maintenance Required 

Another concern is that this will lead to an increase in the workload 
of maintainers, who have been generously volunteering their time and 
have a lot of work as is. Hence, we will be treating parameters 
the same way we have been treating other package transformations- 
they are not expected to be stable, and should be used at the 
user's discretion.

### Availability of Substitutes for Variants 
Lastly, parameterization could lead to an exponential increase in 
the number of substitutes build farms will have to create, and 
thus as such there are only plans on building substitutes for 
default and very popular parameter combinations.

#### Other topics under discussion

Some of the other points of discussion with respect to parameters are
- **Scope**: Parameterization has a very wide and overarching scope,
and it would be useful to have guidelines in place for when a
particular property should be considered for parameterization 
- **Syntax**: There are many proposed syntax designs for
parameterization, and more are welcome! The final syntax will most
probably be an amalgamation of the best parts of all proposed designs.
- **Substitutes**: There is a lot of discussion on exactly what
parameter combinations should be considered for substitutes; while it
is obvious that it won't be possible to build _all_ combinations, some
important combinations can and should be considered for having
substitutes built. We could perhaps have a separate category of
parameter combinations that would both receive substitutes and
support, and make these combinations discoverable through the
UI. Another suggestion is to have user-run channels for specific build
combinations, like for example there could be a RISC-V specific
channel supplying substitutes for the users running RISC-V.

If you would like to join the discussion, check out [this mailing list
discussion about this project](https://lists.gnu.org/archive/html/guix-devel/2023-05/msg00156.html),
and also have a look at the [original thread](https://lists.gnu.org/archive/html/guix-devel/2020-11/msg00312.html) about parameterization.

#### Conclusion

Parameters hold the potential to greatly increase Guix's flexibility,
but will also lead to greater complexity. In my opinion, Guix is
uniquely positioned to take full advantage of the customisability
provided by compile-time options while also enjoying relative
stability thanks to its transactional nature.

#### About Me

I'm a student studying Mathematics and ECE at BITS Pilani, and I love
computers and free software. I am the president of my university's
equivalent of a Free Software Advocacy Group, and I am also one of the
system administrators for my university's High-Performance Computing
System. As an advocate for free software and a Lisp user, I naturally
fell in love with GNU Guix when I discovered it. I have also used
Gentoo for some time in the past, which motivated me to try and bring
something similar to USE flags to Guix.  You can find my blog at
[blog.lispy.tech](https://blog.lispy.tech), where I will be frequently
posting updates about the project.

#### About GNU Guix

[GNU Guix](https://guix.gnu.org) is a transactional package manager
and an advanced distribution of the GNU system that [respects user
freedom](https://www.gnu.org/distros/free-system-distribution-guidelines.html).
Guix can be used on top of any system running the Hurd or the Linux
kernel, or it can be used as a standalone operating system
distribution for i686, x86_64, ARMv7, AArch64 and POWER9 machines.

In addition to standard package management features, Guix supports
transactional upgrades and roll-backs, unprivileged package
management, per-user profiles, and garbage collection.  When used as a
standalone GNU/Linux distribution, Guix offers a declarative,
stateless approach to operating system configuration management.  Guix
is highly customizable and hackable through
[Guile](https://www.gnu.org/software/guile) programming interfaces and
extensions to the [Scheme](http://schemers.org) language.
