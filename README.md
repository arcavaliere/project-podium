# project-podium

Project proposal and tracking platform

# Overview

# Usage

## Setup

### Dependencies:

- [lucerne](http://borretti.me/lucerne/)
- [cl-sql](http://clsql.kpe.io/manual/index.html)

### First Time

Install [sbcl](http://www.sbcl.org/) and [quicklisp](https://www.quicklisp.org/beta/)

### Loading the project

I run the following snippet when I start up my [SLIME](https://common-lisp.net/project/slime/) session:

```lisp
(setf ql:*local-project-directories* '("/path/to/project/code"))
(ql:register-local-projects)
(ql:quickload :project-podium)
```

this should get all the project's dependencies installed and loaded.

# License

Copyright (c) 2020 Alexander R Cavaliere

Licensed under the GPLv3 License.
