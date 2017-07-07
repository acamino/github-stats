<a href="https://www.haskell.org">
  <img src="https://cdn2.iconfinder.com/data/icons/social-aquiocons/512/Aquicon-Github.png"
       alt="GitHub Stats"
       align="right"
       width="150" />
</a>

# GitHub Stats
[![Build Status](https://travis-ci.org/acamino/github-stats.svg?branch=master)](https://travis-ci.org/acamino/github-stats)

GitHub Stats is a side project which motivation was to learn how to use the [req
package](https://hackage.haskell.org/package/req) by Mark Karpov. It implements
a command line utility to make it easier to use.

To get simple stats from any GitHub organization you should run:

```bash
$ stack exec -- github-stats -o stackbuilders
```

The stats produced will have a histogram format. They will look like the
upcoming example:

```bash
#################################################### Haskell 52
#################### Ruby 20
######### TeX 9
######## JavaScript 8
## Agda 2
## CoffeeScript 2
## HTML 2
## Shell 2
## TypeScript 2
# ATS 1
# Elm 1
# PHP 1
```

## Local Development

1. First clone this repository and `cd` into it.

   ```bash
   $ git clone git://github.com/acamino/github-stats.git
   $ cd github-stats
   ```

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/).

1. Get the appropriate GHC for the project.

   ```bash
   $ stack setup
   ```

1. If you want to launch a REPL and have fun with this program.

   ```bash
   $ stack repl
   ```

## Licence

The code in this repository is licensed under the terms of the
[MIT License](http://www.opensource.org/licenses/mit-license.html).
Please see the [LICENSE](LICENSE) file for details.
