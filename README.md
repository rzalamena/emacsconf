emacs configuration
===================

Required Packages
-----------------

Dependencies per language:

| Language   | Packages                           |
|:----------:|------------------------------------|
| bash       | `bash-language-server` (npm)       |
| c          | `clang >= 9`, `global` (gtags)     |
| cmake      | `cmake-language-server` (pip)      |
| elixir     | `erlang`, `elixir`, `elixir-ls`    |
| git        | `git`                              |
| javascript | `typescript-language-server` (npm) |
| markdown   | `discount`                         |
| python     | `python-language-server` (pip)     |
| ruby       | `solargraph` (gem)                 |
| vue        | `vls` (npm/yarn)                   |


Language Servers
----------------

Install the dependencies as shown by the table in `Required Packages` and
don't forget to tell emacs to look for binaries in their installation path:

```el
;; Example of user installed language servers:
(setq exec-path
      (append exec-path '(
                          "/home/user/.npm-packages/bin"    ;; npm/yarn.
                          "/home/user/.gem/ruby/X.Y.Z/bin"  ;; ruby gem.
                          "/home/rzalamena/.local/bin"      ;; python/others.
                          )))
```


Elixir Language Server
----------------------

Download and install somewhere a release of `elixir-ls`:
https://github.com/elixir-lsp/elixir-ls

Configure your emacs variable `lsp-clients-elixir-server-executable` to
point to the generated/installed `language_server.sh` file.


JavaScript/TypeScript/Vue Language Server
-----------------------------------------

`eslint` >= 6 broke compatibility. The easiest fix is just to use version 5:

```sh
npm install -g eslint@5
npm install -g typescript-language-server typescript vls
```

Bash Language Server
--------------------

```sh
npm install -g bash-language-server
```


CMake Language Server
---------------------

```sh
pip install --user cmake-language-server
```


Ruby Language Server
--------------------

To use the ruby language server (solargraph) you need to install two gems:

```sh
# Either install as rvm, user or global.
gem install solargraph rubocop
```

If you are editing a project using the gem hierarchy (e.g. `bin`, `lib`
folders) you are going to need a `<project>.gemspec` file to your workspace
in order for solargraph to find your files.

Here is a sample:

```rb
# frozen_string_literal: true

$LOAD_PATH.unshift "./lib"

Gem::Specification.new do |s|
  s.name = 'project-name'
  s.version = '0.0.1'
  s.license = 'license'
  s.summary = 'summary'
  s.authors = ['author 1']
  s.email = 'your@mail'
  s.files = ['lib/**/*.rb']
  s.executables = ['your-executable]
  # s.extensions = ['ext/<extension>/extconf.rb']
  # s.homepage = 'https://yousite.you'
end
```


Spell Check
-----------

Spell checker configuration has been moved to `custom.el` to allow
changing this setting without creating unstaged changes.

Spell check configuration example for Brazilian Portuguese and American
English:

```el
;; Spell checker: to use this you have installed the `hunspell' package.
(require 'ispell)
(setq ispell-program-name "hunspell"
      ispell-dictionary "en_US,pt_BR")

(ispell-set-spellchecker-params)
(ispell-hunspell-add-multi-dic "en_US,pt_BR")
```


Complex C Projects
------------------

When working with a complex C project (e.g. with command-line definitions
`-DFOO_BAR` or different include paths `-I/new/headers`) you might need
to instruct the compiler/analyzer some additional parameters.

If your compile tools (e.g. Makefile, cmake or other) don't support or
can't generate a compilation database you might try using
[Bear](https://github.com/rizsotto/Bear).

```sh
# Example:
cd your-project
make clean
bear make -j16
# Now you have a `compile_commands.json` file.
```

If flycheck complains about missing header or you need to supply a definition:

```el
((c-mode . (
            ;; Include paths are relative to the open file.
            (flycheck-gcc-include-path . ("." "../" "../secret-headers"))
            (flycheck-clang-include-path . ("." "../" "../secret-headers"))
            ;; Tell the compiler about the makefile guards.
            (flycheck-gcc-args . ("-DHAVE_CONFIG_H" ;; for config.h projects.
                                  "-Wall"
                                  "-Wextra"
                                  "-Wstrict-prototypes"
                                  "-Wmissing-prototypes"
                                  "-Wmissing-declarations"
                                  "-Wshadow"
                                  "-Wpointer-arith"
                                  "-Wconversion"
                                  "-Wpacked"
                                  "-Wswitch-enum"
                                  "-Wimplicit-fallthrough"
                                  ))
            (flycheck-clang-args . ("-DHAVE_CONFIG_H"  ;; for config.h projects.
                                    "-Wall"
                                    "-Wextra"
                                    "-Wstrict-prototypes"
                                    "-Wmissing-prototypes"
                                    "-Wmissing-declarations"
                                    "-Wshadow"
                                    "-Wpointer-arith"
                                    "-Wconversion"
                                    "-Wpacked"
                                    "-Wswitch-enum"
                                    "-Wimplicit-fallthrough"
                                    ))
            )))

```
