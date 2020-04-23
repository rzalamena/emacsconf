emacs configuration
===================

Required Packages
-----------------

Dependencies per language:

| Language   | Packages                        |
|:----------:|---------------------------------|
| C          | `clang`, `global` (gtags)       |
| cmake      | `cmake-language-server` (pip)   |
| Elixir     | `erlang`, `elixir`, `elixir-ls` |
| Git        | `git`                           |
| JavaScript | `vls` (npm/yarn)                |
| Markdown   | `discount`                      |
| Python     | `python-language-server` (pip)  |
| Ruby       | `solargraph` (gem)              |
| Vue        | `vls` (npm/yarn)                |


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
            (flycheck-gcc-args . ("-std=gnu11"
                                  "-DHAVE_CONFIG_H"
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
            (flycheck-clang-args . ("-std=gnu11"
                                    "-DHAVE_CONFIG_H"
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
