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
