emacs configuration
===================

Required Packages
-----------------

Dependencies per language:

| Language   | Packages                  |
|:----------:|---------------------------|
| C          | `clang`, `global` (gtags) |
| Elixir     | `erlang`, `elixir`        |
| Git        | `git`                     |
| JavaScript | `tern`                    |
| Markdown   | `discount`                |


JavaScript auto complete
------------------------

JavaScript auto complete requires a `tern-project` file at the root of
the project. Here is an example for JavaScript on a phoenix framework
project:

```js
{
  "ecmaVersion": 6,
  "libs": [
    "browser"
  ],
  "plugins": {
    "webpack": {
      "configPath": "./assets/webpack.config.js"
    }
  }
}
```

Read the full documentation [here](http://ternjs.net/doc/manual.html#plugins).


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
