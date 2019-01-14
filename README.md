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

