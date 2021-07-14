# Test Trophy in Elm

## Dependencies

- [node v16.1.0][node]
- [elm v0.19][elm-install]
- [nvm][nvm] (optional, used to install node)

## Start
### Install node

```
nvm use && nvm install
```

### Compile code

```
elm make src/GuessNumber.elm --output elm.js
*exo-open index.html
```

\* If you don't have `exo-open` in your `PATH`, simply open the `index.html`
file in your preferred browser.

### Run your tests

```
npm test
```

### Install dependencies

[node]: https://nodejs.org/en/
[nvm]: https://github.com/nvm-sh/nvm
[elm-install]: https://guide.elm-lang.org/install/elm.html
