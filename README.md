<p align="center"><img src="https://raw.githubusercontent.com/evuez/iruka/master/web/images/logo-120x120.png"></p>

# iruka

A file format for flashcards accompanied with a parser and a small web application.

## File format

## Parser

The parser lives in `src/` and `app/`, and can be installed using `make install` (you'll need to install [`stack`](https://docs.haskellstack.org/en/stable/install_and_upgrade/) first).

For now, it only outputs JSON because that's what the web application is using, run `iruka cards.iruka cards.json` to generate a JSON file from an `iruka` file.

## Application

An Elm application, everything related to it is in `web/`.

If you have [`yarn`](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed, it should be as easy as running `make start` to start the local dev server.

---

## Formatting

```
make format
```

## Running the tests

```
make test
```

or

```
make test.watch
```
