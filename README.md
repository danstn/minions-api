# Minions API

Minions API serves bananas to minions

## Setting up

Download and install the haskell-platform https://www.haskell.org/platform/.

If you already installed Haskell and want to remove it - run `sudo uninstall-hs
all --remove`.

Note: brew does not have haskell-platform anymore; brew cask is using an older
version of ghc (7.8), so it is recommended to use the original haskell-platform
from the link above.

## Building

```
./scripts/build.sh
```

## Running

```
cabal run
```

or

```
stack exec minionsApi
```

## Development


_Note_: Use Stack's `file-watch` instead.

Install entr (watch app).

```
brew install entr
```

Watch for changes in `src` and auto -rebuild.

```
./scripts/watch.hs
```

Some curl commands (until we get to integration tests). `request.json` is in the root of the repo.

```
curl -i -X POST -d @minion-post.json http://localhost:8081/minions --header "Content-Type:application/json"
```

```
curl -i -X GET http://localhost:8081/minions
```
