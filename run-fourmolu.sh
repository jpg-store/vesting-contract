find ./src | grep -E ".*.hs" | xargs fourmolu -i
find ./tests | grep -E ".*.hs" | xargs fourmolu -i -o -XBangPatterns
find ./src | grep -E ".*.hs" | xargs fourmolu -i -o -XBangPatterns
