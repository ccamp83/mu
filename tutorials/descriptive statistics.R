libraries()

#### DESCRIPTIVES ####
descriptives(anscombe)

#### EXAMINE ####
# examine ignores non-numeric columns
# by default, it analyzes all numeric columns
examine(anscombe, all.results = T)

# individual columns can be assessed
examine(anscombe, x4)
examine(anscombe, x4, all.results = T)

#### FREQUENCIES ####
frequencies(anscombe)
