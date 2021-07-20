# eva
Change package paths in stack.yaml

For SATExtensible:

Update runMaxHs in SATLabelingExtensible.hs

For windows with wsl:

output <- readProcess "wsl" ["MAXHS_PATH","testformat.txt"] ""

For linux:
output <- readProcess "maxhs" ["testformat.txt"] ""

assuming maxhs is on the PATH