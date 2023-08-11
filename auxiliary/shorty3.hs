main = interact $ unlines . filter ((<20) . length) . lines
