-- Generate all states

-- statesFilter
-- statesFilter [] = []
-- statesFilter [x] = [x]
-- statesFilter [x:xs] = [x:xs''] where
--     xs' = filter xs by noMatch x
--     xs'' = statesFilter xs'
