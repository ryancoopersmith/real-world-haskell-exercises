module Main (main) where

import QC
-- NOTE: this module is not supported anymore; not working :(
import Test.QuickCheck.Batch

options = TestOptions {
    no_of_tests     = 200,
    length_of_tests = 1,
    debug_tests     = False
}

main = do
    runTests "simple" options
        [ run prop_idempotent,
          run prop_sort_model,
          run prop_minimum,
          run prop_maximum,
          run prop_ordered ]

    runTests "complex" options
        [ run prop_permutation,
          run prop_append ]
