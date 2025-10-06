-module(test_key).
-export([test_between/0]).

%% Test the key:between/3 function with various cases
test_between() ->
    io:format("~n=== Testing key:between/3 ===~n"),
    
    % Normal case: From < To
    io:format("~nNormal case (From < To):~n"),
    test_case(5, 2, 7, true, "5 is in (2,7]"),
    test_case(2, 2, 7, false, "2 is NOT in (2,7] (exclusive lower bound)"),
    test_case(7, 2, 7, true, "7 is in (2,7] (inclusive upper bound)"),
    test_case(1, 2, 7, false, "1 is NOT in (2,7]"),
    test_case(8, 2, 7, false, "8 is NOT in (2,7]"),
    
    % Wrap-around case: From > To
    io:format("~nWrap-around case (From > To):~n"),
    test_case(10, 7, 2, true, "10 is in (7,2] wrap-around"),
    test_case(1, 7, 2, true, "1 is in (7,2] wrap-around"),
    test_case(2, 7, 2, true, "2 is in (7,2] wrap-around (inclusive upper)"),
    test_case(7, 7, 2, false, "7 is NOT in (7,2] (exclusive lower bound)"),
    test_case(5, 7, 2, false, "5 is NOT in (7,2]"),
    
    % Full ring case: From == To
    io:format("~nFull ring case (From == To):~n"),
    test_case(5, 3, 3, true, "5 is in full ring (3,3]"),
    test_case(1, 3, 3, true, "1 is in full ring (3,3]"),
    test_case(3, 3, 3, false, "3 is NOT in (3,3] (the only excluded point)"),
    
    % Real scenario from the bug
    io:format("~nReal scenario (IDs: 1, 2, 3):~n"),
    test_case(3, 2, 1, true, "Is 3 between (2,1]? Should be YES for ring to work"),
    test_case(1, 3, 2, true, "Is 1 between (3,2]? Should be YES"),
    test_case(2, 1, 3, true, "Is 2 between (1,3]? Should be YES"),
    
    io:format("~n=== All tests completed ===~n"),
    ok.

test_case(Key, From, To, Expected, Description) ->
    Result = key:between(Key, From, To),
    Status = if
        Result =:= Expected -> "PASS";
        true -> "FAIL"
    end,
    io:format("  [~s] ~s (got ~w, expected ~w)~n", 
              [Status, Description, Result, Expected]).
