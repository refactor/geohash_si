-module(geohash_int_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% EUnit tests
%% ===================================================================

basic_test() ->
    {ok, World} = geohash_int:define_world(20037726.37,-20037726.37,
                                           -20037726.37,20037726.37,
                                          'N'),
    Latitude = 9741705.20,
    Longitude = 5417390.90,
    Level = 24,

    Res = geohash_int:encode(World, Latitude, Longitude, Level),
    ?debugFmt("res: ~p~n", [Res]),
    {ok, Hash} = Res,
    ?assertEqual(225797299132452, maps:get(bits,Hash)),
    ?assertEqual(24, maps:get(level,Hash)).

