-module(geohash_int_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% EUnit tests
%% ===================================================================

geohash_int_encode1_test() ->
    {ok, World} = geohash_int:define_world(-20037726.37,20037726.37,
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

geohash_int_encode2_test() ->
    {ok, World} = geohash_int:define_world(-90,90,
                                           -180,180,
                                          'N'),
    Latitude = 40.689167,
    Longitude = -74.044444,
    Level = 16,

    Res = geohash_int:encode(World, Latitude, Longitude, Level),
    ?debugFmt("res: ~p~n", [Res]),
    {ok, Hash} = Res,
    ?assertEqual(-(1 bsl (Level*2)) + 6002799572, maps:get(bits,Hash)),
    ?assertEqual(Level, maps:get(level,Hash)).
