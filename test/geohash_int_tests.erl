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
    {ok, World} = geohash_int:define_world(-90.0,90.0,
                                           -180.0,180.0,
                                          'N'),
    Latitude = 40.689167,
    Longitude = -74.044444,
    Level = 16,

    Res = geohash_int:encode(World, Latitude, Longitude, Level),
    ?debugFmt("res: ~p~n", [Res]),
    {ok, Hash} = Res,
    % 1707832276 is expected
    ?assertEqual(-(1 bsl (Level*2)) + 6002799572, maps:get(bits,Hash)),
    ?assertEqual(Level, maps:get(level,Hash)).

geohash_int_decode_test() ->
    {ok, World} = geohash_int:define_world(-20037726.37,20037726.37,
                                           -20037726.37,20037726.37,
                                          'N'),
    Hash = #{bits => 225797299132452,
             level=> 24},
    {ok,PosArea} = geohash_int:decode(World, Hash),
    #{xmin := Xmin, xmax := Xmax, ymin := Ymin, ymax := Ymax} = PosArea,
    Latitude = 9741705.20,
    Longitude = 5417390.90,
    Level = 24,
    ?debugFmt("position area: ~p, point: latitude=~p, longitude=~p", [PosArea,Latitude,Longitude]),
    ?assert(Xmin =< Longitude),
    ?assert(Longitude =< Xmax),
    ?assert(Ymin =< Latitude),
    ?assert(Latitude =< Ymax).

geohash_int_decode2_test() ->
    {ok, World} = geohash_int:define_world(-90.0,90.0,
                                           -180.0,180.0,
                                          'N'),
    Latitude = 40.689167,
    Longitude = -74.044444,
    Level = 16,

    Hash = #{bits => 1707832276,
             level=> Level},
    {ok,PosArea} = geohash_int:decode(World, Hash),
    #{xmin := Xmin, xmax := Xmax, ymin := Ymin, ymax := Ymax} = PosArea,
    ?debugFmt("position area: ~p, point: latitude=~p, longitude=~p", [PosArea,Latitude,Longitude]),
    ?assert(Xmin =< Longitude),
    ?assert(Longitude =< Xmax),
    ?assert(Ymin =< Latitude),
    ?assert(Latitude =< Ymax).

geohash_int_decode_leftbottom_n_test() ->
    {ok, World} = geohash_int:define_world(-90.0,90.0,
                                           -180.0,180.0,
                                          'N'),
    Hash0 = #{bits => 0,
             level=> 1},
    {ok,PosArea0} = geohash_int:decode(World, Hash0),
    ?assertMatch(#{xmin:=-180.0,xmax:=0.0,ymin:=-90.0,ymax:=0.0}, PosArea0),

    Hash1 = #{bits => 1,
             level=> 1},
    {ok,PosArea1} = geohash_int:decode(World, Hash1),
    ?assertMatch(#{xmin:=-180.0,xmax:=0.0,ymin:=0.0,ymax:=90.0}, PosArea1),

    Hash2 = #{bits => 2,
             level=> 1},
    {ok,PosArea2} = geohash_int:decode(World, Hash2),
    ?assertMatch(#{xmin:=0.0,xmax:=180.0,ymin:=-90.0,ymax:=0.0}, PosArea2),

    Hash3 = #{bits => 3,
             level=> 1},
    {ok,PosArea3} = geohash_int:decode(World, Hash3),
    ?assertMatch(#{xmin:=0.0,xmax:=180.0,ymin:=0.0,ymax:=90.0}, PosArea3).

geohash_int_decode_leftbottom_z_test() ->
    {ok, World} = geohash_int:define_world(-90.0,90.0,
                                           -180.0,180.0,
                                          'Z'),
    Hash0 = #{bits => 0,
             level=> 1},
    {ok,PosArea0} = geohash_int:decode(World, Hash0),
    ?assertMatch(#{xmin:=-180.0,xmax:=0.0,ymin:=-90.0,ymax:=0.0}, PosArea0),

    Hash1 = #{bits => 1,
             level=> 1},
    {ok,PosArea1} = geohash_int:decode(World, Hash1),
    ?assertMatch(#{xmin:=0.0,xmax:=180.0,ymin:=-90.0,ymax:=0.0}, PosArea1),

    Hash2 = #{bits => 2,
             level=> 1},
    {ok,PosArea2} = geohash_int:decode(World, Hash2),
    ?assertMatch(#{xmin:=-180.0,xmax:=0.0,ymin:=0.0,ymax:=90.0}, PosArea2),

    Hash3 = #{bits => 3,
             level=> 1},
    {ok,PosArea3} = geohash_int:decode(World, Hash3),
    ?assertMatch(#{xmin:=0.0,xmax:=180.0,ymin:=0.0,ymax:=90.0}, PosArea3).

decode_doubleprecision1_test() ->
    {ok, World} = geohash_int:define_world(-20037726.37,20037726.37,
                                           -20037726.37,20037726.37,
                                          'N'),
    Longitude = -1456662.0360231996,
    Latitude = -17004060.830659393,
    Level = 7,
    {ok,Hash} = geohash_int:encode(World, Latitude, Longitude, Level),
    {ok,PosArea} = geohash_int:decode(World, Hash),
    ?assertMatch(#{xmin:=-1565447.37265625,xmax:=-1252357.898125,ymin:=-17219921.09921875,ymax:=-16906831.6246875}, PosArea).

decode_doubleprecision2_test() ->
    {ok, World} = geohash_int:define_world(-20037726.37,20037726.37,
                                           -20037726.37,20037726.37,
                                          'Z'),
    Longitude = 4479913.102077536,
    Latitude = 8698627.278478492,
    Level = 8,
    {ok,Hash} = geohash_int:encode(World, Latitude, Longitude, Level),
    {ok,PosArea} = geohash_int:decode(World, Hash),
    ?assertMatch(#{xmin:=4383252.6434375,xmax:=4539797.380703125,ymin:=8609960.549609374,ymax:=8766505.286875}, PosArea).

get_neighbors_test() ->
    {ok, World} = geohash_int:define_world(-20037726.37,20037726.37,
                                           -20037726.37,20037726.37,
                                          'N'),
    Hash = #{bits => 225797299132452, level => 24},
    {ok, Neighbors} = geohash_int:get_neighbors(World, Hash),
    ?assertMatch(#{east:=225797299132454, west:=225797299132430, south:=225797299132449, north:=225797299132453,
                   northwest:=225797299132431, northeast:=225797299132455,
                   southeast:=225797299132451, southwest:=225797299132427},
                 Neighbors).

get_neighbors1_test() ->
    {ok, World} = geohash_int:define_world(-90.0,90.0,
                                           -180.0,180.0,
                                          'N'),
    Longitude = 100.0,
    Latitude = -10.0,
    Level = 3,
    {ok,Hash} = geohash_int:encode(World, Latitude, Longitude, Level),
    ?assertEqual(3, maps:get(level,Hash)),
    ?assertEqual(4#231, maps:get(bits,Hash)),

    {ok, Neighbors} = geohash_int:get_neighbors(World, Hash),

    ?assertMatch(#{east:=4#233, west:=4#213, south:=4#230, north:=4#320,
                   northwest:=4#302, northeast:=4#322,
                   southeast:=4#232, southwest:=4#212},
                 Neighbors).
    
get_neighbors2_test() ->
    {ok, World} = geohash_int:define_world(-90.0,90.0,
                                           -180.0,180.0,
                                          'Z'),
    Longitude = 100.0,
    Latitude = -10.0,
    Level = 3,
    {ok,Hash0} = geohash_int:encode(World, Latitude, Longitude, Level),
    ?assertEqual(Level, maps:get(level,Hash0)),
    ?assertEqual(4#132, maps:get(bits,Hash0)),

    Hash = #{bits => 4#132, level => Level},
    {ok, Neighbors} = geohash_int:get_neighbors(World, Hash),

    ?assertMatch(#{east:=4#133, west:=4#123, south:=4#130, north:=4#310,
                   northwest:=4#301, northeast:=4#311,
                   southeast:=4#131, southwest:=4#121},
                 Neighbors).
    
