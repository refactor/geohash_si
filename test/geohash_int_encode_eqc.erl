-module(geohash_int_encode_eqc).
-include_lib("proper/include/proper.hrl").

-include("../src/geohash_int.hrl").

-compile(export_all).

prop_geohash_encode_leftbottom() ->
    ?FORALL({Time, Hash, Level},
            encode_point_in_leftbottom(fun tc_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        #{bits:=HashInt, level:=L} = Hash,
                        equals({L,HashInt}, {Level,0})
                    end))).

prop_geohash_fast_encode_leftbottom() ->
    ?FORALL({Time, Hash, Level}, encode_point_in_leftbottom(fun tc_fast_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("fast_encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        #{bits:=HashInt, level:=L} = Hash,
                        equals({L,HashInt}, {Level,0})
                    end))).

prop_geohash_encode_righttop() ->
    ?FORALL({Time, Hash, Level, RightTop}, encode_point_in_righttop(fun tc_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End4 = (1 bsl (Level*2)) - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        ?WHENFAIL(io:format("encode(~p, ~p)=~p, != ~p~n",
                                            [RightTop,Level,HashInt,End4]),
                                  {L,HashInt} == {Level,End4})

                    end))).

prop_geohash_fast_encode_righttop() ->
    ?FORALL({Time,Hash,Level,RightTop}, encode_point_in_righttop(fun tc_fast_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("fast_encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End4 = (1 bsl (Level*2)) - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        ?WHENFAIL(io:format("encode(~p, ~p)=~p, != ~p~n",
                                            [RightTop,Level,HashInt,End4]),
                                  {L,HashInt} == {Level,End4})

                    end))).

prop_geohash_encode_center_leftbottom() ->
    ?FORALL({Time, Hash, Level}, encode_point_in_center_leftbottom(fun tc_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End1 = (1 bsl ((Level-1) * 2)) - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        equals({L,HashInt}, {Level,End1})
                    end))).

prop_geohash_fast_encode_center_leftbottom() ->
    ?FORALL({Time, Hash, Level}, encode_point_in_center_leftbottom(fun tc_fast_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("fast_encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End1 = (1 bsl ((Level-1) * 2)) - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        equals({L,HashInt}, {Level,End1})
                    end))).

prop_geohash_encode_center_righttop() ->
    ?FORALL({Time, Hash, Level}, encode_point_in_center_righttop(fun tc_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End3 = (1 bsl ((Level-1) * 2)) * 3 - 1,
                        Start4 = End3 + 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        equals({L,HashInt}, {Level,Start4})
                    end))).

prop_geohash_fast_encode_center_righttop() ->
    ?FORALL({Time, Hash, Level}, encode_point_in_center_righttop(fun tc_fast_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("fast_encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End3 = (1 bsl ((Level-1) * 2)) * 3 - 1,
                        Start4 = End3 + 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        equals({L,HashInt}, {Level,Start4})
                    end))).

prop_geohash_encode_random_position() ->
    ?FORALL({Time, Hash, Level}, encode_point_in_randomly_area(fun tc_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        Start1 = 0,
                        End4 = (1 bsl (Level*2)) - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        Level == L andalso Start1 =< HashInt andalso HashInt =< End4
                    end))).

prop_geohash_fast_encode_random_position() ->
    ?FORALL({Time, Hash, Level}, encode_point_in_randomly_area(fun tc_fast_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        Start1 = 0,
                        End4 = (1 bsl (Level*2)) - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        Level == L andalso Start1 =< HashInt andalso HashInt =< End4
                    end))).

prop_geohash_encode_position_in_2nd_quadrant() ->
    ?FORALL({Time, Hash, Level,Mode}, encode_point_in_2nd_quadrant(fun tc_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End1 = (1 bsl ((Level-1) * 2)) - 1,
                        Start2 = End1 + 1, End2 = (1 bsl ((Level-1) * 2)) * 2 - 1,
                        Start3 = End2 + 1, End3 = (1 bsl ((Level-1) * 2)) * 3 - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        L == Level
                        andalso (((Mode == 'N') and (Start2 =< HashInt andalso HashInt =< End2))
                                 orelse
                                 ((Mode == 'Z') and (Start3 =< HashInt andalso HashInt =< End3)))
                    end))).

prop_geohash_fast_encode_position_in_2nd_quadrant() ->
    ?FORALL({Time,Hash,Level,Mode}, encode_point_in_2nd_quadrant(fun tc_fast_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End1 = (1 bsl ((Level-1) * 2)) - 1,
                        Start2 = End1 + 1, End2 = (1 bsl ((Level-1) * 2)) * 2 - 1,
                        Start3 = End2 + 1, End3 = (1 bsl ((Level-1) * 2)) * 3 - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        L == Level
                        andalso (((Mode == 'N') and (Start2 =< HashInt andalso HashInt =< End2))
                                 orelse
                                 ((Mode == 'Z') and (Start3 =< HashInt andalso HashInt =< End3)))
                    end))).

prop_geohash_encode_position_in_4th_quadrant() ->
    ?FORALL({Time, Hash, Level,Mode}, encode_point_in_4th_quadrant(fun tc_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End1 = (1 bsl ((Level-1) * 2)) - 1,
                        Start2 = End1 + 1, End2 = (1 bsl ((Level-1) * 2)) * 2 - 1,
                        Start3 = End2 + 1, End3 = (1 bsl ((Level-1) * 2)) * 3 - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        L == Level
                        andalso (((Mode == 'N') and (Start3 =< HashInt andalso HashInt =< End3))
                                 orelse
                                 ((Mode == 'Z') and (Start2 =< HashInt andalso HashInt =< End2)))
                    end))).

prop_geohash_fast_encode_position_in_4th_quadrant() ->
    ?FORALL({Time,Hash,Level,Mode}, encode_point_in_4th_quadrant(fun tc_fast_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End1 = (1 bsl ((Level-1) * 2)) - 1,
                        Start2 = End1 + 1, End2 = (1 bsl ((Level-1) * 2)) * 2 - 1,
                        Start3 = End2 + 1, End3 = (1 bsl ((Level-1) * 2)) * 3 - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        L == Level
                        andalso (((Mode == 'N') and (Start3 =< HashInt andalso HashInt =< End3))
                                 orelse
                                 ((Mode == 'Z') and (Start2 =< HashInt andalso HashInt =< End2)))
                    end))).

encode_point_in_leftbottom(Encode) ->
    ?LET({Point,Level,World}, geohash_eqc_utils:generate_point_at_leftbottem_corner(),
         begin
             {Longitude, Latitude} = Point,
             {Time, Hash} = Encode(World, Latitude, Longitude, Level),
             {Time, Hash, Level}
         end).

encode_point_in_righttop(Encode) ->
    ?LET({Point,Level,World}, geohash_eqc_utils:generate_point_at_righttop_corner(),
         begin
             {Longitude, Latitude} = Point,
             %{Time, {ok, Hash}} = timer:tc(geohash_int, encode, [World,Latitude,Longitude,Level]),
             {Time, Hash} = Encode(World, Latitude, Longitude, Level),
             {Time, Hash, Level, Point}
         end).

encode_point_in_center_leftbottom(Encode) ->
    ?LET({PointAtCenterLB,Level,World}, geohash_eqc_utils:generate_point_at_center_leftbottom(),
         begin
             {Longitude, Latitude} = PointAtCenterLB,
             {Time, Hash} = Encode(World, Latitude, Longitude, Level),
             {Time, Hash, Level}
         end).

encode_point_in_center_righttop(Encode) ->
    ?LET({PointAtCenterRT,Level,World}, geohash_eqc_utils:generate_point_at_center_righttop(),
         begin
             {Longitude, Latitude} = PointAtCenterRT,
             {Time, Hash} = Encode(World, Latitude, Longitude, Level),
             {Time, Hash, Level}
         end).

encode_point_in_randomly_area(Encode) ->
    ?LET({{Longitude,Latitude},Level,World}, geohash_eqc_utils:generate_point_randomly(),
         begin
             {Time, Hash} = Encode(World,Latitude,Longitude,Level),
             {Time, Hash, Level}
         end).

encode_point_in_2nd_quadrant(Encode) ->
    ?LET({PointAtSecondQuadrant,Level,World}, geohash_eqc_utils:generate_point_at_2nd_quadrant(),
         begin
             {Longitude, Latitude} = PointAtSecondQuadrant,
             {Time, Hash} = Encode(World,Latitude,Longitude,Level),
             #{mode:=M} = World,
             {Time, Hash, Level,M}
         end).

encode_point_in_4th_quadrant(Encode) ->
    ?LET({PointAtFourthQuadrant,Level,World}, geohash_eqc_utils:generate_point_at_4th_quadrant(),
         begin
             {Longitude, Latitude} = PointAtFourthQuadrant,
             {Time, Hash} = Encode(World, Latitude, Longitude, Level),
             #{mode:=M} = World,
             {Time, Hash, Level,M}
         end).

tc_encode(World,Latitude,Longitude,Level) ->
     Start = erlang:system_time(nano_seconds),
     {ok,Hash} = geohash_int:encode(World,Latitude,Longitude,Level),
     Time = erlang:system_time(nano_seconds) - Start,
     {Time, Hash}.

tc_fast_encode(World,Latitude,Longitude,Level) ->
     Start = erlang:system_time(nano_seconds),
     {ok,Hash} = geohash_int:fast_encode(World,Latitude,Longitude,Level),
     Time = erlang:system_time(nano_seconds) - Start,
     {Time, Hash}.

