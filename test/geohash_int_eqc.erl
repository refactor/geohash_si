-module(geohash_int_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-define(MAX_LEVEL, 30).

prop_geohash_encode_leftbottom() ->
    ?FORALL({Time, Hash, Level}, generate_point_in_leftbottom(world(), fun tc_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        #{bits:=HashInt, level:=L} = Hash,
                        equals({L,HashInt}, {Level,0})
                    end))).

prop_geohash_fast_encode_leftbottom() ->
    ?FORALL({Time, Hash, Level}, generate_point_in_leftbottom(world(), fun tc_fast_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("fast_encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        #{bits:=HashInt, level:=L} = Hash,
                        equals({L,HashInt}, {Level,0})
                    end))).

prop_geohash_encode_righttop() ->
    ?FORALL({Time, Hash, Level, RightTop}, generate_point_in_righttop(world(), fun tc_encode/4),
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
    ?FORALL({Time,Hash,Level,RightTop}, generate_point_in_righttop(world(), fun tc_fast_encode/4),
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
    ?FORALL({Time, Hash, Level}, generate_point_in_center_leftbottom(world(), fun tc_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End1 = (1 bsl ((Level-1) * 2)) - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        equals({L,HashInt}, {Level,End1})
                    end))).

prop_geohash_fast_encode_center_leftbottom() ->
    ?FORALL({Time, Hash, Level}, generate_point_in_center_leftbottom(world(), fun tc_fast_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("fast_encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End1 = (1 bsl ((Level-1) * 2)) - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        equals({L,HashInt}, {Level,End1})
                    end))).

prop_geohash_encode_center_righttop() ->
    ?FORALL({Time, Hash, Level}, generate_point_in_center_righttop(world(), fun tc_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End3 = (1 bsl ((Level-1) * 2)) * 3 - 1,
                        Start4 = End3 + 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        equals({L,HashInt}, {Level,Start4})
                    end))).

prop_geohash_fast_encode_center_righttop() ->
    ?FORALL({Time, Hash, Level}, generate_point_in_center_righttop(world(), fun tc_fast_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("fast_encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        End3 = (1 bsl ((Level-1) * 2)) * 3 - 1,
                        Start4 = End3 + 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        equals({L,HashInt}, {Level,Start4})
                    end))).

prop_geohash_encode_random_position() ->
    ?FORALL({Time, Hash, Level}, generate_point_randomly(world(), fun tc_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        Start1 = 0,
                        End4 = (1 bsl (Level*2)) - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        Level == L andalso Start1 =< HashInt andalso HashInt =< End4
                    end))).

prop_geohash_fast_encode_random_position() ->
    ?FORALL({Time, Hash, Level}, generate_point_randomly(world(), fun tc_fast_encode/4),
            measure("summary(unit: nanoseconds[ns])", Time,
            collect(with_title("encode time(unit: microseconds[us])"), round(Time/1000),
                    begin
                        Start1 = 0,
                        End4 = (1 bsl (Level*2)) - 1,
                        #{bits:=HashInt, level:=L} = Hash,
                        Level == L andalso Start1 =< HashInt andalso HashInt =< End4
                    end))).

prop_geohash_encode_position_in_2nd_quadrant() ->
    ?FORALL({Time, Hash, Level,Mode}, generate_point_in_2nd_quadrant(world(), fun tc_encode/4),
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
    ?FORALL({Time,Hash,Level,Mode}, generate_point_in_2nd_quadrant(world(), fun tc_fast_encode/4),
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
    ?FORALL({Time, Hash, Level,Mode}, generate_point_in_4th_quadrant(world(), fun tc_encode/4),
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
    ?FORALL({Time,Hash,Level,Mode}, generate_point_in_4th_quadrant(world(), fun tc_fast_encode/4),
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

generate_point_in_leftbottom(Wgen, Encode) ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL),Wgen},
         begin
             #{north:=N,south:=S,west:=W,east:=E} = World,
             CellNum = math:pow(2, Level),
             CellWidth = (E - W) / CellNum,
             CellHight = (N - S) / CellNum,
             LeftBottom = {W + CellWidth * rand:uniform(), S + CellHight * rand:uniform()},
             {Longitude, Latitude} = LeftBottom,
%             {Time, {ok, Hash}} = timer:tc(geohash_int, encode, [World,Latitude,Longitude,Level]),
             {Time, Hash} = Encode(World,Latitude,Longitude,Level),
             {Time, Hash, Level}
         end).

generate_point_in_righttop(Wgen, Encode) ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL),Wgen},
         begin
             #{north:=N,south:=S,west:=W,east:=E} = World,
             CellNum = math:pow(2, Level),
             CellWidth = (E - W) / CellNum,
             CellHight = (N - S) / CellNum,
             RightTop = {E - CellWidth * rand:uniform(), N - CellHight * rand:uniform()},
             {Longitude, Latitude} = RightTop,
             %{Time, {ok, Hash}} = timer:tc(geohash_int, encode, [World,Latitude,Longitude,Level]),
             {Time, Hash} = Encode(World,Latitude,Longitude,Level),
             {Time, Hash, Level, RightTop}
         end).

generate_point_in_center_leftbottom(Wgen, Encode) ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL),Wgen},
         begin
             #{north:=N,south:=S,west:=W,east:=E} = World,
             CellNum = math:pow(2, Level),
             CellWidth = (E - W) / CellNum,
             CellHight = (N - S) / CellNum,
             CenterX = (E + W) / 2,
             CenterY = (N + S) / 2,
             CenterLB = {CenterX - CellWidth * rand:uniform(),
                         CenterY - CellHight * rand:uniform()},
             {Longitude, Latitude} = CenterLB,
             %{Time, {ok, Hash}} = timer:tc(geohash_int, encode, [World,Latitude,Longitude,Level]),
             {Time, Hash} = Encode(World,Latitude,Longitude,Level),
             {Time, Hash, Level}
         end).

generate_point_in_center_righttop(Wgen, Encode) ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL),Wgen},
         begin
             #{north:=N,south:=S,west:=W,east:=E} = World,
             CellNum = math:pow(2, Level),
             CellWidth = (E - W) / CellNum,
             CellHight = (N - S) / CellNum,
             CenterX = (E + W) / 2,
             CenterY = (N + S) / 2,
             CenterRT = {CenterX + CellWidth * rand:uniform(),
                         CenterY + CellHight * rand:uniform()},
             {Longitude, Latitude} = CenterRT,
             %{Time, {ok, Hash}} = timer:tc(geohash_int, encode, [World,Latitude,Longitude,Level]),
             {Time, Hash} = Encode(World,Latitude,Longitude,Level),
             {Time, Hash, Level}
         end).

generate_point_randomly(Wgen, Encode) ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL),Wgen},
         begin
             #{north:=N,south:=S,west:=W,east:=E} = World,
             Longitude = W + (E - W) * rand:uniform(),
             Latitude = S + (N - S) * rand:uniform(),
             %{Time, {ok, Hash}} = timer:tc(geohash_int, encode, [World,Latitude,Longitude,Level]),
             {Time, Hash} = Encode(World,Latitude,Longitude,Level),
             {Time, Hash, Level}
         end).

generate_point_in_2nd_quadrant(Wgen, Encode) ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL),Wgen},
         begin
             #{mode:=M, north:=N,south:=S,west:=W,east:=E} = World,
             HalfWidth = (E - W) / 2,
             HalfHeight = (N - S) / 2,
             SecondQuadrantPoint = {W + HalfWidth*rand:uniform(), N - HalfHeight*rand:uniform()},
             {Longitude, Latitude} = SecondQuadrantPoint,
             %{Time, {ok, Hash}} = timer:tc(geohash_int, encode, [World,Latitude,Longitude,Level]),
             {Time, Hash} = Encode(World,Latitude,Longitude,Level),
             {Time, Hash, Level,M}
         end).

generate_point_in_4th_quadrant(Wgen, Encode) ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL),Wgen},
         begin
             #{mode:=M, north:=N,south:=S,west:=W,east:=E} = World,
             HalfWidth = (E - W) / 2,
             HalfHeight = (N - S) / 2,
             FourthQuadrantPoint = {E - HalfWidth*rand:uniform(), S + HalfHeight*rand:uniform()},
             {Longitude, Latitude} = FourthQuadrantPoint,
             %{Time, {ok, Hash}} = timer:tc(geohash_int, encode, [World,Latitude,Longitude,Level]),
             {Time, Hash} = Encode(World,Latitude,Longitude,Level),
             {Time, Hash, Level,M}
         end).

world() ->
    oneof([world('N'), world('Z')]).

world(M) ->
    ?LET({Ymin,Ymax,Xmin,Xmax}, world_range(),
         begin
            {ok, World} = geohash_int:define_world(float(Ymin),float(Ymax),
                                                   float(Xmin),float(Xmax),
                                                   M),
            World
         end).

world_range() ->
    frequency([{25, {return(-20037726.37),return(20037726.37),return(-20037726.37),return(20037726.37)}},
               {25, {return(-90.0),return(90.0),return(-180.0),return(180.0)}},
               {25, {choose(-20037726,10),choose(11,20037726),choose(-20037726,10),choose(11,20037726)}},
               {25, {choose(-90,1),choose(2,90),choose(-180,1),choose(2,180)}}]).


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

