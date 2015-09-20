-module(geohash_int_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_hashint_at_corners() ->
    ?FORALL({T1, T2}, {position_in_corners(world('N')),position_in_corners(world('Z'))},
            begin
                {Res1,_Time1} = do_encode(T1),
                {Res2,_Time2} = do_encode(T2),
                Res1 andalso Res2
            end).

prop_collect_hashint_encode_time() ->
    numtests(10000,
             ?FORALL({{Res1,Times1},{Res2,Times2}}, {encode_generator('N'),encode_generator('Z')},
                     collect(with_title("average geoint encode time: microseconds(us)"),
                     begin
                         Times = Times1 ++ Times2,
                         round(lists:sum(Times)/length(Times))
                     end,
                     Res1 == true andalso Res2 == true))).

encode_generator(M) ->
    ?LET(P, position_in_corners(world(M)),
         do_encode(P)).

do_encode({LeftBottom,RightTop,CenterLB,CenterRT,RandomPosition,Level,World}) ->
    {Longitude0, Latitude0} = LeftBottom,
    {Time0, {ok, Hash0}} = timer:tc(geohash_int, encode, [World,Latitude0,Longitude0,Level]),
    #{bits:=HashInt0, level:=L0} = Hash0,

    {Longitude3, Latitude3} = RightTop,
    {Time3, {ok,Hash3}} = timer:tc(geohash_int, encode, [World,Latitude3,Longitude3,Level]),
    #{bits:=HashInt3, level:=L3} = Hash3,

    {RandLon, RandLat} = RandomPosition,
    {Time, {ok,RandHash}} = timer:tc(geohash_int, encode, [World, RandLat, RandLon, Level]),
    #{bits:=RHashInt, level:=RL} = RandHash,

    {CenterLBLon, CenterLBLat} = CenterLB,
    {Time1, {ok,Hash1}} = timer:tc(geohash_int, encode, [World, CenterLBLat,CenterLBLon,Level]),
    #{bits:=HashInt1, level:=L1} = Hash1,

    {CenterRTLon, CenterRTLat} = CenterRT,
    {Time2, {ok,Hash2}} = timer:tc(geohash_int, encode, [World, CenterRTLat,CenterRTLon,Level]),
    #{bits:=HashInt2, level:=L2} = Hash2,

    {Level == RL andalso HashInt0 =< RHashInt andalso RHashInt =< HashInt3
     andalso L0 == Level andalso HashInt0 == 0
     andalso L1 == Level andalso HashInt1 == ((1 bsl ((Level-1)*2)) - 1)
     andalso L2 == Level andalso HashInt2 == ((1 bsl ((Level-1)*2)) * 3)
     andalso L3 == Level andalso HashInt3 == ((1 bsl (Level*2)) - 1)
     ,
     [Time0,Time1,Time2,Time3,Time]}.


position_in_corners(Wgen) ->
    ?LET({Level,World}, {eqc_gen:choose(1,36),Wgen},
         begin
             #{north:=N,south:=S,west:=W,east:=E} = World,
             CellNum = math:pow(2, Level),
             CellWidth = (E - W) / CellNum,
             CellHight = (N - S) / CellNum,
             LeftBottom = {W + CellWidth * rand:uniform(), S + CellHight * rand:uniform()},
             RightTop = {E - CellWidth * rand:uniform(), N - CellHight * rand:uniform()},
             CenterX = (E + W) / 2,
             CenterY = (N + S) / 2,
             CenterLB = {CenterX - CellWidth * rand:uniform(),
                         CenterY - CellHight * rand:uniform()},
             CenterRT = {CenterX + CellWidth * rand:uniform(),
                         CenterY + CellHight * rand:uniform()},
             RX = W + (E - W) * rand:uniform(),
             RY = S + (N - S) * rand:uniform(),
             {LeftBottom, RightTop, CenterLB, CenterRT, {RX,RY}, Level,World}
         end).

world(M) ->
    ?LET(R,real(),
         begin
            {ok, World} = geohash_int:define_world(-20037726.37,20037726.37,
                                                   -20037726.37,20037726.37,
                                                   M),
            World
         end).

