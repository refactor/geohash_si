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

do_encode({LeftBottom,RightTop,CenterLB,CenterRT,SecondQuadrantPoint,FourthQuadrantPoint,RandomPosition,Level,World}) ->
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

    {SQPLon, SQPLat} = SecondQuadrantPoint,
    {Time4,{ok,Hash4}} = timer:tc(geohash_int, encode, [World,SQPLat,SQPLon,Level]),
    #{bits:=HashInt4, level:=L4} = Hash4,

    {FQPLon, FQPLat} = FourthQuadrantPoint,
    {Time5,{ok,Hash5}} = timer:tc(geohash_int, encode, [World,FQPLat,FQPLon,Level]),
    #{bits:=HashInt5, level:=L5} = Hash5,

    #{mode:=M} = World,
    Start1 = 0, End1 = (1 bsl ((Level-1) * 2)) - 1,
    Start2 = End1 + 1, End2 = (1 bsl ((Level-1) * 2)) * 2 - 1,
    Start3 = End2 + 1, End3 = ((1 bsl (Level-1) * 2)) * 3 - 1,
    Start4 = End3 + 1, End4 = (1 bsl (Level*2)) - 1,

    {Level == RL andalso HashInt0 =< RHashInt andalso RHashInt =< HashInt3
     andalso L0 == Level andalso HashInt0 == 0
     andalso L1 == Level andalso HashInt1 == End1
     andalso L2 == Level andalso HashInt2 == Start4
     andalso L3 == Level andalso HashInt3 == End4
     andalso L4 == Level andalso (((M == 'N') and (Start2 =< HashInt4 andalso HashInt4 =< End2)) orelse ((M == 'Z') and (Start3 =< HashInt4 andalso HashInt4 =< End3)))
     andalso L5 == Level andalso (((M == 'N') and (Start3 =< HashInt5 andalso HashInt5 =< End3)) orelse ((M == 'Z') and (Start2 =< HashInt5 andalso HashInt5 =< End2)))
     ,
     [Time0,Time1,Time2,Time3,Time4,Time5,Time]}.


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
             SecondQuadrantPoint = {(W + CenterX) / 2, (CenterY + N) / 2},
             FourthQuadrantPoint = {(CenterX + E) / 2, (S + CenterY) / 2},
             RX = W + (E - W) * rand:uniform(),
             RY = S + (N - S) * rand:uniform(),
             {LeftBottom, RightTop, CenterLB, CenterRT, SecondQuadrantPoint, FourthQuadrantPoint, {RX,RY}, Level,World}
         end).

world(M) ->
    ?LET(R,real(),
         begin
            {ok, World} = geohash_int:define_world(-20037726.37,20037726.37,
                                                   -20037726.37,20037726.37,
                                                   M),
            World
         end).

