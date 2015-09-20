-module(geohash_int_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_hashint_at_corners() ->
    ?FORALL({LeftBottom,RightTop,RandomPosition,Level,World}, position_in_corners(world()),
            begin
                {Res,_Time} = do_encode({LeftBottom,RightTop,RandomPosition,Level,World}),
                Res
            end).

prop_collect_hashint_encode_time() ->
    ?FORALL({Res, Time}, encode_generator(),
            collect(with_title("average geoint encode time: microseconds(us)"), Time,
                    Res == true)).

encode_generator() ->
    ?LET(P, position_in_corners(world()),
         do_encode(P)).

do_encode({LeftBottom,RightTop,RandomPosition,Level,World}) ->
    {Longitude0, Latitude0} = LeftBottom,
    {Time0, {ok, Hash0}} = timer:tc(geohash_int, encode, [World,Latitude0,Longitude0,Level]),
    #{bits:=HashInt0, level:=L0} = Hash0,

    {Longitude3, Latitude3} = RightTop,
    {Time1, {ok,Hash3}} = timer:tc(geohash_int, encode, [World,Latitude3,Longitude3,Level]),
    #{bits:=HashInt3, level:=L3} = Hash3,

    {RandLon, RandLat} = RandomPosition,
    {Time, {ok,RandHash}} = timer:tc(geohash_int, encode, [World, RandLat, RandLon, Level]),
    #{bits:=RHashInt, level:=RL} = RandHash,

    {Level == L0 andalso Level == L3 andalso Level == RL
    andalso HashInt0 == 0
    andalso HashInt3 == ((1 bsl (Level*2)) - 1)
    andalso HashInt0 =< RHashInt andalso RHashInt =< HashInt3,
    (Time0+Time1+Time)/3}.


position_in_corners(Wgen) ->
    ?LET({Level,World}, {eqc_gen:choose(1,36),Wgen},
         begin
             #{north:=N,south:=S,west:=W,east:=E} = World,
             X = (E - W) / math:pow(2, Level),
             Y = (N - S) / math:pow(2, Level),
             LeftBottom = {W+X/2, S+Y/2},
             RightTop = {E - X/2, N - Y/2},
             RX = W + (E - W) * rand:uniform(),
             RY = S + (N - S) * rand:uniform(),
             {LeftBottom, RightTop, {RX,RY}, Level,World}
         end).

world() ->
    ?LET(R,real(),
         begin
            {ok, World} = geohash_int:define_world(-20037726.37,20037726.37,
                                                   -20037726.37,20037726.37,
                                                  'N'),
            World
         end).

