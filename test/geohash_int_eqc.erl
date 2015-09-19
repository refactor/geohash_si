-module(geohash_int_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_hashint_at_leftbottom_cell() ->
    ?FORALL({X,Y,L,World}, position_in_leftbottom_cell(),
            begin
                {ok, Hash} = geohash_int:encode(World,Y,X,L),
                #{bits:=HashInt, level:=Level} = Hash,
                0 =< HashInt andalso HashInt =< 3 andalso Level == L
            end).

position_in_leftbottom_cell() ->
    ?LET(Level, eqc_gen:choose(1,36),
         begin
            {ok, World} = geohash_int:define_world(-20037726.37,20037726.37,
                                                   -20037726.37,20037726.37,
                                                  'N'),
             #{north:=N,south:=S,west:=W,east:=E} = World,
             X = (E - W) / math:pow(2, Level),
             Y = (N - S) / math:pow(2, Level),
             {W+X/2, S+Y/2, Level,World}
         end).


