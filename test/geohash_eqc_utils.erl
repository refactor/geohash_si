-module(geohash_eqc_utils).

-include("../src/geohash_int.hrl").

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

generate_point_at_leftbottem_corner() ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL), world()},
         begin
             #{north:=N,south:=S,west:=W,east:=E} = World,
             CellNum = math:pow(2, Level),
             CellWidth = (E - W) / CellNum,
             CellHight = (N - S) / CellNum,
             PointAtLeftBottom = {W + CellWidth * rand:uniform(), S + CellHight * rand:uniform()},
             {PointAtLeftBottom, Level, World}
         end).

generate_point_at_righttop_corner() ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL), world()},
         begin
             #{north:=N,south:=S,west:=W,east:=E} = World,
             CellNum = math:pow(2, Level),
             CellWidth = (E - W) / CellNum,
             CellHight = (N - S) / CellNum,
             PointAtRightTop = {E - CellWidth * rand:uniform(), N - CellHight * rand:uniform()},
             {PointAtRightTop, Level, World}
         end).

generate_point_at_center_leftbottom() ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL), world()},
         begin
             #{north:=N,south:=S,west:=W,east:=E} = World,
             CellNum = math:pow(2, Level),
             CellWidth = (E - W) / CellNum,
             CellHight = (N - S) / CellNum,
             CenterX = (E + W) / 2,
             CenterY = (N + S) / 2,
             PointAtCenterLB = {CenterX - CellWidth * rand:uniform(),
                         CenterY - CellHight * rand:uniform()},
             {PointAtCenterLB, Level, World}
         end).

generate_point_at_center_righttop() ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL), world()},
         begin
             #{north:=N,south:=S,west:=W,east:=E} = World,
             CellNum = math:pow(2, Level),
             CellWidth = (E - W) / CellNum,
             CellHight = (N - S) / CellNum,
             CenterX = (E + W) / 2,
             CenterY = (N + S) / 2,
             PointAtCenterRT = {CenterX + CellWidth * rand:uniform(),
                         CenterY + CellHight * rand:uniform()},
             {PointAtCenterRT, Level, World}
         end).

generate_point_randomly() ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL), world()},
         begin
             #{north:=N,south:=S,west:=W,east:=E} = World,
             Longitude = W + (E - W) * rand:uniform(),
             Latitude = S + (N - S) * rand:uniform(),
             {{Longitude,Latitude}, Level, World}
         end).

generate_point_at_2nd_quadrant() ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL), world()},
         begin
             #{mode:=M, north:=N,south:=S,west:=W,east:=E} = World,
             HalfWidth = (E - W) / 2,
             HalfHeight = (N - S) / 2,
             PointAtSecondQuadrant = {W + HalfWidth*rand:uniform(), N - HalfHeight*rand:uniform()},
             {PointAtSecondQuadrant, Level, World}
         end).

generate_point_at_4th_quadrant() ->
    ?LET({Level,World}, {eqc_gen:choose(1,?MAX_LEVEL), world()},
         begin
             #{mode:=M, north:=N,south:=S,west:=W,east:=E} = World,
             HalfWidth = (E - W) / 2,
             HalfHeight = (N - S) / 2,
             PointAtFourthQuadrant = {E - HalfWidth*rand:uniform(), S + HalfHeight*rand:uniform()},
             {PointAtFourthQuadrant, Level, World}
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


