%Problem#1

%% move top in the middle row

move([X1,8,X3, X4,X5,X6, X7,X8,X9],
     [8,X1,X3, X4,X5,X6, X7,X8,X9]).
move([X1,X2,8, X4,X5,X6, X7,X8,X9],
     [X1,8,X2, X4,X5,X6, X7,X8,X9]).

%% move left in the middle row
move([X1,X2,X3, X4,8,X6,X7,X8,X9],
     [X1,X2,X3, 8,X4,X6,X7,X8,X9]).
move([X1,X2,X3, X4,X5,8,X7,X8,X9],
     [X1,X2,X3, X4,8,X5,X7,X8,X9]).

%% move left in the bottom row
move([X1,X2,X3, X4,X5,X6, X7,8,X9],
     [X1,X2,X3, X4,X5,X6, 8,X7,X9]).
move([X1,X2,X3, X4,X5,X6, X7,X8,8],
     [X1,X2,X3, X4,X5,X6, X7,8,X8]).

%% move right in the top row 
move([8,X2,X3, X4,X5,X6, X7,X8,X9],
     [X2,8,X3, X4,X5,X6, X7,X8,X9]).
move([X1,8,X3, X4,X5,X6, X7,X8,X9],
     [X1,X3,8, X4,X5,X6, X7,X8,X9]).

%% move right in the middle row 
move([X1,X2,X3, 8,X5,X6, X7,X8,X9],
     [X1,X2,X3, X5,8,X6, X7,X8,X9]).
move([X1,X2,X3, X4,8,X6, X7,X8,X9],
     [X1,X2,X3, X4,X6,8, X7,X8,X9]).

%% move right in the bottom row
move([X1,X2,X3, X4,X5,X6,8,X8,X9],
     [X1,X2,X3, X4,X5,X6,X8,8,X9]).
move([X1,X2,X3, X4,X5,X6,X7,8,X9],
     [X1,X2,X3, X4,X5,X6,X7,X9,8]).

%% move up from the middle row
move([X1,X2,X3, 8,X5,X6, X7,X8,X9],
     [8,X2,X3, X1,X5,X6, X7,X8,X9]).
move([X1,X2,X3, X4,8,X6, X7,X8,X9],
     [X1,8,X3, X4,X2,X6, X7,X8,X9]).
move([X1,X2,X3, X4,X5,8, X7,X8,X9],
     [X1,X2,8, X4,X5,X3, X7,X8,X9]).

%% move up from the bottom row
move([X1,X2,X3, X4,X5,X6, X7,8,X9],
     [X1,X2,X3, X4,8,X6, X7,X5,X9]).
move([X1,X2,X3, X4,X5,X6, X7,X8,8],
     [X1,X2,X3, X4,X5,8, X7,X8,X6]).
move([X1,X2,X3, X4,X5,X6, 8,X8,X9],
     [X1,X2,X3, 8,X5,X6, X4,X8,X9]).

%% move down from the top row
move([8,X2,X3, X4,X5,X6, X7,X8,X9],
     [X4,X2,X3, 8,X5,X6, X7,X8,X9]).
move([X1,8,X3, X4,X5,X6, X7,X8,X9],
     [X1,X5,X3, X4,8,X6, X7,X8,X9]).
move([X1,X2,8, X4,X5,X6, X7,X8,X9],
     [X1,X2,X6, X4,X5,8, X7,X8,X9]).

%% move down from the middle row
move([X1,X2,X3, 8,X5,X6, X7,X8,X9],
     [X1,X2,X3, X7,X5,X6, 8,X8,X9]).
move([X1,X2,X3, X4,8,X6, X7,X8,X9],
     [X1,X2,X3, X4,X8,X6, X7,8,X9]).
move([X1,X2,X3, X4,X5,8, X7,X8,X9],
     [X1,X2,X3, X4,X5,X9, X7,X8,8]).



cross(S,G):- % i query here for problem 
     path(S,[S],G). 

path([8,0,1,2,3,4,5,6,7],V,V).  % best case return all visited that satisfy th goal 
                
path(S,V,G):- % continue if u dont find goal
   move(S,News),
  \+ member(News,V),
   write(News),
   path(News,[News|V],G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Problem#2

%Task#1
path(Start, End, any, [[Start, End]]) :-
    connected(Start, End).%its base case when the two station(start &end)is beyond each other so the path is from start to end .

path(Start, End, any, [[Start, Newstart]|Newlist]):-

   connected(Start, Newstart),
   path(Newstart, End, any, Newlist), !.%in this case  newstart is the next station to this start station and newlist is the remining list of the main list when we extract or take the newstart station from this list

path(End, Start, any, [[Start, Newstart]|Newlist]):-
   connected(Start, Newstart),
   path(Newstart, End, any, Newlist), !.%last case which is the last connected in the list

% the same cases but with limit number of stations as given for true and
% false
path(Start, End, N, [[Start,End]]) :-
  N\=any,
  N>0,
  connected(Start, End).

path(Start, End, N, [[Start,Newstart]|Newlist]):-
  N\=any,
  N>0,
  connected(Start, Newstart),
  NN is N-1,
  path(Newstart, End, NN, Newlist),!.


path(End, Start, N, [[Start,Newstart]|Newlist]):-
  N\=any,
  N>0,
  connected(Start, Newstart),
  NN is N-1,
  path(Newstart, End, NN, Newlist),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Task#2
member(N,[H|T]):-
   N=H;
   member(N,T).

connect(N,H):-
  connected(N,H);
  connected(H,N).

connectedStations(Station,CountList,L):-
   connect(Station,X),
   \+ member(X,CountList),
   connectedStations(Station,[X|CountList],L).
   connectedStations(_,L,L).

lengthZerosList([],0).
lengthZerosList([H|T],Counter):-
   lengthZerosList(T,R),
   Counter is 1+R.
   
nStations(X,N):-
   connectedStations(X,[],R),
   lengthZerosList(R,N).
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Task#3
listCount([], 0).
listCount([_|Tail], Count) :-
    listCount(Tail, TailCount),
    Count is TailCount + 1.

cost(Start,End,Cost):- %in this task we do line1(),line2(),facts or data
    %and use path from task 1 and listCount from task 2 to calc the lenght and find the path ...we have 3(3,5,7-EGP) case but in 2 lines 1 and 2

    line1(Start),
    line1(End),
    path(Start, End,any,L),
    listCount(L,N),%l->List ,N->Counter
    N=<7,
    Cost = '3-EGP'.

cost(Start,End,Cost):-
    line2(Start),
    line2(End),
    path(Start, End,any,L),
    listCount(L,N),
    N=<7,
    Cost = '3-EGP'.

cost(Start,End,Cost):-
    line1(Start),
    line1(End),
    path(Start, End,any,L),
    listCount(L,N),
    N<16,
    N>7,
    Cost = '5-EGP'.%more than 7 to 16 stations

cost(Start,End,Cost):-
    line2(Start),
    line2(End),
    path(Start, End,any,L),
    listCount(L,N),
    N<16,
    N>7,
    Cost = '5-EGP'.%more than 7 to 16 stations

cost(Start,End,Cost):-
    line1(Start),
    line2(End),%different lines
    path(Start, End ,any,L),
    listCount(L,N),
    N<16,
    N>7,
    Cost = '5-EGP'.

cost(Start,End,Cost):-
    line2(Start),
    line1(End),
    path(Start, End ,any,L),
    listCount(L,N),
    N<16,
    N>7,
    Cost = '5-EGP'. %different linees


cost(Start,End,Cost):-
    path(Start, End,any,L),
    listCount(L,N),
    N>=16,
    Cost = '7-EGP'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Task#4
chechPath([[X,Y],[]]):-
   [F,S] = [[X,Y],[]],
   [A,B] = F,
   connected(A,B).
   
checkPath([[X,Y],[A,B]]):-
  [F,S] = [[X,Y],[A,B]],
  [W,Q] = F,
  [N,M] = S,
  connected(W,Q),
  Q = N,
  connected(N,M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%first line
connected(new_elmarg,elmarg).
connected(elmarg,ezbet_elnakhl).
connected(ezbet_elnakhl,ain_shams).
connected(ain_shams,elmatareyya).
connected(elmatareyya,helmeyet_elzaitoun).
connected(helmeyet_elzaitoun,hadayeq_elzaitoun).
connected(hadayeq_elzaitoun,saray_elqobba).
connected(saray_elqobba,hammamat_elqobba).
connected(hammamat_elqobba,kobri_elqobba).
connected(kobri_elqobba,manshiet_elsadr).
connected(manshiet_elsadr,eldemerdash).
connected(eldemerdash,ghamra).
connected(ghamra,alshohadaa).
connected(alshohadaa,urabi).
connected(urabi,nasser).
connected(nasser,sadat).
connected(sadat,saad_zaghloul).
connected(saad_zaghloul, alsayyeda_zeinab).
connected(alsayyeda_zeinab,elmalek_elsaleh).
connected(elmalek_elsaleh,margirgis).
connected(margirgis,elzahraa).
connected(elzahraa,dar_elsalam).
connected(dar_elsalam,hadayeq_elmaadi).
connected(hadayeq_elmaadi,maadi).
connected(maadi,thakanat_elmaadi).
connected(thakanat_elmaadi,tora_elbalad).
connected(tora_elbalad,kozzika).
connected(kozzika,tora_elasmant).
connected(tora_elasmant,elmaasara).
connected(elmaasara,hadayeq_helwan).
connected(hadayeq_helwan,wadi_hof).
connected(wadi_hof,helwan_university).
connected(helwan_university,ain_helwan).
connected(ain_helwan,helwan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%second line
connected(shobra_elkheima,koliet_elzeraa).
connected(koliet_elzeraa,mezallat).
connected(mezallat,khalafawy).
connected(khalafawy,sainte_teresa).
connected(sainte_teresa,road_elfarag).
connected(road_elfarag,massara).
connected(massara,alshohadaa).
connected(alshohadaa,ataba).
connected(ataba,naguib).
connected(naguib,sadat).
connected(sadat,opera).
connected(opera,dokki).
connected(dokki,bohooth).
connected(bohooth,cairo_university).
connected(cairo_university,faisal).
connected(faisal,giza).
connected(giza,omm_elmisryeen).
connected(omm_elmisryeen,sakiat_mekki).
connected(sakiat_mekki,elmounib).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%first-line-Staitons
line1(new_elmarg).
line1(elmarg).
line1(ezbet_elnakhl).
line1(ain_shams).
line1(elmatareyya).
line1(helmeyet_elzaitoun).
line1(hadayeq_elzaitoun).
line1(saray_elqobba).
line1(hammamat_elqobba).
line1(kobri_elqobba).
line1(manshiet_elsadr).
line1(eldemerdash).
line1(ghamra).
line1(alshohadaa).
line1(urabi).
line1(nasser).
line1(sadat).
line1(saad_zaghloul).
line1(alsayyeda_zeinab).
line1(elmalek_elsaleh).
line1(margirgis).
line1(elzahraa).
line1(dar_elsalam).
line1(hadayeq_elmaadi).
line1(maadi).
line1(thakanat_elmaadi).
line1(tora_elbalad).
line1(kozzika).
line1(tora_elasmant).
line1(elmaasara).
line1(hadayeq_helwan).
line1(wadi_hof).
line1(helwan_university).
line1(ain_helwan).
line1(helwan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Second-line-Staitons
line2(shobra_elkheima).
line2(koliet_elzeraa).
line2(mezallat).
line2(khalafawy).
line2(sainte_teresa).
line2(road_elfarag).
line2(massara).
line2(alshohadaa).
line2(ataba).
line2(naguib).
line2(sadat).
line2(opera).
line2(dokki).
line2(bohooth).
line2(cairo_university).
line2(faisal).
line2(giza).
line2(omm_elmisryeen).
line2(sakiat_mekki).
line2(elmounib).
