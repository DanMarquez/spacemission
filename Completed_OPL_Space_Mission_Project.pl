% NANI SEARCH - Space Missions Adventure

% Space Missions Adventure is designed to illustrate Prolog programming.  It
% is an implementation of the basic principle of Prolog
% in order to produce an interesting Treasure Hunt Game 

main:- rocket_search.       % main entry point

rocket_search:-
  init_dynamic_facts,     % predicates which are not compiled

  write('SPACE MISSIONS - An Amazing Adventure Game'),nl,
  write('Compiler: Hoang Nguyen & Dan Marquez'),nl,
  nl,
  write('Space Missions Adventure is designed to illustrate Prolog programming.'),nl,
  write('As such, it might be the simplest adventure game.  The game'),nl,
  write('is the primary example used in this tutorial.'),nl,
  write('Full source is included as well.'),nl,
  nl,
  write('Bob has been chosen among 100 engineering students'),nl,
  write('to be the Project Manager of a Collaboratory Project'),nl,
  write('named "Space Missions". His mission is to find all'),nl,
  write('the necessary parts in the manual description created '),nl,
  write('by other students. He carries his backpack with him at all times'),nl,
  write('to bring back the items to the project space.'),
  write('Help Bob locate all the items, but watch out for aliens!'),nl,
  write('The aliens are trying to sabotage the mission by eating the all the'),nl,
  write('project managers.'),nl, 

  nl,
  write('You control the game by using simple English commands'),nl,
  write('expressing the action you wish to take.  You can go to'),nl,
  write('other rooms, look at your surroundings, look in things'),nl,
  write('take things, drop things, inventory the'),nl,
  write('things you have.'),nl,
  write('Get the assembly manual and reference it to check items'),nl,
  write('that have successfully been brought back to the project space.'),nl,
  nl,
  write('Hit any key to continue.'),get0(_),
  write('Type "help" if you need more help on mechanics.'),nl,
  write('Type "hint" if you want a big hint.'),nl,
  write('Type "quit" if you give up.'),nl,
  nl,
  write('Enjoy the hunt.'),nl,
  look,                   % give a look before starting the game
  command_loop.

% command_loop - repeats until either the nani is found or the
%     player types quit

command_loop:-
  repeat,
  get_command(X),
  do(X),
  check_counter,
  (alienfound; assembly_complete; X == quit).
  
  % do - matches the input command with the predicate which carries out
%     the command.  More general approaches which might work in the
%     listener are not supported in the compiler.  This approach
%     also gives tighter control over the allowable commands.

%     The cuts prevent the forced failure at the end of "command_loop"
%     from backtracking into the command predicates.

do(goto(X)):-goto(X),!.
do(nshelp):-nshelp,!.
do(hint):-hint,!.
do(inventory):-inventory,!.
do(take(X)):-take(X),!.
do(drop(X)):-drop(X),!.
do(look):-look,!.
do(look_in(X)):-look_in(X),!.
do(quit):-quit,!.
do(check_manual):-check_manual,!.
do(talkTo(X)):-talkTo(X),!.

% counter

count(Thing,Count) :-
        findall(1,Thing,Length),
        length(Length,Count).
        
check_counter :-
	F is 10, 
	
	count(location(D, 'project space'), C),
	C == F,
	retract(assembly_status(incomplete)),
	asserta(assembly_status(complete)).
check_counter.




% These are the predicates which control exit from the game.  If
% the player has taken the nani, then the call to "have(nani)" will
% succeed and the command_loop will complete.  Otherwise it fails
% and command_loop will repeat.
:- dynamic(have/1).
:- dynamic(response/3).
:- dynamic(alien_status/1).
:- dynamic(assembly_status/1).
:- dynamic(item/1).

have('  ').

alienfound:-
  alien_status(found),        
  write('Dang, the alien ate you alive.'),
  nl,
  write('On to the next project manager!'),
  nl,nl.

assembly_complete:-
  assembly_status(complete),
  write('You did it! In yo face aliens!'),
  nl,
  write('Commence launch sequence immediately'),
  nl, 
  write('Whoosh!'),
  nl,
  write('Good job Bob! Now lets start the rocket to lonesome Pluto....'),
  nl,
  nl.

  


quit:-
  write('Giving up?  Now we can''t get to mars :-('),
  nl,
  nl.
  
 % The help command

nshelp:-
  write('Use simple English sentences to enter commands.'),nl,
  write('The commands can cause you to:'),nl,
  nl,
  write('   go to a room          (ex. go to the office)'),nl,
  write('   look around           (ex. look)'),nl,
  write('   look in something     (ex. look in the desk)'),nl,
  write('   take something        (ex. take the apple)'),nl,
  write('   drop something        (ex. drop the apple)'),nl,
  write('   inventory your things (ex. inventory)'),nl,
  write('   check assembly manual (ex. check assembly manual)'),nl,
  nl,
  write('The examples are verbose, terser commands and synonyms'),nl,
  write('are usually accepted.'),
  nl,
  nl,
  write('Hit any key to continue.'),
  nl,
  nl,
  get0(_),
  look.

hint:-
  write('You can''t find necessary pieces of the rocket until '),nl,
  write('you have the manual. You can''t find things until'),nl,
  write('you talk to people. You can''t assemble the rocket'),nl,
  write('until you have all the necessary parts.'),nl,
  nl,
  look.
  
 % Initial facts describing the world.  Rooms and doors do not change,
% so they are compiled.

% alien status
alien_status(not_found).

% assembly status
assembly_status(incomplete).




% basement
room('frey 110').
room('project space').
room('engineering shop').
room('hydro lab').

% 2nd floor
room('frey 252').

room('frey 254').
room('frey 256').
room('printing room').
room('outside frey').
room('physics room').
room('union').


% connections
door('frey 110', 'project space').
door('project space', 'outside frey').
door('project space', 'union').
door('project space', 'engineering shop').
door('engineering shop', 'hydro lab').
door('project space', 'hydro lab').

door('project space', 'frey 252').
door('frey 252', 'frey 254').

door('frey 254', 'frey 256').
door('frey 252', 'physics room').


% items
item(screwdriver).
item(fuel).
item(bolts).
item('steering panel').
item('flight path').
item('space suits').
item(rocket).




% people
person('dr underwood').
person('john meyer').
person('dr soerens').
person('dr hellgren').
person('paul').




response('There are quite a few items in here','john meyer', one_time).
response('The fate of the mission is in your hands', 'john meyer', continuous).

response('You''ve gotta be careful with that fuel!', 'dr soerens', one_time).
response('One bad move and KABOOM!', 'dr soerens', one_time).
response('Try looking for some stability gloves in the engineering shop', 'dr soerens', one_time).
response('Careful! Use gloves!', 'dr soerens', continuous).

response('There might be a few aliens on this floor.', 'dr underwood', one_time).
response('Hurry up! We''ve got to stay on schedule', 'dr underwood', continuous).

response('It took me a long time to calculate the proper trajectory!', 'dr hellgren', one_time).
response('Don''t lose my hard work!', 'dr hellgren', continuous).

response('You need the proper gear in outer space!', 'paul', one_time).
response('My tweedy printer 9000s can print anything! except a space ship', 'paul', one_time).
response('Go pick up the suits in the tweedy printer', 'paul', one_time).
response('good luck!', 'paul', continuous).



connect(X,Y):-
  door(X,Y).
connect(X,Y):-
  door(Y,X).
  
init_dynamic_facts:-
  assertz(location(backpack, 'frey 110')),
  
  assertz(location('assembly manual', 'project space')),
  assertz(location(rocket, 'project space')),
  assertz(location(cockpit, 'project space')),
  assertz(location(crane, 'project space')),
  assertz(location(bottles,'project space')),
  
  % alien in here
  assertz(location(desk,'engineering shop')), %
  assertz(location(screwdriver, 'engineering shop')),
  assertz(location(closet,'engineering shop')),%
  assertz(location('john meyer','engineering shop')),
  assertz(location(cabinet, 'engineering shop')),%
  assertz(location(alien, cabinet)),
  assertz(location(bolts, desk)),
  assertz(location(gloves, closet)),
  
  % alien in here
  assertz(location(fuel,'hydro lab')),
  assertz(location('dr soerens','hydro lab')),
  assertz(location(locker,'hydro lab')),
  assertz(location(alien, locker)),
  
  
  
  
  assertz(location('cupboard', 'frey 254')),
  assertz(location('steering panel', 'cupboard')),
  
  % alien in here
  assertz(location('dr underwood','frey 252')),
  assertz(location(box, 'frey 252')),
  assertz(location(alien, box)),
  
  
  
  
  
  assertz(location('rocket stand', 'outside frey')),
  
  
  assertz(location(computer, 'physics room')),
  assertz(location('dr hellgren', 'physics room')),
  assertz(location('flight path', computer)),
  
  
  assertz(location(pizza, union)),
  
  
  assertz(location('tweedy printer', 'frey 256')),
  assertz(location('paul', 'frey 256')),
  
  assertz(here('frey 110')).
  
furniture(desk).
furniture(closet).
furniture(locker).
furniture(cabinet).
furniture(cupboard).
furniture(computer).
furniture('tweedy printer').
 
   %%%%%%%% COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%%%


% talk to people

talkTo(Person) :-
	can_Talk(Person),
	response(X,Person,S),
	choose_response(X,Person,S),
	nl.
	
talkTo(_).

% response('Go pick up the suits in the tweedy printer', 'paul', one_time).
choose_response(X,Person,S) :-
	S==one_time,
	write(X),nl,
	nl,
	add_suits(X,Person),
	retract(response(X, Person, one_time)),
	nl.
choose_response(X,Person,S) :-
	write(X),
	nl,
	nl.	
add_suits(X, Person):-
	X == 'Go pick up the suits in the tweedy printer',
	Person == 'paul',
	asserta(location('space suits', 'tweedy printer')),!.
add_suits(X, Person).
	


can_Talk(Person) :-
	here(Place),
	location(Person, Place),nl.
	
can_Talk(Person) :-
	write('Who are you talking to?'), nl,
	write(Person), write(' isn''t here'),
	nl,
	nl,
	fail.

% goto moves the player from room to room.

goto(Room):-
  
  can_go(Room),                 % check for legal move
  %puzzle(goto(Room)),           % check for special conditions
  moveto(Room),                 % go there and tell the player
  
  look.
goto(_):- look.

can_go(Room):-                  % if there is a connection it 
  here(Here),                   % is a legal move.
  have('backpack'),
  connect(Here,Room),!.
  
can_go(Room):-
  have('backpack'),
  respond(['You can''t get to ',Room,' from here']),fail.
  
can_go(Room):-
  nl,
  nl,
  write('Don''t forget your backpack!'),nl,
  write('You need it to carry important stuff.'),
  nl,
  nl,
  fail.
  


moveto(Room):-                  % update the logicbase with the
  retract(here(_)),             % new room
  asserta(here(Room)).




% look lists the things in a room, and the connections

look:-
  here(Here),
  respond(['You are in the ',Here]),
  write('You can see the following things:'),
  nl,
  list_things(Here),
  are_there_people(Here),
  nl,
  write('You can go to the following rooms:'),
  nl,
  list_connections(Here),
  
  nl,
  nl,
  nl.
  

  
are_there_people(Here):-
  location(X, Here),
  person(X),
  write('You can see the following people: '), 
  nl,
  list_people(Here),
  nl.
  
are_there_people(Here).
  
  
list_things(Place):-
  
  location(X,Place),
  
  tab(2),
  
  has_to_be_thing(X),
  check_for_alien(X),nl,
  fail.
list_things(_).

has_to_be_thing(X):-
	
	write(X),
	fail.
has_to_be_thing(X).


check_for_alien(X) :-
  X == alien,
  retract(alien_status(not_found)),
  asserta(alien_status(found)).
check_for_alien(X).
  

list_connections(Place):-
  connect(Place,X),
  tab(2),
  write(X),
  nl,
  fail.
list_connections(_).

list_people(Place):-
	location(X,Place),
	person(X),
	tab(2),
	write(X),
	nl,
	fail.
list_people(_).

% look_in allows the player to look inside a thing which might
% contain other things

look_in(Thing):-
  location(_,Thing),               % make sure there's at least one
  write('The '),
  write(Thing),
  write(' contains:'),
  nl,
  list_things(Thing),
  nl.
  
look_in(Thing):-
  respond(['There is nothing in the ',Thing]),
  nl.

% take allows the player to take something.  As long as the thing is
% contained in the room it can be taken, even if the adventurer hasn't
% looked in the the container which contains it.  Also the thing
% must not be furniture.

take(Thing):-
  is_here(Thing),
  is_takable(Thing),
  move(Thing,have),
  respond(['You now have the ',Thing]),
  nl.


is_here(Thing):-
  here(Here),
  contains(Thing,Here),!.          % don't backtrack
is_here(Thing):-
  respond(['There is no ',Thing,' here']),
  nl,
  fail.

contains(Thing,Here):-             % recursive definition to find
  location(Thing,Here).            % things contained in things etc.
contains(Thing,Here):-
  location(Thing,X),
  contains(X,Here).

is_takable(Thing):-                % you can't take the furniture
  furniture(Thing),
  respond(['You can''t pick up a ',Thing]),
  nl,
  !,fail.
  
is_takable(Thing):-
  Thing == fuel,
  not(have(gloves)),
  respond(['You need the gloves to pick up the fuel!']),
  nl,
  !,fail.  

is_takable(_).                     % not furniture, ok to take

move(Thing,have):-
  retract(location(Thing,_)),      % take it from its old place
  asserta(have(Thing)).            % and add to your possessions


 
% drop - allows the player to transfer a possession to a room

drop(Thing):-
  have(Thing),                     % you must have the thing to drop it
  here(Here),                      % where are we
  retract(have(Thing)),
  asserta(location(Thing,Here)),
  write('Item dropped!'),
  nl.
drop(Thing):-
  respond(['You don''t have the ',Thing]).
  
% inventory list your possesions

inventory:-
  have(X),                         % make sure you have at least one thing
  write('You have: '),
  nl,
  list_possessions.
inventory:-
  write('You have nothing'),
  nl.

list_possessions:-
  have(X),
  tab(2),
  write(X),
  nl,
  fail.
list_possessions.

% check the assembly manual

check_manual:-
  have('assembly manual'),
  write('Items Needed: '),nl,
  list_items,
  write('Remember, items need to be brought back to the '),nl,
  write('project space to be listed as "Retrieved."'),nl,nl.
  
check_manual:-
  write('You forgot to pick up the assembly manual!'),nl,nl.


  
list_items:-
  item(X),
  tab(2),
  write(X),
  check_status(X),
  fail.
list_items.



check_status(X):-
  
  location(X, 'project space'),
  write(', Retrieved.'),!,nl.
  
check_status(X):-
  write(', Missing.'),nl.

  
 




% respond simplifies writing a mixture of literals and variables
 
respond([]):-
  write('.'),nl,nl.
respond([H|T]):-
  write(H),
  respond(T).
  
% Simple English command listener.  It does some semantic checking
% and allows for various synonyms.  Within a restricted subset of
% English, a command can be phrased many ways.  Also non grammatical
% constructs are understood, for example just giving a room name
% is interpreted as the command to goto that room.

% Some interpretation is based on the situation.  Notice that when
% the player says turn on the light it is ambiguous.  It could mean
% the room light (which can't be turned on in the game) or the
% flash light.  If the player has the flash light it is interpreted
% as flash light, otherwise it is interpreted as room light.

get_command(C):-
  readlist(L),        % reads a sentence and puts [it,in,list,form]
  command(X,L,[]),    % call the grammar for command
  C =.. X,!.          % make the command list a structure
get_command(_):-
  respond(['I don''t understand, try again or type help']),fail.
  
% The grammar doesn't have to be real English.  There are two
% types of commands in Nani Search, those with and without a 
% single argument.  A special case is also made for the command
% goto which can be activated by simply giving a room name.

command([Pred,Arg]) --> verb(Type,Pred),nounphrase(Type,Arg).
command([Pred]) --> verb(intran,Pred).
command([goto,Arg]) --> noun(go_place,Arg).

command([talkTo,Per]) --> noun(talk_Person,Per).

% Recognize three types of verbs.  Each verb corresponds to a command,
% but there are many synonyms allowed.  For example the command
% turn_on will be triggered by either "turn on" or "switch on".

verb(go_place,goto) --> go_verb.
verb(thing,V) --> tran_verb(V).
verb(intran,V) --> intran_verb(V).

verb(talk_Person,talkTo) --> talk_verb.

go_verb --> [go].
go_verb --> [go,to].
go_verb --> [g].

talk_verb --> [talk].
talk_verb --> [talk, to].
talk_verb --> [t].

tran_verb(take) --> [take].
tran_verb(take) --> [pick,up].
tran_verb(drop) --> [drop].
tran_verb(drop) --> [put].
tran_verb(drop) --> [put,down].
tran_verb(look_in) --> [look,in].
tran_verb(look_in) --> [look].
tran_verb(look_in) --> [open].

intran_verb(check_manual) --> [check,assembly,manual].
intran_verb(inventory) --> [inventory].
intran_verb(inventory) --> [backpack].
intran_verb(inventory) --> [i].
intran_verb(look) --> [look].
intran_verb(look) --> [look,around].
intran_verb(look) --> [l].
intran_verb(quit) --> [quit].
intran_verb(quit) --> [exit].
intran_verb(quit) --> [end].
intran_verb(quit) --> [bye].
intran_verb(nshelp) --> [help].
intran_verb(hint) --> [hint].

% a noun phrase is just a noun with an optional determiner in front.

nounphrase(Type,Noun) --> det,noun(Type,Noun).
nounphrase(Type,Noun) --> noun(Type,Noun).

det --> [the].
det --> [a].

% Nouns are defined as rooms, or things located somewhere.  We define
% special cases for those things represented in Nani Search by two
% words.  We can't expect the user to type the name in quotes.

noun(go_place,R) --> [R], {room(R)}.
noun(go_place,'frey 110') --> [frey,110].
noun(go_place,'frey 252') --> [frey,252].
noun(go_place,'frey 254') --> [frey,254].
noun(go_place,'frey 256') --> [frey,256].
noun(go_place,'physics room') --> [physics,room].
noun(go_place,'outside frey') --> [outside,frey].
noun(go_place,'project space') --> [project,space].
noun(go_place,'engineering shop') -->[engineering,shop].
noun(go_place,'hydro lab') -->[hydro,lab].

noun(thing,T) --> [T], {location(T,_)}.
noun(thing,T) --> [T], {have(T)}.
noun(thing,'assembly manual') --> [assembly,manual].
noun(thing,'flight path') --> [flight,path].
noun(thing,'steering panel') --> [steering,panel].
noun(thing,'rocket stand') --> [rocket,stand].
noun(thing,'space suits') --> [space,suits].
noun(thing,'tweedy printer') --> [tweedy,printer].

noun(talk_Person, P) --> [P], {person(P)}.
noun(talk_Person, 'john meyer') --> [john,meyer].
noun(talk_Person, 'dr soerens') --> [dr,soerens].
noun(talk_Person, 'dr underwood') --> [dr,underwood].
noun(talk_Person, 'dr hellgren') --> [dr,hellgren].
noun(talk_Person, 'paul') --> [paul].


% readlist - read a list of words, based on a Clocksin & Mellish
% example.

readlist(L):-
  write('> '),
  read_word_list(L).

read_word_list([W|Ws]) :-
  get0(C),
  readword(C, W, C1),       % Read word starting with C, C1 is first new
  restsent(C1, Ws), !.      % character - use it to get rest of sentence

restsent(C,[]) :- lastword(C), !. % Nothing left if hit last-word marker
restsent(C,[W1|Ws]) :-
  readword(C,W1,C1),        % Else read next word and rest of sentence
  restsent(C1,Ws).

readword(C,W,C1) :-         % Some words are single characters
  single_char(C),           % i.e. punctuation
  !, 
  name(W, [C]),             % get as an atom
  get0(C1).
readword(C, W, C1) :-
  is_num(C),                % if we have a number --
  !,
  number_word(C, W, C1, _). % convert it to a genuine number
readword(C,W,C2) :-         % otherwise if character does not
  in_word(C, NewC),         % delineate end of word - keep
  get0(C1),                 % accumulating them until 
  restword(C1,Cs,C2),       % we have all the word     
  name(W, [NewC|Cs]).       % then make it an atom

readword(C,W,C2) :-         % otherwise
  get0(C1),       
  readword(C1,W,C2).        % start a new word

restword(C, [NewC|Cs], C2) :-
  in_word(C, NewC),
  get0(C1),
  restword(C1, Cs, C2).
restword(C, [], C).


single_char(0',).
single_char(0';).
single_char(0':).
single_char(0'?).
single_char(0'!).
single_char(0'.).


in_word(C, C) :- C >= 0'a, C =< 0'z.
in_word(C, L) :- C >= 0'A, C =< 0'Z, L is C + 32.
in_word(0'',0'').
in_word(0'-,0'-).

% Have character C (known integer) - keep reading integers and build
% up the number until we hit a non-integer. Return this in C1,
% and return the computed number in W.

number_word(C, W, C1, Pow10) :- 
  is_num(C),
  !,
  get0(C2),
  number_word(C2, W1, C1, P10),
  Pow10 is P10 * 10,
  W is integer(((C - 0'0) * Pow10) + W1).
number_word(C, 0, C, 0.1).


is_num(C) :-
  C =< 0'9,
  C >= 0'0.

% These symbols delineate end of sentence

lastword(10).   % end if new line entered
lastword(0'.).
lastword(0'!).
lastword(0'?).

 
 


  
  