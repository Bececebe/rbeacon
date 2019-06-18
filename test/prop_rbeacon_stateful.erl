-module(prop_rbeacon_stateful).
-include_lib("proper/include/proper.hrl").

-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).
-export([close/1, publish/2, broadcast_ip/1, subscribe/2, recv/1, unsubscribe/1, silence/1, noecho/1,
		new2/0,close2/1, publish2/2, broadcast_ip2/1, subscribe2/2, recv2/1, unsubscribe2/1, silence2/1, noecho2/1]).

-record(test_state,{rbeacon = null,
                    message = null,
                    subscribed = false,
                    noecho = false,
                    
                    rbeacon2 = null,
                    message2 = null,
                    subscribed2 = false,
                    noecho2 = false}).

%% @doc Default property
prop_prop_rbeacon_stateful() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                {H, S, Res} = run_commands(?MODULE, Cmds),% devuelve  historia comandos,estado final del test acumulado, resultado final 
                ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                    [H, S, Res]),
                          aggregate(command_names(Cmds), Res =:= ok))
            end).

%% @doc Returns the state in which each test case starts.
%
initial_state() ->
    #test_state{}.%registro 

%% @doc Command generator, S is the current state
command(S) ->
    oneof([
		   {call, rbeacon, new , [user_udp_port()]} ,
           {call, ?MODULE, close, [S#test_state.rbeacon]},
           {call, ?MODULE, publish, [S#test_state.rbeacon, binary()]},
           {call, ?MODULE, broadcast_ip, [S#test_state.rbeacon]},
           {call, ?MODULE, subscribe, [S#test_state.rbeacon,""]},%filtra todos los mj ""
           {call, ?MODULE, recv, [S#test_state.rbeacon]},
           {call, ?MODULE, unsubscribe, [S#test_state.rbeacon]},
           {call, ?MODULE, silence, [S#test_state.rbeacon]},
           {call, ?MODULE, noecho, [S#test_state.rbeacon]},
           
           %{call, ?MODULE, new2},
           {call, ?MODULE, close2, [S#test_state.rbeacon2]},
           {call, ?MODULE, publish2, [S#test_state.rbeacon2, binary()]},
           {call, ?MODULE, broadcast_ip2, [S#test_state.rbeacon2]},
           {call, ?MODULE, subscribe2, [S#test_state.rbeacon2,""]},%filtra todos los mj ""
           {call, ?MODULE, recv2, [S#test_state.rbeacon2]},
           {call, ?MODULE, unsubscribe2, [S#test_state.rbeacon2]},
		   {call, ?MODULE, silence2, [S#test_state.rbeacon2]},
           {call, ?MODULE, noecho2, [S#test_state.rbeacon2]}
          ]).

% UDP ports 49152 through 65535
user_udp_port() ->
    integer(49152, 65535).

%% @doc Next state transformation, S is the current state. Returns next state.
next_state(S, V, {call, rbeacon, new, _Port}) when (S#test_state.rbeacon == null) ->
    S#test_state{rbeacon = V};
next_state(S, _V, {call, ?MODULE, close, _Beacon}) ->
    S#test_state{rbeacon = null,subscribed = false,message = null,noecho=false};
next_state(S, _V, {call, ?MODULE, publish, [_Beacon, Binary]}) ->
    S#test_state{message = Binary};
next_state(S, _V, {call, ?MODULE, broadcast_ip, _Beacon}) ->
    S=S;
next_state(S, _V, {call, ?MODULE, subscribe,[_Beacon, _Binary]}) ->
     S#test_state{subscribed = true};
next_state(S, _V, {call, ?MODULE, recv, _Beacon}) ->
    S=S;
next_state(S, _V, {call, ?MODULE, unsubscribe,[_Beacon]}) ->
     S#test_state{subscribed = false};
next_state(S, _V, {call, ?MODULE, silence, [_Beacon]}) ->
    S#test_state{message = null};
next_state(S, _V, {call, ?MODULE, noecho, [_Beacon]}) ->
    S#test_state{noecho = true};

next_state(S, V, {call, rbeacon, new, _Port}) when (S#test_state.rbeacon2 == null) ->
    S#test_state{rbeacon2 = V};
next_state(S, V, {call, ?MODULE, new2}) ->
    S#test_state{rbeacon2 = V};
next_state(S, _V, {call, ?MODULE, close2, _Beacon}) ->
    S#test_state{rbeacon2 = null,subscribed2 = false,message2 = null,noecho2=false};
next_state(S, _V, {call, ?MODULE, publish2, [_Beacon, Binary]}) ->
    S#test_state{message2 = Binary};
next_state(S, _V, {call, ?MODULE, broadcast_ip2, _Beacon}) ->
    S=S;
next_state(S, _V, {call, ?MODULE, subscribe2,[_Beacon, _Binary]}) ->
     S#test_state{subscribed2 = true};
next_state(S, _V, {call, ?MODULE, recv2, _Beacon}) ->
    S=S;
next_state(S, _V, {call, ?MODULE, unsubscribe2,[_Beacon]}) ->
     S#test_state{subscribed2 = false};
next_state(S, _V, {call, ?MODULE, silence2, [_Beacon]}) ->
    S#test_state{message2 = null};
next_state(S, _V, {call, ?MODULE, noecho2, [_Beacon]}) ->
    S#test_state{noecho2 = true}.

% para indicar cuando NO quiero un comando con esto indico que las llamadas pertinentes son new close new close new close
%% @doc Precondition, checked before command is added to the command sequence.
precondition(S, {call, rbeacon, new, _Port}) when (S#test_state.rbeacon == null) ->
    S#test_state.rbeacon == null;
precondition(S, {call, ?MODULE, close, _Beacon}) ->
    S#test_state.rbeacon =/= null;
precondition(S, {call, ?MODULE, publish, [_Beacon, _Binary]}) ->
    S#test_state.rbeacon =/= null;
precondition(S, {call, ?MODULE, broadcast_ip, _Beacon}) ->
    S#test_state.rbeacon =/= null;
precondition(S, {call, ?MODULE, subscribe, [_Beacon, _Binary]}) ->
    S#test_state.rbeacon =/= null;
precondition(S, {call, ?MODULE, recv, [_Beacon]}) ->
    ((S#test_state.rbeacon =/= null ) and (S#test_state.message =/= null) and (S#test_state.subscribed == true)and (S#test_state.noecho==false)) or 
     ((S#test_state.rbeacon =/= null ) and (S#test_state.rbeacon2 =/= null ) and (S#test_state.message2 =/= null) and (S#test_state.subscribed == true));
precondition(S, {call, ?MODULE, unsubscribe, [_Beacon]}) ->
    (S#test_state.rbeacon =/= null ) and (S#test_state.subscribed == true);
precondition(S, {call, ?MODULE, silence, [_Beacon]}) ->
    S#test_state.rbeacon =/= null;    
precondition(S, {call, ?MODULE, noecho, [_Beacon]}) ->
    S#test_state.rbeacon =/= null;

precondition(S, {call, rbeacon, new, _Port}) when (S#test_state.rbeacon2 == null) ->
    S#test_state.rbeacon2 == null;    
precondition(S, {call, ?MODULE, new2}) ->
    S#test_state.rbeacon2 == null;
precondition(S, {call, ?MODULE, close2, _Beacon}) ->
    S#test_state.rbeacon2 =/= null;
precondition(S, {call, ?MODULE, publish2, [_Beacon, _Binary]}) ->
    S#test_state.rbeacon2 =/= null;
precondition(S, {call, ?MODULE, broadcast_ip2, _Beacon}) ->
    S#test_state.rbeacon2 =/= null;
precondition(S, {call, ?MODULE, subscribe2, [_Beacon, _Binary]}) ->
    S#test_state.rbeacon2 =/= null;
precondition(S, {call, ?MODULE, recv2, [_Beacon]}) ->
    ((S#test_state.rbeacon2 =/= null ) and (S#test_state.message2 =/= null) and (S#test_state.subscribed2 == true)and (S#test_state.noecho2==false)) or 
     ((S#test_state.rbeacon2 =/= null ) and (S#test_state.rbeacon =/= null ) and (S#test_state.message =/= null) and (S#test_state.subscribed2 == true));
precondition(S, {call, ?MODULE, unsubscribe2, [_Beacon]}) ->
    (S#test_state.rbeacon2 =/= null ) and (S#test_state.subscribed2 == true);
precondition(S, {call, ?MODULE, silence2, [_Beacon]}) ->
    S#test_state.rbeacon2 =/= null;    
precondition(S, {call, ?MODULE, noecho2, [_Beacon]}) ->
    S#test_state.rbeacon2 =/= null;
precondition(_S, _Call) ->
    false.


%% @doc Postcondition, checked after command has been evaluated
%%      Note: S is the state before next_state(S, _, Call)
postcondition(_S, {call, rbeacon, new, _Port}, {ok, Beacon})  ->
    is_pid(Beacon);
postcondition(_S, {call, ?MODULE, publish, [_Beacon, _Binary]}, ok) ->
    true;
    
postcondition(_S, {call, ?MODULE, broadcast_ip, _Beacon}, {msg}) ->%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%555????????????????????''''
	false;
	%true;
    %is_bitstring (msg);
    %is_bitstring (unicode:characters_to_binary(msg));
    %is_binary(msg);
postcondition(_S, {call, ?MODULE, subscribe, [_Beacon, _Binary]}, ok) ->
    true;
postcondition(_S, {call, ?MODULE, recv, _Beacon}, {ok, msg, dir}) ->%deberia recibir algo como {ok,<<"String">>,{192,168,1,35}}
    %msg==_S#test_state.message;
    false;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%555????????????????????''''
postcondition(_S, {call, ?MODULE, unsubscribe, [_Beacon]}, ok) ->
    true;
postcondition(_S, {call, ?MODULE, silence, [_Beacon]}, ok) ->
    true;
postcondition(_S, {call, ?MODULE, noecho, [_Beacon]}, ok) ->
    true;

  
postcondition(_S, {call, ?MODULE, new2}, {ok, Beacon}) ->
    is_pid(Beacon);
postcondition(_S, {call, ?MODULE, publish2, [_Beacon, _Binary]}, ok) ->
    true;
    
postcondition(_S, {call, ?MODULE, broadcast_ip2, _Beacon}, {msg}) ->%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%555????????????????????''''
	false;
	%true;
    %is_bitstring (msg);
    %is_bitstring (unicode:characters_to_binary(msg));
    %is_binary(msg);
postcondition(_S, {call, ?MODULE, subscribe2, [_Beacon, _Binary]}, ok) ->
    true;
postcondition(_S, {call, ?MODULE, recv2, _Beacon}, {ok, msg, dir}) ->%deberia recibir algo como {ok,<<"String">>,{192,168,1,35}}
    %msg==_S#test_state.message;
    false;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%555????????????????????''''
postcondition(_S, {call, ?MODULE, unsubscribe2, [_Beacon]}, ok) ->
    true;
postcondition(_S, {call, ?MODULE, silence2, [_Beacon]}, ok) ->
    true;
postcondition(_S, {call, ?MODULE, noecho2, [_Beacon]}, ok) ->
    true;
postcondition(_S, _Call, _Res) ->
    true.


     

%% Wrappers para el modelo
close({ok, Beacon}) ->
    rbeacon:close(Beacon).

publish({ok, Beacon}, Binary) ->
    rbeacon:publish(Beacon, Binary).
    
broadcast_ip({ok, Beacon}) ->
    rbeacon:broadcast_ip(Beacon).

subscribe({ok, Beacon}, Binary) ->
    rbeacon:subscribe(Beacon, Binary).

recv({ok, Beacon}) ->
    rbeacon:recv(Beacon).

unsubscribe({ok, Beacon}) ->
    rbeacon:unsubscribe(Beacon).
    
silence({ok, Beacon}) ->
    rbeacon:silence(Beacon).
    
noecho({ok, Beacon}) ->
    rbeacon:noecho(Beacon).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Mismas funciones pero para un segundo Beacon
close2({ok, Beacon}) ->
    rbeacon:close(Beacon).

publish2({ok, Beacon}, Binary) ->
    rbeacon:publish(Beacon, Binary).
    
broadcast_ip2({ok, Beacon}) ->
    rbeacon:broadcast_ip(Beacon).

subscribe2({ok, Beacon}, Binary) ->
    rbeacon:subscribe(Beacon, Binary).

recv2({ok, Beacon}) ->
    rbeacon:recv(Beacon).

unsubscribe2({ok, Beacon}) ->
    rbeacon:unsubscribe(Beacon).
    
silence2({ok, Beacon}) ->
    rbeacon:silence(Beacon).
    
noecho2({ok, Beacon}) ->
    rbeacon:noecho(Beacon).

new2() ->
    rbeacon:new([user_udp_port()]).
