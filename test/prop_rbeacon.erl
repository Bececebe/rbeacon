-module(prop_rbeacon).
-include_lib("proper/include/proper.hrl").

prop_can_send_and_receive_any_string() ->
    numtests(10,
       ?FORALL(String, string(),
            begin
                {ok, Service} = rbeacon:new(9999),
                ok = rbeacon:set_interval(Service, 100),
                ok = rbeacon:publish(Service, unicode:characters_to_binary(String)),
                
                {ok, Client} = rbeacon:new(9999),
                ok = rbeacon:subscribe(Client, ""),%%se subscribe a oir mj y no aplica ningun filtro ""
                
                {ok, Msg, _Addr} = rbeacon:recv(Client),
                
                ok = rbeacon:close(Service),
                ok = rbeacon:close(Client),
                %collect para capturar estadisticas de las pruebas
                % collect(length(String),
                %measure para capturar tb estadisticas -> le da una etiqueta a la medida
                %measure saca min max etc etc 
                measure("Lonxitude do string",
                        length(String),
                % the assert must be the last thing the property does
                equals(unicode:characters_to_binary(String), Msg)
                        )

            end
             )
        ).


