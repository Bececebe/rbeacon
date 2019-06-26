-module(prop_rbeacon).
-include_lib("proper/include/proper.hrl").

%-export([random_port/0]).

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
        
%49152-65535: Son puertos dinámicos o puertos privados
random_port() ->
    49155. 
    
%Prueba basada en propiedades: Probamos que se puede crear el beacon y que este es capaz de compiortarse como tal
%Probamos por ejemplo que el beacon es capaz de publicar ,de subscribirse ,de parar de comunicar y  de recibir usando el puerto escogido aleatoriamente
prop_can_use_different_port() ->
    numtests(10,
       ?FORALL(String,  string(),
            begin
				Port=random_port(),
                {ok, Service} = rbeacon:new(Port),
                ok = rbeacon:set_interval(Service, 100),
                ok = rbeacon:publish(Service, unicode:characters_to_binary("Hola Ana")),
                
                {ok, Client_and_server} = rbeacon:new(Port),
                ok = rbeacon:subscribe(Client_and_server, ""),
                
                {ok, _, _Addr} = rbeacon:recv(Client_and_server),
                
                ok = rbeacon:set_interval(Client_and_server, 100),
                ok=rbeacon:silence(Service),%silenciamos el beacon Service para que este no escuche su propio mj y fracase por tanto la prueba
                ok = rbeacon:publish(Client_and_server, unicode:characters_to_binary("Hola Sara")),
                
                ok = rbeacon:subscribe(Service, ""),
                {ok, Msg_2, _Addr} = rbeacon:recv(Client_and_server),
                
                ok = rbeacon:close(Service),
                ok = rbeacon:close(Client_and_server),
                measure(
						"Lonxitude do string",
                        length("Hola Ana"),
                
               equals(unicode:characters_to_binary("Hola Sara"), Msg_2)
                        )

            end
             )
        ).
        
prop_loquesea() ->
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
