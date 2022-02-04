%% @author Arthur Zarakin, Inga Samkani
%% @date Friday, 17 July 2020
%% @title ERLang conversation client
-module(client). 
-export([start/1,send/1, exit/0, send/2, send/3, send/4]). 

start(Port) when is_integer(Port), Port/=4000 -> %when Port !=4000  %4001, 4002, 4003, 4004
	io:format("Wait a seccond, connecting to the server..."),
	{ok, Socket} = gen_udp:open(0, [binary]), 
   	ok = gen_udp:send(Socket, "localhost", Port, "Ping"),
	Value =
		receive 
    		{udp, Socket, _, _, _} ->
						io:format( "- this port " ++integer_to_list( Port )++" already exist, please start() another one -" )
				after 2000 -> %start the client port only if sent message (ping) not returned, that means that this port unused
						spawn(fun() -> createClientServer(Port) end) 
   		end, 
	gen_udp:close(Socket),
	Value
	;
		%Error handling
start(_) -> io:format("~p~n" ,["Wrong value enteredm please input number only and not the server port (4000)"]).

createClientServer(Port) ->
	persistent_term:put(clientPort, Port), %creating global variable "clientPort"

	{ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
	io:format("server opened socket:~p~n",[Socket]),
	send("client_port_add"), %port resend automatically
	clientSrverLoop(Socket). 


clientSrverLoop(Socket) ->
   inet:setopts(Socket, [{active, once}]), 
   receive			%%matching functions for server receiving orders:
	         {udp, Socket, Host, Port, <<"exit">>} ->		%message from himself
				ThisClientPort = persistent_term:get(clientPort),
				io:format("Client ~p Closed!~n",[ThisClientPort]),
				gen_udp:send(Socket, Host, Port, ""), %feedback
				gen_udp:close(Socket);			%%%loop+client_server close

			{udp, Socket, Host, Port, <<"Ping">>} ->		%message from some client port if the same already port exist
				%io:format("ping sent to this port"),
				gen_udp:send(Socket, Host, Port, ""), %feedback
				clientSrverLoop(Socket);
	   
	 		{udp, Socket, Host, Port, Binary} -> %any binary message that sent to this port
				io:format(binaryToString(Binary)), 
				gen_udp:send(Socket, Host, Port, ""), %feedback
				clientSrverLoop(Socket)
	   
		   	after 9999000 -> 	%close client loop and udp after unsing time 
					io:format("~p~n",["Client server time runs out, Client server Closed"]), 
					gen_udp:close(Socket)
   end. 


%String, String
	send(Order, ValueA) -> send(Order++"\", \""++integer_to_list( ValueA )).
%sString, Intager, Integer
	send(Order, ValueA, ValueB) when is_integer(ValueA) -> send(Order++"\", \""++ integer_to_list( ValueA ) ++"\", \""++integer_to_list( ValueB ));
%String, String, Integer
	send(Order, ValueA, ValueB) -> send(Order++"\", \""++ ValueA ++"\", \""++integer_to_list( ValueB )).
%String, String, Integer, Integer
	send(Order, ValueA, ValueB, ValueC) -> send(Order++"\", \""++ ValueA ++"\", \""++integer_to_list( ValueB )++"\", \""++integer_to_list( ValueC )).
%String
send(Data) -> 		%Send data to port 4000 server
	ThisClientPort_String = integer_to_list( persistent_term:get(clientPort) ), %global variable (4001, 4002, 4003, 4004) 
	{ok, Socket} = gen_udp:open(0, [binary]), 
   	ok = gen_udp:send(Socket, "localhost", 4000, "{\""++Data++"\",\""++ThisClientPort_String++"\"}"),
	Value =
		receive 
    		{udp, Socket, _, _, FeedbackBinary} ->
					io:format( binaryToString( FeedbackBinary ) ) 
				after 10000 ->
					io:format("~p~n",["No server (please open the server)!"]),
					exitSend("")
   		end, 
	gen_udp:close(Socket),
	Value.


exit() -> exitSend( send("client_exit") ).  %send("client_exit")
		%inform the server about closing of this client port

exitSend(AdditionalFunction) ->
	ThisClientPort = persistent_term:get(clientPort),
		{ok, Socket} = gen_udp:open(0, [binary]),  
		ok = gen_udp:send(Socket, "localhost", ThisClientPort , "exit"),
		Value =
			receive 
	    		{udp, Socket, _, _, _} ->
						AdditionalFunction,
						persistent_term:put(clientPort, 0) %set this client port data to 0 (for not use it any more after the loop closed)
					after 2000 ->
						io:format("~p~n",["No client (please start() a new one)!"])  
	   		end, 
		gen_udp:close(Socket),
		Value.


%%Helping functions:
binaryToString(Binary) ->  atom_to_list(binary_to_atom( Binary , latin1)).

