%% @author Arthur Zarakin, Inga Samkani
%% @date Friday, 17 July 2020
%% @title ERLang conversation server
-module(server). 
-export([start/0,send/2,exit/0, portList/1, portList/0]). 


start() -> 
	Pid = spawn(?MODULE, portList, [[]]),
	spawn(fun() -> server(4000,Pid) end).

server(Port, Pid) ->
	{ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
	io:format("server opened socket:~p~n",[Socket]), 
	srverLoop(Socket, Pid). 

srverLoop(Socket,Pid) ->
   inet:setopts(Socket, [{active, once}]), 
   receive 
	  {udp, Socket, Host, Port, <<"portList">>} -> 
	  				gen_udp:send(Socket, Host, Port, ""), %feedback
					Pid ! {"Print"},
					srverLoop(Socket, Pid)
		  ;
	  {udp, Socket, Host, Port, <<"exit">>} ->
	  				io:format("server received:~p~n",["Server Closed!"]),
					gen_udp:send(Socket, Host, Port, ""), %feedback
					gen_udp:close(Socket),
					Pid ! {"Exit"}
		   ;%%%loop+server closed

      {udp, Socket, Host, Port, Binary} -> 
			spawn(fun() -> 
				serverReceiveFunction(Socket, Host, Port, binaryToTuple(Binary), Pid)
				end),
   			srverLoop(Socket, Pid)

   					%server close after....
		after 9999000 -> 	
			io:format("~p~n",["Server time runs out"]), 
			gen_udp:close(Socket)
   end. 

			
	%%matching functions for server receiving orders:
		serverReceiveFunction(Socket, Host, Port, {"client_port_add",Str_Port}, Pid) -> %one of the clients exit
						io:format("server rgistered port ~p  to list~n~n",[Str_Port]),
						Pid ! {"Add", list_to_integer( Str_Port )},
						gen_udp:send(Socket, Host, Port, "this client port "++Str_Port++" is registered to the server. ~n"), %feedback
						Pid ! {"Add_SendAllPortsMessage", list_to_integer( Str_Port )}
		;			%received message about started client, adding his port to port list
		
		serverReceiveFunction(Socket, Host, Port, {"client_exit",Str_Port}, Pid) -> %one of the clients exit
						io:format("server informed that port ~p  is off~n",[Str_Port]),
						Pid ! {"Delete", list_to_integer( Str_Port )},
						gen_udp:send(Socket, Host, Port, ""), %feedback
						Pid ! {"Delete_SendAllPortsMessage", list_to_integer( Str_Port )}
		;			%received message closing port message from the client to inform the server and the others that the port closed
		
		serverReceiveFunction(Socket, Host, Port, {"client_exit_remove_from_server_list",Str_Port}, Pid) -> %one of the clients exit
						io:format("server informed that port ~p  is off~n",[Str_Port]),
						Pid ! {"Delete", list_to_integer( Str_Port )},
						gen_udp:send(Socket, Host, Port, "") %feedback
		;			%remove this unactive port from the port list

				%Fibonachi function
		serverReceiveFunction(Socket, Host, Port, {"fib", Str_ValueA, Str_Port}, Pid) ->
						Message = "(\"fib\",\""++Str_ValueA++"\")=",
						Result = integer_to_list( fib( list_to_integer( Str_ValueA ) ) ),
						gen_udp:send(Socket, Host, Port, "sent"), %feedback
						Pid ! {"SendAllPortsMessage", list_to_integer( Str_Port ), Message, Result}
		;
				%fact function
		serverReceiveFunction(Socket, Host, Port, {"fact", Str_ValueA, Str_Port}, Pid) ->
						Message = Str_ValueA++"! =",
						Result = integer_to_list( fact( list_to_integer( Str_ValueA ) ) ), 
						gen_udp:send(Socket, Host, Port, "sent"), %feedback
						Pid ! {"SendAllPortsMessage", list_to_integer( Str_Port ), Message, Result}
		;
				%add function
		serverReceiveFunction(Socket, Host, Port, {"add", Str_ValueA, Str_ValueB, Str_Port}, Pid) ->
						Message = Str_ValueA++" + "++Str_ValueB++" = ",
						Result =  integer_to_list( list_to_integer( Str_ValueA ) + list_to_integer( Str_ValueB ) ), 
						gen_udp:send(Socket, Host, Port, "sent"), %feedback
						Pid ! {"SendAllPortsMessage", list_to_integer( Str_Port ), Message, Result}
		;
				%multiplier function
		serverReceiveFunction(Socket, Host, Port, {"mult", Str_ValueA, Str_ValueB, Str_Port}, Pid) ->
						Message = Str_ValueA++" X "++Str_ValueB++" = ",
						Result =  integer_to_list( list_to_integer( Str_ValueA ) * list_to_integer( Str_ValueB ) ),
						gen_udp:send(Socket, Host, Port, "sent"), %feedback
						Pid ! {"SendAllPortsMessage", list_to_integer( Str_Port ), Message, Result}
		;

				%line function
		serverReceiveFunction(Socket, Host, Port, {"line", Str_ValueA, Str_ValueB, Str_Port}, Pid) ->
						Message = "(\"line\",\""++Str_ValueA++"\", "++Str_ValueB++")~n",
						Result = line(Str_ValueA, list_to_integer( Str_ValueB ) ),
						gen_udp:send(Socket, Host, Port, "sent"), %feedback
						Pid ! {"SendAllPortsMessage", list_to_integer( Str_Port ), Message, Result}
		;
				%triangle function
		serverReceiveFunction(Socket, Host, Port, {"triangle", Str_ValueA, Str_ValueB, Str_Port}, Pid) ->
						Message = "(\"triangle\",\""++Str_ValueA++"\", "++Str_ValueB++")~n",
						Result = triangle(Str_ValueA, list_to_integer( Str_ValueB ) ),
						gen_udp:send(Socket, Host, Port, "sent"), %feedback
						Pid ! {"SendAllPortsMessage", list_to_integer( Str_Port ), Message, Result}
		;
 
				%rectangle function
		serverReceiveFunction(Socket, Host, Port, {"rectangle", Str_ValueA, Str_ValueB, Str_ValueC,Str_Port}, Pid) ->
				sleep(4000),	%sleeping for 4sec for show the lector about parrel proccesing  (another client will send in parralel triangle after this order and shows that this order will print rectangle after the triangle)
						Message = "(\"rectangle\", \""++Str_ValueA++"\", "++Str_ValueB++", "++Str_ValueC++")~n",
						Result = rectangle( Str_ValueA, list_to_integer( Str_ValueB ), list_to_integer( Str_ValueC ) ),
						gen_udp:send(Socket, Host, Port, "sent"), %feedback
						Pid ! {"SendAllPortsMessage", list_to_integer( Str_Port ), Message, Result}
		;
				%regular string send
		serverReceiveFunction(Socket, Host, Port, {Message,Str_Port}, Pid) ->
						gen_udp:send(Socket, Host, Port, "sent"), %feedback
						Pid ! {"SendAllPortsMessage", list_to_integer( Str_Port ), Message, ""}
		;
				%other cases, error handling
		serverReceiveFunction(Socket, Host, Port, _, _) ->
						io:format("Server Received Unknown Instruction! ~n"), 
						gen_udp:send(Socket, Host, Port, "Server Received Unknown Instruction!") %feedback
		.


	
	


%maths and algos functions
	line(Char,N) -> line("", Char,N).
	line(Total, _,0) -> Total++"~n";
	line(Total, Char,N) when N>0 -> line(Total++Char, Char, N-1).
	
	rectangle(Char, Length, Height) -> rectangle("", Char,Length, Height).
	rectangle(Total, _,_, 0) -> Total;
	rectangle(Total, Char,Length, Height) -> 
				Line = line(Char,Length),
				rectangle(Total++Line, Char,Length, Height-1).
	
	triangle(Char, Base) ->  triangle("", Char, Base, 0).
	triangle(Total, _, Base, Base) -> Total;
	triangle(Total, Char, Base, Count) ->
		Line = line(Char, Count+1),
		triangle(Total++Line, Char,Base, Count+1).

% *returning integer!
	%fibunacci function
	fib(1) -> 1;
	fib(2) -> 1;
	fib(N) -> fib(N-1) + fib(N-2).

% *returning integer!
	%fact (!) function	
	fact(0) -> 1;
	fact(1) -> 1;
	fact(N) -> N*fact(N-1).




allPortsSend([], _, _) -> ok;
allPortsSend([Head_Port | Tail_Port_List], ThisPort, Message) when ThisPort/=0 ->
					%function order here
					send(Message, Head_Port), 
					allPortsSend(Tail_Port_List, ThisPort, Message);
allPortsSend(_, _, _)  -> ok.


portList(PortList) ->		%Pid (internal looped function)
    receive
		{"SendAllPortsMessage", ThisPort ,Message, Result} -> 
			io:format("client"++integer_to_list( ThisPort )++": "++Message++"~n"), %print message on server screen as a documentation
			ExeptThisPort_PortList = lists:delete( ThisPort, PortList ),
            allPortsSend(ExeptThisPort_PortList, ThisPort, "client"++integer_to_list( ThisPort )++": "++Message++Result++"~n"),
			%ExeptPort\SentPort
			portList(PortList)
			;
		{"Print"} ->
            io:fwrite("Server registered client port list: ~w~n",[PortList]),
			portList(PortList)
			;
		{"Delete", Port} ->
			portList(lists:delete(Port,PortList))
			;
		{"Delete_SendAllPortsMessage", Port} ->
			allPortsSend(PortList, Port, "- "++integer_to_list(Port)++" is left to this conversetion - ~n"),
			portList(PortList)
			;		
		{"Add", Port} ->		
			portList( lists:merge( lists:delete(Port,PortList) ,[Port]) ) 
			;	%do not alow the same port added twice
		{"Add_SendAllPortsMessage", Port} ->
			ExeptThisPort_PortList = lists:delete( Port, PortList ),
			allPortsSend(ExeptThisPort_PortList, Port, "- "++integer_to_list(Port)++" is added to this conversetion - ~n"),		
			portList(PortList)
			;
		{"Exit"} -> ok %stop the loop
			;
		_ -> io:fwrite("~w~n",["wrong data to portList"]),
			   portList(PortList)
	end.


%sending message to client port
send(Message, Port) -> 
   {ok, Socket} = gen_udp:open(0, [binary]),  
   ok = gen_udp:send(Socket, "localhost", Port, Message),			%tempuraly 4001
   Value =
		receive 
    		{udp, Socket, _, _, FeedbackBinary} ->
				io:format( binaryToString( FeedbackBinary ) ) %feedback
				after 3000 -> io:format("~p~n",["No client"++integer_to_list( Port )++" (server)!"]),
							  %Remove client port from port list on this server
					send("{\"client_exit_remove_from_server_list\", \""++integer_to_list( Port )++"\"}", 4000) %Action if there is not such client, selft exit request
   		end, 
	gen_udp:close(Socket),
	Value.

%sending message, self order to the server, like "Exit" "portList"
sendSelfOrder(Order) -> 
	{ok, Socket} = gen_udp:open(0, [binary]),  
	ok = gen_udp:send(Socket, "localhost", 4000, Order ),
	Value =
		receive 
  		{udp, Socket, _, _, _} -> "" %No feedback
				after 2000 -> io:format("~p~n",["No server (please start the port 4000 server )!"])  
  		end, 
	gen_udp:close(Socket),
	Value.


	exit() -> sendSelfOrder("exit").
	
	portList() -> sendSelfOrder("portList").



%%Helping functions:
binaryToTuple(Binary) -> stringToTuple(binaryToString(Binary)).

binaryToString(Binary) -> atom_to_list(binary_to_atom( Binary , latin1)).

stringToTuple(String) -> 
		{ok, Terms, _} = erl_scan:string(String++"."),
		{ok, Tuple} = erl_parse:parse_term(Terms),
		Tuple.

sleep(T) ->
   receive 
   after T -> 
      true 
   end. 
