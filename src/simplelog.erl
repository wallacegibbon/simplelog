-module(simplelog).

-behaviour(gen_server).

-export([start_link/1, init/1, terminate/2, code_change/3,
	 handle_call/3, handle_cast/2, handle_info/2]).

-export([record/1, change_basename/1, prepare/1, stop/0, die/0]).

-define(SERVER, ?MODULE).


record(String) when is_list(String); is_binary(String) ->
    gen_server:cast(?MODULE, {record, String}).

change_basename(Basename) ->
    gen_server:call(?MODULE, {basename, Basename}).

prepare(Basename) ->
    gen_server:call(?MODULE, {prepare, Basename}).

stop() ->
    gen_server:call(?MODULE, stop).

die() ->
    gen_server:call(?MODULE, die).

handle_cast({record, _}, #{filename := ""} = State) ->
    io:format("simplelog is not prepared~n"),
    {noreply, State};

handle_cast({record, String}, #{filename := Filename, file := File,
				basename := Basename} = State) ->
    CurFilename = current_logname(Basename),
    if
	CurFilename =/= Filename ->
	    ok = file:close(File),
	    {ok, NewFile} = file:open(CurFilename, [append]),
	    ok = io:format(NewFile, "~s~n", [String]),
	    {noreply, State#{filename := CurFilename, file := NewFile}};
	true ->
	    ok = io:format(File, "~s~n", [String]),
	    {noreply, State}
    end;

handle_cast(Msg, State) ->
    io:format("unknown cast ~p~n", [Msg]),
    {noreply, State}.


handle_call({basename, Basename}, _From, #{filename := Filename,
					   file := File,
					   commontab := Commontab} = State) ->
    CurFilename = current_logname(Basename),
    if
	CurFilename =/= Filename ->
	    ok = file:close(File),
	    {ok, NewFile} = file:open(CurFilename, [append]),
	    NewState = State#{filename := CurFilename, file := NewFile,
			      basename := Basename},
	    true = ets:insert(Commontab, {initstate, NewState}),
	    {reply, changed, NewState};
	true ->
	    {reply, nochange, State}
    end;

handle_call({prepare, Basename}, _From, #{filename := "",
					  commontab := Commontab} = State) ->
    Filename = current_logname(Basename),
    {ok, File} = file:open(Filename, [append]),
    NewState = State#{filename := Filename, file := File,
		      basename := Basename},
    true = ets:insert(Commontab, {initstate, NewState}),
    {reply, ok, NewState};

handle_call({prepare, _}, _From, State) ->
    {reply, already, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(die, _From, State) ->
    command = die.

handle_info(_Info, State) ->
    {noreply, State}.


start_link(Configs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Configs, []).

init(#{commontab := Commontab}) ->
    process_flag(trap_exit, true),
    case ets:lookup(Commontab, initstate) of
	[{initstate, #{filename := Name} = State}] when Name =/= "" ->
	    {ok, File} = file:open(Name, [append]),
	    {ok, State#{file := File}};
	Any ->
	    {ok, #{filename => "", file => 0, basename => "",
		   commontab => Commontab}}
    end.

terminate(Reason, #{filename := Filename, file := File})
  when Filename =/= "" ->
    ok = file:close(File);
terminate(Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

current_logname(Basename) ->
    {{Y, M, D}, _} = calendar:local_time(),
    lists:flatten(io_lib:format("~s.~4..0w-~2..0w-~2..0w.log",
				[Basename, Y, M, D])).

