-module(simplelog).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, handle_info/2,
	 init/1, terminate/2, code_change/3]).

-export([start_link/1, stop/0]).

-export([record/1, change_basename/1, prepare/1]).

-define(SERVER, ?MODULE).


record(String) when is_list(String); is_binary(String) ->
    gen_server:cast(?MODULE, {record, String}).

change_basename(Basename) ->
    gen_server:call(?MODULE, {basename, Basename}).

prepare(Basename) ->
    gen_server:call(?MODULE, {prepare, Basename}).

stop() ->
    gen_server:call(?MODULE, stop).

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


handle_call({basename, Basename}, _From,
	    #{filename := Filename, file := File,
	      commontab := Commontab} = State) when Filename =/= "" ->
    CurFilename = current_logname(Basename),
    if
	CurFilename =/= Filename ->
	    ok = file:close(File),
	    {ok, NewFile} = file:open(CurFilename, [append]),
	    NewState = State#{filename := CurFilename, file := NewFile,
			      basename := Basename},
	    true = ets:insert(Commontab,
			      {top_state,
			       maps:without([commontab], NewState)}),
	    {reply, changed, NewState};
	true ->
	    {reply, nochange, State}
    end;

handle_call({basename, _}, _From, State) ->
    {reply, unready, State};

handle_call({prepare, Basename}, _From, #{filename := "",
					  commontab := Commontab} = State) ->
    Filename = current_logname(Basename),
    {ok, File} = file:open(Filename, [append]),
    NewState = State#{filename := Filename, file := File,
		      basename := Basename},
    true = ets:insert(Commontab, {top_state,
				  maps:without([commontab], NewState)}),
    {reply, ok, NewState};

handle_call({prepare, _}, _From, State) ->
    {reply, already, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_info(_Info, State) ->
    {noreply, State}.


start_link(Configs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Configs, []).

init(#{commontab := Commontab}) ->
    process_flag(trap_exit, true),
    case ets:lookup(Commontab, top_state) of
	[{top_state, #{filename := Name} = State}] when Name =/= "" ->
	    {ok, File} = file:open(Name, [append]),
	    {ok, State#{file := File, commontab => Commontab}};
	_ ->
	    {ok, #{filename => "", file => 0, basename => "",
		   commontab => Commontab}}
    end.

terminate(_Reason, #{filename := Filename, file := File})
  when Filename =/= "" ->
    ok = file:close(File);
terminate(_Reason, _) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

current_logname(Basename) ->
    {{Y, M, D}, _} = calendar:local_time(),
    lists:flatten(io_lib:format("~s.~4..0w-~2..0w-~2..0w.log",
				[Basename, Y, M, D])).

