-module(os_util).
-export([run_erl/10, wait_exe/1, run_exe/1]).

run_erl(Hiden, Name, Host, MnesiaDir, SmpEnable, Pa, Config, Cookie, Option, Wait) ->
	CommandLine = get_erl_cmd(Hiden, Name, Host, MnesiaDir, SmpEnable, Pa, Config, Cookie, Option),
	case Wait of
		wait ->
			wait_exe(CommandLine);
		nowait ->
			run_exe(CommandLine)
	end.

%% 操作系统命令非同步执行(prompt:提示)
wait_exe(CmdLine) ->
	io:format("CommandLine ~p~n", [CmdLine]),
	os:cmd(CmdLine).

%% 操作系统命令同步执行(prompt:提示)
run_exe(CmdLine) ->
	io:format("CommandLine ~p~n", [CmdLine]),
	cmd_ansync(CmdLine).

get_erl_cmd(Hiden, Name, Host, MnesiaDir, SmpEnable, Pa, Config, Cookie, Option) ->
	%% 是否隐藏shell窗口的参数
	HidenOption =
		case os:type() of
			{win32, nt} ->
				"";
			_->
				case Hiden of
					hiden ->
						" -detached";
					_ ->
						""
				end
		end,
	%% 得到节点名字的参数
	NameOption =
		case Name of
			[] -> "";
			_ ->
				sprintf(" -name ~s@~s ", [Name, Host])
		end,
	%% mnesia数据库的存储目录
	MnesiaOption = 
		case MnesiaDir of
			[] ->
				"";
			_ ->
				sprintf(" -mnesia dir '\"~s\"' ", [MnesiaDir])
		end,
	SmpOption =
		case SmpEnable of
			smp ->
				"";
			nosmp ->
				case os:type() of
					{win32, nt} ->
						" ";
					_ ->
						%% linux系统下让smp不要启动(让服务器不要启动跟CPU个数相同的Scheduler)
						" -smp disable "
				end
		end,
	%% 根据不同的操作系统,得到对应的启动节点的命令
	ExeCmd = 
		case os:type() of
			{win32, nt} ->
				"werl.exe";
			_ ->
				%% +P erlang节点系统的最大并发进程数, +K true | false 是否开启kernel poll，就是epoll
				%% +K true|false - 该选项用于打开(true)或关闭(false，默认)Erlang RTS的Kernel poll功能。
				%% 当Kernel poll被关闭时，RTS使用普通的用户态事件轮询接口select/poll进行进程和I/O调度，调度开销较大；
				%% 打开Kernel poll时，RTS将使用内核级事件轮询接口(如Linux上的epoll等)进行调度，开销较小，
				%% 可以提高存在大量进程时的响应速度。
				"erl +P 100000 +K true"
		end,
	%%
    PaOption = 
        case Pa of
            "" ->
                "";
            _->
                " -pa "++Pa
        end,
    %%
    ConfigOption = 
        case Config of
            ""->
                " ";
            _->
                " -config "++Config
        end,
    %%
    CookieOption = 
        case Cookie of
            ""->
                " -setcookie cookie";
            _->
                " -setcookie " ++Cookie
        end,
	%% 得到真实的启动节点的节点参数命令
	lists:append([ExeCmd, HidenOption, NameOption, MnesiaOption, SmpOption, PaOption, ConfigOption, CookieOption, " ", Option]).

%% 同步启动多个节点
cmd_ansync(CommandLine) ->
	SelfPid = self(),
	Fun = fun() -> do_cmd_ansync(CommandLine, SelfPid) end,
	proc_lib:spawn(Fun),
	receive
		ok ->
			ok;
		_ ->
			error
	end.

do_cmd_ansync(CommandLine, FatherPid) ->
	case os:type() of
		{unix, _} ->
			start_unix_cmd(CommandLine, FatherPid, no_unix_shell);
		{win32, Wtype} ->
			Command = 
				case {os:getenv("COMSPEC"), Wtype} of
					{false, windows} ->
						lists:concat(["command.com /c ", CommandLine]);
					{false, _} ->
						lists:concat(["cmd /c ", CommandLine]);
					{Cspec, _} ->
						lists:concat([Cspec, " /c ", CommandLine])
				end,
			Port = erlang:open_port({spawn, Command}, [stream, in, eof, hide]),
			FatherPid ! ok,
			get_data(Port, [])
	end.

get_data(Port, Sofar) ->
	receive
		{Port, {data, Bytes}} ->
			get_data(Port, [Sofar | Bytes]);
		{Port, eof} ->
			Port ! {self(), close},
			receive
				{Port, closed} ->
					true
			end,
			receive
				{'EXIT', Port, _} ->
					ok
			after 1 ->
					ok
			end,
			lists:flatten(Sofar)
	end.


-define(SHELL, "/bin/sh -s unix:cmd 2>&1").
-define(SHELL_HAVE_WINDOW, "xterm -e ").
-define(PORT_CREATOR_NAME, os_cmd_port_creator).

start_unix_cmd(CommandLine, FatherPid, Type) ->
	if
		Type =:= unix_shell ->
			unix_cmd_shell(CommandLine, FatherPid);
		Type =:= no_unix_shell ->
			no_unix_shell(CommandLine, FatherPid);
		true ->
			FatherPid ! {error, "Type Error"}
	end.

%% linux 有窗口的启动系统
unix_cmd_shell(CommandLine, FatherPid) ->
	case erlang:open_port({spawn, ?SHELL_HAVE_WINDOW ++ CommandLine}, [stream]) of
		Port when erlang:is_port(Port) ->
			FatherPid ! ok;
		Error ->
			io:format("Open Port Error ~p~n", [Error]),
			FatherPid ! error
	end.

%% Linux 没有窗口的启动系统
no_unix_shell(CommandLine, FatherPid) ->
	Tag = erlang:make_ref(),
	{Pid, Mref} =
		erlang:spawn_monitor(fun() ->
									 erlang:process_flag(trap_exit, true),
									 Port = start_port(),
									 erlang:port_command(Port, mk_cmd(CommandLine)),
									 FatherPid ! ok,
									 exit({Tag, unix_get_data(Port)})
									 end),
	receive
		{'DOWN', Mref, _, Pid, {Tag, Result}} ->
			Result;
		{'DOWN', Mref, _, Pid, Reason} ->
			exit(Reason)
	end.

%% 开启端口
start_port() ->
	Ref = erlang:make_ref(),
	Request = {Ref, self()},
	{Pid, Mon} = 
		case erlang:whereis(?PORT_CREATOR_NAME) of
			undefined ->
				erlang:spawn_monitor(fun() -> start_port_srv(Request)
									 end);
			P ->
				P ! Request,
				M = erlang:monitor(process, P),
				{P, M}
		end,
	receive
		{Ref, Port} when erlang:is_port(Port) ->
			erlang:demonitor(Mon, [flush]),
			Port;
		{Ref, Error} ->
			erlang:demonitor(Mon, [flush]),
			exit(Error);
		{'DOWN', Mon, process, Pid, _Reason} ->
			start_port()
	end.

start_port_srv(Request) ->
	{group_leader, GL} = erlang:process_info(erlang:whereis(kernel_sup), group_leader),
	true = group_leader(GL, self()),
	erlang:process_flag(trap_exit, true),
	StayAlive = 
		try
			erlang:register(?PORT_CREATOR_NAME, self())
		catch
			error:_ ->
				false
		end,
	start_port_srv_handle(Request),
	case StayAlive of
		true ->
			start_port_srv_loop();
		false ->
			exiting
	end.

start_port_srv_handle({Ref, Client}) ->
	Reply = 
		try
			erlang:open_port({spawn, ?SHELL}, [stream]) of
				Port when erlang:is_port(Port) ->
					(catch erlang:port_connect(Port, Client)),
					unlink(Port),
					Port
		catch
			_:Reason:StackTrace ->
				{Reason, StackTrace}
		end,
	Client ! {Ref, Reply}.

start_port_srv_loop() ->
	receive
		{Ref, Client} = Request when erlang:is_reference(Ref),
									 erlang:is_pid(Client) ->
			start_port_srv_handle(Request);
		_Junk ->
			ignore
	end,
	start_port_srv_loop().

%% We no not allow any input to Cmd (hence commands that want
%% to read from standard input will return immediately).
%% Standard error is redirected to standard ouput.
%% We use ^D (= EOT = 4) to mark the end of the stream
mk_cmd(Cmd) when erlang:is_atom(Cmd) ->
	mk_cmd(erlang:atom_to_list(Cmd));
mk_cmd(Cmd) ->
	%% We insert a new line after the command, in case the command
	%% contains o comment character.
	io_lib:format("(~s\n) </dev/null; echo  \"\^D\"\n", [Cmd]).

unix_get_data(Port) ->
	unix_get_data(Port, []).
unix_get_data(Port, Sofar) ->
	receive
		{Port, {data, Bytes}} ->
			case eot(Bytes) of
				{done, Last} ->
					lists:flatten([Sofar | Last]);
				more ->
					unix_get_data(Port, [Sofar | Bytes])
			end;
		{'EXIT', Port, _} ->
			lists:flatten(Sofar)
	end.

eot(Bs) ->
	eot(Bs, []).

eot([4 | _Bs], As) ->
	{done, lists:reverse(As)};
eot([B | Bs], As) ->
	eot(Bs, [B | As]);
eot([], _As) ->
	more.

sprintf(Format, Data) ->
	lists:flatten(io_lib:format(Format, Data)).