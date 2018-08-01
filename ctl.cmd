@echo off
setlocal
for /f %%i in ('escript bin/node_name') do set nodename=%%i
for /f %%i in ('escript bin/node_name main') do set nodename_main=%%i
for /f %%i in ('escript bin/node_name game') do set nodename_game=%%i
set pa=-pa ebin deps/ebin
set config=-config config/app.config
set cookie=-setcookie wxr
if "%1" == "make" (
        del /f /s /q %~dp0ebin\*.beam
	erl -noshell -pa deps/ebin tools/mmake/ebin -eval "mmake:all(8),erlang:halt(0)."
) else if "%1" == "start" (
	start werl -name %nodename% %pa% %config% %cookie% %option% -s server start
) else if "%1" == "stop" (
	erl -name stop@127.0.0.1 -noshell -hidden %cookie% -eval "rpc:call('%nodename%', server, stop, [], 5000),erlang:halt(0)."
) else if "%1" == "start_all" (
	start werl -name %nodename_main% -wxr server_type main %pa% %config% %cookie% %option% -s server start
	@ping 127.0.0.1 -n 1 >nul
	start werl -name %nodename_game% -wxr server_type game %pa% %config% %cookie% %option% -s server start
	@ping 127.0.0.1 -n 1 >nul
	start werl -name %nodename% %pa% %config% %cookie% %option% -s server start
) else if "%1" == "stop_all" (
	erl -name stop@127.0.0.1 -noshell -hidden %cookie% -eval "rpc:call('%nodename%', server, stop, [], 5000),rpc:call('%nodename_game%', server, stop, [], 5000),rpc:call('%nodename_main%', server, stop, [], 5000),erlang:halt(0)."
) else if "%1" == "proto" (
	escript %~dp0tools/gpb-4.1.9/bin/protoc-erl -I proto -o-erl src/proto -o-hrl include proto.proto
) else if "%1" == "plt" (
	dialyzer --build_plt --apps kernel stdlib compiler erts crypto sasl ssl syntax_tools inets observer
	dialyzer --add_to_plt %~dp0deps/ebin
) else if "%1" == "dialyzer" (
	dialyzer --plt %~dp0.dialyzer_plt --verbose -pa deps/ebin -r ./ebin > dialyzer.log
) else if "%1" == "patch" (
	erl -noshell -pa deps/ebin tools/mmake/ebin -eval "mmake:all(8),erlang:halt(0)."
	7z a -ttar wxr_server.tar bin deps/ebin ebin priv ctl
) else (
	echo make: compile all code
	echo start: start server
	echo stop: stop server
	echo start_all: in develop, start all type servers
	echo stop_all: in develop, stop all type servers
	echo proto: make protobuf
	echo plt: build dialyzer plt
	echo dialyzer: dialyzer code
	echo patch: create ptach tar file
)