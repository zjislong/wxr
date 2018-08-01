#!/bin/bash
#control script
pwd=`pwd`
cd `dirname $0`
nodename=`escript bin/node_name`
pa='-pa ebin deps/ebin'
config='-config config/app.config'
cookie='-setcookie wxr'
option='+P 100000 +K true -detached'
rand=`openssl rand -hex 10`
case $1 in
    make)
        erl -noshell -pa deps/ebin tools/mmake/ebin -eval "mmake:all(8),erlang:halt(0)."
        ;;
    live)
        erl -name ${nodename} ${pa} ${config} ${cookie} -s server start
        ;;
    start)
        erl -name ${nodename} ${pa} ${config} ${cookie} ${option} -s server start
        ;;
    stop)
        erl -name stop_${rand}@127.0.0.1 -noshell -hidden ${cookie} -eval "rpc:call('${nodename}', server, stop, [], 5000),erlang:halt(0)."
        ;;
    debug)
        erl -name debug_${rand}@127.0.0.1 -hidden ${cookie} -remsh ${nodename}
        ;;
    node_list)
        erl -name node_list_${rand}@127.0.0.1 -noshell -hidden ${cookie} -eval "Nodes = rpc:call('${nodename}', erlang, nodes, [], 5000),io:format(\"~p~n\", [Nodes]),erlang:halt(0)."
        ;;
    debug_node)
        erl -name debug_node_${rand}@127.0.0.1 -hidden ${cookie} -remsh $2
        ;;
    patch)
        erl -noshell -pa deps/ebin tools/mmake/ebin -eval "mmake:all(8),erlang:halt(0)."
        tar cvf wxr_server.tar bin deps/ebin ebin priv ctl
        ;;
    *)
        echo make: compile all code
        echo live: start server live
	echo start: start server
	echo stop: stop server
        echo debug: remsh server
        echo node_list: list all nodes
        echo debug_node: remsh optional server
	echo patch: create ptach tar file
        ;;
esac
cd ${pwd}
