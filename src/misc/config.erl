-module(config).
-export([load_config/1]).

load_config(FName)->
	case load_file(FName) of
    {ok, NewEnv} ->
		  NewEnv;
		{error, _} ->
			[]
	end.

load_file(File) ->
  %% We can't use file:consult/1 here. Too bad.
  case erl_prim_loader:get_file(File) of
	  {ok, Bin, _FileName} ->
	    %% Make sure that there is some whitespace at the end of the string
	    %% (so that reading a file with no NL following the "." will work).
      case file_binary_to_list(Bin) of
          {ok, String} ->
              scan_file(String ++ " ");
          error ->
              {error, {none, scan_file, "bad encoding"}}
      end;
	  error ->
	    {error, {none, open_file, "configuration file not found"}}
  end.

scan_file(Str) ->
    case erl_scan:tokens([], Str, 1) of
	{done, {ok, Tokens, _}, Left} ->
	    case erl_parse:parse_term(Tokens) of
		{ok,L}=Res when is_list(L) ->
		    case only_ws(Left) of
			true ->
			    Res;
			false ->
			    %% There was trailing garbage found after the list.
			    config_error()
		    end;
		{ok,_} ->
		    %% Parsing succeeded but the result is not a list.
		    config_error();
		Error ->
		    Error
	    end;
	{done, Result, _} ->
	    {error, {none, parse_file, tuple_to_list(Result)}};
	{more, _} ->
	    {error, {none, load_file, "no ending <dot> found"}}
    end.

only_ws([C|Cs]) when C =< $\s -> only_ws(Cs);
only_ws([$%|Cs]) -> only_ws(strip_comment(Cs));   % handle comment
only_ws([_|_]) -> false;
only_ws([]) -> true.
    
strip_comment([$\n|Cs]) -> Cs;
strip_comment([_|Cs]) -> strip_comment(Cs);
strip_comment([]) -> [].

config_error() ->
    {error,
     {none, load_file,
      "configuration file must contain ONE list ended by <dot>"}}.

file_binary_to_list(Bin) ->
    Enc = case epp:read_encoding_from_binary(Bin) of
              none -> epp:default_encoding();
              Encoding -> Encoding
          end,
    case catch unicode:characters_to_list(Bin, Enc) of
        String when is_list(String) ->
            {ok, String};
        _ ->
            error
    end.