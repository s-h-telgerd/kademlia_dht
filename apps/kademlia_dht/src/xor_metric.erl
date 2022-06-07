%%
%% 
%%

-module(xor_metric).

-export([distance/3]).
%-export([byte_to_bit/1]).
-import(storage, [test/0]).

%%%%%%%%%%%%
%%   API  %%
%%%%%%%%%%%%

%%
%% distance(ID_x, ID_y, Position) -> Result
%%  ID_x, ID_y = 2^5 Octets Node ID
%%  Position = number 2^0 to 256^32, the position of specific bit in ID octets, which needs to compute
%%  Result =
%%      boolean value, true | false, the xor metric as logical distance
%%          false=ID_x and ID_y have same value on the position, logical distance is 0
%%          true=ID_x and ID_y have different value on the position, logical distance is 1
%%      {error, bad_argument}, ID_x, ID_y or Position is/are invalid
%%

distance(ID_x, ID_y, Position) 
    %%  ID_x guards
  when is_binary(ID_x) andalso size(ID_x) == 32
    %%  ID_y guards
   andalso is_binary(ID_y) andalso size(ID_y) == 32
    %%  Position guards
   andalso is_number(Position) andalso Position > 0->
    ID_x_logic = bit_in_bytes(ID_x, Position),
    ID_y_logic = bit_in_bytes(ID_y, Position),
    ID_x_logic xor ID_y_logic;
distance(_ID_x, _ID_y, _Position) ->
    {error, bad_argument}.

%%%%%%%%%%%%
%%   BIF  %%
%%%%%%%%%%%%

%%
%% bit_in_bytes(Bytes, Position) -> Result
%%  Bytes = 2^8 Octets Node ID
%%  Position = number 2^0 to 256^32, the position of specific bit in Bytes octets, which needs to derive
%%  Result = 
%%      Bit, the boolean value of a specific bit in a byte on given position
%%
 
bit_in_bytes(Bytes, Position) ->
    {Byte_index, Bit_index} =
        case Position rem 8 of
            0 -> {round(Position/8), 8};
            Rem -> {floor(Position/8)+1, Rem}
        end,
    <<Byte>> = binary:part(Bytes, {Byte_index, -1}),
    Bit_list = byte_to_bit(Byte),
    Bit = lists:nth(Bit_index, Bit_list),
    Bit.
%%
%% byte_to_bit(Byte) -> Result
%%  Byte = number value of an Octet, 0 < Byte < 256
%%  Result = 
%%      Bit_list, boolean value list of an Octet, [Val0,..,Val7]
%%          Val0,..,Val7    true | false
%%
byte_to_bit(_Byte=0) ->
    lists:duplicate(8, false);
byte_to_bit(Byte) ->
    byte_to_bit(Byte, [], _Iter = 7, _Bit_list_val = 0).
%%
%% byte_to_bit(Byte, Bit_list, Iter, Bit_list_val) -> Result
%%  Byte = number value of an Octet, 0 < Byte < 256
%%  Bit_list = boolean value list to develop
%%  Iter = iteration index of recursion
%%  Result = 
%%      Bit_list, boolean value list of an Octet, [Val0,..,Val7]
%%          Valk=0,..,Valk=7 :: 2^0..2^7  ,Valk = true | false
%%  Example:
%%      byte_to_bit(31).
%%          [true,true,true,true,true,false,false,false]
%%
byte_to_bit(_Byte, Bit_list, Iter, _Bit_list_val) when Iter < 0 ->
    Bit_list;
byte_to_bit(Byte, Bit_list, Iter, Bit_list_val) when Byte == Bit_list_val ->
    New_bit_list = [false] ++ Bit_list,
    byte_to_bit(Byte, New_bit_list, Iter - 1, Bit_list_val);
byte_to_bit(Byte, Bit_list, Iter, Bit_list_val) ->
    Bit_val = math:pow(2, Iter),
    Current_val = Bit_val + Bit_list_val,

    case Byte >= Current_val of
        true ->
            New_bit_list = [true] ++ Bit_list,
            New_bit_list_val = Current_val;
        false ->
            New_bit_list = [false] ++ Bit_list,
            New_bit_list_val = Bit_list_val
    end,
    byte_to_bit(Byte, New_bit_list, Iter - 1, New_bit_list_val).
