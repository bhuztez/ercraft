-module(ercraft_cipher).

-export([encrypt/3, decrypt/3]).

next_iv(_IV, Data) when byte_size(Data) >= 16 ->
    binary:part(Data, {byte_size(Data), -16});
next_iv(IV, Data) ->
    Len = byte_size(Data),
    <<_:(Len)/binary, IV1/binary>> = IV,
    <<IV1/binary, Data/binary>>.

encrypt(Key, IV, PlainText) ->
    CipherText = crypto:block_encrypt(aes_cfb8, Key, IV, PlainText),
    NextIV = next_iv(IV, CipherText),
    {CipherText, NextIV}.

decrypt(Key, IV, CipherText) ->
    PlainText = crypto:block_decrypt(aes_cfb8, Key, IV, CipherText),
    NextIV = next_iv(IV, CipherText),
    {PlainText, NextIV}.
