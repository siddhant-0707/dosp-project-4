-module(signature_ffi).
-export([generate_keypair/0, sign/2, verify/3, base64_encode/1, base64_decode/1]).

%% Generate Ed25519 key pair
%% Returns {PublicKey, PrivateKey} as raw binary
generate_keypair() ->
    %% crypto:generate_key returns different formats in different OTP versions
    %% Try to handle both map format (newer) and tuple format (older)
    case crypto:generate_key(eddsa, ed25519) of
        #{public := PubKey, secret := PrivKey} ->
            {PubKey, PrivKey};
        {PubKey, PrivKey} ->
            {PubKey, PrivKey}
    end.

%% Sign a message with Ed25519 private key
sign(Message, PrivateKey) ->
    crypto:sign(eddsa, none, Message, [PrivateKey, ed25519]).

%% Verify Ed25519 signature
verify(Message, Signature, PublicKey) ->
    crypto:verify(eddsa, none, Message, Signature, [PublicKey, ed25519]).

%% Base64 encode binary to string
base64_encode(Data) ->
    list_to_binary(base64:encode_to_string(Data)).

%% Base64 decode string to binary
base64_decode(Data) ->
    try
        Decoded = base64:decode(Data),
        {ok, Decoded}
    catch
        _:_ -> {error, nil}
    end.
