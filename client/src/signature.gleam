/// Digital signature operations for Ed25519
/// Uses Erlang's crypto module via FFI
import gleam/bit_array

/// Key pair for Ed25519 signing
pub type KeyPair {
  KeyPair(public_key: String, private_key: String)
}

/// Generate a new Ed25519 key pair
/// Returns base64-encoded public and private keys
pub fn generate_keypair() -> KeyPair {
  let #(pub_key, priv_key) = do_generate_keypair()
  KeyPair(
    public_key: base64_encode(pub_key),
    private_key: base64_encode(priv_key),
  )
}

/// Sign a message with a private key
/// Returns base64-encoded signature
pub fn sign(message: String, private_key_b64: String) -> Result(String, String) {
  case base64_decode(private_key_b64) {
    Ok(priv_key) -> {
      let msg_bytes = bit_array.from_string(message)
      let signature = do_sign(msg_bytes, priv_key)
      Ok(base64_encode(signature))
    }
    Error(_) -> Error("Invalid private key encoding")
  }
}

/// Create the message to sign for a post (title + body)
pub fn post_message(title: String, body: String) -> String {
  title <> "\n" <> body
}

// External Erlang FFI functions
@external(erlang, "signature_ffi", "generate_keypair")
fn do_generate_keypair() -> #(BitArray, BitArray)

@external(erlang, "signature_ffi", "sign")
fn do_sign(message: BitArray, private_key: BitArray) -> BitArray

@external(erlang, "signature_ffi", "base64_encode")
fn base64_encode(data: BitArray) -> String

@external(erlang, "signature_ffi", "base64_decode")
fn base64_decode(data: String) -> Result(BitArray, Nil)
