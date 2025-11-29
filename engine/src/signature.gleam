/// Signature module for Ed25519 digital signatures
/// Uses Erlang's public_key module for cryptographic operations
import gleam/bit_array
import gleam/result

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

/// Verify a signature against a message and public key
pub fn verify(
  message: String,
  signature_b64: String,
  public_key_b64: String,
) -> Result(Bool, String) {
  case base64_decode(signature_b64), base64_decode(public_key_b64) {
    Ok(signature), Ok(pub_key) -> {
      let msg_bytes = bit_array.from_string(message)
      Ok(do_verify(msg_bytes, signature, pub_key))
    }
    _, _ -> Error("Invalid encoding")
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

@external(erlang, "signature_ffi", "verify")
fn do_verify(
  message: BitArray,
  signature: BitArray,
  public_key: BitArray,
) -> Bool

@external(erlang, "signature_ffi", "base64_encode")
fn base64_encode(data: BitArray) -> String

@external(erlang, "signature_ffi", "base64_decode")
fn base64_decode(data: String) -> Result(BitArray, Nil)
