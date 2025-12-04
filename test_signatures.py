#!/usr/bin/env python3
"""
Test script for digital signature functionality
Tests the two requirements:
1. Every post has an accompanying signature computed at time of posting
2. Each time a post is "downloaded", the digital signature is checked for correctness
"""

import requests
import base64
import json
from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey, Ed25519PublicKey
from cryptography.hazmat.primitives import serialization

BASE_URL = "http://localhost:8080/api"

class SignatureTestClient:
    def __init__(self):
        self.base_url = BASE_URL
    
    def _request(self, method: str, endpoint: str, data: dict = None) -> dict:
        url = f"{self.base_url}{endpoint}"
        if method == "GET":
            response = requests.get(url)
        else:
            response = requests.post(url, json=data)
        response.raise_for_status()
        return response.json()
    
    def generate_keypair_local(self):
        """Generate Ed25519 keypair locally (client-side)"""
        private_key = Ed25519PrivateKey.generate()
        public_key = private_key.public_key()
        
        # Export keys as raw bytes
        private_bytes = private_key.private_bytes(
            encoding=serialization.Encoding.Raw,
            format=serialization.PrivateFormat.Raw,
            encryption_algorithm=serialization.NoEncryption()
        )
        public_bytes = public_key.public_bytes(
            encoding=serialization.Encoding.Raw,
            format=serialization.PublicFormat.Raw
        )
        
        return {
            "private_key": base64.b64encode(private_bytes).decode('utf-8'),
            "public_key": base64.b64encode(public_bytes).decode('utf-8'),
            "private_key_obj": private_key,
            "public_key_obj": public_key
        }
    
    def sign_message_local(self, message: str, private_key_b64: str) -> str:
        """Sign a message locally (client-side)"""
        private_bytes = base64.b64decode(private_key_b64)
        private_key = Ed25519PrivateKey.from_private_bytes(private_bytes)
        signature = private_key.sign(message.encode('utf-8'))
        return base64.b64encode(signature).decode('utf-8')
    
    def verify_signature_local(self, message: str, signature_b64: str, public_key_b64: str) -> bool:
        """Verify a signature locally"""
        try:
            public_bytes = base64.b64decode(public_key_b64)
            public_key = Ed25519PublicKey.from_public_bytes(public_bytes)
            signature = base64.b64decode(signature_b64)
            public_key.verify(signature, message.encode('utf-8'))
            return True
        except Exception as e:
            print(f"Verification failed: {e}")
            return False
    
    def register(self, username: str, public_key: str) -> dict:
        return self._request("POST", "/register", {
            "username": username,
            "public_key": public_key
        })
    
    def create_subreddit(self, name: str) -> dict:
        return self._request("POST", "/subreddits", {"name": name})
    
    def join_subreddit(self, user_id: int, subreddit_id: int) -> dict:
        return self._request("POST", f"/subreddits/{subreddit_id}/join", {"user_id": user_id})
    
    def create_post(self, subreddit_id: int, author_id: int, title: str, body: str, signature: str) -> dict:
        return self._request("POST", "/posts", {
            "subreddit_id": subreddit_id,
            "author_id": author_id,
            "title": title,
            "body": body,
            "signature": signature
        })
    
    def get_post(self, post_id: int) -> dict:
        return self._request("GET", f"/posts/{post_id}")
    
    def get_post_verified(self, post_id: int) -> dict:
        """Get post with server-side signature verification"""
        return self._request("GET", f"/posts/{post_id}/verified")


def main():
    client = SignatureTestClient()
    
    print("=" * 70)
    print("DIGITAL SIGNATURE FUNCTIONALITY TEST")
    print("=" * 70)
    print()
    
    # Test 1: Generate keypair CLIENT-SIDE
    print("✓ TEST 1: Client-Side Keypair Generation")
    print("-" * 70)
    alice_keys = client.generate_keypair_local()
    print(f"Generated Alice's keypair:")
    print(f"  Public Key:  {alice_keys['public_key'][:32]}...")
    print(f"  Private Key: {alice_keys['private_key'][:32]}...")
    print()
    
    bob_keys = client.generate_keypair_local()
    print(f"Generated Bob's keypair:")
    print(f"  Public Key:  {bob_keys['public_key'][:32]}...")
    print(f"  Private Key: {bob_keys['private_key'][:32]}...")
    print()
    
    # Test 2: Register users with public keys
    print("✓ TEST 2: Register Users with Public Keys")
    print("-" * 70)
    alice = client.register("alice_sig_test", alice_keys['public_key'])
    print(f"Registered Alice (ID: {alice['id']}) with public key")
    
    bob = client.register("bob_sig_test", bob_keys['public_key'])
    print(f"Registered Bob (ID: {bob['id']}) with public key")
    print()
    
    # Test 3: Create subreddit
    print("✓ TEST 3: Create Subreddit")
    print("-" * 70)
    subreddit = client.create_subreddit("signature_test")
    print(f"Created subreddit 'signature_test' (ID: {subreddit['id']})")
    client.join_subreddit(alice['id'], subreddit['id'])
    print(f"Alice joined subreddit")
    print()
    
    # Test 4: Create post with CLIENT-SIDE signature
    print("✓ TEST 4: Create Post with Client-Side Signature")
    print("-" * 70)
    title = "Testing Digital Signatures"
    body = "This post demonstrates client-side signing with Ed25519"
    
    # Compute message to sign (title + newline + body, same as server)
    message_to_sign = f"{title}\n{body}"
    print(f"Message to sign: '{message_to_sign}'")
    
    # Sign the message CLIENT-SIDE
    signature = client.sign_message_local(message_to_sign, alice_keys['private_key'])
    print(f"Generated signature (client-side): {signature[:32]}...")
    
    # Verify signature CLIENT-SIDE before sending
    is_valid_local = client.verify_signature_local(message_to_sign, signature, alice_keys['public_key'])
    print(f"Client-side verification: {'✓ PASSED' if is_valid_local else '✗ FAILED'}")
    print()
    
    # Send post to server WITH signature
    print("Sending post to server with signature...")
    post = client.create_post(subreddit['id'], alice['id'], title, body, signature)
    print(f"✓ Post created (ID: {post['id']})")
    print(f"  Signature in response: {post.get('signature', 'N/A')[:32] if post.get('signature') else 'N/A'}...")
    print()
    
    # Test 5: Download post and verify signature SERVER-SIDE
    print("✓ TEST 5: Download Post and Verify Signature (Server-Side)")
    print("-" * 70)
    print("Fetching post with signature verification...")
    verified_result = client.get_post_verified(post['id'])
    
    print(f"Post retrieved:")
    print(f"  Title: {verified_result['post']['title']}")
    print(f"  Body: {verified_result['post']['body']}")
    print(f"  Signature: {verified_result['post'].get('signature', 'N/A')[:32] if verified_result['post'].get('signature') else 'N/A'}...")
    print(f"  Server verification result: {'✓ VALID' if verified_result['signature_verified'] else '✗ INVALID'}")
    print()
    
    # Test 6: Verify signature CLIENT-SIDE on downloaded post
    print("✓ TEST 6: Verify Downloaded Post Signature (Client-Side)")
    print("-" * 70)
    downloaded_post = client.get_post(post['id'])
    downloaded_message = f"{downloaded_post['title']}\n{downloaded_post['body']}"
    downloaded_signature = downloaded_post.get('signature', '')
    
    if downloaded_signature:
        is_valid_downloaded = client.verify_signature_local(
            downloaded_message,
            downloaded_signature,
            alice_keys['public_key']
        )
        print(f"Downloaded post signature verification: {'✓ VALID' if is_valid_downloaded else '✗ INVALID'}")
    else:
        print("✗ No signature found in downloaded post!")
    print()
    
    # Test 7: Create post WITHOUT signature (should fail or use default)
    print("✓ TEST 7: Create Post WITHOUT Signature")
    print("-" * 70)
    print("Attempting to create post without signature...")
    try:
        post_no_sig = client.create_post(
            subreddit['id'],
            alice['id'],
            "Post without signature",
            "This should either fail or get auto-signed",
            ""  # Empty signature
        )
        print(f"✓ Post created (ID: {post_no_sig['id']})")
        print(f"  Signature: {post_no_sig.get('signature', 'NONE')[:32] if post_no_sig.get('signature') else 'NONE'}...")
        
        # Check if server verified it
        verified = client.get_post_verified(post_no_sig['id'])
        print(f"  Server verification: {'✓ VALID' if verified['signature_verified'] else '✗ INVALID'}")
    except Exception as e:
        print(f"✗ Failed (as expected): {e}")
    print()
    
    # Test 8: Create post with INVALID signature
    print("✓ TEST 8: Create Post with INVALID Signature")
    print("-" * 70)
    print("Creating post with tampered/invalid signature...")
    invalid_sig = base64.b64encode(b"invalid_signature_data_here").decode('utf-8')
    try:
        post_invalid = client.create_post(
            subreddit['id'],
            alice['id'],
            "Post with invalid signature",
            "This signature is invalid",
            invalid_sig
        )
        print(f"✓ Post created (ID: {post_invalid['id']})")
        
        # Verify on server - should fail
        verified_invalid = client.get_post_verified(post_invalid['id'])
        print(f"  Server verification: {'✓ VALID' if verified_invalid['signature_verified'] else '✗ INVALID (as expected)'}")
    except Exception as e:
        print(f"✗ Failed to create post: {e}")
    print()
    
    # Summary
    print("=" * 70)
    print("SUMMARY: Digital Signature Requirements Verification")
    print("=" * 70)
    print()
    print("Requirement 1: 'Every post has an accompanying signature'")
    print(f"  Status: {'✓ PASSED' if post.get('signature') else '✗ FAILED'}")
    print(f"  Evidence: Post {post['id']} has signature: {bool(post.get('signature'))}")
    print()
    print("Requirement 2: 'Each time a post is downloaded, signature is checked'")
    print(f"  Status: {'✓ PASSED' if verified_result.get('signature_verified') is not None else '✗ FAILED'}")
    print(f"  Evidence: Server returned verification status: {verified_result.get('signature_verified')}")
    print()
    print("=" * 70)
    print("All signature tests completed!")
    print("=" * 70)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\n\nTest interrupted by user")
    except Exception as e:
        print(f"\n\n✗ ERROR: {e}")
        import traceback
        traceback.print_exc()

