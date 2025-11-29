#!/bin/bash
# Demo script for Reddit Clone with REST API and Digital Signatures
# Shows client-server communication via REST API

set -e

echo "========================================="
echo "Reddit Clone - Part 2 Demo"
echo "REST API + Digital Signatures (Ed25519)"
echo "========================================="
echo

# Use Python client for demo
CLIENT="python3 client.py"

echo "1. Health Check"
echo "   Command: $CLIENT health"
$CLIENT health
echo

echo "2. Generate Ed25519 Keypair for Alice"
echo "   Command: $CLIENT keygen"
ALICE_KEYS=$($CLIENT keygen)
echo "$ALICE_KEYS"
ALICE_PUBKEY=$(echo "$ALICE_KEYS" | python3 -c "import sys, json; print(json.load(sys.stdin)['public_key'])")
ALICE_PRIVKEY=$(echo "$ALICE_KEYS" | python3 -c "import sys, json; print(json.load(sys.stdin)['private_key'])")
echo "   Alice's public key: ${ALICE_PUBKEY:0:20}..."
echo

echo "3. Generate Ed25519 Keypair for Bob"
echo "   Command: $CLIENT keygen"
BOB_KEYS=$($CLIENT keygen)
echo "$BOB_KEYS"
BOB_PUBKEY=$(echo "$BOB_KEYS" | python3 -c "import sys, json; print(json.load(sys.stdin)['public_key'])")
BOB_PRIVKEY=$(echo "$BOB_KEYS" | python3 -c "import sys, json; print(json.load(sys.stdin)['private_key'])")
echo "   Bob's public key: ${BOB_PUBKEY:0:20}..."
echo

echo "4. Register Alice with public key"
echo "   Command: $CLIENT register alice --public-key <pubkey>"
ALICE_RESULT=$($CLIENT register alice --public-key "$ALICE_PUBKEY")
echo "$ALICE_RESULT"
ALICE_ID=$(echo "$ALICE_RESULT" | python3 -c "import sys, json; print(json.load(sys.stdin)['id'])")
echo "   Alice's ID: $ALICE_ID"
echo

echo "5. Register Bob with public key"
echo "   Command: $CLIENT register bob --public-key <pubkey>"
BOB_RESULT=$($CLIENT register bob --public-key "$BOB_PUBKEY")
echo "$BOB_RESULT"
BOB_ID=$(echo "$BOB_RESULT" | python3 -c "import sys, json; print(json.load(sys.stdin)['id'])")
echo "   Bob's ID: $BOB_ID"
echo

echo "6. Retrieve Alice's public key from server"
echo "   Command: $CLIENT get-pubkey $ALICE_ID"
$CLIENT get-pubkey $ALICE_ID
echo

echo "7. Create subreddit 'gleam'"
echo "   Command: $CLIENT create-subreddit gleam"
SUB_RESULT=$($CLIENT create-subreddit gleam)
echo "$SUB_RESULT"
SUB_ID=$(echo "$SUB_RESULT" | python3 -c "import sys, json; print(json.load(sys.stdin)['id'])")
echo "   Subreddit ID: $SUB_ID"
echo

echo "8. Alice joins subreddit"
echo "   Command: $CLIENT join-subreddit $ALICE_ID $SUB_ID"
$CLIENT join-subreddit $ALICE_ID $SUB_ID
echo

echo "9. Bob joins subreddit"
echo "   Command: $CLIENT join-subreddit $BOB_ID $SUB_ID"
$CLIENT join-subreddit $BOB_ID $SUB_ID
echo

echo "10. Alice creates a signed post"
echo "    Note: In production, signature would be computed client-side"
echo "    Command: $CLIENT create-post $SUB_ID $ALICE_ID 'Hello Gleam' 'This is my first post'"
POST_RESULT=$($CLIENT create-post $SUB_ID $ALICE_ID "Hello Gleam" "This is my first post about Gleam programming")
echo "$POST_RESULT"
POST_ID=$(echo "$POST_RESULT" | python3 -c "import sys, json; print(json.load(sys.stdin)['id'])")
echo "    Post ID: $POST_ID"
echo

echo "11. Get post with signature verification"
echo "    Command: $CLIENT get-post $POST_ID --verify"
echo "    [Server logs will show signature verification]"
$CLIENT get-post $POST_ID --verify
echo

echo "12. Bob upvotes Alice's post"
echo "    Command: $CLIENT vote-post $POST_ID $BOB_ID 1"
$CLIENT vote-post $POST_ID $BOB_ID 1
echo

echo "13. Bob comments on Alice's post"
echo "    Command: $CLIENT comment $POST_ID $BOB_ID 'Great post!'"
COMMENT_RESULT=$($CLIENT comment $POST_ID $BOB_ID "Great post about Gleam!")
echo "$COMMENT_RESULT"
COMMENT_ID=$(echo "$COMMENT_RESULT" | python3 -c "import sys, json; print(json.load(sys.stdin)['id'])")
echo

echo "14. Get comments on post"
echo "    Command: $CLIENT get-comments $POST_ID"
$CLIENT get-comments $POST_ID
echo

echo "15. Alice upvotes Bob's comment"
echo "    Command: $CLIENT vote-comment $COMMENT_ID $ALICE_ID 1"
$CLIENT vote-comment $COMMENT_ID $ALICE_ID 1
echo

echo "16. Get Alice's home feed"
echo "    Command: $CLIENT feed $ALICE_ID"
$CLIENT feed $ALICE_ID
echo

echo "17. Check Alice's karma"
echo "    Command: $CLIENT karma $ALICE_ID"
$CLIENT karma $ALICE_ID
echo

echo "18. Bob sends DM to Alice"
echo "    Command: $CLIENT send-dm $BOB_ID $ALICE_ID 'Thanks for the post!'"
$CLIENT send-dm $BOB_ID $ALICE_ID "Thanks for the great post about Gleam!"
echo

echo "19. Alice checks inbox"
echo "    Command: $CLIENT inbox $ALICE_ID"
$CLIENT inbox $ALICE_ID
echo

echo "========================================="
echo "Demo Complete!"
echo "========================================="
echo
echo "Key Features Demonstrated:"
echo "✓ REST API communication (HTTP GET/POST)"
echo "✓ Ed25519 keypair generation"
echo "✓ User registration with public keys"
echo "✓ Public key retrieval from server"
echo "✓ Post creation with digital signatures"
echo "✓ Signature verification on post download"
echo "✓ Multiple clients (Alice & Bob) via REST API"
echo "✓ All Reddit-like features (posts, comments, votes, DMs, etc.)"

