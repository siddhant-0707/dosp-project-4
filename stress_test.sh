#!/bin/bash
# Stress test for Reddit Clone REST API
# Tests with multiple users and concurrent operations

set -e

CLIENT="python3 client.py"
NUM_USERS=20
NUM_POSTS_PER_USER=5
NUM_COMMENTS=10
NUM_VOTES=50

echo "========================================="
echo "Reddit Clone - Stress Test"
echo "REST API Load Testing"
echo "========================================="
echo "Configuration:"
echo "  Users: $NUM_USERS"
echo "  Posts per user: $NUM_POSTS_PER_USER"
echo "  Comments: $NUM_COMMENTS"
echo "  Votes: $NUM_VOTES"
echo "========================================="
echo

# Array to store user IDs
declare -a USER_IDS
declare -a POST_IDS

echo "1. Creating test subreddits..."
SUB1=$($CLIENT create-subreddit "test_gaming" | python3 -c "import sys, json; print(json.load(sys.stdin)['id'])")
SUB2=$($CLIENT create-subreddit "test_programming" | python3 -c "import sys, json; print(json.load(sys.stdin)['id'])")
SUB3=$($CLIENT create-subreddit "test_memes" | python3 -c "import sys, json; print(json.load(sys.stdin)['id'])")
echo "   Created 3 subreddits: $SUB1, $SUB2, $SUB3"
echo

echo "2. Registering $NUM_USERS users..."
for i in $(seq 1 $NUM_USERS); do
    USERNAME="user$i"
    
    # Generate keypair (optional for stress test)
    KEYS=$($CLIENT keygen 2>/dev/null)
    PUBKEY=$(echo "$KEYS" | python3 -c "import sys, json; print(json.load(sys.stdin)['public_key'])")
    
    # Register user
    USER_RESULT=$($CLIENT register "$USERNAME" --public-key "$PUBKEY" 2>/dev/null)
    USER_ID=$(echo "$USER_RESULT" | python3 -c "import sys, json; print(json.load(sys.stdin)['id'])")
    USER_IDS[$i]=$USER_ID
    
    if [ $((i % 5)) -eq 0 ]; then
        echo "   Registered $i users..."
    fi
done
echo "   ✓ Registered $NUM_USERS users"
echo

echo "3. Users joining subreddits..."
for USER_ID in "${USER_IDS[@]}"; do
    $CLIENT join-subreddit $USER_ID $SUB1 >/dev/null 2>&1
    $CLIENT join-subreddit $USER_ID $SUB2 >/dev/null 2>&1
    $CLIENT join-subreddit $USER_ID $SUB3 >/dev/null 2>&1
done
echo "   ✓ All users joined all subreddits"
echo

echo "4. Creating posts (${NUM_USERS}x${NUM_POSTS_PER_USER} = $((NUM_USERS * NUM_POSTS_PER_USER)) total)..."
POST_COUNT=0
for i in $(seq 1 $NUM_USERS); do
    USER_ID=${USER_IDS[$i]}
    for j in $(seq 1 $NUM_POSTS_PER_USER); do
        # Distribute posts across subreddits
        SUB=$SUB1
        if [ $((j % 3)) -eq 1 ]; then SUB=$SUB2; fi
        if [ $((j % 3)) -eq 2 ]; then SUB=$SUB3; fi
        
        TITLE="Post $j from user$i"
        BODY="This is post number $j from user$i about test topic $((RANDOM % 100))"
        
        POST_RESULT=$($CLIENT create-post $SUB $USER_ID "$TITLE" "$BODY" 2>/dev/null)
        POST_ID=$(echo "$POST_RESULT" | python3 -c "import sys, json; print(json.load(sys.stdin)['id'])")
        POST_IDS[$POST_COUNT]=$POST_ID
        POST_COUNT=$((POST_COUNT + 1))
    done
    
    if [ $((i % 5)) -eq 0 ]; then
        echo "   Created posts for $i users ($POST_COUNT total posts)..."
    fi
done
echo "   ✓ Created $POST_COUNT posts"
echo

echo "5. Creating comments..."
for i in $(seq 1 $NUM_COMMENTS); do
    # Random user
    USER_IDX=$((RANDOM % NUM_USERS + 1))
    USER_ID=${USER_IDS[$USER_IDX]}
    
    # Random post
    POST_IDX=$((RANDOM % POST_COUNT))
    POST_ID=${POST_IDS[$POST_IDX]}
    
    COMMENT_BODY="Comment $i from user$USER_IDX: Great post! Random: $((RANDOM % 1000))"
    $CLIENT comment $POST_ID $USER_ID "$COMMENT_BODY" >/dev/null 2>&1
    
    if [ $((i % 10)) -eq 0 ]; then
        echo "   Created $i comments..."
    fi
done
echo "   ✓ Created $NUM_COMMENTS comments"
echo

echo "6. Generating random votes..."
for i in $(seq 1 $NUM_VOTES); do
    # Random user
    USER_IDX=$((RANDOM % NUM_USERS + 1))
    USER_ID=${USER_IDS[$USER_IDX]}
    
    # Random post
    POST_IDX=$((RANDOM % POST_COUNT))
    POST_ID=${POST_IDS[$POST_IDX]}
    
    # Random vote direction
    if [ $((RANDOM % 2)) -eq 0 ]; then
        VOTE=1
    else
        VOTE=-1
    fi
    
    $CLIENT vote-post $POST_ID $USER_ID $VOTE >/dev/null 2>&1
    
    if [ $((i % 10)) -eq 0 ]; then
        echo "   Generated $i votes..."
    fi
done
echo "   ✓ Generated $NUM_VOTES votes"
echo

echo "7. Testing concurrent reads..."
echo "   Fetching feeds for multiple users in parallel..."
for i in $(seq 1 10); do
    USER_IDX=$((RANDOM % NUM_USERS + 1))
    USER_ID=${USER_IDS[$USER_IDX]}
    $CLIENT feed $USER_ID >/dev/null 2>&1 &
done
wait
echo "   ✓ Concurrent feed fetches completed"
echo

echo "8. Sending direct messages..."
for i in $(seq 1 20); do
    SENDER_IDX=$((RANDOM % NUM_USERS + 1))
    RECIPIENT_IDX=$((RANDOM % NUM_USERS + 1))
    
    # Don't send to yourself
    while [ $SENDER_IDX -eq $RECIPIENT_IDX ]; do
        RECIPIENT_IDX=$((RANDOM % NUM_USERS + 1))
    done
    
    SENDER_ID=${USER_IDS[$SENDER_IDX]}
    RECIPIENT_ID=${USER_IDS[$RECIPIENT_IDX]}
    
    $CLIENT send-dm $SENDER_ID $RECIPIENT_ID "Message $i: Hello from user$SENDER_IDX!" >/dev/null 2>&1
done
echo "   ✓ Sent 20 direct messages"
echo

echo "9. Checking karma for sample users..."
for i in 1 5 10 15 20; do
    USER_ID=${USER_IDS[$i]}
    KARMA_RESULT=$($CLIENT karma $USER_ID)
    KARMA=$(echo "$KARMA_RESULT" | python3 -c "import sys, json; print(json.load(sys.stdin)['karma'])")
    echo "   user$i: $KARMA karma"
done
echo

echo "10. Testing post signature verification..."
# Test a few random posts with verification
for i in $(seq 1 5); do
    POST_IDX=$((RANDOM % POST_COUNT))
    POST_ID=${POST_IDS[$POST_IDX]}
    VERIFIED_RESULT=$($CLIENT get-post $POST_ID --verify 2>/dev/null)
    VERIFIED=$(echo "$VERIFIED_RESULT" | python3 -c "import sys, json; r=json.load(sys.stdin); print(r.get('signature_verified', 'N/A'))")
    echo "   Post $POST_ID signature verified: $VERIFIED"
done
echo

echo "========================================="
echo "Stress Test Complete!"
echo "========================================="
echo
echo "Summary:"
echo "  ✓ $NUM_USERS users registered with Ed25519 keys"
echo "  ✓ $POST_COUNT posts created across 3 subreddits"
echo "  ✓ $NUM_COMMENTS comments posted"
echo "  ✓ $NUM_VOTES votes cast"
echo "  ✓ 20 direct messages sent"
echo "  ✓ Multiple concurrent feed requests"
echo "  ✓ Signature verification tested"
echo
echo "REST API handled $((NUM_USERS + POST_COUNT + NUM_COMMENTS + NUM_VOTES + 20 + 30)) total operations!"





