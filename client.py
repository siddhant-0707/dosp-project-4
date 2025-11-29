#!/usr/bin/env python3
"""
Reddit Clone CLI Client
Communicates with the Gleam engine via REST API
"""

import argparse
import json
import requests
import sys
from typing import Optional

API_BASE_URL = "http://localhost:8080/api"

class RedditClient:
    def __init__(self, base_url: str = API_BASE_URL):
        self.base_url = base_url
        self.session = requests.Session()
    
    def _request(self, method: str, endpoint: str, data: Optional[dict] = None) -> dict:
        """Make HTTP request to the API"""
        url = f"{self.base_url}{endpoint}"
        try:
            if method == "GET":
                response = self.session.get(url)
            elif method == "POST":
                response = self.session.post(url, json=data)
            else:
                raise ValueError(f"Unsupported method: {method}")
            
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(f"Error: {e}", file=sys.stderr)
            if hasattr(e.response, 'text'):
                print(f"Response: {e.response.text}", file=sys.stderr)
            sys.exit(1)
    
    def health_check(self) -> dict:
        """Check if server is running"""
        return self._request("GET", "/health")
    
    def generate_keypair(self) -> dict:
        """Generate a new Ed25519 keypair"""
        return self._request("GET", "/crypto/generate_keypair")
    
    def register(self, username: str, public_key: str = "") -> dict:
        """Register a new user"""
        return self._request("POST", "/register", {
            "username": username,
            "public_key": public_key
        })
    
    def get_account(self, account_id: int) -> dict:
        """Get account by ID"""
        return self._request("GET", f"/accounts/{account_id}")
    
    def get_account_by_username(self, username: str) -> dict:
        """Get account by username"""
        return self._request("GET", f"/accounts/username/{username}")
    
    def get_public_key(self, account_id: int) -> dict:
        """Get user's public key"""
        return self._request("GET", f"/accounts/{account_id}/public_key")
    
    def create_subreddit(self, name: str) -> dict:
        """Create a new subreddit"""
        return self._request("POST", "/subreddits", {"name": name})
    
    def join_subreddit(self, user_id: int, subreddit_id: int) -> dict:
        """Join a subreddit"""
        return self._request("POST", f"/subreddits/{subreddit_id}/join", {
            "user_id": user_id
        })
    
    def leave_subreddit(self, user_id: int, subreddit_id: int) -> dict:
        """Leave a subreddit"""
        return self._request("POST", f"/subreddits/{subreddit_id}/leave", {
            "user_id": user_id
        })
    
    def create_post(self, subreddit_id: int, author_id: int, title: str, body: str, signature: str = "") -> dict:
        """Create a new post"""
        return self._request("POST", "/posts", {
            "subreddit_id": subreddit_id,
            "author_id": author_id,
            "title": title,
            "body": body,
            "signature": signature
        })
    
    def get_post(self, post_id: int) -> dict:
        """Get a post by ID"""
        return self._request("GET", f"/posts/{post_id}")
    
    def get_post_verified(self, post_id: int) -> dict:
        """Get a post with signature verification"""
        return self._request("GET", f"/posts/{post_id}/verified")
    
    def vote_post(self, post_id: int, voter_id: int, value: int) -> dict:
        """Vote on a post (1 for upvote, -1 for downvote)"""
        return self._request("POST", f"/posts/{post_id}/vote", {
            "voter_id": voter_id,
            "value": value
        })
    
    def create_comment(self, post_id: int, author_id: int, body: str, parent_id: int = 0) -> dict:
        """Create a comment on a post"""
        return self._request("POST", f"/posts/{post_id}/comments", {
            "author_id": author_id,
            "body": body,
            "parent_id": parent_id
        })
    
    def get_comments(self, post_id: int) -> dict:
        """Get all comments on a post"""
        return self._request("GET", f"/posts/{post_id}/comments")
    
    def vote_comment(self, comment_id: int, voter_id: int, value: int) -> dict:
        """Vote on a comment"""
        return self._request("POST", f"/comments/{comment_id}/vote", {
            "voter_id": voter_id,
            "value": value
        })
    
    def repost(self, post_id: int, author_id: int) -> dict:
        """Repost a post"""
        return self._request("POST", f"/posts/{post_id}/repost", {
            "author_id": author_id
        })
    
    def get_feed(self, user_id: int) -> dict:
        """Get home feed for a user"""
        return self._request("GET", f"/feed/{user_id}")
    
    def send_dm(self, sender_id: int, recipient_id: int, body: str, in_reply_to: int = 0) -> dict:
        """Send a direct message"""
        return self._request("POST", "/dms", {
            "sender_id": sender_id,
            "recipient_id": recipient_id,
            "body": body,
            "in_reply_to": in_reply_to
        })
    
    def get_inbox(self, user_id: int) -> dict:
        """Get inbox (DMs) for a user"""
        return self._request("GET", f"/dms/inbox/{user_id}")
    
    def get_karma(self, user_id: int) -> dict:
        """Get karma for a user"""
        return self._request("GET", f"/karma/{user_id}")


def main():
    parser = argparse.ArgumentParser(description="Reddit Clone CLI Client")
    parser.add_argument("--server", default=API_BASE_URL, help="API server URL")
    
    subparsers = parser.add_subparsers(dest="command", help="Available commands")
    
    # Health check
    subparsers.add_parser("health", help="Check server health")
    
    # Generate keypair
    subparsers.add_parser("keygen", help="Generate Ed25519 keypair")
    
    # Register user
    register_parser = subparsers.add_parser("register", help="Register a new user")
    register_parser.add_argument("username", help="Username")
    register_parser.add_argument("--public-key", default="", help="Public key (base64)")
    
    # Get account
    get_account_parser = subparsers.add_parser("get-account", help="Get account info")
    get_account_parser.add_argument("account_id", type=int, help="Account ID")
    
    # Get account by username
    get_account_by_username_parser = subparsers.add_parser("get-account-by-username", help="Get account by username")
    get_account_by_username_parser.add_argument("username", help="Username")
    
    # Get public key
    get_pubkey_parser = subparsers.add_parser("get-pubkey", help="Get user's public key")
    get_pubkey_parser.add_argument("account_id", type=int, help="Account ID")
    
    # Create subreddit
    create_sub_parser = subparsers.add_parser("create-subreddit", help="Create a subreddit")
    create_sub_parser.add_argument("name", help="Subreddit name")
    
    # Join subreddit
    join_sub_parser = subparsers.add_parser("join-subreddit", help="Join a subreddit")
    join_sub_parser.add_argument("user_id", type=int, help="User ID")
    join_sub_parser.add_argument("subreddit_id", type=int, help="Subreddit ID")
    
    # Leave subreddit
    leave_sub_parser = subparsers.add_parser("leave-subreddit", help="Leave a subreddit")
    leave_sub_parser.add_argument("user_id", type=int, help="User ID")
    leave_sub_parser.add_argument("subreddit_id", type=int, help="Subreddit ID")
    
    # Create post
    create_post_parser = subparsers.add_parser("create-post", help="Create a post")
    create_post_parser.add_argument("subreddit_id", type=int, help="Subreddit ID")
    create_post_parser.add_argument("author_id", type=int, help="Author ID")
    create_post_parser.add_argument("title", help="Post title")
    create_post_parser.add_argument("body", help="Post body")
    create_post_parser.add_argument("--signature", default="", help="Post signature (base64)")
    
    # Get post
    get_post_parser = subparsers.add_parser("get-post", help="Get a post")
    get_post_parser.add_argument("post_id", type=int, help="Post ID")
    get_post_parser.add_argument("--verify", action="store_true", help="Verify signature")
    
    # Vote post
    vote_post_parser = subparsers.add_parser("vote-post", help="Vote on a post")
    vote_post_parser.add_argument("post_id", type=int, help="Post ID")
    vote_post_parser.add_argument("voter_id", type=int, help="Voter ID")
    vote_post_parser.add_argument("value", type=int, choices=[-1, 1], help="Vote value (1 or -1)")
    
    # Create comment
    comment_parser = subparsers.add_parser("comment", help="Comment on a post")
    comment_parser.add_argument("post_id", type=int, help="Post ID")
    comment_parser.add_argument("author_id", type=int, help="Author ID")
    comment_parser.add_argument("body", help="Comment body")
    comment_parser.add_argument("--parent", type=int, default=0, help="Parent comment ID")
    
    # Get comments
    get_comments_parser = subparsers.add_parser("get-comments", help="Get comments on a post")
    get_comments_parser.add_argument("post_id", type=int, help="Post ID")
    
    # Vote comment
    vote_comment_parser = subparsers.add_parser("vote-comment", help="Vote on a comment")
    vote_comment_parser.add_argument("comment_id", type=int, help="Comment ID")
    vote_comment_parser.add_argument("voter_id", type=int, help="Voter ID")
    vote_comment_parser.add_argument("value", type=int, choices=[-1, 1], help="Vote value")
    
    # Repost
    repost_parser = subparsers.add_parser("repost", help="Repost a post")
    repost_parser.add_argument("post_id", type=int, help="Post ID")
    repost_parser.add_argument("author_id", type=int, help="Author ID")
    
    # Get feed
    feed_parser = subparsers.add_parser("feed", help="Get home feed")
    feed_parser.add_argument("user_id", type=int, help="User ID")
    
    # Send DM
    dm_parser = subparsers.add_parser("send-dm", help="Send direct message")
    dm_parser.add_argument("sender_id", type=int, help="Sender ID")
    dm_parser.add_argument("recipient_id", type=int, help="Recipient ID")
    dm_parser.add_argument("body", help="Message body")
    dm_parser.add_argument("--reply-to", type=int, default=0, help="Reply to DM ID")
    
    # Get inbox
    inbox_parser = subparsers.add_parser("inbox", help="Get inbox")
    inbox_parser.add_argument("user_id", type=int, help="User ID")
    
    # Get karma
    karma_parser = subparsers.add_parser("karma", help="Get karma")
    karma_parser.add_argument("user_id", type=int, help="User ID")
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        sys.exit(1)
    
    client = RedditClient(args.server)
    
    try:
        if args.command == "health":
            result = client.health_check()
        elif args.command == "keygen":
            result = client.generate_keypair()
        elif args.command == "register":
            result = client.register(args.username, args.public_key)
        elif args.command == "get-account":
            result = client.get_account(args.account_id)
        elif args.command == "get-account-by-username":
            result = client.get_account_by_username(args.username)
        elif args.command == "get-pubkey":
            result = client.get_public_key(args.account_id)
        elif args.command == "create-subreddit":
            result = client.create_subreddit(args.name)
        elif args.command == "join-subreddit":
            result = client.join_subreddit(args.user_id, args.subreddit_id)
        elif args.command == "leave-subreddit":
            result = client.leave_subreddit(args.user_id, args.subreddit_id)
        elif args.command == "create-post":
            result = client.create_post(args.subreddit_id, args.author_id, args.title, args.body, args.signature)
        elif args.command == "get-post":
            if args.verify:
                result = client.get_post_verified(args.post_id)
            else:
                result = client.get_post(args.post_id)
        elif args.command == "vote-post":
            result = client.vote_post(args.post_id, args.voter_id, args.value)
        elif args.command == "comment":
            result = client.create_comment(args.post_id, args.author_id, args.body, args.parent)
        elif args.command == "get-comments":
            result = client.get_comments(args.post_id)
        elif args.command == "vote-comment":
            result = client.vote_comment(args.comment_id, args.voter_id, args.value)
        elif args.command == "repost":
            result = client.repost(args.post_id, args.author_id)
        elif args.command == "feed":
            result = client.get_feed(args.user_id)
        elif args.command == "send-dm":
            result = client.send_dm(args.sender_id, args.recipient_id, args.body, args.reply_to)
        elif args.command == "inbox":
            result = client.get_inbox(args.user_id)
        elif args.command == "karma":
            result = client.get_karma(args.user_id)
        else:
            parser.print_help()
            sys.exit(1)
        
        print(json.dumps(result, indent=2))
    
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()

