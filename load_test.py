#!/usr/bin/env python3
"""
Concurrent Load Test for Reddit Clone REST API
Tests system under high concurrent load
"""

import argparse
import concurrent.futures
import json
import random
import sys
import time
from typing import List, Dict
import requests

API_BASE_URL = "http://localhost:8080/api"

class LoadTester:
    def __init__(self, base_url: str = API_BASE_URL):
        self.base_url = base_url
        self.users = []
        self.subreddits = []
        self.posts = []
        self.stats = {
            'requests': 0,
            'errors': 0,
            'total_time': 0.0
        }
    
    def _request(self, method: str, endpoint: str, data: dict = None) -> dict:
        """Make HTTP request and track stats"""
        url = f"{self.base_url}{endpoint}"
        start = time.time()
        try:
            if method == "GET":
                response = requests.get(url, timeout=10)
            else:
                response = requests.post(url, json=data, timeout=10)
            
            elapsed = time.time() - start
            self.stats['requests'] += 1
            self.stats['total_time'] += elapsed
            
            response.raise_for_status()
            return response.json()
        except Exception as e:
            self.stats['errors'] += 1
            return {"error": str(e)}
    
    def setup_users(self, num_users: int):
        """Register users with keypairs"""
        print(f"Setting up {num_users} users...")
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
            futures = []
            for i in range(1, num_users + 1):
                future = executor.submit(self._create_user, f"load_user_{i}")
                futures.append(future)
            
            for future in concurrent.futures.as_completed(futures):
                result = future.result()
                if result and 'id' in result:
                    self.users.append(result)
                if len(self.users) % 10 == 0:
                    print(f"  Registered {len(self.users)} users...")
        
        print(f"✓ Registered {len(self.users)} users")
    
    def _create_user(self, username: str) -> dict:
        """Create a single user with keypair"""
        # Generate keypair
        keys = self._request("GET", "/crypto/generate_keypair")
        if 'error' in keys:
            return None
        
        # Register user
        result = self._request("POST", "/register", {
            "username": username,
            "public_key": keys.get('public_key', '')
        })
        return result
    
    def setup_subreddits(self, num_subs: int):
        """Create test subreddits"""
        print(f"Creating {num_subs} subreddits...")
        topics = ['gaming', 'programming', 'memes', 'news', 'tech', 'science', 
                  'art', 'music', 'movies', 'books', 'sports', 'food']
        
        for i in range(num_subs):
            topic = random.choice(topics)
            result = self._request("POST", "/subreddits", {
                "name": f"load_{topic}_{i}"
            })
            if 'id' in result:
                self.subreddits.append(result)
        
        print(f"✓ Created {len(self.subreddits)} subreddits")
    
    def concurrent_joins(self):
        """All users join all subreddits concurrently"""
        print(f"Users joining subreddits (concurrent)...")
        total_joins = len(self.users) * len(self.subreddits)
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=20) as executor:
            futures = []
            for user in self.users:
                for sub in self.subreddits:
                    future = executor.submit(
                        self._request, "POST", 
                        f"/subreddits/{sub['id']}/join",
                        {"user_id": user['id']}
                    )
                    futures.append(future)
            
            completed = 0
            for future in concurrent.futures.as_completed(futures):
                future.result()
                completed += 1
                if completed % 50 == 0:
                    print(f"  {completed}/{total_joins} joins...")
        
        print(f"✓ Completed {total_joins} joins")
    
    def concurrent_posts(self, posts_per_user: int):
        """Create posts concurrently"""
        print(f"Creating posts ({posts_per_user} per user, concurrent)...")
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=20) as executor:
            futures = []
            for user in self.users:
                for i in range(posts_per_user):
                    sub = random.choice(self.subreddits)
                    future = executor.submit(
                        self._create_post,
                        sub['id'], user['id'], i
                    )
                    futures.append(future)
            
            completed = 0
            for future in concurrent.futures.as_completed(futures):
                result = future.result()
                if result and 'id' in result:
                    self.posts.append(result)
                completed += 1
                if completed % 20 == 0:
                    print(f"  Created {completed} posts...")
        
        print(f"✓ Created {len(self.posts)} posts")
    
    def _create_post(self, sub_id: int, user_id: int, num: int) -> dict:
        """Create a single post"""
        return self._request("POST", "/posts", {
            "subreddit_id": sub_id,
            "author_id": user_id,
            "title": f"Load test post {num} from user {user_id}",
            "body": f"This is test post {num}. Random: {random.randint(1, 10000)}",
            "signature": ""
        })
    
    def concurrent_votes(self, num_votes: int):
        """Cast votes concurrently"""
        print(f"Casting {num_votes} votes (concurrent)...")
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=20) as executor:
            futures = []
            for _ in range(num_votes):
                user = random.choice(self.users)
                post = random.choice(self.posts)
                vote = random.choice([1, -1])
                future = executor.submit(
                    self._request, "POST",
                    f"/posts/{post['id']}/vote",
                    {"voter_id": user['id'], "value": vote}
                )
                futures.append(future)
            
            for future in concurrent.futures.as_completed(futures):
                future.result()
        
        print(f"✓ Cast {num_votes} votes")
    
    def concurrent_reads(self, num_reads: int):
        """Perform concurrent read operations"""
        print(f"Performing {num_reads} concurrent reads...")
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=30) as executor:
            futures = []
            for _ in range(num_reads):
                op = random.choice(['feed', 'post', 'karma'])
                if op == 'feed':
                    user = random.choice(self.users)
                    future = executor.submit(
                        self._request, "GET", f"/feed/{user['id']}"
                    )
                elif op == 'post':
                    post = random.choice(self.posts)
                    future = executor.submit(
                        self._request, "GET", f"/posts/{post['id']}/verified"
                    )
                else:
                    user = random.choice(self.users)
                    future = executor.submit(
                        self._request, "GET", f"/karma/{user['id']}"
                    )
                futures.append(future)
            
            for future in concurrent.futures.as_completed(futures):
                future.result()
        
        print(f"✓ Completed {num_reads} reads")
    
    def print_stats(self):
        """Print performance statistics"""
        print("\n" + "="*50)
        print("Load Test Statistics")
        print("="*50)
        print(f"Total Requests:  {self.stats['requests']}")
        print(f"Errors:          {self.stats['errors']}")
        print(f"Success Rate:    {((self.stats['requests'] - self.stats['errors']) / self.stats['requests'] * 100):.1f}%")
        print(f"Total Time:      {self.stats['total_time']:.2f}s")
        print(f"Avg Response:    {(self.stats['total_time'] / self.stats['requests'] * 1000):.1f}ms")
        print(f"Throughput:      {(self.stats['requests'] / self.stats['total_time']):.1f} req/s")
        print("="*50)


def main():
    parser = argparse.ArgumentParser(description="REST API Load Tester")
    parser.add_argument("--users", type=int, default=50, help="Number of users")
    parser.add_argument("--subreddits", type=int, default=5, help="Number of subreddits")
    parser.add_argument("--posts", type=int, default=3, help="Posts per user")
    parser.add_argument("--votes", type=int, default=200, help="Number of votes")
    parser.add_argument("--reads", type=int, default=100, help="Number of read operations")
    
    args = parser.parse_args()
    
    print("="*50)
    print("Reddit Clone - Concurrent Load Test")
    print("="*50)
    print(f"Configuration:")
    print(f"  Users:       {args.users}")
    print(f"  Subreddits:  {args.subreddits}")
    print(f"  Posts/user:  {args.posts}")
    print(f"  Votes:       {args.votes}")
    print(f"  Reads:       {args.reads}")
    print("="*50)
    print()
    
    tester = LoadTester()
    
    try:
        start_time = time.time()
        
        # Setup phase
        tester.setup_users(args.users)
        tester.setup_subreddits(args.subreddits)
        
        # Concurrent operations
        tester.concurrent_joins()
        tester.concurrent_posts(args.posts)
        tester.concurrent_votes(args.votes)
        tester.concurrent_reads(args.reads)
        
        elapsed = time.time() - start_time
        
        print(f"\n✓ Load test completed in {elapsed:.2f}s")
        tester.print_stats()
        
    except KeyboardInterrupt:
        print("\n\nLoad test interrupted by user")
        tester.print_stats()
    except Exception as e:
        print(f"\nError: {e}", file=sys.stderr)
        tester.print_stats()


if __name__ == "__main__":
    main()





