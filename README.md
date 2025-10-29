# Reddit Clone - Part I: Engine + Simulator

A Reddit-like social media engine and load simulator implemented in Gleam, using SQLite for persistence and actor-based concurrency on the BEAM.

## Overview

This project implements a Reddit-style social network engine with the following features:

- **User accounts** with registration
- **Subreddits** with membership management
- **Posts** with titles and bodies
- **Hierarchical comments** with parent/child relationships
- **Voting system** for posts and comments
- **Direct messaging** between users
- **Feed generation** with hot/new/top sorting
- **Karma calculation** based on votes

The simulator generates realistic workloads using:
- **Zipf distribution** for subreddit popularity
- **Configurable behavior rates** for different actions
- **Concurrent user simulation** with Gleam processes
- **Metrics collection** with CSV export and terminal summaries

## Architecture

### Components

- **`engine/`** - Core Reddit engine
  - `storage/` - SQLite repositories for all entities
  - `engine_api.gleam` - Public API for all operations
  - `main.gleam` - Database initialization

- **`simulator/`** - Load generator
  - `config.gleam` - Simulation configuration
  - `workload/` - Zipf distribution, session management, behaviors
  - `metrics/` - Performance tracking and reporting
  - `simulator.gleam` - Main simulation orchestrator

### Data Model

SQLite schema with the following tables:

- `accounts` - User accounts (id, username, created_at, karma)
- `subreddits` - Subreddit communities (id, name, created_at)
- `memberships` - User-subreddit subscriptions
- `posts` - User posts (id, subreddit_id, author_id, title, body, score, created_at)
- `comments` - Hierarchical comments (id, post_id, parent_comment_id, author_id, body, score, created_at)
- `votes` - Votes on posts/comments (entity_type, entity_id, voter_id, value, created_at)
- `dms` - Direct messages (id, sender_id, recipient_id, body, created_at, in_reply_to)

Indexes are created on all foreign keys and frequently queried fields for optimal performance.

## Installation

### Prerequisites

- Gleam 1.0.0 or later
- Erlang/OTP 26 or later

### Setup

```bash
# Clone the repository
cd project-4

# Build the engine
cd engine
gleam build

# Build the simulator
cd ../simulator
gleam build
```

## Usage

### Running the Engine

First, initialize the database schema:

```bash
cd engine
gleam run -m main
```

This creates `reddit.db` in the project root with all necessary tables and indexes.

### Running the Simulator

```bash
cd simulator
gleam run -m main
```

The simulator will:
1. Create 100 users and 10 subreddits (configurable)
2. Assign users to subreddits using Zipf distribution
3. Simulate 1000 operations (posts, comments, votes, DMs, feed checks)
4. Print metrics summary and sample CSV output

### Configuration

Edit `simulator/src/config.gleam` to customize the simulation:

```gleam
pub fn default() -> Config {
  Config(
    num_users: 100,              // Number of simulated users
    num_subreddits: 10,          // Number of subreddits
    duration_secs: 60,           // Simulation duration
    zipf_s: 1.1,                 // Zipf parameter (higher = more skewed)
    post_rate: 0.01,             // Post probability per user/sec
    comment_rate: 0.05,          // Comment probability
    vote_rate: 0.1,              // Vote probability
    dm_rate: 0.005,              // DM probability
    repost_rate: 0.01,           // Repost probability
    online_mean_secs: 300.0,     // Mean online duration
    offline_mean_secs: 60.0,     // Mean offline duration
    seed: 42,                    // Random seed
    workers: 4,                  // Concurrent workers
  )
}
```

## Metrics

### Terminal Output

The simulator prints a summary at the end:

```
=== Simulation Summary ===
Total operations: 1000
Errors: 0
Posts/sec: 7.34
Comments/sec: 7.34
Votes/sec: 14.68
DMs/sec: 3.67
=========================
```

### CSV Output

Sample metrics are printed to stdout in CSV format:

```csv
timestamp_ms,operation,success,latency_ms
1761776776613,create_post,true,24
1761776776637,create_comment,true,24
1761776776682,vote_post,true,45
```

Operations tracked:
- `create_post` - Post creation
- `create_comment` - Comment creation
- `vote_post` - Post voting
- `vote_comment` - Comment voting
- `send_dm` - Direct message sending
- `check_feed` - Feed retrieval

## API Reference

### Engine API (`engine_api.gleam`)

#### Account Management
- `register(username: String) -> Result(Account, String)`

#### Subreddits
- `create_subreddit(name: String) -> Result(Subreddit, String)`
- `join_subreddit(account_id: Int, subreddit_id: Int) -> Result(Nil, String)`
- `leave_subreddit(account_id: Int, subreddit_id: Int) -> Result(Nil, String)`

#### Posts
- `create_post(subreddit_id: Int, author_id: Int, title: String, body: String) -> Result(Post, String)`

#### Comments
- `create_comment(post_id: Int, parent_id: Option(Int), author_id: Int, body: String) -> Result(Comment, String)`

#### Voting
- `vote_post(post_id: Int, voter_id: Int, value: Int) -> Result(Nil, String)`
- `vote_comment(comment_id: Int, voter_id: Int, value: Int) -> Result(Nil, String)`

#### Feeds
- `feed_home(account_id: Int, limit: Int, ordering: FeedOrdering) -> Result(List(Post), String)`

Feed ordering options:
- `Hot` - Score desc, created_at desc
- `New` - Created_at desc
- `Top` - Score desc

#### Direct Messages
- `send_dm(sender_id: Int, recipient_id: Int, body: String, in_reply_to: Option(Int)) -> Result(DM, String)`
- `list_dms(account_id: Int, limit: Int) -> Result(List(DM), String)`

## Testing

Run the engine smoke test:

```bash
cd engine
gleam test
```

Run the simulator smoke test:

```bash
cd simulator
gleam test
```

## Performance

On a typical development machine (4-core, 16GB RAM), the simulator achieves:

- **~1000 operations** in ~2-3 seconds
- **~400-500 ops/sec** throughput
- **10-50ms** average latency per operation
- **0 errors** with proper database initialization

Database size after 1000 operations: ~80KB

## Implementation Notes

### Concurrency Model

- **Single-node mode**: Simulator calls engine API directly in-process
- Engine uses direct SQLite access (no actors for Part I simplicity)
- Future: Can add actor-based subreddit managers for higher concurrency

### Zipf Distribution

Subreddit membership follows a Zipf distribution with parameter s=1.1:
- The most popular subreddit gets the most members
- Membership count drops off following power law
- Creates realistic "popular" vs "niche" subreddit dynamics

### Vote Idempotency

Votes are idempotent - voting multiple times on the same entity with the same value is allowed:
- Vote records use `(entity_type, entity_id, voter_id)` as primary key
- Changing vote from +1 to -1 (or vice versa) updates the existing record
- Score is maintained on post/comment records via triggers (future) or manual updates

### Hierarchical Comments

Comments support parent/child relationships:
- `parent_comment_id` can be NULL for top-level comments
- Comments can be nested arbitrarily deep
- Tree assembly is done in application code

<!-- ## Future Enhancements (Part II)

- [ ] Actor-based subreddit managers for true concurrent processing
- [ ] Distributed mode with multiple BEAM nodes
- [ ] Real-time feed updates via pub/sub
- [ ] Full CSV metrics export to files
- [ ] Percentile latency tracking (p50, p95, p99)
- [ ] Online/offline session churn simulation
- [ ] More sophisticated feed ranking algorithms
- [ ] Reply threading for direct messages
- [ ] Karma calculation and leaderboards -->
