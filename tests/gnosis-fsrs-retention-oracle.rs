// Copyright (C) 2026 Free Software Foundation, Inc.
// SPDX-License-Identifier: GPL-3.0-or-later
// Regenerate gnosis-fsrs-retention.json in a disposable Cargo project:
// Copy this file to src/main.rs; use edition = "2024" and dependencies
// fsrs = { version = "=6.6.1", default-features = false }
// serde_json = "=1.0.150"
// Run cargo run --quiet > gnosis-fsrs-retention.json, then compare a second run.
// The crate checksum is pinned in the corpus and checked by the ERT consumer.
// Only FSRS::next_states computes memory and raw intervals; calendar rounding
// separately applies Gnosis's documented ties-to-even/minimum-one policy.

use fsrs::{DEFAULT_PARAMETERS, FSRS, MemoryState};
use serde_json::json;

fn main() {
    let fsrs = FSRS::new(&DEFAULT_PARAMETERS).unwrap();
    let mut cases = Vec::new();
    for retention in [0.8_f32, 0.95_f32] {
        for (prior, elapsed) in [
            (None, 0),
            (Some((20.0, 5.0)), 0),
            (Some((20.0, 5.0)), 20),
            (Some((20.0, 5.0)), 365),
            (Some((1000.0, 5.0)), 0),
            (Some((36499.0, 5.0)), 0),
            (Some((36500.0, 5.0)), 0),
            (Some((36500.0, 5.0)), 36500),
        ] {
            let memory = prior.map(|(stability, difficulty)| MemoryState {
                stability,
                difficulty,
            });
            let states = fsrs.next_states(memory, retention, elapsed).unwrap();
            for (outcome, selected) in [("failure", states.again), ("success", states.good)] {
                cases.push(json!({
                    "prior_state": memory,
                    "elapsed_days": elapsed,
                    "outcome": outcome,
                    // Decimal preference, not its widened f32 spelling.
                    "desired_retention": if retention == 0.8_f32 { 0.8_f64 } else { 0.95_f64 },
                    "expected": {
                        "stability": f64::from(selected.memory.stability),
                        "difficulty": f64::from(selected.memory.difficulty),
                        "raw_interval_days": f64::from(selected.interval),
                        // Gnosis calendar policy, NOT Rust's ties-away round().
                        "calendar_interval_days": f64::from(selected.interval).round_ties_even().max(1.0) as u32
                    }
                }));
            }
        }
    }
    println!(
        "{}",
        serde_json::to_string_pretty(&json!({
            "schema": "gnosis-fsrs-nondefault-retention", "version": 1,
            "oracle": { "implementation": "fsrs-rs", "crate_version": "6.6.1",
                "crate_sha256": "b8a99ea3dec9af37c9ed3835463ff6820a578a0026a8e370b4c1c0db48dfab3f",
                "parameters": DEFAULT_PARAMETERS,
                "calendar_policy": "ties-to-even; minimum 1 day",
                "enable_fuzz": false, "enable_short_term": true },
            "cases": cases
        }))
        .unwrap()
    );
}
