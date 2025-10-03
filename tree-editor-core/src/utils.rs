#![allow(dead_code)]

use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, mpsc};
use std::thread;

pub(crate) const RESET: &str = "\x1b[0m";
pub(crate) const RULE_NAME: &str = "\x1b[1;34m"; // Bold Blue
pub(crate) const TERMINAL: &str = "\x1b[32m"; // Green
pub(crate) const RULE_REF: &str = "\x1b[33m"; // Yellow
pub(crate) const OPERATOR: &str = "\x1b[1;37m"; // Bold White
pub(crate) const BRACKET: &str = "\x1b[35m"; // Magenta
pub(crate) const ERROR: &str = "\x1b[1;31m"; // Bold Red

// Colors for different path depths
pub(crate) const DEPTH_COLORS: &[&str] = &[
    "\x1b[1;34m", // Bold Blue (depth 0)
    "\x1b[1;32m", // Bold Green (depth 1)
    "\x1b[1;33m", // Bold Yellow (depth 2)
    "\x1b[1;35m", // Bold Magenta (depth 3)
    "\x1b[1;36m", // Bold Cyan (depth 4)
    "\x1b[1;31m", // Bold Red (depth 5)
    "\x1b[34m",   // Blue (depth 6)
    "\x1b[32m",   // Green (depth 7)
    "\x1b[33m",   // Yellow (depth 8)
    "\x1b[35m",   // Magenta (depth 9)
];

// Run Result-returning jobs: return first Ok(T) immediately, else collect all Err(E).
pub fn first_valid<T, E, F, I>(jobs: I) -> Result<T, Vec<E>>
where
    I: IntoIterator<Item = F>,
    F: FnOnce(&AtomicBool) -> Result<T, E> + Send + 'static,
    T: Send + 'static,
    E: Send + 'static,
{
    let mut collected: Vec<Box<dyn FnOnce(&AtomicBool) -> Result<T, E> + Send + 'static>> =
        Vec::new();
    for j in jobs.into_iter() {
        collected.push(Box::new(j));
    }
    let total = collected.len();
    if total == 0 {
        panic!("no jobs submitted");
    }

    let done = Arc::new(AtomicBool::new(false));

    enum WorkerOutcome<T, E> {
        Ok(T),
        Err(E),
        Panic(Box<dyn std::any::Any + Send>),
    }

    let (tx, rx) = mpsc::channel::<WorkerOutcome<T, E>>();
    let mut spawned = 0usize;
    let mut handles = Vec::new();

    for job in collected.into_iter() {
        let done_cl = done.clone();
        let tx_cl = tx.clone();
        spawned += 1;
        handles.push(thread::spawn(move || {
            let res = catch_unwind(AssertUnwindSafe(|| job(&done_cl)));
            match res {
                Ok(Ok(v)) => {
                    let _ = tx_cl.send(WorkerOutcome::Ok(v));
                }
                Ok(Err(e)) => {
                    let _ = tx_cl.send(WorkerOutcome::Err(e));
                }
                Err(payload) => {
                    let _ = tx_cl.send(WorkerOutcome::Panic(payload));
                }
            }
        }));
    }
    drop(tx);

    let mut errs: Vec<E> = Vec::new();
    let mut result: Option<T> = None;
    while let Ok(msg) = rx.recv() {
        match msg {
            WorkerOutcome::Ok(v) => {
                done.store(true, Ordering::Release);
                result = Some(v);
                break;
            }
            WorkerOutcome::Err(e) => {
                errs.push(e);
                if errs.len() == spawned {
                    break;
                }
            }
            WorkerOutcome::Panic(payload) => {
                // join threads first to get deterministic state before resuming unwind
                for h in handles {
                    let _ = h.join();
                }
                resume_unwind(payload);
            }
        }
    }

    // ensure all threads finish
    for h in handles {
        let _ = h.join();
    }

    if let Some(v) = result {
        return Ok(v);
    }
    Err(errs)
}
