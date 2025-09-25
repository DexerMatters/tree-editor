use std::collections::{BTreeSet, HashMap};
use std::hash::Hash;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, mpsc};
use std::thread;

pub struct JobGroup<T> {
    jobs: Vec<Box<dyn FnOnce(&AtomicBool) -> T + Send + 'static>>,
}

impl<T: Send + 'static> JobGroup<T> {
    pub fn new() -> Self {
        Self { jobs: Vec::new() }
    }
    pub fn with_capacity(n: usize) -> Self {
        Self {
            jobs: Vec::with_capacity(n),
        }
    }

    pub fn push<F>(&mut self, f: F)
    where
        F: FnOnce(&AtomicBool) -> T + Send + 'static,
    {
        self.jobs.push(Box::new(f));
    }

    pub fn from_iter<I, F>(iter: I) -> Self
    where
        I: IntoIterator<Item = F>,
        F: FnOnce(&AtomicBool) -> T + Send + 'static,
    {
        let mut g = JobGroup::new();
        for f in iter {
            g.push(f);
        }
        g
    }

    // Run until the first job finishes; remaining jobs observe the flag and may exit early.
    pub fn run_first(self) -> T {
        let done = Arc::new(AtomicBool::new(false));
        let (tx, rx) = mpsc::channel();
        let mut handles = Vec::with_capacity(self.jobs.len());
        for job in self.jobs {
            // move each job into its own thread
            let done_cl = done.clone();
            let tx_cl = tx.clone();
            handles.push(thread::spawn(move || {
                let res = job(&done_cl);
                if !done_cl.swap(true, Ordering::AcqRel) {
                    let _ = tx_cl.send(res);
                }
            }));
        }
        drop(tx);
        let first = rx.recv().expect("no jobs submitted");
        done.store(true, Ordering::Release);
        for h in handles {
            let _ = h.join();
        }
        first
    }

    // Run all jobs to completion and collect results in submission order.
    pub fn run_all(self) -> Vec<T> {
        let flag = Arc::new(AtomicBool::new(false)); // remains false; signature compatibility
        let mut handles = Vec::with_capacity(self.jobs.len());
        for (idx, job) in self.jobs.into_iter().enumerate() {
            let flag_cl = flag.clone();
            handles.push(thread::spawn(move || (idx, job(&flag_cl))));
        }
        let mut results: Vec<(usize, T)> = Vec::with_capacity(handles.len());
        for h in handles {
            results.push(h.join().expect("thread panicked"));
        }
        results.sort_by_key(|(i, _)| *i);
        results.into_iter().map(|(_, v)| v).collect()
    }
}

// Convenience free functions mirroring previous API (optional)
pub fn first_result<T, F, I>(jobs: I) -> T
where
    I: IntoIterator<Item = F>,
    F: FnOnce(&AtomicBool) -> T + Send + 'static,
    T: Send + 'static,
{
    JobGroup::from_iter(jobs).run_first()
}

pub fn all_results<T, F, I>(jobs: I) -> Vec<T>
where
    I: IntoIterator<Item = F>,
    F: FnOnce(&AtomicBool) -> T + Send + 'static,
    T: Send + 'static,
{
    JobGroup::from_iter(jobs).run_all()
}

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
    let (tx, rx) = mpsc::channel();
    let mut handles = Vec::with_capacity(total);

    for job in collected.into_iter() {
        let done_cl = done.clone();
        let tx_cl = tx.clone();
        handles.push(thread::spawn(move || match job(&done_cl) {
            Ok(v) => {
                if !done_cl.swap(true, Ordering::AcqRel) {
                    let _ = tx_cl.send(Ok(v));
                }
            }
            Err(e) => {
                if !done_cl.load(Ordering::Acquire) {
                    let _ = tx_cl.send(Err(e));
                }
            }
        }));
    }
    drop(tx);

    let mut errs: Vec<E> = Vec::new();
    while let Ok(msg) = rx.recv() {
        match msg {
            Ok(v) => {
                done.store(true, Ordering::Release);
                for h in handles {
                    let _ = h.join();
                }
                return Ok(v);
            }
            Err(e) => {
                errs.push(e);
                if errs.len() == total {
                    for h in handles {
                        let _ = h.join();
                    }
                    return Err(errs);
                }
            }
        }
    }
    // Channel closed without success; ensure joins.
    for h in handles {
        let _ = h.join();
    }
    if errs.len() == total {
        Err(errs)
    } else {
        panic!("inconsistent state")
    }
}
