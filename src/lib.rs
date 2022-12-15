//! Lifetime-erased borrowing across threads.
//!
//! This crate provides two types: [`Lender<'a, T>`][Lender] and [`Loan<T>`]. A
//! `Lender` is constructed from a reference to some value, and it hands out
//! `Loan`s that can be sent to other threads. A `Loan` is like an [`Arc`] that
//! points to data whose lifetime is tied to the `Lender`.
//!
//! The key thing that makes this safe is that **the lender blocks until all
//! outstanding loans are dropped**. Just as [`Mutex`][std::sync::Mutex] pushes
//! Rust's _aliasing_ guarantees from compile-time to run-time, [`Lender`]
//! pushes Rust's _lifetime_ guarantees from compile-time to run-time.
//!
//! The tradeoff made here is that the lender may block for an indeterminate
//! amount of time if loans are passed to other threads. However, this
//! limitation also exists in the alternative solution, which is using [scoped
//! threads] with explicit lifetimes all over the place. And this solution
//! requires no lifetimes!
//!
//! [scoped threads]: std::thread::scope
//!
//! # Example
//!
//! ```
//! use std::thread;
//! use lender_loan::{Lender, Loan};
//!
//! fn use_loan(loan: Loan<Vec<i32>>) {
//!     assert_eq!(*loan, vec![1, 2, 3]);
//! }
//!
//! // Create a value that will be lent to other threads.
//! let mut value = vec![1, 2, 3];
//! Lender::with(&value, |lender: &Lender<'_, Vec<i32>>| {
//!     for _ in 0..100 {
//!         let loan = lender.lend();
//!         thread::spawn(move || use_loan(loan));
//!     }
//! });
//!
//! // It is safe to modify the value again; `Lender::with` blocks until all loans are dropped.
//! value.push(4);
//! ```

#![deny(unsafe_op_in_unsafe_fn)]
//#![warn(missing_docs)]

use core::fmt;
use std::{hash, ops::Deref, sync::Arc};

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lender<'a, T: ?Sized> {
    inner: Arc<LoanInner<'a, T>>,
}

impl<'a, T: ?Sized> Lender<'a, T> {
    pub fn with<R>(value: &T, op: impl FnOnce(&Lender<'_, T>) -> R) -> R {
        let event = Event::new();
        let inner = Arc::new(LoanInner {
            principal: value,
            event: &event,
        });
        let lender = Lender { inner };
        let result = op(&lender);

        // This is the whole thing that makes it safe.
        drop(lender);
        event.wait();

        result
    }

    #[inline]
    pub fn lend(&self) -> Loan<T>
    where
        T: 'static,
    {
        let loaned_inner: Arc<LoanInner<'a, T>> = self.inner.clone();

        // SAFETY: TODO
        let loaned_inner: Arc<LoanInner<'static, T>> = unsafe { std::mem::transmute(loaned_inner) };
        Loan {
            inner: loaned_inner,
        }
    }
}

impl<'a, T: ?Sized> AsRef<T> for Lender<'a, T> {
    #[inline]
    fn as_ref(&self) -> &T {
        self.inner.principal
    }
}

impl<'a, T: ?Sized> Deref for Lender<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.inner.principal
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loan<T: 'static + ?Sized> {
    inner: Arc<LoanInner<'static, T>>,
}

impl<T: 'static + ?Sized> AsRef<T> for Loan<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        self.inner.principal
    }
}

impl<T: 'static + ?Sized> Deref for Loan<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.inner.principal
    }
}

struct LoanInner<'a, T: ?Sized> {
    /// The thing we are lending.
    principal: &'a T,
    /// An event that is signaled when the last reference to the `LoanInner` is dropped.
    event: &'a Event,
}

impl<'a, T: ?Sized> Drop for LoanInner<'a, T> {
    fn drop(&mut self) {
        self.event.trigger();
    }
}

struct Event(rsevents::ManualResetEvent);

impl Event {
    #[inline]
    fn new() -> Self {
        Self(rsevents::ManualResetEvent::new(rsevents::EventState::Unset))
    }

    #[inline]
    fn trigger(&self) {
        self.0.set();
    }

    #[inline]
    fn wait(&self) {
        rsevents::Awaitable::wait(&self.0);
    }
}

impl<'a, T: fmt::Display + ?Sized> fmt::Display for Lender<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.deref(), f)
    }
}

impl<'a, T: fmt::Debug + ?Sized> fmt::Debug for Lender<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.deref(), f)
    }
}

impl<'a, T: ?Sized> fmt::Pointer for Lender<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Pointer::fmt(&(self.deref() as *const T), f)
    }
}

impl<T: 'static + fmt::Display + ?Sized> fmt::Display for Loan<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.deref(), f)
    }
}

impl<T: 'static + fmt::Debug + ?Sized> fmt::Debug for Loan<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.deref(), f)
    }
}

impl<T: 'static + ?Sized> fmt::Pointer for Loan<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Pointer::fmt(&(self.deref() as *const T), f)
    }
}

impl<'a, T: PartialEq + ?Sized> PartialEq for LoanInner<'a, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.principal == other.principal
    }
}

impl<'a, T: Eq + ?Sized> Eq for LoanInner<'a, T> {}

impl<'a, T: PartialOrd + ?Sized> PartialOrd for LoanInner<'a, T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.principal.partial_cmp(other.principal)
    }
}

impl<'a, T: Ord + ?Sized> Ord for LoanInner<'a, T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.principal.cmp(other.principal)
    }
}

impl<'a, T: hash::Hash + ?Sized> hash::Hash for LoanInner<'a, T> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.principal.hash(state);
    }
}
