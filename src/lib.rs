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
//!         std::thread::spawn(move || use_loan(loan));
//!     }
//! });
//!
//! // It is safe to modify the value again; `Lender::with` blocks until all loans are dropped.
//! value.push(4);
//! ```

#![deny(unsafe_op_in_unsafe_fn)]
#![warn(missing_docs)]

use std::{fmt, hash, marker::PhantomData, ops::Deref, sync::Arc};

/// A scoped reference to a value that can be lent to other threads in a
/// lifetime-erased way.
///
/// This type cannot be constructed manually, you can only get one by calling
/// [`Lender::with`]. This type also cannot be cloned. It is the "owner" of the
/// shared reference that is to be lent out.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lender<'a, T: 'a + ?Sized> {
    // Both the lender and all of its loans hold a strong reference to the `LoanInner`.
    inner: Arc<LoanInner<'a, T>>,
}

impl<'a, T: 'a + ?Sized> Lender<'a, T> {
    /// Constructs a [`Lender`] for the provided value.
    ///
    /// A reference to the lender is passed to the provided closure.
    ///
    /// # Example
    ///
    /// ```
    /// use lender_loan::{Lender, Loan};
    ///
    /// fn use_loan(loan: Loan<Vec<i32>>) {
    ///     assert_eq!(*loan, vec![1, 2, 3]);
    /// }
    ///
    /// // Create a value that will be lent to other threads.
    /// let mut value = vec![1, 2, 3];
    /// Lender::with(&value, |lender: &Lender<'_, Vec<i32>>| {
    ///     for _ in 0..100 {
    ///         let loan = lender.lend();
    ///         std::thread::spawn(move || use_loan(loan));
    ///     }
    /// });
    ///
    /// // It is safe to modify the value again; `Lender::with` blocks until all loans are dropped.
    /// value.push(4);
    /// ```
    pub fn with<R>(value: &'a T, op: impl FnOnce(&Lender<'a, T>) -> R) -> R {
        // This one-shot event is borrowed by the `LoanInner` and triggered when
        // the last `Arc<LoanInner>` is dropped.
        let event = Event::new();

        // SAFETY: The event.wait() call below ensures that the event is valid
        // for at least as long as all `Arc<LoanInner>`s.
        let inner = unsafe { LoanInner::new(value, &event) };

        let lender = Lender {
            inner: Arc::new(inner),
        };
        let result = op(&lender);

        // The lender holds a strong reference to the `LoanInner`. Make sure we
        // drop it before waiting on the event, otherwise the `LoanInner` will
        // never drop!
        drop(lender);

        // Wait for all loans to have been dropped.
        // This is the whole point!!!
        event.wait();

        result
    }

    /// Returns the reference to the value being lent.
    ///
    /// # Example
    ///
    /// ```
    /// use lender_loan::Lender;
    ///
    /// let value = vec![1, 2, 3];
    /// Lender::with(&value, |lender| {
    ///     assert_eq!(lender.get() as *const _, &value as *const _);
    /// });
    /// ```
    #[inline]
    pub fn get(&self) -> &'a T {
        self.inner.principal()
    }

    /// Returns a lifetime-erased [`Loan`] of the value being lent.
    ///
    /// The lender will not go out of scope until all loans have been dropped.
    ///
    /// # Example
    ///
    /// ```
    /// use lender_loan::Lender;
    ///
    /// let value = String::from("you owe me tree fiddy");
    /// Lender::with(&value, |lender| {
    ///     let loan = lender.lend();
    ///     std::thread::spawn(move || {
    ///         println!("{}", loan.as_ref());
    ///     });
    /// });
    #[inline]
    pub fn lend(&self) -> Loan<T> {
        let loaned_inner: Arc<LoanInner<'a, T>> = self.inner.clone();

        // Here is where we erase the lifetime.
        //
        // SAFETY: There are two things to make sure of:
        //  1. The `T` pointed to by the `LoanInner` is valid as long as any
        //     `Loan` exists.
        //  2. No `&mut T` can be created until all `Loans` are dropped (since
        //     they are basically `&T`).
        //
        // Both of these are achieved by the `event.wait()` call in
        // `Lender::with`. The event does not fire until all `Loans` have
        // dropped.
        let loaned_inner: Arc<LoanInner<'static, T>> = unsafe { std::mem::transmute(loaned_inner) };
        Loan {
            inner: loaned_inner,
        }
    }
}

impl<'a, T: ?Sized> AsRef<T> for Lender<'a, T> {
    #[inline]
    fn as_ref(&self) -> &T {
        self.inner.principal()
    }
}

impl<'a, T: ?Sized> Deref for Lender<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.inner.principal()
    }
}

/// A lifetime-erased, reference-counted pointer to a [`Lender`]'s data.
///
/// It's basically just an [`Arc`] that tells the [`Lender`] to proceed once all
/// references are dropped.
///
/// This type can only be created by calling [`Lender::lend`].
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loan<T: ?Sized> {
    // Both the lender and all of its loans hold a strong reference to the `LoanInner`.
    //
    // The lifetime is 'static because the lifetime has been erased!
    // This is okay because `Loan` hands out references with lifetimes no longer
    // than that of itself, and the stack frame in which the loaned value lives
    // is guaranteed not to pop until all `Loan`s have dropped.
    inner: Arc<LoanInner<'static, T>>,
}

impl<T: ?Sized> AsRef<T> for Loan<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        self.inner.principal()
    }
}

impl<T: ?Sized> Deref for Loan<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.inner.principal()
    }
}

mod inner {
    use super::*;

    /// The thing that both the [`Lender`] and [`Loan`] hold strong [`Arc`]
    /// references to.
    ///
    /// It is in a private module so the fields cannot be directly accessed.
    pub(super) struct LoanInner<'a, T: ?Sized> {
        /// The thing we are lending. (ha ha, get it, the loan principal?) This
        /// has the same lifetime 'a as the `Lender` that "owns" the original
        /// reference.
        ///
        /// Note: This would _like_ to be a `&'a T`, but that would require that
        /// `T: 'static` since `Loan<T>` stores a `LoanInner<'static, T>`. But
        /// that's an unnecessary restriction; it's okay to lend out loans to
        /// something like `&'a Foo<'b>` since we know `'b: 'a` is implied by
        /// the type system.
        principal: *const T,

        /// An event that is signaled when the last reference to the `LoanInner`
        /// is dropped. This has a lifetime that is shorter than that of the
        /// loan principal, but at least as long as this `LoanInner`.
        event: *const Event,

        _phantom: PhantomData<&'a ()>,
    }

    impl<'a, T: ?Sized> LoanInner<'a, T> {
        /// Returns a new [`LoanInner`].
        ///
        /// # Safety
        ///
        /// The provided [`Event`] reference must be valid for at least as long
        /// as this [`LoanInner`] is valid, because it is dereferenced in the
        /// `Drop` implementation.
        pub(super) unsafe fn new(principal: &'a T, event: &Event) -> Self
        where
            T: 'a,
        {
            Self {
                principal: principal as *const T,
                event: event as *const Event,
                _phantom: PhantomData,
            }
        }

        /// Returns a reference to the value being lent (ha ha, get it, the loan
        /// principal?).
        #[inline(always)]
        pub(super) fn principal(&self) -> &'a T {
            // SAFETY: The principal pointer was constructed from a `&'a T`.
            unsafe { &*self.principal }
        }

        #[inline(always)]
        fn event(&self) -> &Event {
            // SAFETY: The event pointer was constructed from a valid `&Event`,
            // and the caller of new() promised that it would be  valid for at
            // least as long as this LoanInner.
            unsafe { &*self.event }
        }
    }

    impl<'a, T: ?Sized> Drop for LoanInner<'a, T> {
        fn drop(&mut self) {
            self.event().trigger();
        }
    }

    /// SAFETY: The pointers in [`LoanInner`] may as well just be shared
    /// references, so it's `Send` whenever the inner bits are.
    unsafe impl<'a, T> Send for LoanInner<'a, T>
    where
        T: Send + ?Sized,
        Event: Send,
    {
    }

    /// SAFETY: The pointers in [`LoanInner`] may as well just be shared
    /// references, so it's `Sync` whenever the inner bits are.
    unsafe impl<'a, T> Sync for LoanInner<'a, T>
    where
        T: Sync + ?Sized,
        Event: Sync,
    {
    }
}

use inner::LoanInner;

/// A simple one-shot event.
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

impl<T: fmt::Display + ?Sized> fmt::Display for Loan<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.deref(), f)
    }
}

impl<T: fmt::Debug + ?Sized> fmt::Debug for Loan<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.deref(), f)
    }
}

impl<T: ?Sized> fmt::Pointer for Loan<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Pointer::fmt(&(self.deref() as *const T), f)
    }
}

impl<'a, T: PartialEq + ?Sized> PartialEq for LoanInner<'a, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.principal() == other.principal()
    }
}

impl<'a, T: Eq + ?Sized> Eq for LoanInner<'a, T> {}

impl<'a, T: PartialOrd + ?Sized> PartialOrd for LoanInner<'a, T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.principal().partial_cmp(other.principal())
    }
}

impl<'a, T: Ord + ?Sized> Ord for LoanInner<'a, T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.principal().cmp(other.principal())
    }
}

impl<'a, T: hash::Hash + ?Sized> hash::Hash for LoanInner<'a, T> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.principal().hash(state);
    }
}
